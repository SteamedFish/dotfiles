# opencode 配置架构（按项目区分，不按机器）

> 本文档为 dotfiles 项目中 opencode 配置的专用文档。
> **本文档不得包含敏感信息**：API key、内部服务地址、私有/自部署模型名一律不写在这里，只放在 git-crypt 加密的配置文件中。

2026-10 重构：不再按 home/work 机器区分（yadm class），改为**按项目**区分。

## 文件布局

| 文件 | 加密 | 作用 |
|------|------|------|
| `.config/opencode/opencode.jsonc` | git-crypt | 全局主配置。定义 `work` provider（公司内部 proxy）及全部模型 metadata；home 模型由 litellm wellknown provider 自动发现 |
| `.config/opencode/cli.json` | 明文 | OpenCode V2 的 **TUI 侧**配置（V2 不再读 `tui.json`）。只声明"带终端 UI"的插件：目前只有 omo-slim（`/preset` 流程 + 更新 badge）；goal 已改用 opencode 内置实现，无需在此声明 |
| `.config/opencode/oh-my-opencode-slim.jsonc` | git-crypt | 全局默认 `preset: home`；含 home/work 两套 preset 的 fallback 链 |
| `.config/opencode/litellm-token` | git-crypt | litellm（home 模型池）API key 的源副本；实际生效凭证在 `~/.local/share/opencode/auth.json` |
| `.config/opencode/work-token` | git-crypt | work proxy API key，被 opencode.jsonc 以 `{file:~/.config/opencode/work-token}` 引用 |
| `.config/billion-context/billion-context.json` | git-crypt | billion-context 压缩插件配置，含私有模型名 |

## 使用规则

1. **home/个人项目**：只用 `litellm/*` 模型（个人购买）。全局配置默认即是。
2. **work 项目**：在仓库根放 `.opencode/oh-my-opencode-slim.jsonc`（内容 `{"preset": "work"}`），fallback 链 work/* 优先、litellm/* 兜底（work 限流/额度耗尽时自动切换）。注意 `fallback`（重试次数/间隔）是**顶层**配置、对两套 preset 同时生效，没有 per-preset 覆盖；要 work 专属值只能在该项目文件里 deep-merge 覆盖顶层 fallback；再放 `.opencode/opencode.jsonc` 覆盖 `agents.title.model` 为 work provider 下的本地自部署模型（具体模型名见加密后的 opencode.jsonc）及 council preset。
3. **home 项目禁止使用 `work/*` 模型**（公司资源不得用于个人项目）。

## 关键实现细节（踩坑记录）

- opencode v2.1.2 中 provider config 的 `models.*.variants` **必须是 array**：`[{"id": "low", "settings": {"reasoningEffort": "low"}}]`。map 形式（`{"low": {...}}`）会导致整个 provider 被**静默丢弃**、无任何报错。schema 里的 map 定义是误导。
- `{file:...}` 插值支持 `~` 展开，可用于 apiKey 外部化。
- 配置层级：全局 → 沿途 `opencode.json` → 沿途 `.opencode/opencode.json`（更高）。结构体深合并，数组替换；plugins 数组例外为累加（前缀 `-` 可移除）。
- omo-slim 项目级 `.opencode/oh-my-opencode-slim.jsonc` deep-merge 覆盖用户级；`OH_MY_OPENCODE_SLIM_PRESET` 环境变量优先级最高（不要全局 export）。fallback 链对 429/quota/auth/5xx/网络错误自动推进。session 在首个受管 prompt 前捕获 snapshot，改配置只影响新 session。
- `litellm-token` 文件本身无运行时消费者，只是凭证的加密源副本；改 key 后要同步更新 `~/.local/share/opencode/auth.json`（`opencode auth login`）。
- billion-context 配置中 `providers` 的 key 必须以 `http(s)://` 开头才是路由；1M 窗口模型用全局默认 50%/95%，≤262k 窗口模型才加 80%/90% per-model override。
- **omo-slim fallback 的真实语义**（v3.0.6 + opencode v2 host，读 dist 源码 + 日志确认）：
  - v2 host 上只有 retry-hook steering 生效（日志 `v2 replay fallback disabled (no atomic per-turn model switch); retry-hook steering active`），`fallback.initialRetryDelayMs` 被**忽略**（日志 `retry hook ignores initial delay`）。
  - `fallback.maxRetries` 是**一次 descent 内共享**的同模型重试预算，不随换模型重置，只在成功 step / 新用户轮 / 删除 session 时清零。因此**只有链头能吃重试**（1+maxRetries 次），其后每个条目都只试 1 次。想"多试几次"不是调大 maxRetries，而是让链头足够可靠。
  - `sessionTried` 是 Set，按 `provider/model` 字符串去重，链里重复同一 id 无效（含不同 variant）。所以"a b c a b c"在单个 step 内**做不到**；能实现的是"每个成功 step 重置一次 descent"，即跨 step 反复走 a→b→c。`continuationPolicy: retry-primary` 保证降级后下一个 step 仍先试 primary。
  - fallback 配置只有**顶层**（schema 无 per-preset / per-agent 覆盖），要 work 专属调参只能改工作 preset 的链，或在工作仓库 `.opencode/oh-my-opencode-slim.jsonc` 里 deep-merge 覆盖顶层 fallback。
- **work proxy 限流是硬上限**：按**用户**约 14 请求/分钟（滑动窗口，所有模型共用，本地自部署模型也计入），超限秒回 429 `user requests-per-minute limit exceeded`，约 60s 才恢复；模型 ID 清单见加密后的 `opencode.jsonc`。推论：① 429 时换模型没有意义（同一用户额度）；② 链内 work 条目多于 2 个等于白烧配额；③ agentic 并发（explorer 尤其）必然溢出限流，溢出部分交给 home 才是正确行为，别指望配置能"压住不降级"。
- **项目级 `.opencode/opencode.jsonc` 的顶层 `"model"` 也会咬人**：工作仓库上层的项目级配置曾写死一个已从代理下线的模型，导致其下所有新 session 一开就 404。改 work 模型清单时要连项目级默认 model、council 席位一起 grep（`grep -rn "work/" --include=*.jsonc <工作目录>`）。
- **goal 用 opencode 内置实现**（v2.2.1 引入，`packages/core/src/plugin/goal.ts`）：命令 `/goal <objective>` 与 `/goal pause|resume|clear`，TUI 状态行 + Desktop composer pill，状态存 `~/.local/share/opencode/opencode.db` 的 `kv` 表（key = `plugin:` + `opencode.goal` 的 UTF-16 hex + `:session:<ses_id>`，按 session 独立，子代理/分叉不继承）。**完全不可配置**：config schema 无 goal 字段、源码不读 config，行为由硬编码常量决定 —— `MAX_OBJECTIVE=4000`、`MAX_ROUNDS=50`、`MAX_STALLS=3`、`WAIT_LIMIT=30min`（空转即 paused）。裁判用当前 session 的模型（`ctx.session.generate`，每轮多一次请求，吃限流配额，无独立裁判模型可配）；`/goal resume` 把 `rounds` 重置为 1、保留 `maxRounds`，所以 50 轮上限靠手动 resume 续窗。界面仅英文。
- `cli.json` 的 `plugins` 只放**带 `./tui` 导出**的包（判断依据：包 `package.json` 的 `exports`）。判断错方向会有害：server-only 的包（如 billion-context，只有 `.`/`./server`/`./dsh`/`./client`）写进去等于把 server 代码塞进 TUI 进程。当前 cli.json 里只有 omo-slim（TUI 侧提供 `/preset` 流程与更新 badge）；终端侧已加载的插件可在 TUI 里用 `/plugins` 查看，`opencode plugin check` 的 `Server`/`TUI` 两段分别对应这两个文件。
- 插件条目必须带 `@latest`（如 `@prevalentware/opencode-goal-plugin@latest`）：`opencode plugin update` 按声明 spec 重新解析，写死版本号就永远不会升级。`opencode plugin check` 先看有没有新版；新增但未安装的条目会在 `plugin update` 时被安装（实测 v2.1.3 用 `opencode plugin update` 即完成 billion-context 0.1.208→0.1.209 与 goal 插件首装）。
- `opencode plugin update` / `check` 只认全局配置与项目配置里的声明，`--print-logs` 可用于看解析失败原因；`opencode debug config` 可确认 `plugins` 数组是否被正确解析（排除静默丢弃）。
- **goal 插件已卸载**（`@prevalentware/opencode-goal-plugin`，2026-10-29 装、2026-10-31 卸）：它在 V2 需要两处声明（`opencode.jsonc` 管 server 侧命令/工具/状态，`cli.json` 管 TUI 侧侧栏与语言），但内置 `/goal` 出现后两者抢同一命令名 —— 插件 V2 setup 会先 `context.command.list()` 快照、重名直接 `continue`，于是**内置赢**，插件只剩 `/pause_goal`、`/resume_goal` 与 14 个工具，而模型手里的 goal 工具操作的是插件的 `goals.json`，与内置 goal 状态互不相通（模型看不到用户 `/goal` 建的目标，`get_goal` 返回 null）。上游 README 自 0.1.85 起也建议 2.2.1+ 用户改用内置。若将来要恢复：两处都要写 `{package, options}` 对象（`locale` 各侧独立生效），并设 `command_name: "pgoal"` 避开内置命令名（改名后 `history/status/edit/replace/stop` 子命令才会完整注册回来）；注意 `max_no_progress_turns`/`max_prompt_failures` 写 0 会被 `positiveIntegerOrNull2(0) → null` 静默回退默认值（2 / 3），**不是**"立即暂停"，真正 0=无上限的只有 `max_task_block_seconds`。

## 模型选型原则

- 配置里的 `cost` 字段一律按**原始 API 牌价**（models.dev），供公司统计真实消耗；**模型优先级/性价比判断按套餐价**（参考 github.com/FeiZhuLulu/real-api-pricing 的套餐折算）。
- home 订阅：Kimi Allegro（K3.1 与 K3 同价 → K3 淘汰；K3-256k 更便宜；kimi-for-coding 最便宜档）、opencode go $10（仅少数模型性价比好，质量有争议）、deepseek 官方 API（按量，质量对标 AA 评测）、minimax starter（便宜但能力弱）。
- work 计费口径：kimi 类按 Kimi Allegro、step 类按 step plan max、GLM 类按 GLM plan、deepseek 类按官方 API。
- Kimi K2 系列（kimi-for-coding 的 canonical k2.8-preview）2026 年底官方下线，届时 home 链需迁移。
- work proxy 模型清单以加密后的 opencode.jsonc 内注释为准。

## 变更记录

- **2026-10-29（work 链实测修正）**：直连探活 work proxy 后发现三处硬伤并修正：
  ① 链头/council 席位/项目级默认 model 里引用了一个**已从代理下线**的模型（404 `model_not_found`），
  每一步都白烧一次请求或直接报错 —— 已从所有链、席位与项目级配置里移除，并从 `opencode.jsonc`
  的 metadata 中删除；② 某个别名模型实际路由到其 256k 变体的后端（metadata 里写成 1M，已改回
  262144，超长请求会失败）；③ 有两个模型 ID 直连恒 429（group 内无账号），另有一个别名模型的
  后端是别的模型 —— 这些不许进链。另查明网关按**用户**限流 ~14 请求/分钟（所有模型共用，
  超限秒 429、约 60s 恢复），所以"多换几个模型重试"对 429 无效。据此重排 work preset：
  链内 work 条目压到 ≤2、链头换成现役可靠模型、explorer 只留 1 个免费本地条目（它请求量最大，
  是烧光限流额度、把 orchestrator/fixer 挤到 home 的主因）；`fallback.maxRetries` 3→2
  （同模型重试各烧一个配额，429 不会在几秒内恢复），`retryDelayMs` 500→3000，显式固定
  `continuationPolicy: retry-primary`（每个成功 step 后回 primary，下一步重新给 work 机会）。
  具体模型 ID 一律以加密后的 `opencode.jsonc` 注释为准。
- **2026-10-31（卸载 goal 插件，改用内置 `/goal`）**：opencode v2.2.1 起内置 `/goal`（裁判循环）。实测本机 v2.2.4 的 `opencode api get /api/command` 返回三条：`goal`（内置描述）、`pause_goal`/`resume_goal`（插件中文描述），即插件 V2 setup 的防重名快照让内置赢、插件退让。两套状态互不相通（内置存 `opencode.db` 的 kv，插件存 `~/.local/share/opencode-goal-plugin/goals.json`），而模型侧的 goal 工具（`get_goal`/`update_goal_plan` 等 14 个）全部来自插件，导致"用户敲 `/goal`、模型操作另一套目标"的错位；上游 README 自 0.1.85 起亦建议 2.2.1+ 用户改用内置。据此从 `opencode.jsonc` 与 `cli.json` 两处移除插件声明。内置 goal 经查**零配置项**（config schema 无 goal 字段、源码不读 config，常量全硬编码），故不再新增配置；插件状态文件 `goals.json`（v3，12 目标/4 归档）保留作历史，未删除。
- **2026-10-29**：新增 goal 插件 `@prevalentware/opencode-goal-plugin@latest`（`/goal` 长任务模式）。server 侧在 `opencode.jsonc` 用 `{package, options}` 声明（`locale: zh-CN`、`max_auto_turns: 1e6`、`default_token_budget: 1e9` ≈ 无上限，`max_goal_duration_seconds` 留空即无时限；保险丝 `max_no_progress_turns: 4`、`max_prompt_failures: 10`、`max_task_block_seconds: 7200`）；新建 `cli.json` 声明 TUI 侧，并顺带启用 omo-slim 的 `/preset`。均已 `opencode plugin update` 实装（goal 0.1.84），`plugin check` Server/TUI 两侧 current。仓库根 `AGENTS.md` 已删除。（2026-10-31 已卸载，见上条）

## 迁移步骤（从旧机器/旧配置升级）

1. `yadm pull`（或在本 clone `git pull && yadm alt`）后，检查 `~/.config/opencode/` 下残留的渲染产物：旧 `opencode.jsonc`、旧 `oh-my-opencode-slim.jsonc`、旧 `litellm-token`——新版已去模板化，直接以仓库文件为准。
2. work 项目仓库内新增 `.opencode/oh-my-opencode-slim.jsonc` 与 `.opencode/opencode.jsonc`（见「使用规则」）。
3. 旧的 open-webui / pony.ai provider 及 github-copilot 链已废弃删除。
