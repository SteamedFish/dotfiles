# opencode 配置架构（按项目区分，不按机器）

> 本文档为 dotfiles 项目中 opencode 配置的专用文档。
> **本文档不得包含敏感信息**：API key、内部服务地址、私有/自部署模型名一律不写在这里，只放在 git-crypt 加密的配置文件中。

2026-10 重构：不再按 home/work 机器区分（yadm class），改为**按项目**区分。

## 文件布局

| 文件 | 加密 | 作用 |
|------|------|------|
| `.config/opencode/opencode.jsonc` | git-crypt | 全局主配置。定义 `work` provider（公司内部 proxy）及全部模型 metadata；home 模型由 litellm wellknown provider 自动发现 |
| `.config/opencode/oh-my-opencode-slim.jsonc` | git-crypt | 全局默认 `preset: home`；含 home/work 两套 preset 的 fallback 链 |
| `.config/opencode/litellm-token` | git-crypt | litellm（home 模型池）API key 的源副本；实际生效凭证在 `~/.local/share/opencode/auth.json` |
| `.config/opencode/work-token` | git-crypt | work proxy API key，被 opencode.jsonc 以 `{file:~/.config/opencode/work-token}` 引用 |
| `.config/billion-context/billion-context.json` | git-crypt | billion-context 压缩插件配置，含私有模型名 |

## 使用规则

1. **home/个人项目**：只用 `litellm/*` 模型（个人购买）。全局配置默认即是。
2. **work 项目**：在仓库根放 `.opencode/oh-my-opencode-slim.jsonc`（内容 `{"preset": "work"}`），fallback 链 work/* 优先、litellm/* 兜底（work 用量耗尽时自动切换）；再放 `.opencode/opencode.jsonc` 覆盖 `agents.title.model` 为 work provider 下的本地自部署模型（具体模型名见加密后的 opencode.jsonc）及 council preset。
3. **home 项目禁止使用 `work/*` 模型**（公司资源不得用于个人项目）。

## 关键实现细节（踩坑记录）

- opencode v2.1.2 中 provider config 的 `models.*.variants` **必须是 array**：`[{"id": "low", "settings": {"reasoningEffort": "low"}}]`。map 形式（`{"low": {...}}`）会导致整个 provider 被**静默丢弃**、无任何报错。schema 里的 map 定义是误导。
- `{file:...}` 插值支持 `~` 展开，可用于 apiKey 外部化。
- 配置层级：全局 → 沿途 `opencode.json` → 沿途 `.opencode/opencode.json`（更高）。结构体深合并，数组替换；plugins 数组例外为累加（前缀 `-` 可移除）。
- omo-slim 项目级 `.opencode/oh-my-opencode-slim.jsonc` deep-merge 覆盖用户级；`OH_MY_OPENCODE_SLIM_PRESET` 环境变量优先级最高（不要全局 export）。fallback 链对 429/quota/auth/5xx/网络错误自动推进。session 在首个受管 prompt 前捕获 snapshot，改配置只影响新 session。
- `litellm-token` 文件本身无运行时消费者，只是凭证的加密源副本；改 key 后要同步更新 `~/.local/share/opencode/auth.json`（`opencode auth login`）。
- billion-context 配置中 `providers` 的 key 必须以 `http(s)://` 开头才是路由；1M 窗口模型用全局默认 50%/95%，≤262k 窗口模型才加 80%/90% per-model override。

## 模型选型原则

- 配置里的 `cost` 字段一律按**原始 API 牌价**（models.dev），供公司统计真实消耗；**模型优先级/性价比判断按套餐价**（参考 github.com/FeiZhuLulu/real-api-pricing 的套餐折算）。
- home 订阅：Kimi Allegro（K3.1 与 K3 同价 → K3 淘汰；K3-256k 更便宜；kimi-for-coding 最便宜档）、opencode go $10（仅少数模型性价比好，质量有争议）、deepseek 官方 API（按量，质量对标 AA 评测）、minimax starter（便宜但能力弱）。
- work 计费口径：kimi 类按 Kimi Allegro、step 类按 step plan max、GLM 类按 GLM plan、deepseek 类按官方 API。
- Kimi K2 系列（kimi-for-coding 的 canonical k2.8-preview）2026 年底官方下线，届时 home 链需迁移。
- work proxy 模型清单以加密后的 opencode.jsonc 内注释为准。

## 迁移步骤（从旧机器/旧配置升级）

1. `yadm pull`（或在本 clone `git pull && yadm alt`）后，检查 `~/.config/opencode/` 下残留的渲染产物：旧 `opencode.jsonc`、旧 `oh-my-opencode-slim.jsonc`、旧 `litellm-token`——新版已去模板化，直接以仓库文件为准。
2. work 项目仓库内新增 `.opencode/oh-my-opencode-slim.jsonc` 与 `.opencode/opencode.jsonc`（见「使用规则」）。
3. 旧的 open-webui / pony.ai provider 及 github-copilot 链已废弃删除。
