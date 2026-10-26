# dotfiles

个人 dotfiles 仓库（yadm 管理），远程 git@github.com:Steamedfish/dotfiles.git。

## 项目规则

- **Git 工作流**：简单个人项目。topic branch 开发 → 验证后 merge 回 master → push。所有 commit 必须 GPG 签名。yadm 的实际仓库在 `~/.local/share/yadm/repo.git`，本目录是工作 clone。
- **模板机制**：yadm 内建 `yadm alt`（jinja），按 `yadm.class` 渲染 `##template` 后缀文件。`.ssh/config##template.esh` 用 esh。渲染产物已 gitignore，不存在于仓库。
- **敏感信息**：一律 git-crypt 加密（见 `.gitattributes`）。API key、token、私有模型名（含自部署/内部模型名称）都属敏感。新增含敏感信息的文件时，先加 `.gitattributes` 再 `git add`，并 `git-crypt status` 确认。
- **TODO / CHANGELOG**：维护在本文件末尾（见下文）。

## opencode 配置

opencode 相关配置架构、使用规则、踩坑记录见 [.config/opencode/AGENTS.md](.config/opencode/AGENTS.md)。

## TODO

- [ ] work 项目仓库模板（`.opencode/` 两份文件）落地到实际 work 仓库
- [ ] Kimi K2 下线前迁移 home 链的 kimi-for-coding 引用（2026 年底）

## CHANGELOG

### 2026-10-26

- **重构**：opencode 配置从「按机器（yadm class home/work）」改为「按项目」区分。`opencode.jsonc##template`、`oh-my-opencode-slim.jsonc##template`、`litellm-token##template` 去模板化；删除 `opencode-models.json.inc`、废弃的 open-webui/pony.ai provider 与 github-copilot 链、废弃的 locez 模型。
- **新增**：`work` provider（公司内部 proxy，8 个模型全量 metadata，cost 按原始 API 牌价）；`work-token`（git-crypt）；omo-slim home/work 双 preset + fallback 链；billion-context work 段。
- **修复**：`.config/opencode/*.jsonc`、`oh-my-opencode-slim.jsonc`、`billion-context.json` 纳入 git-crypt（此前 oh-my-opencode-slim 与 bili 配置明文泄漏私有模型名；GitHub 历史仍含旧明文，如需彻底清除需 filter-repo 改写历史）。
