---
name: using-git-worktrees
description: Use when starting feature work that needs isolation from current workspace or before executing implementation plans - creates isolated git worktrees with smart directory selection and safety verification
---

# 使用 Git Worktrees

## 概述

Git worktrees 创建共享同一仓库的隔离工作空间，允许同时在多个分支上工作而无需切换。

**核心原则：** 系统化目录选择 + 安全验证 = 可靠隔离。

**开始时声明：** "我正在使用 using-git-worktrees 技能来设置隔离工作空间。"

## 目录选择流程

遵循以下优先级顺序：

### 1. 检查现有目录

```bash
# 按优先级顺序检查
ls -d .worktrees 2>/dev/null     # 首选（隐藏）
ls -d worktrees 2>/dev/null      # 备选
```

**如果找到：** 使用该目录。如果两者都存在，`.worktrees` 优先。

### 2. 检查 AGENTS.md

```bash
grep -i "worktree.*director" AGENTS.md 2>/dev/null
```

**如果指定了偏好：** 直接使用，无需询问。

### 3. 询问用户

如果没有目录存在且 AGENTS.md 中无偏好设置：

```
未找到 worktree 目录。我应该在哪里创建 worktrees？

1. .worktrees/（项目本地，隐藏）
2. ~/.config/superpowers/worktrees/<项目名称>/（全局位置）

您更喜欢哪一个？
```

## 安全验证

### 对于项目本地目录（.worktrees 或 worktrees）

**必须在创建 worktree 前验证目录已被忽略：**

```bash
# 检查目录是否被忽略（尊重本地、全局和系统 gitignore）
git check-ignore -q .worktrees 2>/dev/null || git check-ignore -q worktrees 2>/dev/null
```

**如果未被忽略：**

按照 Jesse 的规则"立即修复损坏的东西"：
1. 将适当的行添加到 .gitignore
2. 提交更改
3. 继续创建 worktree

**为什么关键：** 防止意外将 worktree 内容提交到仓库。

### 对于全局目录（~/.config/superpowers/worktrees）

无需 .gitignore 验证 - 完全在项目之外。

## 创建步骤

### 1. 检测项目名称

```bash
project=$(basename "$(git rev-parse --show-toplevel)")
```

### 2. 创建 Worktree

```bash
# 确定完整路径
case $LOCATION in
  .worktrees|worktrees)
    path="$LOCATION/$BRANCH_NAME"
    ;;
  ~/.config/superpowers/worktrees/*)
    path="~/.config/superpowers/worktrees/$project/$BRANCH_NAME"
    ;;
esac

# 使用新分支创建 worktree
git worktree add "$path" -b "$BRANCH_NAME"
cd "$path"
```

### 3. 运行项目设置

自动检测并运行适当的设置：

```bash
# Node.js
if [ -f package.json ]; then npm install; fi

# Rust
if [ -f Cargo.toml ]; then cargo build; fi

# Python
if [ -f requirements.txt ]; then pip install -r requirements.txt; fi
if [ -f pyproject.toml ]; then poetry install; fi

# Go
if [ -f go.mod ]; then go mod download; fi
```

### 4. 验证干净基线

运行测试以确保 worktree 从干净状态开始：

```bash
# 示例 - 使用项目适当的命令
npm test
cargo test
pytest
go test ./...
```

**如果测试失败：** 报告失败，询问是继续还是调查。

**如果测试通过：** 报告准备就绪。

### 5. 报告位置

```
Worktree 准备就绪，位于 <完整路径>
测试通过（<N> 个测试，0 个失败）
准备实现 <功能名称>
```

## 快速参考

| 情况 | 操作 |
|-----------|--------|
| `.worktrees/` 存在 | 使用它（验证已被忽略） |
| `worktrees/` 存在 | 使用它（验证已被忽略） |
| 两者都存在 | 使用 `.worktrees/` |
| 两者都不存在 | 检查 AGENTS.md → 询问用户 |
| 目录未被忽略 | 添加到 .gitignore + 提交 |
| 基线测试期间测试失败 | 报告失败 + 询问 |
| 没有 package.json/Cargo.toml | 跳过依赖安装 |

## 常见错误

### 跳过忽略验证

- **问题：** Worktree 内容被跟踪，污染 git 状态
- **修复：** 在创建项目本地 worktree 前始终使用 `git check-ignore`

### 假设目录位置

- **问题：** 造成不一致，违反项目约定
- **修复：** 遵循优先级：现有 > AGENTS.md > 询问

### 在测试失败时继续

- **问题：** 无法区分新 bug 与预先存在的问题
- **修复：** 报告失败，获得明确许可后再继续

### 硬编码设置命令

- **问题：** 在使用不同工具的项目上中断
- **修复：** 从项目文件自动检测（package.json 等）

## 示例工作流程

```
您：我正在使用 using-git-worktrees 技能来设置隔离工作空间。

[检查 .worktrees/ - 存在]
[验证已被忽略 - git check-ignore 确认 .worktrees/ 已被忽略]
[创建 worktree：git worktree add .worktrees/auth -b feature/auth]
[运行 npm install]
[运行 npm test - 47 个通过]

Worktree 准备就绪，位于 /Users/jesse/myproject/.worktrees/auth
测试通过（47 个测试，0 个失败）
准备实现 auth 功能
```

## 危险信号

**永远不要：**
- 在未验证已被忽略的情况下创建 worktree（项目本地）
- 跳过基线测试验证
- 在测试失败时未经询问就继续
- 在位置不明确时假设目录位置
- 跳过 AGENTS.md 检查
- **直接在 main/master 分支上工作**（改为创建功能分支）
- **跳过推送提交到远程**（每次提交后都要推送）
- **在测试失败时合并到 main/master**（先修复测试）
- **从 main/master 创建 PR**（始终使用单独的分支）

**始终：**
- 遵循目录优先级：现有 > AGENTS.md > 询问
- 验证项目本地目录已被忽略
- 自动检测并运行项目设置
- 验证干净测试基线
- **为所有变更创建新分支**（通过 `git worktree add -b`）
- **每次提交后推送**（备份 + 可见性）
- **合并前验证测试通过**（保持 main/master 稳定）
- **使用单独的分支创建 PR**（防止后续更改污染 PR）

## 提交规范

Worktree 准备就绪后,遵循以下提交规范:

### 分支策略

**关键: 始终为变更创建新分支。永远不要直接在 main/master 上工作，除非获得明确批准。**

```bash
# Worktree 已通过 `git worktree add -b <分支名称>` 创建新分支
# 这是正确的模式 - 每个功能/修复都有自己的分支
```

**为什么强制要求:**
- 保持 main/master 稳定且可生产部署
- 允许进行中的工作不阻塞他人
- 实现干净的 PR 工作流，不会被后续变更污染
- 使回滚安全（可以删除分支而不影响 main）

### 提交粒度

- **为每个逻辑工作单元创建提交**
  - 每个提交一个功能/修复
  - 保持提交专注且原子化
  - 避免合并无关更改

### 提交信息

**遵循项目现有约定：**

1. **检查现有风格：**
   ```bash
   git log --oneline -20
   ```

2. **使用您观察到的模式：**

   | 项目类型 | 约定 | 示例 |
   |--------------|------------|---------|
   | **版本化库/应用** | 语义化风格 | `feat: add JWT auth`, `fix: handle null user` |
   | **Dotfiles、配置、脚本** | 纯描述性 | `Add dark mode to vimrc`, `Fix typo in bashrc` |
   | **混合/不明确** | 询问用户遵循哪个 | — |

3. **编写描述性信息：**
   - 第一行：简洁摘要（最多 50 个字符）
   - 正文（可选）：解释原因，而非内容
   - 如有相关议题/工单，请引用

### 提交后推送

**必需: 每次提交后推送到远程（如果远程存在）**

```bash
# 检查是否配置了远程
git remote -v

# 每次提交后推送
git push origin <分支名称>

# 首次推送需要 -u 标志
git push -u origin <分支名称>
```

**为什么强制要求:**
- 立即备份工作（防止数据丢失）
- 让团队看到进度
- 实现早期反馈
- 在进一步更改前创建备份
- 允许创建干净的 PR，不会被后续更改污染

**频率: 每次提交后都要推送，而不仅仅是任务结束时。**

### 合并规范

**关键: 只在所有验证通过后才合并到 main/master:**

1. ✅ 所有测试通过
2. ✅ 代码审查通过（如果适用）
3. ✅ 构建成功（如果适用）
4. ✅ 集成测试通过（如果适用）

**永远不要将有问题的代码合并到 main/master。**

### PR 工作流

**创建 Pull Request 时:**

1. **始终使用单独的分支**（已由 worktree 完成）
2. **在创建 PR 前推送分支**（不是 main/master）
3. **从分支 → main/master 创建 PR**（PR 创建后永远不要直接提交到 main）
4. **好处:** main 上的后续更改不会污染此 PR 的变更集

### 历史管理

**开始工作前：**
```bash
# 检查远程更新
git fetch origin
git status  # 检查分支是否落后
```

**回滚操作：**
```bash
# 保留历史（首选）
git revert <提交>

# 撤销提交但保留暂存更改（谨慎使用）
git reset --soft HEAD~1
```

**关键原则：** Git 历史应该清晰地讲述发生了什么变化以及为什么。

## 集成

**被以下技能调用：**
- **brainstorming**（第 4 阶段）- 设计获得批准且将实施时必需
- **subagent-driven-development** - 执行任何任务前必需
- **executing-plans** - 执行任何任务前必需
- 任何需要隔离工作空间的技能

**与以下技能配对：**
- **finishing-a-development-branch** - 工作完成后清理必需
