---
name: finishing-a-development-branch
description: Use when implementation is complete, all tests pass, and you need to decide how to integrate the work - guides completion of development work by presenting structured options for merge, PR, or cleanup
---

# 完成开发分支

## 概述

通过提供清晰选项并处理选定的工作流来指导开发工作的完成。

**核心原则：** 验证测试 → 提供选项 → 执行选择 → 清理。

**开始时声明：** "我正在使用 finishing-a-development-branch 技能来完成这项工作。"

## 流程

### 步骤 1：验证测试

**在提供选项之前，先验证测试是否通过：**

```bash
# 运行项目的测试套件
npm test / cargo test / pytest / go test ./...
```

**如果测试失败：**
```
测试失败（<N> 个失败）。完成前必须修复：

[显示失败]

在测试通过之前无法继续合并/PR。
```

停止。不要进入步骤 2。

**如果测试通过：** 继续到步骤 2。

### 步骤 2：确定基础分支

```bash
# 尝试常见的基础分支
git merge-base HEAD main 2>/dev/null || git merge-base HEAD master 2>/dev/null
```

或者询问："此分支从 main 分出的 - 正确吗？"

### 步骤 3：提供选项

准确提供以下 4 个选项：

```
实现已完成。您想怎么做？

1. 本地合并回 <base-branch>
2. 推送并创建 Pull Request
3. 保持分支不变（我稍后处理）
4. 放弃此工作

选择哪个选项？
```

**不要添加解释** - 保持选项简洁。

### 步骤 4：执行选择

#### 选项 1：本地合并

**关键: 只在所有测试通过后才合并。永远不要将有问题的代码合并到 main/master。**

```bash
# 切换到基础分支
git checkout <base-branch>

# 拉取最新代码
git pull

# 合并功能分支
git merge <feature-branch>

# 必需: 验证合并结果的测试
<test command>

# 如果测试失败 - 不要继续
# 切换回功能分支并修复
git checkout <feature-branch>
# 修复问题，提交，推送
# 从选项 1 的开头重新开始

# 如果测试通过 - 可以安全完成
git branch -d <feature-branch>

# 推送合并结果到远程
git push origin <base-branch>
```

**危险信号:** 如果合并后测试失败，合并为时过早。先在功能分支上修复。

然后：清理工作树（步骤 5）

#### 选项 2：推送并创建 PR

**关键: 始终从单独的分支创建 PR，永远不要从 main/master。**

```bash
# 验证你在功能分支上（不是 main/master）
current_branch=$(git branch --show-current)
if [[ "$current_branch" == "main" || "$current_branch" == "master" ]]; then
    echo "错误: 不能从 main/master 创建 PR。请先创建功能分支。"
    exit 1
fi

# 推送分支（如果尚未推送）
git push -u origin <feature-branch>

# 从功能分支 → 基础分支创建 PR
gh pr create --title "<title>" --body "$(cat <<'EOF'
## 摘要
<2-3 个变更要点>

## 测试计划
- [ ] <验证步骤>
EOF
)"
```

**为什么强制使用单独的分支:**
- 后续对 main/master 的提交不会污染此 PR 的变更
- PR 只显示功能相关的变更，而不是后续工作
- 工作项之间的清晰分离

**PR 创建后:**
- 在 main/master 上继续其他任务
- 此 PR 保持干净，只包含功能相关的提交
- 批准后合并 PR（通过 GitHub UI 或 `gh pr merge`）

然后：清理工作树（步骤 5）

#### 选项 3：保持原样

报告："保留分支 <name>。工作树保留在 <path>。"

**不要清理工作树。**

#### 选项 4：放弃

**先确认：**
```
这将永久删除：
- 分支 <name>
- 所有提交：<commit-list>
- 位于 <path> 的工作树

输入 'discard' 确认。
```

等待准确确认。

如果确认：
```bash
git checkout <base-branch>
git branch -D <feature-branch>
```

然后：清理工作树（步骤 5）

### 步骤 5：清理工作树

**对于选项 1、2、4：**

检查是否在工作树中：
```bash
git worktree list | grep $(git branch --show-current)
```

如果是：
```bash
git worktree remove <worktree-path>
```

**对于选项 3：** 保留工作树。

## 快速参考

| 选项 | 合并 | 推送 | 保留工作树 | 清理分支 | 需要测试 |
|------|------|------|------------|----------|----------|
| 1. 本地合并 | ✓ | ✓ | - | ✓ | ✅ 合并前后都需要 |
| 2. 创建 PR | - | ✓ | ✓ | - | ✅ PR 前需要 |
| 3. 保持原样 | - | - | ✓ | - | - |
| 4. 放弃 | - | - | - | ✓ (强制) | - |

## 常见错误

**跳过测试验证**
- **问题：** 合并不正常的代码，创建失败的 PR
- **修复：** 始终在提供选项前验证测试，合并后也要验证

**在测试失败时合并**
- **问题：** main/master 中的损坏代码会破坏生产环境
- **修复：** 如果合并后测试失败，切换回功能分支，修复，重试

**从 main/master 分支创建 PR**
- **问题：** 后续对 main/master 的提交会污染 PR 的变更
- **修复：** 始终从单独的功能分支创建 PR（永远不要从 main/master）

**开放式问题**
- **问题：** "下一步应该做什么？" → 模糊不清
- **修复：** 准确提供 4 个结构化选项

**自动清理工作树**
- **问题：** 在可能需要时移除工作树（选项 2、3）
- **修复：** 仅对选项 1 和 4 清理工作树

**放弃时不确认**
- **问题：** 意外删除工作
- **修复：** 要求输入 "discard" 确认

## 危险信号

**绝不：**
- 在测试失败时继续
- 不验证合并结果的测试就合并
- 不经确认就删除工作
- 未经明确请求就强制推送
- **在测试失败时合并到 main/master**（先修复，然后合并）
- **从 main/master 分支创建 PR**（使用功能分支）
- **跳过合并后测试验证**（选项 1 需要合并后测试）

**始终：**
- 在提供选项前验证测试
- 准确提供 4 个选项
- 对选项 4 获取文字确认
- 仅对选项 1 和 4 清理工作树
- **合并后运行测试**（选项 1 - 验证合并没有破坏任何东西）
- **在创建 PR 前验证功能分支**（选项 2 - 必须使用分支）
- **推送合并的 main/master 到远程**（选项 1 - 备份变更）

## 集成

**由以下调用：**
- **subagent-driven-development**（步骤 7）- 所有任务完成后
- **executing-plans**（步骤 5）- 所有批次完成后

**配合以下使用：**
- **using-git-worktrees** - 清理由该技能创建的工作树
