---
name: writing-plans
description: Use when you have a spec or requirements for a multi-step task, before touching code
---

# 编写计划

## 概述

编写全面的实施计划，假设工程师对我们的代码库一无所知且品味存疑。记录他们需要知道的一切：每个任务要修改哪些文件、代码、需要检查的测试和文档、如何测试。将整个计划分解为细粒度的任务。DRY（不要重复自己）。YAGNI（你不会需要它）。TDD（测试驱动开发）。频繁提交。

假设他们是熟练的开发者，但几乎不了解我们的工具集或问题域。假设他们不太擅长设计良好的测试。

**开始时声明：**"我正在使用 writing-plans 技能来创建实施计划。"

**上下文：** 这应该在专门的工作树中运行（由 brainstorming 技能创建）。

**保存计划至：** `docs/plans/YYYY-MM-DD-<feature-name>.md`

## 规划前需理解

**在编写计划之前，先理解问题域：**

### 研究检查清单（5-10 分钟）

- [ ] **阅读项目文档** - 检查 `plan/TODO.md`、`plan/CHANGELOG.md` 或项目 `AGENTS.md` 获取上下文
- [ ] **审查现有实现** - 搜索类似功能以避免重复造轮子
- [ ] **搜索工具/辅助函数** - 在规划新功能之前查找现有的验证、错误处理、格式化函数
- [ ] **理解约束** - 识别技术边界、性能要求、兼容性需求
- [ ] **追踪数据路径** - 理解数据如何在系统中流动
- [ ] **澄清模糊点** - 现在就向你的伙伴询问不清楚的需求，而不是在实施过程中

**为什么这很重要：** 没有上下文的规划 = 编写重复代码、违反约束或解决错误问题的任务。

**示例：**
```markdown
❌ 错误：直接跳到规划
任务 1：创建邮箱验证函数
  - 编写 validateEmail(email: string): boolean
  - 添加邮箱格式的正则表达式模式

# 代码库中已存在 EmailValidator 类在 src/utils/validators/！

✅ 正确：经过 5 分钟研究后，找到现有验证器
任务 1：为扩展需求修改 EmailValidator
  - 修改：src/utils/validators/EmailValidator.ts
  - 向现有验证器添加域名白名单检查
  - 复用现有的邮箱格式验证
```

**需要立即澄清的危险信号：**
- 有多种实现方式，各有不同的权衡
- 需求或成功标准不清晰
- 缺少现有系统的信息
- 约束条件相互矛盾

## 细粒度任务粒度

**每个步骤是一个动作（2-5 分钟）：**
- "编写失败的测试" - 步骤
- "运行它确保失败" - 步骤
- "实现最小代码使测试通过" - 步骤
- "运行测试确保通过" - 步骤
- "提交" - 步骤

## 计划文档头部

**每个计划必须以这个头部开始：**

```markdown
# [功能名称] 实施计划

> **必需子技能：**使用 executing-plans 逐任务实施此计划。

**目标：** [一句话描述要构建的内容]

**架构：** [2-3 句话描述方案]

**技术栈：** [关键技术/库]

---
```

## 任务结构

```markdown
### 任务 N：[组件名称]

**文件：**
- 创建：`exact/path/to/file.py`
- 修改：`exact/path/to/existing.py:123-145`
- 测试：`tests/exact/path/to/test.py`

**步骤 1：编写失败的测试**

```python
def test_specific_behavior():
    result = function(input)
    assert result == expected
```

**步骤 2：运行测试验证失败**

运行：`pytest tests/path/test.py::test_name -v`
预期：FAIL with "function not defined"

**步骤 3：编写最小实现**

```python
def function(input):
    return expected
```

**步骤 4：运行测试验证通过**

运行：`pytest tests/path/test.py::test_name -v`
预期：PASS

**步骤 5：提交**

```bash
git add tests/path/test.py src/path/file.py
git commit -m "feat: add specific feature"
```
```

## 记住
- 始终使用确切的文件路径
- 计划中包含完整代码（不是"添加验证"）
- 使用带预期输出的确切命令
- 使用 @ 语法引用相关技能
- DRY、YAGNI、TDD、频繁提交

## 执行交接

保存计划后，提供执行选择：

**"计划已完成并保存至 `docs/plans/<filename>.md`。两种执行选项：**

**1. 子代理驱动（本次会话）** - 我为每个任务分派新的子代理，任务间进行审查，快速迭代

**2. 并行会话（单独会话）** - 使用 executing-plans 打开新会话，批量执行并带检查点

**选择哪种方式？"**

**如果选择子代理驱动：**
- **必需子技能：** 使用 subagent-driven-development
- 保持在本会话
- 每个任务使用新子代理 + 代码审查

**如果选择并行会话：**
- 引导他们在工作树中打开新会话
- **必需子技能：** 新会话使用 executing-plans
