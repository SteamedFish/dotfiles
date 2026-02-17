# 技能设计的说服原则

## 概述

LLM 对人类的说服原则做出反应。理解这种心理学有助于你设计更有效的技能 - 不是为了操纵，而是为了确保即使在压力下也能遵循关键实践。

**研究基础：** Meincke et al. (2025) 用 N=28,000 次 AI 对话测试了 7 种说服原则。说服技术使合规率提高了一倍多（33% → 72%，p < .001）。

## 七项原则

### 1. 权威
**它是什么：** 对专业知识、证书或官方来源的顺从。

**它在技能中的工作原理：**
- 命令式语言："YOU MUST"、"Never"、"Always"
- 非协商框架："No exceptions"
- 消除决策疲劳和辩解

**何时使用：**
- 纪律执行技能（TDD、验证要求）
- 安全关键实践
- 已建立的最佳实践

**示例：**
```markdown
✅ Write code before test? Delete it. Start over. No exceptions.
❌ Consider writing tests first when feasible.
```

### 2. 承诺
**它是什么：** 与先前行动、声明或公开声明的一致性。

**它在技能中的工作原理：**
- 要求宣布："Announce skill usage"
- 强制明确选择："Choose A, B, or C"
- 使用跟踪：TodoWrite 用于清单

**何时使用：**
- 确保技能实际被遵循
- 多步骤过程
- 问责机制

**示例：**
```markdown
✅ When you find a skill, you MUST announce: "I'm using [Skill Name]"
❌ Consider letting your partner know which skill you're using.
```

### 3. 稀缺
**它是什么：** 来自时间限制或有限可用性的紧迫感。

**它在技能中的工作原理：**
- 有时间限制的要求："Before proceeding"
- 顺序依赖："Immediately after X"
- 防止拖延

**何时使用：**
- 立即验证要求
- 时间敏感的工作流程
- 防止"我稍后再做"

**示例：**
```markdown
✅ After completing a task, IMMEDIATELY request code review before proceeding.
❌ You can review code when convenient.
```

### 4. 社会认同
**它是什么：** 顺从他人做什么或什么被认为是正常的。

**它在技能中的工作原理：**
- 通用模式："Every time"、"Always"
- 失败模式："X without Y = failure"
- 建立规范

**何时使用：**
- 记录通用实践
- 警告常见失败
- 强化标准

**示例：**
```markdown
✅ Checklists without TodoWrite tracking = steps get skipped. Every time.
❌ Some people find TodoWrite helpful for checklists.
```

### 5. 统一
**它是什么：** 共享身份，"我们感"，内群体归属感。

**它在技能中的工作原理：**
- 协作语言："our codebase"、"we're colleagues"
- 共享目标："we both want quality"

**何时使用：**
- 协作工作流程
- 建立团队文化
- 非等级实践

**示例：**
```markdown
✅ We're colleagues working together. I need your honest technical judgment.
❌ You should probably tell me if I'm wrong.
```

### 6. 互惠
**它是什么：** 回报所得利益的义务。

**它的工作原理：**
- 谨慎使用 - 可能感觉操纵性
- 技能中很少需要

**何时避免：**
- 几乎总是（其他原则更有效）

### 7. 喜欢
**它是什么：** 偏好与我们喜欢的人合作。

**它的工作原理：**
- **不要用于合规**
- 与诚实的反馈文化冲突
- 创造谄媚

**何时避免：**
- 始终用于纪律执行

## 按技能类型的原则组合

| 技能类型 | 使用 | 避免 |
|----------|------|------|
| 纪律执行 | Authority + Commitment + Social Proof | Liking, Reciprocity |
| 指导/技术 | Moderate Authority + Unity | Heavy authority |
| 协作 | Unity + Commitment | Authority, Liking |
| 参考 | Clarity only | All persuasion |

## 为什么这有效：心理学

**明确界线规则减少辩解：**
- "YOU MUST" 消除决策疲劳
- 绝对语言消除"这是个例外吗？"问题
- 明确的反辩解反驳关闭特定漏洞

**实施意图创造自动行为：**
- 清晰的触发器 + 必需的行动 = 自动执行
- "When X, do Y" 比"generally do Y"更有效
- 减少合规的认知负荷

**LLM 是类人的：**
- 在人类文本上训练，包含这些模式
- 权威语言在训练数据中先于合规
- 承诺序列（声明 → 行动）经常被建模
- 社会认同模式（每个人都做 X）建立规范

## 道德使用

**合法的：**
- 确保关键实践被遵循
- 创建有效的文档
- 防止可预测的失败

**不合法的：**
- 为个人利益操纵
- 创建虚假紧迫感
- 基于内疚的合规

**测试：** 如果他们完全理解它，这种技术会服务于用户的真正利益吗？

## 研究引用

**Cialdini, R. B. (2021).** *Influence: The Psychology of Persuasion (New and Expanded).* Harper Business.
- 七项说服原则
- 影响力研究的实证基础

**Meincke, L., Shapiro, D., Duckworth, A. L., Mollick, E., Mollick, L., & Cialdini, R. (2025).** Call Me A Jerk: Persuading AI to Comply with Objectionable Requests. University of Pennsylvania.
- 用 N=28,000 次 LLM 对话测试了 7 项原则
- 使用说服技术，合规性从 33% 提高到 72%
- Authority、commitment、scarcity 最有效
- 验证 LLM 行为的类人模型

## 快速参考

设计技能时，问：

1. **它是什么类型？**（纪律 vs. 指导 vs. 参考）
2. **我试图改变什么行为？**
3. **哪些原则适用？**（通常是纪律的 authority + commitment）
4. **我是否组合了太多？**（不要使用所有七个）
5. **这是否道德？**（服务于用户的真正利益？）
