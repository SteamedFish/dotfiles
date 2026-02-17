# 技能编写最佳实践

> 学习如何编写有效的技能，让 Claude 能够成功发现和使用。

好的技能简洁、结构良好，并通过实际使用进行测试。本指南提供实用的编写决策，帮助你编写 Claude 能够有效发现和使用的技能。

关于技能如何工作的概念背景，请参阅 [技能概述](/en/docs/agents-and-tools/agent-skills/overview)。

## 核心原则

### 简洁是关键

[上下文窗口](https://platform.claude.com/docs/en/build-with-claude/context-windows) 是一种公共产品。你的技能与 Claude 需要知道的其他所有内容共享上下文窗口，包括：

* 系统提示
* 对话历史
* 其他技能的元数据
* 你的实际请求

并非技能中的每个令牌都有即时成本。启动时，只有所有技能的元数据（名称和描述）被预加载。Claude 仅在技能变得相关时读取 SKILL.md，并仅在需要时读取其他文件。然而，在 SKILL.md 中保持简洁仍然很重要：一旦 Claude 加载它，每个令牌都会与对话历史和其他上下文竞争。

**默认假设**：Claude 已经非常聪明

只添加 Claude 尚未拥有的上下文。质疑每条信息：

* "Claude 真的需要这个解释吗？"
* "我能假设 Claude 知道这个吗？"
* "这段话证明了它的令牌成本吗？"

**好示例：简洁**（大约 50 个令牌）：

````markdown  theme={null}
## Extract PDF text

Use pdfplumber for text extraction:

```python
import pdfplumber

with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```
````

**坏示例：太冗长**（大约 150 个令牌）：

```markdown  theme={null}
## Extract PDF text

PDF (Portable Document Format) files are a common file format that contains
text, images, and other content. To extract text from a PDF, you'll need to
use a library. There are many libraries available for PDF processing, but we
recommend pdfplumber because it's easy to use and handles most cases well.
First, you'll need to install it using pip. Then you can use the code below...
```

简洁版本假设 Claude 知道 PDF 是什么以及库如何工作。

### 设置适当的自由度

将特定性水平与任务的脆弱性和可变性相匹配。

**高自由度**（基于文本的指令）：

在以下情况使用：

* 多种方法都有效
* 决策取决于上下文
* 启发式指导方法

示例：

```markdown  theme={null}
## Code review process

1. Analyze the code structure and organization
2. Check for potential bugs or edge cases
3. Suggest improvements for readability and maintainability
4. Verify adherence to project conventions
```

**中等自由度**（伪代码或带参数的脚本）：

在以下情况使用：

* 存在首选模式
* 一些变化是可接受的
* 配置影响行为

示例：

````markdown  theme={null}
## Generate report

Use this template and customize as needed:

```python
def generate_report(data, format="markdown", include_charts=True):
    # Process data
    # Generate output in specified format
    # Optionally include visualizations
```
````

**低自由度**（特定脚本，很少或没有参数）：

在以下情况使用：

* 操作脆弱且容易出错
* 一致性至关重要
* 必须遵循特定序列

示例：

````markdown  theme={null}
## Database migration

Run exactly this script:

```bash
python scripts/migrate.py --verify --backup
```

Do not modify the command or add additional flags.
````

**类比**：将 Claude 想象成探索路径的机器人：

* **两侧悬崖的窄桥**：只有一种安全的前进方式。提供特定的护栏和精确指令（低自由度）。示例：必须按确切序列运行的数据库迁移。
* **没有危险的开放场地**：许多路径通向成功。给出一般方向，相信 Claude 会找到最佳路线（高自由度）。示例：上下文决定最佳方法的代码审查。

### 用你计划使用的所有模型进行测试

技能作为模型的补充，因此有效性取决于底层模型。用你计划使用的所有模型测试你的技能。

**按模型的测试考虑**：

* **Claude Haiku**（快速、经济）：技能是否提供了足够的指导？
* **Claude Sonnet**（平衡）：技能是否清晰高效？
* **Claude Opus**（强大的推理）：技能是否避免了过度解释？

对 Opus 完美的东西可能需要更多细节才能让 Haiku 理解。如果你计划在多个模型中使用你的技能，目标是适用于所有模型的指令。

## 技能结构

<注意>
  **YAML Frontmatter**：SKILL.md frontmatter 支持两个字段：

  * `name` - 技能的人类可读名称（最多 64 个字符）
  * `description` - 技能功能的单行描述及何时使用（最多 1024 个字符）

  有关完整的技能结构详细信息，请参阅 [技能概述](/en/docs/agents-and-tools/agent-skills/overview#skill-structure)。
</注意>

### 命名约定

使用一致的命名模式，使技能更容易引用和讨论。我们建议使用**动名词形式**（动词 + -ing）作为技能名称，因为这清楚地描述了技能提供的活动或能力。

**好的命名示例（动名词形式）**：

* "Processing PDFs"
* "Analyzing spreadsheets"
* "Managing databases"
* "Testing code"
* "Writing documentation"

**可接受的替代方案**：

* 名词短语："PDF Processing"、"Spreadsheet Analysis"
* 面向行动："Process PDFs"、"Analyze Spreadsheets"

**避免**：

* 模糊名称："Helper"、"Utils"、"Tools"
* 过于通用："Documents"、"Data"、"Files"
* 技能集合内不一致的模式

一致的命名使以下操作更容易：

* 在文档和对话中引用技能
* 一眼理解技能的功能
* 组织和搜索多个技能
* 维护专业、有凝聚力的技能库

### 编写有效的描述

`description` 字段支持技能发现，应包括技能的功能和何时使用。

<警告>
  **始终用第三人称写作**。描述被注入系统提示，不一致的观点会导致发现问题。

  * **好：** "Processes Excel files and generates reports"
  * **避免：** "I can help you process Excel files"
  * **避免：** "You can use this to process Excel files"
</警告>

**具体并包含关键术语**。包括技能的功能以及何时使用的具体触发器/上下文。

每个技能只有一个描述字段。描述对技能选择至关重要：Claude 使用它从潜在的 100+ 可用技能中选择正确的技能。你的描述必须提供足够的细节让 Claude 知道何时选择此技能，而 SKILL.md 的其余部分提供实现细节。

有效示例：

**PDF 处理技能：**

```yaml  theme={null}
description: Extract text and tables from PDF files, fill forms, merge documents. Use when working with PDF files or when the user mentions PDFs, forms, or document extraction.
```

**Excel 分析技能：**

```yaml  theme={null}
description: Analyze Excel spreadsheets, create pivot tables, generate charts. Use when analyzing Excel files, spreadsheets, tabular data, or .xlsx files.
```

**Git 提交助手技能：**

```yaml  theme={null}
description: Generate descriptive commit messages by analyzing git diffs. Use when the user asks for help writing commit messages or reviewing staged changes.
```

避免像这样的模糊描述：

```yaml  theme={null}
description: Helps with documents
```

```yaml  theme={null}
description: Processes data
```

```yaml  theme={null}
description: Does stuff with files
```

### 渐进式披露模式

SKILL.md 作为概述，根据需要指向详细材料，就像入职指南中的目录一样。有关渐进式披露如何工作的解释，请参阅概述中的 [技能如何工作](/en/docs/agents-and-tools/agent-skills/overview#how-skills-work)。

**实用指南：**

* 为获得最佳性能，将 SKILL.md 正文保持在 500 行以下
* 当接近此限制时，将内容拆分为单独的文件
* 使用以下模式有效组织指令、代码和资源

#### 视觉概述：从简单到复杂

一个基本的技能从仅包含元数据和指令的 SKILL.md 文件开始：

<img src="https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=87782ff239b297d9a9e8e1b72ed72db9" alt="Simple SKILL.md file showing YAML frontmatter and markdown body" data-og-width="2048" width="2048" data-og-height="1153" height="1153" data-path="images/agent-skills-simple-file.png" data-optimize="true" data-opv="3" srcset="https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?w=280&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=c61cc33b6f5855809907f7fda94cd80e 280w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?w=560&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=90d2c0c1c76b36e8d485f49e0810dbfd 560w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?w=840&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=ad17d231ac7b0bea7e5b4d58fb4aeabb 840w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?w=1100&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=f5d0a7a3c668435bb0aee9a3a8f8c329 1100w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?w=1650&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=0e927c1af9de5799cfe557d12249f6e6 1650w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-simple-file.png?w=2500&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=46bbb1a51dd4c8202a470ac8c80a893d 2500w" />

随着技能的增长，你可以捆绑仅在需要时加载的额外内容：

<img src="https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=a5e0aa41e3d53985a7e3e43668a33ea3" alt="Bundling additional reference files like reference.md and forms.md." data-og-width="2048" width="2048" data-og-height="1327" height="1327" data-path="images/agent-skills-bundling-content.png" data-optimize="true" data-opv="3" srcset="https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?w=280&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=f8a0e73783e99b4a643d79eac86b70a2 280w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?w=560&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=dc510a2a9d3f14359416b706f067904a 560w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?w=840&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=82cd6286c966303f7dd914c28170e385 840w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?w=1100&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=56f3be36c77e4fe4b523df209a6824c6 1100w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?w=1650&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=d22b5161b2075656417d56f41a74f3dd 1650w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-bundling-content.png?w=2500&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=3dd4bdd6850ffcc96c6c45fcb0acd6eb 2500w" />

完整的技能目录结构可能如下所示：

```
pdf/
├── SKILL.md              # 主要指令（触发时加载）
├── FORMS.md              # 表单填写指南（按需加载）
├── reference.md          # API 参考（按需加载）
├── examples.md           # 使用示例（按需加载）
└── scripts/
    ├── analyze_form.py   # 工具脚本（执行，不加载）
    ├── fill_form.py      # 表单填写脚本
    └── validate.py       # 验证脚本
```

#### 模式 1：带参考的高级指南

````markdown  theme={null}
---
name: PDF Processing
description: Extracts text and tables from PDF files, fills forms, and merges documents. Use when working with PDF files or when the user mentions PDFs, forms, or document extraction.
---

# PDF Processing

## Quick start

Extract text with pdfplumber:
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```

## Advanced features

**Form filling**: See [FORMS.md](FORMS.md) for complete guide
**API reference**: See [REFERENCE.md](REFERENCE.md) for all methods
**Examples**: See [EXAMPLES.md](EXAMPLES.md) for common patterns
````

Claude 仅在需要时加载 FORMS.md、REFERENCE.md 或 EXAMPLES.md。

#### 模式 2：按领域组织

对于具有多个领域的技能，按领域组织内容以避免加载不相关的上下文。当用户询问销售指标时，Claude 只需要阅读与销售相关的模式，而不是财务或营销数据。这保持令牌使用率低且上下文集中。

```
bigquery-skill/
├── SKILL.md (overview and navigation)
└── reference/
    ├── finance.md (revenue, billing metrics)
    ├── sales.md (opportunities, pipeline)
    ├── product.md (API usage, features)
    └── marketing.md (campaigns, attribution)
```

````markdown SKILL.md theme={null}
# BigQuery Data Analysis

## Available datasets

**Finance**: Revenue, ARR, billing → See [reference/finance.md](reference/finance.md)
**Sales**: Opportunities, pipeline, accounts → See [reference/sales.md](reference/sales.md)
**Product**: API usage, features, adoption → See [reference/product.md](reference/product.md)
**Marketing**: Campaigns, attribution, email → See [reference/marketing.md](reference/marketing.md)

## Quick search

Find specific metrics using grep:

```bash
grep -i "revenue" reference/finance.md
grep -i "pipeline" reference/sales.md
grep -i "api usage" reference/product.md
```
````

#### 模式 3：条件细节

显示基本内容，链接到高级内容：

```markdown  theme={null}
# DOCX Processing

## Creating documents

Use docx-js for new documents. See [DOCX-JS.md](DOCX-JS.md).

## Editing documents

For simple edits, modify the XML directly.

**For tracked changes**: See [REDLINING.md](REDLINING.md)
**For OOXML details**: See [OOXML.md](OOXML.md)
```

Claude 仅在用户需要这些功能时阅读 REDLINING.md 或 OOXML.md。

### 避免深度嵌套引用

当从其他引用文件引用文件时，Claude 可能会部分阅读文件。遇到嵌套引用时，Claude 可能会使用 `head -100` 等命令预览内容，而不是阅读整个文件，导致信息不完整。

**保持引用从 SKILL.md 深度一级**。所有参考文件应直接从 SKILL.md 链接，以确保 Claude 在需要时阅读完整文件。

**坏示例：太深**：

```markdown  theme={null}
# SKILL.md
See [advanced.md](advanced.md)...

# advanced.md
See [details.md](details.md)...

# details.md
Here's the actual information...
```

**好示例：深度一级**：

```markdown  theme={null}
# SKILL.md

**Basic usage**: [instructions in SKILL.md]
**Advanced features**: See [advanced.md](advanced.md)
**API reference**: See [reference.md](reference.md)
**Examples**: See [examples.md](examples.md)
```

### 用目录结构较长的参考文件

对于超过 100 行的参考文件，在顶部包含目录。这确保即使在使用部分阅读预览时，Claude 也能看到可用信息的完整范围。

**示例**：

```markdown  theme={null}
# API Reference

## Contents
- Authentication and setup
- Core methods (create, read, update, delete)
- Advanced features (batch operations, webhooks)
- Error handling patterns
- Code examples

## Authentication and setup
...

## Core methods
...
```

然后 Claude 可以根据需要阅读完整文件或跳转到特定部分。

有关此基于文件系统的架构如何实现渐进式披露的详细信息，请参阅下面高级部分的 [运行时环境](#runtime-environment) 部分。

## 工作流程和反馈循环

### 对复杂任务使用工作流程

将复杂操作分解为清晰、顺序的步骤。对于特别复杂的工作流程，提供一个 Claude 可以复制到其响应中并在进行时勾选的清单。

**示例 1：研究综合工作流程**（适用于没有代码的技能）：

````markdown  theme={null}
## Research synthesis workflow

Copy this checklist and track your progress:

```
Research Progress:
- [ ] Step 1: Read all source documents
- [ ] Step 2: Identify key themes
- [ ] Step 3: Cross-reference claims
- [ ] Step 4: Create structured summary
- [ ] Step 5: Verify citations
```

**Step 1: Read all source documents**

Review each document in the `sources/` directory. Note the main arguments and supporting evidence.

**Step 2: Identify key themes**

Look for patterns across sources. What themes appear repeatedly? Where do sources agree or disagree?

**Step 3: Cross-reference claims**

For each major claim, verify it appears in the source material. Note which source supports each point.

**Step 4: Create structured summary**

Organize findings by theme. Include:
- Main claim
- Supporting evidence from sources
- Conflicting viewpoints (if any)

**Step 5: Verify citations**

Check that every claim references the correct source document. If citations are incomplete, return to Step 3.
````

此示例展示了工作流程如何应用于不需要代码的分析任务。清单模式适用于任何复杂的多步骤过程。

**示例 2：PDF 表单填写工作流程**（适用于有代码的技能）：

````markdown  theme={null}
## PDF form filling workflow

Copy this checklist and check off items as you complete them:

```
Task Progress:
- [ ] Step 1: Analyze the form (run analyze_form.py)
- [ ] Step 2: Create field mapping (edit fields.json)
- [ ] Step 3: Validate mapping (run validate_fields.py)
- [ ] Step 4: Fill the form (run fill_form.py)
- [ ] Step 5: Verify output (run verify_output.py)
```

**Step 1: Analyze the form**

Run: `python scripts/analyze_form.py input.pdf`

This extracts form fields and their locations, saving to `fields.json`.

**Step 2: Create field mapping**

Edit `fields.json` to add values for each field.

**Step 3: Validate mapping**

Run: `python scripts/validate_fields.py fields.json`

Fix any validation errors before continuing.

**Step 4: Fill the form**

Run: `python scripts/fill_form.py input.pdf fields.json output.pdf`

**Step 5: Verify output**

Run: `python scripts/verify_output.py output.pdf`

If verification fails, return to Step 2.
````

清晰的步骤防止 Claude 跳过关键验证。清单帮助 Claude 和你跟踪多步骤工作流程的进度。

### 实施反馈循环

**常见模式**：运行验证器 → 修复错误 → 重复

这种模式大大提高了输出质量。

**示例 1：样式指南合规性**（适用于没有代码的技能）：

```markdown  theme={null}
## Content review process

1. Draft your content following the guidelines in STYLE_GUIDE.md
2. Review against the checklist:
   - Check terminology consistency
   - Verify examples follow the standard format
   - Confirm all required sections are present
3. If issues found:
   - Note each issue with specific section reference
   - Revise the content
   - Review the checklist again
4. Only proceed when all requirements are met
5. Finalize and save the document
```

这展示了使用参考文档而不是脚本的验证循环模式。"验证器"是 STYLE_GUIDE.md，Claude 通过阅读和比较来执行检查。

**示例 2：文档编辑过程**（适用于有代码的技能）：

```markdown  theme={null}
## Document editing process

1. Make your edits to `word/document.xml`
2. **Validate immediately**: `python ooxml/scripts/validate.py unpacked_dir/`
3. If validation fails:
   - Review the error message carefully
   - Fix the issues in the XML
   - Run validation again
4. **Only proceed when validation passes**
5. Rebuild: `python ooxml/scripts/pack.py unpacked_dir/ output.docx`
6. Test the output document
```

验证循环及早发现错误。

## 内容指南

### 避免时间敏感信息

不要包含会过时的信息：

**坏示例：时间敏感**（将变成错误的）：

```markdown  theme={null}
If you're doing this before August 2025, use the old API.
After August 2025, use the new API.
```

**好示例**（使用"旧模式"部分）：

```markdown  theme={null}
## Current method

Use the v2 API endpoint: `api.example.com/v2/messages`

## Old patterns

<details>
<summary>Legacy v1 API (deprecated 2025-08)</summary>

The v1 API used: `api.example.com/v1/messages`

This endpoint is no longer supported.
</details>
```

旧模式部分提供历史背景而不杂乱主要内容。

### 使用一致的术语

选择一个术语并在整个技能中使用它：

**好 - 一致**：

* 始终 "API endpoint"
* 始终 "field"
* 始终 "extract"

**坏 - 不一致**：

* 混合 "API endpoint"、"URL"、"API route"、"path"
* 混合 "field"、"box"、"element"、"control"
* 混合 "extract"、"pull"、"get"、"retrieve"

一致性帮助 Claude 理解和遵循指令。

## 常见模式

### 模板模式

为输出格式提供模板。将严格程度与你的需求相匹配。

**对于严格要求**（如 API 响应或数据格式）：

````markdown  theme={null}
## Report structure

ALWAYS use this exact template structure:

```markdown
# [Analysis Title]

## Executive summary
[One-paragraph overview of key findings]

## Key findings
- Finding 1 with supporting data
- Finding 2 with supporting data
- Finding 3 with supporting data

## Recommendations
1. Specific actionable recommendation
2. Specific actionable recommendation
```
````

**对于灵活指导**（当适应有用时）：

````markdown  theme={null}
## Report structure

Here is a sensible default format, but use your best judgment based on the analysis:

```markdown
# [Analysis Title]

## Executive summary
[Overview]

## Key findings
[Adapt sections based on what you discover]

## Recommendations
[Tailor to the specific context]
```

Adjust sections as needed for the specific analysis type.
````

### 示例模式

对于输出质量取决于看到示例的技能，提供输入/输出对，就像常规提示中一样：

````markdown  theme={null}
## Commit message format

Generate commit messages following these examples:

**Example 1:**
Input: Added user authentication with JWT tokens
Output:
```
feat(auth): implement JWT-based authentication

Add login endpoint and token validation middleware
```

**Example 2:**
Input: Fixed bug where dates displayed incorrectly in reports
Output:
```
fix(reports): correct date formatting in timezone conversion

Use UTC timestamps consistently across report generation
```

**Example 3:**
Input: Updated dependencies and refactored error handling
Output:
```
chore: update dependencies and refactor error handling

- Upgrade lodash to 4.17.21
- Standardize error response format across endpoints
```

Follow this style: type(scope): brief description, then detailed explanation.
````

示例帮助 Claude 比单独描述更清楚地理解所需的样式和详细程度。

### 条件工作流程模式

引导 Claude 通过决策点：

```markdown  theme={null}
## Document modification workflow

1. Determine the modification type:

   **Creating new content?** → Follow "Creation workflow" below
   **Editing existing content?** → Follow "Editing workflow" below

2. Creation workflow:
   - Use docx-js library
   - Build document from scratch
   - Export to .docx format

3. Editing workflow:
   - Unpack existing document
   - Modify XML directly
   - Validate after each change
   - Repack when complete
```

<提示>
  如果工作流程变得庞大或复杂，有许多步骤，考虑将它们推入单独的文件，并告诉 Claude 根据手头的任务阅读适当的文件。
</提示>

## 评估和迭代

### 先构建评估

**在编写大量文档之前创建评估。** 这确保你的技能解决实际问题，而不是想象的问题。

**评估驱动开发：**

1. **识别缺口**：在没有技能的情况下在代表性任务上运行 Claude。记录具体的失败或缺失的上下文
2. **创建评估**：构建测试这些场景的三种场景
3. **建立基线**：衡量没有技能时 Claude 的表现
4. **编写最少指令**：创建刚好足够的内容来解决缺口并通过评估
5. **迭代**：执行评估，与基线比较，并完善

这种方法确保你解决实际问题，而不是可能永远不会实现的预期需求。

**评估结构**：

```json  theme={null}
{
  "skills": ["pdf-processing"],
  "query": "Extract all text from this PDF file and save it to output.txt",
  "files": ["test-files/document.pdf"],
  "expected_behavior": [
    "Successfully reads the PDF file using an appropriate PDF processing library or command-line tool",
    "Extracts text content from all pages in the document without missing any pages",
    "Saves the extracted text to a file named output.txt in a clear, readable format"
  ]
}
```

<注意>
  此示例演示了带有简单测试评分标准的数据驱动评估。我们目前不提供运行这些评估的内置方式。用户可以创建自己的评估系统。评估是衡量技能有效性的真实来源。
</注意>

### 与 Claude 迭代开发技能

最有效的技能开发过程涉及 Claude 本身。与一个 Claude 实例（"Claude A"）一起创建将被其他实例（"Claude B"）使用的技能。Claude A 帮助你设计和完善指令，而 Claude B 在实际任务中测试它们。这之所以有效，是因为 Claude 模型既理解如何编写有效的代理指令，又理解代理需要什么信息。

**创建新技能：**

1. **在没有技能的情况下完成任务**：与 Claude A 使用正常提示一起解决问题。在你工作时，你自然会提供上下文、解释偏好和分享程序知识。注意你反复提供的信息。

2. **识别可复用模式**：完成任务后，确定你提供的哪些上下文对类似的未来任务有用。

   **示例**：如果你完成了 BigQuery 分析，你可能提供了表名、字段定义、过滤规则（如"始终排除测试账户"）和常见查询模式。

3. **要求 Claude A 创建技能**："创建一个技能，捕捉我们刚刚使用的 BigQuery 分析模式。包括表模式、命名约定和关于过滤测试账户的规则。"

   <提示>
     Claude 模型原生理解技能格式和结构。你不需要特殊的系统提示或"编写技能"技能来让 Claude 帮助创建技能。只需要求 Claude 创建技能，它就会生成具有适当 frontmatter 和正文内容的正确结构的 SKILL.md 内容。
   </提示>

4. **审查简洁性**：检查 Claude A 是否添加了不必要的解释。问："删除关于胜率含义的解释 - Claude 已经知道那个。"

5. **改进信息架构**：要求 Claude A 更有效地组织内容。例如："组织这个，使表模式在单独的参考文件中。我们以后可能会添加更多表。"

6. **在类似任务上测试**：在相关用例上与 Claude B（加载了技能的全新实例）一起使用该技能。观察 Claude B 是否找到正确的信息、正确应用规则并成功处理任务。

7. **基于观察迭代**：如果 Claude B 遇到困难或遗漏了什么，带着具体情况返回 Claude A："当 Claude 使用这个技能时，它忘记了按 Q4 日期过滤。我们应该添加关于日期过滤模式的部分吗？"

**迭代现有技能：**

当改进技能时，相同的层次模式继续。你在以下之间交替：

* **与 Claude A 一起工作**（帮助完善技能的专家）
* **与 Claude B 一起测试**（使用技能执行实际工作的代理）
* **观察 Claude B 的行为** 并将见解带回 Claude A

1. **在实际工作流程中使用技能**：给 Claude B（加载了技能）实际任务，而不是测试场景

2. **观察 Claude B 的行为**：注意它在哪里挣扎、成功或做出意外选择

   **观察示例**："当我要求 Claude B 提供区域销售报告时，它编写了查询但忘记过滤测试账户，即使技能提到了这个规则。"

3. **返回 Claude A 进行改进**：分享当前的 SKILL.md 并描述你观察到的内容。问："我注意到 Claude B 在要求区域报告时忘记过滤测试账户。技能提到了过滤，但也许它不够突出？"

4. **审查 Claude A 的建议**：Claude A 可能建议重新组织以使规则更突出，使用更强的语言如"MUST filter"而不是"always filter"，或重组工作流程部分。

5. **应用和测试更改**：用 Claude A 的改进更新技能，然后在类似请求上再次与 Claude B 测试

6. **基于使用重复**：继续这个观察-完善-测试循环，因为你遇到新场景。每次迭代都基于真实的代理行为改进技能，而不是假设。

**收集团队反馈：**

1. 与队友分享技能并观察他们的使用
2. 问：技能是否按预期激活？指令是否清晰？缺少什么？
3. 纳入反馈以解决你自己使用模式中的盲点

**为什么这种方法有效**：Claude A 理解代理需求，你提供领域专业知识，Claude B 通过实际使用揭示缺口，迭代完善基于观察到的行为而不是假设改进技能。

### 观察 Claude 如何导航技能

在迭代技能时，注意 Claude 实际如何使用它们。注意：

* **意外的探索路径**：Claude 是否以你未预料的顺序阅读文件？这可能表明你的结构没有你想象的那么直观
* **遗漏的连接**：Claude 是否未能遵循对重要文件的引用？你的链接可能需要更明确或更突出
* **过度依赖某些部分**：如果 Claude 反复阅读同一文件，考虑该内容是否应该在主 SKILL.md 中
* **被忽略的内容**：如果 Claude 从未访问捆绑文件，它可能是不必要的或在主指令中信号不佳

基于这些观察而不是假设进行迭代。技能元数据中的 'name' 和 'description' 特别关键。Claude 在决定是否响应当前任务触发技能时使用这些。确保它们清楚描述技能的功能和何时使用。

## 要避免的反模式

### 避免 Windows 风格路径

始终在前向斜杠文件路径中使用，即使在 Windows 上：

* ✓ **好**：`scripts/helper.py`、`reference/guide.md`
* ✗ **避免**：`scripts\helper.py`、`reference\guide.md`

Unix 风格路径在所有平台上都有效，而 Windows 风格路径在 Unix 系统上会导致错误。

### 避免提供太多选项

除非必要，否则不要呈现多种方法：

````markdown  theme={null}
**Bad example: Too many choices** (confusing):
"You can use pypdf, or pdfplumber, or PyMuPDF, or pdf2image, or..."

**Good example: Provide a default** (with escape hatch):
"Use pdfplumber for text extraction:
```python
import pdfplumber
```

For scanned PDFs requiring OCR, use pdf2image with pytesseract instead."
````

## 高级：带可执行代码的技能

下面的部分侧重于包含可执行脚本的技能。如果你的技能仅使用 Markdown 指令，请跳到 [有效技能清单](#checklist-for-effective-skills)。

### 解决，不要回避

在为技能编写脚本时，处理错误条件而不是回避给 Claude。

**好示例：显式处理错误**：

```python  theme={null}
def process_file(path):
    """Process a file, creating it if it doesn't exist."""
    try:
        with open(path) as f:
            return f.read()
    except FileNotFoundError:
        # Create file with default content instead of failing
        print(f"File {path} not found, creating default")
        with open(path, 'w') as f:
            f.write('')
        return ''
    except PermissionError:
        # Provide alternative instead of failing
        print(f"Cannot access {path}, using default")
        return ''
```

**坏示例：回避给 Claude**：

```python  theme={null}
def process_file(path):
    # Just fail and let Claude figure it out
    return open(path).read()
```

配置参数也应得到证明和记录，以避免"巫毒常数"（Ousterhout 定律）。如果你不知道正确的值，Claude 如何确定它？

**好示例：自文档化**：

```python  theme={null}
# HTTP requests typically complete within 30 seconds
# Longer timeout accounts for slow connections
REQUEST_TIMEOUT = 30

# Three retries balances reliability vs speed
# Most intermittent failures resolve by the second retry
MAX_RETRIES = 3
```

**坏示例：魔法数字**：

```python  theme={null}
TIMEOUT = 47  # Why 47?
RETRIES = 5   # Why 5?
```

### 提供工具脚本

即使 Claude 可以编写脚本，预制脚本也有优势：

**工具脚本的好处**：

* 比生成的代码更可靠
* 节省令牌（不需要在上下文中包含代码）
* 节省时间（不需要代码生成）
* 确保跨使用的一致性

<img src="https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=4bbc45f2c2e0bee9f2f0d5da669bad00" alt="Bundling executable scripts alongside instruction files" data-og-width="2048" width="2048" data-og-height="1154" height="1154" data-path="images/agent-skills-executable-scripts.png" data-optimize="true" data-opv="3" srcset="https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?w=280&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=9a04e6535a8467bfeea492e517de389f 280w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?w=560&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=e49333ad90141af17c0d7651cca7216b 560w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?w=840&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=954265a5df52223d6572b6214168c428 840w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?w=1100&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=2ff7a2d8f2a83ee8af132b29f10150fd 1100w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?w=1650&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=48ab96245e04077f4d15e9170e081cfb 1650w, https://mintcdn.com/anthropic-claude-docs/4Bny2bjzuGBK7o00/images/agent-skills-executable-scripts.png?w=2500&fit=max&auto=format&n=4Bny2bjzuGBK7o00&q=85&s=0301a6c8b3ee879497cc5b5483177c90 2500w" />

上图显示了可执行脚本如何与指令文件一起工作。指令文件（forms.md）引用脚本，Claude 可以执行它而不将其内容加载到上下文中。

**重要区别**：在你的指令中明确 Claude 应该：

* **执行脚本**（最常见）："Run `analyze_form.py` to extract fields"
* **将其作为参考阅读**（用于复杂逻辑）："See `analyze_form.py` for the field extraction algorithm"

对于大多数工具脚本，执行是首选，因为它更可靠和高效。有关脚本执行如何工作的详细信息，请参阅下面的 [运行时环境](#runtime-environment) 部分。

**示例**：

````markdown  theme={null}
## Utility scripts

**analyze_form.py**: Extract all form fields from PDF

```bash
python scripts/analyze_form.py input.pdf > fields.json
```

Output format:
```json
{
  "field_name": {"type": "text", "x": 100, "y": 200},
  "signature": {"type": "sig", "x": 150, "y": 500}
}
```

**validate_boxes.py**: Check for overlapping bounding boxes

```bash
python scripts/validate_boxes.py fields.json
# Returns: "OK" or lists conflicts
```

**fill_form.py**: Apply field values to PDF

```bash
python scripts/fill_form.py input.pdf fields.json output.pdf
```
````

### 使用视觉分析

当输入可以渲染为图像时，让 Claude 分析它们：

````markdown  theme={null}
## Form layout analysis

1. Convert PDF to images:
   ```bash
   python scripts/pdf_to_images.py form.pdf
   ```

2. Analyze each page image to identify form fields
3. Claude can see field locations and types visually
````

<注意>
  在此示例中，你需要编写 `pdf_to_images.py` 脚本。
</注意>

Claude 的视觉能力帮助理解布局和结构。

### 创建可验证的中间输出

当 Claude 执行复杂的开放式任务时，它可能会出错。"计划-验证-执行"模式通过让 Claude 首先在结构化格式中创建计划，然后用脚本验证该计划再执行它来及早捕获错误。

**示例**：想象要求 Claude 根据电子表格更新 PDF 中的 50 个表单字段。没有验证，Claude 可能引用不存在的字段、创建冲突的值、遗漏必填字段或错误地应用更新。

**解决方案**：使用上面显示的工作流程模式（PDF 表单填写），但添加一个在应用更改之前验证的中间 `changes.json` 文件。工作流程变成：分析 → **创建计划文件** → **验证计划** → 执行 → 验证。

**为什么这个模式有效：**

* **及早捕获错误**：验证在应用更改之前发现问题
* **机器可验证**：脚本提供客观验证
* **可逆计划**：Claude 可以在不接触原件的情况下迭代计划
* **清晰的调试**：错误消息指向具体问题

**何时使用**：批处理操作、破坏性更改、复杂验证规则、高风险操作。

**实现提示**：使验证脚本具有详细的特定错误消息，如"Field 'signature_date' not found. Available fields: customer_name, order_total, signature_date_signed" 以帮助 Claude 修复问题。

### 包依赖

技能在具有平台特定限制的代码执行环境中运行：

* **claude.ai**：可以从 npm 和 PyPI 安装包并从 GitHub 仓库拉取
* **Anthropic API**：没有网络访问和运行时包安装

在 SKILL.md 中列出必需的包，并验证它们在 [代码执行工具文档](/en/docs/agents-and-tools/tool-use/code-execution-tool) 中可用。

### 运行时环境

技能在具有文件系统访问、bash 命令和代码执行能力的代码执行环境中运行。有关此架构的概念性解释，请参阅概述中的 [技能架构](/en/docs/agents-and-tools/agent-skills/overview#the-skills-architecture)。

**这如何影响你的编写：**

**Claude 如何访问技能：**

1. **元数据预加载**：启动时，所有技能的 YAML frontmatter 中的名称和描述被加载到系统提示中
2. **按需读取文件**：Claude 使用 bash Read 工具在需要时从文件系统访问 SKILL.md 和其他文件
3. **高效执行脚本**：可以通过 bash 执行工具脚本而不将其完整内容加载到上下文中。只有脚本的输出消耗令牌
4. **大文件无上下文惩罚**：参考文件、数据或文档在实际读取之前不消耗上下文令牌

* **文件路径很重要**：Claude 像文件系统一样导航你的技能目录。使用前向斜杠（`reference/guide.md`），而非反斜杠
* **文件命名描述性**：使用指示内容的名称：`form_validation_rules.md`，而非 `doc2.md`
* **为发现组织**：按领域或功能组织目录
  * 好：`reference/finance.md`、`reference/sales.md`
  * 坏：`docs/file1.md`、`docs/file2.md`
* **捆绑综合资源**：包括完整的 API 文档、广泛的示例、大型数据集；访问前无上下文惩罚
* **对确定性操作优先使用脚本**：编写 `validate_form.py` 而不是要求 Claude 生成验证代码
* **明确执行意图**：
  * "Run `analyze_form.py` to extract fields"（执行）
  * "See `analyze_form.py` for the extraction algorithm"（作为参考阅读）
* **测试文件访问模式**：通过使用真实请求测试验证 Claude 是否可以导航你的目录结构

**示例：**

```
bigquery-skill/
├── SKILL.md (overview, points to reference files)
└── reference/
    ├── finance.md (revenue metrics)
    ├── sales.md (pipeline data)
    └── product.md (usage analytics)
```

当用户询问收入时，Claude 阅读 SKILL.md，看到对 `reference/finance.md` 的引用，并调用 bash 只读取该文件。sales.md 和 product.md 文件保留在文件系统上，在需要之前消耗零上下文令牌。这个基于文件的模型就是渐进式披露成为可能的原因。Claude 可以导航并选择性加载每个任务所需的确切内容。

有关技术架构的完整详细信息，请参阅技能概述中的 [技能如何工作](/en/docs/agents-and-tools/agent-skills/overview#how-skills-work)。

### MCP 工具引用

如果你的技能使用 MCP（模型上下文协议）工具，始终使用完全限定工具名称以避免"找不到工具"错误。

**格式**：`ServerName:tool_name`

**示例**：

```markdown  theme={null}
Use the BigQuery:bigquery_schema tool to retrieve table schemas.
Use the GitHub:create_issue tool to create issues.
```

其中：

* `BigQuery` 和 `GitHub` 是 MCP 服务器名称
* `bigquery_schema` 和 `create_issue` 是这些服务器内的工具名称

没有服务器前缀，Claude 可能无法定位工具，尤其是当多个 MCP 服务器可用时。

### 避免假设工具已安装

不要假设包可用：

````markdown  theme={null}
**Bad example: Assumes installation**:
"Use the pdf library to process the file."

**Good example: Explicit about dependencies**:
"Install required package: `pip install pypdf`

Then use it:
```python
from pypdf import PdfReader
reader = PdfReader("file.pdf")
```"
````

## 技术说明

### YAML frontmatter 要求

SKILL.md frontmatter 仅包括 `name`（最多 64 个字符）和 `description`（最多 1024 个字符）字段。有关完整的结构详细信息，请参阅 [技能概述](/en/docs/agents-and-tools/agent-skills/overview#skill-structure)。

### 令牌预算

为获得最佳性能，将 SKILL.md 正文保持在 500 行以下。如果你的内容超过此限制，使用前面描述的渐进式披露模式将其拆分为单独的文件。有关架构详细信息，请参阅 [技能概述](/en/docs/agents-and-tools/agent-skills/overview#how-skills-work)。

## 有效技能清单

在分享技能之前，验证：

### 核心质量

* [ ] 描述具体并包含关键术语
* [ ] 描述包括技能的功能和何时使用
* [ ] SKILL.md 正文在 500 行以下
* [ ] 额外细节在单独文件中（如果需要）
* [ ] 无时间敏感信息（或在"旧模式"部分中）
* [ ] 整个术语一致
* [ ] 示例具体，非抽象
* [ ] 文件引用深度一级
* [ ] 适当使用渐进式披露
* [ ] 工作流程有清晰的步骤

### 代码和脚本

* [ ] 脚本解决问题而不是回避给 Claude
* [ ] 错误处理显式且有帮助
* [ ] 无"巫毒常数"（所有值都有依据）
* [ ] 指令中列出了必需的包并验证为可用
* [ ] 脚本有清晰的文档
* [ ] 无 Windows 风格路径（所有前向斜杠）
* [ ] 关键操作的验证/验证步骤
* [ ] 质量关键任务的反馈循环

### 测试

* [ ] 创建了至少三个评估
* [ ] 用 Haiku、Sonnet 和 Opus 测试
* [ ] 用真实使用场景测试
* [ ] 纳入了团队反馈（如果适用）

## 下一步

<CardGroup cols={2}>
  <Card title="Get started with Agent Skills" icon="rocket" href="/en/docs/agents-and-tools/agent-skills/quickstart">
    Create your first Skill
  </Card>

  <Card title="Use Skills in Claude Code" icon="terminal" href="/en/docs/claude-code/skills">
    Create and manage Skills in Claude Code
  </Card>

  <Card title="Use Skills with the API" icon="code" href="/en/api/skills-guide">
    Upload and use Skills programmatically
  </Card>
</CardGroup>
