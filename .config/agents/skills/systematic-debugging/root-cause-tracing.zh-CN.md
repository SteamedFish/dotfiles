# 根本原因追踪

## 概述

Bug 通常出现在调用堆栈深处（在错误目录中执行 git init，在错误位置创建文件，用错误路径打开数据库）。你的本能是在错误出现的地方修复，但那是治疗症状。

**核心原则：** 通过调用链向后追踪，直到找到原始触发点，然后在源头修复。

## 何时使用

```dot
digraph when_to_use {
    "Bug 出现在堆栈深处？" [shape=diamond];
    "能向后追踪？" [shape=diamond];
    "在症状点修复" [shape=box];
    "追踪到原始触发点" [shape=box];
    "更好：同时添加纵深防御" [shape=box];

    "Bug 出现在堆栈深处？" -> "能向后追踪？" [label="是"];
    "能向后追踪？" -> "追踪到原始触发点" [label="是"];
    "能向后追踪？" -> "在症状点修复" [label="否 - 死胡同"];
    "追踪到原始触发点" -> "更好：同时添加纵深防御";
}
```

**用于：**
- 错误发生在执行深处（不在入口点）
- 堆栈跟踪显示长调用链
- 不清楚无效数据起源于哪里
- 需要找到哪个测试/代码触发问题

## 追踪流程

### 1. 观察症状
```
错误：git init 在 /Users/jesse/project/packages/core 中失败
```

### 2. 找到直接原因
**什么代码直接导致这个？**
```typescript
await execFileAsync('git', ['init'], { cwd: projectDir });
```

### 3. 问：什么调用了这个？
```typescript
WorktreeManager.createSessionWorktree(projectDir, sessionId)
  → 被 Session.initializeWorkspace() 调用
  → 被 Session.create() 调用
  → 被 Project.create() 处的测试调用
```

### 4. 继续向上追踪
**传递了什么值？**
- `projectDir = ''`（空字符串！）
- 空字符串作为 `cwd` 解析为 `process.cwd()`
- 那是源代码目录！

### 5. 找到原始触发点
**空字符串来自哪里？**
```typescript
const context = setupCoreTest(); // 返回 { tempDir: '' }
Project.create('name', context.tempDir); // 在 beforeEach 之前访问！
```

## 添加堆栈跟踪

当你无法手动追踪时，添加工具：

```typescript
// 在问题操作之前
async function gitInit(directory: string) {
  const stack = new Error().stack;
  console.error('DEBUG git init:', {
    directory,
    cwd: process.cwd(),
    nodeEnv: process.env.NODE_ENV,
    stack,
  });

  await execFileAsync('git', ['init'], { cwd: directory });
}
```

**关键：** 在测试中使用 `console.error()`（不是 logger - 可能不会显示）

**运行并捕获：**
```bash
npm test 2>&1 | grep 'DEBUG git init'
```

**分析堆栈跟踪：**
- 查找测试文件名
- 找到触发调用的行号
- 识别模式（相同测试？相同参数？）

## 查找哪个测试造成污染

如果在测试期间出现某些东西但你不确定是哪个测试：

使用本目录中的二分脚本 `find-polluter.sh`：

```bash
./find-polluter.sh '.git' 'src/**/*.test.ts'
```

逐个运行测试，在第一个污染源停止。查看脚本了解用法。

## 真实示例：空的 projectDir

**症状：** `.git` 在 `packages/core/` 中创建（源代码）

**追踪链：**
1. `git init` 在 `process.cwd()` 中运行 ← 空的 cwd 参数
2. WorktreeManager 被空的 projectDir 调用
3. Session.create() 传递空字符串
4. 测试在 beforeEach 之前访问 `context.tempDir`
5. setupCoreTest() 初始返回 `{ tempDir: '' }`

**根本原因：** 顶层变量初始化访问空值

**修复：** 将 tempDir 设为 getter，如果在 beforeEach 之前访问则抛出

**同时添加纵深防御：**
- 第1层：Project.create() 验证目录
- 第2层：WorkspaceManager 验证非空
- 第3层：NODE_ENV 防护拒绝在 tmpdir 外进行 git init
- 第4层：git init 前的堆栈跟踪日志

## 关键原则

```dot
digraph principle {
    "找到直接原因" [shape=ellipse];
    "能向上追踪一层？" [shape=diamond];
    "向后追踪" [shape=box];
    "这是源头吗？" [shape=diamond];
    "在源头修复" [shape=box];
    "在每一层添加验证" [shape=box];
    "Bug 不可能发生" [shape=doublecircle];
    "永远不要只修复症状" [shape=octagon, style=filled, fillcolor=red, fontcolor=white];

    "找到直接原因" -> "能向上追踪一层？";
    "能向上追踪一层？" -> "向后追踪" [label="是"];
    "能向上追踪一层？" -> "永远不要只修复症状" [label="否"];
    "向后追踪" -> "这是源头吗？";
    "这是源头吗？" -> "向后追踪" [label="否 - 继续"];
    "这是源头吗？" -> "在源头修复" [label="是"];
    "在源头修复" -> "在每一层添加验证";
    "在每一层添加验证" -> "Bug 不可能发生";
}
```

**永远不要只在错误出现的地方修复。** 向后追踪找到原始触发点。

## 堆栈跟踪技巧

**在测试中：** 使用 `console.error()` 而不是 logger - logger 可能被抑制
**在操作之前：** 在危险操作之前记录，不是在它失败之后
**包含上下文：** 目录、cwd、环境变量、时间戳
**捕获堆栈：** `new Error().stack` 显示完整调用链

## 实际影响

来自调试会话（2025-10-03）：
- 通过 5 层追踪找到根本原因
- 在源头修复（getter 验证）
- 添加 4 层防御
- 1847 个测试通过，零污染
