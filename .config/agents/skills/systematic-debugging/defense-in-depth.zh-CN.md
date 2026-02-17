# 纵深防御验证

## 概述

当你修复由无效数据引起的 bug 时，在一个地方添加验证感觉足够了。但那个单一检查可能被不同的代码路径、重构或模拟绕过。

**核心原则：** 在数据通过的每一层都进行验证。让 bug 在结构上不可能发生。

## 为什么需要多层

单一验证："我们修复了 bug"
多层："我们让 bug 不可能发生"

不同层捕获不同情况：
- 入口验证捕获大多数 bug
- 业务逻辑捕获边界情况
- 环境防护防止特定上下文危险
- 调试日志在其他层失败时提供帮助

## 四层

### 第1层：入口点验证
**目的：** 在 API 边界拒绝明显无效的输入

```typescript
function createProject(name: string, workingDirectory: string) {
  if (!workingDirectory || workingDirectory.trim() === '') {
    throw new Error('workingDirectory 不能为空');
  }
  if (!existsSync(workingDirectory)) {
    throw new Error(`workingDirectory 不存在：${workingDirectory}`);
  }
  if (!statSync(workingDirectory).isDirectory()) {
    throw new Error(`workingDirectory 不是目录：${workingDirectory}`);
  }
  // ... 继续
}
```

### 第2层：业务逻辑验证
**目的：** 确保数据对此操作有意义

```typescript
function initializeWorkspace(projectDir: string, sessionId: string) {
  if (!projectDir) {
    throw new Error('workspace 初始化需要 projectDir');
  }
  // ... 继续
}
```

### 第3层：环境防护
**目的：** 在特定上下文中防止危险操作

```typescript
async function gitInit(directory: string) {
  // 在测试中，拒绝在临时目录外进行 git init
  if (process.env.NODE_ENV === 'test') {
    const normalized = normalize(resolve(directory));
    const tmpDir = normalize(resolve(tmpdir()));

    if (!normalized.startsWith(tmpDir)) {
      throw new Error(
        `测试期间拒绝在临时目录外进行 git init：${directory}`
      );
    }
  }
  // ... 继续
}
```

### 第4层：调试工具
**目的：** 为取证捕获上下文

```typescript
async function gitInit(directory: string) {
  const stack = new Error().stack;
  logger.debug('即将执行 git init', {
    directory,
    cwd: process.cwd(),
    stack,
  });
  // ... 继续
}
```

## 应用模式

当你找到 bug 时：

1. **追踪数据流** - 错误值起源于哪里？在哪里使用？
2. **映射所有检查点** - 列出数据通过的每个点
3. **在每一层添加验证** - 入口、业务、环境、调试
4. **测试每一层** - 尝试绕过第1层，验证第2层捕获它

## 会话中的示例

Bug：空的 `projectDir` 导致在源代码中执行 `git init`

**数据流：**
1. 测试设置 → 空字符串
2. `Project.create(name, '')`
3. `WorkspaceManager.createWorkspace('')`
4. `git init` 在 `process.cwd()` 中运行

**添加的四层：**
- 第1层：`Project.create()` 验证非空/存在/可写
- 第2层：`WorkspaceManager` 验证 projectDir 非空
- 第3层：`WorktreeManager` 在测试中拒绝在 tmpdir 外进行 git init
- 第4层：git init 前的堆栈跟踪日志

**结果：** 所有 1847 个测试通过，bug 无法复现

## 关键见解

所有四层都是必需的。在测试期间，每层捕获了其他层遗漏的 bug：
- 不同代码路径绕过了入口验证
- 模拟绕过了业务逻辑检查
- 不同平台上的边界情况需要环境防护
- 调试日志识别了结构性误用

**不要停在一个验证点。** 在每一层添加检查。
