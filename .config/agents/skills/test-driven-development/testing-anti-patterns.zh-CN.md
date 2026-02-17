# 测试反模式

**在以下情况下加载此参考：** 编写或更改测试、添加 mock，或 tempted 向生产代码添加仅测试使用的方法。

## 概述

测试必须验证真实行为，而非 mock 行为。Mock 是隔离的手段，而非测试的对象。

**核心原则：** 测试代码做什么，而非 mock 做什么。

**遵循严格的 TDD 可以防止这些反模式。**

## 铁律

```
1. 永远不要测试 mock 行为
2. 永远不要向生产类添加仅测试使用的方法
3. 永远不要在不理解依赖的情况下进行 mock
```

## 反模式 1：测试 Mock 行为

**违规：**
```typescript
// ❌ 错误：测试 mock 是否存在
test('renders sidebar', () => {
  render(<Page />);
  expect(screen.getByTestId('sidebar-mock')).toBeInTheDocument();
});
```

**为什么这是错误的：**
- 你在验证 mock 是否有效，而非组件是否有效
- mock 存在时测试通过，不存在时失败
- 对真实行为一无所知

**你的合作伙伴的纠正：** "我们在测试 mock 的行为吗？"

**修复：**
```typescript
// ✅ 正确：测试真实组件或不 mock 它
test('renders sidebar', () => {
  render(<Page />);  // 不要 mock sidebar
  expect(screen.getByRole('navigation')).toBeInTheDocument();
});

// 或者如果为了隔离必须 mock sidebar：
// 不要对 mock 做断言 - 测试 sidebar 存在时 Page 的行为
```

### 门控函数

```
在对任何 mock 元素做断言之前：
  问："我在测试真实组件行为还是仅仅 mock 是否存在？"

  如果在测试 mock 是否存在：
    停止 - 删除断言或取消 mock 组件

  改为测试真实行为
```

## 反模式 2：生产代码中的仅测试使用方法

**违规：**
```typescript
// ❌ 错误：destroy() 仅在测试中使用
class Session {
  async destroy() {  // 看起来像生产 API！
    await this._workspaceManager?.destroyWorkspace(this.id);
    // ... 清理
  }
}

// 在测试中
afterEach(() => session.destroy());
```

**为什么这是错误的：**
- 生产类被仅测试使用的代码污染
- 如果在生产环境中意外调用很危险
- 违反 YAGNI 和关注点分离
- 混淆对象生命周期与实体生命周期

**修复：**
```typescript
// ✅ 正确：测试工具处理测试清理
// Session 没有 destroy() - 它在生产环境中是无状态的

// 在 test-utils/
export async function cleanupSession(session: Session) {
  const workspace = session.getWorkspaceInfo();
  if (workspace) {
    await workspaceManager.destroyWorkspace(workspace.id);
  }
}

// 在测试中
afterEach(() => cleanupSession(session));
```

### 门控函数

```
在向生产类添加任何方法之前：
  问："这是否仅被测试使用？"

  如果是：
    停止 - 不要添加它
    改为放入测试工具中

  问："此类是否拥有此资源的生命周期？"

  如果不是：
    停止 - 此类不适合此方法
```

## 反模式 3：在不理解的情况下进行 Mock

**违规：**
```typescript
// ❌ 错误：Mock 破坏了测试逻辑
test('detects duplicate server', () => {
  // Mock 阻止了测试依赖的配置写入！
  vi.mock('ToolCatalog', () => ({
    discoverAndCacheTools: vi.fn().mockResolvedValue(undefined)
  }));

  await addServer(config);
  await addServer(config);  // 应该抛出 - 但不会！
});
```

**为什么这是错误的：**
- 被 mock 的方法有测试依赖的副作用（写入配置）
- 过度 mock 以"安全起见"破坏了实际行为
- 测试因错误原因通过或神秘失败

**修复：**
```typescript
// ✅ 正确：在正确的级别 mock
test('detects duplicate server', () => {
  // Mock 慢的部分，保留测试需要的行为
  vi.mock('MCPServerManager'); // 仅 mock 慢的服务器启动

  await addServer(config);  // 配置已写入
  await addServer(config);  // 检测到重复 ✓
});
```

### 门控函数

```
在 mock 任何方法之前：
  停止 - 先不要 mock

  1. 问："真实方法有什么副作用？"
  2. 问："此测试是否依赖这些副作用？"
  3. 问："我完全理解此测试需要什么吗？"

  如果依赖副作用：
    在更低级别 mock（实际的慢/外部操作）
    或使用保留必要行为的测试替身
    而不是测试依赖的高级方法

  如果不确定测试依赖什么：
    首先使用真实实现运行测试
    观察实际需要发生什么
    然后在正确级别添加最小 mock

  红旗：
    - "我会 mock 这个以安全起见"
    - "这可能很慢，最好 mock 它"
    - 在不理解依赖链的情况下进行 mock
```

## 反模式 4：不完整的 Mock

**违规：**
```typescript
// ❌ 错误：部分 mock - 仅包含你认为需要的字段
const mockResponse = {
  status: 'success',
  data: { userId: '123', name: 'Alice' }
  // 缺失：下游代码使用的 metadata
};

// 之后：当代码访问 response.metadata.requestId 时中断
```

**为什么这是错误的：**
- **部分 mock 隐藏了结构假设** - 你只 mock 了你知道的字段
- **下游代码可能依赖你未包含的字段** - 静默失败
- **测试通过但集成失败** - Mock 不完整，真实 API 完整
- **虚假信心** - 测试对真实行为一无所知

**铁律：** Mock 完整的数据结构，如现实中存在的那样，而非仅你的直接测试使用的字段。

**修复：**
```typescript
// ✅ 正确：镜像真实 API 的完整性
const mockResponse = {
  status: 'success',
  data: { userId: '123', name: 'Alice' },
  metadata: { requestId: 'req-789', timestamp: 1234567890 }
  // 真实 API 返回的所有字段
};
```

### 门控函数

```
在创建 mock 响应之前：
  检查："真实 API 响应包含哪些字段？"

  操作：
    1. 检查文档/示例中的实际 API 响应
    2. 包含系统可能在下游消费的所有字段
    3. 验证 mock 完全匹配真实响应模式

  关键：
    如果你在创建 mock，你必须理解整个结构
    部分 mock 在代码依赖被省略的字段时静默失败

  如果不确定：包含所有文档化的字段
```

## 反模式 5：将集成测试作为事后考虑

**违规：**
```
✅ 实现完成
❌ 未编写测试
"准备测试"
```

**为什么这是错误的：**
- 测试是实现的一部分，不是可选的后续工作
- TDD 本应捕获这一点
- 没有测试就不能声称完成

**修复：**
```
TDD 循环：
1. 编写失败的测试
2. 实现使其通过
3. 重构
4. 然后声称完成
```

## 当 Mock 变得太复杂时

**警告信号：**
- Mock 设置比测试逻辑更长
- Mock 所有东西以使测试通过
- Mock 缺少真实组件拥有的方法
- Mock 变更时测试中断

**你的合作伙伴的问题：** "我们需要在这里使用 mock 吗？"

**考虑：** 使用真实组件的集成测试通常比复杂的 mock 更简单

## TDD 防止这些反模式

**TDD 为何有帮助：**
1. **先写测试** → 迫使你思考你实际在测试什么
2. **观察它失败** → 确认测试测试的是真实行为，而非 mock
3. **最小实现** → 不会混入仅测试使用的方法
4. **真实依赖** → 在 mock 之前，你看到测试实际需要什么

**如果你在测试 mock 行为，你违反了 TDD** - 你在没有先观察测试针对真实代码失败的情况下添加了 mock。

## 快速参考

| 反模式 | 修复 |
|--------------|-----|
| 对 mock 元素做断言 | 测试真实组件或取消 mock |
| 生产代码中的仅测试使用方法 | 移到测试工具中 |
| 在不理解的情况下 mock | 先理解依赖，最小化 mock |
| 不完整的 mock | 完全镜像真实 API |
| 将测试作为事后考虑 | TDD - 测试优先 |
| 过度复杂的 mock | 考虑集成测试 |

## 红旗

- 断言检查 `*-mock` 测试 ID
- 仅在测试文件中调用的方法
- Mock 设置占测试的 >50%
- 移除 mock 时测试失败
- 无法解释为什么需要 mock
- "为了安全起见"进行 mock

## 底线

**Mock 是隔离的工具，而非测试的对象。**

如果 TDD 揭示你在测试 mock 行为，你就走错了路。

修复：测试真实行为或质疑你为什么在 mock。
