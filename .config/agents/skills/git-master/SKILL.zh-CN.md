---
name: git-master
description: MUST USE for ANY git operations. Enforces GPG-signed commits (-S flag always required; stop and ask user to unlock GPG agent on failure) and 50/72 commit message rule. Triggers: 'commit', 'rebase', 'squash', 'who wrote', 'when was X added', 'find the commit that'.
---

# Git Master Agent（中文版）

## 概述

Git 专家 Agent，专注三项技能：
1. **提交架构师**：原子提交、依赖排序、风格检测
2. **Rebase 外科医生**：历史重写、冲突解决、分支清理
3. **历史考古学家**：查找特定变更的引入时间和位置

---

## 提交消息格式：50/72 规则（强制，无例外）

### 规则说明

```
第 1 行（主题）：最多 50 个字符 — 概述提交做了什么
第 2 行：         空行（强制分隔符）
第 3 行+（正文）：每行最多 72 个字符 — 解释为什么和做了什么
```

### 主题行（最多 50 个字符）

- **最多 50 个字符** — 硬限制，绝不超过
- **祈使语气**："Add feature" 而不是 "Added feature" 或 "Adds feature"
- **不加句号**
- **首字母大写**
- **SEMANTIC 风格**：`feat: add login` 包括前缀计算总字符数

### 正文（每行最多 72 个字符）

- 主题和正文之间必须有**空行**
- 每行**最多 72 个字符** — 硬限制，在 72 处换行
- 解释**为什么**做这个变更，而不是做了什么（代码本身表达 what）
- 用破折号（`-`）列举多个要点
- 如适用，引用 issues/PR

### 格式示例

```
# SEMANTIC 风格（主题含前缀不超过 50 字符）
feat: add user authentication

使用 JWT 替换 session cookie 实现认证。
Session 方式不支持水平扩展。

- 新增 /auth/login 和 /auth/refresh 端点
- 使用 httpOnly cookie 存储 token 防止 XSS
- Token 有效期：access 15m，refresh 7d

Closes #123

# PLAIN 风格
Add user authentication

使用 JWT 替换 session cookie 实现认证。
Session 方式不支持水平扩展。
```

### 字符计数验证（每次提交前必做）

```
"feat: add user authentication"  = 30 字符  ✓  （不超过 50）
"fix: resolve race condition in connection pool"  = 47 字符  ✓
"feat: add new user registration with email verification"  = 56 字符  ✗  （超过 50，必须缩短）
```

### 危险信号 — 停下来重写

- 主题行 > 50 个字符 → **缩短**
- 正文前没有空行 → **添加**
- 正文某行 > 72 个字符 → **换行**
- 主题以 `.` 结尾 → **删除**
- 主题使用过去时（"Added"、"Fixed"）→ **改用祈使式**（"Add"、"Fix"）

---

## GPG 签名：所有提交必须 GPG 签名（强制，无例外）

**每一次提交都必须带 GPG 签名，无例外。**

### 基本规则

```
所有 git commit 命令必须包含 -S 标志（或 --gpg-sign）。
禁止：git commit -m "..."
必须：git commit -S -m "..."
```

### 每次提交前：GPG 预检

```bash
# 1. 确认 GPG 密钥可用
gpg --list-secret-keys --keyid-format LONG

# 2. 测试签名是否正常（空跑）
echo "test" | gpg --clearsign > /dev/null 2>&1 && echo "GPG 正常" || echo "GPG 已锁定"
```

### 如果 GPG 签名失败

```
常见错误信息：

error: gpg failed to sign the data
error: secret key not available
error: signing failed: No secret key
Pinentry 错误 / 密码提示失败

-> 不得提交未签名的 commit
-> 不得使用 -n/--no-gpg-sign 绕过
-> 必须停下来，通知用户：
```

**GPG 签名失败时必须发送的消息：**

```
GPG 签名失败，通常是因为 GPG Agent 已锁定或密钥不可用。
请在提交前解锁 GPG Agent：

    gpg-connect-agent reloadagent /bye
    # 或者手动触发一次签名解锁：
    echo "test" | gpg --clearsign

解锁后请告知，我将继续执行提交。
```

### 正确的提交命令

```bash
# 单行消息
git commit -S -m "feat: add user authentication"

# 多行消息（主题 + 正文）
git commit -S -m "feat: add user authentication" \
  -m "使用 JWT 替换 session cookie 以支持水平扩展。"

# Fixup 提交（同样必须签名）
git commit -S --fixup=<目标 hash>

# Amend（同样必须签名）
git commit -S --amend
```

### 危险信号 — 立即停下

- `git commit -m ...` 没有 `-S` → **补上 -S 标志**
- 提交时出现 GPG 错误 → **停下，告知用户解锁 GPG Agent**
- 想用 `--no-gpg-sign` → **禁止**
- 想用 `commit.gpgsign=false` → **禁止**

---

## 核心原则：默认多个提交（强制，无例外）

**一次提交 = 自动失败**

默认行为是创建**多个提交**。单次提交是逻辑错误，不是特性。

**硬性规则：**
```
3+ 个文件变更 -> 必须 2+ 次提交
5+ 个文件变更 -> 必须 3+ 次提交
10+ 个文件变更 -> 必须 5+ 次提交
```

---

## 快速参考

### 50/72 规则速查表

| 行 | 限制 | 规则 |
|----|------|------|
| 主题（第 1 行） | **最多 50 字符** | 祈使语气，不加句号，首字母大写 |
| 第 2 行 | **空行** | 主题与正文之间的强制分隔符 |
| 正文行（3+） | **最多 72 字符** | 解释为什么，长行换行 |

```
✓ "feat: add OAuth2 login support"       （32 字符）
✓ "Fix null pointer in session cleanup"  （37 字符）
✗ "feat: add OAuth2 login support for Google and GitHub accounts"  （61 字符 → 缩短）
```

### 提交风格速查

| git log 显示... | 使用风格 |
|-----------------|----------|
| `feat: xxx`、`fix: yyy` | SEMANTIC |
| `Add xxx`、`Fix yyy`、`xxx 추가` | PLAIN |
| `format`、`lint`、`typo` | SHORT |
| 完整句子 | SENTENCE |
| 以上混合 | 使用多数风格（不默认 semantic） |

### 反模式（自动失败）

1. **绝不一次提交大量文件** - 3+ 个文件必须分 2+ 次提交
2. **绝不默认 semantic 风格** - 先从 git log 检测
3. **绝不分离测试与实现** - 必须放在同一次提交
4. **绝不按文件类型分组** - 按功能/模块分组
5. **绝不重写已推送历史** - 未经明确许可
6. **绝不提交主题超过 50 字符** - 缩短至合规
7. **绝不提交正文行超过 72 字符** - 在 72 处换行
8. **绝不在不带 -S 的情况下提交** - 所有提交必须 GPG 签名
9. **绝不使用 --no-gpg-sign** - GPG 失败时停止并通知用户解锁 GPG Agent

---

## 所有模式的反模式

### 提交模式
- 多个文件一次提交 → 拆分
- 默认 semantic 风格 → 先检测
- 主题行 > 50 字符 → 缩短
- 正文行 > 72 字符 → 换行
- 正文前无空行 → 添加

### Rebase 模式
- Rebase main/master → 绝不
- 用 `--force` 而不是 `--force-with-lease` → 危险
- 工作区有未提交变更就 rebase → 会失败

### 历史搜索模式
- 该用 `-G` 时用了 `-S` → 结果错误
- blame 移动过的代码时不加 `-C` → 归因错误
- bisect 时没有正确设置 good/bad 边界 → 浪费时间
