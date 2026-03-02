---
name: shell-scripting
description: Use when writing bash/shell scripts - covers structured output, dependency management, command variants (GNU vs BSD), and avoiding reinvented wheels
---

# Shell 脚本编写

## 概述

编写健壮、可移植、依赖感知的 Shell 脚本。**核心原则：** 优先使用结构化输出而非文本解析，显式检查依赖，使用现有命令而非自定义实现。

## 使用时机

**在以下情况下使用此技能：**
- 编写任何 bash/shell 脚本（自动化、系统管理、构建脚本）
- 解析命令输出
- 跨平台 Shell 脚本编写（Linux/macOS/BSD）

**触发此技能的症状：**
- 准备使用 `grep`、`sed`、`awk`、`cut` 来解析命令输出
- 添加外部命令调用但未检查其是否存在
- 编写可能已作为命令存在的自定义逻辑

## 核心原则

### 1. 选项风格：可读性优先，POSIX 优先于可移植性

**强制要求：使用长选项（`--option`）以提高可读性，但有一个例外：当短选项是 POSIX 标准而长选项是 GNU 专有扩展时。**

**为什么使用长选项：**
- 自文档化：`tar --create --gzip --file` 比 `tar -czf` 更清晰
- 易于维护：未来的读者能立即理解意图
- 减少错误：无需记忆标志含义

**决策树：**
```
是否存在 --long 选项？
  否  → 使用短选项
  是  → 短选项是 POSIX 标准且长选项是 GNU 专有扩展？
    是  → 使用 POSIX 短选项（跨 Linux/BSD/macOS 可移植）
    否  → 使用 GNU 长选项（可读性获胜）
```

**快速示例：**

| 命令 | 使用这个 | 不使用这个 | 原因 |
|------|---------|-----------|------|
| `ls` | `ls -a -l -h` | `ls --all --long` | POSIX 短选项，GNU 长选项 |
| `grep` | `grep -r -i` | `grep --recursive --ignore-case` | POSIX 短选项，GNU 长选项 |
| `tar` | `tar --create --gzip --file=x.tar.gz` | `tar -czf x.tar.gz` | 两者都支持长选项，清晰度获胜 |
| `mkdir` | `mkdir --parents` | `mkdir -p` | 广泛支持的长选项 |
| `find` | `find --type f --name "*.log"` | `find -type f -name "*.log"` | 首选 GNU 长选项 |

**可接受的短选项：**
- 没有长选项等价物：`tar -C`、`find -print0`
- 标准习语：`set -euo pipefail`、`rm -rf`
- 当长选项是 GNU 专有时的 POSIX 标准：`-a`、`-l`、`-h`、`-r`、`-i`、`-E`、`-n`、`-v`

**不可接受的借口：**
- "只是一行代码" — 一行代码也需要清晰
- "紧急/紧迫" — 紧急情况下清晰度最重要
- "编辑旧代码" — 新代码使用正确的风格
- "更快输入" — 2 秒输入 vs 数小时调试

**自检（4 步）：**
1. `--long` 版本是否存在？检查 `man command`
2. 如果是：`-short` 是 POSIX 且 `--long` 是 GNU 专有？检查 BSD 手册页
3. 如果 POSIX + GNU 专有：使用 POSIX 短选项（可移植性）
4. 否则：使用长选项（可读性）

### 2. 结构化输出优于文本解析

命令通常提供机器可读格式。**始终先检查再解析。**

| ❌ 文本解析 | ✅ 结构化输出 |
|------------|--------------|
| `lsblk \| tail -n +3 \| cut -d' ' -f1` | `lsblk --json --output name,size` |
| `df -h \| grep pattern \| awk '{print $5}'` | `df --output=source,pcent` |
| `ps aux \| grep pattern \| awk '{print $2}'` | `pgrep -f pattern` |

**检查这些标志：**
- `-J`、`--json`：JSON 输出（lsblk、ip、systemctl）
- `--output=`：列选择（df、ps、dpkg）
- `-0`、`-z`：空字符分隔（find、xargs、grep）
- `-n`、`--noheadings`：移除标题（lsblk、lvs、pvs）

### 3. 优先使用现有命令

在编写自定义逻辑之前，先搜索内置命令。

| ❌ 自定义 | ✅ 现有命令 |
|---------|-----------|
| `basename "$part" \| sed 's/[0-9]*$//'` | `lsblk --noheadings --output pkname "$part"` |
| 循环计数文件 | `find . --type f -printf '.' \| wc --bytes` |
| 解析 /proc/meminfo | `free --bytes` 或 `vmstat -s` |

**如何发现：**
```bash
man command                    # 阅读手册查看选项
apropos "keyword"              # 搜索相关工具
command --help | grep json     # 检查结构化输出
```

### 4. 显式检查依赖

**模式：**
```bash
#!/usr/bin/env bash
set -euo pipefail

# 必需：jq、curl、lsblk
# 可选：notify-send（桌面通知）

for cmd in jq curl lsblk; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "错误：需要 $cmd" >&2
        echo "安装：sudo apt install $cmd" >&2
        exit 1
    fi
done
```

### 5. 处理 GNU vs BSD 变体

**不要限制为仅 POSIX。在有益时使用 GNU 特性，但处理可移植性。**

**需要 GNU 的模式：**
```bash
# 此脚本需要 GNU coreutils
# macOS: brew install coreutils

if ! stat --version 2>/dev/null | grep --quiet GNU; then
    echo "错误：需要 GNU stat" >&2
    echo "macOS: brew install coreutils" >&2
    exit 1
fi
```

**跨平台模式：**
```bash
if stat --version 2>/dev/null | grep --quiet GNU; then
    file_time=$(stat --format='%Y' "$file")  # GNU
else
    file_time=$(stat -f '%m' "$file")         # BSD/macOS
fi
```

## 快速参考

### POSIX vs GNU 选项

**POSIX 短选项（使用这些以实现可移植性）：**

| 命令 | POSIX 选项 | 避免 GNU 长选项 |
|------|-----------|----------------|
| `ls` | `-a -l -h -t -r` | `--all --long --human-readable` |
| `grep` | `-r -i -E -n -v` | `--recursive --ignore-case` |
| `df` | `-h -T` | `--human-readable --type` |
| `ps` | `-e -f -o` | （POSIX 中没有长选项） |

**GNU 长选项（当两者都支持或仅 GNU 时使用这些）：**

| 命令 | 首选 | 原因 |
|------|------|------|
| `tar` | `--create --gzip --file` | GNU/BSD 都支持，清晰度获胜 |
| `mkdir` | `--parents` | 广泛可用 |
| `find` | `--printf` | 仅 GNU 特性，无 POSIX 等价物 |

### 结构化输出命令

| 命令 | 标志 | 示例 |
|------|------|------|
| `lsblk` | `-J`（JSON） | `lsblk --json --output name,size` |
| `ip` | `-j`（JSON） | `ip --json addr show` |
| `df` | `--output=` | `df --output=source,pcent` |
| `ps` | `-o` | `ps -eo pid,comm --no-headers` |
| `find` | `-print0` | `find . -print0 \| xargs -0` |

### 依赖检查

```bash
# 单个命令
command -v jq &>/dev/null || { echo "需要 jq" >&2; exit 1; }

# 多个命令
for cmd in jq curl awk; do
    command -v "$cmd" &>/dev/null || { echo "需要 $cmd" >&2; exit 1; }
done
```

### GNU vs BSD 检测

```bash
# 检查 GNU
if cmd --version 2>/dev/null | grep --quiet GNU; then
    # GNU 代码
else
    # BSD 代码
fi
```

## 常见错误

### ❌ 解析 ls 输出
```bash
for file in $(ls *.txt); do echo "$file"; done  # 空格会破坏
```
**修复：** 使用通配符或 `find -print0`

### ❌ 假设命令存在
```bash
result=$(echo '{}' | jq -r '.key')  # 如果缺少 jq 会静默失败
```
**修复：** 先用 `command -v jq` 检查

### ❌ 存在结构化输出时进行文本解析
```bash
df -h | grep sda1 | awk '{print $5}'  # 脆弱
```
**修复：** `df --output=pcent /dev/sda1`

### ❌ 重新发明功能
```bash
for i in {1..5}; do curl "$url" && break; sleep 2; done  # 自定义重试
```
**修复：** `timeout 30 bash -c 'until curl "$1"; do sleep 2; done' _ "$url"`

## 红旗警告

**如果您准备这样做，请停下来：**
- 使用短选项 → 检查：POSIX 标准？那么可以。否则使用 `--long`。
- 在输出上使用 `grep`/`sed`/`awk` → 先检查 `--json` 或 `--output=`
- 调用外部命令 → 添加存在性检查
- 输入"紧急"或"简单一行代码" → 那正是清晰度最重要的时候
- 用正则表达式解析设备名 → 先检查 `lsblk`、`blkid`、`findmnt`
- 编写 10 行以上的自定义逻辑 → 先搜索 `man` 和 `apropos`

## 常见借口

| 借口 | 现实 |
|------|------|
| "短选项输入更快" | 2 秒输入 vs 数小时后调试 |
| "只是一行代码" | 一行代码也需要被阅读和调试 |
| "紧急情况" | 紧急情况下最需要清晰度 |
| "现有代码使用短选项" | 新代码使用正确的风格 |
| "正则表达式比 JSON 简单" | 正则表达式跨版本会破坏，JSON 不会 |
| "不需要依赖检查" | 显式错误节省数小时调试时间 |
| "文本解析在测试中有效" | 生产环境有不同的区域设置/版本 |
| "已经实现，测试就够了" | 测试证明现在，不保证未来 |

## 实际影响

**此技能解决的模式：**
- 文本解析 → 结构化输出（JSON、`--output=`）
- 无依赖文档 → 显式检查和安装提示
- 平台假设 → GNU vs BSD 检测
- 重新发明轮子 → 优先使用现有命令
