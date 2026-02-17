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
- 添加脚本依赖
- 跨平台 Shell 脚本编写（Linux/macOS/BSD）

**触发此技能的症状：**
- 准备使用 `grep`、`sed`、`awk`、`cut` 来解析命令输出
- 添加外部命令调用但未检查其是否存在
- 手动从文本输出中提取字段
- 编写可能已作为命令存在的自定义逻辑

## 核心原则

### 1. 优先使用结构化输出而非文本解析

命令通常支持机器可读的格式（JSON、CSV、空字符分隔）。**始终检查是否可用。**

| ❌ 文本解析 | ✅ 结构化输出 |
|------------|--------------|
| `lsblk \| tail -n +3 \| cut -d' ' -f1` | `lsblk -no name,size -J` (JSON) |
| `df -h \| grep -v '^Filesystem' \| awk '{print $1,$5}'` | `df --output=source,pcent` |
| `ps aux \| grep pattern \| awk '{print $2}'` | `pgrep -f pattern` 或 `ps -o pid= -C command` |
| `find . -name "*.sh" \| wc -l` | `find . -name "*.sh" -printf '.' \| wc -c` |

**为什么这很重要：**
- 文本格式在不同版本/发行版之间会发生变化
- 空白字符、区域设置和格式会破坏解析
- 结构化输出是版本稳定的

**需要检查的命令选项：**
- `-J` 或 `--json`: JSON 输出（lsblk、ip、systemctl 等）
- `--output=`: 选择特定列（df、ps、dpkg）
- `-0` 或 `-z`: 空字符分隔输出（find、xargs、grep）
- `-n` 或 `--noheadings`: 移除标题以便干净解析（lsblk、lvs、pvs）
- `-o` 或 `--format`: 自定义格式字符串（ps、date、git）

### 2. 在实现之前先寻找现有命令

**在编写自定义逻辑之前，先搜索内置命令或标志。**

| ❌ 自定义实现 | ✅ 现有命令/标志 |
|-------------|-----------------|
| `basename "$partition" \| sed 's/[0-9]*$//'` | `lsblk -no pkname "$partition"` |
| `echo "$string" \| tr '[:upper:]' '[:lower:]'` | `printf '%s\n' "${string,,}"` (bash 4+) |
| 循环计数文件 | `find . -type f -printf '.' \| wc -c` |
| 解析 /proc/meminfo | `free -b` 或 `vmstat -s` |
| 自定义重试循环 | `timeout` + `until`/`while` 模式 |

**如何发现：**
```bash
# 阅读相关命令的手册
man lsblk  # 查找 -o, --output 选项
man find   # 检查 -printf, -print0 选项

# 搜索相似工具
apropos "list block"
apropos "partition"

# 检查命令帮助中的机器可读选项
command --help | grep -E 'json|output|format|print'
```

### 3. 跟踪和检查依赖

**始终记录必需和可选的依赖。**

**在脚本开头：**
```bash
#!/usr/bin/env bash

# 必需依赖: jq, curl, lsblk
# 可选依赖: notify-send (用于桌面通知)

set -euo pipefail

# 检查必需依赖
for cmd in jq curl lsblk; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: Required command '$cmd' not found" >&2
        echo "Install with: sudo apt install $cmd" >&2  # 根据发行版调整
        exit 1
    fi
done

# 检查可选依赖
if ! command -v notify-send &>/dev/null; then
    echo "Warning: notify-send not found. Desktop notifications disabled." >&2
fi
```

**在文档/注释中：**
```markdown
## 依赖

### 必需
- `jq` (JSON 处理器) - 安装: `sudo apt install jq` (Debian/Ubuntu), `brew install jq` (macOS)
- `curl` (HTTP 客户端) - 通常预装
- `lsblk` (块设备信息) - util-linux 包的一部分

### 可选
- `notify-send` (桌面通知) - 安装: `sudo apt install libnotify-bin`
```

### 4. 处理 GNU 与 BSD 命令变体

**你不需要仅限 POSIX 的脚本。在有益时使用 GNU 特性，但要检查并指导安装。**

**常用的 GNU 特定特性：**
- `sed -i` (原地编辑)
- `find -printf` (自定义输出格式)
- `grep -P` (Perl 正则)
- `date -d` (相对日期解析)
- `stat -c` (自定义格式)

**GNU 必需脚本的模式：**

```bash
#!/usr/bin/env bash

# 此脚本需要 GNU coreutils (非 BSD)
# macOS 用户: brew install coreutils && export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# 验证 GNU 版本
if ! stat --version 2>/dev/null | grep -q GNU; then
    echo "Error: This script requires GNU stat" >&2
    echo "macOS: brew install coreutils" >&2
    echo "Then add to PATH: export PATH=\"/usr/local/opt/coreutils/libexec/gnubin:\$PATH\"" >&2
    exit 1
fi

# 现在可以安全使用 GNU 特定特性
stat -c '%Y' "$file"  # GNU 格式说明符
```

**跨平台方法（可行时）：**

```bash
# 检测 GNU 与 BSD 并适配
if stat --version 2>/dev/null | grep -q GNU; then
    # GNU stat
    file_time=$(stat -c '%Y' "$file")
else
    # BSD stat (macOS)
    file_time=$(stat -f '%m' "$file")
fi
```

## 常见辩解

**为什么智能体会跳过这些原则以及现实是什么：**

| 借口 | 现实 |
|------|------|
| "正则比检查 JSON 标志更简单" | JSON 标志在 `--help` 中。阅读文档只需 10 秒。正则会在版本间失效。 |
| "为常用命令添加依赖检查不值得" | "常用"因环境而异。30 秒的检查可以防止神秘的生产故障。 |
| "用户会知道要安装依赖" | 他们不会。带有安装命令的显式错误可以节省数小时的调试时间。 |
| "文本解析在我的测试中工作正常" | 你的环境不是生产环境。不同的区域设置/版本会破坏它。 |
| "GNU 与 BSD 对这个脚本不重要" | 当用户在 macOS 上且脚本崩溃时就重要了。尽早检查或优雅失败。 |
| "不需要 lsblk，可以解析设备名" | NVMe 命名（nvme0n1p1）、md 阵列、LVM 会破坏字符串模式。lsblk 了解所有情况。 |
| "已经实现了，测试就足够了" | 测试证明它现在有效。不能证明它不会失效。结构化输出是面向未来的。 |
| "添加 jq 是开销" | Shell 文本解析是更大的开销。jq 是健壮、广泛可用、专门构建的。 |

## 危险信号 - 停止并重新考虑

**如果你正准备：**
- 在命令输出上使用 `grep`/`sed`/`awk`/`cut` → 首先检查命令是否有 `-J`、`--output=` 或 `-o` 标志
- 调用外部命令 → 添加存在检查和安装指南
- 用正则解析设备名 → 检查 `lsblk`、`blkid` 或 `findmnt` 是否已提供该信息
- 假设 GNU 命令存在 → 检查 `--version | grep GNU` 或提供 BSD 替代方案
- 编写 10+ 行自定义逻辑 → 首先搜索 `man` 和 `apropos` 寻找现有命令

**所有这些都意味着：在实现之前阅读命令的手册页。**

## 常见错误

### ❌ 解析 ls 输出
```bash
# 永远不要这样做
for file in $(ls *.txt); do
    echo "$file"
done
```

**原因：** 在空格、换行符、特殊字符上会失效。

**修复：**
```bash
# 使用通配符或 find
for file in *.txt; do
    echo "$file"
done

# 或使用空字符分隔的 find
find . -name "*.txt" -print0 | while IFS= read -r -d '' file; do
    echo "$file"
done
```

### ❌ 假设命令存在
```bash
# 如果 jq 未安装则静默失败
result=$(echo '{"key":"value"}' | jq -r '.key')
```

**修复：**
```bash
if ! command -v jq &>/dev/null; then
    echo "Error: jq required but not installed" >&2
    exit 1
fi
result=$(echo '{"key":"value"}' | jq -r '.key')
```

### ❌ 当结构化选项存在时使用正则
```bash
# 脆弱 - 格式变化时会失效
df -h | grep '/dev/sda1' | awk '{print $5}' | tr -d '%'
```

**修复：**
```bash
# 健壮 - 使用结构化输出
df --output=pcent /dev/sda1 | tail -n1 | tr -d ' %'
```

### ❌ 重复造轮子
```bash
# 自定义重试带睡眠
for i in {1..5}; do
    if curl "$url"; then
        break
    fi
    sleep 2
done
```

**修复：**
```bash
# 使用 timeout + until 模式
timeout 30 bash -c 'until curl "$1"; do sleep 2; done' _ "$url"
```

## 快速参考

### 结构化输出命令

| 命令 | 结构化格式 | 示例 |
|------|-----------|------|
| `lsblk` | `-J` (JSON), `-P` (键=值), `-no` (无标题) | `lsblk -J -o name,size,type` |
| `ip` | `-j` (JSON) | `ip -j addr show` |
| `df` | `--output=` (列选择) | `df --output=source,pcent,target` |
| `ps` | `-o` (自定义格式) | `ps -eo pid,comm,pcpu --no-headers` |
| `systemctl` | `--output=json` | `systemctl show --output=json unit.service` |
| `find` | `-print0`, `-printf` | `find . -type f -printf '%p\t%s\n'` |
| `jq` | JSON 处理器 | `echo '{"a":1}' \| jq -r '.a'` |

### 依赖检查模式

```bash
# 简单检查
command -v cmd &>/dev/null || { echo "cmd required" >&2; exit 1; }

# 带安装提示
if ! command -v jq &>/dev/null; then
    echo "Error: jq required" >&2
    echo "Install: sudo apt install jq" >&2
    exit 1
fi

# 基于数组的多个依赖检查
REQUIRED_DEPS=(jq curl awk)
for dep in "${REQUIRED_DEPS[@]}"; do
    command -v "$dep" &>/dev/null || {
        echo "Error: $dep required" >&2
        exit 1
    }
done
```

### GNU 与 BSD 检测

```bash
# 检查 GNU 版本
if cmd --version 2>/dev/null | grep -q GNU; then
    # GNU 特定代码
else
    # BSD 特定代码
fi

# 或如果必需 GNU 则尽早失败
if ! stat --version 2>/dev/null | grep -q GNU; then
    echo "GNU stat required" >&2
    echo "macOS: brew install coreutils" >&2
    exit 1
fi
```

## 实际影响

**此技能解决的观察到的反模式：**
- 文本解析 (`lsblk | tail | cut`) → `lsblk -J` (JSON)
- 无依赖文档 → 带有安装命令的显式检查
- 平台假设 → GNU 与 BSD 检测
- 重复造轮子 → 使用 `lsblk -no pkname` 而非正则
