# Clean Chroot 构建 - 高级参考

本文档提供主 PKGBUILD 技能中基本工作流程之外的高级 clean chroot 构建技术。

## 在 tmpfs 中构建（加速）

如果您有足够的 RAM（推荐 20GB+）：

```bash
# 创建 tmpfs 挂载
sudo mount --mkdir -t tmpfs -o defaults,size=20G tmpfs /mnt/chroots/arch

# 使用 tmpfs 构建
extra-x86_64-build -c -r /mnt/chroots/arch

# 构建后，卸载
sudo umount /mnt/chroots/arch
```

## 重置损坏的 Chroot

如果 chroot 损坏或被污染：

```bash
# 使用便捷脚本（自动重置）
extra-x86_64-build -c

# 或手动删除 chroot
sudo rm -rf /var/lib/archbuild/extra-x86_64
# 下次构建将重新创建它
```

## 为不同仓库构建

| 仓库 | 构建脚本 | 使用场景 |
|------------|-------------|----------|
| extra (stable) | `extra-x86_64-build` | 大多数软件包、AUR 提交 |
| extra-testing | `extra-testing-x86_64-build` | 预发布测试 |
| extra-staging | `extra-staging-x86_64-build` | 主要重建、依赖链 |
| multilib | `multilib-build` | 32位库 |
| multilib-testing | `multilib-testing-build` | 测试 32位软件包 |
| multilib-staging | `multilib-staging-build` | 主要 32位重建 |

## Clean Chroot 中的自定义依赖

**对于依赖于其他自定义软件包的软件包：**

```bash
# 方法 1：使用便捷脚本（推荐）
extra-x86_64-build -- -I ~/pkgs/custom-dep-1.0-1-x86_64.pkg.tar.zst

# 方法 2：使用手动 chroot
makechrootpkg -c -r $CHROOT -I custom-dep-1.0-1-x86_64.pkg.tar.zst

# 多个依赖
extra-x86_64-build -- \
  -I ~/pkgs/dep1-1.0-1-x86_64.pkg.tar.zst \
  -I ~/pkgs/dep2-2.0-1-x86_64.pkg.tar.zst
```

## 主要重建（多个相互依赖的软件包）

**首选：使用 staging 仓库：**

```bash
# 1. 针对 extra 构建第一个软件包，推送到 staging
extra-x86_64-build
# ... 推送到 extra-staging ...

# 2. 针对 staging 构建剩余软件包
extra-staging-x86_64-build
```

**替代方案：在 chroot 中安装构建的软件包（不推荐用于分发）：**

```bash
# 构建第一个软件包
extra-x86_64-build

# 使用 -n 构建后续软件包（安装到 chroot 中而不清理）
makechrootpkg -n -r /var/lib/archbuild/extra-x86_64

# 此方法：
# - 在构建之间保持 chroot 脏
# - 本地开发更快
# - NOT（不）适合分发（依赖可能错误）
```

## 通过 Clean Chroot 传递 makepkg 参数

```bash
# 强制运行 check()
extra-x86_64-build -- --check

# 跳过完整性检查（仅开发）
extra-x86_64-build -- --skipchecksums

# 多个参数
extra-x86_64-build -- --check --noconfirm

# 使用自定义依赖
extra-x86_64-build -- -I custom.pkg.tar.zst --check
```

## Clean Chroot 构建故障排除

| 问题 | 解决方案 |
|---------|----------|
| "chroot is locked" | 移除 `/var/lib/archbuild/extra-x86_64.lock` |
| Chroot 更新失败 | 使用 `-c` 标志重置：`extra-x86_64-build -c` |
| 空间不足错误 | 清理 chroot 中的旧软件包缓存 |
| 权限被拒绝 | 使用 `sudo` 运行或确保用户在 `wheel` 组中 |
| 找不到自定义仓库 | 编辑 `$CHROOT/root/etc/pacman.conf` |
### 手动 Clean Chroot 设置（高级）

**仅在需要自定义 chroot 位置或配置时才需要：**

```bash
# 1. 安装 devtools
sudo pacman -S devtools

# 2. 创建 chroot 目录
mkdir ~/chroot
CHROOT=$HOME/chroot

# 3. 初始化 chroot（root 子目录是强制的）
mkarchroot $CHROOT/root base-devel

# 4. （可选）编辑 chroot 配置
# - 镜像列表：$CHROOT/root/etc/pacman.d/mirrorlist
# - Pacman 配置：$CHROOT/root/etc/pacman.conf
# - Makepkg 配置：~/.makepkg.conf（由 makechrootpkg 使用）

# 5. 构建前更新 chroot
arch-nspawn $CHROOT/root pacman -Syu

# 6. 构建软件包（在 PKGBUILD 目录中运行）
makechrootpkg -c -r $CHROOT

# -c 标志确保工作 chroot 在构建前被清理
```

**自定义 pacman.conf/makepkg.conf（谨慎使用）：**
```bash
mkarchroot -C custom-pacman.conf -M custom-makepkg.conf $CHROOT/root base-devel
```

**使用自定义依赖构建：**
```bash
makechrootpkg -c -r $CHROOT -I custom-dep-1.0-1-x86_64.pkg.tar.zst
```

**向 makepkg 传递参数：**
```bash
# 强制运行 check()
makechrootpkg -c -r $CHROOT -- --check

# 跳过完整性检查（用于开发）
makechrootpkg -c -r $CHROOT -- --skipchecksums
```

