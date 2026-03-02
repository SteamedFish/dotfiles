# 语言打包共享基线

本文件用于 `compiled-languages` 与 `interpreted-languages` 的共享规则。

## 适用范围

- `compiled-languages` 与 `interpreted-languages` 仅保留语言特有差异。
- 核心 PKGBUILD 流程请使用 `archlinux-pkgbuild`。

## 共享规则

- 架构策略：仅当产物与架构无关时使用 `arch=('any')`；存在原生代码时必须使用具体架构。
- 依赖策略：严格区分 `depends`、`makedepends`、`checkdepends`；仅声明直接依赖。
- 安装路径：遵循 FHS（`/usr`、`/usr/lib`、`/usr/share`；用户可编辑配置放 `/etc` 并配 `backup=()`）。
- 构建隔离：面向提交/分发的包优先使用 clean chroot 构建。
- 验证门槛：宣称完成前，必须对 `PKGBUILD` 与构建产物都运行 `namcap`。
- 打包卫生：引用 `"$pkgdir"` 与 `"$srcdir"`；license 安装到 `/usr/share/licenses/$pkgname/`。

## 关联参考

- `advanced-workflow-and-build.zh-CN.md`
- `policy-quality-and-install-scripts.zh-CN.md`
- `validation-guide.md`
- `fhs-and-vendor-config.md`
