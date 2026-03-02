# Shared Language Packaging Baseline

Use this file for rules shared by `compiled-languages` and `interpreted-languages`.

## Scope

- `compiled-languages` and `interpreted-languages` should focus on language-specific deltas only.
- For core PKGBUILD flow, use `archlinux-pkgbuild`.

## Shared Rules

- Architecture policy: use `arch=('any')` only for architecture-independent outputs; use concrete arches when native code is present.
- Dependency policy: keep `depends`, `makedepends`, and `checkdepends` strictly separated; list direct dependencies only.
- Install paths: comply with FHS (`/usr`, `/usr/lib`, `/usr/share`, `/etc` with `backup=()` where user-edited).
- Build isolation: prefer clean chroot builds for submission-quality packages.
- Validation gate: run `namcap` on both `PKGBUILD` and built package artifacts before claiming completion.
- Packaging hygiene: quote `"$pkgdir"` and `"$srcdir"`; install license to `/usr/share/licenses/$pkgname/`.

## Cross-References

- `advanced-workflow-and-build.md`
- `policy-quality-and-install-scripts.md`
- `validation-guide.md`
- `fhs-and-vendor-config.md`
