# Configuration File Handling in Arch Linux Packages

**User-modifiable config files MUST be listed in the backup=() array to prevent pacman from overwriting them.**

## backup=() Array Rule

**List ALL files in /etc that users might modify:**

```bash
# In PKGBUILD
backup=(
    'etc/myapp/main.conf'
    'etc/myapp/database.conf'
    'etc/myapp/logging.conf'
)
```

**CRITICAL: Paths in backup=() are relative to root, WITHOUT leading slash.**

✅ Correct: `'etc/myapp/main.conf'`  
❌ Wrong: `'/etc/myapp/main.conf'`

## Pacman Behavior with backup=()

| Scenario | Pacman Behavior | User Impact |
|----------|----------------|-------------|
| File NOT modified by user | Replaced with new version silently | Config updated automatically |
| File modified by user | Creates .pacnew with new version | User manually merges changes |
| File in backup=() on removal | Creates .pacsave backup | User config preserved after uninstall |

**Without backup=():** Pacman overwrites user modifications on every upgrade. Database credentials, custom settings, all lost.

## What to Include in backup=()

**INCLUDE:**
- Configuration files users customize (database settings, app config)
- Files containing credentials or secrets
- Files in /etc with site-specific settings

**EXCLUDE:**
- Read-only templates (/usr/share/...)
- Generated files (.cache, .pid)
- Files that should ALWAYS be updated (service files)

## Example PKGBUILD

```bash
pkgname=myapp
backup=(
    'etc/myapp/main.conf'      # Frequently customized
    'etc/myapp/db.conf'        # Contains credentials - MUST preserve
    'etc/myapp/logging.conf'   # Sometimes customized
)
# defaults.conf in /usr/share/ - NOT in backup (read-only template)

package() {
    # Config files in /etc
    install -Dm644 main.conf "$pkgdir/etc/myapp/main.conf"
    install -Dm644 db.conf "$pkgdir/etc/myapp/db.conf"
    install -Dm644 logging.conf "$pkgdir/etc/myapp/logging.conf"
    
    # Template in /usr/share (not in backup)
    install -Dm644 defaults.conf "$pkgdir/usr/share/myapp/defaults.conf"
}
```

## User Workflow with .pacnew Files

After upgrade with modified configs:

```bash
# Find .pacnew files
pacdiff

# Or manually
find /etc -name "*.pacnew"

# View differences
diff /etc/myapp/main.conf /etc/myapp/main.conf.pacnew

# Merge changes manually
# Then remove .pacnew file
rm /etc/myapp/main.conf.pacnew
```

**Resource:** https://man.archlinux.org/man/pacman.8.en (search "HANDLING CONFIG FILES")
