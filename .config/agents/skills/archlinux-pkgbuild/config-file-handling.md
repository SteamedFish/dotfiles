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

## Sensitive Configuration Files (Credentials, Secrets)

**When config files contain passwords, API keys, or other sensitive data, they MUST be protected from unauthorized access.**

### Security Requirements

| Scenario | Permissions | Owner:Group | tmpfiles.d Type | Reasoning |
|----------|-------------|-------------|-----------------|-----------|
| **App writes to config** | `0660` | `appuser:appgroup` | `z` | Read/write by app only, not world-readable |
| **App reads only, admin edits** | `0640` | `root:appgroup` | `z` (or install -m) | App can read, only root writes, not world-readable |
| **Normal config (no secrets)** | `0644` | `root:root` | Default install | Standard readable config |

### Example: Web Application with Credentials

```bash
# In PKGBUILD
backup=(
    'etc/webapps/myapp/config.php'      # App config
    'etc/webapps/myapp/database.php'    # DATABASE CREDENTIALS - needs 0660
)

package() {
    # Install configs normally (pacman will install as root:root 0644)
    install -Dm644 config.php "$pkgdir/etc/webapps/myapp/config.php"
    install -Dm644 database.php "$pkgdir/etc/webapps/myapp/database.php"
    
    # tmpfiles.d will adjust ownership and permissions at install time
    install -Dm644 myapp.tmpfiles "$pkgdir/usr/lib/tmpfiles.d/myapp.conf"
}

# In myapp.tmpfiles:
# For apps that write their own config (e.g., web installers):
z /etc/webapps/myapp/config.php      0660 root  http  -   -
z /etc/webapps/myapp/database.php    0660 root  http  -   -  # Contains DB password!

# Alternative for read-only configs (admin edits manually):
z /etc/webapps/myapp/database.php    0640 root  http  -   -  # Only root writes
```

### Why Use systemd-tmpfiles for Permissions?

**DON'T do this in PKGBUILD:**
```bash
# ❌ WRONG - pacman tracks ownership/permissions, causes conflicts
install -Dm660 -o http -g http database.php "$pkgdir/etc/webapps/myapp/database.php"
```

**DO use tmpfiles.d:**
```bash
# ✅ CORRECT - pacman installs as root:root, tmpfiles.d adjusts at runtime
install -Dm644 database.php "$pkgdir/etc/webapps/myapp/database.php"
# Then tmpfiles.d changes ownership to root:http 0660
```

**Reasons:**
1. Package files should be owned by root in the package
2. Runtime ownership changes via tmpfiles.d (runs on install/boot)
3. Prevents pacman conflicts on upgrades
4. Standard Arch pattern for runtime-modified files

### When Application Writes Configuration

**Scenario:** Web applications with web-based installers (WordPress, Nextcloud, etc.)

**Problem:** Installer needs to write database credentials to `/etc/webapps/app/config.php`

**Solution:**
1. Directory must be writable by application user (via tmpfiles.d)
2. Config files must be writable AND not world-readable (0660)
3. Use tmpfiles.d type `z` to adjust permissions on existing files

**Example (.install file message):**
```bash
post_install() {
    echo "==> Configuration files in /etc/webapps/myapp/ are owned by root:http (0660)"
    echo "==> This allows the web installer to write database credentials"
    echo "==> Permissions are restricted - config files are NOT world-readable"
}
```

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
