(with-eval-after-load "fcitx"
    (setq fcitx-active-evil-states '(insert emacs hybrid))
    (fcitx-aggressive-setup)
    (fcitx-prefix-keys-add "M-m"))

(with-eval-after-load "imenu-list"
    (setq imenu-list-focus-after-activation nil))

(with-eval-after-load "fill-column-indicator"
    (add-hook 'prog-mode-hook 'turn-on-fci-mode))

(with-eval-after-load "indent-guide"
    (add-hook 'prog-mode-hook 'indent-guide-mode))

(with-eval-after-load "pangu-spacing"
    (setq pangu-spacing-real-insert-separtor t))

(with-eval-after-load "org"
    (setq org-startup-indented t)
    (setq org-startup-folded 'showall))
