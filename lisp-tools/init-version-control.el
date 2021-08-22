;;; init-version-control.el --- Version Control Tools -*- lexical-binding: t; -*-
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  Version Control Tools

;;; Code:

(leaf diff-hl
  :url "https://github.com/dgutov/diff-hl"
  :doc "use C-x v or mouse click"
  :straight t
  :custom
  (diff-hl-side . 'right)
  :config
  (when IS-GUI
    (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-posframe))
  :global-minor-mode global-diff-hl-mode
  :global-minor-mode global-diff-hl-show-hunk-mouse-mode
  :hook (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :hook (dired-mode-hook . diff-hl-dired-mode))

(leaf smerge-mode
  :tag "builtin"
  :doc "A lightweight alternative to emerge/ediff"
  :doc "enable the mode, then use C-c ^"
  :hook find-file-hook
  :disabled t
  :blackout t)

(leaf magit
  :url "https://magit.vc/"
  :straight t)

(leaf forge
  :url https://github.com/magit/forge
  :straight t)

(leaf transient
  :url "https://github.com/magit/transient"
  :straight t
  :setq
  (transient-enable-popup-navigation . t)
  (transient-show-common-commands . nil)
  (transient-highlight-mismatched-keys . t))

(leaf transient-posframe
  :url https://github.com/yanghaoxie/transient-posframe
  :straight t
  :global-minor-mode t)

(leaf git-modes
  :url https://github.com/magit/git-modes
  :straight t)

(leaf with-editor
  :url https://github.com/magit/with-editor
  :straight t
  :blackout with-editor-mode
  :hook
  (with-editor-mode-hook . evil-insert-state))

(provide 'init-version-control)
;;; init-version-control.el ends here
