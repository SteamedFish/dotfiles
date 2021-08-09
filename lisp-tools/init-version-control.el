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
  (when window-system
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
  :blackout t)

(leaf magit
  :url "https://magit.vc/"
  :straight t)

(leaf transient
  :url "https://github.com/magit/transient"
  :straight t
  :custom
  (transient-enable-popup-navigation . t)
  (transient-show-common-commands . nil)
  (transient-highlight-mismatched-keys . t))


(provide 'init-version-control)
;;; init-version-control.el ends here
