;;; init-treemacs.el --- setup emacs treemacs -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs treemacs

;;; Code:

(leaf treemacs
  :url https://github.com/Alexander-Miller/treemacs
  :straight t
  :setq
  (treemacs-is-never-other-window . t))

(leaf treemacs-evil
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(leaf treemacs-projectile
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(leaf treemacs-magit
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(leaf treemacs-icons-dired
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(leaf treemacs-perspective
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(leaf treemacs-persp
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(leaf treemacs-all-the-icons
  :url https://github.com/Alexander-Miller/treemacs
  :straight t)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
