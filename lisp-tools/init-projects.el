;;; init-projects.el --- configure dired -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  configure projects

;;; Code:

(leaf projectile
  :url https://projectile.mx
  :straight t
  :global-minor-mode t
  :bind
  ([remap evil-jump-to-tag] . projectile-find-tag)
  ([remap find-tag] . projectile-find-tag)
  :setq
  (projectile-globally-ignored-files . '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes . '(".elc" ".pyc" ".o"))
  (projectile-ignored-projects '("~"))
  :config
  (when IS-MAC (blackout 'projectile-mode))
  (projectile-add-known-project (expand-file-name "~/.config/emacs"))
  (dolist (dir '("~/work/" "~/projects"))
    (when (file-directory-p dir)
      (add-to-list 'projectile-project-search-path (expand-file-name dir)))))

(provide 'init-projects)
;;; init-projects.el ends here
