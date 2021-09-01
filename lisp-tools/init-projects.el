;;; init-projects.el --- configure dired -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  configure projects

;;; Code:

(leaf projectile
  :url https://projectile.mx
  :doc "C-x p"
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

(leaf counsel-projectile
  :url https://github.com/ericdanan/counsel-projectile
  :straight t
  :bind
  ;; counsel-projectile-git-grep
  ;; counsel-projectile-org-capture
  ;; counsel-projectile-org-agenda
  ;; counsel-projectile
  ([remap projectile-find-file] . counsel-projectile-find-file)
  ([remap projectile-find-file-dwim] . counsel-projectile-find-file-dwim)
  ([remap projectile-find-dir]  . counsel-projectile-find-dir)
  ([remap projectile-switch-to-buffer]  . counsel-projectile-switch-to-buffer)
  ([remap projectile-grep]  . counsel-projectile-grep)
  ([remap projectile-ag]  . counsel-projectile-ag)
  ([remap projectile-ripgrep]  . counsel-projectile-rg)
  ([remap projectile-switch-project]  . counsel-projectile-switch-project))

(provide 'init-projects)
;;; init-projects.el ends here
