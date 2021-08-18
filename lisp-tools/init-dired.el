;;; init-dired.el --- configure dired -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  configure dired

;;; Code:

(leaf dired
  :tag "builtin"
  :setq
  (dired-recursive-deletes . 'always)
  (dired-recursive-copies . 'always)
  (dired-listing-switches . "-alh --group-directories-first")
  (dired-auto-revert-buffer . t)
  :config
  (when IS-MAC
    (unless (executable-find "gls")
      (system-packages-ensure "coreutils"))
    (setq insert-directory-program "gls")))

(leaf dired-git-info
  :url https://github.com/clemera/dired-git-info
  :straight t
  :bind
  (:dired-mode-map
   (")" . dired-git-info-mode)))

(leaf dired-rsync
  :url https://github.com/stsquad/dired-rsync
  :straight t
  :bind
  (:dired-mode-map
   ("C-c C-r" . dired-rsync)))

(leaf diredfl
  :url https://github.com/purcell/diredfl
  :straight t
  :global-minor-mode diredfl-global-mode)

(leaf all-the-icons-dired
  :url https://github.com/jtbm37/all-the-icons-dired
  :straight t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf dired-aux
  :tag "builtin"
  :require t)

(leaf dired-x
  :tag "builtin"
  :require t
  :setq
  :config
  (let ((cmd (cond
              (IS-MAC "open")
              (IS-LINUX "xdg-open")
              (IS-WINDOWS "start")
              (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  (setq dired-omit-files (concat dired-omit-files
                                 "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$")))

(leaf fd-dired
  :url https://github.com/yqrashawn/fd-dired
  :straight t
  :require t
  :ensure-system-package fd)

(provide 'init-dired)
;;; init-dired.el ends here
