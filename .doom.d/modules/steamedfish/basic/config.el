;;; steamedfish/basic/config.el  -*- lexical-binding: t; -*-

(setq
 user-full-name "SteamedFish"
 user-mail-address "steamedfish@hotmail.com"
 frame-title-format '("%b - " user-full-name "'s Emacs")
 calendar-latitude 23.1247
 calendar-longitude 113.3612
 calendar-location-name "Tianhe, Guangzhou"
 message-log-max 10000
 delete-by-moving-to-trash t)

(when IS-MAC
  (setq mac-system-move-file-to-trash-use-finder t))

(add-hook! (prog-mode text-mode conf-mode)
  (setq show-trailing-whitespace t))

(use-package! generic-x
  :config
  (setq generic-use-find-file-hook t)
  (setq generic-extras-enable-list
    (append generic-default-modes
      generic-mswindows-modes
      generic-unix-modes
      generic-other-modes))
  ;; must reload generic-x after setting extras-enable-list
  (load "generic-x"))

(when IS-ANDROID
  (setq browse-url-browser-function 'browse-url-xdg-open))

(after! evil-escape
  (setq evil-escape-key-sequence nil))

(after! dired
  ;; emacs by default disable this command
  (put 'dired-find-alternate-file 'disabled nil)
  ;; make dired automatically refresh buffer after any file change
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (map!
   :map dired-mode-map
   ;; don't create new buffer when navigating
   :n "<return>"  #'dired-find-alternate-file
   :n "^"         (λ! (find-alternate-file ".."))))

(after! deft
  (setq deft-directory "~/Dropbox/org/"))

(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(after! projectile
  (projectile-add-known-project "~/dotfiles")
  (projectile-add-known-project "~/.emacs.d")
  (dolist (dir '("~/work/" "~/dumbo/" "~/Dropbox/" "~/Qsync/Work/" "~/dotfiles/"))
    (when (file-directory-p dir)
      (add-to-list 'projectile-project-search-path dir))))

(after! evil
  ;; do not quit emacs with those ex commands
  (evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)
  (evil-ex-define-cmd "x[it]" (lambda() (interactive) (save-buffer) (kill-buffer-and-window))))

(load! "email.el")
