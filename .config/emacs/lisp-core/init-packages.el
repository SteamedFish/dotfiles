;;; init-packages.el --- setup for straight and leaf -*- lexical-binding: t; -*-

;;; Commentary:

;; setup for straight.el and leaf.el

;;; Code:

;;; bootstrap straight.el

;; straight.el bootstrap
(setq straight-recipes-emacsmirror-use-mirror t
      straight-repository-branch "develop")

(setq straight-disable-native-compile
      (when (fboundp 'native-comp-available-p)
        (not (native-comp-available-p))))

(setq straight-base-dir my-data-dir)
(setq straight-build-dir (format "build-%s" emacs-version))

;; This is the best way, but need python3 and watchexec to work
(if (and (executable-find "python3")
         (executable-find "watchexec"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications '(find-at-startup find-when-checking)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; prevent emacs's builtin org from being loaded
;;; `straight-use-package' both registers AND builds/loads the newer Org
;;; immediately, so no other package can pull in the builtin Org first
;;; (avoids "Org version mismatch").
(straight-use-package 'org)
(straight-use-package 'org-contrib)

;;; bootstrap leaf.el

(unless (fboundp 'leaf)
  (when (fboundp 'straight-use-package)
    (straight-use-package 'leaf)))

(leaf leaf
  :url https://github.com/conao3/leaf.el
  :setq
  ;; don't lazy-load any packages
  (leaf-defer-keywords    . nil)
  (leaf-expand-leaf-defer . nil))

(leaf leaf-keywords
  :url https://github.com/conao3/leaf-keywords.el
  :init
  (straight-use-package 'leaf-keywords)
  (straight-use-package 'system-packages)
  (straight-use-package 'blackout)
  :config
  ;; initialize leaf-keywords.el
  (leaf-keywords-init))

(leaf system-packages
  :url https://gitlab.com/jabranham/system-packages
  :setq (system-packages-noconfirm . t)
  :config
  (when IS-MAC
    ;; emacs-app-nightly will use apt-get for some reason
    (setq system-packages-package-manager 'brew
          system-packages-use-sudo        nil))
  (when IS-TERMUX
    (setq system-packages-package-manager 'apt
          system-packages-use-sudo        nil))
  ;; Run package-manager commands via plain shell `sudo' instead of TRAMP's
  ;; `/sudo::'.  TRAMP's sudo method needs an interactive password prompt,
  ;; which is unavailable in `--batch' (noninteractive) mode, so `update emacs'
  ;; failed with "exited abnormally with code 1" for every package-manager
  ;; install regardless of package state.  This box has passwordless sudo
  ;; (`sudo -n -v' succeeds), so a bare `sudo' works even in batch.
  (advice-add 'system-packages--run-command :around
              #'(lambda (orig-fun &rest fn-args)
                  (if (and system-packages-use-sudo
                           (executable-find "sudo"))
                      (let* ((action  (nth 0 fn-args))
                             (pack    (nth 1 fn-args))
                             (extra   (nth 2 fn-args))
                             (command (system-packages-get-command action pack extra)))
                        (async-shell-command (concat "sudo " command)
                                             "*system-packages*"))
                    (apply orig-fun fn-args)))))

(leaf blackout
  :url https://github.com/radian-software/blackout)

(leaf straight
  :url https://github.com/radian-software/straight.el
  :ensure-system-package (watchexec python))

;; experimental/unstable extension of straight.el
(leaf straight-x
  :url https://github.com/radian-software/straight.el
  :commands (straight-x-fetch-all straight-x-pull-all))

;; convert use-package declarations to leaf
(leaf leaf-convert
  :url https://github.com/conao3/leaf-convert.el
  :straight t
  :commands (leaf-convert-insert-template
             leaf-convert-from-use-package))

(provide 'init-packages)
;;; init-packages.el ends here
