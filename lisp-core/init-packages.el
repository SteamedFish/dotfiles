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
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; prevent emacs's builtin org from being loaded

(straight-register-package 'org)
(straight-register-package 'org-contrib)

;;; bootstrap leaf.el

(unless (fboundp 'leaf)
  (when (fboundp 'straight-use-package)
    (straight-use-package 'leaf)))

(leaf leaf
  :url https://github.com/conao3/leaf.el
  :setq
  ;; don't lazy-load any packages
  (leaf-defer-keywords . nil)
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
  :setq (system-packages-noconfirm . t))

(leaf blackout
  :url https://github.com/raxod502/blackout)

(leaf straight
  :url https://github.com/raxod502/straight.el
  :ensure-system-package (watchexec python))

;; experimental/unstable extension of straight.el
(leaf straight-x
  :url https://github.com/raxod502/straight.el
  :commands (straight-x-fetch-all straight-x-pull-all))

(provide 'init-packages)
;;; init-packages.el ends here
