;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

;; add to load-path
(push (expand-file-name "lisp-core"  user-emacs-directory) load-path)
(push (expand-file-name "lisp-theme" user-emacs-directory) load-path)
(push (expand-file-name "lisp-tools" user-emacs-directory) load-path)
(push (expand-file-name "lisp-lang"  user-emacs-directory) load-path)

(defconst my-data-dir (expand-file-name (concat user-emacs-directory ".local/"))
  "The root directory where I put data files")
(unless (file-exists-p my-data-dir)
  (make-directory my-data-dir))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-ANDROID (eq system-type 'android))
(defconst IS-TERMUX
  ;; If running inside Android Termux
  (string-match-p "-linux-android$" system-configuration))

;; detect if we can run gui
(defconst IS-GUI
  (cond
   ((display-graphic-p) t)
   ((when window-system) t)
   ((string-match "--without-x" system-configuration-options) nil)
   ;; set to t if running in noninteractive mode; otherwise nil
   (t noninteractive)))

;; https://marek-g.github.io/posts/tips_and_tricks/emacs_on_android/
(when IS-ANDROID
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))
(when IS-ANDROID
  (setq tls-program '("gnutls-cli -p %p %h"
                      "gnutls-cli -p %p %h --protocols ssl3")))

;; lisp-core
(require 'init-earlyinit)
(require 'init-packages)
(require 'init-setups)
(require 'init-keybindings)
(require 'init-gcmh)

;; lisp-theme
(require 'init-theme)
(require 'init-fonts)
(require 'init-treemacs)

;; lisp-tools
(require 'init-notifier)
(require 'init-projects)
(require 'init-dired)
(require 'init-helpers)
(require 'init-evil)
(require 'init-ivy)
(require 'init-edittools)
(require 'init-company)
(require 'init-lsp)
(require 'init-checkers)
(require 'init-calendar)
(require 'init-smalltools)
(require 'init-version-control)
(require 'init-chinese)
(require 'init-term)
(require 'init-eaf)
(require 'init-pass)
(require 'init-telega)
(require 'init-ement)

;; lisp-lang
(require 'init-generic-x)
(require 'init-orgmode)
(require 'init-puppet)
(require 'init-elisp)
(require 'init-sh)
(require 'init-rust)
(require 'init-golang)
(require 'init-python)
(require 'init-yaml)
(require 'init-plantuml)
(require 'init-web)
(require 'init-debiandev)

(provide 'init)
;;; init.el ends here
