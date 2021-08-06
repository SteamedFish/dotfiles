;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

;; add to load-path
(push (expand-file-name "lisp-core" user-emacs-directory) load-path)
(push (expand-file-name "lisp-theme" user-emacs-directory) load-path)
(push (expand-file-name "lisp-tools" user-emacs-directory) load-path)
(push (expand-file-name "lisp-lang" user-emacs-directory) load-path)

(defconst my-data-dir (concat user-emacs-directory ".local/")
  "The root directory where I put data files")
(unless (file-exists-p my-data-dir)
  (make-directory my-data-dir))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-ANDROID
  ;; If running inside Android Termux
  (string-match-p "-linux-android$" system-configuration))

;; lisp-core
(require 'init-earlyinit)
(require 'init-packages)
(require 'init-setups)
(require 'init-helpers)
(require 'init-keybindings)
(require 'init-gcmh)

;; lisp-theme
(require 'init-theme)

;; lisp-tools
(require 'init-evil)
(require 'init-calendar)
(require 'init-smalltools)
(require 'init-eaf)
(require 'init-telega)

;; lisp-lang
(require 'init-generic-x)
(require 'init-puppet)
(require 'init-elisp)


(provide 'init)
;;; init.el ends here
