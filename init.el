;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

;; add to load-path
(push (expand-file-name "lisp-core" user-emacs-directory) load-path)
(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-earlyinit)
(require 'init-straight)
(require 'init-leaf)
(require 'init-setups)
(require 'init-evil)


(require 'init-gcmh)

(provide 'init)
;;; init.el ends here
