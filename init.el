;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

;; add to load-path
(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-startup)
(require 'init-setups)
(require 'init-straight)

;; load custom-file
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
