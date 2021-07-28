;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

;; add to load-path
(push (expand-file-name "lisp-essential" user-emacs-directory) load-path)
(push (expand-file-name "lisp" user-emacs-directory) load-path)

(defconst my-data-dir (concat user-emacs-directory ".local/")
  "The root directory where I put data files")
(unless (file-exists-p my-data-dir)
  (make-directory my-data-dir))

(require 'init-earlyinit)
(require 'init-straight)
(require 'init-leaf)
(require 'init-setups)


(require 'init-gcmh)

(provide 'init)
;;; init.el ends here
