;;; early-init.el --- Emacs 27+ pre-init config -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

(push (expand-file-name "lisp-core" user-emacs-directory) load-path)
(require 'init-earlyinit)


;;; early-init.el ends here
