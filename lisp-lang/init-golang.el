;;; init-golang.el --- golang mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  golang mode

;;; Code:

(leaf go-mode
  :straight t)

(leaf go-dlv
  :straight t)

(leaf go-fill-struct
  :straight t)

(leaf go-impl
  :straight t)

(leaf flycheck-golangci-lint
  :straight t
  :ensure-system-package golangci-lint)

(leaf go-eldoc
  :straight t)

(leaf go-guru
  :straight t)

(leaf go-tag
  :straight t)

(leaf go-gen-test
  :straight t)

(leaf gotest
  :straight t)

(leaf go-playground
  :straight t)

(provide 'init-golang)
;;; init-golang.el ends here
