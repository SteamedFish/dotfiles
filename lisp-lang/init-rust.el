;;; init-rust.el --- rust mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  rust mode

;;; Code:

(leaf rustic
  :url https://github.com/brotzeit/rustic
  :ensure-system-package rust-analyzer
  :straight t)

(leaf rust-playground
  :url https://github.com/grafov/rust-playground
  :straight t)

(provide 'init-rust)
;;; init-rust.el ends here
