;;; init-pass.el --- config pass -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config pass

;;; Code:

(leaf pass
  :url https://github.com/NicolasPetton/pass
  :straight t
  :doc "M-x pass")

(leaf password-store
  :url https://git.zx2c4.com/password-store/
  :url https://github.com/zx2c4/password-store
  :straight t
  :setq
  (password-store-password-length . 12))

(leaf password-store-otp
  :url https://github.com/volrath/password-store-otp.el
  :straight t)

(leaf auth-source-pass
  :tag "builtin"
  :doc use (auth-source-pass-get "login" "xxxx") to get username
  :doc use (auth-source-pass-get 'secret "xxxx") to get password
  :config
  (auth-source-pass-enable))

(leaf ivy-pass
  :url https://github.com/ecraven/ivy-pass
  :straight t
  :doc "M-x ivy-pass")

(provide 'init-pass)
;;; init-pass.el ends here
