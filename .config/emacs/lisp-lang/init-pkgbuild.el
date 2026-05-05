;;; init-pkgbuild.el --- PKGBUILD mode -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  PKGBUILD mode for Arch Linux package development

;;; Code:

(leaf pkgbuild-mode
  :url https://github.com/juergenhoetzel/pkgbuild-mode
  :straight t
  :mode "\\.PKGBUILD\\'"
  :hook
  (pkgbuild-mode-hook . (lambda ()
                          (setq indent-tabs-mode nil))))

(provide 'init-pkgbuild)
;;; init-pkgbuild.el ends here
