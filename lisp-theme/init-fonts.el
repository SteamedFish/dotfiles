;;; init-fonts.el --- setup emacs fonts -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs fonts

;;; Code:

(leaf mac-win
  :tag "builtin"
  :when (eq window-system 'mac)
  :global-minor-mode mac-auto-operator-composition-mode)

;; ensure-system-package don't support brew casks very well
;; (leaf cus-start
;;   :tag "builtin"
;;   :when IS-MAC
;;   :when window-system
;;   :ensure-system-package (font-recursive
;;                           font-recursive-code
;;                           font-sarasa-gothic
;;                           font-noto-color-emoji
;;                           font-noto-emoji
;;                           font-noto-sans-cjk-sc
;;                           font-noto-sans-symbols
;;                           font-noto-sans-symbols-2))

(leaf cus-start
  :tag "builtin"
  :when IS-LINUX
  :when window-system
  :ensure-system-package (ttf-recursive
                          ttf-sarasa-gothic
                          noto-fonts
                          noto-fonts-cjk
                          noto-fonts-emoji
                          noto-fonts-extra))

(leaf cus-start
  :tag "builtin"
  :when window-system
  :config
  (set-face-attribute 'default nil :font "Rec Mono Duotone"))
(provide 'init-fonts)
;;; init-fonts.el ends here
