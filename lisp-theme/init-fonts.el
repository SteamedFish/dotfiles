;;; init-fonts.el --- setup emacs fonts -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup emacs fonts

;;; Code:

(leaf mac-win
  :tag "builtin"
  :when (eq window-system 'mac)
  :global-minor-mode mac-auto-operator-composition-mode)

(leaf cus-start
  :tag "builtin"
  :doc "install required fonts"
  :when IS-GUI
  :config
  (let* ((fonts-alist
          (cond
           (IS-LINUX '(("Rec Mono Duotone" . "ttf-recursive")
                       ("Sarasa Mono SC" . "ttf-sarasa-gothic")
                       ("Noto Sans Symbols" . "noto-fonts")
                       ("Noto Color Emoji" . "noto-fonts-emoji")))
           (IS-MAC '(("Rec Mono Duotone" . "font-recursive-code")
                     ("Recursive" . "font-recursive")
                     ("Sarasa Mono SC" . "font-sarasa-gothic")
                     ("Noto Color Emoji" . "font-noto-color-emoji")
                     ("Noto Sans CJK SC" . "font-noto-sans-cjk-sc")
                     ("Noto Sans Symbols" . "font-noto-sans-symbols")
                     ("Noto Sans Symbols2" . "font-noto-sans-symbols-2")))
           (t '(())))))

    (cl-loop for (key . value) in fonts-alist
             unless (member key (font-family-list))
             do (system-packages-ensure value))))


(leaf cus-start
  :tag "builtin"
  :when IS-GUI
  :config
  (set-face-attribute 'default nil :font "Rec Mono Duotone")
  (set-fontset-font t nil "Symbola" nil 'append))

(leaf face-remap
  :tag "builtin"
  :blackout buffer-face-mode
  :config
  (make-face 'my-align-by-sarasa)
  (set-face-font 'my-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
  (defun my-align-with-sarasa-font ()
    "aligh chinese and engligh by setting font to Sarasa"
    (interactive)
    (when (member "Sarasa Mono SC" (font-family-list))
      (setq buffer-face-mode-face 'my-align-by-sarasa)
      (buffer-face-mode))))

(leaf prog-mode
  :tag "builtin"
  :global-minor-mode global-prettify-symbols-mode)

(provide 'init-fonts)
;;; init-fonts.el ends here
