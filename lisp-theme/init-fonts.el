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
                       ("Noto Color Emoji" . "noto-fonts-emoji")
                       ("Symbola" . "ttf-symbola")))
           (IS-MAC '(("Rec Mono Duotone" . "font-recursive-code")
                     ("Recursive" . "font-recursive")
                     ("Sarasa Mono SC" . "font-sarasa-gothic")
                     ("Noto Sans CJK SC" . "font-noto-sans-cjk-sc")
                     ("Noto Sans Symbols" . "font-noto-sans-symbols")
                     ("Noto Sans Symbols2" . "font-noto-sans-symbols-2")))
           (t '(())))))

    (cl-loop for (key . value) in fonts-alist
             unless (member key (font-family-list))
             do (system-packages-ensure value))))


(leaf faces
  :tag "builtin"
  :when IS-GUI
  :config
  (set-face-attribute 'default nil :font (font-spec :family "Rec Mono Duotone" :size 14))
  ;; add some symbol fonts for font failback
  (set-fontset-font t nil "Noto Color Emoji" nil 'append)
  (set-fontset-font t nil "Apple Color Emoji" nil 'append)
  (set-fontset-font t nil "Noto Sans Symbols" nil 'append)
  (set-fontset-font t nil "Noto Sans Symbols2" nil 'append)
  (set-fontset-font t nil "Symbola" nil 'append))

(leaf face-remap
  :tag "builtin"
  :blackout buffer-face-mode
  :when IS-GUI
  :config
  (make-face 'my-align-by-sarasa)
  (set-face-font 'my-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
  (defun my-align-with-sarasa-font ()
    "aligh chinese and engligh by setting font to Sarasa"
    (interactive)
    (when (member "Sarasa Mono SC" (font-family-list))
      (setq buffer-face-mode-face 'my-align-by-sarasa)
      (make-variable-buffer-local 'face-font-rescale-alist)
      ;; make symbols smaller
      (add-to-list 'face-font-rescale-alist '("-Noto Color Emoji-" . 0.8))
      (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.8))
      (add-to-list 'face-font-rescale-alist '("-Noto Sans Symbols-" . 0.8))
      (add-to-list 'face-font-rescale-alist '("-Noto Sans Symbols2-" . 0.8))
      (add-to-list 'face-font-rescale-alist '("-Symbola-" . 0.8))
      (buffer-face-mode))))

(leaf prog-mode
  :tag "builtin"
  :when IS-GUI
  :global-minor-mode global-prettify-symbols-mode)

(leaf all-the-icons
  :url "https://github.com/domtronn/all-the-icons.el"
  :straight t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts)))

(provide 'init-fonts)
;;; init-fonts.el ends here
