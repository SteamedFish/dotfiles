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
  :setq-default (line-spacing . 0)
  :hook (text-mode-hook . (lambda () (setq-local line-spacing 1)))
  :config
  (let* ((fonts-alist
          (cond
           (IS-LINUX '(("Rec Mono Duotone"     . "ttf-recursive")
                       ("Sarasa Mono SC"       . "ttf-sarasa-gothic")
                       ("Victor Mono"          . "ttf-victor-mono")
                       ("VictorMono Nerd Font" . "ttf-victor-mono-nerd")
                       ("Noto Sans Symbols"    . "noto-fonts")
                       ("Noto Color Emoji"     . "noto-fonts-emoji")
                       ("HanaMinA"             . "ttf-hanazono")
                       ("Symbola"              . "ttf-symbola")))
           (IS-MAC   '(("Rec Mono Duotone"     . "font-recursive-code")
                       ("Recursive"            . "font-recursive")
                       ("Victor Mono"          . "font-victor-mono")
                       ("VictorMono Nerd Font" . "font-victor-mono-nerd-font")
                       ("Sarasa Mono SC"       . "font-sarasa-gothic")
                       ("HanaMinA"             . "font-hanamina")
                       ("Noto Sans CJK SC"     . "font-noto-sans-cjk-sc")
                       ("Noto Sans Symbols"    . "font-noto-sans-symbols")
                       ("Noto Sans Symbols 2"   . "font-noto-sans-symbols-2")))
           (t        '(())))))

    (cl-loop for (key . value) in fonts-alist
             unless (find-font (font-spec :name key))
             do (system-packages-ensure value))))


(leaf faces
  :tag "builtin"
  :when IS-GUI
  :unless noninteractive ;; `set-fontset-font' will cause segfault in noninteractive mode
  :config
  (set-face-attribute 'default nil :font (font-spec :family "Rec Mono Duotone" :size 14))
  (cond
   (IS-LINUX
    (set-face-font 'variable-pitch "Recursive Sans Casual Static"))
   (IS-MAC
    (set-face-font 'variable-pitch "Recursive")))
  (set-face-font 'fixed-pitch "Rec Mono Duotone")
  (let ((chinese-font (cond
                       (IS-MAC "Hiragino Sans GB")
                       (t "Noto Sans CJK SC"))))
    (dolist (charset '(han cjk-misc bopomofo))
      (set-fontset-font nil charset chinese-font nil 'prepend)
      (set-fontset-font nil charset "HanaMinA"   nil 'append)
      (set-fontset-font nil charset "HanaMinB"   nil 'append)))

  (set-fontset-font nil 'mathematical "Symbola"       nil 'prepend)

  (set-fontset-font nil 'emoji "Noto Color Emoji"     nil 'append)
  (set-fontset-font nil 'emoji "Apple Color Emoji"    nil 'prepend)
  (set-fontset-font nil 'emoji "Noto Sans Symbols"    nil 'append)
  (set-fontset-font nil 'emoji "Noto Sans Symbols2"   nil 'append)
  (set-fontset-font nil 'emoji "Symbola"              nil 'append)

  ;; font fallback
  (set-fontset-font t   nil    "Symbola"              nil 'append))

(leaf face-remap
  :tag "builtin"
  :blackout buffer-face-mode
  :when IS-GUI
  :config
  (defun my-align-font ()
    "Align chinese and english by adjust font sizes"
    (interactive)
    (make-variable-buffer-local 'face-font-rescale-alist)
    ;; make symbols smaller, 14 * 0.93 = 13 14 * 0.86 = 12, 14 * 0.79 = 11
    (add-to-list 'face-font-rescale-alist '("-Noto Sans CJK SC-"    . 0.86))
    (add-to-list 'face-font-rescale-alist '("-Pingfang SC-"         . 0.86))
    (add-to-list 'face-font-rescale-alist '("-Hiragino Sans GB-"    . 0.93))
    (add-to-list 'face-font-rescale-alist '("-Fira Sans-"           . 0.86))
    (add-to-list 'face-font-rescale-alist '("-Symbola-"             . 0.93))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-"   . 0.71)))

  (defun my-nerd-font ()
    "configure nerd font for buffer"
    (interactive)
    (setq-local buffer-face-mode-face '(:family "VictorMono Nerd Font Mono"))
    (buffer-face-mode t)))

(leaf prog-mode
  :tag "builtin"
  :when IS-GUI
  :setq
  (prettify-symbols-unprettify-at-point . 'right-edge)
  :global-minor-mode global-prettify-symbols-mode)

(leaf all-the-icons
  :url "https://github.com/domtronn/all-the-icons.el"
  :straight t
  :config
  (unless (or (find-font (font-spec :name "all-the-icons")) IS-GUI)
    (all-the-icons-install-fonts)))

(leaf mixed-pitch
  :url https://gitlab.com/jabranham/mixed-pitch
  :straight t
  :blackout t
  :hook
  (text-mode-hook    . mixed-pitch-mode)
  (helpful-mode-hook . mixed-pitch-mode))

(provide 'init-fonts)
;;; init-fonts.el ends here
