;;; init-helpers.el --- all the helpers -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  all the helpers
;;
;;; Code:


(leaf helpful
  :url "https://github.com/Wilfred/helpful"
  :straight t
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key]      . helpful-key)
   ([remap describe-symbol]   . helpful-symbol)))

(leaf elisp-demos
  :url "https://github.com/xuchunyang/elisp-demos"
  :straight t
  :after helpful
  :advice
  (:after helpful-update elisp-demos-advice-helpful-update))

(leaf which-key
  :url "https://github.com/justbur/emacs-which-key"
  :straight t
  :global-minor-mode t
  :blackout t)

(leaf which-key-posframe
  :url "https://github.com/yanghaoxie/which-key-posframe"
  :when window-system
  :straight t
  :setq (which-key-posframe-poshandler . 'posframe-poshandler-frame-bottom-center)
  :global-minor-mode t)

(leaf ibuffer
  :tag "builtin"
  :straight nil
  :bind (([remap list-buffers] . ibuffer)))

(leaf help
  :tag "builtin"
  :straight nil
  :custom (help-window-select . t))

(leaf ido
  :tag "builtin"
  :unless (or (straight--installed-p 'helm)
              (straight--installed-p 'counsel)
              (straight--installed-p 'vertico)
              (straight--installed-p 'selectrum))
  :global-minor-mode t
  :setq (ido-enable-flex-matching . t))

(leaf amx
  :url https://github.com/DarwinAwardWinner/amx
  :straight t
  :unless (or (straight--installed-p 'helm)
              (straight--installed-p 'counsel)
              (straight--installed-p 'vertico)
              (straight--installed-p 'selectrum))
  :bind ([remap execute-extended-command] . amx)
  :custom
  `(amx-save-file . ,(concat my-data-dir "data/amx-items"))
  (amx-history-length . 20))

(leaf mini-frame
  :url "https://github.com/muffinmad/emacs-mini-frame"
  :when window-system
  :custom
  '(mini-frame-show-parameters . '((top . 9990)
                                   (width . 1.0)
                                   (left . 0.5)
                                   (leight . 1)))
  :global-minor-mode t
  :straight t)

(provide 'init-helpers)
;;; init-helpers.el ends here
