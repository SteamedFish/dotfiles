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

(leaf ibuffer
  :tag "builtin"
  :straight nil
  :bind (([remap list-buffers] . ibuffer)))

(leaf ido
  :tag "builtin"
  :unless (or (fboundp 'helm-mode) (fboundp 'ivy-mode) (fboundp 'vertico-mode))
  :global-minor-mode t
  :setq (ido-enable-flex-matching . t))

(leaf smex
  :url "https://github.com/nonsequitur/smex"
  :straight t
  :unless (or (fboundp 'helm-mode) (fboundp 'ivy-mode) (fboundp 'vertico-mode))
  :bind ([remap execute-extended-command] . smex)
  :custom `(smex-save-file . ,(concat my-data-dir "data/smex-items"))
  :config (smex-initialize))

(provide 'init-helpers)
;;; init-helpers.el ends here
