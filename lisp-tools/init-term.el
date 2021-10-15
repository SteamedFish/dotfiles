;;; init-smalltools.el --- term -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; term

;;; Code:

(leaf vterm
  :url https://github.com/akermu/emacs-libvterm
  :straight t
  :comment TODO: use buffer-local-mode to configure a nerd font for vterm
  :when (fboundp 'module-load)
  :hook
  (vterm-mode-hook . evil-insert-state)
  :setq
  (vterm-always-compile-module . t)
  :config
  (unless (executable-find "vterm-ctrl")
    (system-packages-ensure "libvterm")))

(leaf multi-vterm
  :url https://github.com/suonlight/multi-vterm
  :straight t
  :bind
  ;; FIXME: this remap is not working
  ([remap vterm] . multi-vterm)
  :config
  (evil-define-key 'normal vterm-mode-map (kbd ",c")  #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")  #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")  #'multi-vterm-prev))

(provide 'init-term)
;;; init-term.el ends here
