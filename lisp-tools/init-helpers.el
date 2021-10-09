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
  :doc "helpful-callable
        helpful-function
        helpful-macro
        helpful-command
        helpful-key
        helpful-variable
        helpful-at-point"
  :straight t
  :bind
  (([remap describe-function]             . helpful-callable)
   ([remap describe-command]              . helpful-command)
   ([remap describe-variable]             . helpful-variable)
   ([remap describe-key]                  . helpful-key)
   ([remap describe-symbol]               . helpful-symbol)
   ([remap Info-goto-emacs-command-node]  . helpful-function)))


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
  :when IS-GUI
  :straight t
  :setq (which-key-posframe-poshandler . 'posframe-poshandler-frame-bottom-center)
  :global-minor-mode t)

(leaf ibuffer
  :tag "builtin"
  :straight nil
  :bind (([remap list-buffers] . ibuffer)))

(leaf ibuffer-projectile
  :url https://github.com/purcell/ibuffer-projectile
  :straight t
  :hook
  (ibuffer-hook . ibuffer-projectile-set-filter-groups))

(leaf ibuffer-vc
  :url https://github.com/purcell/ibuffer-vc
  :straight t
  :disabled t
  :hook
  (ibuffer-hook . ibuffer-vc-set-filter-groups-by-vc-root))

(leaf help
  :tag "builtin"
  :straight nil
  :setq (help-window-select . t))

(leaf popper
  :url https://github.com/karthink/popper
  :straight t
  :setq
  (popper-reference-buffers . '(("\\*Messages\\*$"
                                 helpful-mode
                                 help-mode)))
  :global-minor-mode t
  :config
  (when (display-graphic-p)
    (setq popper-display-function #'display-buffer-in-child-frame))
  (setq   popper-group-function   #'popper-group-by-projectile))

(leaf ido
  :tag "builtin"
  :unless (or (file-directory-p (straight--build-dir "swiper"))
              (file-directory-p (straight--build-dir "helm"))
              (file-directory-p (straight--build-dir "vertico"))
              (file-directory-p (straight--build-dir "selectrum")))
  :global-minor-mode t
  :setq (ido-enable-flex-matching . t))

(leaf amx
  :url https://github.com/DarwinAwardWinner/amx
  :straight t
  :after ido
  :unless (or (file-directory-p (straight--build-dir "swiper"))
              (file-directory-p (straight--build-dir "helm"))
              (file-directory-p (straight--build-dir "vertico"))
              (file-directory-p (straight--build-dir "selectrum")))
  :bind ([remap execute-extended-command] . amx)
  :setq
  (amx-history-length . 20))

(leaf maple-minibuffer
  :url https://github.com/honmaple/emacs-maple-minibuffer
  :straight (maple-minibuffer :host github :repo "honmaple/emacs-maple-minibuffer")
  :when IS-GUI
  :when IS-MAC ;; TODO: not working on Linux
  ;; :unless (file-directory-p (straight--build-dir "ivy-posframe"))
  :global-minor-mode t
  :setq
  (maple-minibuffer:position-type . 'frame-center)
  (maple-minibuffer:width         . 0.7)
  (maple-minibuffer:action        . '(read-from-minibuffer read-string))
  (maple-minibuffer:ignore-action . '(evil-ex
                                      eval-expression
                                      evil-ex-search-forward
                                      evil-ex-search-backward)))

(provide 'init-helpers)
;;; init-helpers.el ends here
