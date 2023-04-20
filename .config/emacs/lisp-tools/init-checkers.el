;;; init-checkers.el --- checkers -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  checkers

;;; Code:

(leaf flycheck
  :url https://www.flycheck.org/
  :straight t
  :blackout t
  :global-minor-mode global-flycheck-mode)

(leaf flycheck-popup-tip
  :url https://github.com/flycheck/flycheck-popup-tip
  :straight t
  :when IS-GUI
  :hook (flycheck-mode-hook . flycheck-popup-tip-mode))

(leaf flycheck-pos-tip
  :url https://github.com/flycheck/flycheck-pos-tip
  :straight t
  :unless IS-GUI
  :hook (flycheck-mode-hook . flycheck-pos-tip-mode))

(leaf flycheck-posframe
  :url https://github.com/alexmurray/flycheck-posframe
  :straight t
  :when IS-GUI
  :hook
  (flycheck-mode-hook                  . flycheck-posframe-mode)
  (flycheck-posframe-inhibit-functions . company--active-p)
  (flycheck-posframe-inhibit-functions . evil-insert-state-p)
  (flycheck-posframe-inhibit-functions . evil-replace-state-p))

(leaf spell-fu
  :url https://gitlab.com/ideasman42/emacs-spell-fu
  :straight t
  ;; "spell-fu is much faster than ispell. But I cannot configure it correctly"
  :disabled t
  :setq
  `(spell-fu-directory . ,(concat my-data-dir "etc/spell-fu/"))
  ;; TODO: exclude more faces
  :global-minor-mode global-spell-fu-mode)

(leaf flyspell
  :tag "builtin"
  :blackout (flyspell-mode flyspell-prog-mode)
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  (conf-mode-hook . flyspell-prog-mode)
  (yaml-mode-hook . flyspell-prog-mode)
  :config

  (pcase (cond ((executable-find "aspell")    'aspell)
               ((executable-find "hunspell")  'hunspell)
               ((executable-find "enchant-2") 'enchant))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra"
                               "--run-together"))

     (unless ispell-personal-dictionary
       (setq ispell-personal-dictionary
             (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                               (concat my-data-dir "etc/")))))
    (`hunspell
     (setq ispell-program-name "hunspell"))
    (`enchant
     (setq ispell-program-name "enchant-2"))
    (_ (system-packages-ensure "aspell"))))


(leaf flyspell-correct
  :url https://github.com/d12frosted/flyspell-correct
  :straight t
  :bind
  ([remap ispell-word] . flyspell-correct-at-point)
  (:flyspell-mode-map
   ("C-;" . flyspell-correct-wrapper)))

(leaf flyspell-correct-ivy
  :url https://github.com/d12frosted/flyspell-correct
  :straight t)

(leaf flyspell-lazy
  :url https://github.com/rolandwalker/flyspell-lazy
  :doc "Improve Emacs flyspell responsiveness using idle timers."
  :straight t
  :disabled t
  :setq
  (flyspell-lazy-idle-seconds        . 1)
  (flyspell-lazy-window-idle-seconds . 3)
  :global-minor-mode flyspell-lazy-mode)

(leaf grammarly
  :url https://github.com/emacs-grammarly/grammarly
  :straight t
  :config
  (setq grammarly-username (auth-source-pass-get "login" "grammarly.com")
        grammarly-password (auth-source-pass-get 'secret "grammarly.com")))

(leaf keytar
  :url https://github.com/emacs-grammarly/keytar
  :straight t
  :pre-setq
  `(keytar-install-dir . ,(concat my-data-dir "data/keytar"))
  :config
  (unless (file-exists-p (concat my-data-dir
                                 "data/keytar/bin/keytar"))
    (keytar-install)))

(leaf flycheck-grammarly
  :url https://github.com/emacs-grammarly/flycheck-grammarly
  :straight t
  :require t)

(leaf lsp-grammarly
  :url https://github.com/emacs-grammarly/lsp-grammarly
  :doc "Login manually with M-x lsp-grammarly-login"
  :straight t
  ;; Don't globally enable it for every text buffer.
  ;; Enable it manually.
  ;; :hook
  ;; (text-mode-hook . lsp)
  :config
  (unless (file-exists-p
           (concat lsp-server-install-dir
                   "npm//@emacs-grammarly/unofficial-grammarly-language-server/bin/unofficial-grammarly-language-server"))
    (lsp-install-server nil 'grammarly-ls)))

(provide 'init-checkers)
;;; init-checkers.el ends here
