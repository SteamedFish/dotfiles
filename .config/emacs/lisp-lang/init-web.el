;;; init-web.el --- web mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  web mode

;;; Code:

(leaf web-mode
  :url https://web-mode.org/
  :straight t
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.l?eex\\'"
  :mode "\\.sface\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :setq
  (web-mode-enable-html-entities-fontification . t)
  (web-mode-auto-close-style                   . 1)
  (web-mode-enable-auto-pairing                . t)
  (web-mode-enable-current-element-highlight   . t)
  :hook
  (web-mode-hook . lsp)
  (web-mode-hook . (lambda () (add-to-list 'company-backends
                                      '(company-css company-web-html))))
  :config
  (unless (file-exists-p
           (concat lsp-server-install-dir
                   "npm/vscode-langservers-extracted/bin/vscode-html-language-server"))
    (lsp-install-server nil 'html-ls))
  (unless (file-exists-p
           (concat lsp-server-install-dir
                   "npm/vscode-langservers-extracted/bin/vscode-css-language-server"))
    (lsp-install-server nil 'css-ls))
  (unless (file-exists-p
           (concat lsp-server-install-dir
                   "npm/vls/bin/vls"))
    (lsp-install-server nil 'vls)))

(leaf company-web
  :url https://github.com/osv/company-web
  :straight t)

(leaf rainbow-mode
  :url https://elpa.gnu.org/packages/rainbow-mode.html
  :straight t
  :blackout t
  :hook (css-mode-hook
         scss-mode-hook
         stylus-mode-hook))

(leaf css-mode
  :tag "builtin"
  :hook
  (css-mode-hook  . lsp)
  (scss-mode-hook . lsp))

(leaf counsel-css
  :url https://github.com/hlissner/emacs-counsel-css
  :straight t
  :hook
  (css-mode-hook . counsel-css-imenu-setup))

(leaf sass-mode
  :url https://github.com/nex3/sass-mode
  :straight t
  :hook
  (sass-mode-hook . (lambda () (push 'company-css company-backends)))
  (sass-mode-hook . lsp))

(leaf js-mode
  :tag "builtin"
  :hook
  (js-mode-hook     . lsp)
  (js-jsx-mode-hook . lsp)
  ;; there's several different lsp servers for js
  ;; trying deno to see if its good
  :ensure-system-package deno
  :setq
  (js-chain-indent  . t)
  (js-indent-level  . t))

(leaf js2-mode
  :url https://github.com/mooz/js2-mode
  :straight t)

(leaf rjsx-mode
  :url https://github.com/felipeochoa/rjsx-mode
  :straight t
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :ensure-system-package node)

(leaf pug-mode
  :url https://github.com/hlissner/emacs-pug-mode
  :straight t
  :hook
  (pug-mode-hook . (lambda () (push 'company-web-jade company-backends))))

(leaf slim-mode
  :url https://github.com/slim-template/emacs-slim
  :straight t
  :hook
  (slim-mode-hook . (lambda () (push 'company-web-slim company-backends))))

(leaf vue-mode
  :url https://github.com/AdamNiederer/vue-mode
  :straight t
  :disabled t
  :hook
  (vue-mode-hook . lsp))

(leaf vue-html-mode
  :url https://github.com/AdamNiederer/vue-html-mode/
  :straight t
  :disabled t)

(leaf emmet-mode
  :url https://github.com/smihica/emmet-mode/tree/6b2e554f7fd27f732810f4b14ea01e3c54b7b3da
  :straight t
  :blackout 'emmet-mode
  :hook
  (emmet-mode-hook . yas-minor-mode-on)
  ((css-mode-hook
    web-mode-hook
    html-mode-hook
    haml-mode-hook
    nxml-mode-hook
    rjsx-mode-hook
    reason-mode-hook
    vue-mode-hook) . emmet-mode)
  :setq
  (emmet-move-cursor-between-quotes . t))

(leaf haml-mode
  :url https://github.com/nex3/haml-mode
  :straight t)

(leaf jade-mode
  :url https://github.com/brianc/jade-mode
  :straight t)

(leaf typescript-mode
  :url https://github.com/emacs-typescript/typescript.el
  :straight t)

(leaf tide
  :url https://github.com/ananthakumaran/tide
  :doc "Tide - TypeScript Interactive Development Environment for Emacs "
  :straight t
  :hook
  (tide-mode-hook       . tide-hl-identifier-mode)
  (typescript-mode-hook . tide-setup)
  (before-save-hook     . tide-format-before-save))

(leaf js2-refactor
  :url https://github.com/js-emacs/js2-refactor.el
  :straight t
  :blackout t
  :hook
  (js2-mode-hook          . js2-refactor-mode)
  (rjsx-mode-hook         . js2-refactor-mode)
  (js2-refactor-mode-hook . evil-normalize-keymaps))

(leaf npm-mode
  :url https://github.com/mojochao/npm-mode
  :straight t
  :blackout t
  ;; :global-minor-mode npm-global-mode
  (js-mode-hook         . npm-mode)
  (typescript-mode-hook . npm-mode))

(provide 'init-web)
;;; init-web.el ends here
