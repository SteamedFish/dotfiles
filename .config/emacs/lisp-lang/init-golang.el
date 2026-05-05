;;; init-golang.el --- golang mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  golang mode

;;; Code:

(leaf go-mode
  :url https://github.com/dominikh/go-mode.el
  :straight t
  :ensure-system-package gopls
  :hook
  (go-mode-hook . lsp-deferred))

(leaf ob-go
  :url https://github.com/pope/ob-go
  :straight t
  :config
  (add-to-list 'org-src-lang-modes '("go" . go)))

(leaf go-guru
  :url https://github.com/dominikh/go-mode.el
  :straight t
  ;; Disabled because guru was removed from x/tools and superseded by gopls.
  :disabled t
  :config
  (unless (executable-find "guru")
    (async-shell-command
     "GO111MODULE=on go install golang.org/x/tools/cmd/guru@latest")))

(leaf go-dlv
  :url https://github.com/benma/go-dlv.el
  :doc "Go Delve: Debug Go Programs with the GUD"
  :straight t
  :config
  (unless (executable-find "dlv")
    (cond
     (IS-LINUX (system-packages-ensure "delve"))
     (IS-MAC (async-shell-command
              "GO111MODULE=on go install github.com/go-delve/delve/cmd/dlv")))))

(leaf go-fill-struct
  :url https://github.com/s-kostyaev/go-fill-struct
  :straight t
  :config
  (unless (executable-find "fillstruct")
    (async-shell-command
     "GO111MODULE=on go install github.com/davidrjenni/reftools/cmd/fillstruct@latest")))

(leaf go-impl
  :url https://github.com/emacsorphanage/go-impl
  :straight t
  :config
  (unless (executable-find "impl")
    (cond
      (IS-LINUX (when (executable-find "yay")
                  (async-shell-command
                    (concat "yay "
                             "--sync "
                             "--aur "
                             "--noconfirm "
                             "--answerclean None "
                             "--answerdiff None "
                             "--answeredit None "
                             "--answerupgrade None "
                             "--removemake "
                             "--cleanafter "
                             "--redownloadall "
                             "go-impl"))))
      (IS-MAC (async-shell-command
               "GO111MODULE=on go install github.com/josharian/impl@latest"))))
  (unless (executable-find "godoc")
    (async-shell-command
     "GO111MODULE=on go install golang.org/x/tools/cmd/godoc@latest")))

(leaf flycheck-golangci-lint
  :url https://github.com/weijiangan/flycheck-golangci-lint
  :straight t
  :config
  (unless (executable-find "golangci-lint")
    (cond
     (IS-LINUX (system-packages-ensure "golangci-lint"))
     (IS-MAC (system-packages-ensure "golangci-lint"))))
  :hook (go-mode-hook . flycheck-golangci-lint-setup))

(leaf go-eldoc
  :url https://github.com/emacsorphanage/go-eldoc
  :straight t
  ;; Disabled because it depends on unmaintained gocode; gopls provides docs.
  :disabled t
  :hook (go-mode-hook . go-eldoc-setup)
  :config
  (unless (executable-find "gocode")
    (async-shell-command
     "GO111MODULE=on go install github.com/nsf/gocode")))

(leaf go-tag
  :url https://github.com/brantou/emacs-go-tag
  :straight t
  :config
  (unless (executable-find "gomodifytags")
    (cond
      (IS-LINUX (when (executable-find "yay")
                  (async-shell-command
                   (concat "yay "
                            "--sync "
                            "--aur "
                            "--noconfirm "
                            "--answerclean None "
                            "--answerdiff None "
                            "--answeredit None "
                            "--answerupgrade None "
                            "--removemake "
                            "--cleanafter "
                            "--redownloadall "
                            "gomodifytags"))))
      (IS-MAC
       (async-shell-command
        "GO111MODULE=on go install github.com/fatih/gomodifytags@latest")))))

(leaf go-gen-test
  :url https://github.com/s-kostyaev/go-gen-test
  :straight t
  :config
  (unless (executable-find "gotests")
    (cond
     (IS-LINUX (when (executable-find "yay")
                 (async-shell-command
                  (concat "yay "
                          "--sync "
                          "--aur "
                          "--noconfirm "
                          "--answerclean None "
                          "--answerdiff None "
                          "--answeredit None "
                          "--answerupgrade None "
                          "--removemake "
                          "--cleanafter "
                          "--redownloadall "
                          "gotests"))))
     (IS-MAC
      (async-shell-command
       "GO111MODULE=on go install github.com/cweill/gotests/gotests@latest")))))

(leaf gotest
  :url https://github.com/nlamirault/gotest.el
  :straight t)

(leaf go-playground
  :url https://github.com/kosh04/emacs-go-playground
  :straight t)

(provide 'init-golang)
;;; init-golang.el ends here
