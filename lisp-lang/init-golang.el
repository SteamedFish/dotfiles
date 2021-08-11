;;; init-golang.el --- golang mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  golang mode

;;; Code:

(leaf go-mode
  :url https://github.com/dominikh/go-mode.el
  :straight t
  :ensure-system-package gopls)

(leaf go-guru
  :url https://github.com/dominikh/go-mode.el
  :straight t
  :config
  (unless (executable-find "guru")
    (cond
     (IS-LINUX (system-packages-ensure "go-tools"))
     (IS-MAC (async-shell-command
              "GO111MODULE=on go get -u golang.org/x/tools/cmd/guru")))))

(leaf go-dlv
  :url https://github.com/benma/go-dlv.el
  :doc "Go Delve: Debug Go Programs with the GUD"
  :straight t
  :config
  (unless (executable-find "dlv")
    (cond
     (IS-LINUX (system-packages-ensure "delve"))
     (IS-MAC (async-shell-command
              "GO111MODULE=on go get -u github.com/go-delve/delve/cmd/dlv")))))

(leaf go-fill-struct
  :url https://github.com/s-kostyaev/go-fill-struct
  :straight t
  :config
  (unless (executable-find "fillstruct")
    (async-shell-command
     "GO111MODULE=on go get -u github.com/davidrjenni/reftools/cmd/fillstruct")))

(leaf go-impl
  :url https://github.com/emacsorphanage/go-impl
  :straight t
  :config
  (unless (executable-find "impl")
    (cond
     (IS-LINUX (system-packages-ensure "go-impl"))
     (IS-MAC (async-shell-command
              "GO111MODULE=on go get -u github.com/josharian/impl@latest"))))
  (unless (executable-find "godoc")
    (cond
     (IS-LINUX (system-packages-ensure "go-tools"))
     (IS-MAC (async-shell-command
              "GO111MODULE=on go get -u golang.org/x/tools/cmd/godoc")))))

(leaf flycheck-golangci-lint
  :url https://github.com/weijiangan/flycheck-golangci-lint
  :straight t
  :ensure-system-package golangci-lint
  :hook (go-mode-hook . flycheck-golangci-lint-setup))

(leaf go-eldoc
  :url https://github.com/emacsorphanage/go-eldoc
  :straight t
  :hook (go-mode-hook . go-eldoc-setup)
  :config
  (cond
   (IS-LINUX
    (system-packages-ensure "gocode-daemon"))
   (IS-MAC
    (unless (executable-find "gocode")
      (async-shell-command
       "GO111MODULE=on go get -u github.com/nsf/gocode")))))

(leaf go-tag
  :url https://github.com/brantou/emacs-go-tag
  :straight t
  :config
  (unless (executable-find "gomodifytags")
    (cond
     (IS-LINUX
      (system-packages-ensure "gomodifytags"))
     (IS-MAC
      (async-shell-command
       "GO111MODULE=on go get -u github.com/fatih/gomodifytags@latest")))))

(leaf go-gen-test
  :url https://github.com/s-kostyaev/go-gen-test
  :straight t
  :config
  (unless (executable-find "gotests")
    (async-shell-command
     "GO111MODULE=on go get -u github.com/cweill/gotests/...")))

(leaf gotest
  :url https://github.com/nlamirault/gotest.el
  :straight t)

(leaf go-playground
  :url https://github.com/kosh04/emacs-go-playground
  :straight t)

(provide 'init-golang)
;;; init-golang.el ends here
