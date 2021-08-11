;;; init-setups.el --- basic setups -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

(leaf startup
  :tag "builtin"
  :setq
  `(auto-save-list-file-prefix . ,(concat my-data-dir "autosave/")))

(leaf cus-start
  :tag "builtin"
  :custom ((user-full-name . "SteamedFish")
           (user-mail-address . "steamedfish@hotmail.com")
           (frame-title-format . '("%b - " user-full-name "'s Emacs")))
  :setq-default
  (bidi-display-reordering . 'left-to-right)
  (bidi-paragraph-direction . 'left-to-right)
  (indent-tabs-mode . nil)
  (tab-width . 8)
  :setq
  `(read-process-output-max . ,(* 1024 1024))
  (tabify-regexp . "^\t* [ \t]+")
  (visible-bell . t)
  (fill-column . 80)
  (locale-coding-system . 'utf-8)
  (enable-recursive-minibuffers . t)
  (history-length . 1000)
  :hook
  (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  (text-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  (conf-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-charset-priority 'unicode)
  (cond
    (IS-MAC
      ;; mac-* variables are used by the special emacs-mac build of Emacs by
      ;; Yamamoto Mitsuharu, while other builds use ns-*.
      (setq mac-command-modifier      'super
            ns-command-modifier       'super
            mac-option-modifier       'meta
            ns-option-modifier        'meta
            ;; Free up the right option for character composition
            mac-right-option-modifier 'none
            ns-right-option-modifier  'none))
    (IS-WINDOWS
      (setq w32-lwindow-modifier 'super
            w32-rwindow-modifier 'super))))

(leaf ffap
  :tag "builtin"
  :custom
  (ffap-machine-p-known . 'reject))

(leaf simple
  :tag "builtin"
  :custom
  (async-shell-command-buffer . 'new-buffer))

(leaf mule-cmds
  :tag "builtin"
  :config
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))

(leaf mule
  :tag "builtin"
  :config
  (set-buffer-file-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (modify-coding-system-alist 'process "*" 'utf-8))

(leaf select
  :tag "builtin"
  :unless IS-WINDOWS
  :setq (selection-coding-system . 'utf-8))

(leaf cus-edit
  :tag "builtin"
  :init
  (custom-set-variables `(custom-file ,(concat my-data-dir "custom.el")))
  :config
  (when (and (bound-and-true-p custom-file) (file-exists-p custom-file))
    (load custom-file)))

(leaf no-littering
  :url "https://github.com/emacscollective/no-littering"
  :straight t
  :require t
  :pre-setq `((no-littering-etc-directory . ,(expand-file-name "etc/" my-data-dir))
              (no-littering-var-directory . ,(expand-file-name "data/" my-data-dir))))

(leaf exec-path-from-shell
  :url https://github.com/purcell/exec-path-from-shell
  :straight t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("GOPATH"
                                    "GO111MODULE"
                                    "GOPROXY"
                                    "SHELL"
                                    "SSH_AUTH_SOCK")))

;; remember last location
(leaf saveplace
  :tag "builtin"
  :global-minor-mode save-place-mode)

(leaf recentf
  :tag "builtin"
  :global-minor-mode t
  :config
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(leaf savehist
  :tag "builtin"
  :global-minor-mode t
  :pre-setq
  (savehist-addtional-variables . '(mark-ring
                                    global-mark-ring
                                    search-ring
                                    regexp-search-ring
                                    extended-command-history)))

(leaf uniquify
  :tag "builtin"
  :require t)

(leaf server
  :tag "builtin"
  :when window-system
  :require t
  :config (unless (server-running-p)
            (server-start)))

(provide 'init-setups)
;;; init-setups.el ends here
