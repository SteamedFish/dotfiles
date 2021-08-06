;;; init-setups.el --- basic setups -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

(leaf startup
  :setq
  `(auto-save-list-file-prefix . ,(concat my-data-dir "autosave/")))

(leaf cus-start
  :doc "builtins"
  :tag "builtin" "internal"
  :ensure nil
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
  :init
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

(leaf indent
  :setq (tab-always-indent . nil))

(leaf files
  :setq (require-final-newline . t))

(leaf mule-cmds
  :config
  (set-language-environment "UTF-8"))

(leaf select
  :unless IS-WINDOWS
  :setq (selection-coding-system . 'utf-8))

(leaf cus-edit
  :ensure nil
  :tag "builtin" "internal"
  :init
  (custom-set-variables `(custom-file ,(concat my-data-dir "custom.el")))
  :config
  (when (and (bound-and-true-p custom-file) (file-exists-p custom-file))
    (load custom-file)))

(leaf no-littering
  :straight t
  :pre-setq `((no-littering-etc-directory . ,(expand-file-name "etc/" my-data-dir))
              (no-littering-var-directory . ,(expand-file-name "data/" my-data-dir))))

;; remember last location
(leaf saveplace
  :straight nil
  :config (save-place-mode +1))

(leaf uniquify
  :require t)

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf mouse
  :setq
  (mouse-yank-at-point . t))

(leaf ediff-wind
  :setq (ediff-choose-window-setup-function . 'ediff-setup-windows-plain))

(leaf files
  :config
  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat my-data-dir
                                                   "backups"))))))

(provide 'init-setups)
;;; init-setups.el ends here
