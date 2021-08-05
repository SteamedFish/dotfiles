;;; init-setups.el --- basic setups -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

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
  (add-hook 'after-init-hook
    (lambda ()
      (when (and (bound-and-true-p custom-file) (file-exists-p custom-file))
        (load custom-file)))))

(leaf no-littering
  :straight t
  :pre-setq `((no-littering-etc-directory . ,(expand-file-name "etc/" my-data-dir))
              (no-littering-var-directory . ,(expand-file-name "data/" my-data-dir))))

;; remember last location
(leaf saveplace
  :straight nil
  :config (save-place-mode +1))

(provide 'init-setups)
;;; init-setups.el ends here
