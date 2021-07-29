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
           (frame-title-format . '("%b - " user-full-name "'s Emacs"))))

(leaf cus-edit
  :ensure nil
  :tag "builtin" "internal"
  :init
  (custom-set-variables `(custom-file ,(concat my-data-dir "custom.el")))
  :config
  (add-hook 'after-init-hook
    (lambda ()
      (when (and (stringp custom-file) (file-exists-p custom-file))
        (load custom-file)))))

(leaf no-littering
  :straight t
  :pre-setq `((no-littering-etc-directory . ,(expand-file-name "etc/" my-data-dir))
              (no-littering-var-directory . ,(expand-file-name "data/" my-data-dir))))

(provide 'init-setups)
;;; init-setups.el ends here
