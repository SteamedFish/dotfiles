;;; init-setups.el --- basic setups -*- lexical-binding: t; -*-

;;; Commentary:

;; This file bootstraps the configuration

;;; Code:


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq user-full-name "SteamedFish"
      user-mail-address "steamedfish@hotmail.com"
      frame-title-format '("%b - " user-full-name "'s Emacs"))

(provide 'init-setups)
;;; init-setups.el ends here
