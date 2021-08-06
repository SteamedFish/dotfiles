;;; init-smalltools.el --- small tools that don't have a long config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  small tools that don't have a long config

;;; Code:

(leaf ssh-deploy
  :url "https://github.com/cjohansson/emacs-ssh-deploy"
  :straight t
  :init
  (push '(ssh-deploy-on-explicit-save . t) safe-local-variable-values))

(leaf wakatime-mode
  :url "https://github.com/wakatime/wakatime-mode"
  :straight t
  :blackout t
  :global-minor-mode global-wakatime-mode)

(leaf rfc-mode
  :url "https://github.com/galdor/rfc-mode"
  :straight t
  :setq `((rfc-mode-directory . ,(concat my-data-dir "data/rfc/"))))

(leaf all-the-icons
  :url "https://github.com/domtronn/all-the-icons.el"
  :straight t)

(leaf netease-cloud-music
  :url "https://github.com/SpringHan/netease-cloud-music.el"
  :require (cl-macs)
  :straight (netease-cloud-music
             :host github
             :repo "SpringHan/netease-cloud-music.el")
  :ensure-system-package (mpv node)
  :custom
  `(netease-cloud-music-cache-directory . ,(concat my-data-dir "data/netease-cloud-music"))
  :config
  (unless (file-directory-p (concat netease-cloud-music-cache-directory "/api"))
    (netease-cloud-music-download-api))
  :commands (netease-cloud-music eaf-open-netease-cloud-music))

(provide 'init-smalltools)
;;; init-smalltools.el ends here
