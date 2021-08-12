;;; init-smalltools.el --- small tools that don't have a long config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  small tools that don't have a long config

;;; Code:

(leaf ssh-deploy
  :url "https://github.com/cjohansson/emacs-ssh-deploy"
  :straight t
  :pre-push ((safe-local-variable-values . '(ssh-deploy-on-explicit-save . t))))

(leaf wakatime-mode
  :url "https://github.com/wakatime/wakatime-mode"
  :straight t
  :blackout t
  :config
  (unless (executable-find "wakatime")
    (cond
     (IS-LINUX (system-packages-ensure "wakatime"))
     (IS-MAC (system-packages-ensure "wakatime-cli"))))
  :global-minor-mode global-wakatime-mode)

(leaf rfc-mode
  :url "https://github.com/galdor/rfc-mode"
  :straight t
  :setq `((rfc-mode-directory . ,(concat my-data-dir "data/rfc/"))))

(leaf all-the-icons
  :url "https://github.com/domtronn/all-the-icons.el"
  ;; TODO: automatically download fonts if not exist
  :straight t)

(leaf osx-trash
  :url https://github.com/emacsorphanage/osx-trash
  :straight t
  :when IS-MAC
  :ensure-system-package trash
  :config
  (osx-trash-setup))

(leaf keycast
  :url https://github.com/tarsius/keycast
  :doc "enable by keycast-mode or keycast-log-mode, keycast-mode is not working right now"
  :straight (keycast :host github :repo "tarsius/keycast"))

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
    (require 'netease-cloud-music)
    (netease-cloud-music-download-api))
  :commands (netease-cloud-music eaf-open-netease-cloud-music))

(provide 'init-smalltools)
;;; init-smalltools.el ends here
