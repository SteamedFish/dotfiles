;;; init-smalltools.el --- small tools that don't have a long config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  small tools that don't have a long config

;;; Code:

(leaf ffap
  :tag "builtin"
  :doc ffap: find file at point
  :require t
  :setq
  (ffap-machine-p-local   . 'reject)
  (ffap-machine-p-known   . 'reject)
  (ffap-machine-p-unknown . 'reject)
  :config
  (ffap-bindings))

(leaf rg
  :url https://github.com/dajva/rg.el
  :doc "C-c s to search"
  :straight t
  :ensure-system-package (rg . ripgrep)
  :config
  ;; (rg-enable-menu)
  (rg-enable-default-bindings))

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
  :hook (rfc-mode-hook . mixed-pitch-mode)
  :setq `((rfc-mode-directory . ,(concat my-data-dir "data/rfc/"))))

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

(leaf explain-pause-mode
  :url https://github.com/lastquestion/explain-pause-mode
  :doc "use M-x explain-pause-top to find what makes emacs slow"
  :straight t
  :blackout t)

(leaf xr
  :url https://github.com/mattiase/xr
  :doc reverse of rx
  :straight t)

(leaf esup
  :url https://github.com/jschaf/esup
  :doc "use M-x esup to benchmark emacs startup time"
  :straight t)

(leaf youtube-dl
  :url https://github.com/skeeto/youtube-dl-emacs
  :straight t)

(leaf pcre2el
  :url https://github.com/joddie/pcre2el
  :doc "C-c /"
  :straight t
  :global-minor-mode rxt-global-mode)

(leaf netease-cloud-music
  :url "https://github.com/SpringHan/netease-cloud-music.el"
  :require (cl-macs)
  :straight (netease-cloud-music
             :host github
             :repo "SpringHan/netease-cloud-music.el")
  :ensure-system-package (mpv node)
  :setq
  `(netease-cloud-music-cache-directory . ,(concat my-data-dir "data/netease-cloud-music"))
  :config
  (unless (file-directory-p (concat netease-cloud-music-cache-directory "/api"))
    (require 'netease-cloud-music)
    (netease-cloud-music-download-api))
  :commands (netease-cloud-music eaf-open-netease-cloud-music))

(provide 'init-smalltools)
;;; init-smalltools.el ends here
