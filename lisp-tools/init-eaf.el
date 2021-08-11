;;; init-eaf.el --- config Emacs Application Framework -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config Emacs Application Framework

;;; Code:

(leaf epc
  :url "https://github.com/kiwanami/emacs-epc"
  :doc "required by eaf"
  :straight t
  :when window-system)

(leaf eaf
  :url "https://github.com/manateelazycat/emacs-application-framework"
  :when window-system
  :straight (eaf
             :host github
             :repo "manateelazycat/emacs-application-framework"
             :files ("*"))
  :commands (eaf-open-browser eaf-open eaf-open-browser-with-history
             eaf-open-camera eaf-open-terminal eaf-file-sender-qrcode
             eaf-file-sender-qrcode-in-dired eaf-file-receiver-qrcode
             eaf-open-airshare eaf-open-rss-reader eaf-create-mindmap
             eaf-open-mindmap eaf-open-office eaf-open-demo
             eaf-open-this-from-dired
             eaf-describe-bindings eaf-open-bookmark eaf-open-external
             eaf-get-path-or-url eaf-toggle-fullscreen eaf-share-path-or-url)
  :custom
  `(eaf-config-location . ,(concat my-data-dir "data/eaf/"))
  (eaf-find-alternate-file-in-dired . t)
  ;; (browse-url-browser-function . #'eaf-open-browser)
  (eaf-browser-continue-where-left-off . t)
  (eaf-browser-default-search-engine . "duckduckgo")
  (eaf-browser-dark-mode . t)
  (eaf-terminal-dark-mode . t)
  (eaf-pdf-dark-mode . "follow")
  (eaf-mindmap-dark-mode . "follow")
  (eaf-browse-blank-page-url . "https://duckduckgo.com/")
  (eaf-browser-enable-adblocker . t)
  (eaf-browser-remember-history . t))
  ;; install dependencies will cause eaf's repo modified,
  ;; which will trigger a rebuild, which cleans up installed dependencies
  ;; :config
  ;; (let* ((leaf-dir (file-name-directory (locate-library "eaf")))
  ;;        (default-directory eaf-dir))
  ;;   (unless (file-exists-p
  ;;            (expand-file-name
  ;;             (concat eaf-dir "app/terminal/node_modules")))
  ;;     (if (executable-find "yay")
  ;;         (async-shell-command "./install-eaf.sh --ignore-py-deps")
  ;;       (eaf-install-dependencies)))))


(leaf eaf-evil
  :url "https://github.com/manateelazycat/emacs-application-framework"
  :when window-system
  :after eaf)

(leaf eaf-org
  :url "https://github.com/manateelazycat/emacs-application-framework"
  :when window-system
  :after eaf)

(provide 'init-eaf)
;;; init-eaf.el ends here
