;;; init-eaf.el --- config Emacs Application Framework -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config Emacs Application Framework

;;; Code:

(leaf epc
  :doc "required by eaf"
  :straight t)

(leaf eaf
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
  :require (eaf-evil eaf-org)
  :custom
  `(eaf-config-location . ,(concat my-data-dir "etc/eaf/"))
  (eaf-find-alternate-file-in-dired . t)
  (browse-url-browser-function . #'eaf-open-browser)
  (eaf-browser-continue-where-left-off . t)
  (eaf-browser-default-search-engine . 'duckduckgo)
  :config
  (eaf-setq eaf-browser-dark-mode "true")
  (eaf-setq eaf-terminal-dark-mode "true")
  (eaf-setq eaf-pdf-dark-mode "true")
  (eaf-setq eaf-mindmap-dark-mode "true")
  (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com/"))

(provide 'init-eaf)
;;; init-eaf.el ends here
