;;; steamedfish/tools/config.el -*- lexical-binding: t; -*-

;; gpg 可以读取在 emacs 中输入的密码
(use-package! pinentry
  :config (pinentry-start))

(after! treemacs
  (treemacs-tag-follow-mode t))

(after! calendar
  (setq
    calendar-latitude 23.1247
    calendar-longitude 113.3612
    calendar-location-name "Tianhe, Guangzhou"
    calendar-mark-holidays-flag t
    calendar-mark-diary-entries-flag t))

(use-package! eaf
  :when IS-LINUX
  :commands (eaf-open-browser eaf-open eaf-open-browser-with-history
              eaf-open-camera eaf-open-terminal eaf-file-sender-qrcode
              eaf-file-sender-qrcode-in-dired eaf-file-receiver-qrcode
              eaf-open-airshare eaf-open-rss-reader eaf-create-mindmap
              eaf-open-mindmap eaf-open-office eaf-open-demo
              eaf-open-this-from-dired
              eaf-describe-bindings eaf-open-bookmark eaf-open-external
              eaf-get-path-or-url eaf-toggle-fullscreen eaf-share-path-or-url)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (setq
    eaf-config-location (concat doom-local-dir "eaf/")
    eaf-find-alternate-file-in-dired t
    browse-url-browser-function #'eaf-open-browser
    eaf-browser-continue-where-left-off t
    eaf-browser-default-search-engine 'duckduckgo)
  (eaf-setq eaf-browser-dark-mode "true")
  (eaf-setq eaf-terminal-dark-mode "true")
  (eaf-setq eaf-pdf-dark-mode "true")
  (eaf-setq eaf-mindmap-dark-mode "true")
  (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com/")

  (defun eaf-org-open-file (file &optional link)
    "An wrapper function on `eaf-open'."
    (eaf-open file))

  ;; use `emacs-application-framework' to open PDF file: link
  (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file)))

(use-package! counsel-tramp
  :commands(counsel-tramp counsel-tramp-quit))

(use-package! eaf-evil
  :after eaf)

(use-package! eaf-org
  :after eaf)

(use-package! netease-cloud-music
  :commands (netease-cloud-music netease-cloud-music-change-repeat-mode))
