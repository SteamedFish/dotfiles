;;; init-telega.el --- setup telega -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup telega

;;; Code:

(leaf telega
  :url https://github.com/zevlg/telega.el
  :straight t
  :doc "use s for filter"
  :commands telega
  :ensure-system-package (cmake make gperf)
  :hook
  (telega-load-hook . telega-mode-line-mode)
  (telega-load-hook . telega-notifications-mode)
  (telega-chat-mode-hook . my-align-with-sarasa-font)
  (telega-root-mode-hook . my-align-with-sarasa-font)
  (telega-chat-mode-hook . (lambda () (visual-line-mode t)))
  (telega-chat-mode-hook . (lambda ()
                             (set (make-local-variable 'company-backends)
                                  (append '(telega-company-telegram-emoji
                                            telega-company-username
                                            telega-company-botcmd
                                            telega-company-hashtag)
                                          (when (telega-chat-bot-p telega-chatbuf--chat)
                                            '(telega-company-botcmd))))))
  :bind
  (:telega-chat-mode-map
   ("RET"         . newline)
   ("<s-return>"  . telega-chatbuf-input-send)
   ("C-c c"       . telega-chat-with))
  (:telega-root-mode-map
   ("C-c c"       . telega-chat-with))
  :setq
  `(telega-directory . ,(expand-file-name (concat my-data-dir "data/telega")))
  (telega-completing-read-function . 'ivy-completing-read)
  (telega-use-tracking-for . nil)
  (telega-usl-shorten-use-images . t)
  (telega-mnz-use-language-detection . 10)
  (telega-chat-ret-always-sends-message . nil)
  (telega-known-inline-bots . '("@shufmbot"
                                "@jiebabot"
                                "@toptoh_bot"
                                "@nasy_nautc_bot"
                                "@nasy_words_bot"
                                "@PythaGataBot"
                                "@DeezerMusicBot"
                                "@shuibiaobot"))
  (telega-chat-history-limit . 100)
  (telega-sticker-set-download . t)
  (telega-chat-show-deleted-messages-for . '(all))
  :config
  ;; (evil-set-initial-state 'telega-chat-mode 'insert)
  (add-to-list 'all-the-icons-mode-icon-alist
               '(telega-root-mode all-the-icons-fileicon "telegram"
                                  :heigt 1.0
                                  :v-adjust -0.2
                                  :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(telega-chat-mode all-the-icons-fileicon "telegram"
                                  :heigt 1.0
                                  :v-adjust -0.2
                                  :face all-the-icons-blue)))

(leaf telega-url-shorten
  :url https://github.com/zevlg/telega.el
  :straight nil
  :after telega
  :global-minor-mode (global-telega-url-shorten-mode))


(leaf telega-mnz
  :url https://github.com/zevlg/telega.el
  :straight nil
  :after telega
  :blackout t
  :global-minor-mode global-telega-mnz-mode)

(provide 'init-telega)
;;; init-telega.el ends here
