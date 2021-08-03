;;; init-telega.el --- setup telega -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup telega

;;; Code:

(leaf telega
  :straight t
  :commands telega
  :hook
  (telega-load-hook . telega-mode-line-mode)
  (telega-load-hook . telega-notifications-mode)
  `(telega-chat-mode-hook . ,(visual-line-mode t))
  (telega-chat-mode-hook . (lambda ()
                                 (set (make-local-variable 'company-backends)
                                      (append '(telega-company-telegram-emoji
                                                telega-company-username
                                                telega-company-botcmd
                                                telega-company-hashtag)
                                              (when (telega-chat-bot-p telega-chatbuf--chat)
                                                '(telega-company-botcmd))))))
  :bind (:telega-msg-button-map
         ("RET"        . newline)
         ("<s-return>" . telega-chatbuf-input-send))
  :custom
  `(telega-directory . ,(expand-file-name "~/.config/telega"))
  (telega-use-tracking-for . nil)
  (telega-usl-shorten-use-images . t)
  (telega-mnz-use-language-detection . 10)
  (telega-known-inline-bots . '("@shufmbot"
                                "@jiebabot"
                                "@toptoh_bot"
                                "@nasy_nautc_bot"
                                "@nasy_words_bot"
                                "@PythaGataBot"
                                "@DeezerMusicBot"))
  (telega-chat-history-limit . 100)
  (telega-sticker-set-download . t)
  (telega-symbol-reply . "↫")
  (telega-chat-show-deleted-messaes-for . '(all)))

(leaf telega-url-shorten
  :straight nil
  :after telega
  :global-minor-mode (global-telega-url-shorten-mode))


(leaf telega-mnz
  :straight nil
  :after telega
  :global-minor-mode global-telega-mnz-mode)

(provide 'init-telega)
;;; init-telega.el ends here
