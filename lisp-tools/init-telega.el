;;; init-telega.el --- setup telega -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup telega

;;; Code:

(leaf telega
  :url https://github.com/zevlg/telega.el
  :straight t
  :doc "use s for filter, M-g to move and g a / g A for other functions"
  :doc "to make animated stickers move, install https://github.com/zevlg/tgs2png"
  :commands telega
  :ensure-system-package (cmake make gperf)
  :hook
  (telega-open-file-hook . telega-edit-file-mode)
  (telega-chat-mode-hook . my-align-font)
  (telega-root-mode-hook . my-align-font)
  (telega-chat-mode-hook . mixed-pitch-mode)
  (telega-root-mode-hook . mixed-pitch-mode)
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
  `(telega-directory                      . ,(expand-file-name (concat my-data-dir "data/telega")))
  `(telega-symbol-forward                 . ,(compose-chars ?💬 ?🠒))
  (telega-completing-read-function        . 'ivy-completing-read)
  (telega-use-tracking-for                . nil)
  (telega-chat-ret-always-sends-message   . nil)
  (telega-known-inline-bots               . '("@shufmbot"
                                              "@jiebabot"
                                              "@toptoh_bot"
                                              "@nasy_nautc_bot"
                                              "@nasy_words_bot"
                                              "@PythaGataBot"
                                              "@DeezerMusicBot"
                                              "@nixcaosaysbot"
                                              "@shuibiaobot"))
  (telega-chat-history-limit              . 100)
  (telega-sticker-set-download            . t)
  (telega-chat-fill-column                . 40)
  (telega-chat-show-deleted-messages-for  . '(all))
  :config
  (add-hook 'telega-load-hook #'telega-mode-line-mode)
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
                                  :face all-the-icons-blue))
  (defun my-telega--set-window-width (width)
    "Set the width of the telega buffer to WIDTH."
    (unless (one-window-p)
      (setq window-size-fixed t)
      (cond
       ((> (window-width) width)
        (shrink-window-horizontally (- (window-width) width)))
       ((< (window-width) width)
        (enlarge-window-horizontally (- width (window-width)))))))
  (add-hook 'telega-chat-mode-hook (lambda() (my-telega--set-window-width 52)))
  (add-hook 'telega-root-mode-hook (lambda() (my-telega--set-window-width 52))))

(leaf telega-url-shorten
  :url https://github.com/zevlg/telega.el
  :straight nil
  :after telega
  :setq
  (telega-url-shorten-use-images . t)
  :global-minor-mode (global-telega-url-shorten-mode))


(leaf telega-mnz
  :url https://github.com/zevlg/telega.el
  :straight nil
  :after telega
  :blackout t
  :setq
  (telega-mnz-use-language-detection . 10)
  :global-minor-mode global-telega-mnz-mode)

(leaf telega-alert
  :url https://github.com/zevlg/telega.el
  :straight nil
  :after telega
  (if IS-MAC
      (add-hook 'telega-load-hook #'telega-alert-mode)
    (add-hook 'telega-load-hook #'telega-notifications-mode)))

(leaf telega-transient
  :url https://github.com/zevlg/telega.el
  :straight nil
  :after telega
  :config
  (add-hook 'telega-load-hook #'telega-transient-mode))

(provide 'init-telega)
;;; init-telega.el ends here
