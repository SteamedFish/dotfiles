;;; steamedfish/telega/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :commands (telega)
  :config

  (telega-mode-line-mode 1)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 50 :quit nil :modeline t :select t)
  (set-popup-rule! "^◀[[({<].*[\])>}]$"
    :side 'right :size 50 :quit nil :modeline t :select t)

  (setq telega-use-tracking-for nil
        telega-known-inline-bots '("@shufmbot"
                                   "@jiebabot"
                                   "@toptoh_bot"
                                   "@nasy_nautc_bot"
                                   "@nasy_words_bot"
                                   "@PythiaGataBot")
        telega-chat-history-limit 100
        telega-sticker-set-download t
        telega-chat-button-width 28
        ;;telega-root-fill-column 48
        telega-chat-fill-column 37
        telega-chat-button-width 37
        telega-chat-show-deleted-messages-for '(all))

  ;; copied from https://github.com/sarg/dotfiles/
  ;; use ivy to choose different chats
  (when (featurep! :completion ivy)
    (load! "+ivy")
    (map! :map (telega-root-mode-map telega-chat-mode-map)
          "C-S-M-s-c" #'ivy-telega-chat-with))

  (when (featurep! :completion company)
    (add-hook 'telega-chat-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     (append '(telega-company-telegram-emoji
                               telega-company-username
                               telega-company-hashtag)
                             (when (telega-chat-bot-p telega-chatbuf--chat)
                               '(telega-company-botcmd)))))))

  (when (featurep! :editor evil)
    (map!
     (:map telega-msg-button-map
       "j" #'telega-button-forward
       "k" #'telega-button-backward
       "K" #'telega-msg-delete-marked-or-at-point)))

  ;; use RET to add newline and C-RET to send
  (when (featurep! :config default)
    (map! :map telega-chat-mode-map
      "RET"              #'+default/newline-below
      "<s-return>"       #'telega-chatbuf-input-send))

  (add-hook 'telega-chat-mode-hook
    (visual-line-mode t))

  (set-evil-initial-state! 'telega-chat-mode 'insert)

  ;; Sarasa Mono SC can make font align correctly,
  ;; even with mixed Chinese and English
  (when (member "Sarasa Mono SC" (font-family-list))
    (make-face 'telega-align-by-sarasa)
    (set-face-font 'telega-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
    (add-hook! '(telega-chat-mode-hook telega-root-mode-hook)
      (buffer-face-set 'telega-align-by-sarasa)))

  (when (featurep! :steamedfish chinese)
    ;; use chinese by default in telega
    (add-hook! 'telega-chat-mode-hook
      ;; active rime by default
      ;; for some unknown reason directly calling `active-input-method'
      ;; is not working, but it works with `run-at-time'
      ((lambda () (run-at-time nil nil 'activate-input-method "rime")))))

  (after! all-the-icons
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-root-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-chat-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-blue))))
