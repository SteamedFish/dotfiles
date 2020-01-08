;;; steamedfish/telega/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :commands (telega)
  :hook (telega-chat-mode . doom-mark-buffer-as-real-h)
  :config

  (telega-mode-line-mode 1)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 60 :quit nil :modeline t :select t)
  (set-popup-rule! "^◀[[({<].*[\])>}]$"
    :side 'right :size 60 :quit nil :modeline t :select t)

  (setq telega-use-tracking t
        telega-known-inline-bots '("@shufmbot")
        telega-sticker-set-download t
        telega-chat-button-width 28
        ;;telega-root-fill-column 48
        telega-chat-fill-column 47
        telega-chat-button-width 47)

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
                     (append '(telega-company-emoji
                               telega-company-username
                               telega-company-hashtag)
                             (when (telega-chat-bot-p telega-chatbuf--chat)
                               '(telega-company-botcmd)))))))

  (when (featurep! :editor evil)
    (map!
     (:map telega-msg-button-map
       "k" nil
       "l" nil)))

  ;; use RET to add newline and C-RET to send
  (when (featurep! :config default)
    (map! :map telega-chat-mode-map
      "RET"              #'+default/newline-below
      "<C-return>"       #'telega-chatbuf-input-send))

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
      ;; active pyim by default
      ;; for some unknown reason directly calling `active-input-method'
      ;; is not working, but it works with `run-at-time'
      ((lambda () (run-at-time nil nil 'activate-input-method "pyim")))
      ;; don't do dynamic english, we always want chinese here
      (setq pyim-english-input-switch-functions
            (delete 'pyim-probe-dynamic-english pyim-english-input-switch-functions))))

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
