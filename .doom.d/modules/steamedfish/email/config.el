;;; steamedfish/email/config.el -*- lexical-binding: t; -*-

(after! sendmail
  (setq
    message-sendmail-envelope-from 'header
    mail-envelope-from 'header
    mail-interactive t
    mail-default-directory "~/.mail/"))

(after! mu4e
  (setq
    mu4e-compose-crypto-reply-plain-policy 'sign
    mu4e-compose-crypto-reply-encrypted-policy 'sign-and-encrypt
    mu4e-compose-forward-as-attachment t))
