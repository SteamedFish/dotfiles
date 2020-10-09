;;; steamedfish/irc/config.el -*- lexical-binding: t; -*-

(when (featurep! :app irc)
  (after! circe
    (evil-set-initial-state 'circe-chat-mode 'insert)
    (setq
     circe-default-quit-message "Ahhhhhhhhh!"
     circe-default-part-message "Ahhhhhhhhh!"))
  (map!
   :leader
   :prefix ("o" . "open")
   :desc "Open IRC"    :n "I"  #'=irc)
  (set-irc-server!
   "Freenode"
   `(
     :host "irc.freenode.net"
     :tls t
     :port 6697
     :nick "SteamedFish"
     :nickserv-password (lambda (&rest _) (+pass-get-secret "irc"))
     :nickserv-mask "^NickServ!NickServ@services\\.$"
     :nickserv-identify-challenge "\C-b/msg\\s-NickServ\\s-identify\\s-<password>\C-b"
     :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
     :nickserv-identify-confirmation "^You are now identified for .*\\.$"
     :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
     :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"
     :channels (:after-auth
                "#emacs"
                "#emacs-beginners"
                "#org-mode"
                "#archlinux-cn"
                "#archlinux-cn-offtopic"
                "#archlinux-cn-game"
                "#debian-cn"
                "#openstack-chinese"
                "#chinalug"
                "#fedora-zh"
                "#ubuntu-cn"
                "#gentoo-cn"
                "#mandarin"
                "#vim-china"
                "#kde-cn"
                "#qt-cn"
                "#ppmm")))
  (set-irc-server!
   "OFTC"
   `(
     :host "irc.oftc.net"
     :tls t
     :port 6697
     :nick "SteamedFish"
     :nickserv-password (lambda (&rest _) (+pass-get-secret "irc"))
     :nickserv-mask "^NickServ!services@services\\.oftc\\.net$"
     :nickserv-identify-challenge "This nickname is registered and protected."
     :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password} {nick}"
     :nickserv-identify-confirmation "^You are successfully identified as .*\\.$"
     :channels (:after-auth
                "#arch-cn"
                "#debian-zh"
                "#steamedfish"
                "#njulug"
                "#dot"))))
