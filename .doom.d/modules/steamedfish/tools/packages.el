;; -*- no-byte-compile: t; -*-
;;; steamedfish/tools/packages.el

(package! pinentry)
(package! eaf
  :recipe (:host github
            :repo "manateelazycat/emacs-application-framework"
            :files ("*")))
(package! netease-cloud-music
  :recipe (:host github
            :repo "SpringHan/netease-cloud-music.el"))
