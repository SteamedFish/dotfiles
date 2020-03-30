;; -*- no-byte-compile: t; -*-
;;; steamedfish/tools/packages.el

(package! pinentry)
(package! eaf
  :recipe (:host github
            :repo "manateelazycat/emacs-application-framework"
            :files ("*")))
