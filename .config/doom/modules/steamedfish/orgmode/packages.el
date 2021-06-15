;; -*- no-byte-compile: t; -*-
;;; steamedfish/orgmode/packages.el

(package! org-noter)
(package! ob-mermaid)
(package! valign
  :recipe (:host github :repo "casouri/valign"))
(package! org-roam-server)
(package! ftable
  :recipe (:host github :repo "casouri/ftable"))
(package! org-alert)
(package! org-contrib)
;;(package! org-plus-contrib) ;; already included in doom config
