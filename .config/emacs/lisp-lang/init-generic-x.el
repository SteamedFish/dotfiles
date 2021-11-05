;;; init-generic-x.el --- config emacs's builtin generic-x mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config emacs's builtin generic-x mode

;;; Code:

;; before require generic-x we append more modes
;; to make emacs support filetypes such as dosini
(leaf generic-x
  :tag "builtin"
  :pre-setq
  '((generic-extras-enable-list . '(apache-conf-generic-mode
                                    apache-log-generic-mode
                                    hosts-generic-mode
                                    java-manifest-generic-mode
                                    java-properties-generic-mode
                                    javascript-generic-mode
                                    show-tabs-generic-mode
                                    vrml-generic-mode
                                    bat-generic-mode
                                    inf-generic-mode
                                    ini-generic-mode
                                    rc-generic-mode
                                    reg-generic-mode
                                    rul-generic-mode
                                    alias-generic-mode
                                    ansible-inventory-generic-mode
                                    etc-fstab-generic-mode
                                    etc-modules-conf-generic-mode
                                    etc-passwd-generic-mode
                                    etc-services-generic-mode
                                    etc-sudoers-generic-mode
                                    fvwm-generic-mode
                                    inetd-conf-generic-mode
                                    mailagent-rules-generic-mode
                                    mailrc-generic-mode
                                    named-boot-generic-mode
                                    named-database-generic-mode
                                    prototype-generic-mode
                                    resolve-conf-generic-mode
                                    samba-generic-mode
                                    x-resource-generic-mode
                                    xmodmap-generic-mode
                                    astap-generic-mode
                                    ibis-generic-mode
                                    pkginfo-generic-mode
                                    spice-generic-mode)))
  :require t
  :setq
  '((generic-use-find-file-hook . t)))


(provide 'init-generic-x)
;;; init-generic-x.el ends here
