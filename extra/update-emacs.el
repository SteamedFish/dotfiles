;;; update-emacs.el --- update all the packages -*- lexical-binding: t; -*-

;;; Commentary:

;; run this file in terminal:
;; emacs \
;;    --batch \
;;    --debug-init \
;;    --load ~/.config/emacs/init.el \
;;    --load ~/.config/emacs/extra/update-emacs.el

;;; Code:

(cond
 ((fboundp 'straight-x-pull-all)
  (straight-x-pull-all))
 ((fboundp 'straight-pull-all)
  (straight-pull-all))
 ((fboundp 'paradox-upgrade-packages)
  (paradox-upgrade-packages))
 ((fboundp 'paradox-upgrade-packages)
  (paradox-upgrade-packages))
 (t
  (progn
    (let ((package-menu-async nil))
      (package-list-packages))
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute 'noquery)
      (user-error nil)))))

(provide 'update-emacs)
;;; update-emacs.el ends here
