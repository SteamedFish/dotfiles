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
  (straight-x-pull-all)
  (straight-check-all)
  (straight-remove-unused-repos))
 ((fboundp 'straight-pull-all)
  (straight-pull-all)
  (straight-check-all)
  (straight-remove-unused-repos))
 ((fboundp 'paradox-upgrade-packages)
  (progn
    (unless (boundp 'paradox-github-token)
      (setq paradox-github-token t))
    (paradox-upgrade-packages)
    (princ
     (if (get-buffer "*Paradox Report*")
         (with-current-buffer "*Paradox Report*" (buffer-string))
       "\nNothing to upgrade\n"))))
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
