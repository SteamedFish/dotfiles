;;; update-emacs.el --- update all the packages -*- lexical-binding: t; -*-

;;; Commentary:

;; run this file in terminal:
;; emacs \
;;    --batch \
;;    --debug-init \
;;    --load ~/.config/emacs/init.el \
;;    --load ~/.config/emacs/extra/update-emacs.el

;;; Code:

(defvar paradox-github-token)
(defvar package-menu-async)

(declare-function lsp-update-servers "init-lsp")
(declare-function package-list-packages "package")
(declare-function package-menu-execute "package")
(declare-function package-menu-mark-upgrades "package")
(declare-function paradox-upgrade-packages "paradox")
(declare-function straight-check-all "straight")
(declare-function straight-pull-all "straight")
(declare-function straight-vc-git-fetch-from-remote "straight")
(declare-function straight-x-pull-all "straight-x")

(defvar update-emacs-straight-fetch-retry-count 3
  "Maximum attempts for straight.el Git fetches during batch updates.")

(defvar update-emacs-straight-fetch-retry-delay 2
  "Initial delay in seconds before retrying straight.el Git fetches.")

(defun update-emacs--straight-git-fetch-with-retries (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, retrying transient straight.el fetch failures.

This advice is intentionally installed only while this update script runs.  It
keeps straight.el's normal behavior for interactive sessions, but gives batch
updates a bounded chance to recover from transient network failures before
straight.el reaches its interactive prompt path."
  (let ((attempt 1)
        (delay update-emacs-straight-fetch-retry-delay)
        (max-attempts (max 1 update-emacs-straight-fetch-retry-count)))
    (catch 'done
      (while (<= attempt max-attempts)
        (condition-case err
            (throw 'done (apply orig-fun args))
          (error
           (if (or (not (string-match-p "Failed to run git fetch"
                                        (error-message-string err)))
                   (>= attempt max-attempts))
               (signal (car err) (cdr err))
             (message "straight git fetch failed; retrying in %s seconds (%s/%s): %s"
                      delay attempt max-attempts (error-message-string err))
             (sleep-for delay)
             (setq delay (* 2 delay))
             (setq attempt (1+ attempt)))))))))

(defun update-emacs--with-straight-fetch-retries (thunk)
  "Run THUNK with scoped straight.el Git fetch retries in batch mode."
  (if (and noninteractive (fboundp 'straight-vc-git-fetch-from-remote))
      (unwind-protect
          (progn
            (advice-add 'straight-vc-git-fetch-from-remote
                        :around #'update-emacs--straight-git-fetch-with-retries)
            (funcall thunk))
        (advice-remove 'straight-vc-git-fetch-from-remote
                       #'update-emacs--straight-git-fetch-with-retries))
    (funcall thunk)))

(update-emacs--with-straight-fetch-retries
 (lambda ()
   (cond
    ((fboundp 'straight-x-pull-all)
     (straight-x-pull-all)
     (straight-check-all))
    ((fboundp 'straight-pull-all)
     (straight-pull-all)
     (straight-check-all))
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
         (user-error nil)))))))

(if (fboundp 'lsp-update-servers)
    (lsp-update-servers))

(provide 'update-emacs)
;;; update-emacs.el ends here
