;;; init-straight.el --- setup for straight.el -*- lexical-binding: t; -*-

;;; Commentary:

;; setup for straight.el

;;; Code:

(setq straight-recipes-emacsmirror-use-mirror t
      straight-repository-branch "develop")

(setq straight-disable-native-compile
  (when (fboundp 'native-comp-available-p)
    (not (native-comp-available-p))))

;; This is the best way, but need python3 and watchexec to work
(if (and (executable-find "python3")
         (executable-find "watchexec"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications '(find-at-startup find-when-checking)))


;; official bootstrap code
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'init-straight)
;;; init-straight.el ends here
