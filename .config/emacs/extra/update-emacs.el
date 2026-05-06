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

(defvar straight-base-dir)
(defvar straight-build-dir)

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

(defun update-emacs--native-compile-directory (directory)
  "Native-compile DIRECTORY when native compilation is available."
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p)
             (fboundp 'native-compile-directory)
             (file-directory-p directory))
    (message "Native-compiling %s" directory)
    (native-compile-directory directory)))

(defun update-emacs--native-compile-file (file)
  "Native-compile FILE when it still exists."
  (when (and (fboundp 'native-compile)
             (stringp file)
             (file-exists-p file))
    (message "Native-compiling %s" file)
    (native-compile file)))

(defun update-emacs--straight-build-directory ()
  "Return the active straight.el build directory, or nil if unavailable."
  (when (and (boundp 'straight-base-dir)
             (boundp 'straight-build-dir))
    (expand-file-name (concat "straight/" straight-build-dir)
                      straight-base-dir)))

(defun update-emacs--native-compile-loaded-straight-files ()
  "Native-compile straight.el files loaded during this batch startup."
  (let ((straight-build-directory (update-emacs--straight-build-directory)))
    (when straight-build-directory
      (dolist (entry load-history)
        (let ((file (car entry)))
          (when (and (stringp file)
                     (string-prefix-p straight-build-directory file)
                     (string-suffix-p ".el" file))
            (update-emacs--native-compile-file file)))))))

(defun update-emacs--remove-directory-if-present (directory)
  "Delete DIRECTORY when it exists."
  (when (file-directory-p directory)
    (message "Removing old ELN cache %s" directory)
    (delete-directory directory t)))

(defun update-emacs--remove-old-eln-cache ()
  "Remove obsolete native compilation cache directories.

Delete the old default `eln-cache/' location and versioned cache
subdirectories that do not belong to the running Emacs version."
  (let* ((default-cache (expand-file-name "eln-cache" user-emacs-directory))
         (active-cache (file-name-as-directory
                        (expand-file-name ".local/data/eln-cache"
                                          user-emacs-directory)))
         (current-version (regexp-quote emacs-version)))
    (update-emacs--remove-directory-if-present default-cache)
    (when (file-directory-p active-cache)
      (dolist (entry (directory-files active-cache t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p entry)
                   (not (string-match-p current-version
                                        (file-name-nondirectory
                                         (directory-file-name entry)))))
          (update-emacs--remove-directory-if-present entry))))))

(defun update-emacs--native-compile-loaded-code ()
  "Native-compile code that GUI startup would otherwise compile lazily."
  (when (and (require 'comp nil t)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    ;; Batch startup disables deferred native compilation to avoid stray async
    ;; jobs, so compile synchronously here before the next GUI session starts.
    (dolist (directory (delq nil
                             (list
                              (expand-file-name "lisp-core" user-emacs-directory)
                              (expand-file-name "lisp-theme" user-emacs-directory)
                              (expand-file-name "lisp-tools" user-emacs-directory)
                              (expand-file-name "lisp-lang" user-emacs-directory))))
      (update-emacs--native-compile-directory directory))
    ;; Compile only loaded straight.el package files.  Whole-directory native
    ;; compilation can hit disabled packages whose optional files are broken.
    (update-emacs--native-compile-loaded-straight-files)
    (update-emacs--remove-old-eln-cache)
    (when (fboundp 'native-compile-prune-cache)
      (native-compile-prune-cache))))

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

(update-emacs--native-compile-loaded-code)

(if (fboundp 'lsp-update-servers)
    (lsp-update-servers))

(provide 'update-emacs)
;;; update-emacs.el ends here
