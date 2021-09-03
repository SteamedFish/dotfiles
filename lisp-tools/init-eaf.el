;;; init-eaf.el --- config Emacs Application Framework -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config Emacs Application Framework

;;; Code:

(leaf epc
  :url "https://github.com/kiwanami/emacs-epc"
  :doc "required by eaf"
  :straight t
  :when IS-GUI)

(leaf eaf
  :url "https://github.com/manateelazycat/emacs-application-framework"
  :when IS-GUI
  :when IS-LINUX
  :straight (eaf
             :host github
             :repo "manateelazycat/emacs-application-framework"
             :files ("*"))
  :setq
  `(eaf-config-location                 . ,(concat my-data-dir "data/eaf/"))
  (eaf-find-alternate-file-in-dired     . t)
  ;; (browse-url-browser-function       . #'eaf-open-browser)
  (eaf-browser-continue-where-left-off  . t)
  (eaf-browser-default-search-engine    . "duckduckgo")
  (eaf-browser-dark-mode                . t)
  (eaf-terminal-dark-mode               . t)
  (eaf-pdf-dark-mode                    . "follow")
  (eaf-mindmap-dark-mode                . "follow")
  (eaf-browse-blank-page-url            . "https://duckduckgo.com/")
  (eaf-browser-enable-adblocker         . t)
  (eaf-browser-remember-history         . t)
  :config
  ;; install dependencies will cause eaf's repo modified,
  ;; which will trigger a rebuild, which cleans up installed dependencies
  (unless (file-exists-p (concat straight-base-dir "straight/repos/emacs-application-framework/app"))
    (let* ((eaf-dir (file-name-directory (locate-library "eaf")))
           (repo-dir (concat straight-base-dir "straight/repos/emacs-application-framework/"))
           (default-directory eaf-dir))
      (cond ((eq system-type 'gnu/linux)
             (async-shell-command
              (concat "cd " eaf-dir "&&"
                      "./install-eaf.py --ignore-py-deps" "&&"
                      "cd " repo-dir "&&"
                      "git reset --hard HEAD")))
            ((memq system-type '(cygwin windows-nt ms-dos))
             (async-shell-command (format "node %s" (concat "install-eaf-win32.js" "&"))))
            ((eq system-type 'darwin)
             (async-shell-command
              (concat "cd " eaf-dir "&&"
                      "./install-eaf.py" "&&"
                      "cd " repo-dir "&&"
                      "git reset --hard HEAD")))))
    ;; FIXME: when this executes; the async-shell-command may not finish yet
    (let* ((modified-file (concat straight-base-dir "straight/modified/emacs-application-framework")))
      (when (file-exists-p modified-file)
        (delete-file modified-file)))))


(leaf eaf-evil
  :url "https://github.com/manateelazycat/emacs-application-framework"
  :when IS-GUI
  :after eaf)

(leaf eaf-org
  :url "https://github.com/manateelazycat/emacs-application-framework"
  :when IS-GUI
  :after eaf)

(provide 'init-eaf)
;;; init-eaf.el ends here
