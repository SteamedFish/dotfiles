;;; init-chinese.el --- chinese related -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  chinese related

;;; Code:

(leaf evil-pinyin
  :url "https://github.com/laishulu/evil-pinyin"
  :straight t
  :pre-setq
  (evil-pinyin-with-search-rule . 'always)
  :global-minor-mode global-evil-pinyin-mode)

(leaf rime
  :url https://github.com/DogLooksGood/emacs-rime
  :straight t
  :when (or IS-MAC IS-LINUX)
  :when (fboundp 'module-load)
  :when IS-GUI
  :bind
  ("C-SPC"                    . toggle-input-method)
  (:rime-active-mode-map
   ("M-j"                     . rime-inline-ascii))
  (:rime-mode-map
   ("M-j"                     . rime-force-enable))
  :pre-setq
  (default-input-method       . "rime")
  :setq
  (rime-show-candidate        . 'posframe)
  (rime-inline-ascii-trigger  . 'shift-l)
  :init
  ;; TODO: when using emacs-mac, make sure system IME is always disabled
  (when IS-LINUX
    (setq rime-user-data-dir (expand-file-name "~/.config/fcitx/rime/"))
    (unless (executable-find "rime_patch")
      (system-packages-ensure "librime")))
  (when IS-MAC
    (setq rime-user-data-dir (expand-file-name "~/Library/Rime/"))
    (system-packages-ensure "unzip")
    (setq rime-librime-root (concat my-data-dir "data/librime/dist"))
    (unless (file-exists-p (concat my-data-dir "data/librime/"))
      (make-directory (concat my-data-dir "data/librime")))
    (unless (file-exists-p (concat my-data-dir "data/librime/dist/bin/rime_patch"))
      (require 'url)
      (url-copy-file
       "https://github.com/rime/librime/releases/download/1.7.3/rime-1.7.3-osx.zip"
       (concat my-data-dir "data/librime/rime-1.7.3-osx.zip")
       t)
      (async-shell-command (concat "cd " my-data-dir "data/librime"
                                   "&& unzip rime-1.7.3-osx.zip"))))
  :hook
  (after-change-major-mode-hook . (lambda () (activate-input-method default-input-method)))
  ;; ((test-mode-hook telega-chat-mode-hook) . (lambda ()
  ;;                                             (setq-local
  ;;                                              rime-disable-predicates
  ;;                                              (cons 'rime-predicate-after-ascii-char-p rime-disable-predicates))))
  ((prog-mode-hook minibuffer-setup-hook) . (lambda ()
                                              ;; (setq-local
                                              ;;  rime-disable-predicates
                                              ;;  (cons 'rime-predicate-after-alphabet-char-p rime-disable-predicates))
                                              ;; (setq-local
                                              ;;  rime-disable-predicates
                                              ;;  (cons 'rime-predicate-after-ascii-char-p rime-disable-predicates))
                                              (setq-local
                                               rime-disable-predicates
                                               (cons 'rime-predicate-punctuation-after-ascii-p rime-disable-predicates))
                                              (setq-local
                                               rime-disable-predicates
                                               (cons 'rime-predicate-space-after-ascii-p rime-disable-predicates))
                                              (setq-local
                                               rime-disable-predicates
                                               (cons 'rime-predicate-space-after-cc-p rime-disable-predicates))))
  :setq-default
  ;; TODO: set this based on different modes
  (rime-disable-predicates . '(rime-predicate-prog-in-code-p
                               rime-predicate-evil-mode-p
                               rime-predicate-ace-window-p
                               rime-predicate-hydra-p
                               rime-predicate-org-in-src-block-p
                               (lambda () (minibufferp))
                               rime-predicate-org-latex-mode-p
                               (lambda () (button-at (point)))
                               rime-predicate-after-alphabet-char-p
                               rime-predicate-after-ascii-char-p
                               rime-predicate-current-uppercase-letter-p
                               rime-predicate-tex-math-or-command-p)))


(leaf sis
  :url "https://github.com/laishulu/emacs-smart-input-source"
  :straight t
  :unless (fboundp 'rime-mode)
  :disabled t
  :config
  (when (eq window-system 'mac)
    (sis-ism-lazyman-config
     "com.apple.keylayout.US"
     "im.rime.inputmethod.Squirrel.Rime"))
  (when IS-LINUX
    (sis-ism-lazyman-config "1" "2" 'fcitx))
  :setq
  (sis-inline-tighten-head-rule   . 0)
  (sis-inline-tighten-tail-rule   . 0)
  (sis-inline-single-space-close  . t)
  :global-minor-mode
  (sis-global-cursor-color-mode
   sis-global-respect-mode
   sis-global-context-mode
   sis-global-inline-mode))

(leaf ace-pinyin
  :url https://github.com/cute-jumper/ace-pinyin
  :straight t
  :pre-setq (ace-pinyin-use-avy . t)
  :blackout (ace-pinyin-global-mode ace-pinyin-mode)
  :global-minor-mode ace-pinyin-global-mode)

(leaf pinyin-search
  :url https://github.com/xuchunyang/pinyin-search.el
  :doc "usr M-x pinyin-search to search"
  :doc "FIXME: work with swipper"
  :straight t
  :setq (pinyin-search-activated . t))

(leaf fanyi
  :url https://github.com/condy0919/fanyi.el
  :doc "use M-x fanyi-dwim to translate"
  :straight t)

(leaf google-translate
  :url https://github.com/atykhonov/google-translate
  :tag "TODO"
  :straight t
  :require 'google-translate-default-ui)

(provide 'init-chinese)
;;; init-chinese.el ends here
