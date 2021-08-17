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

(leaf sis
  :url "https://github.com/laishulu/emacs-smart-input-source"
  :straight t
  :unless (fboundp 'rime-mode)
  :config
  (when (eq window-system 'mac)
    (sis-ism-lazyman-config
      "com.apple.keylayout.US"
      "im.rime.inputmethod.Squirrel.Rime"))
  (when IS-LINUX
    (sis-ism-lazyman-config "1" "2" 'fcitx))
  :setq
  (sis-inline-tighten-head-rule . 0)
  (sis-inline-tighten-tail-rule . 0)
  (sis-inline-single-space-close . t)
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

(provide 'init-chinese)
;;; init-chinese.el ends here
