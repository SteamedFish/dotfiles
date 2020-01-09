;;; steamedfish/chinese/config.el  -*- lexical-binding: t; -*-

(if (eq window-system 'mac)
    (mac-auto-ascii-mode t))

(use-package! youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe
             youdao-dictionary-search-at-point
             youdao-dictionary-search-at-point+
             youdao-dictionary-search-from-input
             youdao-dictionary-search-and-replace
             youdao-dictionary-play-voice-at-point
             youdao-dictionary-play-voice-from-input)
  :init
  (map!
   (:when (display-graphic-p)
     :leader
     :prefix ("l" . "lookup")
     :desc "Lookup Youdao"
     :n "y"  #'youdao-dictionary-search-at-point-posframe)
   (:when (not (display-graphic-p))
     :leader
     :prefix ("l" . "lookup")
     :desc "Lookup Youdao"
     :n "y"  #'youdao-dictionary-search-at-point+)
   :leader
   :prefix ("l" . "lookup")
   :desc "Lookup Youdao Input"
   :n "Y"  #'youdao-dictionary-search-from-input)
  :config
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file (concat doom-cache-dir "youdao.cache")
        youdao-dictionary-use-chinese-word-segmentation t)
  (map!
   :map youdao-dictionary-mode-map
   :n "q" #'quit-window
   :n "p" #'youdao-dictionary-play-voice-of-current-word
   :n "y" #'youdao-dictionary-play-voice-at-point))

(use-package! sdcv
  :commands (sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-search-input
             sdcv-search-input+)
  :init
  (map!
   :leader
   :prefix ("l" . "lookup")
   :n "d" #'sdcv-search-pointer+
   :n "D" #'sdcv-search-input+)
  :config
  (setq sdcv-say-word-p nil
        sdcv-dictionary-data-dir (concat (xdg-config-home) "/stardict")
        sdcv-dictionary-simple-list '("简明英汉字典增强版")
        sdcv-dictionary-complete-list '("简明英汉字典增强版")))

(use-package! cal-china-x
  :after calendar
  :config
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        calendar-holidays (append
                           cal-china-x-important-holidays
                           cal-china-x-general-holidays)))

(use-package! pyim
  :commands pyim-convert-string-at-point
  :bind
  (("C-S-M-s-SPC" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer))
  :config
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  (map! :map 'pyim-mode-map
        "." #'pyim-page-next-page
        "," #'pyim-page-previous-page)

  (if IS-LINUX
      (setq pyim-default-scheme 'rime)
    (setq pyim-default-scheme 'quanpin))

  (setq default-input-method "pyim"
        pyim-page-length 9
        pyim-dicts
        `((:name
           "pyim-bigdict"
           :file
           ,(expand-file-name (concat doom-private-dir "etc/pyim/pyim-bigdict.pyim.gz")))))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 C-S-M-s-SPC 快捷键，强制将光标前的拼音字符串转换为中文。
  ;; 4. 当光标在按钮上时，切换到英文输入。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  ;; detect if current point is at button
                  (lambda () (button-at (point)))))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation
                  +chinese/pyim-probe-punctuation-after-english-letter))

  ;; pyim will reset all local variables in this list for some reason,
  ;; but we don't want some of them to be reseted everytime pyim is
  ;; activate of deactivate, so we can use different settings in
  ;; different buffers.
  ;; https://github.com/tumashu/pyim/issues/342
  (setq pyim-local-variable-list
        (delete 'pyim-english-input-switch-functions pyim-local-variable-list))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; enable ime by default
  (when IS-LINUX
    (add-hook! '(text-mode-hook prog-mode-hook)
      'toggle-input-method)))

(use-package! liberime-config
  :when IS-LINUX
  :init
  (setq liberime-user-data-dir (concat doom-private-dir "etc/rime"))
  (add-hook 'after-liberime-load-hook
    (lambda () (liberime-select-schema "luna_pinyin_simp")))
  :config
  (setq pyim-title "ㄓ"))
