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
  :after liberime
  :commands pyim-convert-string-at-point
  :after-call after-find-file pre-command-hook
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

  (if (or IS-LINUX IS-MAC)
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
    '(pyim-probe-isearch-mode
       pyim-probe-program-mode
       pyim-probe-org-structure-template
       pyim-probe-evil-normal-mode
       pyim-probe-org-speed-commands
       ;; detect if current point is at button
       (lambda () (button-at (point)))))

  (add-hook! 'prog-mode-hook
    (add-to-list 'pyim-english-input-switch-functions
      'pyim-probe-dynamic-english))

  (add-hook! 'text-mode-hook
    (add-to-list 'pyim-english-input-switch-functions
      'pyim-probe-auto-english))

  (add-hook! '(text-mode-hook prog-mode-hook)
    ;; active pyim by default
    ;; for some unknown reason directly calling `active-input-method'
    ;; is not working, but it works with `run-at-time'
    ((lambda () (run-at-time nil nil 'activate-input-method "pyim"))))

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

(use-package! liberime
  :when (or IS-LINUX IS-MAC)
  :init
  (setq liberime-user-data-dir (concat doom-local-dir "rime/"))
  (when IS-LINUX
    (setq liberime-shared-data-dir (expand-file-name "~/.config/fcitx/rime/")))
  (when IS-MAC
    (setq liberime-shared-data-dir (expand-file-name "~/Library/Rime/")))
  (setq pyim-title "ㄓ")
  (add-hook! 'liberime-after-start-hook
    (lambda ()
      (run-with-timer 5 1
        (liberime-select-schema "luna_pinyin_simp"))))
  (add-hook! 'after-init-hook
   #'liberime-sync)
  :config
  (unless (file-exists-p (concat (liberime-get-library-directory)
                               "build/liberime-core"
                               module-file-suffix))
    (liberime-build)))

;; Support pinyin in Ivy
;; Input prefix ';' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package! pinyinlib
  :commands pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
        (and (fboundp '+ivy-prescient-non-fuzzy)
          (+ivy-prescient-non-fuzzy str))
        (ivy--regex-plus str)))

    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
        (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
        ((equal str "") nil)
        (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
        ((equal (substring str 0 1) ";")
          (mapconcat
            #'my-pinyinlib-build-regexp-string
            (remove nil (mapcar
                          #'my-pinyin-regexp-helper
                          (split-string
                            (replace-regexp-in-string ";" "" str)
                            "")))
            ""))
        (t nil)))))

    ;; (mapcar
    ;;   (lambda (item)
    ;;     (let ((key (car item))
    ;;            (value (cdr item)))
    ;;       (when (member value '(+ivy-prescient-non-fuzzy
    ;;                              ivy--regex-plus))
    ;;         (setf (alist-get key ivy-re-builders-alist)
    ;;           #'ivy--regex-pinyin))))
    ;;   ivy-re-builders-alist)))
