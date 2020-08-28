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

(use-package! rime
  :commands (rime-activate)
  :bind
  ("C-M-S-s-SPC" . #'toggle-input-method)
  ("C-M-S-s-j" . #'+chinese/rime-convert-string-at-point)
  (:map rime-mode-map
    ("C-M-S-s-SPC" . #'rime-force-enable))
  (:map rime-active-mode-map
    ("C-M-S-s-SPC" . #'rime-inline-ascii))
  :init
  (setq
    default-input-method "rime"
    rime-user-data-dir (concat doom-private-dir "etc/rime"))
  (when IS-MAC
    ;; manually install librime here
    (setq rime-librime-root (concat doom-local-dir "librime/dist")))
  (add-hook! '(after-change-major-mode-hook)
    ((lambda () (activate-input-method default-input-method))))

  (setq-default rime-disable-predicates
    '(rime-predicate-prog-in-code-p
       rime-predicate-evil-mode-p
       rime-predicate-ace-window-p
       rime-predicate-hydra-p
       rime-predicate-org-in-src-block-p
       (lambda () (minibufferp))
       rime-predicate-org-latex-mode-p
       (lambda () (button-at (point)))
       rime-predicate-current-uppercase-letter-p
       rime-predicate-tex-math-or-command-p))

  (add-hook! '(prog-mode-hook minibuffer-setup-hook)
    (setq-local rime-disable-predicates
      (cons 'rime-predicate-after-alphabet-char-p rime-disable-predicates))
    (setq-local rime-disable-predicates
      (cons 'rime-predicate-after-ascii-char-p rime-disable-predicates))
    (setq-local rime-disable-predicates
      (cons 'rime-predicate-punctuation-after-ascii-p rime-disable-predicates))
    (setq-local rime-disable-predicates
      (cons 'rime-predicate-space-after-ascii-p rime-disable-predicates))
    (setq-local rime-disable-predicates
      (cons 'rime-predicate-space-after-cc-p rime-disable-predicates)))
  (add-hook! '(text-mode-hook telega-chat-mode-hook)
   (setq-local rime-disable-predicates
         (cons 'rime-predicate-after-ascii-char-p rime-disable-predicates)))
  :config
  (setq
    rime-show-candidate 'posframe
    mode-line-mule-info '((:eval (rime-lighter)))
    rime-inline-ascii-trigger 'shift-l))


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
