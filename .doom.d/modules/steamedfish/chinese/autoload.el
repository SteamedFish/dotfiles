;;; steamedfish/chinese/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chinese/switch-input-source ()
  "switch between input source.
   You can get full list of sources by (mac-input-source-list)"
  (interactive)
  (if (eq window-system 'mac)
      (let ((US "com.apple.keylayout.US")
            (CN "im.rime.inputmethod.Squirrel.Rime"))
        (pcase (mac-input-source)
          ((pred (string= US))
           (mac-select-input-source CN))
          ((pred (string= CN))
           (mac-select-input-source US))))
    (error "only support emacs-mac")))

;;;###autoload
(defun +chinese/pyim-probe-punctuation-after-english-letter (char)
  "detect of punctuation is after english letter
   pyim should use half width in this situation"
  (let ((str-before-1 (pyim-char-before-to-string 0))
         (puncts (mapcar 'car pyim-punctuation-dict)))
    (and (string-match "[A-Za-z ]" str-before-1)
      (member (char-to-string char) puncts))))
