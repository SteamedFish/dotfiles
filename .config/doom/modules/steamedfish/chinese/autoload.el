;;; steamedfish/chinese/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chinese/rime-convert-string-at-point ()
  "Convert the string at point to Chinese using the current input scheme.
Search back from the current cursor for available string (if
a ring is selected, use it) as the input code, call the current
int scheme to convert to Chinese."
  (interactive)
  (let ((string (if mark-active
                  (buffer-substring-no-properties
                    (region-beginning) (region-end))
                  (buffer-substring-no-properties
                    (point) (max (line-beginning-position) (- (point) 80)))))
         code
         length)
    (cond ((string-match "\\([a-z]+\\|[[:punct:]]\\)[[:blank:]]*$" string)
            (setq code (replace-regexp-in-string
                         "^[-']" ""
                         (match-string 0 string)))
            (setq length (length code))
            (setq code (replace-regexp-in-string " +" "" code))
            (if mark-active
              (delete-region (region-beginning) (region-end))
              (when (> length 0)
                (delete-char (- 0 length))))
            (when (> length 0)
              (setq unread-command-events
                (append (listify-key-sequence code)
                  unread-command-events))))
      (t (message "`+chinese/rime-convert-string-at-point' did nothing.")))))
