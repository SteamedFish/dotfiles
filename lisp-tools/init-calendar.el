;;; init-calendar.el --- config Emacs calendar -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config Emacs calendar

;;; Code:

(leaf calendar
  :tag "builtin"
  :setq
  '((calendar-latitude . 23.1247)
    (calendar-longitude . 113.3612)
    (calendar-location-name . "Tianhe, Guangzhou")
    (calendar-mark-holidays-flag . t)
    (calendar-mark-diary-entries-flag . t)))

(leaf cal-china-x
  :url "https://github.com/xwl/cal-china-x"
  :straight t
  :require t
  :setq
  (cal-china-x-important-holidays . cal-china-x-chinese-holidays)
  ;; `(clendar-holidays . ,(append cal-china-x-chinese-holidays
  ;;                          cal-china-x-general-holidays)))
  :config
  (setq calendar-holidays (append cal-china-x-chinese-holidays
                            cal-china-x-general-holidays)))
(leaf appt
  :tag "builtin"
  :unless noninteractive
  :config (appt-activate)
  :setq '((appt-display-mode-line . t)
          (appt-display-diary . t)))


(provide 'init-calendar)
;;; init-calendar.el ends here
