;;; init-calendar.el --- config Emacs calendar -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config Emacs calendar

;;; Code:

(leaf calendar
  :straight nil
  :setq
  '((calendar-latitude . 23.1247)
    (calendar-longitude . 113.3612)
    (calendar-location-name . "Tianhe, Guangzhou")
    (calendar-mark-holidays-flag . t)
    (calendar-mark-diary-entries-flag . t)))

(leaf appt
  :straight nil
  :config (appt-activate)
  :setq '((appt-display-mode-line . t)
          (appt-display-diary . t)))


(provide 'init-calendar)
;;; init-calendar.el ends here
