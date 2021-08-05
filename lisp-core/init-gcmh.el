;;; init-gcmh.el --- setup for gcmh -*- lexical-binding: t; -*-

;;; Commentary:

;; setup for straight.el

;;; Code:

(leaf gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :setq
  (gcmh-idle-delay . 1)
  `(gcmh-high-cons-threshold . ,(* 32 1024 1024))
  :blackout t)

(provide 'init-gcmh)
;;; init-gcmh.el ends here
