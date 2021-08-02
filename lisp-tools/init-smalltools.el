;;; init-smalltools.el --- small tools that don't have a long config -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  small tools that don't have a long config

;;; Code:


(leaf wakatime-mode
  :straight t
  :blackout t
  :config
  (global-wakatime-mode))


(provide 'init-smalltools)
;;; init-smalltools.el ends here
