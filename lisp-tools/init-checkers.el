;;; init-checkers.el --- checkers -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  checkers

;;; Code:

(leaf flycheck
  :url https://www.flycheck.org/
  :straight t
  :blackout t
  :global-minor-mode global-flycheck-mode)

(leaf flycheck-popup-tip
  :url https://github.com/flycheck/flycheck-popup-tip
  :straight t
  :when IS-GUI
  :hook (flycheck-mode-hook . flycheck-popup-tip-mode))

(leaf flycheck-pos-tip
  :url https://github.com/flycheck/flycheck-pos-tip
  :straight t
  :unless IS-GUI
  :hook (flycheck-mode-hook . flycheck-pos-tip-mode))

(leaf flycheck-posframe
  :url https://github.com/alexmurray/flycheck-posframe
  :straight t
  :when IS-GUI
  :hook
  (flycheck-mode-hook                  . flycheck-posframe-mode)
  (flycheck-posframe-inhibit-functions . company--active-p)
  (flycheck-posframe-inhibit-functions . evil-insert-state-p)
  (flycheck-posframe-inhibit-functions . evil-replace-state-p))

(provide 'init-checkers)
;;; init-checkers.el ends here
