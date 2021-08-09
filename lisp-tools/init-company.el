;;; init-company.el --- Language Server Protocal -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;  Language Server Protocal

;;; Code:

(leaf company
  :url http://company-mode.github.io/
  :straight t
  :global-minor-mode global-company-mode
  :blackout t
  :pre-setq
  (company-idle-delay . 0.1)
  (company-minimum-prefix-length . 1)
  (company-global-modes . '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)))

(leaf company-prescient
  :url https://github.com/raxod502/prescient.el
  :straight t
  :global-minor-mode t)

(leaf company-box
  :url https://github.com/sebastiencs/company-box
  :straight t
  :when window-system
  :hook (company-mode . company-box-mode))

(leaf company-quickhelp
  :url https://github.com/company-mode/company-quickhelp
  :straight t
  :hook (global-company-mode . company-quickhelp-mode)
  :pre-setq (company-quickhelp-delay . 0.3))

(leaf company-quickhelp-terminal
  :url https://github.com/jcs-elpa/company-quickhelp-terminal
  :straight t
  :unless window-system
  :bind (:company-active-map
         ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook ((global-company-mode . company-quickhelp-mode)
         (company-quickhelp-mode  . company-quickhelp-terminal-mode)))

(provide 'init-company)
;;; init-company.el ends here
