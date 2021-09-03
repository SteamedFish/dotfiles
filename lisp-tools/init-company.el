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
  (company-idle-delay             . 0.1)
  (company-minimum-prefix-length  . 1)
  (company-auto-commit            . nil)
  (company-show-quick-access      . t)
  (company-backends               . '((company-capf :with company-yasnippet)
                                      (company-dabbrev-code
                                       company-keywords
                                       company-files)
                                      company-dabbrev))
  (company-global-modes           . '(not
                                      message-mode
                                      help-mode
                                      gud-mode
                                      eshell-mode
                                      shell-mode
                                      vterm-mode)))

(leaf company-prescient
  :url https://github.com/raxod502/prescient.el
  :straight t
  :global-minor-mode t)

(leaf company-box
  :url https://github.com/sebastiencs/company-box
  :straight t
  :when IS-GUI
  :blackout t
  :hook (company-mode-hook . company-box-mode)
  :setq
  (company-box-icons-alist . 'company-box-icons-all-the-icons))

(leaf company-quickhelp
  :url https://github.com/company-mode/company-quickhelp
  :straight t
  :global-minor-mode t
  :pre-setq (company-quickhelp-delay . 0.3))

(leaf company-quickhelp-terminal
  :url https://github.com/jcs-elpa/company-quickhelp-terminal
  :straight t
  :unless IS-GUI
  :global-minor-mode t
  :bind (:company-active-map
         ([remap company-show-doc-buffer] . company-quickhelp-manual-begin)))

(leaf company-math
  :url https://github.com/vspinu/company-math
  :straight t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(leaf company-dict
  :url https://github.com/hlissner/emacs-company-dict
  :straight t
  :setq `(company-dict-dir . ,(concat my-data-dir "/data/company-dict/"))
  :config
  (add-to-list 'company-backends 'company-dict))

(leaf corfu
  :url https://github.com/minad/corfu
  :straight t
  :disabled t
  :setq
  (corfu-auto               . t)
  (corfu-echo-documentation . nil)
  :global-minor-mode corfu-global-mode)

(provide 'init-company)
;;; init-company.el ends here
