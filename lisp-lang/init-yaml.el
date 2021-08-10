;;; init-yaml.el --- yaml mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  yaml mode

;;; Code:

(leaf yaml-mode
  :url https://github.com/yoshiki/yaml-mode
  :straight t)

(leaf ansible
  :url https://github.com/k1LoW/emacs-ansible
  :straight t
  :config (blackout 'ansible)
  :init
  (defun my-ansible-enable-maybe ()
    "Return non-nil if `ansible' should be enabled for the current file."
    (when
        (and (stringp buffer-file-name)
             (string-match
              ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\|ansible/.+\\)"
              buffer-file-name))
      (ansible 1)
      (ansible-doc-mode 1)
      (add-to-list 'company-backends 'company-ansible)))
 :hook (yaml-mode-hook . my-ansible-enable-maybe))

(leaf ansible-doc
  :url https://github.com/emacsorphanage/ansible-doc
  :straight t
  :blackout t)

(leaf company-ansible
  :url https://github.com/krzysztof-magosa/company-ansible
  :straight t)

(provide 'init-yaml)
;;; init-yaml.el ends here
