;;; init-puppet.el --- puppet mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  puppet mode

;;; Code:

(leaf puppet-mode
  :url "https://github.com/voxpupuli/puppet-mode"
  :straight t
  :setq
  '((puppet-fontify-variables-in-comments . t)))

(provide 'init-puppet)
;;; init-puppet.el ends here
