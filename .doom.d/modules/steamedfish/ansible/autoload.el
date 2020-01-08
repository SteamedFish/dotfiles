;;; steamedfish/ansible/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ansible/ansible-enable-maybe ()
  "Return non-nil if `ansible' should be enabled for the current file."
  (when
      (and (stringp buffer-file-name)
           (string-match
            ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\|ansible/.+\\)"
            buffer-file-name))
    (ansible 1)
    (ansible-doc-mode 1)
    (add-to-list 'company-backends 'company-ansible)))
