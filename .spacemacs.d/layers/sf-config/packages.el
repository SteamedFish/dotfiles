(defconst sf-config-packages
    '(
         (company-english-helper :location local)
         ))

(defun sf-config/init-company-english-helper ()
    (use-package company-english-helper)
    )
