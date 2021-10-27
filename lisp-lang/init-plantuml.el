;;; init-plantuml.el --- plantuml mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  plantuml mode

;;; Code:

(leaf plantuml-mode
  :url https://github.com/skuro/plantuml-mode
  :straight t
  :when IS-GUI
  :ensure-system-package plantuml
  :setq
  `(plantuml-jar-path         . ,(cond
                                  (IS-MAC
                                   "/usr/local/opt/plantuml/libexec/plantuml.jar")
                                  (IS-LINUX
                                   "/usr/share/java/plantuml/plantuml.jar")))
  (plantuml-default-exec-mode . 'executable)
  :config
  (add-to-list 'auto-mode-alist    '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml"       . plantuml)))

(leaf flycheck-plantuml
  :url https://github.com/alexmurray/flycheck-plantuml
  :straight t
  :when IS-GUI
  :require t
  :config (flycheck-plantuml-setup))

(leaf ob-plantuml
  :comment provided by org-plus-contrib
  :require t
  :setq
  (org-plantuml-jar-path . plantuml-jar-path)
  :config
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
