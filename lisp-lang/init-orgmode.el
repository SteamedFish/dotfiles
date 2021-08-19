;;; init-orgmode.el --- orgmode mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  orgmode mode

;;; Code:

(leaf org
  :url https://orgmode.org/
  :straight t
  :blackout org-indent-mode
  :setq
  (org-startup-indented . t)
  :config
  (setq org-babel-do-load-languages '((emacs-lisp . t)
                                      (awk . t)
                                      (C . t)
                                      (calc . t)
                                      (css . t)
                                      (ditaa . t)
                                      (dot . t)
                                      (gnuplot . t)
                                      (latex . t)
                                      (ledger . t)
                                      (makefile . t)
                                      (maxima . t)
                                      (org . t)
                                      (perl . t)
                                      (plantuml . t)
                                      (python . t)
                                      (ruby . t)
                                      (screen . t)
                                      (shell . t)
                                      (sql . t)
                                      (sqlite . t)))
  (org-babel-do-load-languages 'org-babel-do-load-languages)
  :bind
  (:org-mode-map
   ([remap org-set-tags-command] . counsel-org-tag)))

(leaf org-contrib
  :url https://orgmode.org/worg/org-contrib/
  :straight t)

(leaf ob-go
  :url https://github.com/pope/ob-go
  :straight t
  :config
  (add-to-list 'org-babel-do-load-languages '(go . t))
  (org-babel-do-load-languages 'org-babel-do-load-languages))

(leaf ox-gfm
  :url https://github.com/larstvei/ox-gfm
  :doc "Convert to Github Flavored Markdown"
  :straight t
  :require t)

(leaf org-superstar
  :url https://github.com/integral-dw/org-superstar-mode
  :straight t
  :when IS-GUI
  :hook (org-mode-hook . org-superstar-mode))

(leaf org-fancy-priorities
  :url https://github.com/harrybournis/org-fancy-priorities
  :straight t
  :blackout t
  :hook (org-mode-hook . org-fancy-priorities-mode))

(leaf org-rich-yank
  :url https://github.com/unhammer/org-rich-yank
  :doc "yank rich text"
  :straight t
  :bind
  (:org-mode-map
   ("M-S-s-v" . org-rich-yank)
   ("C-M-y" . org-rich-yank)))

(leaf toc-org
  :url https://github.com/snosov1/toc-org
  :doc "Table of Contensts"
  :straight t
  :hook (org-mode . toc-org-mode))

(provide 'init-orgmode)
;;; init-orgmode.el ends here
