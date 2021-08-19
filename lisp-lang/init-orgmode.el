;;; init-orgmode.el --- orgmode mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  orgmode mode

;;; Code:

(leaf org
  :url https://orgmode.org/
  :straight t
  :blackout org-indent-mode
  :blackout org-num-mode
  :setq
  (org-startup-indented . t)
  (org-startup-numerated . t)
  (org-startup-folded . nil)
  `(org-directory . ,(expand-file-name "~/work/org/"))
  (org-agenda-files . org-directory)
  (org-log-done . 'time)
  (org-cache-invisible-edits . 'smart)
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
  (org-babel-do-load-languages 'org-babel-do-load-languages 'org-babel-do-load-languages)
  :bind
  (:org-mode-map
   ([remap org-set-tags-command] . counsel-org-tag)))

(leaf org-contrib
  :url https://orgmode.org/worg/org-contrib/
  :straight t)

(leaf evil-org
  :url https://github.com/Somelauw/evil-org-mode
  :straight t
  :require (evil-org evil-org-agenda)
  :hook (org-mode-hook . evil-org-mode)
  :pre-setq
  (evil-org-key-theme . '(navigation
                          insert
                          return ;; TODO
                          textobjects
                          additional
                          shift ;; TODO
                          todo ;; TODO
                          heading ;; TODO
                          calendar))
  :config
  (evil-org-set-key-theme)
  (evil-org-agenda-set-keys))


(leaf ob-go
  :url https://github.com/pope/ob-go
  :straight t
  :config
  (add-to-list 'org-babel-do-load-languages '(go . t))
  (org-babel-do-load-languages 'org-babel-do-load-languages 'org-babel-do-load-languages))

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

(leaf org-mime
  :url https://github.com/org-mime/org-mime
  :doc "sends HTML email with orgmode"
  :straight t
  :tag "todo" "need keybinding")

(leaf org-timeline
  :url https://github.com/Fuco1/org-timeline
  :doc "Add graphical view of agenda timeline"
  :straight t
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

(leaf org-fragtog
  :url https://github.com/io12/org-fragtog
  :doc "Automatically toggle LaTeX fragment previews"
  :straight t
  :hook (org-mode-hook . org-fragtog-mode))

(leaf org-preview-html
  :url https://github.com/lujun9972/org-preview-html
  :doc "M-x org-preview-html-mode"
  :straight t)

(leaf org-tree-slide
  :url https://github.com/takaxp/org-tree-slide
  :straight t
  :bind
  (:org-tree-slide-mode-map
   ("<left>" . org-tree-slide-move-previous-tree)
   ("<right>" . org-tree-slide-move-next-tree))
  :hook ((org-tree-slide-play . (lambda ()
                                  (text-scale-increase 4)
                                  (org-display-inline-images)
                                  (read-only-mode 1)))
         (org-tree-slide-stop . (lambda ()
                                  (text-scale-increase 0)
                                  (org-remove-inline-images)
                                  (read-only-mode -1))))
  :pre-setq
  (org-tree-slide-header . nil)
  (org-tree-slide-slide-in-effect . t)
  (org-tree-slide-heading-emphasis . nil)
  (org-tree-slide-cursor-init . t)
  (org-tree-slide-modeline-display . 'outside)
  (org-tree-slide-skip-done . nil)
  (org-tree-slide-skip-comments . t)
  (org-tree-slide-skip-outline-level . 3))

(leaf org-roam
  :url https://www.orgroam.com
  :straight t
  :config (org-roam-setup)
  :pre-setq
  `(org-roam-directory . (concat org-directory "roam/"))
  (org-roam-v2-ack . t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory)))

(leaf org-super-agenda
  :url https://github.com/alphapapa/org-super-agenda
  :straight t
  :global-minor-mode org-super-agenda-mode)

(leaf elgantt
  :url https://github.com/legalnonsense/elgantt
  :doc "A Gantt chart/calendar for Orgmode"
  :doc "M-x elgantt-open"
  :straight t
  :setq
  (elgantt-header-type . 'outline)
  (elgantt-insert-blank-line-between-top-level-header . t)
  (elgantt-startup-folded . nil)
  (elgantt-show-header-depth . t)
  (elgantt-draw-overarching-headers . t))

(leaf literate-calc-mode
  :url https://github.com/sulami/literate-calc-mode.el
  :straight t)

(provide 'init-orgmode)
;;; init-orgmode.el ends here
