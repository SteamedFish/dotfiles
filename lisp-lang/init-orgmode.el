;;; init-orgmode.el --- orgmode mode -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  orgmode mode

;;; Code:


;; Thie setq must set outside leaf block
(setq org-directory (expand-file-name "~/work/org/"))

(leaf org
  :url https://orgmode.org/
  :straight t
  :blackout org-indent-mode
  :blackout org-num-mode
  :init
  (setq org-agenda-files (delete (expand-file-name "~/work/org/archive.org")
                                 (file-expand-wildcards (concat org-directory "*.org"))))
  :setq
  (org-startup-indented . t)
  (org-startup-numerated . t)
  (org-num-skip-tags . '("TOC"))
  (org-hide-emphasis-markers . t)
  (org-pretty-entities . t)
  (org-startup-folded . nil)
  (org-link-descriptive . t)
  (org-log-done . 'time)
  (org-catch-invisible-edits . 'smart)
  `(org-archive-location . ,(concat org-directory "archive.org::* From %s"))
  (org-log-done . 'time)
  (org-log-done-with-time . t)
  `(org-ellipsis . ,(if (char-displayable-p ?) "  " nil))
  (org-startup-with-inline-images . t)
  (org-html-validation-link . nil)
  (org-agenda-include-diary . t)
  (org-fontify-whole-heading-line . t)
  (org-fontify-done-headline . t)
  (org-fontify-quote-and-verse-blocks . t)
  :config
  (custom-theme-set-faces 'user
                          '(org-level-1 ((t (:height 1.75 :inherit outline-1))))
                          '(org-level-2 ((t (:height 1.5 :inherit outline-2))))
                          '(org-level-3 ((t (:height 1.25 :inherit outline-3))))
                          '(org-level-4 ((t (:height 1.1 :inherit outline-4)))))
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images)
  :bind
  (:org-mode-map
   ([remap org-set-tags-command] . counsel-org-tag)))

(leaf org-contrib
  :url https://orgmode.org/worg/org-contrib/
  :straight t
  :tag "TODO" ;; need to require each contrib file
  :require ox-confluence)

(leaf evil-org
  :url https://github.com/Somelauw/evil-org-mode
  :straight t
  :blackout evil-org-mode
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


(leaf ox-gfm
  :url https://github.com/larstvei/ox-gfm
  :doc "Convert to Github Flavored Markdown"
  :straight t
  :require t)

(leaf org-superstar
  :url https://github.com/integral-dw/org-superstar-mode
  :straight t
  :when IS-GUI
  :setq
  (org-superstar-special-todo-items . t)
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
  :hook (org-mode-hook . toc-org-mode))

(leaf ox-pandoc
  :url https://github.com/kawabata/ox-pandoc
  :straight t
  :ensure-system-package pandoc
  :require t
  :setq
  (org-pandoc-options . '((standalone . t)))
  (org-pandoc-options-for-beamer-pdf . '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf . '((pdf-engine . "xelatex")))
  ;; special extensions for markdown_github output
  (org-pandoc-format-extensions . '(markdown_github+pipe_tables+raw_html)))

(leaf org-mime
  :url https://github.com/org-mime/org-mime
  :doc "sends HTML email with orgmode"
  :straight t
  :tag "TODO" "need keybinding")

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

(leaf org-appear
  :url https://github.com/awth13/org-appear
  :doc "automatically show emphasis markers"
  :straight t
  :setq
  (org-appear-autolinks . t)
  (org-appear-autosubmarkers . t)
  (org-appear-autoemphasis . t)
  (org-appear-autoentities . t)
  (org-appear-autokeywords . t)
  :hook (org-mode-hook . org-appear-mode))

(leaf org-tree-slide
  :url https://github.com/takaxp/org-tree-slide
  :straight t
  :bind
  (:org-tree-slide-mode-map
   ("<left>" . org-tree-slide-move-previous-tree)
   ("<right>" . org-tree-slide-move-next-tree))
  :hook ((org-tree-slide-play-hook . (lambda ()
                                       (text-scale-increase 5)
                                       (read-only-mode 1)
                                       (writeroom-mode 1)
                                       (display-line-numbers-mode -1)))
         (org-tree-slide-stop-hook . (lambda ()
                                       (text-scale-increase 0)
                                       (read-only-mode -1)
                                       (writeroom-mode -1)
                                       (display-line-numbers-mode 1)))
         (org-tree-slide-after-narrow-hook . org-display-inline-images))
  :pre-setq
  (org-tree-slide-header . nil)
  (org-tree-slide-slide-in-effect . t)
  (org-tree-slide-heading-emphasis . t)
  (org-tree-slide-cursor-init . t)
  (org-tree-slide-modeline-display . 'outside)
  (org-tree-slide-skip-done . nil)
  (org-tree-slide-skip-comments . t)
  (org-tree-slide-skip-outline-level . 0)
  (org-tree-slide-breadcrumbs . "-->")
  :config
  (org-tree-slide-presentation-profile))

(leaf org-roam
  :url https://www.orgroam.com
  :straight t
  :after org
  :setq
  `(org-roam-directory . ,(concat org-directory "roam/"))
  (org-roam-v2-ack . t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory)))

(leaf org-super-agenda
  :url https://github.com/alphapapa/org-super-agenda
  :straight t
  :global-minor-mode org-super-agenda-mode)

(leaf org-ql
  :url https://github.com/alphapapa/org-ql
  :straight t)

(leaf elgantt
  :url https://github.com/legalnonsense/elgantt
  :doc "A Gantt chart/calendar for Orgmode"
  :doc "M-x elgantt-open"
  :straight (elgantt :host github :repo "legalnonsense/elgantt")
  :setq
  (elgantt-header-type . 'outline)
  (elgantt-insert-blank-line-between-top-level-header . t)
  (elgantt-startup-folded . nil)
  (elgantt-show-header-depth . t)
  (elgantt-draw-overarching-headers . t))

(leaf literate-calc-mode
  :url https://github.com/sulami/literate-calc-mode.el
  :straight t)

(leaf valign
  :url https://github.com/casouri/valign
  :blackout t
  :straight t
  :hook (org-mode-hook . valign-mode))

(leaf org-alert
  :url https://github.com/spegoraro/org-alert
  :straight t
  :require t
  :config (org-alert-enable))

(leaf org-pretty-tags
  :url https://gitlab.com/marcowahl/org-pretty-tags
  :straight t
  :blackout org-pretty-tags-mode
  :global-minor-mode org-pretty-tags-global-mode)

(leaf org-menu
  :url https://github.com/sheijk/org-menu
  :straight (org-menu :host github :repo "sheijk/org-menu")
  :doc  "M-x org-menu"
  ;; currently not working
  :tag "TODO")

(leaf emacs-reveal
  :url https://gitlab.com/oer/emacs-reveal
  :doc https://gitlab.com/oer/emacs-reveal-howto
  :doc "BUG: must disable hl-todo and tree-sitter otherwise export will fail"
  :straight (emacs-reveal :host gitlab :repo "oer/emacs-reveal" :files ("*" ".*"))
  :disabled t
  :pre-setq
  `(oer-reveal-org-includes-dir . ,(concat my-data-dir "etc/oer-reveal-org-include-dir"))
  :setq
  (emacs-reveal-managed-install-p . t)
  (org-re-reveal-theme . "moon")
  (org-re-reveal-embed-local-resources . nil)
  (org-re-reveal-progress . t)
  (org-re-reveal-single-file . nil)
  (org-re-reveal-slide-number . t)
  (org-re-reveal-export-notes-to-pdf . t)
  :config
  (oer-reveal-setup-submodules t)
  (oer-reveal-generate-include-files t)
  (oer-reveal-publish-setq-defaults)
  ;; (setq org-re-reveal-root (concat (file-name-directory (locate-library "emacs-reveal")) "emacs-reveal-submodules/reveal.js"))
  :require t)

(provide 'init-orgmode)
;;; init-orgmode.el ends here
