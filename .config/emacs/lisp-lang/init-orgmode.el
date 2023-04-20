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
  :init
  (setq org-agenda-files (delete (expand-file-name "~/work/org/archive.org")
                                 (file-expand-wildcards (concat org-directory "*.org"))))
  :setq
  (org-startup-indented                . t)
  (org-startup-numerated               . t)
  (org-hide-emphasis-markers           . t)
  (org-pretty-entities                 . t)
  (org-startup-folded                  . nil)
  (org-link-descriptive                . t)
  (org-catch-invisible-edits           . 'smart)
  `(org-archive-location               . ,(concat org-directory "archive.org::* From %s"))
  (org-log-done                        . 'time)
  (org-log-done-with-time              . t)
  `(org-ellipsis                       . ,(if (char-displayable-p ?⌄) "  ⌄" nil))
  (org-startup-with-inline-images      . t)
  (org-html-validation-link            . nil)
  (org-agenda-include-diary            . t)
  (org-fontify-whole-heading-line      . t)
  (org-fontify-done-headline           . t)
  (org-fontify-quote-and-verse-blocks  . t)
  (org-confirm-babel-evaluate          . nil)
  (org-tags-column                     . 0)
  :config
  (custom-theme-set-faces 'user
                          '(org-level-1 ((t (:height 1.75 :inherit outline-1))))
                          '(org-level-2 ((t (:height 1.5  :inherit outline-2))))
                          '(org-level-3 ((t (:height 1.25 :inherit outline-3))))
                          '(org-level-4 ((t (:height 1.1  :inherit outline-4)))))
  :hook
  (org-babel-after-execute-hook   . org-redisplay-inline-images)
  :bind
  (:org-mode-map
   ([remap org-set-tags-command]  . counsel-org-tag)))

(leaf org-num
  :url https://orgmode.org/
  :straight nil
  :blackout org-num-mode
  ;; TODO: hl-line-mode will cause org-num looking bad
  :setq
  (org-num-skip-tags       . '("TOC"))
  (org-num-skip-footnotes  . t)
  (org-num-max-level       . 2))

(leaf org-src
  :straight nil
  :setq
  (org-src-preserve-indentation . nil))

(leaf org-capture
  :bind
  (:ctl-x-map
   ("c" . org-capture))
  :config
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U")))))

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

(leaf org-outer-indent
  :url https://github.com/rougier/org-outer-indent
  :straight (org-outer-indent :host github :repo "rougier/org-outer-indent")
  ;; we made org headlines larger, which breaks org-outer-indent-mode
  :disabled t
  :hook (org-mode-hook . org-outer-indent-mode))

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
  :hook (org-mode-hook . toc-org-mode)
  :setq
  (toc-org-max-depth . 5))

(leaf org-bars
  :url https://github.com/tonyaldon/org-bars
  :doc adds bars to the virtual indentation
  :straight (org-bars :host github :repo "tonyaldon/org-bars")
  :disabled t
  :hook (org-mode-hook . org-bars-mode))

(leaf ox-pandoc
  :url https://github.com/kawabata/ox-pandoc
  :straight t
  :ensure-system-package pandoc
  :unless noninteractive
  :require t
  :setq
  (org-pandoc-options                 . '((standalone . t)))
  (org-pandoc-options-for-beamer-pdf  . '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf   . '((pdf-engine . "xelatex")))
  ;; special extensions for markdown_github output
  (org-pandoc-format-extensions       . '(markdown_github+pipe_tables+raw_html)))

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
  (org-appear-autolinks       . t)
  (org-appear-autosubmarkers  . t)
  (org-appear-autoemphasis    . t)
  (org-appear-autoentities    . t)
  (org-appear-autokeywords    . t)
  :hook (org-mode-hook . org-appear-mode))

(leaf org-tree-slide
  :url https://github.com/takaxp/org-tree-slide
  :straight t
  :bind
  (:org-tree-slide-mode-map
   ("<left>" . org-tree-slide-move-previous-tree)
   ("<right>" . org-tree-slide-move-next-tree)
   ("q" . org-tree-slide-mode))
  :hook ((org-tree-slide-play-hook . (lambda ()
                                       (evil-normal-state)
                                       (text-scale-increase 3)
                                       (read-only-mode 1)
                                       (writeroom-mode 1)
                                       (darkroom-mode 1)
                                       ;; (centered-window-mode 1)
                                       (flyspell-mode -1)
                                       (display-line-numbers-mode -1)))
         (org-tree-slide-stop-hook . (lambda ()
                                       (display-line-numbers-mode 1)
                                       (flyspell-mode 1)
                                       ;; (centered-window-mode -1)
                                       (darkroom-mode -1)
                                       (writeroom-mode -1)
                                       (read-only-mode -1)
                                       (text-scale-increase 0)))
         (org-tree-slide-after-narrow-hook . org-display-inline-images))
  ;; :config
  ;; different profiles are actually different setq presets
  ;; we set them by ourself
  ;; (org-tree-slide-presentation-profile)
  :setq
  (org-tree-slide-header                       . t)
  (org-tree-slide-content-margin-top           . 2)
  (org-tree-slide-slide-in-blank-lines         . 10)
  (org-tree-slide-slide-in-effect              . t)
  (org-tree-slide-heading-emphasis             . nil)
  (org-tree-slide-cursor-init                  . t)
  (org-tree-slide-modeline-display             . 'outside)
  (org-tree-slide-skip-done                    . nil)
  (org-tree-slide-never-touch-face             . t)
  (org-tree-slide-skip-comments                . t)
  (org-tree-slide-skip-outline-level           . 0)
  (org-tree-slide-breadcrumbs                  . " ➥ ")
  (org-tree-slide-breadcrumbs-hide-todo-state  . nil))

(leaf org-roam
  :url https://www.orgroam.com
  :straight t
  :after org
  :setq
  `(org-roam-directory  . ,(concat org-directory "roam/"))
  (org-roam-v2-ack      . t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (org-roam-db-autosync-mode t))

(leaf org-roam-ui
  :url https://github.com/org-roam/org-roam-ui
  :straight (org-roam-ui :host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :blackout (org-roam-ui-mode org-roam-ui-follow-mode)
  ;; :advice
  ;; (:before save-buffers-kill-emacs httpd-stop)
  :setq
  (org-roam-ui-sync-theme      . t)
  (org-roam-ui-follow          . t)
  (org-roam-ui-update-on-save  . t)
  (org-roam-ui-open-on-start   . t)
  :config
  (unless noninteractive
    (org-roam-ui-mode t)))

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
  (elgantt-header-type                                 . 'outline)
  (elgantt-insert-blank-line-between-top-level-header  . t)
  (elgantt-startup-folded                              . nil)
  (elgantt-draw-overarching-headers                    . t))

(leaf literate-calc-mode
  :url https://github.com/sulami/literate-calc-mode.el
  :straight t)

(leaf valign
  :url https://github.com/casouri/valign
  :blackout t
  :unless noninteractive
  :straight t
  :hook
  (org-mode-hook       . valign-mode)
  (markdown-mode-hook  . valign-mode))

(leaf restclient
  :url https://github.com/pashky/restclient.el
  :straight t)

(leaf ob-restclient
  :url https://github.com/alf/ob-restclient.el
  :straight t
  :require t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

(leaf org-alert
  :url https://github.com/spegoraro/org-alert
  :straight t
  :require t
  ;; org-alert hangs up emacs 29
  :disabled t
  :config (org-alert-enable))

(leaf org-pretty-tags
  :url https://gitlab.com/marcowahl/org-pretty-tags
  :straight t
  :blackout org-pretty-tags-mode
  :global-minor-mode org-pretty-tags-global-mode)

(leaf org-gcal
  :url https://github.com/myuhe/org-gcal.el
  :doc "when first running you must run it manually to let it popup login window"
  :straight t
  ;; switched to org-caldav
  :disabled t
  :init
  (run-at-time "5 min" 300 #'org-gcal-sync t)
  :setq
  (org-gcal-notify-p . nil)
  :config
  (setq org-gcal-client-id     (auth-source-pass-get "login" "shopee/gcal")
        org-gcal-client-secret (auth-source-pass-get 'secret "shopee/gcal"))
  (setq org-gcal-fetch-file-alist
        `((,(auth-source-pass-get "login" "shopee/email") . ,(expand-file-name (concat org-directory "gcal.org"))))))

(leaf oauth2
  :url https://elpa.gnu.org/packages/oauth2.html
  :straight t
  :doc required by org-caldav)

(leaf org-caldav
  :url https://github.com/dengste/org-caldav
  :straight t
  :setq
  (org-caldav-url  . 'google)
  :config
  ;;(setq org-caldav-calendar-id (concat (url-hexify-string (auth-source-pass-get "login" "shopee/email")) "@group.v.calendar.google.com"))
  (setq org-caldav-calendar-id (auth-source-pass-get "login" "shopee/email"))
  (setq org-caldav-oauth2-client-id     (auth-source-pass-get "login" "shopee/gcal")
        org-caldav-oauth2-client-secret (auth-source-pass-get 'secret "shopee/gcal"))
  (setq org-caldav-files
        (list (expand-file-name (concat org-directory "gcal.org"))))
  (setq org-caldav-inbox
        (expand-file-name (concat org-directory "gcal.org"))))

(leaf iscroll
  :url https://github.com/casouri/iscroll
  :straight t
  :blackout t
  :hook (text-mode-hook . iscroll-mode))

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
  `(oer-reveal-org-includes-dir         . ,(concat my-data-dir "etc/oer-reveal-org-include-dir"))
  :setq
  (emacs-reveal-managed-install-p       . t)
  (org-re-reveal-theme                  . "moon")
  (org-re-reveal-embed-local-resources  . nil)
  (org-re-reveal-progress               . t)
  (org-re-reveal-single-file            . nil)
  (org-re-reveal-slide-number           . t)
  (org-re-reveal-export-notes-to-pdf    . t)
  :config
  (oer-reveal-setup-submodules t)
  (oer-reveal-generate-include-files t)
  (oer-reveal-publish-setq-defaults)
  ;; (setq org-re-reveal-root (concat (file-name-directory (locate-library "emacs-reveal")) "emacs-reveal-submodules/reveal.js"))
  :require t)

(provide 'init-orgmode)
;;; init-orgmode.el ends here
