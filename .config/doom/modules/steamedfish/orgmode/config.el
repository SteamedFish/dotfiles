;;; steamedfish/orgmode/config.el  -*- lexical-binding: t; -*-

(after! org
  ;; make org-ellipsis's face same as current heading,
  ;; instead of having its own
  (custom-set-faces!
   '(org-ellipsis :foreground nil))
  (setq org-directory (expand-file-name "~/work/org/")
    org-roam-directory (file-name-as-directory (expand-file-name (concat org-directory "/roam")))
    org-roam-db-location (expand-file-name "org-roam.db" doom-cache-dir)
    diary-file (expand-file-name (concat org-directory "/diary"))
    ;; all files but later.org should be put in agenda
    org-agenda-files
    (delete (expand-file-name "~/work/org/later.org")
      (file-expand-wildcards (concat org-directory "*.org")))
    ;; one archive file instead of many
    org-archive-location
    (concat org-directory "archive.org::* From %s")
    org-log-done 'time
    org-log-done-with-time t
    org-ellipsis (if (char-displayable-p ?⬎) "  ⬎" nil)
    org-startup-with-inline-images t
    org-html-validation-link nil
    org-agenda-include-diary t)

  ;; refresh images after C-c C-c
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (when (featurep! :lang plantuml)
    (add-to-list 'org-src-lang-modes
                 '("plantuml" . plantuml))))

(use-package! ox-confluence
  :after org
  :commands org-confluence-export-as-confluence)

(after! org-clock
  (setq
   org-clock-into-drawer t
   org-clock-out-remove-zero-time-clocks t))

(after! plantuml-mode
  (setq plantuml-jar-path
        (cond
         (IS-MAC
          "/usr/local/opt/plantuml/libexec/plantuml.jar")
         (IS-LINUX
          "/usr/share/java/plantuml/plantuml.jar"))
        org-plantuml-jar-path plantuml-jar-path))

(after! ob-mermaid
  ;; install `mermaid-cli' AUR package
  (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

(use-package! org-noter
  :commands (org-noter org-noter-insert-note)
  :init
  (map! :map (pdf-view-mode-map nov-mode-map)
        ;; use N to create new note file
        ;; i to insert new note
        ;; K to kill the note file
        :n   "N" #'org-noter
        :n   "i" #'org-noter-insert-note
        :n   "K" #'org-noter-kill-session)
  (map! :map 'org-mode-map
        :n   "n" 'org-noter)
  :config
  (setq org-noter-default-notes-file-names '("ref-notes-by-steamedfish.org")
        org-noter-auto-save-last-location t
        org-noter-always-create-frame nil
        org-noter-insert-note-no-questions t))

(use-package! valign
  :after org
  :commands valign-mode
  :init
  (add-hook 'org-mode-hook #'valign-mode)
  :config
  (setq valign-fancy-bar t))

(use-package! ftable
  :commands (ftable-fill ftable-reformat))

(after! org-tree-slide
  :config
  (setq org-tree-slide-modeline-display 'outside
    org-tree-slide-fold-subtrees-skipped nil))

(after! org-roam
  ;; https://github.com/org-roam/org-roam-server/issues/115#issuecomment-730006834
  (smartparens-global-mode -1)
  (org-roam-server-mode)
  (smartparens-global-mode +1))

(use-package! org-roam-server
  :after org
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
