;;; init-ivy.el --- config ivy -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config ivy

;;; Code:

(leaf counsel
  :url https://oremacs.com/swiper/
  :straight t
  :blackout (ivy-mode counsel-mode)
  :global-minor-mode (ivy-mode counsel-mode)
  :bind
  ([remap isearch-forward]                . swiper-isearch)
  ([remap isearch-backward]               . swiper-isearch-backward)
  ([remap apropos]                        . counsel-apropos)
  ([remap bookmark-jump]                  . counsel-bookmark)
  ([remap compile]                        . counsel-compile)
  ([remap describe-bindings]              . counsel-descbinds)
  ([remap describe-face]                  . counsel-faces)
  ([remap describe-function]              . counsel-describe-function)
  ([remap describe-variable]              . counsel-describe-variable)
  ([remap describe-symbol]                . counsel-describe-symbol)
  ([remap evil-show-marks]                . counsel-mark-ring)
  ([remap execute-extended-command]       . counsel-M-x)
  ([remap find-file]                      . counsel-find-file)
  ([remap find-library]                   . counsel-find-library)
  ([remap imenu]                          . counsel-imenu)
  ([remap info-lookup-symbol]             . counsel-info-lookup-symbol)
  ([remap load-theme]                     . counsel-load-theme)
  ([remap locate]                         . counsel-locate)
  ([remap org-goto]                       . counsel-org-goto)
  ([remap org-set-tags-command]           . counsel-org-tag)
  ([remap recentf-open-files]             . counsel-recentf)
  ([remap set-variable]                   . counsel-set-variable)
  ([remap swiper]                         . counsel-grep-or-swiper)
  ([remap insert-char]                    . counsel-unicode-char)
  ([remap yank-pop]                       . counsel-yank-pop)
  ;; the default keybinding of C-x C-r was #'find-file-read-only
  ;; which is not very useful
  ("C-x C-r" . counsel-recentf)
  :pre-setq
  ;; (ivy-re-builders-alist . '((counsel-rg      . ivy--regex-plus)
  ;;                            (swiper          . ivy--regex-plus)
  ;;                            (swiper-isearch  . ivy--regex-plus)
  ;;                            (t               . ivy--regex-fuzzy)))
  (ivy-more-chars-alist  . '((counsel-rg      . 1)
                             (counsel-search  . 2)
                             (counsel-grep    . 2)
                             (t               . 3)))
  (ivy-flx-limit . 10000)
  :setq
  (ivy-wrap                            . t)
  (ivy-use-selectable-prompt           . t)
  (ivy-use-virtual-buffers             . t)
  (ivy-fixed-height-minibuffer         . t)
  (ivy-sort-max-size                   . 7500)
  (ivy-use-virtual-buffers             . nil)
  (ivy-virtual-abbreviate              . 'full)
  (ivy-initial-inputs-alist            . nil)
  (counsel-describe-function-function  . #'helpful-callable)
  (counsel-describe-variable-function  . #'helpful-variable)
  (counsel-describe-symbol-function    . #'helpful-symbol)
  :config
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  (add-to-list 'savehist-additional-variables   'counsel-compile-history)
  ;; `counsel-imenu' -- no sorting for imenu. Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist        '(counsel-imenu))
  ;; `counsel-locate'
  (when IS-MAC
    ;; Use spotlight on mac by default since it doesn't need any additional setup
    (setq counsel-locate-cmd #'counsel-locate-cmd-mdfind)))

(leaf ivy-avy
  :url https://github.com/abo-abo/avy
  :straight t
  :config
  ;; this bind "C-'" to `avy-isearch' in `isearch-mode-map'
  (avy-setup-default))

(leaf prescient
  :url https://github.com/raxod502/prescient.el
  :straight t
  :pre-setq
  (prescient-filter-method           . '(literal regexp initialism fuzzy))
  :setq
  (prescient-sort-full-matches-first . t)
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :url https://github.com/raxod502/prescient.el
  :comment TODO: disable flx?
  :after counsel
  :straight t
  :global-minor-mode ivy-prescient-mode)

(leaf flx
  :url https://github.com/lewang/flx
  :straight t
  :require t)

(leaf all-the-icons-ivy-rich
  :url https://github.com/seagle0128/all-the-icons-ivy-rich
  :straight t
  :global-minor-mode t)

(leaf ivy-rich
  :url https://github.com/Yevgnen/ivy-rich
  :straight t
  :after all-the-icons-ivy-rich
  :global-minor-mode (ivy-rich-mode ivy-rich-project-root-cache-mode))

(leaf ivy-posframe
  :url https://github.com/tumashu/ivy-posframe
  :straight t
  ;; :unless (file-directory-p (straight--build-dir "maple-minibuffer"))
  :when IS-LINUX ;; TODO: I prefer maple-minibuffer, but it has some issue on Linux
  :when IS-GUI
  :blackout t
  :global-minor-mode t)


(provide 'init-ivy)
;;; init-ivy.el ends here
