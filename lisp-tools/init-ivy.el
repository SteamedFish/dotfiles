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
  ([remap isearch-forward] . swiper-isearch)
  ([remap isearch-backward] . swiper-isearch-backward)
  ;; the default keybinding of C-x C-r was #'find-file-read-only
  ;; which is not very useful
  ("C-x C-r" . counsel-recentf)
  :setq
  (ivy-use-selectable-prompt . t)
  (ivy-use-virtual-buffers . t)
  (ivy-fixed-height-minibuffer . t)
  (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)))
  (counsel-describe-function-function . #'helpful-callable)
  (counsel-describe-variable-function . #'helpful-variable)
  (counsel-describe-symbol-function   . #'helpful-symbol))

(leaf prescient
  :url https://github.com/raxod502/prescient.el
  :straight t
  :disabled t
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :url https://github.com/raxod502/prescient.el
  :after counsel
  :straight t
  :global-minor-mode ivy-prescient-mode)

(leaf all-the-icons-ivy-rich
  :url https://github.com/seagle0128/all-the-icons-ivy-rich
  :straight t
  :global-minor-mode t)

(leaf ivy-rich
  :url https://github.com/Yevgnen/ivy-rich
  :straight t
  :global-minor-mode (ivy-rich-mode ivy-rich-project-root-cache-mode))

(leaf ivy-posframe
  :url https://github.com/tumashu/ivy-posframe
  :straight t
  :unless (file-directory-p (straight--build-dir "maple-minibuffer"))
  :when IS-GUI
  :blackout t
  :global-minor-mode t)


(provide 'init-ivy)
;;; init-ivy.el ends here
