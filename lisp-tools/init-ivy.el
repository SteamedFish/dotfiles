;;; init-ivy.el --- config ivy -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  config ivy

;;; Code:

(leaf counsel
  :url https://oremacs.com/swiper/
  :straight t
  :blackout (ivy-mode counsel-mode)
  :global-minor-mode (ivy-mode)
  :hook (ivy-mode . counsel-mode)
  :pre-setq
  (enable-recursive-minibuffers . t)
  :bind
  ([remap isearch-forward] . swiper-isearch)
  ([remap isearch-backward] . swiper-isearch-backward)
  :custom
  (ivy-use-selectable-prompt . t)
  (ivy-use-virtual-buffers . t)
  (ivy-fixed-height-minibuffer . t))
  
(leaf prescient
  :url https://github.com/raxod502/prescient.el
  :straight t
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :url https://github.com/raxod502/prescient.el
  :after counsel
  :straight t
  :global-minor-mode ivy-prescient-mode)

(leaf all-the-icons-ivy-rich
  :url TODO
  :straight t
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))

(leaf ivy-rich
  :url TODO
  :straight t
  :hook ((counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . ivy-rich-project-root-cache-mode)))
 
(leaf ivy-posframe
  :url TODO
  :straight t
  :when window-system
  :hook (ivy-mode . ivy-posframe-mode))


(provide 'init-ivy)
;;; init-ivy.el ends here
