;;; steamedfish/ui/config.el -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; emacs-mac special
  (when (eq window-system 'mac)
    (setq mac-auto-operator-composition-characters
          "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~")
    (mac-auto-operator-composition-mode))
  (when (eq window-system 'ns)
    (setq ns-use-thin-smoothing t
          ns-use-native-fullscreen nil
          ns-use-fullscreen-animation t))

  ;; fonts
  (cl-loop for font in '(
                         "Rec Mono Duotone"
                         "Rec Mono Casual"
                         "Rec Mono SemiCasual"
                         "Rec Mono Linear"
                         "Victor Mono"
                         "Roboto Mono"
                         "mononoki Nerd Font"
                         "Fira Mono"
                         "Fira Code")
           if (member font (font-family-list))
           return
           (setq doom-font (font-spec :family font :size 12.5)
                 doom-big-font (font-spec :family font :size 21)))

  (if (member "Recursive Sans Casual Static" (font-family-list))
      (setq doom-variable-pitch-font (font-spec :family "Recursive Sans Casual Static"))
    (when (member "Fira Sans" (font-family-list))
      (setq doom-variable-pitch-font (font-spec :family "Fira Sans"))))
  (when (member "Hiragino Sans GB" (font-family-list))
    (setq doom-unicode-font (font-spec :family "Hiragino Sans GB")))

  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font 't 'symbol
      (font-spec :family "Noto Color Emoji")
      nil 'prepend))

  (cl-loop for font in '("Hiragino Sans GB"
                         "Sarasa Mono SC"
                         "Noto Sans CJK SC"
                         "Source Han Sans SC")
           if (member font (font-family-list))
           return
           (set-fontset-font t 'han (font-spec :family font) nil 'prepend))

  (add-hook 'window-setup-hook #'toggle-frame-maximized)
  (add-hook 'window-setup-hook #'+ui/toggle-transparency))

(map!
 "s-u"                         #'+ui/toggle-transparency
 :leader
 :prefix ("t" . "toggle")
 :desc "Transparency"
 :n "t" #'+ui/toggle-transparency)

(after! doom-modeline
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-enable-word-count t
        doom-modeline-indent-info t))

;; (use-package! nyan-mode
;;   :after doom-modeline
;;   :init
;;   (setq nyan-animate-nyancat t
;;         nyan-wavy-trail t)
;;   (nyan-mode t))

(after! evil
  ;; create split window at the more correct place
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(use-package! doom-themes
  :init
  (setq doom-theme 'doom-nord
    fancy-splash-image "~/.config/doom/banner/ue-colorful.png"
    doom-themes-treemacs-theme "doom-colors")
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t))

(after! hl-fill-column
  ;; by default it uses `brightblack' as background, which is not recognized.
  ;; set to a static value instead.
  (set-face-background 'hl-fill-column-face "#555555"))

(use-package! highlight-indent-guides
  :init
  ;; when use character, the default caracter ?\2502
  ;; which is https://unicode-table.com/en/2502/ │
  ;; may have wrong width with certain fonts
  (setq highlight-indent-guides-method 'column))

(use-package! paradox
  :after-call list-packages paradox-list-packages
  :init
  (define-key!
    [remap list-packages]            #'paradox-list-packages)
  :config
  (setq paradox-github-token (+pass-get-secret "env/paradox")
        paradox-display-star-count t
        paradox-display-download-count t
        paradox-execute-asynchronously t
        paradox-display-buffer-name t)
  (map!
   :map paradox-menu-mode-map
   "H"  #'paradox-menu-quick-help
   "J"  #'paradox-next-describe
   "K"  #'paradox-previous-describe
   "L"  #'paradox-menu-view-commit-list
   "o"  #'paradox-menu-visit-homepage
   "j"  #'paradox-next-entry
   "k"  #'paradox-previous-entry)
  (paradox-enable))

;; Display icons for all buffers in ibuffer.
;; (use-package! all-the-icons-ivy-rich
;;   :after ivy-rich
;;   :init
;;   (all-the-icons-ivy-rich-mode t))

;; Display icons for all buffers in ivy-rich.
;; (use-package! all-the-icons-ibuffer
;;   :after ibuffer
;;   :init
;;   (all-the-icons-ibuffer-mode t))
