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
  (cl-loop for font in '("Operator Mono Lig"
                         "Operator Mono"
                         "Fira Mono"
                         "Fira Code")
           if (member font (font-family-list))
           return
           (setq doom-font (font-spec :family font :size 12.5)
                 doom-big-font (font-spec :family font :size 21)))

  (if (member "Operator SSm" (font-family-list))
      (setq doom-variable-pitch-font (font-spec :family "Operator SSm"))
    (when (member "Fira Sans" (font-family-list))
      (setq doom-variable-pitch-font (font-spec :family "Fira Sans"))))
  (when (member "Hiragino Sans GB" (font-family-list))
    (setq doom-unicode-font (font-spec :family "Hiragino Sans GB" :size 14.5)))

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

(use-package! nyan-mode
  :after doom-modeline
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  (nyan-mode t))

(use-package! doom-themes
  :init
  (setq doom-theme 'doom-nord
    fancy-splash-image "~/.doom.d/banner/ue-colorful.png")
  :config
  (if (member "OperatorMono Nerd Font" (font-family-list))
      (custom-set-faces!
       '((font-lock-comment-face
          font-lock-constant-face
          font-lock-keyword-face)
         :slant italic))))

(after! hl-fill-column
  ;; by default it uses `brightblack' as background, which is not recognized.
  ;; set to a static value instead.
  (set-face-background 'hl-fill-column-face "#555555"))

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
