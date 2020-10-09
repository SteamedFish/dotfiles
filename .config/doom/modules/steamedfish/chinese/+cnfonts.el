;;; steamedfish/chinese/+cnfonts.el -*- lexical-binding: t; -*-

(use-package! cnfonts
  :defer t
  :when (display-graphic-p)
  :init (cnfonts-enable)
  :config
  (setq
   cnfonts-default-step 4
   cnfonts-directory (concat doom-etc-dir "cnfonts/")
   cnfonts-personal-fontnames
   '(
     ("Noto Mono" "Noto Sans" "Noto Serif")
     ("Hiragino Sans GB" "Source Han Sans SC" "Source Han Serif SC" "Noto Sans CJK SC" "Noto Sans Mono CJK SC" "Noto Serif CJK SC" "Sarasa Gothic SC" "Sarasa Mono T SC" "Sarasa UI SC" "Sarasa Mono SC")
     ("Hiragino Sans GB" "Source Han Sans SC" "Source Han Serif SC" "Noto Sans CJK SC" "Noto Sans Mono CJK SC" "Noto Serif CJK SC" "Sarasa Gothic SC" "Sarasa Mono T SC" "Sarasa UI SC" "Sarasa Mono SC")
     ))
  (map!
   :leader
   :prefix ("t" . "toggle")
   :desc "Increase Fontsize"  :n "+"  #'cnfonts-increase-fontsize
   :desc "Decrease Fontsize"  :n "-"  #'cnfonts-decrease-fontsize
   :desc "Reset Fontsize"     :n "0"  #'cnfonts-reset-fontsize))
