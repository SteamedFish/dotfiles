;; -*- no-byte-compile: t; -*-
;;; steamedfish/chinese/packages.el

(if (featurep! cnfontx)
    (package! cnfonts))
(package! youdao-dictionary)
(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv"))
(package! cal-china-x)
(package! pangu-spacing
  :disable t)
(package! pyim
  :disable t)
(package! fcitx
  :disable t)
(package! posframe)
(package! rime)
(package! pinyinlib)
