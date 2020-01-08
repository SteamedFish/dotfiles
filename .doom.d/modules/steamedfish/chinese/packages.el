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
(package! posframe)
(when IS-LINUX
  (package! liberime-config
    :recipe (
              :host github
              :repo "merrickluo/liberime"
              :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el"))))
