;;; steamedfish/imenu-list/config.el -*- lexical-binding: t; -*-

(after! imenu-list
  (map!
   (:prefix-map ("o" . "open")
     :desc "imenu-list"                  :n "i"   #'+imenu-list/toggle)))
