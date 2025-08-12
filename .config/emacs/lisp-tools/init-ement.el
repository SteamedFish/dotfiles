;;; init-ement.el --- setup ement.el -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs.

;;; Commentary:

;;  setup ement.el

;;; Code:

(leaf plz
  :url https://github.com/alphapapa/plz.el
  :doc HTTP library for Emacs
  :disabled t
  :straight (plz :host github :repo "alphapapa/plz.el"))

(leaf ement
  :url https://github.com/alphapapa/ement.el
  :doc matrix client for Emacs
  :straight (ement :host github :repo "alphapapa/ement.el")
  :disabled t
  :setq
  (ement-save-sessions . t)
  `(ement-sessions-file . ,(expand-file-name (concat my-data-dir "data/ement.el"))))


(provide 'init-ement)
;;; init-ement.el ends here
