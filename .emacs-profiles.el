;;; .emacs-profiles.el  -*- lexical-binding: t; -*-


(("default" . ((user-emacs-directory . "~/.config/emacs")))
 ("spacemacs" . ((user-emacs-directory . "~/emacs-distros/spacemacs")
                 (env . (("SPACEMACSDIR" . "~/.spacemacs.d")))))
 ("doom" . ((user-emacs-directory . "~/emacs-distros/doom-emacs")
            (env . (("DOOMDIR" . "~/.config/doom")))))
 ("test". ((user-emacs-directory . "~/.emacs.d-test")))
 ("centaur". ((user-emacs-directory . "~/emacs-distros/centaur"))))
