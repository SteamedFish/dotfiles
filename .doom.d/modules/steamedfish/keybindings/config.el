;;; steamedfish/keybindings/config.el  -*- lexical-binding: t; -*-

(map!
 ;; those super related keybindings are copied from doom
 ;; just removes (when IS-MAC) because I want them work in Linux

 ;; from ~/.emacs.d/modules/config/default/+evil-bindings.el
 (:when (featurep! :ui workspaces)
   :g "s-t"   #'+workspace/new
   :g "s-T"   #'+workspace/display
   :n "s-1"   #'+workspace/switch-to-0
   :n "s-2"   #'+workspace/switch-to-1
   :n "s-3"   #'+workspace/switch-to-2
   :n "s-4"   #'+workspace/switch-to-3
   :n "s-5"   #'+workspace/switch-to-4
   :n "s-6"   #'+workspace/switch-to-5
   :n "s-7"   #'+workspace/switch-to-6
   :n "s-8"   #'+workspace/switch-to-7
   :n "s-9"   #'+workspace/switch-to-8
   :n "s-0"   #'+workspace/switch-to-final)

 ;; from ~/.emacs.d/modules/config/default/config.el
 "s-`" #'other-frame  ; fix frame-switching
 ;; fix OS window/frame navigation/manipulation keys
 "s-w" #'delete-window
 "s-W" #'delete-frame
 "s-n" #'+default/new-buffer
 "s-N" #'make-frame
 "s-q" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
 "C-s-f" #'toggle-frame-fullscreen
 ;; Restore somewhat common navigation
 "s-l" #'goto-line
 ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
 ;; it imposes some other functionality and overhead we don't need)
 "s-f" #'swiper
 "s-z" #'undo
 "s-Z" #'redo
 "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "s-v" #'yank
 "s-s" #'save-buffer
 :v "s-x" #'kill-region
 ;; Buffer-local font scaling
 "s-+" #'doom/reset-font-size
 "s-=" #'doom/increase-font-size
 "s--" #'doom/decrease-font-size
 ;; Conventional text-editing keys & motions
 "s-a" #'mark-whole-buffer
 "s-/" (λ! (save-excursion (comment-line 1)))
 :n "s-/" #'evilnc-comment-or-uncomment-lines
 :v "s-/" #'evilnc-comment-operator
 :gi  [s-backspace] #'doom/backward-kill-to-bol-and-indent
 :gi  [s-left]      #'doom/backward-to-bol-or-indent
 :gi  [s-right]     #'doom/forward-to-last-non-comment-or-eol
 :gi  [M-backspace] #'backward-kill-word
 :gi  [M-left]      #'backward-word
 :gi  [M-right]     #'forward-word

 ;; the following are my own configuration
 :leader
 (:prefix ("b" . "buffer")
   :desc "Read Only"                     :n "r"   #'view-mode
   :desc "Revert Edit"                   :n "R"   #'revert-buffer)
 (:prefix ("F" . "Frame")
   :desc "Delete Frame"                  :n "d"   #'delete-frame
   :desc "Delete Other Frame"            :n "D"   #'delete-other-frame
   :desc "Other Frame"                   :n "o"   #'other-frame
   :desc "Make Frame"                    :n "n"   #'make-frame
   :desc "Switch to buffer Other Frame"  :n "f"   #'switch-to-buffer-other-frame
   :desc "Toggle Frame Maximized"        :n "F"   #'toggle-frame-maximized
   :desc "Toggle Frame Fullscreen"       :n "C-F" #'toggle-frame-fullscreen
   :desc "Display buffer Other Frame"    :n "O"   #'display-buffer-other-frame))

;; when at the edge of buffers, go to the next node
(after! info
  (map!
   :map Info-mode-map
   :n "j"    (λ!
              (condition-case nil
                  (evil-next-line)
                (error (Info-scroll-up))))
   :n "k"    (λ!
              (condition-case nil
                  (evil-previous-line)
                (error (Info-scroll-down))))))
