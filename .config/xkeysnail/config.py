#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# Mac-like keybindings
# https://support.apple.com/zh-cn/HT201236
define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "konsole"), {
    K("Super-x"): [K("C-x"), set_mark(False)],
    K("Super-c"): [K("C-c"), set_mark(False)],
    K("Super-v"): [K("C-v"), set_mark(False)],
    K("Super-z"): [K("C-z"), set_mark(False)],
    K("Super-a"): [K("C-home"), K("C-a"), set_mark(True)],
    K("Super-f"): [K("C-f"), set_mark(False)],
    K("Super-o"): K("C-o"),
    K("Super-s"): K("C-s"),
    K("Super-TAB"): K("C-TAB"),
}, "Mac-like keys")

# Emacs-like keybindings in non-Emacs applications
define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "konsole"), {
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-p"): with_mark(K("up")),
    K("C-n"): with_mark(K("down")),
    K("C-h"): with_mark(K("backspace")),
    # Forward/Backward word
    K("M-b"): with_mark(K("C-left")),
    K("M-f"): with_mark(K("C-right")),
    # Beginning/End of line
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    # Page up/down
    K("M-v"): with_mark(K("page_up")),
    K("C-v"): with_mark(K("page_down")),
    # Beginning/End of file
    K("M-Shift-comma"): with_mark(K("C-home")),
    K("M-Shift-dot"): with_mark(K("C-end")),
    # Newline
    K("C-m"): K("enter"),
    K("C-j"): K("enter"),
    K("C-o"): [K("enter"), K("left")],
    # Copy
    K("C-w"): [K("C-x"), set_mark(False)],
    K("M-w"): [K("C-c"), set_mark(False)],
    K("C-y"): [K("C-v"), set_mark(False)],
    # Delete
    K("C-d"): [K("delete"), set_mark(False)],
    K("M-d"): [K("C-delete"), set_mark(False)],
    # Kill line
    K("C-k"): [K("Shift-end"), K("C-x"), set_mark(False)],
    # Undo
    K("C-slash"): [K("C-z"), set_mark(False)],
    K("C-Shift-ro"): K("C-z"),
    # Mark
    #K("C-space"): set_mark(True),
    K("C-M-space"): with_or_set_mark(K("C-right")),
    # Search
    K("C-s"): K("F3"),
    K("C-r"): K("Shift-F3"),
    K("M-Shift-key_5"): K("C-h"),
    # Cancel
    K("C-g"): [K("esc"), set_mark(False)],
    # Escape
    K("C-q"): escape_next_key,
    # C-x YYY
    K("C-x"): {
        # C-x h (select all)
        K("h"): [K("C-home"), K("C-a"), set_mark(True)],
        # C-x C-f (open)
        K("C-f"): K("C-o"),
        # C-x C-s (save)
        K("C-s"): K("C-s"),
        # C-x k (kill tab)
        K("k"): K("C-f4"),
        # C-x C-c (exit)
        K("C-c"): K("C-q"),
        # cancel
        K("C-g"): pass_through_key,
        # C-x u (undo)
        K("u"): [K("C-z"), set_mark(False)],
    }
}, "Emacs-like keys")


# [Conditional keybindings] Terminal
define_keymap(re.compile("Xfce4-terminal|konsole"), {
    # Keep Ctrl terminal controls and add Cmd as Ctrl+Shift
    K("Super-q"): K("Ctrl-Shift-q"),
    K("Super-w"): K("Ctrl-Shift-w"),
    K("Super-e"): K("Ctrl-Shift-e"),
    K("Super-r"): K("Ctrl-Shift-r"),
    K("Super-t"): K("Ctrl-Shift-t"),
    K("Super-z"): K("Ctrl-Shift-z"),
    K("Super-u"): K("Ctrl-Shift-u"),
    K("Super-i"): K("Ctrl-Shift-i"),
    K("Super-o"): K("Ctrl-Shift-o"),
    K("Super-p"): K("Ctrl-Shift-p"),
    K("Super-a"): K("Ctrl-Shift-a"),
    K("Super-s"): K("Ctrl-Shift-s"),
    K("Super-d"): K("Ctrl-Shift-d"),
    K("Super-f"): K("Ctrl-Shift-f"),
    K("Super-g"): K("Ctrl-Shift-g"),
    K("Super-h"): K("Ctrl-Shift-h"),
    K("Super-j"): K("Ctrl-Shift-j"),
    K("Super-k"): K("Ctrl-Shift-k"),
    K("Super-l"): K("Ctrl-Shift-l"),
    K("Super-y"): K("Ctrl-Shift-y"),
    K("Super-x"): K("Ctrl-Shift-x"),
    K("Super-c"): K("Ctrl-Shift-c"),
    K("Super-v"): K("Ctrl-Shift-v"),
    K("Super-b"): K("Ctrl-Shift-b"),
    K("Super-n"): K("Ctrl-Shift-n"),
    K("Super-m"): K("Ctrl-Shift-m"),
    K("Super-minus"): K("Ctrl-minus"),
    K("Super-key_0"): K("Ctrl-key_0"),
    K("Super-key_1"): K("Ctrl-key_1"),
    K("Super-key_2"): K("Ctrl-key_2"),
    K("Super-key_3"): K("Ctrl-key_3"),
    K("Super-key_4"): K("Ctrl-key_4"),
    K("Super-key_5"): K("Ctrl-key_5"),
    K("Super-key_6"): K("Ctrl-key_6"),
    K("Super-key_7"): K("Ctrl-key_7"),
    K("Super-key_8"): K("Ctrl-key_8"),
    K("Super-key_9"): K("Ctrl-key_9"),
    K("Super-Space"): K("Ctrl-Space")
}, "Terminal")
