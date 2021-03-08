;;; sunrise-buttons.el --- Clickable shortcut buttons for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 11 Jun 2008
;; Version: 1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, sunrise commander, shortcut buttons
;; URL: https://github.com/sunrise-commander/sunrise-commander

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more de- tails.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here is a small extension that may be of help to new users who want to get
;; acquainted fast with the most frequent functions found in the Sunrise
;; Commander and their keybindings. Once installed, it displays a panel with
;; mouse clickable buttons that show some of the most useful actions performed
;; by Sunrise and their respective bindings in the bottom window (a.k.a.
;; viewer window here) every time the main panels are invoked. You can execute
;; any of these functions by clicking the appropriate button, but the
;; extension was conceived more as a simple cheat sheet (a very, very limited
;; one, as you can easily learn by pressing the last button, labeled
;; "More...") than as a real interface to Sunrise and Dired functions.
;; Eventually, if you like this kind of interaction with the program you can
;; add your own commands to the list and let this extension manage the
;; creation and layout of the buttons for you.

;; This extension was developed on GNU Emacs 23 on Linux, and tested on GNU
;; Emacs 22 and 23 for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-buttons) to your .emacs file, preferably right
;; after (require 'sunrise).

;; 3) Evaluate the new expression, or reload your .emacs file, or restart
;; Emacs.

;; That's it - the next time you activate Sunrise you'll see a nice button
;; panel in the viewer window.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'cus-edit)

(require 'sunrise)

(defvar sunrise-buttons-buffer-name "*Sunrise Buttons*"
  "Name of the Sunrise buttons buffer.")

(defvar sunrise-buttons-command-adapter nil
  "Function to use to execute button commands, or nil to do the default.")

(defvar sunrise-buttons-list
  '(("GotoDir([F2,]j,/)" 'sunrise-goto-dir
     "Go to any directory in active pane")
    ("View([F3,]v,o)"    'sunrise-quick-view
     "View selected file or directory in this window")
    ("Open([F4,]Enter)"  'sunrise-advertised-find-file
     "Visit selected file or directory")
    ("Copy([F5,]C)"      'sunrise-do-copy
     "Copy selected files to passive pane")
    ("Rename([F6,]R)"    'sunrise-do-rename
     "Move selected files to passive pane")
    ("Clone(K)"          'sunrise-do-clone
     "Clone selected files to passive pane")
    ("NewDir([F7,]+)"    'dired-create-directory
     "Create new directory in active pane")
    ("Delete([F8,]D)"    'sunrise-do-delete
     "Delete selected files from active pane")
    nil
    ("DirUp([C-PgUp,]J)" 'sunrise-dired-prev-subdir
     "Go to parent directory in active pane")
    ("DirBack(M-y)"      'sunrise-history-prev
     "Go to previous directory in history")
    ("DirFrwd(M-u)"      'sunrise-history-next
     "Go to next directory in history")
    ("HardLink(H)"       'sunrise-do-hardlink
     "Make hard link of selected file in passive pane")
    ("SymLink(S)"        'sunrise-do-symlink
     "Make absolute symlink of selected entry in passive pane")
    ("RelSymLink(Y)"     'sunrise-do-relsymlink
     "Make relative symlink of selected entry in passive pane")
    ("Hidden(C-o)"       'sunrise-omit-mode
     "Hide/Show hidden files in active pane")
    ("Attrs(C-Bksp)"     'sunrise-toggle-attributes
     "Hide/Show file attributes in active pane")
    nil
    ("Other(Tab)"        'sunrise-change-window
     "Switch to passive pane")
    ("ClonePane(M-o)"    'sunrise-synchronize-panes
     "Make both panes contain the same directory")
    ("Swap(M-t)"         'sunrise-transpose-panes
     "Transpose panes")
    ("Refresh(g)"        'revert-buffer
     "Rescan directory in active pane")
    ("Align(C-cC-s)"     'sunrise-split-toggle
     "Change panes alignment (vertical/horizontal/top)")
    ("Sort(s)"           'sunrise-interactive-sort
     "Sort interactively entries in active pane")
    ("Mark([Ins,]m)"     'dired-mark
     "Mark selected entry in active pane")
    ("Unmark(Bksp)"      'dired-unmark-backward
     "Unmark last selected entry inactive pane")
    nil
    ("History(C-cC-d)"   'sunrise-recent-directories
     "Display listing of recently visited directories")
    ("Recent(C-cC-r)"    'sunrise-recent-files
     "Display listing of recently visited files")
    ("Restore(C-cC-c)"   'sunrise-buttons-restore-mode
     "Dismiss VIRTUAL or WDired mode")
    ("Find(C-cC-f)"      'sunrise-find
     "Find files and directories interactively")
    ("FName(C-cC-n)"     'sunrise-find-name
     "Find files and directories by name pattern")
    ("FGrep(C-cC-g)"     'sunrise-find-grep
     "Find files containing some expression")
    ("Follow(;)"         'sunrise-follow-file
     "Follow file (go to same directory as file)")
    ("Locate(C-cC-l)"    'sunrise-locate
     "Find files and directories using locate database")
    nil
    ("Search(A)"         'sunrise-do-search
     "Search for string/regexp in all marked entries")
    ("Compare(C-M-=)"    'sunrise-compare-dirs
     "Compare directories in panes")
    ("Diff(=)"           'sunrise-diff
     "Compare selected entries using diff")
    ("Ediff(C-=)"        'sunrise-ediff
     "Compare selected entries using ediff")
    ("Store(C-c>)"       'sunrise-checkpoint-save
     "Remember current position of panes as name")
    ("Recall(C-c.)"      'sunrise-checkpoint-restore
     "Set panes to a previously remembered position")
    ("Home(M-a)"         'sunrise-beginning-of-buffer
     "Go to first entry in active pane")
    ("End(M-e)"          'sunrise-end-of-buffer
     "Go to last entry in active pane")
    nil
    ("FindReplace(Q)"    'sunrise-do-query-replace-regexp
     "Find and replace in all selected entries")
    ("Fuzzy(C-c/)"       'sunrise-fuzzy-narrow
     "Narrow pane contents with fuzzy matching")
    ("CmdLine(C-ct)"     'sunrise-term
     "Open Command line in this window")
    ("WDired(C-xC-q)"    'sunrise-buttons-editable-pane
     "Edit active pane using wdired")
    ("SyncNav(C-cC-z)"   'sunrise-sync
     "Toggle on/off synchronized navigation mode")
    ("LongLines(M-l)"    'sunrise-toggle-truncate-lines
     "Truncate/Restore long lines in active pane")
    ("More...(h)"        'sunrise-describe-mode
     "More commands and keybindings")
    ("Quit([F10,]q)"     'sunrise-quit
     "Dismiss Sunrise Commander"))
  "Sunrise button definitions.")

(defun sunrise-buttons-click ()
  "Handle all click events that take place in the buttons buffer."
  (interactive)
  (unwind-protect (call-interactively 'widget-button-click)
    (sunrise-select-window sunrise-selected-window)))

(defun sunrise-buttons-mouse ()
  "Handle all mouse events other than clicks in the button buffers."
  (interactive)
  (sunrise-select-window sunrise-selected-window))

(defun sunrise-buttons-sunrise-quit-function ()
  "Sunrise Buttons handler for `sunrise-quit-hook'."
  (let ((buttons (get-buffer sunrise-buttons-buffer-name)))
    (if buttons (bury-buffer buttons))))

(defun sunrise-buttons-kill-buffer-function ()
  "Sunrise Buttons handler for `kill-buffer-hook'."
  (when (and (sunrise-running-p)
             (eq (current-buffer) other-window-scroll-buffer))
    (sunrise-buttons-display)))

(define-derived-mode sunrise-buttons-mode Custom-mode "Sunrise Buttons"
  "Sunrise Commander Buttons panel mode."
  :group 'sunrise
  (set-keymap-parent sunrise-buttons-mode-map custom-mode-map)
  (make-local-variable 'double-click-time)
  (setq double-click-time nil)
  (make-local-variable 'double-click-fuzz)
  (setq double-click-fuzz 0)
  (mapc (lambda (x)
          (define-key sunrise-buttons-mode-map x 'sunrise-buttons-click))
        '([down-mouse-1] [down-mouse-2] [down-mouse-3]))
  (mapc (lambda (x)
          (define-key sunrise-buttons-mode-map x 'sunrise-buttons-mouse))
        '([(control tab)] "\C-c\t"
          [mouse-1]             [mouse-2]             [mouse-3]
          [drag-mouse-1]        [drag-mouse-2]        [drag-mouse-3]
          [double-mouse-1]      [double-mouse-2]      [double-mouse-3]
          [triple-mouse-1]      [triple-mouse-2]      [triple-mouse-3]
          [double-drag-mouse-1] [double-drag-mouse-2] [double-drag-mouse-3]
          [triple-drag-mouse-1] [triple-drag-mouse-2] [triple-drag-mouse-3]
          [double-down-mouse-1] [double-down-mouse-2] [double-down-mouse-3]
          [triple-down-mouse-1] [triple-down-mouse-2] [triple-down-mouse-3])))

(add-hook 'sunrise-start-hook 'sunrise-buttons-display)
(add-hook 'sunrise-quit-hook 'sunrise-buttons-sunrise-quit-function)
(add-hook 'kill-buffer-hook 'sunrise-buttons-kill-buffer-function)

(defun sunrise-buttons-display ()
  "Display the buttons buffer in the viewer window.
If no buttons buffer exists yet, creates one."
  (unless (and (boundp 'sunrise-popviewer-mode)
               (symbol-value 'sunrise-popviewer-mode))
    (apply 'require '(cus-edit))
    (sunrise-select-viewer-window t)
    (cond ((buffer-live-p other-window-scroll-buffer)
           ;; Don't nuke quick views!
           (switch-to-buffer other-window-scroll-buffer))
          ((get-buffer "*terminal*")
           ;; Prefer terminals.
           (switch-to-buffer "*terminal*"))
          (t
           (switch-to-buffer sunrise-buttons-buffer-name)
           (setq truncate-lines t)
           (setq line-spacing 5)
           (setq cursor-in-non-selected-windows nil)
           (if (not (eq major-mode 'sunrise-buttons-mode))
               (let ((line-spacing 2)
                     (cursor-in-non-selected-windows nil))
                 (sunrise-buttons-render)))))
    (sunrise-select-window sunrise-selected-window)))

(defun sunrise-buttons-render ()
  "Populate current buffer with all widgets in `sunrise-buttons-list'."
  (sunrise-buttons-mode)
  (let ((mc-keys-on (sunrise-buttons-mc-keys-p))
        (maxlen (sunrise-buttons-maxtaglen)))
    (mapc (lambda (x) (sunrise-buttons-build x mc-keys-on maxlen))
          sunrise-buttons-list))
  (sunrise-buttons-eol)
  (goto-char (point-min)))

(defun sunrise-buttons-build (spec mc-keys-on maxlen)
  "Build and render a new widget in the Sunrise Buttons buffer.

SPEC is an element of `sunrise-buttons-list' (list containing
tag, action and hint).

MC-KEYS-ON is a flag that indicates whether Midnight Commander
style keybindings have been activated in Sunrise.

MAXLEN is the length of the longest tag in the list."
  (if (or (null spec)
          (> (+ (current-column) maxlen) (- (window-width) (/ maxlen 2))))
      (sunrise-buttons-eol)
    (let ((tag (cl-first spec))
          (action (cl-second spec))
          (hint (cl-third spec)))
      (if mc-keys-on
          (setq tag (replace-regexp-in-string "\\[\\|\\]" "" tag))
        (setq tag (replace-regexp-in-string "\\[.*\\]" "" tag)))
      (setq tag (sunrise-buttons-normalize-tag tag maxlen ? ))
      (widget-create 'push-button :tag tag
                     :action (sunrise-buttons-action action)
                     :help-echo hint)
      (insert-char ?  1)
      (put-text-property
       (1- (point)) (point) 'display (list 'space :width 0.15)))))

(defun sunrise-buttons-eol ()
  "Terminate the current row of buttons while building the buttons buffer.
Centers it if necessary."
  (let* ((gap (- (window-width) (current-column) 2))
         (margin (/ gap 2)))
    (if (> margin 0)
        (save-excursion (beginning-of-line) (insert-char ?  margin)))
    (unless (eq ?\n (char-before)) (insert "\n"))))

(defun sunrise-buttons-mc-keys-p ()
  "Determine whether mc-style keybindings have been activated in Sunrise."
  (eq 'sunrise-goto-dir (cdr (assq 'f2 sunrise-mode-map))))

(defun sunrise-buttons-maxtaglen ()
  "Calculate the length of the longest tag in `sunrise-buttons-list'."
  (let* ((regexp (if (sunrise-buttons-mc-keys-p) "\\[\\|\\]" "\\[.*\\]"))
         (lenfun (lambda (x)
                   (if x
                       (length (replace-regexp-in-string regexp "" (car x)))
                     0))))
    (apply 'max (mapcar lenfun sunrise-buttons-list))))

(defun sunrise-buttons-normalize-tag (tag total-length fill-char)
  "Lengthen the given TAG to TOTAL-LENGTH.

Prepends and appends FILL-CHAR until the text appears
approximately centered on its button."
  (let* ((fill-length (- total-length (length tag)))
         (before (/ fill-length 2))
         (after (- fill-length before)))
    (concat (make-string before fill-char)
            tag
            (make-string after fill-char))))

(defun sunrise-buttons-action (action)
  "Return button command to perform ACTION inside the currently active pane."
  `(lambda (&rest ignore)
     (interactive)
     (sunrise-select-window sunrise-selected-window)
     (if sunrise-buttons-command-adapter
         (run-with-timer 0.01 nil
                         (funcall sunrise-buttons-command-adapter ,action))
       (run-with-timer 0.01 nil (sunrise-buttons-do ,action)))))

(defun sunrise-buttons-do (action)
  "Execute ACTION interactively as response to the click of a button."
  (hl-line-mode -1)
  (call-interactively action)
  (when (memq major-mode
              '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
    (hl-line-mode 1)
    (sunrise-graphical-highlight))
  t)

(defun sunrise-buttons-editable-pane ()
  "Call `sunrise-editable-pane' and display an informative message.
Used inside the Sunrise Buttons buffer."
  (interactive)
  (sunrise-editable-pane)
  (message
   "Push [Restore] button or C-c C-c when done, ESC C-c C-c to cancel"))

(defun sunrise-buttons-restore-mode ()
  "Implement the [Restore] action in the Sunrise buttons panel."
  (interactive)
  (cond ((eq major-mode 'sunrise-virtual-mode) (sunrise-virtual-dismiss))
        ((eq major-mode 'sunrise-tree-mode) (eval '(sunrise-tree-dismiss)))
        ((string= mode-name "Editable Dired") (eval '(wdired-finish-edit)))
        (t (message "Already in regular mode"))))

(provide 'sunrise-buttons)

;;; sunrise-buttons.el ends here
