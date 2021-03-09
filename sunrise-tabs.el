;;; sunrise-tabs.el --- Tabs for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 24 Oct 2009
;; Version: 1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, sunrise commander, tabs
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

;; This extension brings tab-based navigation to the Sunrise Commander. It
;; adds to the list of optional mechanisms already available in Sunrise for
;; moving around the file system (like regular bookmarks, checkpoints, history
;; rings, materialized virtual buffers, navigable paths and file-following)
;; another way to maintain a list of selected locations one wants to return
;; later on, or to compose "breadcrumb trails" for complex repetitive
;; operations.

;; The main difference between tabs and other mechanisms is that once a buffer
;; has been assigned to a tab, it will not be killed automatically by Sunrise,
;; so it's possible to keep it around as long as necessary with all its marks
;; and state untouched. Tabs can be persisted across sessions using the
;; DeskTop feature.

;; Creating, using and destroying tabs are fast and easy operations, either
;; with mouse or keyboard:

;; * Press C-j (or select Sunrise > Tabs > Add Tab in the menu) to create a
;; new tab or to rename an already existing tab.

;; * Press C-k (or right-click the tab) to kill an existing tab. Combine with
;; M- (M-C-k) to kill the tab on the passive pane. Prefix with a digit to kill
;; tabs by relative order (e.g. 2 C-k kills the second tab in the current
;; pane, while 4 M-C-k kills the fourth tab in the passive pane).

;; * Press C-n and C-p to move from tab to tab ("Next", "Previous"), or simply
;; left-click on the tab to focus its assigned buffer. These two keybindings
;; can be prefixed with an integer to move faster.

;; * The last four bindings can be combined with Meta (i.e. M-C-j, M-C-k,
;; M-C-n and M-C-p) to perform the equivalent operation on the passive pane or
;; (when in synchronized navigation mode) on both panes simultaneously.

;; * Press * C-k to kill in one go all the tabs in the current pane.
;; Similarly, press * M-C-k to wipe all the tabs off the passive pane or (when
;; synchronized mode is active) on both panes simultaneously.

;; * Killing the current buffer with C-x k automatically switches to the one
;; assigned to the first available tab (if any).

;; The extension is provided as a minor mode, so you can enable / disable it
;; totally by using the command `sunrise-tabs-mode'.

;; It does *not* pretend to be a generic solution for tabs in Emacs. If you
;; need one, have a look at TabBar mode
;; (http://www.emacswiki.org/emacs/TabBarMode) by David Ponce. I wrote this
;; just because it turned out to be easier to write this than to customize
;; tabbar to behave exactly like I wanted inside the Sunrise panes. It's meant
;; to be simple and to work nicely with Sunrise with just a few tabs (up to
;; 10-15 per pane, maybe).

;; It was written on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 23) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-tabs) expression to your .emacs file somewhere
;; after the (require 'sunrise) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart
;; Emacs.

;; 4) Enjoy ;-)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'desktop))

(require 'sunrise)

(defcustom sunrise-tabs-follow-panes t
  "Whether tabs should be swapped too when transposing the Sunrise panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-tabs-max-tabsize 10
  "Maximum width of a Sunrise Commander tab."
  :group 'sunrise
  :type 'integer)

(defcustom sunrise-tabs-truncation-style 'right
  "On which side should we truncate the tag of a new tab.

Tags longer than `sunrise-tabs-max-tabsize' are truncated."
  :group 'sunrise
  :type '(choice
          (const :tag "Truncate from the right" right)
          (const :tag "Truncate from the left"  left)))

(defcustom sunrise-tabs-tag-provider
  (defun sunrise-tabs-tag-provider-default (buffer-name)
    "Default provider of tags based on the buffer name of a pane."
    buffer-name)
  "Function to use to determine the tag to use when creating a new tab.
It should take one argument which is to be interpreted as the
name of the buffer that contains the pane to be assigned to the
tab."
  :group 'sunrise
  :type 'function)

(defface sunrise-tabs-active-face
  '((((type tty) (class color) (min-colors 88))
     :background "white")
    (((type tty) (class color) (min-colors 8))
     :background "green" :foreground "yellow" :bold t)
    (((type tty) (class mono)) :inverse-video t)
    (t
     :inherit variable-pitch :bold t :background "white" :height 0.9))
  "Face of the currently selected tab in any of the Sunrise panes."
  :group 'sunrise)

(defface sunrise-tabs-inactive-face
  '((((type tty) (class color) (min-colors 88))
     :background "color-84" :foreground "white")
    (((type tty) (class color) (min-colors 8))
     :background "white" :foreground "cyan")
    (t
     :inherit variable-pitch :background "gray95" :height 0.9))
  "Face of all non-selected tabs in both Sunrise panes."
  :group 'sunrise)

(defface sunrise-tabs-separator-face
  '((t (:height 0.3)))
  "Face of the string used to separate the Sunrise tabs from one another."
  :group 'sunrise)

(defconst sunrise-tabs-sep
  #(" " 0 1 (face sunrise-tabs-separator-face))
  "Sunrise Tabs separator character.")

(defconst sunrise-tabs-ligature
  #(" ║" 0 1 (face sunrise-tabs-separator-face))
  "Sunrise Tabs line separator string.")

(defconst sunrise-tabs-max-cache-length
  30
  "Max number of tab labels cached for reuse.")

(defvar sunrise-tabs
  '((left) (right)))

(defvar sunrise-tabs-labels-cache
  '((left) (right)))

(defvar sunrise-tabs-line-cache
  '((left) (right)))

(defvar sunrise-tabs-mode
  nil)

(defvar sunrise-tabs-on
  nil)

;;; ==========================================================================
;;; Core functions:

(defun sunrise-tabs-add ()
  "Assign the current buffer to exactly one tab in the active pane.
If a tab for the current buffer already exists, invoke `sunrise-tabs-rename'."
  (interactive)
  (let ((tab-name (buffer-name))
        (tab-set (assq sunrise-selected-window sunrise-tabs)))
    (if (member tab-name (cdr tab-set))
        (call-interactively 'sunrise-tabs-rename)
      (setcdr tab-set (cons tab-name (cdr tab-set)))))
  (sunrise-tabs-refresh))

(defun sunrise-tabs-remove (&optional tab-buffer side)
  "Remove the tab to which TAB-BUFFER is assigned in the active pane.

If TAB-BUFFER is nil, removes the tab to which the current buffer
is assigned, if any.

SIDE is 'left or 'right."
  (interactive "P")
  (let* ((side (or side sunrise-selected-window))
         (tab-name (if (integerp tab-buffer)
                       (nth tab-buffer (assoc side sunrise-tabs))
                     (buffer-name tab-buffer)))
         (tab-buffer (and tab-name (get-buffer tab-name)))
         (tab-set (assq side sunrise-tabs)))
    (setcdr tab-set (delete tab-name (cdr tab-set)))
    (unless (or (null tab-buffer)
                (eq tab-buffer (current-buffer))
                (eq tab-buffer (sunrise-other 'buffer)))
      (kill-buffer (get-buffer tab-name))))
  (sunrise-tabs-refresh))

(defun sunrise-tabs-clean ()
  "Remove all tabs from the current pane."
  (interactive)
  (while (nth 1 (assoc sunrise-selected-window sunrise-tabs))
    (sunrise-tabs-remove 1)))

(defun sunrise-tabs-kill (&optional name side)
  "Remove the tab named NAME from the active pane and kill its buffer.

The buffer is not killed when currently visible or assigned to
another tab.

SIDE is 'left or 'right."
  (interactive)
  (let ((to-kill (or (and name (get-buffer name)) (current-buffer)))
        (side (or side sunrise-selected-window)))
    (sunrise-tabs-remove to-kill side)
    (if (and (not (memq to-kill
                        (list sunrise-left-buffer sunrise-right-buffer)))
             (not (member to-kill
                          (apply 'append (mapcar 'cdr sunrise-tabs)))))
        (kill-buffer to-kill))
    (sunrise-tabs-refresh)))

(defun sunrise-tabs-next (&optional n)
  "Move focus to the next tab (left to right) in the active pane.
With a prefix argument N, moves focus to the tab N places ahead,
or to the last one if there are fewer tabs than requested."
  (interactive "p")
  (sunrise-tabs-step n))

(defun sunrise-tabs-prev (&optional n)
  "Move focus to the previous tab (right to left) in the active pane.
With a prefix argument N, moves focus to the tab N places behind,
or to the first one if there are fewer tabs than requested."
  (interactive "p")
  (sunrise-tabs-step n t))

(defun sunrise-tabs-step (count &optional back)
  "Move focus from the current tab to the one COUNT places ahead or behind.
The direction depends on the value of BACK."
  (let* ((stack (cdr (assq sunrise-selected-window sunrise-tabs)))
         (stack (if back (reverse stack) stack))
         (target (member (buffer-name) stack)))
    (unless (null stack)
      (if (or (null count) (zerop count))
          (setq count 1))
      (if (< 1 (length target))
          (sunrise-tabs-switch-to-buffer
           (or (nth count target) (car (last target))))
        (sunrise-tabs-switch-to-buffer (car stack))))))

(defun sunrise-tabs-switch-to-buffer (to-buffer)
  "Change context of the active Sunrise pane when switching buffers.

TO-BUFFER is the target buffer."
  (let ((from-buffer (current-buffer))
        (sunrise-current-path-faces
         (with-current-buffer to-buffer sunrise-current-path-faces)))
    (unless (eq from-buffer to-buffer)
      (sunrise-save-aspect (switch-to-buffer to-buffer))
      (setq sunrise-this-directory default-directory)
      (set (sunrise-symbol sunrise-selected-window 'buffer)
           (current-buffer))
      (set (sunrise-symbol sunrise-selected-window 'directory)
           default-directory)
      (unless (eq from-buffer (sunrise-other 'buffer))
        (with-current-buffer from-buffer
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))))
      (condition-case nil
          (revert-buffer t t)
        (error (ignore)))
      (sunrise-history-push default-directory))
    (sunrise-tabs-refresh)))

(defun sunrise-tabs-focus (name side)
  "Give focus to the tab with name NAME in SIDE pane."
  (unless (eq side sunrise-selected-window)
    (sunrise-change-window))
  (sunrise-tabs-switch-to-buffer name))

(defun sunrise-tabs-kill-and-go ()
  "Kill the current Sunrise buffer and move to the next one.
This kills the buffer, removes its assigned tab (if any) and
moves to the next buffer tabbed in the active pane, unless there
are no more tabbed buffers to fall back to, in which case just
removes the tab."
  (interactive)
  (let ((to-kill (current-buffer))
        (stack (cdr (assq sunrise-selected-window sunrise-tabs))))
    (if (null stack)
        (sunrise-kill-pane-buffer)
      (sunrise-tabs-kill)
      (setq stack (cdr stack))
      (sunrise-tabs-next)
      (unless (or (null stack)
                  (eq to-kill (current-buffer))
                  (eq to-kill (sunrise-other 'buffer)))
        (kill-buffer to-kill)))))

(defun sunrise-tabs-rename (&optional new-name)
  "Rename the current Sunrise tab to NEW-NAME."
  (interactive "sRename current tab to: ")
  (let* ((key (buffer-name))
         (cache (assq sunrise-selected-window sunrise-tabs-labels-cache))
         (label (cadr cache)))
    (if label
        (sunrise-tabs-redefine-label key new-name))))

(defun sunrise-tabs-transpose ()
  "Swap tabsets from one pane to the other."
  (interactive)
  (cl-labels ((flip (side) (setcar side (cdr (assq (car side)
                                                   sunrise-side-lookup)))))
    (dolist (registry (list sunrise-tabs sunrise-tabs-labels-cache))
      (mapc #'flip registry)))
  (sunrise-in-other (sunrise-tabs-refresh))
  (sunrise-tabs-refresh))

(defadvice sunrise-transpose-panes
    (after sunrise-tabs-advice-transpose-panes ())
  "Synchronize the tabs with the panes if so required.

See the variable `sunrise-tabs-follow-panes'. Activated in the
function `sunrise-tabs-engage'."
  (if sunrise-tabs-follow-panes (sunrise-tabs-transpose)))

;;; ==========================================================================
;;; Graphical interface:

(defun sunrise-tabs-focus-cmd (name side)
  "Return a function to give focus to the named NAME in the SIDE pane."
  (let ((selector (if (eq side (caar sunrise-tabs)) #'caar #'caadr)))
    `(lambda ()
       (interactive)
       (sunrise-tabs-focus ,name (funcall ',selector sunrise-tabs)))))

(defun sunrise-tabs-rename-cmd (name)
  "Return a function to rename the tab named NAME in both panes."
  `(lambda (&optional new-name)
     (interactive "sRename tab to: ")
     (sunrise-tabs-redefine-label ,name new-name)))

(defun sunrise-tabs-kill-cmd (name side)
  "Return a function to delete the tab named NAME in the SIDE pane."
  (let ((selector (if (eq side (caar sunrise-tabs)) #'caar #'caadr)))
    `(lambda ()
       (interactive)
       (if (eq sunrise-selected-window (funcall ',selector sunrise-tabs))
           (sunrise-tabs-kill ,name)
         (sunrise-in-other
          (sunrise-tabs-kill ,name))))))

(defsubst sunrise-tabs-propertize-tag (string face keymap)
  "Propertize STRING with FACE and KEYMAP so it can be used as a tab tag."
  (propertize string
              'face face
              'help-echo
              "mouse-1: select tab\n\mouse-2: rename tab\n\mouse-3: kill tab"
              'local-map keymap))

(defun sunrise-tabs-make-tag (name as-active &optional tag)
  "Return a propertized string for decorating a tab named NAME.
AS-ACTIVE determines whether to propertize it as an active or a
passive tab (nil = passive, t = active). The optional argument
TAG allows to provide a pretty name to label the tab."
  (let ((tag (sunrise-tabs-truncate (or tag name)))
        (side sunrise-selected-window)
        (keymap (make-sparse-keymap)))
    (setq tag (concat sunrise-tabs-sep tag sunrise-tabs-sep))
    (define-key keymap [header-line mouse-1]
      (sunrise-tabs-focus-cmd name side))
    (define-key keymap [header-line mouse-2]
      (sunrise-tabs-rename-cmd name))
    (define-key keymap [header-line mouse-3]
      (sunrise-tabs-kill-cmd name side))
    (if as-active
        (sunrise-tabs-propertize-tag tag 'sunrise-tabs-active-face keymap)
      (sunrise-tabs-propertize-tag tag 'sunrise-tabs-inactive-face keymap))))

(defun sunrise-tabs-truncate (tag)
  "Truncate and add an ellipsis mark to the given TAG if necessary."
  (if (>= sunrise-tabs-max-tabsize (length tag))
      tag
    (cl-case sunrise-tabs-truncation-style
      (right (concat (substring tag 0 sunrise-tabs-max-tabsize) "…"))
      (left (concat "…" (substring tag (* -1 sunrise-tabs-max-tabsize))))
      (t (ignore)))))

(defun sunrise-tabs-make-label (name &optional alias)
  "Return a new label for decorating a tab named NAME.
A label is a dotted pair of tags, for active and passive state.
The new label is put in cache for later reuse. The optional
argument ALIAS allows to provide a pretty name to label the tab."
  (let* ((alias (or alias (apply sunrise-tabs-tag-provider (list name))))
         (label (cons (sunrise-tabs-make-tag name t alias)
                      (sunrise-tabs-make-tag name nil alias)))
         (entry (list (cons name label)))
         (cache (assq sunrise-selected-window sunrise-tabs-labels-cache)))
    (setcdr cache (append (cdr cache) entry))
    label))

(defun sunrise-tabs-trim-label (label)
  "Remove all properties and trailing whitespace from LABEL."
  (replace-regexp-in-string "^\\s-+\\|\\s-+$"
                            ""
                            (substring-no-properties label)))

(defun sunrise-tabs-redefine-label (name alias)
  "Change the name displayed on the tab with assigned buffer NAME to ALIAS.
By default, a tab is named after its assigned buffer. This function allows to
give tabs names that are more readable or simply easier to remember."
  (let* ((alias (sunrise-tabs-trim-label (or alias ""))) (cache))
    (when (string= "" alias)
      (setq alias (buffer-name)))
    (setq cache (assq sunrise-selected-window sunrise-tabs-labels-cache))
    (setcdr cache (delq nil
                        (mapcar (lambda(x)
                                  (and (not (equal (car x) name)) x))
                                (cdr cache))))
    (sunrise-tabs-make-label name alias)
    (sunrise-tabs-refresh)))

(defun sunrise-tabs-get-tag (name is-active)
  "Retrieve the cached tag for the tab named NAME in state IS-ACTIVE.
nil = inactive, t = active. Creates new labels when needed."
  (let* ((cache (assq sunrise-selected-window sunrise-tabs-labels-cache))
         (label (cdr (assoc name (cdr cache)))))
    (if (null label)
        (setq label (sunrise-tabs-make-label name)))
    (if (< sunrise-tabs-max-cache-length (length (cdr cache)))
        (setcdr cache (cddr cache)))
    (if is-active (car label) (cdr label))))

(defun sunrise-tabs-make-line ()
  "Assemble a new tab line from cached tags and put it in the line cache."
  (if (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
      (let ((tab-set (cdr (assq sunrise-selected-window sunrise-tabs)))
            (tab-line (if (or (cdar sunrise-tabs)
                              (cdr (cadr sunrise-tabs))) "" nil))
            (current-name (buffer-name)))
        (mapc (lambda (x)
                (let ((is-current (equal current-name x)))
                  (setq tab-line
                        (concat tab-line sunrise-tabs-sep
                                (sunrise-tabs-get-tag x is-current)))))
              tab-set)
        (setcdr (assq sunrise-selected-window sunrise-tabs-line-cache)
                tab-line)
        tab-line)
    nil))

(defsubst sunrise-tabs-empty-p (line)
  ""
  (or (null line) (string= "" line)))

(defsubst sunrise-tabs-empty-mask (line)
  ""
  (or (and (null line) "") line))

(defsubst sunrise-tabs-empty-null (line)
  ""
  (if (sunrise-tabs-empty-p line) nil line))

(defun sunrise-tabs-nonempty-p (line-list)
  "Return non-nil if LINE-LIST contains at least one non-nil element."
  (or (not (sunrise-tabs-empty-p (car line-list)))
      (and (cdr line-list) (sunrise-tabs-nonempty-p (cdr line-list)))))

(defun sunrise-tabs-xor (list1 list2)
  "Replacement for function `set-exclusive-or'.
Used to avoid dependency on cl-seq.el."
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) nil)
        (t
         (let (result)
           (mapc (lambda (element)
                   (if (member element result)
                       (setq result (delete element result))
                     (setq result (cons element result))))
                 (append list1 list2))
           result))))

(defun sunrise-tabs-refresh ()
  "Update `header-line-format' in both panes.
Uses the line cache for the passive one, and assembles a new tab
line for the active one. In the (corner) case when both panes
contain the same buffer, glues together the tab lines with a
``double bar'' separator."
  (setq sunrise-tabs-mode sunrise-tabs-on)
  (sunrise-tabs-make-line)
  (let ((line-list (mapcar 'cdr sunrise-tabs-line-cache))
        (same-buffer (eq sunrise-left-buffer sunrise-right-buffer)))
    (if same-buffer
        (setq header-line-format
              (and (sunrise-tabs-nonempty-p line-list)
                   (mapconcat 'concat line-list sunrise-tabs-ligature)))
      (let ((other-buffer (sunrise-other 'buffer)))
        (if (eq 'right sunrise-selected-window)
            (setq line-list (nreverse line-list)))
        (if (apply 'sunrise-tabs-xor (mapcar 'sunrise-tabs-empty-p line-list))
            (setq line-list (mapcar 'sunrise-tabs-empty-mask line-list))
          (setq line-list (mapcar 'sunrise-tabs-empty-null line-list)))

        (setq header-line-format (car line-list))

        (when (buffer-live-p other-buffer)
          (with-current-buffer other-buffer
            (setq header-line-format (cadr line-list)))))))
  (force-window-update))

;;; ==========================================================================
;;; Private interface:

(defun sunrise-tabs-bury-all ()
  "Bury all currently tabbed buffers."
  (let ((all-buffers (apply 'append (mapcar 'cdr sunrise-tabs))))
    (if all-buffers
        (mapc 'bury-buffer all-buffers))))

(defun sunrise-tabs-protect-buffer ()
  "Protect the current buffer from being automatically disposed
by Sunrise when moving to another directory (called from
`kill-buffer-query-functions' hook.)"
  (let ((tab-name (buffer-name)))
    (not (or (member tab-name (car sunrise-tabs))
             (member tab-name (cadr sunrise-tabs))))))

(defun sunrise-tabs-engage ()
  "Enable the Sunrise Tabs extension."
  (setq sunrise-tabs-on t)
  (add-hook 'sunrise-refresh-hook 'sunrise-tabs-refresh)
  (add-hook 'sunrise-quit-hook 'sunrise-tabs-bury-all)
  (add-hook 'kill-buffer-query-functions 'sunrise-tabs-protect-buffer)
  (ad-activate 'sunrise-transpose-panes)
  (ad-activate 'sunrise-editable-pane)
  (sunrise-tabs-refresh))

(defun sunrise-tabs-disengage ()
  "Disable the Sunrise Tabs extension."
  (setq sunrise-tabs-on nil)
  (remove-hook 'sunrise-refresh-hook 'sunrise-tabs-refresh)
  (remove-hook 'sunrise-quit-hook 'sunrise-tabs-bury-all)
  (remove-hook 'kill-buffer-query-functions 'sunrise-tabs-protect-buffer)
  (ad-deactivate 'sunrise-transpose-panes)
  (ad-deactivate 'sunrise-editable-pane)
  (setq header-line-format (default-value 'header-line-format))
  (sunrise-in-other
   (setq header-line-format (default-value 'header-line-format))))

;;; ==========================================================================
;;; User interface:

(defvar sunrise-tabs-mode-map (make-sparse-keymap))
(define-key sunrise-tabs-mode-map [(control ?j)] 'sunrise-tabs-add)
(define-key sunrise-tabs-mode-map [(control ?k)] 'sunrise-tabs-remove)
(define-key sunrise-tabs-mode-map "*\C-k" 'sunrise-tabs-clean)
(define-key sunrise-tabs-mode-map [(control ?p)] 'sunrise-tabs-prev)
(define-key sunrise-tabs-mode-map [(control ?n)] 'sunrise-tabs-next)
(define-key sunrise-tabs-mode-map [(meta tab)] 'sunrise-tabs-next)

(define-key sunrise-tabs-mode-map [(control meta ?j)]
  (lambda ()
    (interactive)
    (sunrise-in-other (sunrise-tabs-add))))

(define-key sunrise-tabs-mode-map [(control meta ?k)]
  (lambda ()
    (interactive)
    (sunrise-in-other (call-interactively 'sunrise-tabs-remove))))

(define-key sunrise-tabs-mode-map [(control meta ?p)]
  (lambda ()
    (interactive)
    (sunrise-in-other (sunrise-tabs-prev))))

(define-key sunrise-tabs-mode-map [(control meta ?n)]
  (lambda ()
    (interactive)
    (sunrise-in-other (sunrise-tabs-next))))

(define-key sunrise-tabs-mode-map [(control meta tab)]
  (lambda ()
    (interactive)
    (sunrise-in-other (sunrise-tabs-next))))

(define-key sunrise-tabs-mode-map "*\C-\M-k"
  (lambda ()
    (interactive)
    (sunrise-in-other (sunrise-tabs-clean))))

(define-key sunrise-tabs-mode-map "\C-xk" 'sunrise-tabs-kill-and-go)
(define-key sunrise-tabs-mode-map "\M-T"  'sunrise-tabs-transpose)

(define-minor-mode sunrise-tabs-mode
  "Tabs support for the Sunrise Commander file manager.
This minor mode provides the following keybindings:

       C-j ........... Create new tab (or rename existing tab) in active pane.
       C-k ........... Kill the tab of the current buffer in the active pane.
       C-n ........... Move to the next tab in the active pane.
       C-p ........... Move to the previous tab in the active pane.

       C-M-j ......... Assign the current buffer to a tab in the passive pane.
       C-M-k ......... Kill the tab of the current buffer in the passive pane.
       C-M-n ......... Move to the next tab in the passive pane.
       C-M-p ......... Move to the previous tab in the passive pane.

       C-x k ......... Kill buffer and move to the next tabbed one (if any).
"
  nil nil sunrise-tabs-mode-map
  (unless (memq major-mode
                '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
    (setq sunrise-tabs-mode nil)
    (error "Sorry, this mode can be used only within the Sunrise Commander."))
  (if sunrise-tabs-mode
      (sunrise-tabs-engage)
    (sunrise-tabs-disengage)))

(defvar sunrise-tabs-editable-dired-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map sunrise-tabs-mode-map)
    (define-key map "\C-n"  'dired-next-line)
    (define-key map "\C-p"  'dired-previous-line)
    (define-key map "\C-tn" 'sunrise-tabs-next)
    (define-key map "\C-tp" 'sunrise-tabs-prev)
    map)
  "Keymap for managing tabs inside Editable Dired mode panes.")

(defadvice sunrise-editable-pane (after sunrise-tabs-advice-editable-pane ())
  "Install `sunrise-tabs-editable-dired-map' when in Editable Dired mode."
  (add-to-list 'minor-mode-overriding-map-alist
               `(sunrise-tabs-mode . ,sunrise-tabs-editable-dired-map)))

;;; ==========================================================================
;;; Bootstrap:

(defun sunrise-tabs-menu-init ()
  "Initialize the Sunrise Tabs extension menu."
  (unless (lookup-key sunrise-mode-map [menu-bar Sunrise])
    (define-key sunrise-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Tabs")))
    (define-key sunrise-mode-map [menu-bar Sunrise tabs]
      (cons "Tabs" menu-map))
    (define-key menu-map [help]
      '("Help" . (lambda ()
                   (interactive)
                   (describe-function 'sunrise-tabs-mode))))
    (define-key menu-map [transpose]
      '("Transpose" . sunrise-tabs-transpose))
    (define-key menu-map [kill]
      '("Kill and go to next" . sunrise-tabs-kill-and-go))
    (define-key menu-map [next]
      '("Next"         . sunrise-tabs-next))
    (define-key menu-map [prev]
      '("Previous"     . sunrise-tabs-prev))
    (define-key menu-map [remove]
      '("Remove"       . sunrise-tabs-remove))
    (define-key menu-map [add]
      '("Add/Rename"   . sunrise-tabs-add))))

(defun sunrise-tabs-start-once ()
  "Bootstrap the tabs mode on the first execution of the Sunrise Commander,
after module installation."
  (sunrise-tabs-mode t)
  (sunrise-tabs-menu-init)
  (remove-hook 'sunrise-start-hook 'sunrise-tabs-start-once)
  (unintern 'sunrise-tabs-menu-init obarray)
  (unintern 'sunrise-tabs-start-once obarray))
(add-hook 'sunrise-start-hook 'sunrise-tabs-start-once)

;;; ==========================================================================
;;; Desktop support:

(defun sunrise-tabs-desktop-save-buffer (_desktop-dir)
  "Return additional desktop data to save tabs of the current Sunrise buffer."
  (let* ((left-tab (car (member (buffer-name) (assoc 'left sunrise-tabs))))
         (left-cache (cdr (assq 'left sunrise-tabs-labels-cache)))
         (left-label (cadr (assoc left-tab left-cache)))
         (right-tab (car (member (buffer-name) (assoc 'right sunrise-tabs))))
         (right-cache (cdr (assq 'right sunrise-tabs-labels-cache)))
         (right-label (cadr (assoc right-tab right-cache))))
    (delq
     nil
     (list
      (and left-label
           (cons 'left-tab (sunrise-tabs-trim-label left-label)))
      (and right-label
           (cons 'right-tab (sunrise-tabs-trim-label right-label)))))))

(defun sunrise-tabs-desktop-restore-buffer (_desktop-buffer-file-name
                                            _desktop-buffer-name
                                            desktop-buffer-misc)
  "Restore all tabs in a Sunrise (normal or VIRTUAL) buffer from a desktop file."
  (mapc (lambda (side)
          (let* ((sunrise-selected-window side)
                 (tab-symbol (intern (concat (symbol-name side) "-tab")))
                 (name (buffer-name))
                 (label (cdr (assq tab-symbol desktop-buffer-misc)))
                 (tab-set (assq side sunrise-tabs)))
            (when label
              (setcdr tab-set (cons name (cdr tab-set)))
              (sunrise-tabs-make-label name label))))
        '(left right))
  (unless sunrise-tabs-on
    (sunrise-tabs-engage)))

(defun sunrise-tabs-reset-state ()
  "Reset some environment variables that control the behavior of
tabs in the Sunrise Commander (used for desktop support)."
  (mapc (lambda (x) (setcdr x nil)) sunrise-tabs-labels-cache)
  (mapc (lambda (x) (setcdr x nil)) sunrise-tabs)
  nil)

;; Append the previous functions to the generic desktop support in Sunrise:
(add-to-list 'sunrise-desktop-save-handlers
             'sunrise-tabs-desktop-save-buffer)
(add-to-list 'sunrise-desktop-restore-handlers
             'sunrise-tabs-desktop-restore-buffer)

;; Activate tabs support after desktop restoration:
(add-hook
 'desktop-after-read-hook
 (defun sunrise-tabs-desktop-after-read-function ()
   (unless (assq 'sunrise-tabs-on desktop-globals-to-clear)
     (add-to-list 'desktop-globals-to-clear
                  '(sunrise-tabs-on . (sunrise-tabs-reset-state))))))

(defun sunrise-tabs-unload-function ()
  (sunrise-ad-disable "^sunrise-tabs-"))

(provide 'sunrise-tabs)

;;; sunrise-tabs.el ends here
