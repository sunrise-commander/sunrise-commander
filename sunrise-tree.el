;;; sunrise-tree.el --- Tree view for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2010-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 4 May 2010
;; Version: 1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, sunrise commander, directories tree navigation
;; URL: https://github.com/sunrise-commander/sunrise-commander

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension adds to the Sunrise Commander file manager a directories-only
;; tree view that can be used for fast navigation, as well as for several basic
;; operations on files and directories. It uses the excellent "tree-widget.el"
;; library written by David Ponce and works the same in text consoles as well as
;; in graphical environments, using either the mouse or just the keyboard.

;; For more information on the Sunrise Commander, other extensions and cool tips
;; & tricks visit http://www.emacswiki.org/emacs/Sunrise_Commander

;; This extension was developed on GNU Emacs 24 on Linux and tested on GNU Emacs
;; 22 and 24 for Linux, and on EmacsW32 (version 23) for Windows.

;;; Installation:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-tree) expression to your .emacs file somewhere
;; after the (require 'sunrise) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) You may have to customize the `tree-widget-image-enable' variable if all
;; you get are text-only icons (e.g. "[+]" and "[X]") in your graphical
;; environment, while you'd rather prefer looking at pretty graphical ones.

;; WARNING: If you use Slime be aware that some versions of this package include
;; an older version of tree-widget.el that may clobber the one in Emacs and make
;; this extension work improperly. At least that's the case in Debian for i386:
;; slime comes with version 21.4 of tree-widget, but the extension requires 22.1
;; or better.

;;; Usage:

;; In order to explain the different ways this extension is used, it's necessary
;; to first introduce a few concepts:
;; * A Sunrise Tree View pane displays a list of directories arranged in a tree-
;;   like structure. There is exactly one such TREE in every Tree View pane.
;; * Each node in this tree is called FOLDER and represents one directory in the
;;   file system. A folder can be in one of two states: OPEN or CLOSED. When the
;;   folder is open, its children (if any) are displayed under it in the tree.
;; * The top-most folder in every tree is called the ROOT of the tree. All other
;;   folders in the same tree represent sub-directories of the root directory.
;; * To FOCUS a given folder means to replace the current tree with one that has
;;   that folder as its root.
;; * The opposite operation of focusing a folder, i.e. showing it in the context
;;   of a broader tree, is called BLURRING the folder.
;; * Finally, to EXPLODE a given folder means to open it, then all its children,
;;   then all the children of its children and so on, as many times as the value
;;   of  the   `sunrise-tree-explosion-ratio'  option   (which  can   be  customized)
;;   specifies. This  is an additive  operation, which means that  exploding the
;;   same directory  many times  will open  more of  its descendants  deeper and
;;   deeper until the tree runs out of closed folders in that branch.

;; The Sunrise Tree View mode offers three different ways of navigating the file
;; system: with the mouse (for rodent lovers), with the arrow keys (for rookies)
;; and with other keys nearer the home row (for keyboard junkies).

;; 1. With the mouse:

;; * Meta + Shift + left click anywhere inside a pane to switch between tree and
;;   normal modes.
;; * Left click on a folder or anywhere beside it to open or close the folder.
;; * Middle click on a folder, or anywhere beside it, to just select it without
;;   changing its state.
;; * Shift + left click on a folder or anywhere beside it to focus it.
;; * Meta + left click on a folder or anywhere beside it to blur it. Meta + left
;;   click anywhere else in the pane to blur the currently selected folder.
;; * Control + left click on a folder or anywhere beside it to explode it.
;; * Double click on a folder to dismiss tree view and visit the folder.
;; * Left or Middle click anywhere on the path line at the top of the pane to go
;;   directly to the directory which path ends at that point in the line.

;; 2. With the arrow keys:

;; * Meta + Shift + down switches between tree and normal modes.
;; * Up and down move the cursor up and down (duh!)
;; * Right opens a folder if it was closed, or browses it if it was open.
;; * Left closes a folder if it was open, or jumps up to its parent folder if it
;;   was closed.
;; * Shift + right focuses the selected folder.
;; * Shift + left blurs the selected folder.
;; * Control + right explodes the selected folder.
;; * If you're in a text console and  the bindings above don't work for you, try
;;   using Escape instead of Shift (not combined -- first press escape, then the
;;   arrow key) and C-c instead of Control.

;; 3. With alphanumeric keys:

;; * C-t Space (alternatively C-t Return) - switch between modes.
;; * n, p - move cursor up/down.
;; * Space, Return - open closed folder / browse already open folder.
;; * Backspace - close open folder / jump to parent of already closed folder.
;; * C-c f - focus the selected folder.
;; * C-c b - blur the selected folder.
;; * C-Return, C-c Return - explode the selected folder.
;; * f - browse the selected folder in normal mode.
;; * v, o - view the selected folder in the passive pane, in whatever mode it
;;   happens to be at that moment.
;; * C-c C-c - dismiss tree view and return to normal mode.

;; * C-q is simply another binding for the usual pane synchronization (C-c C-z)
;;   already present in Sunrise Commander Core, which in tree mode performs the
;;   "Quick View" operation required by the OFM standard.

;; * C-u C-s, C-u C-r - "sticky" interactive search. This works like the regular
;; isearch, but when the current search is finished with a Return, the folder
;; the cursor ends on is automatically opened and a new (forward) Isearch
;; starts, so one can continue searching among the children of that folder. This
;; allows for extremely fast navigation across lengthy paths of directories with
;; just a few keystrokes. To terminate a sticky search, press C-g or (once
;; again) Return. Sticky searches can be made default in tree panes by
;; customizing the variable `sunrise-tree-isearch-always-sticky' - when set, prefix
;; the command to start a regular (non-sticky) interactive search.

;; * When AVFS support is active, press "#" to toggle the display of compressed
;; archives in Tree View panes.

;; Additionally, most of the original keybindings from Sunrise apply (wherever
;; it makes sense, of course). For instance switching/transposing/laying out
;; panes (Tab, M-Tab, C-c, C-s), showing / hiding hidden directories (C-o),
;; jumping to parent/arbitrary directory (J, j) and many more, including the
;; following file manipulation commands: copy (C), clone (K), rename (R), delete
;; (D), symlink (S), relative symlink (Y), create a new directory (+) and show
;; file size (y).

;; All directory commands from the Sunrise Buttons extension are also supported.

;; Hey, and don't forget to enjoy ;-)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'desktop))

(require 'hl-line)
(require 'tree-widget)

(require 'sunrise)

(defcustom sunrise-tree-explosion-ratio 3
  "Maximum number of directory levels to recursively open at a time.
Used by the command `sunrise-tree-explode-branch'."
  :group 'sunrise
  :type 'integer)

(defcustom sunrise-tree-isearch-always-sticky nil
  "Whether interactive searches are always sticky in tree panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-tree-avfs-handlers-alist '(("\\.od[fgpst]$" . "#uzip/")
                                              ("\\.oxt$"       . "#uzip/")
                                              ("\\.sx[dmicw]$" . "#uzip/"))
  "List of AVFS handlers to manage specific file extensions in Tree View mode."
  :group 'sunrise
  :type 'alist)

(defvar sunrise-tree-root nil
  "Root widget of the current tree view.")

(defvar sunrise-tree-open-paths nil
  "List of paths to all the directories open in the current tree view.")

(defvar sunrise-tree-avfs-seen nil
  "List of paths to big compressed archives visited through AVFS.")

(defvar sunrise-tree-cursor nil
  "Cons cell of the from (LABEL . FILEPATH).
FILEPATH is the path to the selected directory in the current tree
view. LABEL is the name displayed in the tree representing FILEPATH")

(defvar sunrise-tree-mode-map (make-sparse-keymap)
  "Keymap for the Sunrise Commander Tree View.")

(defvar sunrise-tree-omit-archives t "")

(define-widget 'sunrise-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs  'sunrise-tree-expand-dir
  :has-children   t)

;;; ============================================================================
;;; GUI Management functions:

(defun sunrise-tree-focus-widget ()
  "Move point to the first button widget in the current line (if any)."
  (interactive)
  (beginning-of-line)
  (unless (get-char-property (point) 'button)
    (while (not (or (eolp) (get-char-property (point) 'button)))
      (forward-char))))

(defun sunrise-tree-get-button ()
  "Return the first button widget in the current line (if any)."
  (sunrise-tree-focus-widget)
  (get-char-property (point) 'button))

(defun sunrise-tree-get-branch ()
  "Return the first tree widget in the current line (if any)."
  (widget-get (sunrise-tree-get-button) :parent))

(defun sunrise-tree-get-cursor ()
  "Return a cursor as documented in `sunrise-tree-cursor'."
  (let* ((cursor-node (sunrise-tree-get-button))
         (cursor-tree (if cursor-node (widget-get cursor-node :parent)))
         (cursor-node (widget-get cursor-node :node))
         (cursor-tag (widget-get cursor-node :tag))
         (cursor-path (sunrise-tree-path-line (widget-get cursor-tree :path))))
    (and cursor-tag cursor-path (cons cursor-tag cursor-path))))

(defun sunrise-tree-update-cursor ()
  "Update the cursor (cf. `sunrise-tree-cursor').
Also updates other graphical elements of the interface, depending
on the position of the point."
  (setq sunrise-tree-cursor (sunrise-tree-get-cursor))
  (when sunrise-tree-cursor
    (setq sunrise-this-directory (cdr sunrise-tree-cursor))
    (sunrise-tree-highlight)
    (setq default-directory (file-name-as-directory (cdr sunrise-tree-cursor)))
    (when (and (featurep 'sunrise-modeline)
               (not (eq mode-line-format (default-value 'mode-line-format))))
      (if (fboundp 'sunrise-modeline-refresh)
          (sunrise-modeline-refresh))
      (force-mode-line-update))
    (if (and sunrise-synchronized
             (not (eq sunrise-left-buffer sunrise-right-buffer))
             (eq (selected-window) (sunrise-this 'window)))
        (sunrise-tree-advertised-find-file-other))))

(defun sunrise-tree-refresh-dir (widget &rest _ignore)
  "Refresh WIDGET parent (or own) tree children. IGNORE other arguments."
  (let ((tree (if (tree-widget-p widget)
                  widget
                (widget-get widget :parent))))
    (widget-put tree :args nil) ;; Clear the tree children cache.
    (widget-value-set tree (widget-value tree))) ;; Redraw the tree node.
  (if (fboundp 'sunrise-tabs-refresh)
      (sunrise-tabs-refresh))
  (sunrise-highlight))

(defun sunrise-tree-refresh-branch (&optional prefix)
  "Revert the currently selected branch in the directory tree.
If no branch is selected, then select the root node and revert
the whole tree. If PREFIX is non-nil, close all open
subdirectories in the tree first."
  (interactive "P")
  (if prefix
      (setq sunrise-tree-open-paths nil))
  (let ((button (sunrise-tree-get-button)))
    (unless button
      (sunrise-tree-beginning-of-buffer)
      (setq button (sunrise-tree-get-button)))
    (sunrise-tree-refresh-dir button)))

(defun sunrise-tree-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert the current Sunrise Tree View buffer."
  (interactive)
  (sunrise-tree-refresh-branch))

(defun sunrise-tree-widget (e &optional open)
  "Return a widget to display directory E.
With a non-nil optional argument OPEN, display the widget as open
initially."
  (let ((is-open (or open (member e sunrise-tree-open-paths)))
        (tag (sunrise-chop ?/ e)))
    (setq tag (file-name-as-directory (file-name-nondirectory tag)))
    `(sunrise-tree-dir-widget
      :open ,is-open
      :node (push-button
             :tag ,tag
             :format "%[%t%]\n"
             :notify sunrise-tree-refresh-dir)
      :path ,e)))

(defun sunrise-tree-path-line (&optional path)
  "Transform PATH into a suitable path line for displaying at the pane top."
  (sunrise-chop ?/ (expand-file-name (or path (cdr sunrise-tree-cursor) ""))))

(defun sunrise-tree-highlight ()
  "Set up the path line in the current Sunrise Tree buffer."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (delete-region (point) (line-end-position))
      (widget-insert (propertize (concat (sunrise-tree-path-line nil) " ")
                                 'face 'sunrise-passive-path-face))
      (forward-line 1)
      (delete-region (line-beginning-position) (line-end-position))
      (widget-insert
       (format "%s" (if sunrise-tree-omit-archives "" "virtual directories: ON ")))
      (sunrise-highlight))))

(defun sunrise-tree-check-virtual-size (entry)
  "Allow user to abort before trying to access a large archive through AVFS."
  ;; TODO: use function abort-if-file-too-large instead:
  (if (and sunrise-avfs-root
           (sunrise-overlapping-paths-p sunrise-avfs-root entry)
           (string-match "^\\([^#]+\\)#" entry))
      (let* ((root (match-string 1 entry))
             (root (substring root (length sunrise-avfs-root)))
             (size (nth 7 (file-attributes root))))
        (when (and large-file-warning-threshold
                   (not (member root sunrise-tree-avfs-seen))
                   (> size large-file-warning-threshold))
          (or (y-or-n-p
               (format "File %s is large (%dMB), really open? "
                       (file-name-nondirectory root) (/ size 1048576)))
              (error "Aborted"))
          (sunrise-tree-avfs-register-seen root)))))

(defun sunrise-tree-list (dir)
  "Return the list of subdirectories in DIR."
  (let ((entries (directory-files dir 'full)) dirs entry rel-entry)
    (while entries
      (setq entry (car entries)
            rel-entry (file-relative-name entry (concat entry "/.."))
            entries (cdr entries))

      (cond ((eq ?. (string-to-char (substring entry -1)))
             (ignore))

            ((and dired-omit-mode (eq ?. (string-to-char rel-entry)))
             (ignore))

            ((file-directory-p entry)
             (setq dirs (cons entry dirs)))

            ((and (not sunrise-tree-omit-archives) (sunrise-avfs-directory-p entry))
             (setq dirs (cons (sunrise-tree-avfs-dir entry) dirs)))

            (t (ignore))))
    (nreverse dirs)))

(defun sunrise-tree-avfs-dir (filename)
  "Return the virtual path for accessing FILENAME through AVFS in Tree View panes.
Returns nil if AVFS cannot manage this kind of file."
  (let* ((handler
          (or (assoc-default filename sunrise-tree-avfs-handlers-alist 'string-match)
              (assoc-default filename sunrise-avfs-handlers-alist 'string-match)))
         (vdir (concat filename handler)))
    (unless (sunrise-overlapping-paths-p sunrise-avfs-root vdir)
      (setq vdir (concat sunrise-avfs-root vdir)))
    (sunrise-tree-path-line vdir)))

(defun sunrise-tree-expand-dir (tree)
  "Return TREE widget children. Reuse :args cache if it exists."
  (or (widget-get tree :args)
      (let ((dir (widget-get tree :path)))
        (message "Reading directory '%s'..." dir)
        (condition-case err
            (prog1
             (mapcar 'sunrise-tree-widget (sunrise-tree-list dir))
             (message "Reading directory '%s'...done" dir))
          (error
           (widget-put tree :open nil)
           (message "%s" (error-message-string err))
           nil)))))

(defun sunrise-tree-register-path (widget)
  "Add path from WIDGET to the current Sunrise Tree buffer's list of open paths."
  (let ((path (sunrise-tree-path-line (widget-get widget :path))))
    (setq sunrise-tree-open-paths
          (if (widget-get widget :open)
              (cons path sunrise-tree-open-paths)
            (delete path sunrise-tree-open-paths)))))
(add-hook 'tree-widget-after-toggle-functions 'sunrise-tree-register-path)

(defun sunrise-tree-avfs-register-seen (path)
  "Add the PATH to the list of (big) archives visited through AVFS."
  (setq sunrise-tree-avfs-seen (cons path (delete path sunrise-tree-avfs-seen))))

(defun sunrise-tree-build (root)
  "Delete the current tree widget and build a new one at ROOT."
  (interactive "DSunrise Tree Root: ")
  (setq default-directory
        (file-name-as-directory (setq root (expand-file-name root))))
  (let ((inhibit-read-only t)
        (all (overlay-lists)))
    (erase-buffer)
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all))
    (tree-widget-set-theme "folder")
    (widget-insert (format "%s\n\n" (sunrise-tree-path-line root)))
    (set
     (make-local-variable 'sunrise-tree-root)
     (widget-create (sunrise-tree-widget root t)))
    (widget-setup)
    (if sunrise-tree-cursor
        (sunrise-tree-search-cursor)
      (sunrise-tree-beginning-of-buffer))
    (sunrise-tree-refresh-branch)
    (sunrise-tree-register-path sunrise-tree-root)))

(defun sunrise-tree-build-new (root)
  "Build a new Tree View pane in a new buffer at ROOT."
  (interactive "DSunrise Tree Root: ")
  (let ((default-directory root))
    (sunrise-save-aspect
     (sunrise-alternate-buffer
      (switch-to-buffer (generate-new-buffer "Sunrise Tree")))
     (sunrise-tree-mode))))

(defun sunrise-tree-goto-dir (root &optional keep-state)
  "`sunrise-goto-dir' replacement for buffers in Sunrise Tree mode.
See also the variable `sunrise-goto-dir-function'."
  (interactive)
  (setq root (expand-file-name root))
  (let ((cursor sunrise-tree-cursor)
        (open-paths sunrise-tree-open-paths))
    (sunrise-tree-build-new root)
    (when keep-state
      (setq sunrise-tree-cursor cursor
            sunrise-tree-open-paths (mapcar 'identity open-paths))))
  (sunrise-keep-buffer)
  (sunrise-history-push root))

(defadvice sunrise-focus-filename
    (around sunrise-tree-advice-focus-filename (filename))
  "Force deactivation of Sunrise Tree View before focusing a regular file."
  (if (eq major-mode 'sunrise-tree-mode)
      (if (file-directory-p filename)
          (let* ((path (directory-file-name (expand-file-name filename)))
                 (label (file-name-as-directory (file-name-nondirectory path))))
            (with-no-warnings (sunrise-tree-search-cursor (cons label path))))
        (with-no-warnings (sunrise-tree-dismiss))
        ad-do-it)
    ad-do-it))
(ad-activate 'sunrise-focus-filename)

;;; ============================================================================
;;; GUI interaction functions:

(defun sunrise-tree-next-line ()
  "Move point to the next line in the current pane."
  (interactive)
  (forward-line)
  (sunrise-tree-update-cursor))

(defun sunrise-tree-previous-line ()
  "Move point to the previous line in the current pane."
  (interactive)
  (forward-line -1)
  (sunrise-tree-update-cursor))

(defun sunrise-tree-scroll-down (&optional arg)
  "Scroll down the current pane without moving the point (if possible)."
  (interactive)
  (scroll-down arg)
  (sunrise-tree-update-cursor))

(defun sunrise-tree-scroll-up (&optional arg)
  "Scroll up the current pane without moving the point (if possible)."
  (interactive)
  (scroll-up arg)
  (sunrise-tree-update-cursor))

(defun sunrise-tree-beginning-of-buffer ()
  "Move cursor to the top of the current Sunrise Tree View pane."
  (interactive)
  (goto-char (widget-get sunrise-tree-root :from))
  (sunrise-tree-update-cursor))

(defun sunrise-tree-end-of-buffer ()
  "Move cursor to the bottom of the current Sunrise Tree View pane."
  (interactive)
  (forward-sentence)
  (sunrise-tree-update-cursor))

(defun sunrise-tree-toggle-branch (&optional action)
  "Open/close (graphically) the node selected in the current Sunrise Tree pane.
Optional ACTION is one of the symbols `open' or `close' and
allows to specify whether the node has to be open only if closed,
or closed only if open."
  (interactive)
  (let* ((branch (sunrise-tree-get-branch))
         (is-open (widget-get branch :open))
         (path))
    (unless is-open
      (setq path (widget-get branch :path))
      (sunrise-tree-check-virtual-size path)
      t)
    (when (or (and is-open (eq action 'close))
              (and (not is-open) (eq action 'open))
              (null action))
      (sunrise-tree-focus-widget)
      (widget-button-press (point))
      t)))

(defun sunrise-tree-open-branch ()
  "Unfold (graphically) the directory selected in the current Sunrise Tree pane.
Displays the subdirectories directly under it."
  (interactive)
  (if (widget-get (sunrise-tree-get-branch) :open)
      (sunrise-tree-advertised-find-file)
    (sunrise-tree-toggle-branch 'open)))

(defun sunrise-tree-close-branch ()
  "Fold the selected directory.
Hides all subdirectories being displayed under it or any of its
subdirectories."
  (interactive)
  (sunrise-tree-toggle-branch 'close))

(defun sunrise-tree-collapse-branch ()
  "If the current folder is open, close it.
If it is closed, move to its parent directory, building a new
tree if necessary."
  (interactive)
  (let ((branch (sunrise-tree-get-branch)))
    (if (widget-get branch :open)
        (sunrise-tree-close-branch)
      (sunrise-tree-prev-subdir)
      (unless (eq (sunrise-tree-get-branch) sunrise-tree-root)
        (sunrise-tree-close-branch)))))

(defun sunrise-tree-explode-branch (&optional level branch)
  "Open the selected directory and all its open subdirectories recursively.
The number of levels is determined by the variable
`sunrise-tree-explosion-ratio'. LEVEL and BRANCH optional arguments
are used only internally to control recursion."
  (interactive)
  (unless (or level branch)
    (recenter (truncate (/ (window-body-height) 10.0))))
  (let ((level (or level sunrise-tree-explosion-ratio))
        (branch (or branch (sunrise-tree-get-branch))))
    (when (and branch (< 0 level))
      (unless (widget-get branch :open)
        (setq level (1- level))
        (funcall #'tree-widget-action branch))
      (dolist (child (cdr (widget-get branch :children)))
        (sunrise-tree-explode-branch level child)))))

(defun sunrise-tree-search-cursor (&optional init-cursor recursing)
  "Try to move the point to the node represented by INIT-CURSOR.
If it is nil, use the value of `sunrise-tree-cursor' instead. On
failure, put the point at the top of the pane."
  (let ((cursor (or init-cursor sunrise-tree-cursor)) new-cursor)
    (if (null cursor)
        (sunrise-tree-beginning-of-buffer)
      (unless recursing (goto-char (point-min)))
      (when (search-forward (car cursor) (point-max) t)
        (setq new-cursor (sunrise-tree-get-cursor))
        (if (and new-cursor (not (sunrise-equal-dirs (cdr cursor) (cdr new-cursor))))
            (progn
              (sunrise-tree-next-line)
              (sunrise-tree-search-cursor cursor t))
          (sunrise-tree-update-cursor))))))

(defun sunrise-tree-isearch-prompt ()
  "Display the message that appears when a sticky search is launched."
  (message (propertize "Sunrise Tree sticky I-search (C-g to exit): "
                       'face 'minibuffer-prompt)))

(defvar sunrise-tree-isearch-mode-commands
  '(([S-return]  . 'sunrise-tree-focus-branch)
    ([S-right]   . 'sunrise-tree-focus-branch)
    ([?\e right] . 'sunrise-tree-focus-branch)
    ("\C-cf"     . 'sunrise-tree-focus-branch)

    ([M-return]  . 'sunrise-tree-blur-branch)
    ([S-left]    . 'sunrise-tree-blur-branch)
    ([?\e left]  . 'sunrise-tree-blur-branch)
    ("\C-cb"     . 'sunrise-tree-blur-branch)

    ([C-return]  . 'sunrise-tree-explode-branch)
    ([C-right]   . 'sunrise-tree-explode-branch)
    ([3 right]   . 'sunrise-tree-explode-branch)
    ("\C-c\C-m"  . 'sunrise-tree-explode-branch))
  "Keybindings installed in `isearch-mode' during a sticky search.")

(defsubst sunrise-tree-isearch-command (binding)
  `(lambda () (interactive) (sunrise-tree-post-isearch ,(cdr binding))))

(defun sunrise-tree-isearch-setup ()
  "Set up Isearch to perform sticky searches in Sunrise Tree panes.
Used from `isearch-mode-hook'."
  (add-hook 'isearch-mode-end-hook 'sunrise-tree-post-isearch)
  (set (make-local-variable 'search-nonincremental-instead) nil)
  (define-key isearch-mode-map "\C-c" (make-sparse-keymap))
  (mapc (lambda (binding)
          (define-key
            isearch-mode-map
            (car binding) (sunrise-tree-isearch-command binding)))
        sunrise-tree-isearch-mode-commands)
  (run-hooks 'sunrise-refresh-hook))

(defun sunrise-tree-isearch-done ()
  "Clean up the Isearch hook and keymap after a sticky search."
  (remove-hook 'isearch-mode-end-hook 'sunrise-tree-post-isearch)
  (kill-local-variable 'search-nonincremental-instead)
  (mapc (lambda (binding)
          (define-key isearch-mode-map (car binding) nil))
        sunrise-tree-isearch-mode-commands)
  (define-key isearch-mode-map "\C-c" 'isearch-other-control-char)
  (isearch-done)
  (setq isearch-mode-end-hook-quit t))

(defun sunrise-tree-isearch-forward (&optional prefix)
  "Prefixable version of `isearch-forward' used in Sunrise Tree mode.
With PREFIX, starts a new `isearch-forward' immediately after the
previous one exits as long as C-g is not pressed."
  (interactive "P")
  (if (or (and prefix (not sunrise-tree-isearch-always-sticky))
          (and (not prefix) sunrise-tree-isearch-always-sticky))
      (sunrise-tree-sticky-isearch-forward)
    (isearch-forward nil t)))

(defun sunrise-tree-sticky-isearch-forward ()
  "Chain Isearch operations to allow fast navigation through long file paths.
Press C-g to abort, or Return twice on a folder to dismiss Tree
View and visit that folder."
  (interactive)
  (sunrise-tree-isearch-setup)
  (isearch-forward nil t)
  (run-with-idle-timer 0.01 nil 'sunrise-tree-isearch-prompt))

(defun sunrise-tree-isearch-backward (&optional prefix)
  "Prefixable version of `isearch-backward' used in Sunrise Tree mode.
With PREFIX, starts a new `isearch-forward' immediately after the
previous search exits until C-g is pressed."
  (interactive "P")
  (if (or (and prefix (not sunrise-tree-isearch-always-sticky))
          (and (not prefix) sunrise-tree-isearch-always-sticky))
      (sunrise-tree-isearch-setup))
  (isearch-backward nil t)
  (run-with-idle-timer 0.01 nil 'sunrise-tree-isearch-prompt))

(defun sunrise-tree-sticky-isearch-backward ()
  "Chain Isearch operations to allow fast navigation through long file paths.
Press C-g to abort, or Return twice on a folder to dismiss Tree
View and visit that folder."
  (interactive)
  (sunrise-tree-isearch-setup)
  (isearch-backward nil t))

(defun sunrise-tree-post-isearch (&optional command)
  "Installed in `isearch-mode-end-hook' during sticky Isearch operations."
  (sunrise-tree-update-cursor)
  (cond (command (sunrise-tree-isearch-command-loop command))
        (isearch-mode-end-hook-quit (sunrise-tree-isearch-done))
        ((equal "" isearch-string) (sunrise-tree-open-branch))
        ((and (sunrise-tree-toggle-branch 'open)
              (widget-get (sunrise-tree-get-branch) :args))
         (recenter (truncate (/ (window-body-height) 10.0))))
        (t (ignore)))
  (unless isearch-mode-end-hook-quit
    (sunrise-tree-sticky-isearch-forward)))

(defun sunrise-tree-isearch-command-loop (command)
  (funcall command)
  (let* ((msg "Sunrise Tree: sticky Isearch (C-g to exit)")
         (key (read-key-sequence msg))
         (next-command (lookup-key sunrise-tree-mode-map key)))
    (while (memq next-command '(sunrise-tree-explode-branch
                                sunrise-tree-focus-branch
                                sunrise-tree-blur-branch))
      (funcall next-command)
      (setq key (read-key-sequence msg)
            next-command (lookup-key sunrise-tree-mode-map key)))
    (isearch-unread (listify-key-sequence key))
    (setq isearch-mode-end-hook-quit nil)))

(defun sunrise-tree-focus-branch ()
  "Replace the current tree with a new one rooted in the selected directory."
  (interactive)
  (unless (eq (sunrise-tree-get-branch) sunrise-tree-root)
    (sunrise-tree-goto-dir (cdr sunrise-tree-cursor) t)
    (if sunrise-tree-open-paths (revert-buffer))))

(defun sunrise-tree-blur-branch ()
  "Change root of the current tree to its parent, keeping the cursor position."
  (interactive)
  (let ((cursor sunrise-tree-cursor))
    (unless (eq (sunrise-tree-get-branch) sunrise-tree-root)
      (sunrise-tree-beginning-of-buffer))
    (sunrise-tree-prev-subdir t)
    (revert-buffer)
    (sunrise-tree-search-cursor cursor))
  (recenter))

(defun sunrise-tree-omit-mode (&optional force)
  "Toggle `dired-omit-mode' in the current Sunrise Tree View pane."
  (interactive)
  (setq dired-omit-mode (or force (not dired-omit-mode)))
  (revert-buffer))

(defun sunrise-tree-avfs-toggle ()
  "Toggle display of compressed archives in Sunrise Tree View panes."
  (interactive)
  (if sunrise-avfs-root
      (let ((cursor sunrise-tree-cursor))
        (setq sunrise-tree-omit-archives (not sunrise-tree-omit-archives))
        (sunrise-tree-refresh-dir sunrise-tree-root)
        (sunrise-tree-search-cursor cursor)
        (recenter (truncate (/ (window-body-height) 10.0))))))

(defun sunrise-tree-handle-mouse-event (e handler)
  "Handle mouse event E by updating point and calling HANDLER.
Return t if the event was successfully handled."
  (when (and e (eq major-mode 'sunrise-tree-mode))
    (mouse-set-point e)
    (when (sunrise-tree-get-button)
      (sunrise-tree-update-cursor)
      (funcall handler)
      t)))

(defun sunrise-tree-mouse-toggle-branch (e)
  "Open/close (graphically) the folder clicked with the mouse.
Also handle the case when the click occurs on the path line."
  (interactive "e")
  (or (sunrise-tree-handle-mouse-event e 'sunrise-tree-toggle-branch)
      (sunrise-mouse-advertised-find-file e)))

(defun sunrise-tree-mouse-focus-branch (e)
  "Version of `sunrise-tree-focus-branch' (which see) for the mouse."
  (interactive "e")
  (sunrise-tree-handle-mouse-event e 'sunrise-tree-focus-branch))

(defun sunrise-tree-mouse-blur-branch (e)
  "Version of `sunrise-tree-blur-branch' (which see) for the mouse."
  (interactive "e")
  (or (sunrise-tree-handle-mouse-event e 'sunrise-tree-blur-branch)
      (sunrise-tree-blur-branch)))

(defun sunrise-tree-mouse-explode-branch (e)
  "Version of `sunrise-tree-explode-branch' (which see) for the mouse."
  (interactive "e")
  (sunrise-tree-handle-mouse-event e 'sunrise-tree-explode-branch))

;;; ============================================================================
;;; File system navigation functions:

(defun sunrise-tree-prev-subdir (&optional keep-state)
  "Move to the parent of currently selected directory in Tree View mode.
Resets the list of open directories unless KEEP-STATE is not
nil."
  (interactive)
  (let* ((branch (sunrise-tree-get-branch))
         (parent (widget-get branch :parent)))
    (cond
     ((tree-widget-p parent)
      (goto-char (widget-get parent :from))
      (sunrise-tree-update-cursor))

     ((sunrise-equal-dirs default-directory "/")
      (ignore))

     ((eq branch sunrise-tree-root)
      (sunrise-tree-register-path sunrise-tree-root)
      (sunrise-tree-goto-dir ".." keep-state)
      (sunrise-tree-beginning-of-buffer)))))

(defun sunrise-tree-jump-up ()
  (interactive)
  (sunrise-tree-prev-subdir t)
  (revert-buffer))

(defun sunrise-tree-advertised-find-file ()
  "Visit the currently selected file or directory in Sunrise Tree View mode."
  (interactive)
  (let ((target (cdr sunrise-tree-cursor))
        (sunrise-goto-dir-function nil)
        (in-search (memq 'sunrise-tree-post-isearch isearch-mode-end-hook)))
    (sunrise-tree-check-virtual-size target)
    (if in-search (sunrise-tree-isearch-done))
    (sunrise-save-aspect (sunrise-alternate-buffer (sunrise-goto-dir target)))
    (if in-search (sunrise-sticky-isearch))))

(defun sunrise-tree-mouse-advertised-find-file (e)
  "Visit a file or directory selected using the mouse in the current pane."
  (interactive "e")
  (sunrise-tree-handle-mouse-event e 'sunrise-tree-advertised-find-file))

(defun sunrise-tree-advertised-find-file-other ()
  "Visit the currently selected file or directory in the passive pane."
  (interactive)
  (let ((target (cdr sunrise-tree-cursor)) (side (sunrise-other))
        (sunrise-inhibit-highlight t))
    (sunrise-tree-check-virtual-size target)
    (save-selected-window
      (select-window (sunrise-other 'window))
      (sunrise-goto-dir target)
      (sunrise-display-attributes (point-min) (point-max) sunrise-show-file-attributes)
      (sunrise-keep-buffer side)
      (if (fboundp 'sunrise-modeline-refresh) (sunrise-modeline-refresh))
      (if (fboundp 'sunrise-tabs-refresh) (sunrise-tabs-refresh)))))

(defun sunrise-tree-sync ()
  "Toggle the synchronized navigation feature in Sunrise Tree View panes."
  (interactive)
  (sunrise-sync)
  (sunrise-tree-update-cursor))

;;; ============================================================================
;;; File system manipulation functions:

(defun sunrise-tree-get-filename (&optional _localp _no-error)
  "Replacement for `dired-get-filename' in Sunrise Tree functions."
  (cdr sunrise-tree-cursor))

(defun sunrise-tree-show-file-type (_file &optional _deref-symlinks)
  "Replacement for `dired-show-file-type' in Sunrise Tree functions."
  (message "%s: directory" (directory-file-name (car sunrise-tree-cursor))))

(defmacro sunrise-tree-adapt-dired-command (form)
  "Evaluate FORM with a few Dired functions locally redefined.
Necessary so the basic Dired file manipulation commands can work
in Sunrise Tree View mode."
  `(let ((ad-redefinition-action 'accept))
     (cl-letf (((symbol-function 'dired-get-filename)
                (symbol-function 'sunrise-tree-get-filename))
               ((symbol-function 'dired-show-file-type)
                (symbol-function 'sunrise-tree-show-file-type)))
       ,form)))

(defun sunrise-tree-do-copy ()
  "Recursively copy all selected files and directories between panes.
Copies files from the active to the passive pane."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-do-copy)))

(defun sunrise-tree-do-clone ()
  "Recursively clone all selected files and directories between panes.
Clones files from active to the passive pane."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-do-clone)))

(defun sunrise-tree-do-symlink ()
  "Create symbolic links in the passive pane to selected files in the active pane."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-do-symlink)))

(defun sunrise-tree-do-relsymlink ()
  "Make relative symbolic links in the passive pane to selected files in the active pane."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-do-relsymlink)))

(defun sunrise-tree-do-rename ()
  "Recursively move all selected files and directories between panes.
Moves files from the active pane to the passive pane."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-do-rename))
  (unless (file-exists-p (cdr sunrise-tree-cursor))
    (sunrise-tree-prev-subdir)
    (sunrise-tree-refresh-branch)))

(defun sunrise-tree-do-delete ()
  "Remove the directory selected in the current Sunrise Tree View pane."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-do-delete))
  (unless (file-exists-p (cdr sunrise-tree-cursor))
    (sunrise-tree-prev-subdir)
    (sunrise-tree-refresh-branch)))

(defun sunrise-tree-show-files-info ()
  "Version of `sunrise-show-files-info' (which see) for Sunrise Tree View panes."
  (interactive)
  (sunrise-tree-adapt-dired-command (sunrise-show-files-info)))

(defun sunrise-tree-create-directory (directory)
  "Create a new directory in Sunrise Tree View mode."
  (interactive
   (list (read-file-name "Create directory: "
                         (file-name-as-directory (cdr sunrise-tree-cursor)))))
  (let* ((expanded (directory-file-name (expand-file-name directory)))
         (parent (file-name-directory expanded)))
    (make-directory expanded t)
    (when (sunrise-equal-dirs parent (cdr sunrise-tree-cursor))
      (sunrise-tree-toggle-branch 'open)
      (sunrise-tree-refresh-branch)
      (search-forward
       (file-name-as-directory (file-name-nondirectory expanded)) nil t)
      (sunrise-tree-update-cursor))))

;;;###autoload
(define-derived-mode sunrise-tree-mode nil "Sunrise Tree View"
  "Tree view for the Sunrise Commander file manager."
  :group 'sunrise
  (set-keymap-parent sunrise-tree-mode-map sunrise-mode-map)
  (mapc 'make-local-variable '(sunrise-tree-open-paths
                               sunrise-tree-cursor
                               hl-line-sticky-flag
                               isearch-mode-end-hook
                               desktop-save-buffer
                               revert-buffer-function
                               sunrise-goto-dir-function))
  (add-hook 'isearch-mode-end-hook 'sunrise-tree-update-cursor)
  (setq desktop-save-buffer        'sunrise-tree-desktop-save-buffer
        hl-line-sticky-flag        nil
        revert-buffer-function     'sunrise-tree-revert-buffer
        sunrise-goto-dir-function       'sunrise-tree-goto-dir)
  (setq dired-omit-mode t)

  (when (boundp 'sunrise-buttons-command-adapter)
    (setq-local sunrise-buttons-command-adapter 'sunrise-tree-buttons-command-adapter))

  (set (make-local-variable 'buffer-quit-function) 'sunrise-quit)
  (set (make-local-variable 'track-mouse) sunrise-cursor-follows-mouse)
  (hl-line-mode 1)
  (unless sunrise-tree-root
    (sunrise-tree-build default-directory)))

;;; ============================================================================
;;; Sunrise Core + Tabs + Mode Line integration:

(defun sunrise-tree-view (&optional desktop-mode)
  "Switch from Sunrise normal mode to Tree View.
If DESKTOP-MODE is non-nil, do not kill the current
buffer (necessary during `desktop-read')."
  (interactive)
  (sunrise-save-aspect
   (if desktop-mode
       (switch-to-buffer (generate-new-buffer "Sunrise Tree"))
     (sunrise-alternate-buffer
      (switch-to-buffer (generate-new-buffer "Sunrise Tree"))))
   (sunrise-tree-mode))
  (if (fboundp 'sunrise-modeline-setup)
      (sunrise-modeline-setup))
  (if (fboundp 'sunrise-tabs-engage)
      (sunrise-tabs-engage))
  (sunrise-force-passive-highlight))

(defun sunrise-tree-mouse-view (e)
  "Switch from Sunrise normal mode to Tree View using the mouse."
  (interactive "e")
  (sunrise-mouse-change-window e)
  (sunrise-tree-view))

(defun sunrise-tree-dismiss ()
  "Switch from Tree View to normal mode."
  (interactive)
  (let ((target (widget-get sunrise-tree-root :path))
        (sunrise-goto-dir-function nil))
    (sunrise-save-aspect
     (sunrise-alternate-buffer (sunrise-goto-dir target)))))

(defun sunrise-tree-mouse-dismiss (e)
  "Switch from Tree View to normal mode using the mouse."
  (interactive "e")
  (sunrise-mouse-change-window e)
  (sunrise-tree-dismiss))

;;; ============================================================================
;;; Sunrise Buttons integration:

(defvar sunrise-tree-button-commands
  '((sunrise-do-copy               . sunrise-tree-do-copy)
    (sunrise-do-clone              . sunrise-tree-do-clone)
    (sunrise-do-symlink            . sunrise-tree-do-symlink)
    (sunrise-do-relsymlink         . sunrise-tree-do-relsymlink)
    (sunrise-do-rename             . sunrise-tree-do-rename)
    (sunrise-do-delete             . sunrise-tree-do-delete)
    (sunrise-goto-dir              . sunrise-goto-dir)
    (sunrise-advertised-find-file  . sunrise-tree-advertised-find-file)
    (sunrise-quick-view            . sunrise-tree-advertised-find-file-other)
    (sunrise-dired-prev-subdir     . sunrise-tree-prev-subdir)
    (sunrise-change-window         . sunrise-change-window)
    (sunrise-synchronize-panes     . sunrise-synchronize-panes)
    (sunrise-sync                  . sunrise-tree-sync)
    (sunrise-beginning-of-buffer   . sunrise-tree-beginning-of-buffer)
    (sunrise-end-of-buffer         . sunrise-tree-end-of-buffer)
    (sunrise-term                  . sunrise-term)
    (sunrise-describe-mode         . sunrise-describe-mode)
    (sunrise-transpose-panes       . sunrise-transpose-panes)
    (revert-buffer            . revert-buffer)
    (sunrise-split-toggle          . sunrise-split-toggle)
    (sunrise-toggle-truncate-lines . sunrise-toggle-truncate-lines)
    (dired-create-directory   . sunrise-tree-create-directory)
    (sunrise-history-prev          . sunrise-history-prev)
    (sunrise-history-next          . sunrise-history-next))
  "Sunrise Buttons-to-Tree commands translation table.")

(defun sunrise-tree-buttons-command-adapter (command)
  "Execute the given buttons command in the current Sunrise Tree View pane.
If the command doesn't make sense in the current context, first
switch to normal mode, then execute."
  (let ((translation (cdr (assq command sunrise-tree-button-commands))))
    (if translation
        (call-interactively translation)
      (sunrise-tree-dismiss)
      (call-interactively command))))

;;; ============================================================================
;;; Tree Widget adapter functions:

(defun sunrise-tree-widget-forward (arg)
  "`widget-forward' replacement for use in `tree-widget-button-keymap'."
  (interactive "p")
  (if (eq major-mode 'sunrise-tree-mode)
      (sunrise-change-window)
    (widget-forward arg)))

(defun sunrise-tree-widget-button-press (pos &optional event)
  "`widget-button-press' replacement for use in `tree-widget-button-keymap'."
  (interactive "@d")
  (if (eq major-mode 'sunrise-tree-mode)
      (sunrise-tree-open-branch)
    (widget-button-press pos event)))

(defun sunrise-tree-widget-button-click (event)
  "`widget-button-click' replacement for use in `tree-widget-button-keymap'."
  (interactive "e")
  (unless (eq major-mode 'sunrise-tree-mode)
    (tree-widget-button-click event)))

;;; ============================================================================
;;; Sunrise Tree View keybindings:

(define-key sunrise-mode-map "\C-t "             'sunrise-tree-view)
(define-key sunrise-mode-map "\C-t\C-m"          'sunrise-tree-view)
(define-key sunrise-mode-map [(shift meta down)] 'sunrise-tree-view)
(define-key sunrise-mode-map [?\e down]          'sunrise-tree-view)

(define-key sunrise-tree-mode-map "\C-m"     'sunrise-tree-open-branch)
(define-key sunrise-tree-mode-map " "        'sunrise-tree-toggle-branch)
(define-key sunrise-tree-mode-map "\C-o"     'sunrise-tree-omit-mode)
(define-key sunrise-tree-mode-map "n"        'sunrise-tree-next-line)
(define-key sunrise-tree-mode-map "p"        'sunrise-tree-previous-line)
(define-key sunrise-tree-mode-map "\t"       'sunrise-change-window)
(define-key sunrise-tree-mode-map "g"        'sunrise-tree-refresh-branch)
(define-key sunrise-tree-mode-map "J"        'sunrise-tree-jump-up)
(define-key sunrise-tree-mode-map "j"        'sunrise-tree-build-new)
(define-key sunrise-tree-mode-map "f"        'sunrise-tree-advertised-find-file)
(define-key sunrise-tree-mode-map "v"        'sunrise-tree-advertised-find-file-other)
(define-key sunrise-tree-mode-map "o"        'sunrise-tree-advertised-find-file-other)
(define-key sunrise-tree-mode-map "\M-a"     'sunrise-tree-beginning-of-buffer)
(define-key sunrise-tree-mode-map "\M-e"     'sunrise-tree-end-of-buffer)
(define-key sunrise-tree-mode-map "\C-s"     'sunrise-tree-isearch-forward)
(define-key sunrise-tree-mode-map "\C-cs"    'sunrise-tree-sticky-isearch-forward)
(define-key sunrise-tree-mode-map "\C-r"     'sunrise-tree-isearch-backward)
(define-key sunrise-tree-mode-map "\C-cr"    'sunrise-tree-sticky-isearch-backward)
(define-key sunrise-tree-mode-map "\C-c\C-c" 'sunrise-tree-dismiss)
(define-key sunrise-tree-mode-map "\C-cf"    'sunrise-tree-focus-branch)
(define-key sunrise-tree-mode-map "\C-cb"    'sunrise-tree-blur-branch)
(define-key sunrise-tree-mode-map "\C-c\C-m" 'sunrise-tree-explode-branch)
(define-key sunrise-tree-mode-map "\C-t "    'sunrise-tree-dismiss)
(define-key sunrise-tree-mode-map "\C-t\C-m" 'sunrise-tree-dismiss)

(define-key sunrise-tree-mode-map "#" 'sunrise-tree-avfs-toggle)

(define-key sunrise-tree-mode-map [up]   'sunrise-tree-previous-line)
(define-key sunrise-tree-mode-map [down] 'sunrise-tree-next-line)

(define-key sunrise-tree-mode-map [right]     'sunrise-tree-open-branch)
(define-key sunrise-tree-mode-map [S-right]   'sunrise-tree-focus-branch)
(define-key sunrise-tree-mode-map [?\e right] 'sunrise-tree-focus-branch)
(define-key sunrise-tree-mode-map [C-right]   'sunrise-tree-explode-branch)
(define-key sunrise-tree-mode-map [3 right]   'sunrise-tree-explode-branch) ;; "\C-c <right>"

(define-key sunrise-tree-mode-map [left]     'sunrise-tree-collapse-branch)
(define-key sunrise-tree-mode-map [S-left]   'sunrise-tree-blur-branch)
(define-key sunrise-tree-mode-map [?\e left] 'sunrise-tree-blur-branch)
(define-key sunrise-tree-mode-map [C-left]   'sunrise-tree-collapse-branch)
(define-key sunrise-tree-mode-map [3-left]   'sunrise-tree-collapse-branch) ;; "\C-c <left>"
(define-key sunrise-tree-mode-map [delete]   'sunrise-tree-collapse-branch)

(define-key sunrise-tree-mode-map [next]              'sunrise-tree-scroll-up)
(define-key sunrise-tree-mode-map [prior]             'sunrise-tree-scroll-down)
(define-key sunrise-tree-mode-map [backspace]         'sunrise-tree-collapse-branch)
(define-key sunrise-tree-mode-map [C-return]          'sunrise-tree-explode-branch)
(define-key sunrise-tree-mode-map [S-return]          'sunrise-tree-focus-branch)
(define-key sunrise-tree-mode-map [M-return]          'sunrise-tree-blur-branch)
(define-key sunrise-tree-mode-map [(shift meta down)] 'sunrise-tree-dismiss)
(define-key sunrise-tree-mode-map [?\e down]          'sunrise-tree-dismiss)

(define-key sunrise-tree-mode-map "C" 'sunrise-tree-do-copy)
(define-key sunrise-tree-mode-map "K" 'sunrise-tree-do-clone)
(define-key sunrise-tree-mode-map "R" 'sunrise-tree-do-rename)
(define-key sunrise-tree-mode-map "D" 'sunrise-tree-do-delete)
(define-key sunrise-tree-mode-map "S" 'sunrise-tree-do-symlink)
(define-key sunrise-tree-mode-map "Y" 'sunrise-tree-do-relsymlink)
(define-key sunrise-tree-mode-map "y" 'sunrise-tree-show-files-info)
(define-key sunrise-tree-mode-map "+" 'sunrise-tree-create-directory)

(define-key sunrise-tree-mode-map "\C-c\C-z" 'sunrise-tree-sync)
(define-key sunrise-tree-mode-map "\C-q"     'sunrise-tree-sync)

(dotimes (n 10)
  (define-key sunrise-tree-mode-map (number-to-string n) 'digit-argument))

(mapc (lambda (x)
        (define-key sunrise-tree-mode-map x nil))
      '("Q" "F" "A" "k" "s" "\C-c/"))

(define-key sunrise-tree-mode-map [mouse-1] 'sunrise-tree-mouse-toggle-branch)
(define-key sunrise-tree-mode-map [mouse-2] (lambda ()
                                              (interactive)
                                              (call-interactively 'mouse-set-point)
                                              (sunrise-advertised-find-file)
                                              (sunrise-tree-update-cursor)))
(define-key sunrise-tree-mode-map [double-mouse-1] 'sunrise-tree-mouse-advertised-find-file)
(define-key sunrise-tree-mode-map [S-mouse-1] 'sunrise-tree-mouse-focus-branch)
(define-key sunrise-tree-mode-map [M-mouse-1] 'sunrise-tree-mouse-blur-branch)
(define-key sunrise-tree-mode-map [C-mouse-1] 'sunrise-tree-mouse-explode-branch)

(define-key sunrise-tree-mode-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key sunrise-tree-mode-map (kbd "<M-down-mouse-1>") 'ignore)
(define-key sunrise-tree-mode-map (kbd "<C-down-mouse-1>") 'ignore)

(define-key sunrise-mode-map (kbd "<M-S-down-mouse-1>") 'sunrise-tree-mouse-view)
(define-key sunrise-tree-mode-map (kbd "<M-S-down-mouse-1>") 'sunrise-tree-mouse-dismiss)

(define-key tree-widget-button-keymap "\t" 'sunrise-tree-widget-forward)
(define-key tree-widget-button-keymap "\C-m" 'sunrise-tree-widget-button-press)
(define-key tree-widget-button-keymap [down-mouse-1] 'sunrise-tree-widget-button-click)
(define-key tree-widget-button-keymap [mouse-1] 'tree-widget-button-click)
(define-key tree-widget-button-keymap [double-mouse-1] 'sunrise-tree-mouse-advertised-find-file)
(define-key tree-widget-button-keymap [C-mouse-1] 'sunrise-tree-mouse-explode-branch)

;;; ============================================================================
;;; Bootstrap:

(defun sunrise-tree-menu-init ()
  "Initialize the Sunrise Tree extension menu."

  (unless (lookup-key sunrise-mode-map [menu-bar Sunrise])
    (define-key sunrise-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Tree View")))
    (define-key sunrise-mode-map [menu-bar Sunrise tree-view]
      (cons "Tree View" menu-map))
    (define-key menu-map [enable-tree-view] '("enable" . sunrise-tree-view)))

  (define-key sunrise-tree-mode-map [menu-bar Sunrise]
    (cons "Sunrise" (make-sparse-keymap)))
  (let ((menu-map (make-sparse-keymap "Tree View")))
    (define-key sunrise-tree-mode-map [menu-bar Sunrise tree-view]
      (cons "Tree View" menu-map))
    (define-key menu-map [enable-tree-view] nil)
    (define-key menu-map [disable-tree-view] '("dismiss" . sunrise-tree-dismiss)))

  (remove-hook 'sunrise-start-hook 'sunrise-tree-menu-init)
  (unintern 'sunrise-tree-menu-init obarray))

(add-hook 'sunrise-start-hook 'sunrise-tree-menu-init)

(defun sunrise-tree-unload-function ()
  (sunrise-ad-disable "^sunrise-tree-"))

;;; ============================================================================
;;; Desktop support:

(defun sunrise-tree-desktop-save-buffer (desktop-dir)
  "Return additional data for saving a Sunrise tree buffer to a desktop file."
  (append `((root . ,(widget-get sunrise-tree-root :path))
            (open-paths . ,sunrise-tree-open-paths)
            (cursor ,sunrise-tree-cursor)
            (omit-mode . ,dired-omit-mode))
          (if (eq (current-buffer) sunrise-left-buffer) '((left . t)))
          (if (eq (current-buffer) sunrise-right-buffer) '((right . t)))
          (if (fboundp 'sunrise-tabs-desktop-save-buffer)
              (sunrise-tabs-desktop-save-buffer desktop-dir))))

(defun sunrise-tree-desktop-restore-buffer (desktop-buffer-file-name
                                            desktop-buffer-name
                                            desktop-buffer-misc)
  "Restore a Sunrise tree buffer from a description in a desktop file."
  (sunrise-tree-view t)
  (setq sunrise-tree-open-paths (cdr (assoc 'open-paths desktop-buffer-misc)))
  (setq dired-omit-mode (cdr (assoc 'omit-mode desktop-buffer-misc)))
  (setq sunrise-tree-cursor (cadr (assoc 'cursor desktop-buffer-misc)))
  (sunrise-tree-build (cdr (assoc 'root desktop-buffer-misc)))
  (sunrise-tree-update-cursor)
  (mapc (lambda (side)
          (when (cdr (assoc side desktop-buffer-misc))
            (set (sunrise-symbol side 'buffer) (current-buffer))
            (set (sunrise-symbol side 'directory) (cdr sunrise-tree-cursor))))
        '(left right))
  (if (fboundp 'sunrise-tabs-desktop-restore-buffer)
      (sunrise-tabs-desktop-restore-buffer desktop-buffer-file-name
                                           desktop-buffer-name
                                           desktop-buffer-misc)))

(add-to-list 'desktop-buffer-mode-handlers
             '(sunrise-tree-mode . sunrise-tree-desktop-restore-buffer))

(provide 'sunrise-tree)

;;; sunrise-tree.el ends here
