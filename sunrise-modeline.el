;;; sunrise-modeline.el --- Navigable mode line for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 10 Oct 2009
;; Version: 2
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, sunrise commander, modeline, path mode line
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

;; This extension modifies the format of the mode lines under the Sunrise
;; Commander panes so they display only the paths to the current directories
;; (or the tail if the whole path is too long) and a row of three small icons.
;; These icons are by default plain ASCII characters, but nicer semigraphical
;; versions (in Unicode) can also be used by customizing the variable
;; `sunrise-modeline-use-utf8-marks'.
;;
;; Here is the complete list of indicator icons (in ASCII and Unicode) and
;; their respective meanings:
;;
;;                     (ascii) (unicode)
;; 1. Pane modes:         *        ☼    Normal mode
;;                        !        ⚡    Editable Pane mode
;;                        @        ☯    Virtual Directory mode
;;                        T        ⚘    Tree View mode (with tree extension)
;;
;; 2. Navigation modes:   &        ⚓    Synchronized Navigation
;;                        $        ♻    Sticky Search
;;
;; 3. Transient states:   #        ♥    Contents snapshot available
;;
;; (if you can't see the icons on the right, don't use utf8 marks)

;; The regular mode line format remains available: press C-c m to toggle
;; between one format and the other.

;; The extension is provided as a minor mode, so you can enable / disable it
;; totally by issuing the command `sunrise-modeline'.

;; It was written on GNU Emacs 24 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-modeline) expression to your .emacs file
;; somewhere after the (require 'sunrise) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart
;; Emacs.

;; 4) Enjoy ;-)

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'desktop)
(require 'easymenu)

(require 'sunrise)

(defcustom sunrise-modeline-use-utf8-marks nil
  "Set to t to use fancy marks (using UTF-8 glyphs) in the mode line."
  :group 'sunrise
  :type 'boolean)

;; slot 0 -- pane modes:
(defconst sunrise-modeline-norm-mark '("*" . "☼"))
(defconst sunrise-modeline-edit-mark '("!" . "⚡"))
(defconst sunrise-modeline-virt-mark '("@" . "☯"))
(defconst sunrise-modeline-tree-mark '("T" . "⚘"))

;; slot 1 -- navigation modes:
(defconst sunrise-modeline-sync-mark '("&" . "⚓"))
(defconst sunrise-modeline-srch-mark '("$" . "♻"))

;; slot 2 -- transient states:
(defconst sunrise-modeline-bkup-mark '("#" . "♥"))

(defface sunrise-modeline-separator-face
  '((t (:height 0.3)))
  "Face of the string used to separate the state indicators from one another."
  :group 'sunrise)

(defconst sunrise-modeline-sep
  #(" " 0 1 (face sunrise-modeline-separator-face))
  "Sunrise Modeline separator character.")

;;; ==========================================================================
;;; Core functions:

(defvar sunrise-modeline-mark-map (make-sparse-keymap))
(define-key sunrise-modeline-mark-map [mode-line mouse-1]
  'sunrise-modeline-popup-menu)
(define-key sunrise-modeline-mark-map [mode-line mouse-2]
  'sunrise-modeline-popup-menu)

(defvar sunrise-modeline-path-map (make-sparse-keymap))
(define-key sunrise-modeline-path-map [mode-line mouse-1]
  'sunrise-modeline-navigate-path)
(define-key sunrise-modeline-path-map [mode-line mouse-2]
  'sunrise-modeline-navigate-path)

(defun sunrise-modeline-select-mark (mark &optional slot)
  "Select the right character for the given MARK in SLOT.

Depends on whether UTF-8 has been enabled in the mode line via
the variable `sunrise-modeline-use-utf8-marks'."
  (let ((select (if sunrise-modeline-use-utf8-marks #'cdr #'car))
        (slot (or slot 0)))
    (cl-case slot
      (0 (funcall select (cl-case mark
                           (edit sunrise-modeline-edit-mark)
                           (virt sunrise-modeline-virt-mark)
                           (tree sunrise-modeline-tree-mark)
                           (t sunrise-modeline-norm-mark))))
      (1 (cond ((or (memq 'sunrise-sticky-post-isearch isearch-mode-end-hook)
                    (memq 'sunrise-tree-post-isearch isearch-mode-end-hook))
                (funcall select sunrise-modeline-srch-mark))
               (sunrise-synchronized
                (funcall select sunrise-modeline-sync-mark))
               (t " ")))
      (t (if (buffer-live-p sunrise-backup-buffer)
             (funcall select sunrise-modeline-bkup-mark)
           " ")))))

(defun sunrise-modeline-select-mode (mark)
  "Assemble the indicators section on the left of the modeline.

The MARK argument is as for `sunrise-modeline-select-mark`."
  (concat sunrise-modeline-sep (sunrise-modeline-select-mark mark 0)
          sunrise-modeline-sep (sunrise-modeline-select-mark mark 1)
          sunrise-modeline-sep (sunrise-modeline-select-mark mark 2)
          sunrise-modeline-sep))

(defun sunrise-modeline-setup ()
  "Determine the mode indicator (character) to display in the mode line.
On success, sets the mode line format by calling
`sunrise-modeline-set'."
  (let ((mode nil))
    (cl-case major-mode
      (sunrise-mode
       (setq mode (sunrise-modeline-select-mode
                   (if buffer-read-only 'norm 'edit))))
      (sunrise-tree-mode
       (setq mode (sunrise-modeline-select-mode 'tree)))
      (sunrise-virtual-mode
       (setq mode (sunrise-modeline-select-mode 'virt))))
    (if mode (sunrise-modeline-set mode))))

(defun sunrise-modeline-set (mark)
  "Adjust the current mode line format.

Uses the mode indicator MARK and the path to the current
directory of the pane. Truncates the path if it is longer than
the available width of the pane."
  (let ((path (expand-file-name default-directory))
        (path-length (length default-directory))
        (max-length (- (window-width) 12)))
    (if (< max-length path-length)
        (setq path
              (concat "..." (substring path (- path-length max-length)))))
    (eval
     `(setq mode-line-format
            '("%[" ,(sunrise-modeline-mark mark) "%] "
              ,(sunrise-modeline-path path))))))

(defun sunrise-modeline-mark (marks-string)
  "Propertize MARKS-STRING for use in displaying the mode line indicators."
  (let ((mode-name "") (marks (split-string marks-string "|")))
    (setq mode-name
          (concat
           (cond ((member (sunrise-modeline-select-mark 'edit) marks)
                  "Editable Pane Mode")
                 ((member (sunrise-modeline-select-mark 'virt) marks)
                  "Virtual Directory Mode")
                 ((member (sunrise-modeline-select-mark 'tree) marks)
                  "Tree View Mode")
                 (t "Normal Mode"))
           (if sunrise-synchronized " | Synchronized Navigation" "")
           (if (or (memq 'sunrise-sticky-post-isearch isearch-mode-end-hook)
                   (memq 'sunrise-tree-post-isearch isearch-mode-end-hook))
               " | Sticky Search"
             "")
           (if (buffer-live-p sunrise-backup-buffer)
               " | Snapshot Available" "")))
    (propertize marks-string
                'font 'bold
                'mouse-face 'mode-line-highlight
                'help-echo (format "Sunrise Commander: %s" mode-name)
                'local-map sunrise-modeline-mark-map)))

(defun sunrise-modeline-path (path)
  "Propertize the string PATH for use in the mode line format.
PATH is the current directory in the file system."
  (propertize path
              'local-map sunrise-modeline-path-map
              'mouse-face 'mode-line-highlight
              'help-echo "Click to navigate directory path"
              'sunrise-selected-window sunrise-selected-window))

(defun sunrise-modeline-navigate-path ()
  "Handle click events occuring on the mode line directory path.
Analyzes all click events detected on the directory path and
modifies the current directory of the corresponding panel
accordingly."
  (interactive)
  (let* ((event (caddr (cddadr last-input-event)))
         (path (car event)) (pos (cdr event)) (slash) (levels))
    (or (eq sunrise-selected-window
            (get-text-property 0 'sunrise-selected-window path))
        (sunrise-change-window))
    (setq slash (string-match "/" path pos)
          levels (- (length (split-string (substring path slash) "/")) 2))
    (if (< 0 levels)
        (sunrise-dired-prev-subdir levels)
      (sunrise-beginning-of-buffer))))

;;; ==========================================================================
;;; Private interface:

(defvar sunrise-modeline)

(defun sunrise-modeline-refresh ()
  "Refresh the Sunrise Commander navigation modeline."
  (setq sunrise-modeline t)
  (sunrise-modeline-setup))

(defun sunrise-modeline-engage ()
  "Activate and enforce the navigation mode line format."
  (add-hook 'sunrise-refresh-hook 'sunrise-modeline-refresh)
  (sunrise-modeline-setup)
  (sunrise-in-other (sunrise-modeline-setup)))

(defun sunrise-modeline-disengage ()
  "De-activate the navigation mode line format, restoring the default one."
  (remove-hook 'sunrise-refresh-hook 'sunrise-modeline-refresh)
  (setq mode-line-format (default-value 'mode-line-format))
  (sunrise-in-other
   (setq mode-line-format (default-value 'mode-line-format))))

(defun sunrise-modeline-toggle (&optional force)
  "Toggle display of the navigation mode line format.

Todo: document the FORCE argument."
  (interactive)
  (cond ((and force (< 0 force)) (sunrise-modeline-engage))
        ((and force (> 0 force)) (sunrise-modeline-disengage))
        (t
         (if (eq mode-line-format (default-value 'mode-line-format))
             (sunrise-modeline-engage)
           (sunrise-modeline-disengage)))))

;;; ==========================================================================
;;; User interface:

(defvar sunrise-modeline-map (make-sparse-keymap))
(define-key sunrise-modeline-map "\C-cm" 'sunrise-modeline-toggle)

(define-minor-mode sunrise-modeline
  "Provide navigable mode line for the Sunrise Commander.
This is a minor mode that provides a single keybinding:

  C-c m .............. Toggle between navigation and default mode line formats

  To totally disable this extension do: M-x sunrise-modeline <RET>"

  nil (sunrise-modeline-select-mode 'norm) sunrise-modeline-map
  (unless (memq major-mode
                '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
    (setq sunrise-modeline nil)
    (error "Sorry, this mode can be used only within the Sunrise Commander"))
  (sunrise-modeline-toggle 1))

(defvar sunrise-modeline-menu
  (easy-menu-create-menu
   "Mode Line"
   '(["Toggle navigation mode line" sunrise-modeline-toggle t]
     ["Navigation mode line help"
      (lambda ()
        (interactive)
        (describe-function 'sunrise-modeline))])))

(defun sunrise-modeline-popup-menu ()
  "Show the Sunrise Modeline popup menu."
  (interactive)
  (popup-menu sunrise-modeline-menu))

;;; ==========================================================================
;;; Bootstrap:

(defun sunrise-modeline-menu-init ()
  "Initialize the Sunrise Mode Line extension menu."
  (unless (lookup-key sunrise-mode-map [menu-bar Sunrise])
    (define-key sunrise-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Mode Line")))
    (define-key sunrise-mode-map [menu-bar Sunrise mode-line]
      (cons "Mode Line" menu-map))
    (define-key menu-map [help]
      '("Help" . (lambda ()
                   (interactive)
                   (describe-function 'sunrise-modeline))))
    (define-key menu-map [disable]
      '("Toggle" . sunrise-modeline-toggle))))

(defun sunrise-modeline-start-once ()
  "Bootstrap the Sunrise Commander modeline extension.

Run on the first execution of the Sunrise Commander after module
installation."
  (sunrise-modeline t)
  (sunrise-modeline-menu-init)
  (remove-hook 'sunrise-start-hook 'sunrise-modeline-start-once)
  (unintern 'sunrise-modeline-menu-init obarray)
  (unintern 'sunrise-modeline-start-once obarray))
(add-hook 'sunrise-start-hook 'sunrise-modeline-start-once)

;;; ==========================================================================
;;; Desktop support:

(add-to-list 'desktop-minor-mode-table '(sunrise-modeline nil))

(defun sunrise-modeline-desktop-restore-function (&rest _)
  "Call this instead of `sunrise-modeline' when restoring a desktop."
  (sunrise-modeline-refresh))

(add-to-list 'desktop-minor-mode-handlers
             '(sunrise-modeline . sunrise-modeline-desktop-restore-function))

(provide 'sunrise-modeline)

;;; sunrise-modeline.el ends here
