;; sunrise-commander.el --- Two-pane file manager for Emacs based on Dired and
;; inspired by MC.

;; Copyright (C) 2007 José Alfredo Romero L.

;; Author: José Alfredo Romero L. <joseito@poczta.onet.pl>
;; Keywords: Sunrise Commander Midnight

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here is another two-pane mc emulation layer for emacs. It's built on top of
;; Dired and takes advantage of all its features, offering at the same time the
;; double pane interface I'd been missing so badly since I started using
;; regularly emacs (for everything!). I tried both Ilya Zakharevich's nc.el and
;; Kevin Burton's mc.el, but none of them was what I was looking for (though
;; mc.el was near the ideal).

;; A lot of this code has been shamelessly copied from Kevin's mc.el and only
;; slightly modified. Another part of it - the code for recursive file copying
;; and renaming - was adapted from the dired extensions written by Kurt Nørmarks
;; for LAML (http://www.cs.aau.dk/~normark/scheme/distribution/laml/).

;; I have added to the mix several useful functions:

;; * Sunrise is implemented as a derived major mode confined inside the pane
;; buffers, so its buffers and dired ones can live together without easymenu or
;; viper to avoid key binding collisions.

;; * It automatically closes unused buffers and tries to never keep open more
;; than the one or two used to display the panes.

;; * Each pane has its own history ring: press M-y / M-u for moving backwards /
;; forwards in the history of directories.

;; * Press C-= for "smart" file comparison using ediff. It compares together the
;; first two files marked on each pane or, if no files have been marked, it
;; assumes that the second pane contains a file with the same name as the
;; selected one and tries to compare these two. You can also mark whole lists of
;; files to be compared and then just press C-= for comparing the next pair.

;; * Press = for fast "smart" file comparison -- like above, but using regular
;; * diff.

;; * Press C-M-= for directory comparison (only by date and/or size, by now).

;; * Press C-x t to open a bash terminal into the current pane's directory.

;; * Press M-t to swap the panes.

;; It doesn't even try to look like MC, so the help window is gone (you're in
;; emacs, so you know your bindings, right?).

;; This is version 1 $Rev$ of the Sunrise Commander.
;; Please note that it was written and tested only on GNU Emacs version 23 (from
;; CVS). I *am* aware that there are several functions (including, alas, file
;; and directory comparison) that simply will not work on GNU Emacs 21, but
;; unfortunately I do not have the time to port them back. I don't know either
;; if it will work at all on XEmacs, so try at your own risk. All contributions
;; and/or bug reports will be very welcome.

;;; Installation and Usage:

;; 1) Put this file somewhere in your emacs load-path.

;; 2) Add a (require 'sunrise-commander) to your .emacs file.

;; 3) If you want the function keys bound to the usual MC commands (F5 for copy,
;; F6 for rename, and so on) add: (sunrise-mc-keys) after the "require" line
;; (IMHO these bindings are not optimal for emacs, but I'm including them
;; because some MC power users may have them too deeply embedded in their spinal
;; cord)

;; 4) Evaluate the new lines, or reload your .emacs file, or restart emacs.

;; 5) Type M-x sunrise to invoke the Sunrise Commander (or much better: bind the
;; function to your favorite key combination). Type C-h m for information on
;; available key bindings.

;; 6) Enjoy :)

;;; Code:

(require 'dired)
(require 'font-lock)
(require 'browse-url)
(setq dired-dwim-target t
      dired-recursive-deletes 'top
      dired-listing-switches "-alp")

(eval-when-compile (require 'term))

(defface sr-directory-face '((t (:bold t)))
  "Face used to highlight directories."
  :group 'sunrise)

(defface sr-symlink-face '((t (:italic t)))
  "Face used to highlight symbolic links."
  :group 'sunrise)

(defface sr-symlink-directory-face '((t (:italic t)))
  "Face used to highlight symbolic directory links."
  :group 'sunrise)

(defface sr-window-selected-face '((t (:background "#ace6ac" :foreground "yellow" :height 140)))
  "Face used to show a selected window"
  :group 'sunrise)

(defface sr-window-not-selected-face '((t (:background "white" :foreground "lightgray" :height 140)))
  "Face used to show an unselected window"
  :group 'sunrise)

(defvar sr-restore-buffer nil
  "Buffer to restore when sr is quit.")

(defvar sr-prior-window-configuration nil
  "Window configuration before sr was started.")

(defvar sr-running nil "True when sr commander mode is running.")

(defvar sr-current-window-overlay nil
  "Holds the current overlay which marks the current dired buffer.")

(defvar sr-left-directory "~/"
  "Dired directory for the left window.  See variable `dired-directory'.")

(defvar sr-right-directory "~/"
  "Dired directory for the right window.  See variable `dired-directory'.")

(defvar sr-other-directory "~/"
  "Dired directory in the window that is currently *not* selected")

(defvar sr-left-window nil
  "The left window of dired.")

(defvar sr-right-window nil
  "The right window of dired.")

(defvar sr-selected-window 'left
  "The window to select when sr starts up.")

(defvar sr-ediff-on nil
  "Flag that indicates whether an ediff is being done by SR")

(defvar sr-dired-directory ""
  "Directory inside which sr-mode is currently active")

(defcustom sr-window-split-style 'horizontal
  "The current window split configuration.  May be either 'horizontal or 'vertical."
  :group 'sunrise
  :type '(choice
          (const horizontal)
          (const vertical)))

(defvar sr-start-message
  "Been coding all night? Enjoy the Sunrise! (or press q to quit)"
  "Message to display when `sr' is started.")

;;; ============================================================================
;;; This is the core of Sunrise: the main idea is to apply sr-mode only inside
;;; Sunrise buffers while keeping all of dired-mode untouched.

(define-derived-mode sr-mode dired-mode "Sunrise Commander"
  "Two-pane file manager for Emacs based on Dired and inspired by MC. The
  following keybindings are available:

        C-x g ......... go to directory
        U ............. go to parent directory
        M-y ........... go to previous directory in history
        M-u ........... go to next directory in history
        M-a ........... go to beginning of current directory
        M-e ........... go to end of current directory
        Tab ........... go to other pane

        C-c C-s ....... toggle pane orientation (vertical/horizontal)
        M-t ........... transpose panes
        M-o ........... synchronize panes
        C-o ........... show/hide hidden files (requires dired-omit-mode)
        b ............. browse current directory using w3m
        g ............. refresh pane

        C-= ........... smart compare files (ediff)
        = ............. fast smart compare files (plain diff)
        C-M-= ......... compare directories

        Return ........ visit selected file
        + ............. create new directory
        C ............. copy marked (or current) files and directories
        c ............. copy (using traditional dired-do-copy)
        R ............. rename marked (or current) files and directories
        r ............. rename (using traditional dired-do-rename)
        D ............. delete marked (or current) files and directories

        C-x t ......... open shell (bash) into current directory
        q ............. quit Sunrise Commander

Additionally, if you activate the mc-compatible keybindings (by invoking the
sunrise-mc-keys function) you'll get the following ones:

        F2 ............ go to directory
        F3 ............ visit selected file
        F4 ............ visit selected file
        F5 ............ copy marked (or current) files and directories
        F6 ............ rename marked (or current) files and directories
        F7 ............ create new directory
        F8 ............ delete marked (or current) files and directories
        F10 ........... quit Sunrise Commander
        Insert ........ mark file
        C-PgUp ........ go to parent directory

Also any dired keybinding (not overridden by any of the above) can be used in
Sunrise, like G for changing group, M for changing mode and so on."
  :group 'sunrise
  (set-keymap-parent sr-mode-map dired-mode-map))

(defmacro sr-within (dir form)
  (list 'progn
        (list 'setq 'sr-dired-directory
              (list 'file-name-as-directory
                    (list 'abbreviate-file-name dir)))
        (list 'ad-activate (list 'quote 'dired-find-buffer-nocreate))
        form
        (list 'ad-deactivate (list 'quote 'dired-find-buffer-nocreate))
        (list 'setq 'sr-dired-directory "")))

(defun sr-dired-mode ()
  (if (string= sr-dired-directory dired-directory)
      (sr-mode)
    (message (concat "Sunrise: " sr-dired-directory " != " dired-directory))))
(add-hook 'dired-before-readin-hook 'sr-dired-mode)

(defadvice dired-find-buffer-nocreate
  (before sr-advice-findbuffer (dirname &optional mode))
  (if (string= sr-dired-directory dirname)
      (setq mode 'sr-mode)))

(defadvice bookmark-jump
  (around sr-advice-bookmark-jump (str))
  (if (equalp major-mode 'sr-mode)
      (progn
        (setq sr-dired-directory (bookmark-get-filename str))
        ad-do-it
        (setq sr-dired-directory "")
        (hl-line-mode)
        (sr-highlight))
    ad-do-it))
(list 'ad-activate (quote 'bookmark-jump))

;;; ============================================================================
;;; Sunrise Commander keybindings:

(define-key sr-mode-map [return]             'sr-advertised-find-file)
(define-key sr-mode-map "\C-xg"              'sr-goto-dir)
(define-key sr-mode-map "U"                  'sr-dired-prev-subdir)
(define-key sr-mode-map "\M-y"               'sr-history-prev)
(define-key sr-mode-map "\M-u"               'sr-history-next)
(define-key sr-mode-map [tab]                'sr-change-window)
(define-key sr-mode-map "\M-a"               'sr-beginning-of-buffer)
(define-key sr-mode-map "\M-e"               'sr-end-of-buffer)
(define-key sr-mode-map [?\C-c?\C-s]         'sr-split-toggle)
(define-key sr-mode-map "\M-t"               'sr-transpose-panes)
(define-key sr-mode-map "\M-o"               'sr-synchronize-panes)
(define-key sr-mode-map "\C-o"               'dired-omit-mode)
(define-key sr-mode-map "b"                  'sr-browse)
(define-key sr-mode-map "g"                  'sr-revert-buffer)

(define-key sr-mode-map "C"                  'sr-do-copy)
(define-key sr-mode-map "c"                  'dired-do-copy)
(define-key sr-mode-map "R"                  'sr-do-rename)
(define-key sr-mode-map "r"                  'dired-do-rename)
(define-key sr-mode-map "\C-xt"              'sr-bash)

(define-key sr-mode-map "="                  'sr-diff)
(define-key sr-mode-map [(control ?\=)]      'sr-ediff)
(define-key sr-mode-map [(control meta ?\=)] 'sr-compare-dirs)

(define-key sr-mode-map "q"                  'keyboard-escape-quit)

(defun sunrise-mc-keys ()
  "Binds the function keys F2 to F10 the traditional MC way"
  (interactive)
  (define-key sr-mode-map [(f2)]            'sr-goto-dir)
  (define-key sr-mode-map [(f3)]            'sr-advertised-find-file)
  (define-key sr-mode-map [(f4)]            'sr-advertised-find-file)
  (define-key sr-mode-map [(f5)]            'sr-do-copy)
  (define-key sr-mode-map [(f6)]            'sr-do-rename)
  (define-key sr-mode-map [(f7)]            'dired-create-directory)
  (define-key sr-mode-map [(f8)]            'dired-do-delete)
  (define-key sr-mode-map [(f10)]           'keyboard-escape-quit)
  (define-key sr-mode-map [(insert)]        'dired-mark)
  (define-key sr-mode-map [(control prior)] 'sr-dired-prev-subdir))

;;; ============================================================================
;;; Initialization and finalization functions:

(defun sunrise(&optional left-directory right-directory) 
  "Starts the Sunrise Commander.  If the param `left-directory' is given the
left window will display this directory (the same for `right-directory').
Specifying nil for any of these values uses the default, ie. home."
  (interactive)
  (message "Starting Sunrise Commander...")
  
  (if (not sr-running)
      (progn
        (catch 'exit

          (if left-directory
              (setq sr-left-directory left-directory))

          (if right-directory
              (setq sr-right-directory right-directory))
          
          (setq sr-running t)
          
          (setq sr-restore-buffer (current-buffer))
          
          (setq sr-prior-window-configuration (current-window-configuration))
          
          (sr-setup-windows)

          (message sr-start-message)
          (recursive-edit))
        (sr-quit))
    (progn
      (sr-quit)
      (message "All life leaps out to greet the light...")
      (exit-recursive-edit))))

(defun sunrise-cd()
  "Run SR but give it the current directory to use."
  (interactive)
  (let((left-directory default-directory))
    (sunrise left-directory)))

(defun sr-dired(directory)
  "Visits the given directory (or file) in sr-mode"
  (interactive
   (list 
    (read-file-name "Change directory (file or pattern): " nil nil nil)))

  (if (and (file-exists-p directory)
           (file-readable-p directory))
      (if (file-directory-p directory)
          (progn
            (sr-goto-dir directory)
            (sr-setup-interface))
        (progn
          (sr-quit)
          (exit-recursive-edit)))))

(defun sr-setup-interface()
  "Sets up the logical SR interface"
  (interactive)
  (if (equal major-mode 'sr-mode)
      (progn
        (sr-highlight)
        (font-lock-mode 1)

        ;;update the mode with the current directory

        (let(basic-line-format)

          (setq basic-line-format (concat " " (expand-file-name dired-directory)))
        
          (setq mode-line-format basic-line-format))

        ;;if the point is below the .. directory... this is OK.  else set it to
        ;;the correct dir.

        (let(first-logic-point)
          (save-excursion
            (if (re-search-forward "\\.\\./$" nil t)
                (setq first-logic-point (match-beginning 0))))
          
          ;;if the current point is less than the idea point... first-logic-point....
          (if (and first-logic-point
                   (< (point) first-logic-point))
              (goto-char first-logic-point))))))

;;; ============================================================================
;;; Window management functions:

(defun sr-setup-windows()
  "Setup the SR window configuration (two windows in sr-mode.)"
  
    ;;get rid of other windows if they exist.
  (delete-other-windows)

  ;;now create the bottom window
  (split-window (selected-window) (* 2 (/ (window-height) 3)))
          
  (if (equal sr-window-split-style 'horizontal)
      (split-window-horizontally)
    (if (equal sr-window-split-style 'vertical)
        (split-window-vertically)
      (error "Don't know how to split this window: %s" sr-window-split-style)))
  
  ;;setup dired in both windows
  (sr-dired sr-left-directory)
  (setq sr-left-window (selected-window))
          
  (other-window 1)
  (sr-dired sr-right-directory)
  (setq sr-right-window (selected-window))

  ;;select the correct window
  (sr-select-window sr-selected-window))

(defun sr-select-window(window)
  "Select/highlight the given sr window (right or left)."
  (hl-line-mode 0)
  (if (string= (symbol-name window) "left")
      (progn
        (select-window sr-left-window)
        (setq sr-selected-window 'left))
    (progn
      (select-window sr-right-window)
      (setq sr-selected-window 'right)))
  (hl-line-mode 1)
  (sr-highlight))

(defun sr-highlight()
  "Highlight the current buffer, destroying the previous buffer highlight if
  necessary."

  ;;update the last overlay
  (if sr-current-window-overlay
      (overlay-put sr-current-window-overlay 'face 'sr-window-not-selected-face))
  
  (save-excursion
    (let(begin end)

      ;;determine begining and end
      (goto-char (point-min))
      (search-forward "/" nil t)
      (setq begin (1- (point)))

      (search-forward ":" nil t)
      (setq end (1- (point)))

      ;;setup overlay
      (setq sr-current-window-overlay (make-overlay begin end))

      (overlay-put sr-current-window-overlay 'face 'sr-window-selected-face)

      (overlay-put sr-current-window-overlay 'window (selected-window)))))

(defun sr-quit()
  "Quit SR and restore emacs to previous operation."
  (interactive)
  (if sr-running
      (progn
        (setq sr-running nil)
        (sr-save-directories)

        ;;restore previous window setup
        (delete-other-windows)
        (set-window-configuration sr-prior-window-configuration)
        (if (buffer-live-p sr-restore-buffer)
            (set-buffer sr-restore-buffer))

        ;;NOTE: never exit the recursive edit here.  functions should do this
        ;;themselves
        (toggle-read-only -1))))

(defun sr-save-directories()
  "Save the current directories in the sr buffer to use the next time sr starts
  up."
  ;;update directory variables..
  (if (window-live-p sr-left-window)
      (progn
        (set-buffer (window-buffer sr-left-window))
        (if (equal major-mode 'sr-mode)
            (setq sr-left-directory dired-directory))
        (bury-buffer)))

  (if (window-live-p sr-right-window)
      (progn
        (set-buffer (window-buffer sr-right-window))
        (if (equal major-mode 'sr-mode)
            (setq sr-right-directory dired-directory))
        (bury-buffer))))

;;; ============================================================================
;;; File system navigation functions:

(defun sr-advertised-find-file()
  "Call dired-advertised-find-file but also perform additional actions"
  (interactive)

  ;;if the current line is not a directory ~exit..
  (save-excursion

    (let(filename)
      (setq filename (expand-file-name (dired-get-filename nil t)))

      (if filename
          (if (file-directory-p filename)
              (progn
                (hl-line-mode 0)
                (sr-within filename
                            (if (string= sr-other-directory dired-directory)
                                (dired-advertised-find-file)
                              (dired-find-alternate-file)))
                (sr-history-push dired-directory)
                (sr-highlight)
                (hl-line-mode 1))
            (progn
              (sr-quit)
              (find-file filename)
              (exit-recursive-edit)))))))

(defun sr-goto-dir (dir)
  "Changes the current directory in the active pane to the given one"
  (interactive
   (list 
    (read-file-name "Change directory (file or pattern): " nil nil nil)))
  (hl-line-mode 0)
  (sr-within dir
              (if (or (not dired-directory)
                      (string= sr-other-directory dired-directory))
                  (dired dir)
                (find-alternate-file dir)))
  (sr-history-push dired-directory)
  (sr-highlight)
  (sr-beginning-of-buffer)
  (hl-line-mode 1))

(defun sr-dired-prev-subdir()
  "Go to the previous subdirectory."
  (interactive)
  (if (not (string= dired-directory "/"))
      (sr-goto-dir (expand-file-name ".."))
    (error "Already at root")))

(defun sr-history-push (element)
  "Pushes a new path into the history ring of the current pane"
  (let* (
         (hist (get sr-selected-window 'history))
         (len (length hist))
        )
    (if (>= len 20)
        (nbutlast hist (- len 19)))
    (if (not (string= element (car hist)))
        (progn
          (push element hist)
          (put sr-selected-window 'history hist)))))

(defun sr-history-next ()
  "Changes the current directory to the next one (if any) in the history list of
the current pane"
  (interactive)
  (let (
        (hist (get sr-selected-window 'history))
        (item nil)
       )
    (while (and (> (length hist) 0)
                (or (not item)
                    (not (file-directory-p item))))
      (setq item (car (last hist)))
      (nbutlast hist))
    (if item
        (progn
          (push item hist)
          (put sr-selected-window 'history hist)
          (sr-goto-dir item)))))

(defun sr-history-prev ()
  "Changes the current directory to the previous one (if any) in the history
list of the current pane"
  (interactive)
  (let (
        (hist (get sr-selected-window 'history))
        (item nil)
       )
    (while (and (> (length hist) 0)
                (or (not item)
                    (not (file-directory-p (car hist)))))
      (setq item (pop hist)))
    (if item
        (progn
          (nconc hist (list item))
          (put sr-selected-window 'history hist)
          (sr-goto-dir (car hist))))))

;;; ============================================================================
;;; SR interface interaction functions:

(defun sr-change-window()
  "Change to the other sr buffer"
  (interactive)
  (setq sr-other-directory dired-directory)
  ;; this is much smarter than using (other-window) because we don't want to
  ;; include windows that were created accidentally.
  (if (equal (selected-window) sr-right-window)
      (sr-select-window 'left)
    (sr-select-window 'right)))

(defun sr-beginning-of-buffer()
  "Go to the first directory/file in dired."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "\\.\\./$" nil t)
      (progn
        (goto-char (match-beginning 0))
        (dired-next-line 1))))

(defun sr-end-of-buffer()
  "Go to the last directory/file in dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun sr-split-toggle()
  "If sr is currently configured for vertical splitting... change it to
horizontal and vice-versa."
  (interactive)
  (if (equal sr-window-split-style 'horizontal)
      (sr-split-setup 'vertical)
    (sr-split-setup 'horizontal)))

(defun sr-split-setup(split-type)
  (setq sr-window-split-style split-type)
  (if sr-running
      (progn
        (delete-other-windows)
        (sr-setup-windows)))
  (redraw-display)
  (message "Split is now %s." (symbol-name split-type)))

(defun sr-transpose-panes ()
  "Changes the order of the panes"
  (interactive)
  (if (not (equal dired-directory sr-other-directory))
           (let ((this dired-directory))
             (sr-dired sr-other-directory)
             (sr-change-window)
             (sr-dired this)
             (sr-change-window))
      nil))

(defun sr-synchronize-panes ()
  "Changes the directory in the other pane to that in the current one"
  (interactive)
  (let ((target dired-directory))
    (sr-change-window)
    (sr-goto-dir target)
    (sr-change-window)))

(defun sr-browse ()
  "Browse the directory/file on the current line."
  (interactive)
  (let(filename)
    (setq filename (dired-get-filename))
    (if filename
        (let(url)
          (setq url (concat "file://" filename))
          (message "Browsing %s " url)
          (browse-url url)))))

(defun sr-revert-buffer ()
  "Refreshes the current pane"
  (interactive)
  (revert-buffer)
  (sr-highlight))

;;; ============================================================================
;;; File manipulation functions:

(defun sr-do-copy ()
  "Copies recursively selected files and directories from one pane to the other"
  (interactive)
  (save-excursion
    (let* (
           (selected-files (dired-get-marked-files nil))
           (files-count (length selected-files))
           (files-count-str (int-to-string files-count))
          )
      (if (> files-count 0)
          (if (string= dired-directory sr-other-directory)
              (dired-do-copy)
            (if (y-or-n-p (concat "Copy " files-count-str
                                  " files to " sr-other-directory "? "))
                (progn
                  (dired-unmark-all-marks)
                  (sr-change-window)
                  (sr-copy-files selected-files dired-directory)
                  (sr-revert-buffer)
                  (sr-change-window)
                  (message (concat "Done: "
                                   (int-to-string (length selected-files))
                                   " file(s) dispatched")))))
        
        (message "Empty selection. Nothing done.")))))

(defun sr-do-rename ()
  "Moves recursively selected files and directories from one pane to the other"
  (interactive)
  (save-excursion
    (let* (
           (selected-files (dired-get-marked-files nil))
           (files-count (length selected-files))
           (files-count-str (int-to-string files-count))
          )
      (if (> files-count 0)
          (if (string= dired-directory sr-other-directory)
              (dired-do-rename)
            (if (y-or-n-p (concat "Move " files-count-str
                                  " files to " sr-other-directory "? "))
                (progn
                  (dired-unmark-all-marks)
                  (sr-change-window)
                  (sr-move-files selected-files dired-directory)
                  (sr-revert-buffer)
                  (sr-change-window)
                  (sr-revert-buffer)
                  (message (concat "Done: "
                                   (int-to-string (length selected-files))
                                   " file(s) dispatched")))))
        
        (message "Empty selection. Nothing done.")))))

(defun sr-copy-files (file-path-list target-dir &optional do-overwrite)
  "Copies all files in file-path-list (list of full paths) to target dir"
  (mapcar
   (function 
    (lambda (f)
	(cond ((file-directory-p f) 
               (let* (
                      (name (file-name-nondirectory f))
                      (initial-path (file-name-directory f))
                     )
                 (sr-copy-directory initial-path name target-dir do-overwrite)))

	      ((file-regular-p f)
               (let* (
                      (name (sr-file-name-proper (file-name-nondirectory f)))
          	      (ext (file-name-extension f))
	              (name-ext (concat name (if ext (concat "." ext) "")))
                      (target-file (concat target-dir name-ext))
                     )
                 (message (concat f " => " target-file))
                 (if (file-exists-p target-file)
                     (if (or (eq do-overwrite 'ALWAYS)
                             (setq do-overwrite (ask-overwrite target-file)))
                         (copy-file f target-file t t))
                   (copy-file f target-file t t))))

	      (t nil))))
   file-path-list))

(defun sr-copy-directory (in-dir d to-dir do-overwrite)
  "Copies directory d in in-dir to to-dir, and recursively, all files too.
indir/d => to-dir/d"
  (if (not (sr-overlapping-paths-p (concat in-dir d) to-dir))
      (progn
        (if (string= "" d)
            (setq to-dir (concat to-dir (sr-directory-name-proper in-dir))))
        (if (not (file-exists-p (concat to-dir d))) ; directory in-dir/d does not exist
	    (make-directory (concat to-dir d))) ; makes d in to-dir
	(let* (
               (files-in-d (append (sr-list-of-files (concat in-dir d))
                                   (sr-list-of-directories (concat in-dir d))))
	       (file-paths-in-d 
		(mapcar (lambda (f) (concat in-dir d f)) files-in-d))
	       )
	  (sr-copy-files file-paths-in-d (concat to-dir d) do-overwrite)))
    (error "You cannot copy a directory into itself or one of its subdirectories")))

(defun sr-move-files (file-path-list target-dir &optional do-overwrite)
  "Moves all files in file-path-list (list of full paths) to target dir"
  (mapcar
   (function 
    (lambda (f)
      (cond ((file-directory-p f)
             (let* (
                    (name (sr-file-name-proper (file-name-nondirectory f)))
                    (target-subdir target-dir)
                    (initial-path (file-name-directory f))
                   )
               (if (string= "" name)
                   (setq target-subdir (concat target-dir (sr-directory-name-proper f))))
               (if (file-exists-p target-subdir)
                   (if (or (eq do-overwrite 'ALWAYS)
                           (setq do-overwrite (ask-overwrite target-subdir)))
                       (sr-move-directory initial-path name target-dir do-overwrite))
                 (sr-move-directory initial-path name target-dir do-overwrite))))

            ((file-regular-p f)
             (let* (
                    (name (sr-file-name-proper (file-name-nondirectory f)))
                    (ext (file-name-extension f))
                    (name-ext (concat name (if ext (concat "." ext) "")))
                    (target-file (concat target-dir name-ext))
                   )
               (message (concat f " => " target-file))
               (if (file-exists-p target-file)
                   (if (or (eq do-overwrite 'ALWAYS)
                           (setq do-overwrite (ask-overwrite target-file)))
                       (rename-file f target-file t))
                 (rename-file f target-file t))))

            (t nil))))
   file-path-list))

(defun sr-move-directory (in-dir d to-dir do-overwrite)
  "Copies recursively the given directory d from in-dir to to-dir, then removes
the original one"
  (sr-copy-directory in-dir d to-dir do-overwrite)
  (let ((delete-dir (concat in-dir d)))
    (dired-delete-file delete-dir 'always)))

(defun ask-overwrite (file-name)
  "Asks whether to overwrite a given file."
  (y-n-or-a-p (concat "File " file-name " exists. OK to overwrite? ")))

(defun y-n-or-a-p (prompt)
  "Prompts for an answer to an alternative of the type y/n/a (where 'a' stands
for 'always') and returns t if the answer is 'y', nil if the answer is 'n' or
the symbol ALWAYS."
  (setq prompt (concat prompt "([y]es, [n]o or [a]lways)"))
  (let ((resp -1))
    (while (not (memq resp '(?y ?Y ?n ?N ?a ?A)))
      (setq resp (read-event prompt))
      (setq prompt "Please answer [y]es, [n]o or [a]lways "))
    (if (>= resp 97)
        (setq resp (- resp 32)))
    (cond ((eq resp 89) t)
          ((eq resp 65) 'ALWAYS)
          (t nil))))

(defun sr-overlapping-paths-p (dir1 dir2)
  "Determines whether the directory dir2 is located inside the directory dir1"
  (if (>= (length dir2) (length dir1))
      (equal (substring dir2 0 (length dir1)) dir1)
      nil))

(defun sr-list-of-directories (dir)
 "Return a list of directories in DIR. Each entry in the list is a string. The
list does not include the current directory and the parent directory."
 (sr-filter (function (lambda (x) (not (or (equal x ".") (equal x "..")))))
  (sr-filter
   (function (lambda (x)
	       (file-directory-p (concat dir x))))
   (directory-files dir))))

(defun sr-list-of-files (dir)
  "Return a list of regular files in DIR. Each entry in the list is a string."
  (sr-filter
   (function (lambda (x)
	       (file-regular-p (concat dir x))))
   (directory-files dir)))

(defun sr-filter (p x)
"Filter takes two arguments: a predicate P and a list X.
Return the elements of the list X that satisfy the predicate P"
;;Non-recursive version of sr-filter. Overwrites the previous recursive one.
  (let ((res-list nil))
    (while x
      (if (apply p (list (car x)))
          (setq res-list (cons (car x) res-list)))
      (setq x (cdr x)))
    (reverse res-list)))

(defun sr-find-last-point (str)
  "Return the position of the last point in the string str.  Do not allow to
pass '/' while looking for the point. If no point is found under these
conditions, return nil."
  (let ((idx (- (length str) 1)))
    (while (and (>= idx 0) (not (eq (aref str idx) ?.)) (not (eq (aref str idx) ?/))) (setq idx (- idx 1)))
    (if (and (>= idx 0) (eq (aref str idx) ?.)) idx nil)))

(defun sr-file-name-proper (filename)
  "Takes as input a filename, without directory path.
Return the file name proper. That is, the file name without the file extension.
If no dot is found, return filename.  Use the native Emacs Lisp function
file-name-nondirectory to access the the proper file name and the extension of a
file path.  Use the native Emacs Lisp function file-name-directory to access the
directory path of a file path."
  (let ((point-idx (sr-find-last-point filename)))
    (if point-idx 
        (substring filename 0 point-idx)
        filename)))

(defun sr-directory-name-proper (file-path)
  "Takes as input an absolute or relative, forward slash terminated path to a directory.
Return the proper name of the directory, without initial path.  The remaining
part of file-path can be accessed by the function parent-directory."
  (let (
        (file-path-1 (substring file-path 0 (- (length file-path) 1)))
        (lastchar (substring file-path (- (length file-path) 1)))
        )
    (concat (file-name-nondirectory file-path-1) lastchar)))

;;; ============================================================================
;;; Directory and file comparison functions:

(defun sr-compare-dirs()
  "Compares paned directories between themselves"
  (interactive)
  (dired-compare-directories sr-other-directory (ask-compare-dirs-predicate)))

(defun ask-compare-dirs-predicate ()
  (let (
        (resp -1)
        (prompt "Compare by (d)ate, (s)ize or (a)ll? ")
       )
    (while (not (memq resp '(?d ?D ?s ?S ?a ?A)))
      (setq resp (read-event prompt))
      (setq prompt "Please select: Compare by (d)ate, (s)ize or (a)ll? "))
    (if (>= resp 97)
        (setq resp (- resp 32)))
    (cond ((eq resp 68)
           (list 'not (list '= 'mtime1 'mtime2)))
          ((eq resp 83)
           (list 'not (list '= 'size1 'size2)))
          (t
           (list 'or
                 (list 'not (list '= 'mtime1 'mtime2))
                 (list 'not (list '= 'size1 'size2)))))))

(defun sr-diff ()
  "Runs diff on the top two marked files in both panes"
  (interactive)
  (eval (sr-diff-form 'diff)))

(defun sr-ediff ()
  "Runs ediff on the two top marked files in both panes"
  (interactive)
  (let ((form (sr-diff-form 'ediff)))
    (setq sr-ediff-on t)
    (sr-save-directories)
    (eval form)))

(add-hook 'ediff-quit-hook
          (lambda ()
            (if sr-ediff-on
                (progn
                  (setq sr-ediff-on nil)
                  (delete-other-windows)
                  (if (buffer-live-p sr-restore-buffer)
                      (switch-to-buffer sr-restore-buffer))
                  (sr-setup-windows))
              nil)))

(defun sr-diff-form (fun)
  "Determines the arguments to be passed to the diff function and returns the
  form to evaluate to perform the comparison"
  (let ((this (sr-pop-mark))
        (other nil))
    (if (not this)
        (setq this (car (dired-get-marked-files t))))
    (if (string= dired-directory sr-other-directory)
        (setq other (sr-pop-mark))
      (progn
        (sr-change-window)
        (setq other (sr-pop-mark))
        (sr-change-window)
        (if (not other)
            (setq other this))))
    (setq this (concat dired-directory this))
    (setq other (concat sr-other-directory other))
    (list fun this other)))

(defun sr-pop-mark ()
  "Pops the first mark in the current dired buffer"
  (let ((marks (dired-get-marked-files t nil nil t)))
    (if (< 1 (length marks))
        (progn
          (dired-unmark-all-marks)
          (if (not (equalp t (car marks)))
              (progn
                (mapcar (lambda (x)
                          (dired-mark-files-regexp (concat "^" (regexp-quote x) "$")))
                        (cdr marks))
                (car marks))
            (second marks)))
      nil)))

;;; ============================================================================
;;; Miscellaneous functions:

(defun sr-bash ()
  "Runs bash in a new buffer (or switches to an existing one) and cd's to the
current one in the selected pane"
  (interactive)
  (let ((dir dired-directory))
    (sr-quit)
    (term "bash")
    (term-send-raw-string (concat "cd " dir "")))
    (exit-recursive-edit))

;;directories which are symlinked.
(font-lock-add-keywords 'sr-mode '(("\\(^..l.*/$\\)" 1 'sr-symlink-directory-face keep)))

;;symbolic links (which do not end with a trailing slash
(font-lock-add-keywords 'sr-mode '(("\\(^..l.*[^/]$\\)" 1 'sr-symlink-face keep)))

(provide 'sunrise-commander)

;;; sunrise-commander.el ends here

; LocalWords:  laml dired MC IMHO easymenu keymap IMO Dired's
