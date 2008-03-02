;; sunrise-commander.el  ---  Two-pane file manager for Emacs based on Dired and
;; inspired by MC.

;; Copyright (C) 2007 2008 José Alfredo Romero L. (j0s3l0)

;; Author: José Alfredo Romero L. <joseito@poczta.onet.pl>
;; Keywords: Sunrise Commander Emacs File Manager Midnight Norton Orthodox

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation,  either  version  3 of the License, or (at your option) any later
;; version.
;; 
;; This  program  is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See the GNU General Public License for more de-
;; tails.

;; You  should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here  is  another two-pane mc emulation layer for emacs. It's built on top of
;; Dired and takes advantage of all its features, offering at the same time  the
;; double  pane  interface I'd been missing so badly since I started using regu-
;; larly emacs (for everything!). I tried  both  Ilya  Zakharevich's  nc.el  and
;; Kevin  Burton's  mc.el,  but  none of them was what I was looking for (though
;; mc.el was near the ideal).

;; A  lot  of  this code has been shamelessly copied from Kevin's mc.el and only
;; slightly modified. Another part of it - the code for recursive  file  copying
;; and  renaming - was adapted from the dired extensions written by Kurt Nørmark
;; for LAML (http://www.cs.aau.dk/~normark/scheme/distribution/laml/).

;; I have added to the mix several useful functions:

;; *  Sunrise  is  implemented  as a derived major mode confined inside the pane
;; buffers, so its buffers and dired ones can live together without easymenu  or
;; viper to avoid key binding collisions.

;; *  It  automatically  closes unused buffers and tries to never keep open more
;; than the one or two used to display the panes.

;; *  Each pane has its own history ring: press M-y / M-u for moving backwards /
;; forwards in the history of directories.

;; * Press C-= for "smart" file comparison using ediff. It compares together the
;; first two files marked on each pane or, if no files have been marked, it  as-
;; sumes that the second pane contains a file with the same name as the selected
;; one and tries to compare these two. You can also mark whole lists of files to
;; be compared and then just press C-= for comparing the next pair.

;; *  Press  = for fast "smart" file comparison -- like above, but using regular
;; diff.

;; * Press C-M-= for directory comparison (only by date and/or size, by now).

;; * Press C-c t to open a bash terminal into the current pane's directory.

;; * Press M-t to swap the panes.

;; * Press C-c C-s to change the layout of the panes (horizontal/vertical/top)

;; *  Press  C-x C-q   to put the current pane in Editable Dired mode (allows to
;; edit the pane as if it were a regular file -- press C-c C-c  to  commit  your
;; changes to the filesystem).

;; *  Sunrise VIRTUAL mode integrates dired-virtual mode to Sunrise, allowing to
;; capture find and locate results in regular files and to use them later as  if
;; they  were  directories  with  all  Dired  and  Sunrise  operations  at  your
;; fingertips.

;; * The results of the following operations are displayed in VIRTUAL mode:
;;     find-dired-name (by pressing C-c C-n),
;;     find-grep-name  (by pressing C-c C-g),
;;     find-dired      (by pressing C-c C-f),
;;     locate          (by pressing C-c C-l).

;; * Press C-c C-r to display the listing of all recently visited files (also in
;; VIRTUAL mode).

;; * Supports AVFS (http://www.inf.bme.hu/~mszeredi/avfs/) for transparent navi-
;; gation inside compressed archives (*.zip, *.tgz, *.tar.bz2, *.deb, etc. etc.)
;; You  need to have AVFS with coda or fuse installed and running on your system
;; for this to work, though.

;; *  Terminal  integration and Command line expansion (NEW!) integrates tightly
;; with term-mode to allow interaction between terminal emulators in  line  mode
;; (C-c  C-j)  and  the panes: the most important navigation commands (up, down,
;; mark, unmark, go to parent dir) can be executed on the active  pane  directly
;; from  the  terminal  by  pressing the usual keys with Meta: <M-up>, <M-down>,
;; etc. Additionally, the following substitutions are automagically performed in
;; term-line-mode:
;;     %f - expands to the currently selected file in the left pane
;;     %F - expands to the currently selected file in the right pane
;;     %m - expands to the list of all marked files in the left pane
;;     %M - expands to the list of all marked files in the right pane
;;     %d - expands to the current directory in the left pane
;;     %D - expands to the current directory in the right pane

;; * etc. ;-)

;; It  doesn't  even  try to look like MC, so the help window is gone (you're in
;; emacs, so you know your bindings, right?).

;; This is version 2 $Rev$ of the Sunrise Commander.

;; Please note that it was written and tested only on GNU Emacs version 23 (from
;; CVS). I *am* aware that there are several functions  (including,  alas,  file
;; and  directory comparison) that simply will not work on GNU Emacs 21, but un-
;; fortunately I do not have the time to port them back. I don't know either  if
;; it  will  work  at  all on XEmacs, so try at your own risk. All contributions
;; and/or bug reports will be very welcome.

;;; Installation and Usage:

;; 1) Put this file somewhere in your emacs load-path.

;; 2) Add a (require 'sunrise-commander) to your .emacs file.

;; 3) If you want the function keys bound to the usual MC commands (F5 for copy,
;; F6 for rename, and so on) add: (sunrise-mc-keys)  after  the  "require"  line
;; (IMHO  these  bindings  are not optimal for emacs, but I'm including them be-
;; cause some MC power users may have them too deeply embedded in  their  spinal
;; cord)

;; 4)  If  you have AVFS running on your system and want to use it directly from
;; Sunrise, add: (sunrise-avfs "[path-to-AVFS-root]") after the "require"  line.

;; 5)  Choose  some  unused  extension for files to be opened in Sunrise VIRTUAL
;; mode and add it to auto-mode-alist, e.g. if you want  to  name  your  virtual
;; folders like *.svrm just add to your .emacs file a line like the following:
;;
;;     (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

;; 6) Evaluate all the new lines, or reload your .emacs file, or restart emacs.

;; 7) Type M-x sunrise to invoke the Sunrise Commander (or much better: bind the
;; function to your favorite key combination). Type C-h m  for   information  on
;; available key bindings.

;; 8) Enjoy :)

;;; Code:

(require 'dired)
(require 'dired-x)
(require 'font-lock)
(eval-when-compile (require 'term))
(require 'recentf)
(recentf-mode 1)

(setq dired-recursive-deletes 'top
      dired-listing-switches "-alp"
      ;; dired-listing-switches "--time-style=locale --group-directories-first -alphgG"
      recentf-max-saved-items 100
      recentf-max-menu-items 20)

(defcustom sr-terminal-program "bash"
  "The program to use for terminal emulation."
  :group 'sunrise)

(defcustom sr-window-split-style 'horizontal
  "The current window split configuration.  May be 'horizontal, 'vertical or 'top"
  :group 'sunrise
  :type '(choice
          (const horizontal)
          (const vertical)
          (const top)))

(defvar sr-restore-buffer nil
  "Buffer to restore when sr is quit.")

(defvar sr-prior-window-configuration nil
  "Window configuration before sr was started.")

(defvar sr-running nil "True when sr commander mode is running.")

(defvar sr-current-window-overlay nil
  "Holds the current overlay which marks the current dired buffer.")

(defvar sr-left-directory "~/"
  "Dired directory for the left window.  See variable `dired-directory'.")

(defvar sr-left-buffer nil
  "Dired buffer for the left window.")

(defvar sr-right-directory "~/"
  "Dired directory for the right window.  See variable `dired-directory'.")

(defvar sr-right-buffer nil
  "Dired buffer for the right window.")

(defvar sr-other-directory "~/"
  "Dired directory in the window that is currently *not* selected")

(defvar sr-checkpoint-registry (acons "~" (list sr-left-directory sr-right-directory) nil)
  "Registry of currently defined checkpoints")

(defvar sr-left-window nil
  "The left window of dired.")

(defvar sr-right-window nil
  "The right window of dired.")

(defvar sr-selected-window 'left
  "The window to select when sr starts up.")

(defvar sr-ediff-on nil
  "Flag that indicates whether an ediff is being done by Sunrise")

(defvar sr-dired-directory ""
  "Directory inside which sr-mode is currently active")

(defvar sr-start-message
  "Been coding all night? Enjoy the Sunrise! (or press q to quit)"
  "Message to display when `sr' is started.")

(defvar sr-avfs-root nil
  "The root of the AVFS virtual filesystem to use for navigating compressed
   archives. Set to a non-nil value to activate AVFS support.")

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

;;; ============================================================================
;;; This is the core of Sunrise: the main idea is to apply sr-mode only inside
;;; Sunrise buffers while keeping all of dired-mode untouched.

(define-derived-mode sr-mode dired-mode "Sunrise Commander"
  "Two-pane file manager for Emacs based on Dired and inspired by MC. The
  following keybindings are available:

        / ............. go to directory
        U ............. go to parent directory
        M-y ........... go to previous directory in history
        M-u ........... go to next directory in history
        C-> ........... save named checkpoint (a.k.a. \"bookmark panes\")
        C-c > ......... save named checkpoint (a.k.a. \"bookmark panes\")
        C-.    ........ restore named checkpoint
        C-c .  ........ restore named checkpoint
        M-a ........... go to beginning of current directory
        M-e ........... go to end of current directory
        Tab ........... go to other pane
        C-Tab.......... go to viewer window
        C-c Tab ....... go to viewer window (console portable) 

        C-c C-s ....... change pane layout (vertical/horizontal/top-only)
        M-t ........... transpose panes
        M-o ........... synchronize panes
        C-o ........... show/hide hidden files (requires dired-omit-mode)
        b ............. browse current directory using w3m
        g ............. refresh pane
        C-Backspace ... hide/show file attributes in pane
        C-c Backspace . hide/show file attributes in pane (console portable)
        s ............. change sorting order or files (name/size/time/extension)

        C-= ........... smart compare files (ediff)
        = ............. fast smart compare files (plain diff)
        C-M-= ......... compare directories

        C-c C-f ....... execute find-dired in Sunrise VIRTUAL mode
        C-c C-n ....... execute find-name-dired in Sunrise VIRTUAL mode
        C-c C-g ....... execute find-grep-dired in Sunrise VIRTUAL mode
        C-c C-l ....... execute locate in Sunrise VIRTUAL mode
        C-c C-r ....... browse list of recently visited files (requires recentf)

        Return ........ visit selected file
        o ............. quick view selected file (scroll with C-M-v, C-M-S-v)
        + ............. create new directory
        C ............. copy marked (or current) files and directories
        c ............. copy (using traditional dired-do-copy)
        R ............. rename marked (or current) files and directories
        r ............. rename (using traditional dired-do-rename)
        D ............. delete marked (or current) files and directories
        C-x C-q ....... put current pane in Editable Dired mode

        C-c t ......... open terminal in current directory
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

(define-derived-mode sr-virtual-mode dired-virtual-mode "Sunrise VIRTUAL"
  "Sunrise Commander Virtual Mode. Useful for reusing find and locate results."
  :group 'sunrise
  (set-keymap-parent sr-virtual-mode-map sr-mode-map)
  (sr-highlight)
  (hl-line-mode 1)
  (define-key sr-virtual-mode-map "g" nil))

(defmacro sr-within (dir form)
  "Puts the given form in Sunrise context"
  (list 'progn
        (list 'setq 'sr-dired-directory
              (list 'file-name-as-directory
                    (list 'abbreviate-file-name dir)))
        (list 'ad-activate (list 'quote 'dired-find-buffer-nocreate))
        form
        (list 'ad-deactivate (list 'quote 'dired-find-buffer-nocreate))
        (list 'setq 'sr-dired-directory "")))

(defun sr-dired-mode ()
  "Sets Sunrise mode in every Dired buffer opened in Sunrise (called in hook)"
  (if (string= sr-dired-directory dired-directory)
      (sr-mode)
    (message (concat "Sunrise: " sr-dired-directory " != " dired-directory))))
(add-hook 'dired-before-readin-hook 'sr-dired-mode)

;; This is a hack to avoid some dired mode quirks:
(defadvice dired-find-buffer-nocreate
  (before sr-advice-findbuffer (dirname &optional mode))
  (if (string= sr-dired-directory dirname)
      (setq mode 'sr-mode)))

;; Handles panes opened from bookmarks in Sunrise:
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

;; Tweaks the target directory guessing mechanism:
(defadvice dired-dwim-target-directory
  (around sr-advice-dwim-target ())
  (if sr-running
      (setq ad-return-value sr-other-directory)
    ad-do-it))
(ad-activate 'dired-dwim-target-directory)

;;; ============================================================================
;;; Sunrise Commander keybindings:

(define-key sr-mode-map "\C-m"                'sr-advertised-find-file)
(define-key sr-mode-map "o"                   'sr-quick-view)
(define-key sr-mode-map "/"                   'sr-goto-dir)
(define-key sr-mode-map "U"                   'sr-dired-prev-subdir)
(define-key sr-mode-map "\M-y"                'sr-history-prev)
(define-key sr-mode-map "\M-u"                'sr-history-next)
(define-key sr-mode-map [(control >)]         'sr-checkpoint-save)
(define-key sr-mode-map "\C-c>"               'sr-checkpoint-save)
(define-key sr-mode-map [(control .)]         'sr-checkpoint-restore)
(define-key sr-mode-map "\C-c."               'sr-checkpoint-restore)
(define-key sr-mode-map "\t"                  'sr-change-window)
(define-key sr-mode-map [(control tab)]       'sr-select-viewer-window)
(define-key sr-mode-map "\C-c\t"              'sr-select-viewer-window)
(define-key sr-mode-map "\M-a"                'sr-beginning-of-buffer)
(define-key sr-mode-map "\M-e"                'sr-end-of-buffer)
(define-key sr-mode-map "\C-c\C-s"            'sr-split-toggle)
(define-key sr-mode-map "\M-t"                'sr-transpose-panes)
(define-key sr-mode-map "\M-o"                'sr-synchronize-panes)
(define-key sr-mode-map "\C-o"                'sr-omit-mode)
(define-key sr-mode-map "b"                   'sr-browse)
(define-key sr-mode-map "g"                   'sr-revert-buffer)
(define-key sr-mode-map [(control backspace)] 'sr-toggle-attributes)
(define-key sr-mode-map "\C-c\d"              'sr-toggle-attributes)
(define-key sr-mode-map "s"                   'sr-interactive-sort)

(define-key sr-mode-map "C"                   'sr-do-copy)
(define-key sr-mode-map "c"                   'dired-do-copy)
(define-key sr-mode-map "R"                   'sr-do-rename)
(define-key sr-mode-map "r"                   'dired-do-rename)
(define-key sr-mode-map "\C-x\C-q"            'sr-editable-pane)
(define-key sr-mode-map "\C-ct"               'sr-term)

(define-key sr-mode-map "="                   'sr-diff)
(define-key sr-mode-map [(control ?\=)]       'sr-ediff)
(define-key sr-mode-map [(control meta ?\=)]  'sr-compare-dirs)

(define-key sr-mode-map "\C-c\C-f"            'sr-find)
(define-key sr-mode-map "\C-c\C-n"            'sr-find-name)
(define-key sr-mode-map "\C-c\C-g"            'sr-find-grep)
(define-key sr-mode-map "\C-c\C-l"            'sr-locate)
(define-key sr-mode-map "\C-c\C-r"            'sr-recent-files)

(define-key sr-mode-map "q"                   'keyboard-escape-quit)

(defun sunrise-mc-keys ()
  "Binds the function keys F2 to F10 the traditional MC way"
  (interactive)
  (define-key sr-mode-map [(f2)]            'sr-goto-dir)
  (define-key sr-mode-map [(f3)]            'sr-quick-view)
  (define-key sr-mode-map [(f4)]            'sr-advertised-find-file)
  (define-key sr-mode-map [(f5)]            'sr-do-copy)
  (define-key sr-mode-map [(f6)]            'sr-do-rename)
  (define-key sr-mode-map [(f7)]            'dired-create-directory)
  (define-key sr-mode-map [(f8)]            'dired-do-delete)
  (define-key sr-mode-map [(f10)]           'keyboard-escape-quit)
  (define-key sr-mode-map [(insert)]        'dired-mark)
  (define-key sr-mode-map [(control prior)] 'sr-dired-prev-subdir))

(defun sunrise-avfs (root)
  "Activates AVFS support and sets the root of the virtual filesystem."
  (interactive "DAVFS root directory: ")
  (setq root (expand-file-name root))
  (let ((tail (string-match "/$" root)))
    (if tail
        (setq root (substring root 0 tail))))
  (setq sr-avfs-root root))

;;; ============================================================================
;;; Initialization and finalization functions:

(defun sunrise (&optional left-directory right-directory) 
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

(defun sunrise-cd ()
  "Run Sunrise but give it the current directory to use."
  (interactive)
  (let((left-directory default-directory))
    (if (buffer-live-p sr-left-buffer)
        (kill-buffer sr-left-buffer))
    (sunrise left-directory)))

(defun sr-dired (directory)
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

(defun sr-setup-interface ()
  "Sets up the logical Sunrise interface"
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

(defun sr-symbol (name context)
  "Helper function for macro sr-setup-pane."
  (intern (concat "sr-" name "-" (symbol-name context))))

(defmacro sr-setup-pane (name)
  "Helper macro for function sr-setup-windows."
  (list 'progn
        (list 'setq (sr-symbol name 'window) (list 'selected-window))
        (list 'if (list 'buffer-live-p (sr-symbol name 'buffer))
              (list 'progn
                    (list 'switch-to-buffer (sr-symbol name 'buffer))
                    (list 'setq (sr-symbol name 'directory) 'dired-directory))
              (list 'sr-dired (sr-symbol name 'directory)))))

(defun sr-setup-windows()
  "Setup the Sunrise window configuration (two windows in sr-mode.)"

  ;;get rid of all windows except one (not any of the panes!)
  (sr-select-viewer-window)
  (delete-other-windows)

  ;;now create the bottom window
  (split-window (selected-window) (* 2 (/ (window-height) 3)))

  (cond 
   ((equal sr-window-split-style 'horizontal) (split-window-horizontally))
   ((equal sr-window-split-style 'vertical)   (split-window-vertically))
   ((equal sr-window-split-style 'top)        (split-window-vertically))
   (t (error "ERROR: Don't know how to split this window: %s" sr-window-split-style)))

  ;;setup sunrise on both panes
  (sr-setup-pane "left")
  (other-window 1)
  (let ((sr-selected-window 'right))
    (sr-setup-pane "right"))

  ;;select the correct window
  (sr-select-window sr-selected-window)

  (if (equal sr-window-split-style 'top)
      (delete-window sr-right-window)
    (sr-force-passive-highlight)))

;; Keeps the size of the Sunrise panes constant:
(add-hook 'window-size-change-functions
          (lambda (frame)
            (if (and sr-running
                     (not sr-ediff-on)
                     (window-live-p sr-left-window))
                (save-selected-window
                  (select-window sr-left-window)
                  (let* ((my-style-factor (if (equal sr-window-split-style 'horizontal) 2 1))
                         (my-pane-height (* my-style-factor (/ (frame-height) 3)))
                         (my-delta (- my-pane-height (window-height))))
                    (enlarge-window my-delta))))))

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

(defun sr-select-viewer-window ()
  "Tries to select a window that is not a sr pane"
  (interactive)
  (dotimes (times 2)
    (if (memq (selected-window) (list sr-left-window sr-right-window))
        (other-window 1))))

(defun sr-highlight()
  "Highlight the current buffer, destroying the previous buffer highlight if
  necessary."

  ;;update the last overlay
  (if sr-current-window-overlay
      (overlay-put sr-current-window-overlay 'face 'sr-window-not-selected-face))
  
  (save-excursion
    (let(begin end)
      (goto-char (point-min))

      ;;hide avfs virtual filesystem root (if any):
      (if (not (null sr-avfs-root))
          (let ((next (search-forward sr-avfs-root nil t))
                (len (length sr-avfs-root))
                (overlay))
            (while (not (null next))
              (progn
                (setq overlay (make-overlay (- next len) next))
                (overlay-put overlay 'invisible t)
                (overlay-put overlay 'intangible t)
                (setq next (search-forward sr-avfs-root nil t))))
            (goto-char (point-min))))

      ;;determine begining and end
      (search-forward-regexp "\\S " nil t)
      (setq begin (1- (point)))
      (end-of-line)
      (setq end (1- (point)))

      ;;setup overlay
      (setq sr-current-window-overlay (make-overlay begin end))
      (overlay-put sr-current-window-overlay 'face 'sr-window-selected-face)
      (overlay-put sr-current-window-overlay 'window (selected-window)))))

(defun sr-force-passive-highlight ()
  (sr-change-window)
  (sr-change-window))

(defun sr-quit()
  "Quit Sunrise and restore emacs to previous operation."
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
            (progn
              (setq sr-left-directory dired-directory)
              (setq sr-left-buffer (window-buffer sr-left-window))))
        (bury-buffer)))

  (if (window-live-p sr-right-window)
      (progn
        (set-buffer (window-buffer sr-right-window))
        (if (equal major-mode 'sr-mode)
            (progn
              (setq sr-right-directory dired-directory)
              (setq sr-right-buffer (window-buffer sr-right-window))))
        (bury-buffer))))

;;; ============================================================================
;;; File system navigation functions:

(defun sr-advertised-find-file()
  "Call dired-advertised-find-file but also perform additional actions"
  (interactive)
  (save-excursion
    (let ((filename (expand-file-name (dired-get-filename nil t))))
      (if filename
          (if (file-directory-p filename)
              (sr-goto-dir filename)
            (sr-find-file filename))))))

(defun sr-find-file (filename)
  "Determines the proper way of handling a file. If the file is a compressed
   archive and AVFS has been activated, first tries to display it as a catalogue
   in the VFS, otherwise just visits the file."
  (if (not (null sr-avfs-root))
      (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
        (if (or (eq 'archive-mode mode)
                (eq 'tar-mode mode)
                (and (listp mode) (eq 'jka-compr (second mode)))
                (eq 'avfs-mode mode))
            (let ((vfile (concat sr-avfs-root filename "#/")))
              (if (file-directory-p vfile)
                  (progn
                    (sr-goto-dir vfile)
                    (setq filename nil)))))
        (if (eq 'sr-virtual-mode mode)
            (progn
              (find-file filename)
              (sr-history-push filename)
              (setq filename nil)))))

  (if (null filename) ;;the file is a virtual directory:
      (if (eq sr-selected-window 'left)
          (setq sr-left-buffer (window-buffer))
        (setq sr-right-buffer (window-buffer)))

    (progn ;;the file is a regular file:
      (sr-quit)
      (condition-case description
          (find-file filename)
        (error (message (second description))))
      (exit-recursive-edit))))

(defun sr-goto-dir (dir)
  "Changes the current directory in the active pane to the given one"
  (interactive
   (list 
    (read-file-name "Change directory (file or pattern): " nil nil nil)))

  (if (and (not (null sr-avfs-root))
           (null (posix-string-match "#" dir)))
        (setq dir (replace-regexp-in-string sr-avfs-root "" dir)))

  (hl-line-mode 0)
  (sr-within dir
              (if (or (not dired-directory)
                      (string= sr-other-directory dired-directory))
                  (dired dir)
                (find-alternate-file dir)))
  (if (eq sr-selected-window 'left)
      (setq sr-left-buffer (window-buffer))
    (setq sr-right-buffer (window-buffer)))
  (sr-history-push dired-directory)
  (sr-highlight)
  (sr-beginning-of-buffer)
  (hl-line-mode 1))

(defun sr-dired-prev-subdir()
  "Go to the previous subdirectory."
  (interactive)
  (if (not (string= dired-directory "/"))
      (sr-goto-dir (expand-file-name ".."))
    (error "ERROR: Already at root")))

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
    (while (and (> (length hist) 0) (not item))
      (setq item (car (last hist)))
      (nbutlast hist))
    (if item
        (progn
          (push item hist)
          (put sr-selected-window 'history hist)
          (if (file-directory-p item)
              (sr-goto-dir item)
            (sr-find-file item))))))

(defun sr-history-prev ()
  "Changes the current directory to the previous one (if any) in the history
list of the current pane"
  (interactive)
  (let (
        (hist (get sr-selected-window 'history))
        (item nil)
       )
    (while (and (> (length hist) 0) (not item))
      (setq item (pop hist)))
    (if item
        (progn
          (nconc hist (list item))
          (put sr-selected-window 'history hist)
          (setq item (car hist))
          (if (file-directory-p item)
              (sr-goto-dir item)
            (sr-find-file item))))))

(defun sr-checkpoint-save (name)
  "Allows to give a name to the current directories in the Sunrise panes, so
they can be restored later."
  (interactive "sCheckpoint name to save? ")
  (let ((my-window (selected-window))
        (my-cell))
    (setq my-cell (assoc-string name sr-checkpoint-registry))
    (select-window sr-left-window)
    (setq sr-left-directory dired-directory)
    (select-window sr-right-window)
    (setq sr-right-directory dired-directory)
    (select-window my-window)
    (if (null my-cell)
        (setq sr-checkpoint-registry
              (acons name
                     (list sr-left-directory sr-right-directory)
                     sr-checkpoint-registry))
      (setcdr my-cell (list sr-left-directory sr-right-directory)))
  (message (concat "Checkpoint \"" name "\" saved"))))

(defun sr-checkpoint-restore (name)
  "Allows to restore a previously saved checkpoint."
  (interactive "sCheckpoint name to restore? " )
  (let ((cp-list (assoc-string name sr-checkpoint-registry))
        (my-window))
    (if (null cp-list)
        (error (concat "No such checkpoint: " name)))
    (setq my-window (selected-window))
    (select-window sr-left-window)
    (sr-goto-dir (second cp-list))
    (select-window sr-right-window)
    (sr-goto-dir (third cp-list))
    (select-window my-window)
    (sr-force-passive-highlight)))

;;; ============================================================================
;;; Graphical interface interaction functions:

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
  (cond
   ((equal sr-window-split-style 'horizontal) (sr-split-setup 'vertical))
   ((equal sr-window-split-style 'vertical)   (sr-split-setup 'top))
   ((equal sr-window-split-style 'top)        (sr-split-setup 'horizontal))
   (t                                         (sr-split-setup 'horizontal))))

(defun sr-split-setup(split-type)
  (setq sr-window-split-style split-type)
  (if sr-running
      (if (equal sr-window-split-style 'top)
          (delete-window sr-right-window)
        (sr-setup-windows))
    (message "Split is now %s." (symbol-name split-type))))

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
  (if (featurep 'browse-url)
      (let(filename)
        (setq filename (dired-get-filename))
        (if filename
            (let(url)
              (setq url (concat "file://" filename))
              (message "Browsing %s " url)
              (browse-url url))))
    (error "ERROR: Feature browse-url not available!")))

(defun sr-revert-buffer ()
  "Refreshes the current pane"
  (interactive)
  (revert-buffer)
  (sr-force-passive-highlight))

(defun sr-omit-mode ()
  "Toggles dired-omit-mode"
  (interactive)
  (dired-omit-mode)
  (sr-highlight))

(defun sr-quick-view ()
  "Opens the selected file on the other window without selecting it. Kills
   any other buffer opened previously the same way."
  (interactive)
  (let ((home (selected-window)))
    (if (buffer-live-p other-window-scroll-buffer)
        (kill-buffer other-window-scroll-buffer))
    (dired-find-file-other-window)
    (setq other-window-scroll-buffer (current-buffer))
    (select-window home)))

(defun sr-hide-attributes ()
  "Hides the attributes of all files in the active pane."
  (save-excursion
    (sr-unhide-attributes)
    (goto-char (point-min))
    (let ((next (re-search-forward directory-listing-before-filename-regexp nil t))
          (attr-list nil)
          (overlay nil))
      (while (not (null next))
        (beginning-of-line)
        (setq overlay (make-overlay (point) (- next 1)))
        (setq attr-list (cons overlay attr-list))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'intangible t)
        (next-line)
        (setq next (re-search-forward directory-listing-before-filename-regexp nil t)))
      (put sr-selected-window 'hidden-attrs attr-list))))

(defun sr-unhide-attributes ()
  "Shows the (hidden) attributes of all files in the active pane."
  (let ((attr-list (get sr-selected-window 'hidden-attrs)))
    (if (not (null attr-list))
        (progn
          (mapcar 'delete-overlay attr-list)
          (put sr-selected-window 'hidden-attrs nil)))))
(add-hook 'dired-after-readin-hook 'sr-unhide-attributes)

(defun sr-toggle-attributes ()
  "Hides/Shows the attributes of all files in the active pane."
  (interactive)
  (if (null (get sr-selected-window 'hidden-attrs))
      (sr-hide-attributes)
    (sr-unhide-attributes)))

(defun sr-sort-order (label option)
  "Changes the sorting order of the active pane by appending additional options
   to dired-listing-switches"
  (put sr-selected-window 'sorting-order label)
  (dired-sort-other (concat dired-listing-switches option))
  (sr-revert-buffer)
  (message (concat "Sunrise: sorting entries by " label)))

(defun sr-interactive-sort (order)
  "Prompts for a new sorting order for the active pane and applies it."
  (interactive "cSort by (n)ame, (s)ize, (t)ime or e(x)tension? ")
  (if (>= order 97)
      (setq order (- order 32)))
  (cond ((eq order ?T) (sr-sort-order "TIME"      "t"))
        ((eq order ?S) (sr-sort-order "SIZE"      "S"))
        ((eq order ?X) (sr-sort-order "EXTENSION" "X"))
        (t             (sr-sort-order "NAME"      "" ))))

;;; ============================================================================
;;; File manipulation functions:

(defun sr-editable-pane ()
  "Puts the current pane in Editable Dired mode (wdired)"
  (interactive)
  (let ((subdir-alist dired-subdir-alist))
    (dired-mode)
    (setq dired-subdir-alist subdir-alist)
    (wdired-change-to-wdired-mode)))

;; Puts the pane back in Sunrise mode after being edited with wdired:
(defadvice wdired-finish-edit
  (after sr-advice-wdired-finish-edit ())
  (if sr-running
      (progn
        (sr-mode)
        (sr-force-passive-highlight))))
(ad-activate 'wdired-finish-edit)

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
    (error "ERROR: You cannot copy a directory into itself or one of its subdirectories")))

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
 (let (result)
   (setq result
         (sr-filter (function (lambda (x) (not (or (equal x ".") (equal x "..")))))
                    (sr-filter
                     (function (lambda (x)
                                 (file-directory-p (concat dir "/" x))))
                     (directory-files dir))))
   (mapcar (lambda (x) (concat x "/")) result)))

(defun sr-list-of-files (dir)
  "Return a list of regular files in DIR. Each entry in the list is a string."
  (sr-filter
   (function (lambda (x)
               (file-regular-p (concat dir "/" x))))
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
    (cond ((eq point-idx nil) filename)
          ((eq point-idx   0) filename)
          (t (substring filename 0 point-idx)))))

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
  "Prompts for the criterion to use for comparing two directories."
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
;;; File search functions:

(defun sr-find-apply (fun pattern)
  "Helper function for functions sr-find, sr-find-name and sr-find-grep."
  (let ((find-ls-option (cons (concat "-exec ls -d " dired-listing-switches " \\{\\} \\;") "ls -ld")))
    (apply fun (list dired-directory pattern)))
  (sr-virtual-mode)
  (sr-keep-buffer))

(defun sr-find (pattern)
  "Runs find-dired passing the current directory as first parameter."
  (interactive "sRun find (with args): ")
  (sr-find-apply 'find-dired pattern))

(defun sr-find-name (pattern)
  "Runs find-name-dired passing the current directory as first parameter."
  (interactive "sFind name pattern: ")
  (sr-find-apply 'find-name-dired pattern))

(defun sr-find-grep (pattern)
  "Runs find-grep-dired passing the current directory as first parameter."
  (interactive "sFind files containing pattern: ")
  (sr-find-apply 'find-grep-dired pattern))

(defun sr-locate ()
  "Runs locate with the necessary options to produce a buffer that can be put in
   sunrise virtual mode"
  (interactive)
  (switch-to-buffer "*Locate*")
  (let ((locate-prompt-for-command t)
        (locate-make-command-line (lambda (arg) (list "locate" arg "| xargs ls -d" dired-listing-switches))))
    (call-interactively 'locate))
  (sr-virtual-mode)
  (sr-keep-buffer))

(defun sr-recent-files ()
  "Displays the history of recent files maintained by recentf in sunrise virtual
   mode."
  (interactive)
  (sr-switch-to-clean-buffer "*Recent Files*")
  (insert "Recent Files: \n")
  (let ((dired-actual-switches dired-listing-switches))
    (condition-case nil
        (dired-insert-directory "/" dired-listing-switches recentf-list)
      (error
         (recentf-cleanup)
         (sr-switch-to-clean-buffer "*Recent Files*")
         (insert "Recent Files: \n")
         (dired-insert-directory "/" dired-listing-switches recentf-list)))
    (sr-virtual-mode)
    (sr-keep-buffer)))

(defun sr-switch-to-clean-buffer (name)
  (switch-to-buffer name)
  (if (equalp major-mode 'sr-virtual-mode)
      (progn
        (kill-buffer nil)
        (switch-to-buffer name))
    (progn
      (goto-char (point-min))
      (flush-lines "."))))

;; This cleans up the current pane after deletion from the history of recent
;; files:
(defadvice dired-do-flagged-delete
  (after sr-advice-dired-do-flagged-delete (&optional nomessage))
  (if (string= (buffer-name) "*Recent Files*")
      (sr-recent-files)))
(ad-activate 'dired-do-flagged-delete)

;;; ============================================================================
;;; TI (Terminal Integration) and CLEX (Command Line EXpansion) functions:

(defun sr-term ()
  "Runs terminal in a new buffer (or switches to an existing one) and cd's to the
current directory in the active pane"
  (interactive)
  (let ((dir dired-directory))
    (sr-select-viewer-window)
    (term sr-terminal-program)
    (term-send-raw-string (concat "cd " dir ""))))

(defmacro sr-ti (form)
  "Puts the given form in the context of the selected pane"
  (list 'if 'sr-running
        (list 'progn
              (list 'sr-select-window 'sr-selected-window)
              (list 'hl-line-mode 0)
              form
              (list 'hl-line-mode 1)
              (list 'sr-select-viewer-window))))

(defun sr-ti-previous-line ()
  "Runs previous-line on active pane from the terminal window."
  (interactive)
  (sr-ti (forward-line -1)))

(defun sr-ti-next-line ()
  "Runs next-line on active pane from the terminal window."
  (interactive)
  (sr-ti (forward-line 1)))

(defun sr-ti-select ()
  "Runs dired-advertised-find-file on active pane from the terminal window."
  (interactive)
  (sr-ti (sr-advertised-find-file)))

(defun sr-ti-mark ()
  "Runs dired-mark on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-mark 1)))

(defun sr-ti-unmark ()
  "Runs dired-unmark-backward on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-unmark-backward 1)))

(defun sr-ti-prev-subdir ()
  "Runs dired-prev-subdir on active pane from the terminal window."
  (interactive)
  (sr-ti (sr-dired-prev-subdir)))

(defun sr-clex-file (pane)
  "Returns the currently selected file in the given pane"
  (save-window-excursion
    (if (equal pane 'right)
        (select-window sr-right-window)
      (select-window sr-left-window))
    (condition-case nil
        (concat (dired-get-filename) " ")
      (error ""))))

(defun sr-clex-marked (pane)
  "Returns a string containing the list of marked files in the given pane."
  (save-window-excursion
    (if (equal pane 'right)
        (select-window sr-right-window)
      (select-window sr-left-window))
    (condition-case nil
        (mapconcat 'identity (dired-get-marked-files) " ")
      (error ""))))

(defun sr-clex-dir (pane)
  "Returns the current directory in the given pane."
  (if (equal pane 'right)
      (concat sr-right-directory " ")
    (concat sr-left-directory " ")))

;; This performs the command line substitution while in CLEX mode:
(defadvice term-send-raw-string
  (around sr-advice-term-send-raw-string (chars))
  (ad-deactivate 'term-send-raw-string)
  (let ((my-char (string-to-char chars)))
    (setq chars
          (cond ((eq my-char ?m) (sr-clex-marked 'left ))
                ((eq my-char ?f) (sr-clex-file   'left ))
                ((eq my-char ?d) (sr-clex-dir    'left ))
                ((eq my-char ?M) (sr-clex-marked 'right))
                ((eq my-char ?F) (sr-clex-file   'right))
                ((eq my-char ?D) (sr-clex-dir    'right))
                (t chars)))
    ad-do-it)
  (term-line-mode))

(defun sr-clex-activate ()
  "Activates the Command Line EXpansion feature in all active terminals."
  (interactive)
  (ad-activate 'term-send-raw-string)
  (term-char-mode)
  (message "Sunrise: CLEX Mode is ON"))

;; Sunrise TI & CLEX key bindings in term-line mode:
(add-hook 'term-mode-hook
          '(lambda () (progn
                        (define-key term-mode-map [M-up] 'sr-ti-previous-line)
                        (define-key term-mode-map [A-up] 'sr-ti-previous-line)
                        (define-key term-mode-map [M-down] 'sr-ti-next-line)
                        (define-key term-mode-map [A-down] 'sr-ti-next-line)
                        (define-key term-mode-map "\M-\C-m" 'sr-ti-select)
                        (define-key term-mode-map "\C-\M-j" 'sr-ti-select)
                        (define-key term-mode-map "\M-m" 'sr-ti-mark)
                        (define-key term-mode-map [M-backspace] 'sr-ti-unmark)
                        (define-key term-mode-map "\M-\d" 'sr-ti-unmark)
                        (define-key term-mode-map "\M-U" 'sr-ti-prev-subdir)
                        (define-key term-mode-map "%" 'sr-clex-activate)
)))

;;; ============================================================================
;;; Miscellaneous functions:

(defun sr-keep-buffer ()
  "Keeps the currently selected buffer as one of the panes, even if it does not
   belong to the pane's history ring. Useful for maintaining the contents of a
   pane during layout switching."
  (if (equal sr-selected-window 'left)
      (setq sr-left-buffer (current-buffer))
    (setq sr-right-buffer (current-buffer))))

;;directories which are symlinked.
(font-lock-add-keywords 'sr-mode '(("\\(^..l.*/$\\)" 1 'sr-symlink-directory-face keep)))

;;symbolic links (which do not end with a trailing slash
(font-lock-add-keywords 'sr-mode '(("\\(^..l.*[^/]$\\)" 1 'sr-symlink-face keep)))

(provide 'sunrise-commander)

;;; sunrise-commander.el ends here

; LocalWords:  laml dired MC IMHO easymenu keymap IMO Dired's
