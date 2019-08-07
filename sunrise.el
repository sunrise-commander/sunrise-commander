;;; sunrise.el --- two-pane file manager for Emacs based on Dired and inspired by MC  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2015 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 24 Sep 2007
;; Version: 6
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Keywords: files, dired, midnight commander, norton, orthodox
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

;; The Sunrise Commander is a double-pane file manager for Emacs. It's built
;; atop of Dired and takes advantage of all its power, but also provides many
;; handy features of its own:

;; * Sunrise is implemented as a derived major mode confined inside the pane
;; buffers, so its buffers and Dired ones can live together without easymenu or
;; viper to avoid key binding collisions.

;; * It automatically closes unused buffers and tries to never keep open more
;; than the one or two used to display the panes, though this behavior may be
;; disabled if desired.

;; * Each pane has its own history stack: press M-y / M-u for moving backwards /
;; forwards in the history of directories.

;; * Press M-t to swap (transpose) the panes.

;; * Press C-= for "smart" file comparison using `ediff'. It compares together
;; the first two files marked on each pane or, if no files have been marked, it
;; assumes that the second pane contains a file with the same name as the
;; selected one and tries to compare these two. You can also mark whole lists of
;; files to be compared and then just press C-= for comparing the next pair.

;; * Press = for fast "smart" file comparison -- like above, but using regular
;; diff.

;; * Press C-M-= for directory comparison (by date / size / contents of files).

;; * Press C-c C-s to change the layout of the panes (horizontal/vertical/top)

;; * Press C-c / to interactively refine the contents of the current pane using
;; fuzzy (a.k.a. flex) matching, then:
;;    - press Delete or Backspace to revert the buffer to its previous state
;;    - press Return, C-n or C-p to exit and accept the current narrowed state
;;    - press Esc or C-g to abort the operation and revert the buffer
;;    - use ! to prefix characters that should NOT appear after a given position
;; Once narrowed and accepted, you can restore the original contents of the pane
;; by pressing g (revert-buffer).

;; * Sticky search: press C-c s to launch an interactive search that will remain
;; active from directory to directory, until you hit a regular file or press C-g

;; * Press C-x C-q to put the current pane in Editable Dired mode (allows to
;; edit the pane as if it were a regular file -- press C-c C-c to commit your
;; changes to the filesystem, or C-c C-k to abort).

;; * Press y to recursively calculate the total size (in bytes) of all files and
;; directories currently selected/marked in the active pane.

;; * Sunrise VIRTUAL mode integrates dired-virtual mode to Sunrise, allowing to
;; capture grep, find and locate results in regular files and to use them later
;; as if they were directories with all the Dired and Sunrise operations at your
;; fingertips.
;;
;; * The results of the following operations are displayed in VIRTUAL mode:
;;    - find-name-dired (press C-c C-n),
;;    - find-dired      (press C-c C-f),
;;    - grep            (press C-c C-g),
;;    - locate          (press C-c C-l),
;;    - list all recently visited files (press C-c C-r -- requires recentf),
;;    - list all directories in active pane's history ring (press C-c C-d).

;; * Supports AVFS (http://avf.sourceforge.net/) for transparent navigation
;; inside compressed archives (*.zip, *.tgz, *.tar.bz2, *.deb, etc. etc.)
;; You need to have AVFS with coda or fuse installed and running on your system
;; for this to work, though.

;; * Opening terminals directly from Sunrise:
;;    - Press C-c C-t to inconditionally open a new terminal into the currently
;;      selected directory in the active pane.
;;    - Use C-c t to switch to the last opened terminal, or (when already inside
;;      a terminal) to cycle through all open terminals.
;;    - Press C-c T to switch to the last opened terminal and change directory
;;      to the one in the current directory.
;;    - Press C-c M-t to be prompted for a program name, and then open a new
;;      terminal using that program into the currently selected directory
;;      (eshell is a valid value; if no program can be found with the given name
;;      then the value of `sunrise-terminal-program' is used instead).

;; * Terminal integration and Command line expansion: integrates tightly with
;; `eshell' and `term-mode' to allow interaction between terminal emulators in
;; line mode (C-c C-j) and the panes: the most important navigation commands
;; (up, down, mark, unmark, go to parent dir) can be executed on the active pane
;; directly from the terminal by pressing the usual keys with Meta: <M-up>,
;; <M-down>, etc. Additionally, the following substitutions are automagically
;; performed in `eshell' and `term-line-mode':
;;     %f - expands to the currently selected file in the left pane
;;     %F - expands to the currently selected file in the right pane
;;     %m - expands to the list of paths of all marked files in the left pane
;;     %M - expands to the list of paths of all marked files in the right pane
;;     %n - expands to the list of names of all marked files in the left pane
;;     %N - expands to the list of names of all marked files in the right pane
;;     %d - expands to the current directory in the left pane
;;     %D - expands to the current directory in the right pane
;;     %a - expands to the list of paths of all marked files in the active pane
;;     %A - expands to the current directory in the active pane
;;     %p - expands to the list of paths of all marked files in the passive pane
;;     %P - expands to the current directory in the passive pane

;; * Cloning of complete directory trees: press K to clone the selected files
;; and directories into the passive pane. Cloning is a more general operation
;; than copying, in which all directories are recursively created with the same
;; names and structures at the destination, while what happens to the files
;; within them depends on the option you choose:
;;    - "(F)ile System of..." clones the FS structure of paths in a VIRTUAL pane,
;;    - "(D)irectories only" ignores all files, copies only directories,
;;    - "(C)opies" performs a regular recursive copy of all files and dirs,
;;    - "(H)ardlinks" makes every new file a (hard) link to the original one
;;    - "(S)ymlinks" creates absolute symbolic links for all files in the tree,
;;    - "(R)elative symlinks” creates relative symbolic links.

;; * Passive navigation: the usual navigation keys (n, p, Return, U, ;) combined
;; with Meta allow to move across the passive pane without actually having to
;; switch to it.

;; * Synchronized navigation: press C-c C-z to enable / disable synchronized
;; navigation. In this mode, the passive navigation keys (M-n, M-p, M-Return,
;; etc.) operate on both panes simultaneously. I've found this quite useful for
;; comparing hierarchically small to medium-sized directory trees (for large to
;; very large directory trees one needs something on the lines of diff -r
;; though).

;; * And much more -- press ? while in Sunrise mode for basic help, or h for a
;; complete list of all keybindings available (use C-e and C-y to scroll).

;; There is no help window like in MC, but if you really miss it, just get and
;; install the sunrise-buttons extension.

;; A lot of this code was once adapted from Kevin Burton's mc.el, but it has
;; evolved considerably since then. Another part (the code for file copying and
;; renaming) derives originally from the Dired extensions written by Kurt
;; Nørmark for LAML (http://www.cs.aau.dk/~normark/scheme/distribution/laml/).

;; It's written on GNU Emacs 25 on Linux and tested on GNU Emacs 22, 23, 24 and
;; 25 for Linux and on EmacsW32 (version 23) for Windows. I have also received
;; feedback from users reporting it works OK on the Mac. It does not work either
;; on GNU Emacs 21 or XEmacs -- please drop me a line if you would like to help
;; porting it. All contributions and/or bug reports will be very welcome.

;; For more details on the file manager, several available extensions and many
;; cool tips & tricks visit http://www.emacswiki.org/emacs/Sunrise_Commander

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise) to your .emacs file.

;; 3) Choose some unused extension for files to be opened in Sunrise VIRTUAL
;; mode and add it to `auto-mode-alist', e.g. if you want to name your virtual
;; directories like *.svrm just add to your .emacs file a line like the
;; following:
;;
;;     (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sunrise-virtual-mode))

;; 4) Evaluate the new lines, or reload your .emacs file, or restart Emacs.

;; 5) Type M-x sunrise to invoke the Sunrise Commander (or much better: bind the
;; function to your favorite key combination). The command `sunrise-cd' invokes
;; Sunrise and automatically selects the current file wherever it is in the
;; filesystem. Type h at any moment for information on available key bindings.

;; 6) Type M-x customize-group <RET> sunrise <RET> to customize options, fonts
;; and colors (activate AVFS support here, too).

;; 7) Enjoy :)

;;; Code:

(require 'advice)
(require 'desktop)
(require 'dired)
(require 'dired-aux)
(require 'dired-x)
(require 'enriched)
(require 'esh-mode)
(require 'find-dired)
(require 'font-lock)
(require 'hl-line)
(require 'sort)
(require 'term)
(require 'tramp)
(eval-when-compile (require 'cl)
                   (require 'recentf))

(eval-and-compile
  (unless (fboundp 'cl-labels)
    (defalias 'cl-labels 'labels))
  (unless (fboundp 'cl-letf)
    (defalias 'cl-letf 'letf)))

(defgroup sunrise nil
  "The Sunrise Commander File Manager."
  :group 'files)

(defcustom sunrise-show-file-attributes t
  "Whether to initially display file attributes in Sunrise panes.
You can always toggle file attributes display pressing
\\<sunrise-mode-map>\\[sunrise-toggle-attributes]."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-autoload-extensions t
  "Whether to load extensions immediately after their declaration, or when the
SC core is loaded (e.g. when using autoload cookies)."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-show-hidden-files nil
  "Whether to initially display hidden files in Sunrise panes.
You can always toggle hidden files display pressing
\\<sunrise-mode-map>\\[dired-omit-mode].
You can also customize what files are considered hidden by setting
`dired-omit-files' and `dired-omit-extensions' in your .emacs file."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-visit-buffer-function 'sunrise-visit-buffer-in-current-frame
  "Determines how newly opened buffers are to be displayed.
The following options are supported:

* Visit in current frame - Quit Sunrise and display the new
buffer in the current frame.

* Visit in dedicated frame - Create a separate dedicated frame
and display the buffer in it. The frame will be automatically
destroyed when the buffer is killed.

* Other - Provide your own function to display the given buffer."
  :group 'sunrise
  :type '(choice
          (function-item :tag "Visit in current frame" sunrise-visit-buffer-in-current-frame)
          (function-item :tag "Visit in dedicated frame" special-display-popup-frame)
          (function :tag "Other")))

(defcustom sunrise-terminal-kill-buffer-on-exit t
  "Whether to kill terminal buffers after their shell process ends."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-terminal-program "eshell"
  "The program to use for terminal emulation.
If this value is set to \"eshell\", the Emacs shell (`eshell')
will be used."
  :group 'sunrise
  :type 'string)

(defcustom sunrise-listing-switches "-al"
  "Listing switches passed to `ls' when building Sunrise buffers.
\(Cf. `dired-listing-switches'.)
  Most portable value: -al
  Recommended value on GNU systems: \
--time-style=locale --group-directories-first -alDhgG"
  :group 'sunrise
  :type 'string)

(defcustom sunrise-virtual-listing-switches "-ald"
  "Listing switches for building buffers in `sunrise-virtual-mode'.
Should not contain the -D option. See also `sunrise-listing-switches'."
  :group 'sunrise
  :type 'string)

(defcustom sunrise-cursor-follows-mouse t
  "Determines whether the cursor inside the Sunrise panes should
follow the mouse in graphical environments."
  :group 'sunrise
  :type 'boolean
  :set (defun sunrise-set-cursor-follows-mouse (symbol value)
         "Setter function for the `sunrise-set-cursor-follows-mouse' custom option."
         (mapc (lambda (buf)
                 (with-current-buffer buf
                   (when (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
                     (setq track-mouse value))))
               (buffer-list))
         (set-default symbol value)))

(defcustom sunrise-mouse-events-threshold 10
  "Number of mouse movement events to ignore before following it
with the cursor. This helps to avoid capturing accidentally the
cursor when Sunrise is activated."
  :group 'sunrise
  :type 'integer)

(defcustom sunrise-avfs-root nil
  "Root of the AVFS virtual filesystem used for navigating compressed archives.
Setting this value activates AVFS support."
  :group 'sunrise
  :type '(choice
          (const :tag "AVFS support disabled" nil)
          (const :tag "~/.avfs (default mountavfs mount point)" "~/.avfs")
          (directory :tag "Other AVFS root directory")))

(defcustom sunrise-avfs-handlers-alist '(("\\.[jwesh]ar$" . "#uzip/")
                                    ("\\.wsar$"      . "#uzip/")
                                    ("\\.xpi$"       . "#uzip/")
                                    ("\\.apk$"       . "#uzip/")
                                    ("\\.iso$"       . "#iso9660/")
                                    ("\\.patch$"     . "#/")
                                    ("\\.txz$"       . "#/")
                                    ("."             . "#/"))
  "List of AVFS handlers to manage specific file extensions."
  :group 'sunrise
  :type 'alist)

(defcustom sunrise-md5-shell-command "md5sum %f | cut -d' ' -f1 2>/dev/null"
  "Shell command to use for calculating MD5 sums for files.
Used when comparing directories using the ``(c)ontents'' option.
Use %f as a placeholder for the name of the file."
  :group 'sunrise
  :type 'string)

(defcustom sunrise-window-split-style 'horizontal
  "The current window split configuration.
May be `horizontal', `vertical' or `top'."
  :group 'sunrise
  :type '(choice
          (const horizontal)
          (const vertical)
          (const top)))

(defcustom sunrise-windows-locked t
  "When non-nil, vertical size of the panes will remain constant."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-windows-default-ratio 66
  "Percentage of the total height of the frame to use by default for the Sunrise
Commander panes."
  :group 'sunrise
  :type 'integer
  :set (defun sunrise-set-windows-default-ratio (symbol value)
         "Setter function for the `sunrise-windows-default-ratio' custom option."
         (if (and (integerp value) (>= value 0) (<= value 100))
             (set-default symbol value)
           (error "Invalid value: %s" value))))

(defcustom sunrise-history-length 20
  "Number of entries to keep in each pane's history rings."
  :group 'sunrise
  :type 'integer)

(defcustom sunrise-kill-unused-buffers t
  "Whether buffers should be killed automatically by Sunrise when not displayed
in any of the panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-kill-quick-view-buffers t
  "Whether opening a new buffer in quick-view mode should kill any other buffer
opened previously in the same manner."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-confirm-kill-viewer t
  "Whether to ask for confirmation before killing a buffer opened in quick-view
mode."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-attributes-display-mask nil
  "Contols hiding/transforming columns with `sunrise-toggle-attributes'.
If set, its value must be a list of symbols, one for each
attributes column. If the symbol is nil, then the corresponding
column will be hidden, and if it's not nil then the column will
be left untouched. The symbol may also be the name of a function
that takes one string argument and evaluates to a different
string -- in this case this function will be used to transform
the contents of the corresponding column and its result will be
displayed instead."
  :group 'sunrise
  :type '(repeat symbol))

(defcustom sunrise-fast-backup-extension ".bak"
  "Determines the extension to append to the names of new files
created with the `sunrise-fast-backup-files' function (@!). This can
be either a simple string or an s-expression to be evaluated at
run-time."
  :group 'sunrise
  :type '(choice
          (string :tag "Literal text")
          (sexp :tag "Symbolic expression")))

(defcustom sunrise-traditional-other-window nil
  "Sunrise modifies the behavior of the `other-window' command,
so that focus is always given to the currently selected pane when
switching from external windows. If you'd prefer the original
Emacs behavior instead, then set this flag to t."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-fuzzy-negation-character ?!
  "Character to use for negating patterns when fuzzy-narrowing a pane."
  :group 'sunrise
  :type '(choice
          (const :tag "Fuzzy matching negation disabled" nil)
          (character :tag "Fuzzy matching negation character" ?!)))

(defcustom sunrise-init-hook nil
  "List of functions to be called before the Sunrise panes are displayed."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sunrise-start-hook nil
  "List of functions to be called after the Sunrise panes are displayed."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sunrise-refresh-hook nil
  "List of functions to be called every time a pane is refreshed."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sunrise-quit-hook nil
  "List of functions to be called after the Sunrise panes are hidden."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sunrise-recursive-grep-supported t
  "Whether the command specified by `sunrise-grep-command' supports
the 'recursive' (-r) option."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-grep-command "grep"
  "Full path to the grep command for Sunrise to use in grep
  operations. In contrast to `grep-command' this one does *not*
  support any options."
  :group 'sunrise
  :type 'string)

(defvar sunrise-restore-buffer nil
  "Buffer to restore when Sunrise quits.")

(defvar sunrise-prior-window-configuration nil
  "Window configuration before Sunrise was started.")

(defvar sunrise-running nil
  "True when Sunrise commander mode is running.")

(defvar sunrise-synchronized nil
  "True when synchronized navigation is on")

(defvar sunrise-current-window-overlay nil
  "Holds the current overlay which marks the current Dired buffer.")

(defvar sunrise-clex-hotchar-overlay nil
  "Overlay used to highlight the hot character (%) during CLEX operations.")

(defvar sunrise-left-directory "~/"
  "Dired directory for the left window. See variable `dired-directory'.")

(defvar sunrise-left-buffer nil
  "Dired buffer for the left window.")

(defvar sunrise-left-window nil
  "The left window of Dired.")

(defvar sunrise-right-directory "~/"
  "Dired directory for the right window. See variable `dired-directory'.")

(defvar sunrise-right-buffer nil
  "Dired buffer for the right window.")

(defvar sunrise-right-window nil
  "The right window of Dired.")

(defvar sunrise-current-frame nil
  "The frame Sunrise is active on (if any).")

(defvar sunrise-this-directory "~/"
  "Dired directory in the active pane.
This isn't necessarily the same as `dired-directory'.")

(defvar sunrise-other-directory "~/"
  "Dired directory in the passive pane.")

(defvar sunrise-selected-window 'left
  "The window to select when Sunrise starts up.")

(defvar sunrise-selected-window-width nil
  "The width the selected window should have on startup.")

(defvar sunrise-history-registry '((left) (right))
  "Registry of visited directories for both panes.")

(defvar sunrise-history-stack '((left 0 . 0) (right 0 . 0))
  "History stack counters.
The first counter on each side tracks (by value) the absolute
depth of the stack and (by sign) the direction it is currently
being traversed. The second counter points at the position of the
element that is immediately beneath the top of the stack.")

(defvar sunrise-ti-openterms nil
  "Stack of currently open terminal buffers.")

(defvar sunrise-ediff-on nil
  "Flag that indicates whether an `ediff' is being currently done.")

(defvar sunrise-clex-on nil
  "Flag that indicates that a CLEX operation is taking place.")

(defvar sunrise-virtual-buffer nil
  "Local flag that indicates the current buffer was originally in
  VIRTUAL mode.")

(defvar sunrise-dired-directory ""
  "Directory inside which `sunrise-mode' is currently active.")

(defvar sunrise-start-message
  "Been coding all night? Enjoy the Sunrise! (or press q to quit)"
  "Message to display when Sunrise is started.")

(defvar sunrise-panes-height nil
  "Current height of the pane windows.
Initial value is 2/3 the viewport height.")

(defvar sunrise-current-path-faces nil
  "List of faces to display the path in the current pane (first wins)")
(make-variable-buffer-local 'sunrise-current-path-faces)

(defvar sunrise-inhibit-highlight nil
  "Special variable used to temporarily inhibit highlighting in panes.")

(defvar sunrise-inhibit-switch nil
  "Special variable used to inhibit switching from one pane to the other.")

(defvar sunrise-find-items nil
  "Special variable used by `sunrise-find' to control the scope of find operations.")

(defvar sunrise-desktop-save-handlers nil
  "List of extension-defined handlers to save Sunrise buffers with desktop.")

(defvar sunrise-desktop-restore-handlers nil
  "List of extension-defined handlers to restore Sunrise buffers from desktop.")

(defvar sunrise-backup-buffer nil
  "Variable holding a buffer-local value of the backup buffer.")
(make-variable-buffer-local 'sunrise-backup-buffer)

(defvar sunrise-goto-dir-function nil
  "Function to use to navigate to a given directory, or nil to do
the default.  The function receives one argument DIR, which is
the directory to go to.")

(defvar sunrise-mouse-events-count 0
  "Number of mouse movement events received before activating the
  `sunrise-cursor-follows-mouse' feature.")

(defconst sunrise-side-lookup (list '(left . right) '(right . left))
  "Trivial alist used by the Sunrise Commander to lookup its own passive side.")

(defface sunrise-active-path-face
  '((((type tty) (class color) (min-colors 8))
     :background "green" :foreground "yellow" :bold t)
    (((type tty) (class mono)) :inverse-video t)
    (t :background "#ace6ac" :foreground "yellow" :bold t :height 120))
  "Face of the directory path in the active pane."
  :group 'sunrise)

(defface sunrise-passive-path-face
  '((((type tty) (class color) (min-colors 8) (background dark))
     :background "black" :foreground "cyan")
    (((type tty) (class color) (min-colors 8) (background light))
     :background "white" :foreground "cyan")
    (t :background "white" :foreground "lightgray" :bold t :height 120))
  "Face of the directory path in the passive pane."
  :group 'sunrise)

(defface sunrise-editing-path-face
  '((t :background "red" :foreground "yellow" :bold t :height 120))
  "Face of the directory path in the active pane while in editable pane mode."
  :group 'sunrise)

(defface sunrise-highlight-path-face
  '((t :background "yellow" :foreground "#ace6ac" :bold t :height 120))
  "Face of the directory path on mouse hover."
  :group 'sunrise)

(defface sunrise-clex-hotchar-face
  '((t :foreground "red" :bold t))
  "Face of the hot character (%) in CLEX mode.
Indicates that a CLEX substitution may be about to happen."
  :group 'sunrise)

;;; ============================================================================
;;; This is the core of Sunrise: the main idea is to apply `sunrise-mode' only inside
;;; Sunrise buffers while keeping all of `dired-mode' untouched.

;;; preserve this variable when switching from `dired-mode' to another mode
(put 'dired-subdir-alist 'permanent-local t)

;;;###autoload
(define-derived-mode sunrise-mode dired-mode "Sunrise Commander"
  "Two-pane file manager for Emacs based on Dired and inspired by MC.
The following keybindings are available:

        /, j .......... go to directory
        p, n .......... move cursor up/down
        M-p, M-n ...... move cursor up/down in passive pane
        ^, J .......... go to parent directory
        M-^, M-J ...... go to parent directory in passive pane
        Tab ........... switch to other pane
        C-Tab.......... switch to viewer window
        C-c Tab ....... switch to viewer window (console compatible)
        RET, f ........ visit selected file/directory
        M-RET, M-f .... visit selected file/directory in passive pane
        C-c RET ....... visit selected in passive pane (console compatible)
        b ............. visit selected file/directory in default browser
        F ............. visit all marked files, each in its own window
        C-u F ......... visit all marked files in the background
        o,v ........... quick visit selected file (scroll with C-M-v, C-M-S-v)
        C-u o, C-u v .. kill quick-visited buffer (restores normal scrolling)
        X ............. execute selected file
        C-u X.......... execute selected file with arguments

        + ............. create new directory
        M-+ ........... create new empty file(s)
        C ............. copy marked (or current) files and directories
        R ............. rename marked (or current) files and directories
        D ............. delete marked (or current) files and directories
        S ............. soft-link selected file/directory to passive pane
        Y ............. do relative soft-link of selected file in passive pane
        H ............. hard-link selected file to passive pane
        K ............. clone selected files and directories into passive pane
        N ............. in place copy/rename/link marked (or current) entries
        M-C ........... copy (using traditional dired-do-copy)
        M-R ........... rename (using traditional dired-do-rename)
        M-D ........... delete (using traditional dired-do-delete)
        M-S............ soft-link (using traditional dired-do-symlink)
        M-Y............ do relative soft-link (traditional dired-do-relsymlink)
        M-H............ hard-link selected file/directory (dired-do-hardlink)
        A ............. search marked files for regular expression
        Q ............. perform query-replace-regexp on marked files
        C-q ........... search occurrences of a string in marked files
        C-c s ......... start a \"sticky\" interactive search in the current pane

        M-a ........... move to beginning of current directory
        M-e ........... move to end of current directory
        M-y ........... go to previous directory in history
        M-u ........... go to next directory in history
        C-M-y ......... go to previous directory in history on passive pane
        C-M-u ......... go to next directory in history on passive pane

        g, C-c C-c .... refresh pane
        s ............. sort entries (by name, number, size, time or extension)
        r ............. reverse the order of entries in the active pane (sticky)
        C-o ........... show/hide hidden files (requires dired-omit-mode)
        C-Backspace ... hide/show file attributes in pane
        C-c Backspace . hide/show file attributes in pane (console compatible)
        y ............. show file type / size of selected files and directories.
        M-l ........... truncate/continue long lines in pane
        C-c v ......... put current panel in VIRTUAL mode
        C-c C-v ....... create new pure VIRTUAL buffer
        C-c C-w ....... browse directory tree using w3m

        M-t ........... transpose panes
        M-o ........... synchronize panes
        C-c C-s ....... change panes layout (vertical/horizontal/top-only)
        [ ............. enlarges the right pane by 5 columns
        ] ............. enlarges the left pane by 5 columns
        } ............. enlarges the panes vertically by 1 row
        C-} ........... enlarges the panes vertically as much as it can
        C-c } ......... enlarges the panes vertically as much as it can
        { ............. shrinks the panes vertically by 1 row
        C-{ ........... shrinks the panes vertically as much as it can
        C-c { ......... shrinks the panes vertically as much as it can
        \\ ............. restores the size of all windows back to «normal»
        C-c C-z ....... enable/disable synchronized navigation

        C-= ........... smart compare files (ediff)
        C-c = ......... smart compare files (console compatible)
        = ............. fast smart compare files (plain diff)
        C-M-= ......... compare panes
        C-x = ......... compare panes (console compatible)

        C-c C-f ....... execute Find-dired in Sunrise VIRTUAL mode
        C-c C-n ....... execute find-Name-dired in Sunrise VIRTUAL mode
        C-u C-c C-g ... execute find-Grep-dired with additional grep options
        C-c C-g ....... execute grep in Sunrise VIRTUAL mode
        C-c C-l ....... execute Locate in Sunrise VIRTUAL mode
        C-c C-r ....... browse list of Recently visited files (requires recentf)
        C-c C-c ....... [after find, locate or recent] dismiss virtual buffer
        C-c / ......... narrow the contents of current pane using fuzzy matching
        C-c b ......... partial Branch view of selected items in current pane
        C-c p ......... Prune paths matching regular expression from current pane
        ; ............. follow file (go to same directory as selected file)
        M-; ........... follow file in passive pane
        C-M-o ......... follow a projection of current directory in passive pane

        C-> ........... save named checkpoint (a.k.a. \"bookmark panes\")
        C-c > ......... save named checkpoint (console compatible)
        C-.    ........ restore named checkpoint
        C-c .  ........ restore named checkpoint

        C-x C-q ....... put pane in Editable Dired mode (commit with C-c C-c)
        @! ............ fast backup files (not dirs!), each to [filename].bak

        C-c t ......... open new terminal or switch to already open one
        C-c T ......... open terminal AND/OR change directory to current
        C-c C-t ....... open always a new terminal in current directory
        C-c M-t ....... open a new terminal using an alternative shell program
        q, C-x k ...... quit Sunrise Commander, restore previous window setup
        M-q ........... quit Sunrise Commander, don't restore previous windows

Additionally, the following traditional commander-style keybindings are provided
\(these may be disabled by customizing the `sunrise-use-commander-keys' option):

        F2 ............ go to directory
        F3 ............ quick visit selected file
        F4 ............ visit selected file
        F5 ............ copy marked (or current) files and directories
        F6 ............ rename marked (or current) files and directories
        F7 ............ create new directory
        F8 ............ delete marked (or current) files and directories
        F10 ........... quit Sunrise Commander
        C-F3 .......... sort contents of current pane by name
        C-F4 .......... sort contents of current pane by extension
        C-F5 .......... sort contents of current pane by time
        C-F6 .......... sort contents of current pane by size
        C-F7 .......... sort contents of current pane numerically
        S-F7 .......... soft-link selected file/directory to passive pane
        Insert ........ mark file
        C-PgUp ........ go to parent directory

Any other dired keybinding (not overridden by any of the above) can be used in
Sunrise, like G for changing group, M for changing mode and so on.

Some more bindings are available in terminals opened using any of the Sunrise
functions (i.e. one of: C-c t, C-c T, C-c C-t, C-c M-t):

        C-c Tab ....... switch focus to the active pane
        C-c t ......... cycle through all currently open terminals
        C-c T ......... cd to the directory in the active pane
        C-c C-t ....... open new terminal, cd to directory in the active pane
        C-c ; ......... follow the current directory in the active pane
        C-c { ......... shrink the panes vertically as much as possible
        C-c } ......... enlarge the panes vertically as much as possible
        C-c \\ ......... restore the size of all windows back to «normal»
        C-c C-j ....... put terminal in line mode
        C-c C-k ....... put terminal back in char mode

The following bindings are available only in line mode (eshell is considered to
be *always* in line mode):

        M-<up>, M-P ... move cursor up in the active pane
        M-<down>, M-N . move cursor down in the active pane
        M-Return ...... visit selected file/directory in the active pane
        M-J ........... go to parent directory in the active pane
        M-G ........... refresh active pane
        M-Tab ......... switch to passive pane (without leaving the terminal)
        M-M ........... mark selected file/directory in the active pane
        M-Backspace ... unmark previous file/directory in the active pane
        M-U ........... remove all marks from the active pane
        C-Tab ......... switch focus to the active pane

In a terminal in line mode the following substitutions are also performed
automatically:

       %f - expands to the currently selected file in the left pane
       %F - expands to the currently selected file in the right pane
       %m - expands to the list of paths of all marked files in the left pane
       %M - expands to the list of paths of all marked files in the right pane
       %n - expands to the list of names of all marked files in the left pane
       %N - expands to the list of names of all marked files in the right pane
       %d - expands to the current directory in the left pane
       %D - expands to the current directory in the right pane
       %a - expands to the list of paths of all marked files in the active pane
       %A - expands to the current directory in the active pane
       %p - expands to the list of paths of all marked files in the passive pane
       %P - expands to the current directory in the passive pane
       %% - inserts a single % sign.
"
  :group 'sunrise
  (unless (string-match "\\(Sunrise\\)" (buffer-name))
    (rename-buffer (concat (buffer-name) " (Sunrise)") t))
  (set-keymap-parent sunrise-mode-map dired-mode-map)
  (sunrise-highlight)
  (dired-omit-mode dired-omit-mode)

  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows (sunrise-truncate-v t))

  (set (make-local-variable 'buffer-read-only) t)
  (set (make-local-variable 'dired-header-face) 'sunrise-passive-path-face)
  (set (make-local-variable 'dired-recursive-deletes) 'top)
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'desktop-save-buffer) 'sunrise-desktop-save-buffer)
  (set (make-local-variable 'revert-buffer-function) 'sunrise-revert-buffer)
  (set (make-local-variable 'buffer-quit-function) 'sunrise-quit)
  (set (make-local-variable 'sunrise-show-file-attributes) sunrise-show-file-attributes)
  (set (make-local-variable 'mouse-1-click-follows-link) nil)
  (set (make-local-variable 'track-mouse) sunrise-cursor-follows-mouse)
  (set (make-local-variable 'hl-line-sticky-flag) nil)
  (hl-line-mode 1)
)

;;;###autoload
(define-derived-mode sunrise-virtual-mode dired-virtual-mode "Sunrise VIRTUAL"
  "Sunrise Commander Virtual Mode. Useful for reusing find and locate results."
  :group 'sunrise
  (set-keymap-parent sunrise-virtual-mode-map sunrise-mode-map)
  (sunrise-highlight)
  (enriched-mode -1)

  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows (sunrise-truncate-v t))

  (set (make-local-variable 'buffer-read-only) t)
  (set (make-local-variable 'dired-header-face) 'sunrise-passive-path-face)
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'desktop-save-buffer) 'sunrise-desktop-save-buffer)
  (set (make-local-variable 'revert-buffer-function) 'sunrise-revert-buffer)
  (set (make-local-variable 'buffer-quit-function) 'sunrise-quit)
  (set (make-local-variable 'sunrise-show-file-attributes) sunrise-show-file-attributes)
  (set (make-local-variable 'mouse-1-click-follows-link) nil)
  (set (make-local-variable 'track-mouse) sunrise-cursor-follows-mouse)
  (set (make-local-variable 'hl-line-sticky-flag) nil)
  (hl-line-mode 1)

  (define-key sunrise-virtual-mode-map "\C-c\C-c" 'sunrise-virtual-dismiss)
  (define-key sunrise-virtual-mode-map "\C-cv"    'sunrise-backup-buffer))

(defmacro sunrise-within (dir form)
  "Evaluate FORM in Sunrise context."
  `(unwind-protect
       (progn
         (setq sunrise-dired-directory
               (file-name-as-directory (abbreviate-file-name ,dir)))
         (ad-activate 'dired-find-buffer-nocreate)
         ,form)
     (ad-deactivate 'dired-find-buffer-nocreate)
     (setq sunrise-dired-directory "")))

(defmacro sunrise-save-aspect (&rest body)
  "Restore omit mode, hidden attributes and point after a directory transition."
  `(let ((inhibit-read-only t)
         (omit (or dired-omit-mode -1))
         (attrs (eval 'sunrise-show-file-attributes))
         (path-faces sunrise-current-path-faces))
     ,@body
     (dired-omit-mode omit)
     (if path-faces
         (setq sunrise-current-path-faces path-faces))
     (if (string= "NUMBER" (get sunrise-selected-window 'sorting-order))
         (sunrise-sort-by-operation 'sunrise-numerical-sort-op))
     (if (get sunrise-selected-window 'sorting-reverse)
         (sunrise-reverse-pane))
     (setq sunrise-show-file-attributes attrs)
     (sunrise-display-attributes (point-min) (point-max) sunrise-show-file-attributes)
     (sunrise-restore-point-if-same-buffer)))

(defmacro sunrise-save-selected-window (&rest body)
  "Execute BODY, then select the previously selected window.
During the operation, `sunrise-inhibit-switch' is set to t.
Uses `save-selected-window' internally."
  `(let ((sunrise-inhibit-switch t))
     (save-selected-window
       ,@body)))

(defmacro sunrise-alternate-buffer (form)
  "Execute FORM in a new buffer, after killing the previous one."
  `(let ((dispose nil))
     (unless (or (not (or dired-directory (eq major-mode 'sunrise-tree-mode)))
                 (eq sunrise-left-buffer sunrise-right-buffer))
       (setq dispose (current-buffer)))
     ,form
     (setq sunrise-this-directory default-directory)
     (sunrise-keep-buffer)
     (sunrise-highlight)
     (when (and sunrise-kill-unused-buffers (buffer-live-p dispose))
       (with-current-buffer dispose
         (bury-buffer)
         (set-buffer-modified-p nil)
         (unless (kill-buffer dispose)
           (kill-local-variable 'sunrise-current-path-faces))))))

(defun sunrise-assert-other ()
  "Signal an error if we have no other pane."
  (unless (window-live-p (sunrise-other))
    (user-error "No other Sunrise Commander pane")))

(defmacro sunrise-in-other (form)
  "Execute FORM in the context of the passive pane.
Helper macro for passive & synchronized navigation."
  `(let ((home sunrise-selected-window))
     (let ((sunrise-inhibit-highlight t))
       (if sunrise-synchronized ,form)
       (sunrise-change-window)
       (condition-case description
           ,form
         (error (message (cadr description)))))
     (if (not sunrise-running)
         (sunrise-select-window home)
       (run-hooks 'sunrise-refresh-hook)
       (sunrise-change-window))))

(defmacro sunrise-silently (&rest body)
  "Inhibit calls to `message' in BODY."
  `(cl-letf (((symbol-function 'message) (lambda (_msg &rest _args) (ignore))))
     ,@body))

(eval-and-compile
  (defun sunrise-symbol (side type)
    "Synthesize Sunrise symbols (`sunrise-left-buffer', `sunrise-right-window', etc.)."
    (intern (concat "sunrise-" (symbol-name side) "-" (symbol-name type)))))

(defun sunrise-dired-mode ()
  "Set Sunrise mode in every Dired buffer opened in Sunrise (called in a hook)."
  (if (and sunrise-running
           (eq (selected-frame) sunrise-current-frame)
           (sunrise-equal-dirs dired-directory default-directory)
           (not (eq major-mode 'sunrise-mode)))
      (let ((dired-listing-switches dired-listing-switches)
            (sorting-options (or (get sunrise-selected-window 'sorting-options) "")))
        (unless (string-match tramp-file-name-regexp default-directory)
          (setq dired-listing-switches
                (concat sunrise-listing-switches sorting-options)))
        (sunrise-mode)
        (dired-unadvertise dired-directory))))
(add-hook 'dired-before-readin-hook 'sunrise-dired-mode)

(defun sunrise-bookmark-jump ()
  "Handle panes opened from bookmarks in Sunrise."
  (when (and sunrise-running
             (memq (selected-window) (list sunrise-left-window sunrise-right-window)))
    (let ((last-buf (symbol-value (sunrise-symbol sunrise-selected-window 'buffer))))
      (setq dired-omit-mode (with-current-buffer last-buf dired-omit-mode))
      (setq sunrise-this-directory default-directory)
      (if (sunrise-equal-dirs sunrise-this-directory sunrise-other-directory)
          (sunrise-synchronize-panes t)
        (revert-buffer))
      (sunrise-keep-buffer)
      (unless (memq last-buf (list (current-buffer) (sunrise-other 'buffer)))
        (kill-buffer last-buf)))))
(add-hook 'bookmark-after-jump-hook 'sunrise-bookmark-jump)

(defun sunrise-virtualize-pane ()
  "Put the current normal view in VIRTUAL mode."
  (interactive)
  (when (eq major-mode 'sunrise-mode)
    (let ((focus (dired-get-filename 'verbatim t)))
      (sunrise-save-aspect
       (when (eq sunrise-left-buffer sunrise-right-buffer)
         (dired default-directory)
         (sunrise-keep-buffer))
       (sunrise-virtual-mode))
      (if focus (sunrise-focus-filename focus)))))

(defun sunrise-virtual-dismiss ()
  "Restore normal pane view in Sunrise VIRTUAL mode."
  (interactive)
  (when (eq major-mode 'sunrise-virtual-mode)
    (let ((focus (dired-get-filename 'verbatim t)))
      (sunrise-process-kill)
      (sunrise-save-aspect
       (sunrise-alternate-buffer (sunrise-goto-dir sunrise-this-directory))
       (if focus (sunrise-focus-filename focus))
       (revert-buffer)))))

(defun sunrise-backup-buffer ()
  "Create a backup copy of the current buffer.
Used as a cache during revert operations."
  (interactive)
  (sunrise-kill-backup-buffer)
  (let ((buf (current-buffer)))
    (setq sunrise-backup-buffer (generate-new-buffer "*Sunrise Backup*"))
    (with-current-buffer sunrise-backup-buffer
      (insert-buffer-substring buf))
    (run-hooks 'sunrise-refresh-hook)))

(defun sunrise-kill-backup-buffer ()
  "Kill the backup buffer associated to the current one, if there is any."
  (when (buffer-live-p sunrise-backup-buffer)
    (kill-buffer sunrise-backup-buffer)
    (setq sunrise-backup-buffer nil)))
(add-hook 'kill-buffer-hook       'sunrise-kill-backup-buffer)
(add-hook 'change-major-mode-hook 'sunrise-kill-backup-buffer)

(add-to-list 'enriched-translations '(invisible (t "x-invisible")))
(defun sunrise-enrich-buffer ()
  "Activate `enriched-mode' before saving a Sunrise buffer to a file.
This is done so all its dired-filename attributes are kept in the file."
  (if (memq major-mode '(sunrise-mode sunrise-virtual-mode))
      (enriched-mode 1)))
(add-hook 'before-save-hook 'sunrise-enrich-buffer)

(defun sunrise-extend-with (extension &optional filename)
  "Try to enhance Sunrise with EXTENSION (argument must be a symbol).
An extension can be loaded from optional FILENAME. If found, the extension is
immediately loaded, but only if `sunrise-autoload-extensions' is not nil."
  (when sunrise-autoload-extensions
    (require extension filename t)))

(defadvice dired-find-buffer-nocreate
    (before sunrise-advice-findbuffer (dirname &optional mode))
  "A hack to avoid some Dired mode quirks in the Sunrise Commander."
  (if (sunrise-equal-dirs sunrise-dired-directory dirname)
      (setq mode 'sunrise-mode)))
;; ^--- activated by sunrise-within macro

(defadvice dired-dwim-target-directory
    (around sunrise-advice-dwim-target ())
  "Tweak the target directory guessing mechanism when Sunrise Commander is on."
  (if (and sunrise-running (eq (selected-frame) sunrise-current-frame))
      (setq ad-return-value sunrise-other-directory)
    ad-do-it))
(ad-activate 'dired-dwim-target-directory)

(defadvice select-window
    (after sunrise-ad-select-window (window &optional norecord))
  "Detect Sunrise pane switches and update tracking state accordingly."
  (sunrise-detect-switch))
(ad-activate 'select-window)

(defadvice other-window
    (around sunrise-advice-other-window (count &optional all-frames))
  "Select the correct Sunrise Commander pane when switching from other windows."
  (if (or (not sunrise-running) sunrise-ediff-on)
      ad-do-it
    (let ((from (selected-window))
          (to (next-window)))
      (if (or sunrise-traditional-other-window
              (not (memq to (list sunrise-left-window sunrise-right-window)))
              (memq from (list sunrise-left-window sunrise-right-window)))
          ad-do-it
        (sunrise-select-window sunrise-selected-window))))
  (sunrise-detect-switch))
(ad-activate 'other-window)

(defadvice use-hard-newlines
    (around sunrise-advice-use-hard-newlines (&optional arg insert))
  "Stop asking if I want hard lines the in Sunrise Commander, just guess."
  (if (memq major-mode '(sunrise-mode sunrise-virtual-mode))
      (let ((inhibit-read-only t))
        (setq insert 'guess)
        ad-do-it)
    ad-do-it))
(ad-activate 'use-hard-newlines)

(defadvice dired-insert-set-properties
    (around sunrise-advice-dired-insert-set-properties (beg end))
  "Manage hidden attributes in files added externally (e.g. from find-dired) to
the Sunrise Commander."
  (if (not (memq major-mode '(sunrise-mode sunrise-virtual-mode)))
      ad-do-it
    (with-no-warnings
      (sunrise-display-attributes beg end sunrise-show-file-attributes))))
(ad-activate 'dired-insert-set-properties)

;;; ============================================================================
;;; Sunrise Commander keybindings:

(define-key sunrise-mode-map "\C-m"        'sunrise-advertised-find-file)
(define-key sunrise-mode-map "f"           'sunrise-advertised-find-file)
(define-key sunrise-mode-map "X"           'sunrise-advertised-execute-file)
(define-key sunrise-mode-map "o"           'sunrise-quick-view)
(define-key sunrise-mode-map "v"           'sunrise-quick-view)
(define-key sunrise-mode-map "/"           'sunrise-goto-dir)
(define-key sunrise-mode-map "j"           'sunrise-goto-dir)
(define-key sunrise-mode-map "^"           'sunrise-dired-prev-subdir)
(define-key sunrise-mode-map "J"           'sunrise-dired-prev-subdir)
(define-key sunrise-mode-map ";"           'sunrise-follow-file)
(define-key sunrise-mode-map "\M-t"        'sunrise-transpose-panes)
(define-key sunrise-mode-map "\M-o"        'sunrise-synchronize-panes)
(define-key sunrise-mode-map "\C-\M-o"     'sunrise-project-path)
(define-key sunrise-mode-map "\M-y"        'sunrise-history-prev)
(define-key sunrise-mode-map "\M-u"        'sunrise-history-next)
(define-key sunrise-mode-map "\C-c>"       'sunrise-checkpoint-save)
(define-key sunrise-mode-map "\C-c."       'sunrise-checkpoint-restore)
(define-key sunrise-mode-map "\C-c\C-z"    'sunrise-sync)
(define-key sunrise-mode-map "\C-c\C-c"    'revert-buffer)

(define-key sunrise-mode-map "\t"          'sunrise-change-window)
(define-key sunrise-mode-map "\C-c\t"      'sunrise-select-viewer-window)
(define-key sunrise-mode-map "\M-a"        'sunrise-beginning-of-buffer)
(define-key sunrise-mode-map "\M-e"        'sunrise-end-of-buffer)
(define-key sunrise-mode-map "\C-c\C-s"    'sunrise-split-toggle)
(define-key sunrise-mode-map "]"           'sunrise-enlarge-left-pane)
(define-key sunrise-mode-map "["           'sunrise-enlarge-right-pane)
(define-key sunrise-mode-map "}"           'sunrise-enlarge-panes)
(define-key sunrise-mode-map "{"           'sunrise-shrink-panes)
(define-key sunrise-mode-map "\\"          'sunrise-lock-panes)
(define-key sunrise-mode-map "\C-c}"       'sunrise-max-lock-panes)
(define-key sunrise-mode-map "\C-c{"       'sunrise-min-lock-panes)
(define-key sunrise-mode-map "\C-o"        'dired-omit-mode)
(define-key sunrise-mode-map "b"           'sunrise-browse-file)
(define-key sunrise-mode-map "\C-c\C-w"    'sunrise-browse-pane)
(define-key sunrise-mode-map "\C-c\d"      'sunrise-toggle-attributes)
(define-key sunrise-mode-map "\M-l"        'sunrise-toggle-truncate-lines)
(define-key sunrise-mode-map "s"           'sunrise-interactive-sort)
(define-key sunrise-mode-map "r"           'sunrise-reverse-pane)
(define-key sunrise-mode-map "\C-e"        'sunrise-scroll-up)
(define-key sunrise-mode-map "\C-y"        'sunrise-scroll-down)
(define-key sunrise-mode-map " "           'sunrise-scroll-quick-view)
(define-key sunrise-mode-map "\M- "        'sunrise-scroll-quick-view-down)
(define-key sunrise-mode-map [?\S- ]       'sunrise-scroll-quick-view-down)

(define-key sunrise-mode-map "C"           'sunrise-do-copy)
(define-key sunrise-mode-map "K"           'sunrise-do-clone)
(define-key sunrise-mode-map "R"           'sunrise-do-rename)
(define-key sunrise-mode-map "D"           'sunrise-do-delete)
(define-key sunrise-mode-map "x"           'sunrise-do-flagged-delete)
(define-key sunrise-mode-map "S"           'sunrise-do-symlink)
(define-key sunrise-mode-map "Y"           'sunrise-do-relsymlink)
(define-key sunrise-mode-map "H"           'sunrise-do-hardlink)
(define-key sunrise-mode-map "N"           'sunrise-inplace)
(define-key sunrise-mode-map "\M-C"        'dired-do-copy)
(define-key sunrise-mode-map "\M-R"        'dired-do-rename)
(define-key sunrise-mode-map "\M-D"        'dired-do-delete)
(define-key sunrise-mode-map "\M-S"        'dired-do-symlink)
(define-key sunrise-mode-map "\M-Y"        'dired-do-relsymlink)
(define-key sunrise-mode-map "\M-H"        'dired-do-hardlink)
(define-key sunrise-mode-map "\C-x\C-q"    'sunrise-editable-pane)
(define-key sunrise-mode-map "@"           'sunrise-fast-backup-files)
(define-key sunrise-mode-map "\M-+"        'sunrise-create-files)

(define-key sunrise-mode-map "="           'sunrise-diff)
(define-key sunrise-mode-map "\C-c="       'sunrise-ediff)
(define-key sunrise-mode-map "\C-x="       'sunrise-compare-panes)

(define-key sunrise-mode-map "\C-c\C-f"    'sunrise-find)
(define-key sunrise-mode-map "\C-c\C-n"    'sunrise-find-name)
(define-key sunrise-mode-map "\C-c\C-g"    'sunrise-grep)
(define-key sunrise-mode-map "\C-cb"       'sunrise-flatten-branch)
(define-key sunrise-mode-map "\C-cp"       'sunrise-prune-paths)
(define-key sunrise-mode-map "\C-c\C-l"    'sunrise-locate)
(define-key sunrise-mode-map "\C-c/"       'sunrise-fuzzy-narrow)
(define-key sunrise-mode-map "\C-c\C-r"    'sunrise-recent-files)
(define-key sunrise-mode-map "\C-c\C-d"    'sunrise-recent-directories)
(define-key sunrise-mode-map "\C-cv"       'sunrise-virtualize-pane)
(define-key sunrise-mode-map "\C-c\C-v"    'sunrise-pure-virtual)
(define-key sunrise-mode-map "Q"           'sunrise-do-query-replace-regexp)
(define-key sunrise-mode-map "\C-q"        'sunrise-multi-occur)
(define-key sunrise-mode-map "F"           'sunrise-do-find-marked-files)
(define-key sunrise-mode-map "A"           'sunrise-do-search)
(define-key sunrise-mode-map "\C-cs"       'sunrise-sticky-isearch-forward)
(define-key sunrise-mode-map "\C-cr"       'sunrise-sticky-isearch-backward)
(define-key sunrise-mode-map "\C-x\C-f"    'sunrise-find-file)
(define-key sunrise-mode-map "y"           'sunrise-show-files-info)

(define-key sunrise-mode-map "\M-n"        'sunrise-next-line-other)
(define-key sunrise-mode-map [M-down]      'sunrise-next-line-other)
(define-key sunrise-mode-map [A-down]      'sunrise-next-line-other)
(define-key sunrise-mode-map "\M-p"        'sunrise-prev-line-other)
(define-key sunrise-mode-map [M-up]        'sunrise-prev-line-other)
(define-key sunrise-mode-map [A-up]        'sunrise-prev-line-other)
(define-key sunrise-mode-map "\M-j"        'sunrise-goto-dir-other)
(define-key sunrise-mode-map "\M-\C-m"     'sunrise-advertised-find-file-other)
(define-key sunrise-mode-map "\M-f"        'sunrise-advertised-find-file-other)
(define-key sunrise-mode-map "\C-c\C-m"    'sunrise-advertised-find-file-other)
(define-key sunrise-mode-map "\M-^"        'sunrise-prev-subdir-other)
(define-key sunrise-mode-map "\M-J"        'sunrise-prev-subdir-other)
(define-key sunrise-mode-map "\M-m"        'sunrise-mark-other)
(define-key sunrise-mode-map "\M-M"        'sunrise-unmark-backward-other)
(define-key sunrise-mode-map "\M-U"        'sunrise-unmark-all-marks-other)
(define-key sunrise-mode-map "\M-;"        'sunrise-follow-file-other)
(define-key sunrise-mode-map "\C-\M-y"     'sunrise-history-prev-other)
(define-key sunrise-mode-map "\C-\M-u"     'sunrise-history-next-other)

(define-key sunrise-mode-map "\C-ct"       'sunrise-term)
(define-key sunrise-mode-map "\C-cT"       'sunrise-term-cd)
(define-key sunrise-mode-map "\C-c\C-t"    'sunrise-term-cd-newterm)
(define-key sunrise-mode-map "\C-c\M-t"    'sunrise-term-cd-program)
(define-key sunrise-mode-map "\C-c;"       'sunrise-follow-viewer)
(define-key sunrise-mode-map "q"           'sunrise-quit)
(define-key sunrise-mode-map "\C-xk"       'sunrise-kill-pane-buffer)
(define-key sunrise-mode-map "\M-q"        'sunrise-cd)
(define-key sunrise-mode-map "h"           'sunrise-describe-mode)
(define-key sunrise-mode-map "?"           'sunrise-summary)
(define-key sunrise-mode-map "k"           'dired-do-kill-lines)
(define-key sunrise-mode-map [remap undo]  'sunrise-undo)
(define-key sunrise-mode-map [remap undo-only] 'sunrise-undo)
(define-key sunrise-mode-map [backspace]   'dired-unmark-backward)

(define-key sunrise-mode-map [mouse-1]        'sunrise-mouse-advertised-find-file)
(define-key sunrise-mode-map [mouse-2]        'sunrise-mouse-change-window)
(define-key sunrise-mode-map [mouse-movement] 'sunrise-mouse-move-cursor)

(define-key sunrise-mode-map [(control >)]         'sunrise-checkpoint-save)
(define-key sunrise-mode-map [(control .)]         'sunrise-checkpoint-restore)
(define-key sunrise-mode-map [(control tab)]       'sunrise-select-viewer-window)
(define-key sunrise-mode-map [(control backspace)] 'sunrise-toggle-attributes)
(define-key sunrise-mode-map [(control ?\=)]       'sunrise-ediff)
(define-key sunrise-mode-map [(control meta ?\=)]  'sunrise-compare-panes)
(define-key sunrise-mode-map [(control })]         'sunrise-max-lock-panes)
(define-key sunrise-mode-map [(control {)]         'sunrise-min-lock-panes)

(defvar sunrise-commander-keys
  '(([(f2)]            . sunrise-goto-dir)
    ([(f3)]            . sunrise-quick-view)
    ([(f4)]            . sunrise-advertised-find-file)
    ([(f5)]            . sunrise-do-copy)
    ([(f6)]            . sunrise-do-rename)
    ([(f7)]            . dired-create-directory)
    ([(f8)]            . sunrise-do-delete)
    ([(f10)]           . sunrise-quit)
    ([(control f3)]    . sunrise-sort-by-name)
    ([(control f4)]    . sunrise-sort-by-extension)
    ([(control f5)]    . sunrise-sort-by-time)
    ([(control f6)]    . sunrise-sort-by-size)
    ([(control f7)]    . sunrise-sort-by-number)
    ([(shift f7)]      . sunrise-do-symlink)
    ([(insert)]        . sunrise-mark-toggle)
    ([(control prior)] . sunrise-dired-prev-subdir))
  "Traditional commander-style keybindings for the Sunrise Commander.")

(defcustom sunrise-use-commander-keys t
  "Whether to use traditional commander-style function keys (F5 = copy, etc)"
  :group 'sunrise
  :type 'boolean
  :set (defun sunrise-set-commander-keys (symbol value)
         "Setter function for the `sunrise-use-commander-keys' custom option."
         (if value
             (mapc (lambda (x)
                     (define-key sunrise-mode-map (car x) (cdr x))) sunrise-commander-keys)
           (mapc (lambda (x)
                   (define-key sunrise-mode-map (car x) nil)) sunrise-commander-keys))
         (set-default symbol value)))

;;; ============================================================================
;;; Initialization and finalization functions:

;;;###autoload
(defun sunrise (&optional left-directory right-directory filename)
  "Toggle the Sunrise Commander file manager.
If LEFT-DIRECTORY is given, the left window will display that
directory (same for RIGHT-DIRECTORY). Specifying nil for any of
these values uses the default, ie. $HOME."
  (interactive)
  (message "Starting Sunrise Commander...")

  (if (not sunrise-running)
      (let ((welcome sunrise-start-message))
        (if left-directory
            (setq sunrise-left-directory left-directory))
        (if right-directory
            (setq sunrise-right-directory right-directory))

        (sunrise-switch-to-nonpane-buffer)
        (setq sunrise-restore-buffer (current-buffer)
              sunrise-current-frame (window-frame (selected-window))
              sunrise-prior-window-configuration (current-window-configuration))
        (sunrise-setup-windows)
        (if filename
            (condition-case description
                (sunrise-focus-filename (file-name-nondirectory filename))
              (error (setq welcome (cadr description)))))
        (setq sunrise-this-directory default-directory)
        (sunrise-highlight) ;;<-- W32Emacs needs this
        (hl-line-mode 1)
        (message "%s" welcome)
        (setq sunrise-running t))
    (let ((my-frame (window-frame (selected-window))))
      (sunrise-quit)
      (message "All life leaps out to greet the light...")
      (unless (eq my-frame (window-frame (selected-window)))
        (select-frame my-frame)
        (sunrise left-directory right-directory filename)))))

;;;###autoload
(defun sunrise-dired (&optional target switches)
  "Visit the given TARGET (file or directory) in `sunrise-mode'.
If provided, use SWITCHES instead of `sunrise-listing-switches'."
  (interactive
   (list
    (read-file-name "Visit (file or directory): " nil nil nil)))
  (let* ((target (expand-file-name (or target default-directory)))
         (file (if (file-directory-p target) nil target))
         (directory (if file (file-name-directory target) target))
         (dired-omit-mode (if sunrise-show-hidden-files -1 1))
         (sunrise-listing-switches (or switches sunrise-listing-switches)))
    (unless (file-readable-p directory)
      (error "%s is not readable!" (sunrise-directory-name-proper directory)))
    (unless (and sunrise-running (eq (selected-frame) sunrise-current-frame)) (sunrise))
    (sunrise-select-window sunrise-selected-window)
    (if file
        (sunrise-follow-file file)
      (sunrise-goto-dir directory))
    (hl-line-mode 1)
    (sunrise-display-attributes (point-min) (point-max) sunrise-show-file-attributes)
    (sunrise-this 'buffer)))

(defun sunrise-choose-cd-target ()
  "Select a suitable target directory for cd operations."
  (if (and sunrise-running (eq (selected-frame) sunrise-current-frame))
      sunrise-this-directory
    default-directory))

;;;###autoload
(defun sunrise-cd ()
  "Toggle the Sunrise Commander FM keeping the current file in focus.
If Sunrise is off, enable it and focus the file displayed in the current buffer.
If Sunrise is on, disable it and switch to the buffer currently displayed in the
viewer window."
  (interactive)
  (if (not (and sunrise-running
                (eq (window-frame sunrise-left-window) (selected-frame))))
      (sunrise-dired (or (buffer-file-name) (sunrise-choose-cd-target)))
    (sunrise-quit t)
    (message "Hast thou a charm to stay the morning-star in his steep course?")))

(defun sunrise-this (&optional type)
  "Return object of type TYPE corresponding to the active side of the manager.
If TYPE is not specified (nil), returns a symbol (`left' or `right').
If TYPE is `buffer' or `window', returns the corresponding buffer
or window."
  (if type
      (symbol-value (sunrise-symbol sunrise-selected-window type))
    sunrise-selected-window))

(defun sunrise-other (&optional type)
  "Return object of type TYPE corresponding to the passive side of the manager.
If TYPE is not specified (nil), returns a symbol (`left' or `right').
If TYPE is `buffer' or `window', returns the corresponding
buffer or window."
  (let ((side (cdr (assq sunrise-selected-window sunrise-side-lookup))))
    (if type
        (symbol-value (sunrise-symbol side type))
      side)))

;;; ============================================================================
;;; Window management functions:

(defmacro sunrise-setup-pane (side)
  "Helper macro for the function `sunrise-setup-windows'."
  `(let ((sunrise-selected-window ',side))
     (setq ,(sunrise-symbol side 'window) (selected-window))
     (if (buffer-live-p ,(sunrise-symbol side 'buffer))
         (progn
           (switch-to-buffer ,(sunrise-symbol side 'buffer))
           (setq ,(sunrise-symbol side 'directory) default-directory))
       (let ((sunrise-running t))
         (sunrise-dired ,(sunrise-symbol side 'directory))))))

(defun sunrise-setup-visible-panes ()
  "Set up sunrise on all visible panes."
  (sunrise-setup-pane left)
  (unless (eq sunrise-window-split-style 'top)
    (other-window 1)
    (sunrise-setup-pane right)))

(defun sunrise-setup-windows()
  "Set up the Sunrise window configuration (two windows in `sunrise-mode')."
  (run-hooks 'sunrise-init-hook)
  ;;get rid of all windows except one (not any of the panes!)
  (sunrise-select-viewer-window)
  (delete-other-windows)
  (if (buffer-live-p other-window-scroll-buffer)
      (switch-to-buffer other-window-scroll-buffer)
    (sunrise-switch-to-nonpane-buffer))

  ;;now create the viewer window
  (unless (and sunrise-panes-height (< sunrise-panes-height (frame-height)))
    (setq sunrise-panes-height (sunrise-get-panes-size)))
  (if (and (<= sunrise-panes-height (* 2 window-min-height))
           (eq sunrise-window-split-style 'vertical))
      (setq sunrise-panes-height (* 2 window-min-height)))
  (split-window (selected-window) sunrise-panes-height)

  (case sunrise-window-split-style
    (horizontal (split-window-horizontally))
    (vertical   (split-window-vertically))
    (top        (ignore))
    (t (error "Unrecognised `sunrise-window-split-style' value: %s"
              sunrise-window-split-style)))

  (sunrise-setup-visible-panes)

  ;;select the correct window
  (sunrise-select-window sunrise-selected-window)
  (sunrise-restore-panes-width)
  (run-hooks 'sunrise-start-hook))

(defun sunrise-switch-to-nonpane-buffer ()
  "Try to switch to a buffer that is *not* a Sunrise pane."
  (let ((start (current-buffer)))
    (while (and
              start
              (or (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
                  (memq (current-buffer) (list sunrise-left-buffer sunrise-right-buffer))))
        (bury-buffer)
        (if (eq start (current-buffer)) (setq start nil)))))

(defun sunrise-restore-prior-configuration ()
  "Restore the configuration stored in `sunrise-prior-window-configuration' if any.
Return t if a configuration to restore could be found, nil otherwise."
  (when sunrise-prior-window-configuration
    (set-window-configuration sunrise-prior-window-configuration)
    (if (buffer-live-p sunrise-restore-buffer)
        (set-buffer sunrise-restore-buffer))
    t))

(defun sunrise-lock-window (_frame)
  "Resize the left Sunrise pane to have the \"right\" size."
  (when sunrise-running
    (if (not (window-live-p sunrise-left-window))
        (setq sunrise-running nil)
      (let ((sunrise-windows-locked sunrise-windows-locked))
        (when (> window-min-height (- (frame-height)
                                      (window-height sunrise-left-window)))
          (setq sunrise-windows-locked nil))
        (and sunrise-windows-locked
             (not sunrise-ediff-on)
             (not (eq sunrise-window-split-style 'vertical))
             (window-live-p sunrise-left-window)
             (sunrise-save-selected-window
               (select-window sunrise-left-window)
               (let* ((sunrise-panes-height (or sunrise-panes-height (window-height)))
                      (my-delta (- sunrise-panes-height (window-height))))
                 (enlarge-window my-delta))
               (scroll-right)
               (when (window-live-p sunrise-right-window)
                 (select-window sunrise-right-window)
                 (scroll-right))))))))

;; This keeps the size of the Sunrise panes constant:
(add-hook 'window-size-change-functions 'sunrise-lock-window)

(defun sunrise-highlight(&optional face)
  "Set up the path line in the current buffer.
With optional FACE, register this face as the current face to display the active
path line."
  (when (and (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
             (not sunrise-inhibit-highlight))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (sunrise-hide-avfs-root)
        (sunrise-highlight-broken-links)
        (sunrise-graphical-highlight face)
        (sunrise-force-passive-highlight)
        (run-hooks 'sunrise-refresh-hook)))))

(defun sunrise-unhighlight (face)
  "Remove FACE from the list of faces of the active path line."
  (when face
    (setq sunrise-current-path-faces (delq face sunrise-current-path-faces))
    (overlay-put sunrise-current-window-overlay 'face
                 (or (car sunrise-current-path-faces) 'sunrise-active-path-face))))

(defun sunrise-hide-avfs-root ()
  "Hide the AVFS virtual filesystem root (if any) on the path line."
  (if sunrise-avfs-root
      (let ((start nil) (end nil)
            (next (search-forward sunrise-avfs-root (point-at-eol) t)))
        (if next (setq start (- next (length sunrise-avfs-root))))
        (while next
          (setq end (point)
                next (search-forward sunrise-avfs-root (point-at-eol) t)))
        (when end
          (add-text-properties start end '(invisible t))))))

(defun sunrise-highlight-broken-links ()
  "Mark broken symlinks with an exclamation mark."
  (let ((dired-marker-char ?!))
    (while (search-forward-regexp dired-re-sym nil t)
      (unless (or (not (eq 32 (char-after (line-beginning-position))))
                  (file-exists-p (dired-get-filename)))
        (dired-mark 1)))))

(defsubst sunrise-invalid-overlayp ()
  "Test for invalidity of the current buffer's graphical path line overlay.
Returns t if the overlay is no longer valid and should be replaced."
  (or (not (overlayp sunrise-current-window-overlay))
      (eq (overlay-start sunrise-current-window-overlay)
          (overlay-end sunrise-current-window-overlay))))

(defun sunrise-graphical-highlight (&optional face)
  "Set up the graphical path line in the current buffer.
\(Fancy fonts and clickable path.)"
  (let ((begin) (end) (inhibit-read-only t))

    (when (sunrise-invalid-overlayp)
      ;;determine begining and end
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp "\\S " nil t)
        (setq begin (1- (point)))
        (end-of-line)
        (setq end (1- (point))))

      ;;build overlay
      (when sunrise-current-window-overlay
        (delete-overlay sunrise-current-window-overlay))
      (set (make-local-variable 'sunrise-current-window-overlay)
           (make-overlay begin end))

      ;;path line hover effect:
      (add-text-properties
       begin
       end
       '(mouse-face sunrise-highlight-path-face
                    help-echo "click to move up")
       nil))
    (when face
      (setq sunrise-current-path-faces (cons face sunrise-current-path-faces)))
    (overlay-put sunrise-current-window-overlay 'face
                 (or (car sunrise-current-path-faces) 'sunrise-active-path-face))
    (overlay-put sunrise-current-window-overlay 'window (selected-window))))

(defun sunrise-force-passive-highlight (&optional revert)
  "Set up the graphical path line in the passive pane.
With optional argument REVERT, executes `revert-buffer' on the passive buffer."
    (unless (or (not (buffer-live-p (sunrise-other 'buffer)))
                (eq sunrise-left-buffer sunrise-right-buffer))
      (with-current-buffer (sunrise-other 'buffer)
        (when sunrise-current-window-overlay
          (delete-overlay sunrise-current-window-overlay))
        (when (and revert
                   (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode)))
          (revert-buffer)))))

(defun sunrise-quit (&optional norestore)
  "Quit Sunrise and restore Emacs to the previous state."
  (interactive)
  (if (not sunrise-running)
      (bury-buffer)
    (let ((buffer-read-only nil))
      (setq sunrise-running nil
            sunrise-current-frame nil)
      (sunrise-save-directories)
      (sunrise-save-panes-width)
      (when (or norestore (not (sunrise-restore-prior-configuration)))
        (sunrise-select-viewer-window)
        (delete-other-windows))
      (sunrise-bury-panes)
      (run-hooks 'sunrise-quit-hook))))

(add-hook 'delete-frame-functions
          (lambda (frame)
            (if (and sunrise-running (eq frame sunrise-current-frame)) (sunrise-quit))))

(defun sunrise-save-directories ()
  "Save current directories in the panes to use them at the next startup."
  (save-current-buffer
    (when (window-live-p sunrise-left-window)
      (set-buffer (window-buffer sunrise-left-window))
      (when (memq major-mode '(sunrise-mode sunrise-tree-mode))
        (setq sunrise-left-directory default-directory)
        (setq sunrise-left-buffer (current-buffer))))

    (when (window-live-p sunrise-right-window)
      (set-buffer (window-buffer sunrise-right-window))
      (when (memq major-mode '(sunrise-mode sunrise-tree-mode))
        (setq sunrise-right-directory default-directory)
        (setq sunrise-right-buffer (current-buffer))))))

(defun sunrise-bury-panes ()
  "Send both pane buffers to the end of the `buffer-list'."
  (mapc (lambda (x)
          (bury-buffer (symbol-value (sunrise-symbol x 'buffer))))
        '(left right)))

(defun sunrise-save-panes-width ()
  "Save the width of the panes to use them at the next startup."
  (unless sunrise-selected-window-width
    (if (and (window-live-p sunrise-left-window)
             (window-live-p sunrise-right-window))
        (setq sunrise-selected-window-width
              (window-width
               (symbol-value (sunrise-symbol sunrise-selected-window 'window))))
      (setq sunrise-selected-window-width t))))

(defun sunrise-restore-panes-width ()
  "Restore the last registered pane width."
  (when (and (eq sunrise-window-split-style 'horizontal)
             (numberp sunrise-selected-window-width))
    (enlarge-window-horizontally
     (min (- sunrise-selected-window-width (window-width))
          (- (frame-width) (window-width) window-min-width)))))

(defun sunrise-resize-panes (&optional reverse)
  "Enlarge (or shrink, if REVERSE is t) the left pane by 5 columns."
  (when (and (window-live-p sunrise-left-window)
             (window-live-p sunrise-right-window))
    (let ((direction (or (and reverse -1) 1)))
      (sunrise-save-selected-window
        (select-window sunrise-left-window)
        (enlarge-window-horizontally (* 5 direction))))
    (setq sunrise-selected-window-width nil)))

(defun sunrise-enlarge-left-pane ()
  "Enlarge the left pane by 5 columns."
  (interactive)
  (when (< (1+ window-min-width) (window-width sunrise-right-window))
      (sunrise-resize-panes)
      (sunrise-save-panes-width)))

(defun sunrise-enlarge-right-pane ()
  "Enlarge the right pane by 5 columns."
  (interactive)
  (when (< (1+ window-min-width) (window-width sunrise-left-window))
      (sunrise-resize-panes t)
      (sunrise-save-panes-width)))

(defun sunrise-get-panes-size (&optional size)
  "Tell what the maximal, minimal and normal pane sizes should be."
  (let ((frame (frame-height)))
    (case size
      (max (max (- frame window-min-height 1) 5))
      (min (min (1+ window-min-height) 5))
      (t  (/ (* sunrise-windows-default-ratio (frame-height)) 100)))))

(defun sunrise-enlarge-panes ()
  "Enlarge both panes vertically."
  (interactive)
  (let ((sunrise-windows-locked nil)
        (max (sunrise-get-panes-size 'max))
        (ratio 1)
        delta)
    (sunrise-save-selected-window
      (when (eq sunrise-window-split-style 'vertical)
        (select-window sunrise-right-window)
        (setq ratio 2)
        (setq delta (- max (window-height)))
        (if (> (/ max ratio) (window-height))
            (shrink-window (if (< 2 delta) -2 -1))))
      (select-window sunrise-left-window)
      (if (> (/ max ratio) (window-height))
          (shrink-window -1))
      (setq sunrise-panes-height (* (window-height) ratio)))))

(defun sunrise-shrink-panes ()
  "Shink both panes vertically."
  (interactive)
  (let ((sunrise-windows-locked nil)
        (min (sunrise-get-panes-size 'min))
        (ratio 1)
        delta)
    (sunrise-save-selected-window
      (when (eq sunrise-window-split-style 'vertical)
        (select-window sunrise-right-window)
        (setq ratio 2)
        (setq delta (- (window-height) min))
        (if (< min (window-height))
            (shrink-window (if (< 2 delta) 2 1))))
      (select-window sunrise-left-window)
      (if (< min (window-height))
          (shrink-window 1))
      (setq sunrise-panes-height (* (window-height) ratio)))))

(defun sunrise-lock-panes (&optional height)
  "Resize and lock the panes at some vertical position.
The optional argument determines the height to lock the panes at.
Valid values are `min' and `max'; given any other value, locks
the panes at normal position."
  (interactive)
  (if sunrise-running
    (if (not (and (window-live-p sunrise-left-window)
                  (or (window-live-p sunrise-right-window)
                      (eq sunrise-window-split-style 'top))))
        (sunrise-setup-windows)
      (setq sunrise-panes-height (sunrise-get-panes-size height))
      (let ((locked sunrise-windows-locked))
        (setq sunrise-windows-locked t)
        (if height
            (shrink-window 1)
          (setq sunrise-selected-window-width t)
          (balance-windows))
        (unless locked
          (sit-for 0.1)
          (setq sunrise-windows-locked nil))))
    (sunrise)))

(defun sunrise-max-lock-panes ()
  (interactive)
  (sunrise-save-panes-width)
  (sunrise-lock-panes 'max))

(defun sunrise-min-lock-panes ()
  (interactive)
  (sunrise-save-panes-width)
  (sunrise-lock-panes 'min))

(defun sunrise-mouse-disown-cursor ()
  "Reset the mouse movement event counter. This is used to
implement the `sunrise-cursor-follows-mouse' feature."
  (setq sunrise-mouse-events-count 0))
(add-hook 'sunrise-init-hook 'sunrise-mouse-disown-cursor)

;;; ============================================================================
;;; File system navigation functions:

(defun sunrise-advertised-find-file (&optional filename)
  "Handle accesses to file system objects through the user interface.
Includes cases when the user presses return, f or clicks on the path line."
  (interactive)
  (unless filename
    (if (eq 1 (line-number-at-pos)) ;; <- Click or Enter on path line.
        (let* ((path (buffer-substring (point) (point-at-eol)))
               (levels (1- (length (split-string path "/")))))
          (if (< 0 levels)
              (sunrise-dired-prev-subdir levels)
            (sunrise-beginning-of-buffer)))
      (setq filename (dired-get-filename nil t)
            filename (and filename (expand-file-name filename)))))
  (if filename
      (if (file-exists-p filename)
          (sunrise-find-file filename)
        (error "Sunrise: nonexistent target"))))

(defun sunrise-advertised-execute-file (&optional prefix)
  "Execute the currently selected file in a new subprocess."
  (interactive "P")
  (let ((path (dired-get-filename nil t)) (label) (args))
    (if path
        (setq label  (file-name-nondirectory path))
      (error "Sunrise: no executable file on this line"))
    (unless (and (not (file-directory-p path)) (file-executable-p path))
      (error "Sunrise: \"%s\" is not an executable file" label))
    (when prefix
      (setq args (read-string (format "arguments for \"%s\": " label))
            label (format "%s %s" label args)))
    (message "Sunrise: executing \"%s\" in new process" label)
    (if args
        (apply #'start-process (append (list "Sunrise Subprocess" nil path)
                                       (split-string args)))
      (start-process "Sunrise Subprocess" nil path))))

(defun sunrise-find-file (filename &optional wildcards)
  "Determine the proper way of handling an object in the file system.
FILENAME can be either a regular file, a regular directory, a
Sunrise VIRTUAL directory, or a virtual directory served by
AVFS."
  (interactive (find-file-read-args "Find file or directory: " nil))
  (cond ((file-directory-p filename) (sunrise-find-regular-directory filename))
        ((and (sunrise-avfs-directory-p filename) (sunrise-avfs-dir filename))
         (sunrise-find-regular-directory (sunrise-avfs-dir filename)))
        ((sunrise-virtual-directory-p filename) (sunrise-find-virtual-directory filename))
        (t (sunrise-find-regular-file filename wildcards))))

(defun sunrise-virtual-directory-p (filename)
  "Tell whether FILENAME is the path to a Sunrise VIRTUAL directory."
  (eq 'sunrise-virtual-mode (assoc-default filename auto-mode-alist 'string-match)))

(defun sunrise-avfs-directory-p (filename)
  "Tell whether FILENAME can be seen as the root of an AVFS virtual directory."
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (and sunrise-avfs-root
         (or (eq 'archive-mode mode)
             (eq 'tar-mode mode)
             (and (listp mode) (eq 'jka-compr (cadr mode)))
             (not (equal "." (sunrise-assoc-key filename
                                           sunrise-avfs-handlers-alist
                                           'string-match)))))))

(defun sunrise-find-regular-directory (directory)
  "Visit the given regular directory in the active pane."
  (setq directory (file-name-as-directory directory))
  (let ((parent (expand-file-name "../")))
    (if (and (not (sunrise-equal-dirs parent default-directory))
             (sunrise-equal-dirs directory parent))
        (sunrise-dired-prev-subdir)
      (sunrise-goto-dir directory))))

(defun sunrise-find-virtual-directory (sunrise-virtual-dir)
  "Visit the given Sunrise VIRTUAL directory in the active pane."
  (sunrise-save-aspect
   (sunrise-alternate-buffer (find-file sunrise-virtual-dir)))
  (sunrise-history-push sunrise-virtual-dir)
  (set-visited-file-name nil t)
  (sunrise-keep-buffer)
  (sunrise-backup-buffer))

(defun sunrise-find-regular-file (filename &optional wildcards)
  "Visit FILENAME as a regular file with WILDCARDS.
\(See `find-file' for more details on wildcard expansion.)"
  (condition-case description
      (let ((buffer (find-file-noselect filename nil nil wildcards)))
        (funcall sunrise-visit-buffer-function buffer))
    (error (message "%s" (cadr description)))))

(defun sunrise-visit-buffer-in-current-frame (buffer)
  "Deactivate Sunrise and display the given buffer in the current frame."
  (sunrise-save-panes-width)
  (sunrise-quit)
  (set-window-configuration sunrise-prior-window-configuration)
  (switch-to-buffer buffer))

(defun sunrise-avfs-dir (filename)
  "Return the virtual path for accessing FILENAME through AVFS.
Returns nil if AVFS cannot manage this kind of file."
  (let* ((handler (assoc-default filename sunrise-avfs-handlers-alist 'string-match))
         (vdir (concat filename handler)))
    (unless (sunrise-overlapping-paths-p sunrise-avfs-root vdir)
      (setq vdir (concat sunrise-avfs-root vdir)))
    (if (file-attributes vdir) vdir nil)))

(defun sunrise-goto-dir (dir)
  "Change the current directory in the active pane to the given one."
  (interactive "DChange directory (file or pattern): ")
  (if sunrise-goto-dir-function
      (funcall sunrise-goto-dir-function dir)
    (unless (and (eq major-mode 'sunrise-mode) (sunrise-equal-dirs dir default-directory))
      (if (and sunrise-avfs-root
               (null (posix-string-match "#" dir)))
          (setq dir
                (replace-regexp-in-string
                 (directory-file-name (expand-file-name sunrise-avfs-root)) "" dir)))
      (sunrise-save-aspect
       (sunrise-within dir (sunrise-alternate-buffer (dired dir))))
      (sunrise-history-push default-directory)
      (sunrise-beginning-of-buffer))))

(defun sunrise-dired-prev-subdir (&optional count)
  "Go to the parent directory, or COUNT subdirectories upwards."
  (interactive "P")
  (unless (sunrise-equal-dirs default-directory "/")
    (let* ((count (or count 1))
           (to (replace-regexp-in-string "x" "../" (make-string count ?x)))
           (from (expand-file-name (substring to 1)))
           (from (sunrise-directory-name-proper from))
           (from (replace-regexp-in-string "\\(?:#.*/?$\\|/$\\)" "" from))
           (to (replace-regexp-in-string "\\.\\./$" "" (expand-file-name to))))
      (sunrise-goto-dir to)
      (unless (sunrise-equal-dirs from to)
        (sunrise-focus-filename from)))))

(defun sunrise-follow-file (&optional target-path)
  "Go to the same directory where the selected file is.
Very useful inside Sunrise VIRTUAL buffers."
  (interactive)
  (if (null target-path)
      (setq target-path (dired-get-filename nil t)))

  (let ((target-dir (file-name-directory target-path))
        (target-symlink (file-symlink-p target-path))
        (target-file))

    ;; if the target is a symlink and there's nothing more interesting to do
    ;; then follow the symlink:
    (when (and target-symlink
               (string= target-dir (dired-current-directory))
               (not (eq major-mode 'sunrise-virtual-mode)))
      (unless (file-exists-p target-symlink)
        (error "Sunrise: file is a symlink to a nonexistent target"))
      (setq target-path target-symlink)
      (setq target-dir (file-name-directory target-symlink)))

    (setq target-file (file-name-nondirectory target-path))

    (when target-dir ;; <-- nil in symlinks to other files in same directory:
      (setq target-dir (sunrise-chop ?/ target-dir))
      (sunrise-goto-dir target-dir))
    (sunrise-focus-filename target-file)))

(defun sunrise-follow-viewer ()
  "Go to the directory of the file displayed in the viewer window."
  (interactive)
  (when sunrise-running
    (let* ((viewer (sunrise-viewer-window))
           (viewer-buffer (if viewer (window-buffer viewer)))
           (target-dir) (target-file))
      (when viewer-buffer
        (with-current-buffer viewer-buffer
          (setq target-dir default-directory
                target-file (sunrise-directory-name-proper (buffer-file-name)))))
      (sunrise-select-window sunrise-selected-window)
      (if target-dir (sunrise-goto-dir target-dir))
      (if target-file (sunrise-focus-filename target-file)))))

(defun sunrise-project-path ()
  "Find projections of the active directory over the passive one.

Locates interactively all descendants of the directory in the passive pane that
have a path similar to the directory in the active pane.

For instance, if the active pane is displaying directory /a/b/c and the passive
one is displaying /x/y, this command will check for the existence of any of the
following: /x/y/a/b/c, /x/y/b/c, /x/y/c and /x/y. Each (existing) directory
located according to this schema will be known hereafter as a 'projection of the
directory /a/b/c over /x/y'.

If many projections of the active directory over the passive one exist, one can
rotate among all of them by invoking `sunrise-project-path' repeatedly : they will be
visited in order, from longest path to shortest."

  (interactive)
  (let* ((sunrise-synchronized nil)
         (path (sunrise-chop ?/ (expand-file-name (dired-current-directory))))
         (pos (if (< 0 (length path)) 1)) (candidate) (next-key))
    (while pos
      (setq candidate (concat sunrise-other-directory (substring path pos))
            pos (string-match "/" path (1+ pos))
            pos (if pos (1+ pos)))
      (when (and (file-directory-p candidate)
                 (not (sunrise-equal-dirs sunrise-this-directory candidate)))
        (sunrise-goto-dir-other candidate)
        (setq next-key (read-key-sequence "(press C-M-o again for more)"))
        (if (eq (lookup-key sunrise-mode-map next-key) 'sunrise-project-path)
            (sunrise-history-prev-other)
          (setq unread-command-events (listify-key-sequence next-key)
                pos nil))))
    (unless next-key
      (message "Sunrise: sorry, no suitable projections found"))))

(defun sunrise-history-push (element)
  "Push a new path into the history stack of the current pane."
  (let ((type (sunrise-history-entry-type element)))
    (when type
      (let* ((pane (assoc sunrise-selected-window sunrise-history-registry))
             (hist (cdr pane))
             (len (length hist)))
        (if (>= len sunrise-history-length)
            (nbutlast hist (- len sunrise-history-length)))
        (when (eq 'local type)
          (setq element (abbreviate-file-name (sunrise-chop ?/ element))))
        (setq hist (delete element hist))
        (push element hist)
        (setcdr pane hist))
      (sunrise-history-stack-reset))))

(defun sunrise-history-next ()
  "Navigate forward in the history of the active pane."
  (interactive)
  (let ((side (assoc sunrise-selected-window sunrise-history-stack)))
    (unless (zerop (cadr side))
      (sunrise-history-move -1))
    (when (zerop (cadr side))
      (sunrise-history-stack-reset))))

(defun sunrise-history-prev ()
  "Navigate backwards in the history of the active pane."
  (interactive)
  (let ((history (cdr (assoc sunrise-selected-window sunrise-history-registry)))
        (stack (cdr (assoc sunrise-selected-window sunrise-history-stack))))
    (when (< (abs (cdr stack)) (1- (length history)))
      (sunrise-history-move 1))))

(defun sunrise-history-move (step)
  "Traverse the history of the active pane in a stack-like fashion.
This function re-arranges the history list of the current pane so as to make it
simulate a stack of directories, from which one can 'pop' the current directory
and 'push' it back, keeping the most recently visited entries always near the
top of the stack."
  (let* ((side (assoc sunrise-selected-window sunrise-history-stack))
         (depth (cadr side)) (goal) (target-dir))
    (when (> 0 (* step depth))
      (sunrise-history-stack-reset))
    (setq goal  (1+ (cddr side))
          depth (* step (+ (abs depth) step))
          target-dir (sunrise-history-pick goal))
    (when target-dir
      (sunrise-goto-dir target-dir)
      (setcdr side (cons depth goal)))))

(defun sunrise-history-stack-reset ()
  "Reset the current history stack counter."
  (let ((side (assoc sunrise-selected-window sunrise-history-stack)))
    (setcdr side '(0 . 0))))

(defun sunrise-history-pick (position)
  "Return directory at POSITION in current history.
If the entry was removed or made inaccessible since our last visit, remove it
from the history list and check among the previous ones until an accessible
directory is found, or the list runs out of entries."
  (let* ((history (cdr (assoc sunrise-selected-window sunrise-history-registry)))
         (target (nth position history)))
    (while (not (sunrise-history-entry-type target))
      (delete target history)
      (setq target (nth position history)))
    target))

(defun sunrise-history-entry-type (entry)
  "Determine the type of the given history ENTRY.
Evaluate to: 'tramp if the entry is a valid remote entry, 'local
if the entry represents a directory in the local file system, or
nil if the argument is not a valid history entry."
  (when entry
    (let ((isTramp (string-match tramp-file-name-regexp entry)))
      (if isTramp
          'tramp
        (if (file-accessible-directory-p entry) 'local)))))

(defun sunrise-history-purge-remote()
  "Remove all remote entries from the history of directories."
  (interactive)
  (mapc
   (lambda (side)
     (let ((pane (assoc side sunrise-history-registry))
           (regex tramp-file-name-regexp))
       (setcdr pane (delq nil (mapcar (lambda (x)
                                        (and (not (string-match regex x)) x))
                                      (cdr pane))))))
   '(left right)))

(defun sunrise-require-checkpoints-extension (&optional noerror)
  "Bootstrap code for checkpoint support.
Just tries to require the appropriate checkpoints extension
depending on the version of bookmark.el being used."
  (require 'bookmark nil t)
  (let* ((feature
          (cond ((fboundp 'bookmark-make-record) 'sunrise-checkpoints)
                (t 'sunrise-old-checkpoints)))
         (name (symbol-name feature)))
    (or
     (not (featurep 'sunrise))
     (require feature nil t)
     noerror
     (error "Feature `%s' not found!\
For checkpoints to work, download http://joseito.republika.pl/%s.el.gz\
and add it to your `load-path'" name name))))

(defmacro sunrise-checkpoint-command (function-name)
  `(defun ,function-name (&optional arg)
     (interactive)
     (sunrise-require-checkpoints-extension)
     (if (commandp #',function-name)
         (call-interactively #',function-name)
       (funcall #',function-name arg))))
(sunrise-checkpoint-command sunrise-checkpoint-save)
(sunrise-checkpoint-command sunrise-checkpoint-restore)
(sunrise-checkpoint-command sunrise-checkpoint-handler)
;;;###autoload (autoload 'sunrise-checkpoint-handler "sunrise" "" t)

(defun sunrise-do-find-marked-files (&optional noselect)
  "Sunrise replacement for `dired-do-find-marked-files'."
  (interactive "P")
  (let* ((files (delq nil (mapcar (lambda (x)
                                    (and (file-regular-p x) x))
                                  (dired-get-marked-files)))))
    (unless files
      (error "Sunrise: no regular files to open"))
    (unless noselect (sunrise-quit))
    (dired-simultaneous-find-file files noselect)))

;;; ============================================================================
;;; Graphical interface interaction functions:

(defun sunrise-detect-switch ()
  "Detect Sunrise pane switches and update tracking state accordingly."
  (when (and sunrise-running
             (not sunrise-inhibit-switch)
             (eq (selected-window) (sunrise-other 'window)))
    (let ((there sunrise-this-directory))
      (setq sunrise-selected-window (sunrise-other)
            sunrise-selected-window-width nil
            sunrise-this-directory default-directory
            sunrise-other-directory there)
      (sunrise-save-panes-width)
      (sunrise-highlight))))

(defun sunrise-change-window()
  "Change to the other Sunrise pane."
  (interactive)
  (sunrise-select-window (sunrise-other))
  (setq sunrise-selected-window-width nil))

(defun sunrise-mouse-change-window (e)
  "Change to the Sunrise pane clicked in by the mouse."
  (interactive "e")
  (mouse-set-point e))

(defun sunrise-mouse-move-cursor (event)
  "Move the cursor to the current mouse position.
This function is called only if the `sunrise-cursor-follows-mouse' custom variable
\(which see) has not been set to nil."
  (interactive "e")
  (if (<= sunrise-mouse-events-count sunrise-mouse-events-threshold)
      (setq sunrise-mouse-events-count (1+ sunrise-mouse-events-count))
    (when (mouse-movement-p event)
      (let ((mouse-pos (cadadr event))
            (mouse-win (caadr event)))
        (when (eq mouse-win (sunrise-other 'window))
          (sunrise-change-window))
        (when (numberp mouse-pos)
          (goto-char mouse-pos))))))

(defun sunrise-select-window (side)
  "Select/highlight the given Sunrise window (right or left)."
  (select-window (symbol-value (sunrise-symbol side 'window))))

(defun sunrise-viewer-window ()
  "Return an active window that can be used as the viewer."
  (if (or (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
          (memq (current-buffer) (list sunrise-left-buffer sunrise-right-buffer)))
      (let ((current-window (selected-window)) (target-window))
        (dotimes (_times 2)
          (setq current-window (next-window current-window))
          (unless (memq current-window (list sunrise-left-window sunrise-right-window))
            (setq target-window current-window)))
        target-window)
    (selected-window)))

(defun sunrise-select-viewer-window (&optional force-setup)
  "Select a window that is not a Sunrise pane.
If no suitable active window can be found and FORCE-SETUP is set,
calls the function `sunrise-setup-windows' and tries once again."
  (interactive "p")
  (let ((selected sunrise-selected-window)
        (viewer (sunrise-viewer-window)))
    (if (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
        (hl-line-mode 1))
    (if viewer
        (select-window viewer)
      (when force-setup
        (sunrise-setup-windows)
        (select-window (sunrise-viewer-window))))
    (setq sunrise-selected-window selected)))

(defun sunrise-beginning-of-buffer()
  "Go to the first directory/file in Dired."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward directory-listing-before-filename-regexp nil t)
    (dotimes (_times 2)
      (when (looking-at "\.\.?/?$")
        (dired-next-line 1)))))

(defun sunrise-end-of-buffer()
  "Go to the last directory/file in Dired."
  (interactive)
  (goto-char (point-max))
  (re-search-backward directory-listing-before-filename-regexp)
  (dired-next-line 0))

(defun sunrise-focus-filename (filename)
  "Try to select FILENAME in the current buffer."
  (if (and dired-omit-mode
           (string-match (dired-omit-regexp) filename))
      (dired-omit-mode -1))
  (let ((sunrise-inhibit-highlight t)
        (expr (sunrise-chop ?/ filename)))
    (cond ((file-symlink-p filename)
           (setq expr (concat (regexp-quote expr) " ->")))
          ((file-directory-p filename)
           (setq expr (concat (regexp-quote expr) "\\(?:/\\|$\\)")))
          ((file-regular-p filename)
           (setq expr (concat (regexp-quote expr) "$"))))
    (setq expr (concat "[0-9] +" expr))
    (beginning-of-line)
    (unless (re-search-forward expr nil t)
      (re-search-backward expr nil t)))
  (beginning-of-line)
  (re-search-forward directory-listing-before-filename-regexp nil t))

(defun sunrise-split-toggle()
  "Change Sunrise window layout from horizontal to vertical to top and so on."
  (interactive)
  (case sunrise-window-split-style
    (horizontal (sunrise-split-setup 'vertical))
    (vertical (sunrise-split-setup 'top))
    (top (progn
           (sunrise-split-setup 'horizontal)
           (sunrise-in-other (revert-buffer))))
    (t (sunrise-split-setup 'horizontal))))

(defun sunrise-split-setup(split-type)
  (setq sunrise-window-split-style split-type)
  (when sunrise-running
    (when (eq sunrise-window-split-style 'top)
      (sunrise-select-window 'left)
      (delete-window sunrise-right-window)
      (setq sunrise-panes-height (window-height)))
    (sunrise-setup-windows))
  (message "Sunrise: split style changed to \"%s\"" (symbol-name split-type)))

(defun sunrise-transpose-panes ()
  "Change the order of the panes."
  (interactive)
  (unless (eq sunrise-left-buffer sunrise-right-buffer)
    (mapc (lambda (x)
            (let ((left (sunrise-symbol 'left x)) (right (sunrise-symbol 'right x)) (tmp))
              (setq tmp (symbol-value left))
              (set left (symbol-value right))
              (set right tmp)))
          '(directory buffer window))
    (let ((tmp sunrise-this-directory))
      (setq sunrise-this-directory sunrise-other-directory
            sunrise-other-directory tmp))
    (let ((here sunrise-selected-window))
      (select-window sunrise-right-window)
      (sunrise-setup-visible-panes)
      (sunrise-select-window here))))

(defun sunrise-synchronize-panes (&optional reverse)
  "Change the directory in the other pane to that in the current one.
If the optional parameter REVERSE is non-nil, performs the
opposite operation, ie. changes the directory in the current pane
to that in the other one."
  (interactive "P")
  (sunrise-assert-other)
  (let ((target (current-buffer)) (sunrise-inhibit-highlight t))
    (sunrise-change-window)
    (if reverse
        (setq target (current-buffer))
      (sunrise-alternate-buffer (switch-to-buffer target))
      (sunrise-history-push default-directory))
    (sunrise-change-window)
    (when reverse
      (sunrise-alternate-buffer (switch-to-buffer target))
      (sunrise-history-push default-directory)
      (revert-buffer)))
  (sunrise-highlight))

(defun sunrise-browse-pane ()
  "Browse the directory in the active pane."
  (interactive)
  (if (not (featurep 'browse-url))
      (error "Sunrise: feature `browse-url' not available!")
    (let ((url (concat "file://" (expand-file-name default-directory))))
      (message "Browsing directory %s " default-directory)
      (if (featurep 'w3m)
          (eval '(w3m-goto-url url))
        (browse-url url)))))

(defun sunrise-browse-file (&optional file)
  "Display the selected file in the default web browser."
  (interactive)
  (unless (featurep 'browse-url)
    (error "ERROR: Feature browse-url not available!"))
  (setq file (or file (dired-get-filename)))
  (sunrise-save-selected-window
    (sunrise-select-viewer-window)
    (let ((buff (current-buffer)))
      (browse-url (concat "file://" file))
      (unless (eq buff (current-buffer))
        (sunrise-scrollable-viewer (current-buffer)))))
  (message "Browsing \"%s\" in web browser" file))

(defun sunrise-revert-buffer (&optional _ignore-auto _no-confirm)
  "Revert the current pane using the contents of the backup buffer (if any).
If the buffer is non-virtual the backup buffer is killed."
  (interactive)
  (if (buffer-live-p sunrise-backup-buffer)
      (let ((marks (dired-remember-marks (point-min) (point-max)))
            (focus (dired-get-filename 'verbatim t))
            (inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring sunrise-backup-buffer)
        (sunrise-beginning-of-buffer)
        (dired-mark-remembered marks)
        (if focus (sunrise-focus-filename focus))
        (dired-change-marks ?\t ?*)
        (if (eq 'sunrise-mode major-mode) (sunrise-kill-backup-buffer)))
    (unless (or (eq major-mode 'sunrise-virtual-mode)
                (local-variable-p 'sunrise-virtual-buffer))
      (dired-revert)
      (if (string= "NUMBER" (get sunrise-selected-window 'sorting-order))
          (sunrise-sort-by-number t)
        (if (get sunrise-selected-window 'sorting-reverse)
            (sunrise-reverse-pane)))))
  (sunrise-display-attributes (point-min) (point-max) sunrise-show-file-attributes)
  (sunrise-highlight))

(defun sunrise-kill-pane-buffer ()
  "Kill the buffer currently displayed in the active pane, or quit Sunrise.
Custom variable `sunrise-kill-unused-buffers' controls whether unused buffers are
killed automatically by Sunrise when the user navigates away from the directory
they contain. When this flag is set, all requests to kill the current buffer are
managed by just calling `sunrise-quit'."
  (interactive)
  (if sunrise-kill-unused-buffers
      (sunrise-quit)
    (kill-buffer (current-buffer))
    (let ((_x (pop (cdr (assoc sunrise-selected-window sunrise-history-registry)))))
      (sunrise-history-stack-reset))))

(defun sunrise-quick-view (&optional arg)
  "Quickly view the currently selected item.
On regular files, opens the file in quick-view mode (see `sunrise-quick-view-file'
for more details), on directories, visits the selected directory in the passive
pane, and on symlinks follows the file the link points to in the passive pane.
With optional argument kills the last quickly viewed file without opening a new
buffer."
  (interactive "P")
  (if arg
      (sunrise-quick-view-kill)
    (let ((name (dired-get-filename nil t)))
      (cond ((file-directory-p name) (sunrise-quick-view-directory name))
            ((file-symlink-p name) (sunrise-quick-view-symlink name))
            (t (sunrise-quick-view-file))))))

(defun sunrise-quick-view-kill ()
  "Kill the last buffer opened using quick view (if any)."
  (let ((buf other-window-scroll-buffer))
    (when (and (buffer-live-p buf)
               (or (not sunrise-confirm-kill-viewer)
                   (y-or-n-p (format "Kill buffer %s? " (buffer-name buf)))))
      (setq other-window-scroll-buffer nil)
      (save-window-excursion (kill-buffer buf)))))

(defun sunrise-quick-view-directory (name)
  "Open the directory NAME in the passive pane."
  (let ((name (expand-file-name name)))
    (sunrise-in-other (sunrise-advertised-find-file name))))

(defun sunrise-quick-view-symlink (name)
  "Follow the target of the symlink NAME in the passive pane."
  (let ((name (expand-file-name (file-symlink-p name))))
    (if (file-exists-p name)
        (sunrise-in-other (sunrise-follow-file name))
      (error "Sunrise: file is a symlink to a nonexistent target"))))

(defun sunrise-quick-view-file ()
  "Open the selected file on the viewer window without selecting it.
Kills any other buffer opened previously the same way."
  (let ((split-width-threshold (* 10 (window-width)))
        (filename (expand-file-name (dired-get-filename nil t))))
    (sunrise-save-selected-window
      (condition-case description
          (progn
            (sunrise-select-viewer-window)
            (find-file filename)
            (if (and sunrise-kill-quick-view-buffers
                     (not (eq (current-buffer) other-window-scroll-buffer))
                     (buffer-live-p other-window-scroll-buffer))
                (kill-buffer other-window-scroll-buffer))
            (sunrise-scrollable-viewer (current-buffer)))
        (error (message "%s" (cadr description)))))))

;; These clean up after a quick view:
(add-hook 'sunrise-quit-hook (defun sunrise-sunrise-quit-function ()
                          (setq other-window-scroll-buffer nil)))
(add-hook 'kill-buffer-hook
          (defun sunrise-kill-viewer-function ()
            (if (eq (current-buffer) other-window-scroll-buffer)
                (setq other-window-scroll-buffer  nil))))

(defun sunrise-mask-attributes (beg end)
  "Manage the hiding of attributes in region from BEG to END.
Selective hiding of specific attributes can be controlled by customizing the
`sunrise-attributes-display-mask' variable."
  (let ((cursor beg) props)
    (cl-labels ((sunrise-make-display-props
            (display-function-or-flag)
            (cond ((functionp display-function-or-flag)
                   `(display
                     ,(apply display-function-or-flag
                             (list (buffer-substring cursor (1- (point)))))))
                  ((null display-function-or-flag) '(invisible t))
                  (t nil))))
      (if sunrise-attributes-display-mask
          (block block
            (mapc (lambda (do-display)
                    (search-forward-regexp "\\w")
                    (search-forward-regexp "\\s-")
                    (forward-char -1)
                    (setq props (sunrise-make-display-props do-display))
                    (when props
                      (add-text-properties cursor (point) props))
                    (setq cursor (point))
                    (if (>= (point) end) (return-from block)))
                  sunrise-attributes-display-mask))
        (unless (>= cursor end)
          (add-text-properties cursor (1- end) '(invisible t)))))))

(defun sunrise-display-attributes (beg end visiblep)
  "Manage the display of file attributes in the region from BEG to END.
if VISIBLEP is nil then shows file attributes in region, otherwise hides them."
  (let ((inhibit-read-only t) (next))
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (while (and (null next) (< (point) end))
        (forward-line 1)
        (setq next (dired-move-to-filename)))
      (while (and next (< next end))
        (beginning-of-line)
        (forward-char 1)
        (if (not visiblep)
            (sunrise-mask-attributes (point) next)
          (remove-text-properties (point) next '(invisible t))
          (remove-text-properties (point) next '(display)))
        (forward-line 1)
        (setq next (dired-move-to-filename))))))

(defun sunrise-toggle-attributes ()
  "Hide/Show the attributes of all files in the active pane."
  (interactive)
  (setq sunrise-show-file-attributes (not sunrise-show-file-attributes))
  (sunrise-display-attributes (point-min) (point-max) sunrise-show-file-attributes))

(defun sunrise-toggle-truncate-lines ()
  "Enable/Disable truncation of long lines in the active pane."
  (interactive)
  (if (sunrise-truncate-p)
      (progn
        (setq truncate-partial-width-windows (sunrise-truncate-v nil))
        (message "Sunrise: wrapping long lines"))
    (progn
      (setq truncate-partial-width-windows (sunrise-truncate-v t))
      (message "Sunrise: truncating long lines")))
  (sunrise-silently (dired-do-redisplay)))

(defun sunrise-truncate-p ()
  "Return non-nil if `truncate-partial-width-windows' affects the current pane.
Used by `sunrise-toggle-truncate-lines'."
  (if (numberp truncate-partial-width-windows)
      (< 0 truncate-partial-width-windows)
    truncate-partial-width-windows))

(defun sunrise-truncate-v (active)
  "Return the appropriate value for `truncate-partial-width-widows'.
Depends on the Emacs version being used. Used by
`sunrise-toggle-truncate-lines'."
  (or (and (version<= "23" emacs-version)
           (or (and active 3000) 0))
      active))

(defun sunrise-sort-order (label option)
  "Change the sorting order of the active pane.
Appends additional options to `dired-listing-switches' and
reverts the buffer."
  (if (eq major-mode 'sunrise-virtual-mode)
      (sunrise-sort-virtual option)
    (let ((option (if (= 0 (length option)) option (concat " -" option))))
      (put sunrise-selected-window 'sorting-order label)
      (put sunrise-selected-window 'sorting-options option)
      (let ((dired-listing-switches dired-listing-switches))
        (unless (string-match "^/ftp:" default-directory)
          (setq dired-listing-switches sunrise-listing-switches))
        (dired-sort-other (concat dired-listing-switches option) t))
      (revert-buffer)))
  (message "Sunrise: sorting entries by %s" label))

(defmacro sunrise-defun-sort-by (postfix options)
  "Helper macro for defining `sunrise-sort-by-xxx' functions."
  `(defun ,(intern (format "sunrise-sort-by-%s" postfix)) ()
     ,(format "Sorts the contents of the current Sunrise pane by %s." postfix)
     (interactive)
     (sunrise-sort-order ,(upcase postfix) ,options)))
(sunrise-defun-sort-by "name" "")
(sunrise-defun-sort-by "extension" "X")
(sunrise-defun-sort-by "time" "t")
(sunrise-defun-sort-by "size" "S")

(defun sunrise-sort-by-number (&optional inhibit-label)
  "Sort the contents of the current Sunrise pane numerically.
Displays entries containing unpadded numbers in a more logical
order than when sorted alphabetically by name."
  (interactive)
  (sunrise-sort-by-operation 'sunrise-numerical-sort-op (unless inhibit-label "NUMBER"))
  (if (get sunrise-selected-window 'sorting-reverse) (sunrise-reverse-pane)))

(defun sunrise-interactive-sort (order)
  "Prompt for a new sorting order for the active pane and apply it."
  (interactive "cSort by (n)ame, n(u)mber, (s)ize, (t)ime or e(x)tension? ")
  (if (>= order 97)
      (setq order (- order 32)))
  (case order
    (?U (sunrise-sort-by-number))
    (?T (sunrise-sort-by-time))
    (?S (sunrise-sort-by-size))
    (?X (sunrise-sort-by-extension))
    (t  (sunrise-sort-by-name))))

(defun sunrise-reverse-pane (&optional interactively)
  "Reverse the contents of the active pane."
  (interactive "p")
  (let ((line (line-number-at-pos))
        (reverse (get sunrise-selected-window 'sorting-reverse)))
    (sunrise-sort-by-operation 'identity)
    (when interactively
      (put sunrise-selected-window 'sorting-reverse (not reverse))
      (goto-char (point-min)) (forward-line (1- line))
      (re-search-forward directory-listing-before-filename-regexp nil t))))

(defun sunrise-sort-virtual (option)
  "Manage sorting of buffers in Sunrise VIRTUAL mode."
  (let ((opt (string-to-char option)) (inhibit-read-only t) (beg) (end))
    (case opt
      (?X (sunrise-end-of-buffer)
          (setq end (point-at-eol))
          (sunrise-beginning-of-buffer)
          (setq beg (point-at-bol))
          (sort-regexp-fields nil "^.*$" "[/.][^/.]+$" beg end))
      (?t (sunrise-sort-by-operation
           (lambda (x) (sunrise-attribute-sort-op 5 t x)) "TIME"))
      (?S (sunrise-sort-by-operation
           (lambda (x) (sunrise-attribute-sort-op 7 t x)) "SIZE"))
      (t (sunrise-sort-by-operation
          (lambda (x) (sunrise-attribute-sort-op -1 nil x)) "NAME")))))

(defun sunrise-sort-by-operation (operation &optional label)
  "General function for reordering the contents of a Sunrise pane.
OPERATION is a function that receives a list produced by
`sunrise-build-sort-lists', reorders it in some way, transforming it
into a list that can be passed to `sort-reorder', so the records
in the current buffer are reordered accordingly. The LABEL is a
string that will be used to set the sorting order of the current
pane and then displayed in the minibuffer; if it's not provided
or its value is nil then the ordering enforced by this function
is transient and can be undone by reverting the pane, or by
moving it to a different directory. See `sunrise-numerical-sort-op'
and `sunrise-attribute-sort-op' for examples of OPERATIONs."
  (interactive)
  (let ((messages (> (- (point-max) (point-min)) 50000))
        (focus (dired-get-filename 'verbatim t))
        (inhibit-read-only t))
    (if messages (message "Finding sort keys..."))
    (let* ((sort-lists (sunrise-build-sort-lists))
           (old (reverse sort-lists))
           (beg) (end))
      (if messages (message "Sorting records..."))
      (setq sort-lists (apply operation (list sort-lists)))
      (if messages (message "Reordering buffer..."))
      (save-excursion
        (save-restriction
          (sunrise-end-of-buffer)
          (setq end (point-at-eol))
          (sunrise-beginning-of-buffer)
          (setq beg (point-at-bol))
          (narrow-to-region beg end)
          (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done")))
    (sunrise-highlight)
    (if focus (sunrise-focus-filename focus))
    (when label
      (put sunrise-selected-window 'sorting-order label)
      (message "Sunrise: sorting entries by %s" label)))
  nil)

(defun sunrise-numerical-sort-op (sort-lists)
  "Strategy used to numerically sort contents of a Sunrise pane.
Used by `sunrise-sort-by-operation'. See `sunrise-sort-by-number' for more
on this kind of sorting."
  (mapcar
   'cddr
   (sort
    (sort
     (mapcar
      (lambda (x)
        (let ((key (buffer-substring-no-properties (car x) (cddr x))))
          (append
           (list key
                 (string-to-number (replace-regexp-in-string "^[^0-9]*" "" key))
                 (cdr x))
           (cdr x))))
      sort-lists)
     (lambda (a b) (string< (car a) (car b))))
    (lambda (a b) (< (cadr a) (cadr b))))))

(defun sunrise-attribute-sort-op (nth-attr as-number sort-lists)
  "Strategy used to sort contents of a Sunrise pane according to file attributes.
Used by `sunrise-sort-by-operation'. See `file-attributes' for a list
of supported attributes and their positions. Directories are
forced to remain always on top. NTH-ATTR is the position of the
attribute to use for sorting, or -1 for the name of the file.
AS-NUMBER determines whether comparisons will be numeric or
alphabetical. SORT-LISTS is a list of positions obtained from
`sunrise-build-sort-lists'."
  (let ((attributes (sunrise-files-attributes))
        (zero (if as-number 0 "")))
    (mapcar
     'cddr
     (sort
      (sort
       (mapcar
        (lambda (x)
          (let* ((key (buffer-substring-no-properties (car x) (cddr x)))
                 (key (sunrise-chop ?/ (replace-regexp-in-string " -> .*$" "" key)))
                 (attrs (assoc-default key attributes))
                 (index))
            (when attrs
              (setq attrs (apply 'cons attrs)
                    index (or (nth (1+ nth-attr) attrs) zero))
              (append (list (cadr attrs) index (cdr x)) (cdr x)))))
        sort-lists)
       (lambda (a b) (sunrise-compare nth-attr (cadr b) (cadr a))))
      (lambda (a b)
        (if (and (car a) (car b))
            (sunrise-compare nth-attr (cadr b) (cadr a))
          (and (car a) (not (stringp (car a))))))))))

(defun sunrise-build-sort-lists ()
  "Analyse contents of the current Sunrise pane for `sunrise-sort-by-operation'.
Builds a list of dotted lists of the form (a b . c) -- where 'a'
is the position at the start of the file name in an entry, while
'b' and 'c' are the start and end positions of the whole entry.
These lists are used by `sunrise-sort-by-operation' to sort the
contents of the pane in arbitrary ways."
  (delq nil
        (mapcar
         (lambda (x) (and (atom (car x)) x))
         (save-excursion
           (sunrise-beginning-of-buffer)
           (beginning-of-line)
           (sort-build-lists 'forward-line 'end-of-line 'dired-move-to-filename
                             nil)))))

(defun sunrise-compare (mode a b)
  "General comparison function, used to sort files in VIRTUAL buffers.
MODE must be a number; if it is less than 0, the direction of the
comparison is inverted: (sunrise-compare -1 a b) === (sunrise-compare 1
b a). Compares numbers using `<', strings case-insensitively
using `string<' and lists recursively until the first two
elements that are non-equal are found."
  (if (< mode 0) (let (tmp) (setq tmp a a b b tmp mode (abs mode))))
  (cond ((or (null a) (null b)) nil)
        ((and (listp a) (listp b)) (if (= (car a) (car b))
                                       (sunrise-compare mode (cdr a) (cdr b))
                                     (sunrise-compare mode (car a) (car b))))
        ((and (stringp a) (stringp b)) (string< (downcase a) (downcase b)))
        ((and (numberp a) (numberp b)) (< a b))
        (t nil)))

(defun sunrise-scroll-up ()
  "Scroll the current pane or (if active) the viewer pane 1 line up."
  (interactive)
  (if (buffer-live-p other-window-scroll-buffer)
      (sunrise-save-selected-window
        (sunrise-select-viewer-window)
        (scroll-up 1))
    (scroll-up 1)))

(defun sunrise-scroll-down ()
  "Scroll the current pane or (if active) the viewer pane 1 line down."
  (interactive)
  (if (buffer-live-p other-window-scroll-buffer)
      (sunrise-save-selected-window
        (sunrise-select-viewer-window)
        (scroll-down 1))
    (scroll-down 1)))

(defun sunrise-scroll-quick-view ()
  "Scroll down the viewer window during a quick view."
  (interactive)
  (if other-window-scroll-buffer (scroll-other-window)))

(defun sunrise-scroll-quick-view-down ()
  "Scroll down the viewer window during a quick view."
  (interactive)
  (if other-window-scroll-buffer (scroll-other-window-down nil)))

(defun sunrise-undo ()
  "Restore selection as it was before the last file operation."
  (interactive)
  (dired-undo)
  (sunrise-highlight))

;;; ============================================================================
;;; Passive & synchronized navigation functions:

(defun sunrise-sync ()
  "Toggle the Sunrise synchronized navigation feature."
  (interactive)
  (setq sunrise-synchronized (not sunrise-synchronized))
  (mapc 'sunrise-mark-sync (list sunrise-left-buffer sunrise-right-buffer))
  (message "Sunrise: sync navigation is now %s" (if sunrise-synchronized "ON" "OFF"))
  (run-hooks 'sunrise-refresh-hook)
  (sunrise-in-other (run-hooks 'sunrise-refresh-hook)))

(defun sunrise-mark-sync (&optional buffer)
  "Change `mode-name' depending on whether synchronized navigation is enabled."
  (save-window-excursion
    (if buffer
        (switch-to-buffer buffer))
    (setq mode-name (concat "Sunrise "
                            (if sunrise-synchronized "SYNC-NAV" "Commander")))))

;; This advertises synchronized navigation in all new buffers:
(add-hook 'sunrise-mode-hook 'sunrise-mark-sync)

(defun sunrise-next-line-other ()
  "Move the cursor down in the passive pane."
  (interactive)
  (sunrise-in-other (dired-next-line 1)))

(defun sunrise-prev-line-other ()
  "Move the cursor up in the passive pane."
  (interactive)
  (sunrise-in-other (dired-next-line -1)))

(defun sunrise-goto-dir-other (dir)
  "Change the current directory in the passive pane to the given one."
  (interactive (list (read-directory-name
                      "Change directory in PASSIVE pane (file or pattern): "
                      sunrise-other-directory)))
  (sunrise-in-other (sunrise-goto-dir dir)))

(defun sunrise-advertised-find-file-other ()
  "Open the file/directory selected in the passive pane."
  (interactive)
  (if sunrise-synchronized
      (let ((target (sunrise-directory-name-proper (dired-get-filename))))
        (sunrise-change-window)
        (if (file-directory-p target)
            (sunrise-goto-dir (expand-file-name target))
          (if (y-or-n-p "Unable to synchronize. Disable sync navigation? ")
              (sunrise-sync)))
        (sunrise-change-window)
        (sunrise-advertised-find-file))
    (sunrise-in-other (sunrise-advertised-find-file))))

(defun sunrise-mouse-advertised-find-file (_e)
  "Open the file/directory pointed to by the mouse."
  (interactive "e")
  (sunrise-advertised-find-file))

(defun sunrise-prev-subdir-other (&optional count)
  "Go to the previous subdirectory in the passive pane."
  (interactive "P")
  (let ((count (or count 1)))
    (sunrise-in-other (sunrise-dired-prev-subdir count))))

(defun sunrise-follow-file-other ()
  "Go to the directory of the selected file, but in the passive pane."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (sunrise-in-other (sunrise-follow-file filename))))

(defun sunrise-history-prev-other ()
  "Change to previous directory (if any) in the passive pane's history list."
  (interactive)
  (sunrise-in-other (sunrise-history-prev)))

(defun sunrise-history-next-other ()
  "Change to the next directory (if any) in the passive pane's history list."
  (interactive)
  (sunrise-in-other (sunrise-history-next)))

(defun sunrise-mark-other (arg)
  "Mark the current (or next ARG) files in the passive pane."
  (interactive "P")
  (setq arg (or arg 1))
  (sunrise-in-other (dired-mark arg)))

(defun sunrise-unmark-backward-other (arg)
  (interactive "p")
  (sunrise-in-other (dired-unmark-backward arg)))

(defun sunrise-unmark-all-marks-other ()
  "Remove all marks from the passive pane."
  (interactive)
  (sunrise-in-other (dired-unmark-all-marks)))

;;; ============================================================================
;;; Progress feedback functions:

(defun sunrise-progress-prompt (op-name)
  "Build the default progress feedback message."
  (concat "Sunrise: " op-name "... "))

(defun sunrise-make-progress-reporter (op-name totalsize)
  "Make a new Sunrise progress reporter.
Prepends two integers (accumulator and scale) to a standard
progress reporter (built using `make-progress-reporter' from
subr.el): accumulator keeps the current state of the reporter,
and scale is used when the absolute value of 100% is bigger than
`most-positive-fixnum'."
  (let ((accumulator 0) (scale 1) (maxval totalsize))
    (when (> totalsize most-positive-fixnum)
      (setq scale (/ totalsize most-positive-fixnum))
      (setq maxval most-positive-fixnum))
    (list accumulator scale
          (make-progress-reporter
           (sunrise-progress-prompt op-name) 0 maxval 0 1 0.5))))

(defun sunrise-progress-reporter-update (reporter size)
  "Update REPORTER (a Sunrise progress reporter) by adding SIZE to its state."
  (let ((scale (cadr reporter)))
    (setcar reporter (+ (truncate (/ size scale)) (car reporter)))
    (progress-reporter-update (car (cddr reporter)) (car reporter))))

(defun sunrise-progress-reporter-done (reporter)
  "Print REPORTER's feedback message followed by \"done\" in echo area."
  (progress-reporter-done (car (cddr reporter))))

;;; ============================================================================
;;; File manipulation functions:

(defun sunrise-create-files (&optional qty)
  "Interactively create empty file(s) with the given name or template.
Optional prefix argument specifies the number of files to create.
*NEVER* overwrites existing files. A template may contain one
%-sequence like those used by `format', but the only supported
specifiers are: d (decimal), x (hex) or o (octal)."
  (interactive "p")
  (let* ((qty (or (and (integerp qty) (< 0 qty) qty) 1))
         (prompt (if (>= 1 qty) "Create file: "
                   (format "Create %d files using template: " qty)))
         (filename (read-file-name prompt)) (name))
    (with-temp-buffer
      (if (>= 1 qty)
          (unless (file-exists-p filename) (write-file filename))
        (unless (string-match "%[0-9]*[dox]" filename)
          (setq filename (concat filename ".%d")))
        (setq filename (replace-regexp-in-string "%\\([^%]\\)" "%%\\1" filename)
              filename (replace-regexp-in-string
                        "%%\\([0-9]*[dox]\\)" "%\\1" filename))
        (dotimes (n qty)
          (setq name (format filename (1+ n)))
          (unless (file-exists-p name) (write-file name)))))
    (sunrise-revert-buffer)))

(defun sunrise-editable-pane ()
  "Put the current pane in File Names Editing mode (`wdired-mode')."
  (interactive)
  (sunrise-graphical-highlight 'sunrise-editing-path-face)
  (let* ((was-virtual (eq major-mode 'sunrise-virtual-mode))
         (major-mode 'dired-mode))
    (wdired-change-to-wdired-mode)
    (if was-virtual
        (set (make-local-variable 'sunrise-virtual-buffer) t)))
  (run-hooks 'sunrise-refresh-hook))

(defun sunrise-readonly-pane (as-virtual)
  "Put the current pane back in Sunrise mode."
  (when as-virtual
    (sunrise-virtual-mode)
    (sunrise-force-passive-highlight t))
  (dired-build-subdir-alist)
  (sunrise-revert-buffer))

(defmacro sunrise-protect-terminate-wdired (&rest body)
  "Compile the `cl-letf' forms used in `sunrise-terminate-wdired'.
This macro allows interpreted code to work without requiring
cl-macs at runtime."
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (ignore)))
          ((symbol-function 'revert-buffer)
           (lambda (&optional ignore-auto noconfirm preserve-modes))
           (ignore)))
     ,@body))

(defun sunrise-terminate-wdired (fun)
  "Restore the current pane's original mode after editing with WDired."
  (ad-add-advice
   fun
   (ad-make-advice
    (intern (concat "sunrise-advice-" (symbol-name fun))) nil t
    `(advice
      lambda ()
      (if (not sunrise-running)
          ad-do-it
        (let ((was-virtual (local-variable-p 'sunrise-virtual-buffer))
              (saved-point (point)))
          (sunrise-save-aspect
           (setq major-mode 'wdired-mode)
           (sunrise-protect-terminate-wdired ad-do-it)
           (sunrise-readonly-pane was-virtual)
           (goto-char saved-point))
          (sunrise-unhighlight 'sunrise-editing-path-face)))))
   'around 'last)
  (ad-activate fun nil))
(sunrise-terminate-wdired 'wdired-finish-edit)
(sunrise-terminate-wdired 'wdired-abort-changes)

(defun sunrise-do-copy ()
  "Copy selected files and directories recursively to the passive pane."
  (interactive)
  (let* ((items (dired-get-marked-files nil))
         (vtarget (sunrise-virtual-target))
         (target (or vtarget sunrise-other-directory))
         (progress))
    (if (and (not vtarget) (sunrise-equal-dirs default-directory sunrise-other-directory))
        (dired-do-copy)
      (when (sunrise-ask "Copy" target items #'y-or-n-p)
        (if vtarget
            (progn
              (sunrise-copy-virtual)
              (message "Done: %d items(s) copied" (length items)))
          (progn
            (setq progress (sunrise-make-progress-reporter
                            "copying" (sunrise-files-size items)))
            (sunrise-clone items target #'copy-file progress ?C)
            (sunrise-progress-reporter-done progress)))
        (sunrise-silently (dired-unmark-all-marks))))))

(defun sunrise-do-symlink ()
  "Symlink selected files or directories from one pane to the other."
  (interactive)
  (if (sunrise-equal-dirs default-directory sunrise-other-directory)
      (dired-do-symlink)
    (sunrise-link #'make-symbolic-link "Symlink" dired-keep-marker-symlink)))

(defun sunrise-do-relsymlink ()
  "Symlink selected files or directories from one pane to the other relatively.
See `dired-make-relative-symlink'."
  (interactive)
  (if (sunrise-equal-dirs default-directory sunrise-other-directory)
      (dired-do-relsymlink)
    (sunrise-link #'dired-make-relative-symlink
             "RelSymLink"
             dired-keep-marker-relsymlink)))

(defun sunrise-do-hardlink ()
  "Same as `dired-do-hardlink', but refuse to hardlink files to VIRTUAL buffers."
  (interactive)
  (if (sunrise-virtual-target)
      (error "Cannot hardlink files to a VIRTUAL buffer, try (C)opying instead")
    (dired-do-hardlink)))

(defun sunrise-do-rename ()
  "Move selected files and directories recursively from one pane to the other."
  (interactive)
  (when (sunrise-virtual-target)
    (error "Cannot move files to a VIRTUAL buffer, try (C)opying instead"))
  (if (sunrise-equal-dirs default-directory sunrise-other-directory)
      (dired-do-rename)
    (let ((marked (dired-get-marked-files)))
      (when (sunrise-ask "Move" sunrise-other-directory marked #'y-or-n-p)
        (let ((names (mapcar #'file-name-nondirectory marked))
              (progress (sunrise-make-progress-reporter "renaming" (length marked)))
              (inhibit-read-only t))
          (sunrise-in-other
           (progn
             (sunrise-move-files marked default-directory progress)
             (revert-buffer)
             (when (eq major-mode 'sunrise-mode)
               (dired-mark-remembered
                (mapcar (lambda (x) (cons (expand-file-name x) ?R)) names))
               (sunrise-focus-filename (car names)))))
          (sunrise-progress-reporter-done progress))
        (sunrise-silently (revert-buffer))))))

(defun sunrise-do-delete ()
  "Remove selected files from the file system."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (mode (sunrise-ask "Delete" nil files #'sunrise-y-n-or-a-p))
         (deletion-mode (cond ((eq mode 'ALWAYS) 'always)
                              (mode 'top)
                              (t (error "(No deletions performed)")))))
    (mapc (lambda (x)
            (message "Deleting %s" x)
            (dired-delete-file x deletion-mode delete-by-moving-to-trash))
          files)
    (if (eq major-mode 'sunrise-virtual-mode)
        (dired-do-kill-lines)
      (revert-buffer))))

(defun sunrise-do-flagged-delete ()
  "Remove flagged files from the file system."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
         (regexp (dired-marker-regexp)) )
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
        (sunrise-do-delete)
      (message "(No deletions requested)"))))

(defun sunrise-do-clone-prompt (&optional is-fs)
  "Prompt for the criteria to use when performing a clone operation."
  (let* ((menu "(D)irectories only, (C)opies, (H)ardlinks, (S)ymlinks or (R)elative symlinks? ")
         (maybe-fs (and (sunrise-virtual-source) (not (sunrise-virtual-target))))
         (prompt (cond (is-fs (concat "Clone as file system of: " menu))
                       (maybe-fs (concat "Clone as: (F)ile System of: " menu))
                       (t (concat "Clone as: " menu))))
         (resp (read-event prompt)))

    (cond ((and maybe-fs (memq resp '(?f ?F))) (sunrise-do-clone-prompt t))
          ((not (memq resp '(?d ?D ?c ?C ?h ?H ?s ?S ?r ?R))) (sunrise-do-clone-prompt))
          (is-fs (list resp ?t))
          (t (list resp)))))

(defun sunrise-do-clone (&optional mode is-fs)
  "Clone all selected items recursively into the passive pane."
  (interactive (sunrise-do-clone-prompt))

  (if (sunrise-virtual-target)
      (error "Cannot clone into a VIRTUAL buffer, try (C)opying instead"))
  (if (sunrise-equal-dirs default-directory sunrise-other-directory)
      (error "Cannot clone inside one single directory, please select a\
 different one in the passive pane"))

  (let ((target sunrise-other-directory) clone-op items progress)
    (if (and mode (>= mode 97)) (setq mode (- mode 32)))
    (setq clone-op
          (case mode
            (?D nil)
            (?C #'copy-file)
            (?H #'add-name-to-file)
            (?S #'make-symbolic-link)
            (?R #'dired-make-relative-symlink)
            (t (error "Invalid cloning mode: %c" mode))))
    (setq items (dired-get-marked-files nil))
    (setq progress (sunrise-make-progress-reporter
                    "cloning" (sunrise-files-size items)))
    (if is-fs
        (sunrise-clone-fs (dired-get-marked-files t) target clone-op progress)
      (sunrise-clone (dired-get-marked-files nil) target clone-op progress ?K))
    (dired-unmark-all-marks)
    (message "Done: %d items(s) dispatched" (length items))))

(defun sunrise-fast-backup-files ()
  "Make backup copies of all marked files inside the same directory.
The extension to append to each filename can be controlled by
setting the value of the `sunrise-fast-backup-extension' custom
variable. Directories are not copied."
  (interactive)
  (let ((extension (if (listp sunrise-fast-backup-extension)
                       (eval sunrise-fast-backup-extension)
                     sunrise-fast-backup-extension)))
    (dired-do-copy-regexp "$" extension))
  (revert-buffer))

(defun sunrise-clone-fs (items target clone-op progress)
  "Clone all the given ITEMS (paths to files and/or directories)
recursively to TARGET (a directory), but keeping the directory
structure given by every path in ITEMS. CLONE-OP is the cloning
operation and PROGRESS is the progress monitor."
  (mapc (lambda (i)
          (let* ((from (expand-file-name i))
                 (to (concat (directory-file-name target) "/"
                             (or (file-name-directory i) ""))))
            (unless (file-exists-p to)
              (make-directory to t))
            (sunrise-clone (list from) to clone-op progress nil)))
        items))

(defun sunrise-clone (items target clone-op progress mark-char)
  "Clone all the given ITEMS (files and directories) recursively
to TARGET (a directory) using CLONE-OP as the cloning operation
and reporting progress to the given PROGRESS monitor. Finally,
mark all resulting artifacts with the MARK-CHAR mark."
  (let ((names (mapcar #'file-name-nondirectory items))
        (inhibit-read-only t))
    (with-current-buffer (sunrise-other 'buffer)
      (sunrise-clone-files items target clone-op progress))
    (when (window-live-p (sunrise-other 'window))
      (sunrise-in-other
       (progn
         (revert-buffer)
         (when (and mark-char (memq major-mode '(sunrise-mode sunrise-virtual-mode)))
           (dired-mark-remembered
            (mapcar (lambda (x) (cons (expand-file-name x) mark-char)) names))
           (sunrise-focus-filename (car names))))))))

(defun sunrise-clone-files (file-paths target-dir clone-op progress
                                  &optional do-overwrite)
  "Clone all files in FILE-PATHS to TARGET-DIR using CLONE-OP to
clone the files. FILE-PATHS should be a list of absolute paths."
  (setq target-dir (replace-regexp-in-string "/?$" "/" target-dir))
  (mapc
   (function
    (lambda (f)
      (sunrise-progress-reporter-update progress (nth 7 (file-attributes f)))
      (let* ((name (file-name-nondirectory f))
             (target-file (concat target-dir name))
             (symlink-to (file-symlink-p (sunrise-chop ?/ f)))
             (clone-args (list f target-file t)))
        (cond
         (symlink-to
          (progn
            (if (file-exists-p symlink-to)
                (setq symlink-to (expand-file-name symlink-to)))
            (make-symbolic-link symlink-to target-file do-overwrite)))

         ((file-directory-p f)
          (let ((initial-path (file-name-directory f)))
            (unless (file-symlink-p initial-path)
              (sunrise-clone-directory
               initial-path name target-dir clone-op progress do-overwrite))))

         (clone-op
          ;; (message "[[Cloning: %s => %s]]" f target-file)
          (if (eq clone-op 'copy-file)
              (setq clone-args
                    (append clone-args (list dired-copy-preserve-time))))
          (if (file-exists-p target-file)
              (if (or (eq do-overwrite 'ALWAYS)
                      (setq do-overwrite (sunrise-ask-overwrite target-file)))
                  (apply clone-op clone-args))
            (apply clone-op clone-args)))))))
   file-paths))

(defun sunrise-clone-directory (in-dir d to-dir clone-op progress do-overwrite)
  "Clone directory IN-DIR/D and all its files recursively to TO-DIR.
IN-DIR/D => TO-DIR/D using CLONE-OP to clone the files."
  (setq d (replace-regexp-in-string "/?$" "/" d))
  (if (string= "" d)
      (setq to-dir (concat to-dir (sunrise-directory-name-proper in-dir))))
  (let* ((files-in-d (sunrise-list-of-contents (concat in-dir d)))
         (file-paths-in-d
          (mapcar (lambda (f) (concat in-dir d f)) files-in-d)))
    (unless (file-exists-p (concat to-dir d))
      (make-directory (concat to-dir d)))
    (sunrise-clone-files file-paths-in-d (concat to-dir d) clone-op progress do-overwrite)))

(defsubst sunrise-move-op (file target-dir progress do-overwrite)
  "Helper function used by `sunrise-move-files' to rename files and directories."
  (condition-case nil
      (dired-rename-file file target-dir do-overwrite)
    (error
     (sunrise-clone-directory file "" target-dir 'copy-file progress do-overwrite)
     (dired-delete-file file 'always))))

(defun sunrise-move-files (file-path-list target-dir progress &optional do-overwrite)
  "Move all files in FILE-PATH-LIST (list of full paths) to TARGET-DIR."
  (mapc
   (function
    (lambda (f)
      (if (file-directory-p f)
          (progn
            (setq f (replace-regexp-in-string "/?$" "/" f))
            (sunrise-progress-reporter-update progress 1)
            (sunrise-move-op f target-dir progress do-overwrite))
        (let* ((name (file-name-nondirectory f))
               (target-file (concat target-dir name)))
          ;; (message "Renaming: %s => %s" f target-file)
          (sunrise-progress-reporter-update progress 1)
          (if (file-exists-p target-file)
              (if (or (eq do-overwrite 'ALWAYS)
                      (setq do-overwrite (sunrise-ask-overwrite target-file)))
                  (dired-rename-file f target-file t))
            (dired-rename-file f target-file t)) ))))
   file-path-list))

(defun sunrise-link (creator action marker)
  "Helper function for implementing `sunrise-do-symlink' and `sunrise-do-relsymlink'."
  (if (sunrise-virtual-target)
      (error "Cannot link files to a VIRTUAL buffer, try (C)opying instead.")
    (dired-create-files creator action (dired-get-marked-files nil)
                        (lambda (from)
                          (setq from (sunrise-chop ?/ from))
                          (if (file-directory-p from)
                              (setq from (sunrise-directory-name-proper from))
                            (setq from (file-name-nondirectory from)))
                          (expand-file-name from sunrise-other-directory))
                        marker)))

(defun sunrise-inplace ()
  "Allow to select an in-place operation and execute it.
In-place operations are file operations that are executed in the
context of the current pane, totally ignoring the other one."
  (interactive)
  (let ((mode (read-char "In-place: (C)opy, (R)ename, (H)ardlink, (S)ymlink")))
    (if (and mode (>= mode 97)) (setq mode (- mode 32)))
    (case mode
      (?C (sunrise-inplace-do #'copy-file "Copy in place to"))
      (?R (sunrise-inplace-do #'rename-file "Rename in place to"))
      (?H (sunrise-inplace-do #'add-name-to-file "Add name in place"))
      (?S (sunrise-inplace-do #'make-symbolic-link "Link in place to"))
      (t (sunrise-inplace)))))

(defun sunrise-inplace-do (action prompt)
  "Perform the given ACTION in the context of the current pane.
The given PROMPT will be displayed to the user interactively."
  (let* ((marked (dired-get-marked-files))
         (prompt (concat prompt ": "))
         (target
          (if (cdr marked)
              (read-directory-name prompt)
            (read-file-name
             prompt nil nil nil (file-name-nondirectory (car marked)))))
         (progress (sunrise-make-progress-reporter "working" (length marked)))
         (inhibit-read-only t))

    (when (< 1 (length marked))
      (if (file-exists-p target)
          (unless (file-directory-p target)
            (error "Sunrise: Multiple selection, but target is not a directory"))
        (if (y-or-n-p (format "Directory %s does not exit. Create? " target))
            (make-directory target t)
          (error "Sunrise: Unable to proceed - aborting"))))

    (mapc (lambda (x)
            (if (and (not (equal (expand-file-name x) (expand-file-name target)))
                     (or (not (file-exists-p target))
                         (file-directory-p target)
                         (y-or-n-p (format "File %s exists. OK to overwrite? "
                                           target))))
                (funcall action x target t)))
          marked)
    (revert-buffer)
    (sunrise-progress-reporter-done progress)))

(defun sunrise-virtual-source ()
  "if the active pane is in VIRTUAL mode, return its name as a string.
Otherwise return nil."
  (if (eq major-mode 'sunrise-virtual-mode)
      (or (buffer-file-name) "Sunrise VIRTUAL buffer")
    nil))

(defun sunrise-virtual-target ()
  "If the passive pane is in VIRTUAL mode, return its name as a string.
Otherwise return nil."
  (save-window-excursion
    (switch-to-buffer (sunrise-other 'buffer))
    (sunrise-virtual-source)))

(defun sunrise-copy-virtual ()
  "Manage copying of files or directories to buffers in VIRTUAL mode."
  (let ((fileset (dired-get-marked-files nil))
        (inhibit-read-only t) (beg))
    (sunrise-change-window)
    (goto-char (point-max))
    (setq beg (point))
    (mapc (lambda (file)
            (insert-char 32 2)
            (setq file (dired-make-relative file default-directory)
                  file (sunrise-chop ?/ file))
            (insert-directory file sunrise-virtual-listing-switches))
          fileset)
    (sunrise-display-attributes beg (point-at-eol) sunrise-show-file-attributes)
    (unwind-protect
        (delete-region (point) (line-end-position))
      (progn
        (sunrise-change-window)
        (dired-unmark-all-marks)))))

(defun sunrise-ask (prompt target files function)
  "Use FUNCTION to ask whether to do PROMPT on FILES with TARGET as destination."
  (if (and files (listp files))
      (let* ((len (length files))
             (msg (if (< 1 len)
                      (format "* [%d items]" len)
                    (file-name-nondirectory (car files)))))
        (if target
            (setq msg (format "%s to %s" msg target)))
        (funcall function (format "%s %s? " prompt msg)))))

(defun sunrise-ask-overwrite (file-name)
  "Ask whether to overwrite the given FILE-NAME."
  (sunrise-y-n-or-a-p (format "File %s exists. OK to overwrite? " file-name)))

(defun sunrise-y-n-or-a-p (prompt)
  "Ask the user with PROMPT for an answer y/n/a ('a' stands for 'always').
Returns t if the answer is y/Y, nil if the answer is n/N or the
symbol `ALWAYS' if the answer is a/A."
  (setq prompt (concat prompt "([y]es, [n]o or [a]lways)"))
  (let ((resp -1))
    (while (not (memq resp '(?y ?Y ?n ?N ?a ?A)))
      (setq resp (read-event prompt))
      (setq prompt "Please answer [y]es, [n]o or [a]lways "))
    (if (>= resp 97)
        (setq resp (- resp 32)))
    (case resp
      (?Y t)
      (?A 'ALWAYS)
      (t nil))))

(defun sunrise-overlapping-paths-p (dir1 dir2)
  "Return non-nil if directory DIR2 is located inside directory DIR1."
  (when (and dir1 dir2)
    (setq dir1 (expand-file-name (file-name-as-directory dir1))
          dir2 (expand-file-name dir2))
    (if (>= (length dir2) (length dir1))
        (equal (substring dir2 0 (length dir1)) dir1)
      nil)))

(defun sunrise-list-of-contents (dir)
  "Return the list of all files in DIR as a list of strings."
  (sunrise-filter (function (lambda (x) (not (string-match "\\.\\.?/?$" x))))
             (directory-files dir)))

(defun sunrise-list-of-directories (dir)
 "Return the list of directories in DIR as a list of strings.
The list does not include the current directory and the parent directory."
 (let ((result (sunrise-filter (function (lambda (x)
                                      (file-directory-p (concat dir "/" x))))
                          (sunrise-list-of-contents dir))))
   (mapcar (lambda (x) (concat x "/")) result)))

(defun sunrise-list-of-files (dir)
  "Return the list of regular files in DIR as a list of strings.
Broken links are *not* considered regular files."
  (sunrise-filter
   (function (lambda (x) (file-regular-p (concat dir "/" x))))
   (sunrise-list-of-contents dir)))

(defun sunrise-filter (p x)
  "Return the elements of the list X that satisfy the predicate P."
  (let ((res-list nil))
    (while x
      (if (apply p (list (car x)))
          (setq res-list (cons (car x) res-list)))
      (setq x (cdr x)))
    (reverse res-list)))

(defun sunrise-directory-name-proper (file-path)
  "Return the proper name of the directory FILE-PATH, without initial path."
  (if file-path
      (let (
            (file-path-1 (substring file-path 0 (- (length file-path) 1)))
            (lastchar (substring file-path (- (length file-path) 1)))
            )
        (concat (file-name-nondirectory file-path-1) lastchar))))

;;; ============================================================================
;;; Directory and file comparison functions:

(defun sunrise-compare-panes ()
  "Compare the contents of Sunrise panes."
  (interactive)
  (let* ((file-alist1 (sunrise-files-attributes))
         (other (sunrise-other 'buffer))
         (file-alist2 (with-current-buffer other (sunrise-files-attributes)))
         (progress
          (sunrise-make-progress-reporter
           "comparing" (+ (length file-alist1) (length file-alist2))))
         (predicate `(prog1 ,(sunrise-ask-compare-panes-predicate)
                            (sunrise-progress-reporter-update ',progress 1)))
         (file-list1 (mapcar 'cadr (dired-file-set-difference
                                    file-alist1 file-alist2 predicate)))
         (file-list2 (mapcar 'cadr (dired-file-set-difference
                                    file-alist2 file-alist1 predicate))))
    (sunrise-md5 nil)
    (dired-mark-if (member (dired-get-filename nil t) file-list1) nil)
    (with-current-buffer other
      (dired-mark-if (member (dired-get-filename nil t) file-list2) nil))
    (message "Marked in pane1: %s files, in pane2: %s files"
             (length file-list1)
             (length file-list2))
    (sit-for 0.2)))

(defun sunrise-ask-compare-panes-predicate ()
  "Prompt for the criterion to use for comparing the contents of the panes."
  (let ((prompt "Compare by (d)ate, (s)ize, date_(a)nd_size, (n)ame \
or (c)ontents? ")
        (response -1))
    (while (not (memq response '(?d ?D ?s ?S ?a ?A ?n ?N ?c ?C)))
      (setq response (read-event prompt))
      (setq prompt "Please select: Compare by (d)ate, (s)ize, date_(a)nd_size,\
 (n)ame or (c)ontents? "))
    (if (>= response 97)
        (setq response (- response 32)))
    (case response
      (?D `(not (= mtime1 mtime2)))
      (?S `(not (= size1 size2)))
      (?N nil)
      (?C `(not (string= (sunrise-md5 file1 t) (sunrise-md5 file2 t))))
      (t `(or (not (= mtime1 mtime2)) (not (= size1 size2)))))))

(defun sunrise-files-attributes ()
  "Return a list of all file names and attributes in the current pane.
The list has the same form as the one returned by
`dired-files-attributes', but contains all the files currently
displayed in VIRTUAL panes."
  (delq
   nil
   (mapcar
    (lambda (file-name)
      (unless (member file-name '("." ".."))
        (let ((full-file-name (expand-file-name file-name default-directory)))
          (list file-name full-file-name (file-attributes full-file-name)))))
    (sunrise-pane-files))))

(defun sunrise-pane-files ()
  "Return the list of files in the current pane.
For VIRTUAL panes, returns the list of all files being currently
displayed."
  (delq
   nil
   (if (eq major-mode 'sunrise-virtual-mode)
       (sunrise-buffer-files (current-buffer))
     (directory-files default-directory))))

(defvar sunrise-md5 '(nil) "Memoization cache for the sunrise-md5 function.")
(defun sunrise-md5 (file-alist &optional memoize)
  "Build and execute a shell command to calculate the MD5 checksum of a file.
Second element of FILE-ALIST is the absolute path of the file. If
MEMOIZE is non-nil, save the result into the `sunrise-md5' alist so it
can be reused the next time this function is called with the same
path. This cache can be cleared later calling `sunrise-md5' with nil
as its first argument."
  (if (null file-alist)
      (setq sunrise-md5 '(nil))
    (let* ((filename (cadr file-alist))
           (md5-digest (cdr (assoc filename sunrise-md5)))
           (md5-command))
      (unless md5-digest
        (setq md5-command
              (replace-regexp-in-string
               "%f" (format "\"%s\"" filename) sunrise-md5-shell-command))
        (setq md5-digest (shell-command-to-string md5-command))
        (if memoize
            (push (cons filename md5-digest) sunrise-md5)))
      md5-digest)))

(defun sunrise-diff ()
  "Run `diff' on the top two marked files in both panes."
  (interactive)
  (eval (sunrise-diff-form 'diff))
  (sunrise-scrollable-viewer (get-buffer "*Diff*")))

(defun sunrise-ediff ()
  "Run `ediff' on the two top marked files in both panes."
  (interactive)
  (eval (sunrise-diff-form 'ediff)))

(add-hook 'ediff-before-setup-windows-hook
          (defun sunrise-ediff-before-setup-windows-function ()
            (setq sunrise-ediff-on t)))

(add-hook 'ediff-quit-hook
          (defun sunrise-ediff-quit-function ()
            (setq sunrise-ediff-on nil)
            (when sunrise-running
              (if (buffer-live-p sunrise-restore-buffer)
                  (switch-to-buffer sunrise-restore-buffer))
              (delete-other-windows)
              (sunrise-setup-windows)
              (sunrise-graphical-highlight))))

(defun sunrise-diff-form (fun)
  "Return the appropriate form to evaluate for comparing files using FUN."
  (let ((this (sunrise-pop-mark)) (other nil))
    (unless this
      (setq this (car (dired-get-marked-files t))))
    (if (sunrise-equal-dirs default-directory sunrise-other-directory)
        (setq other (sunrise-pop-mark))
      (progn
        (sunrise-change-window)
        (setq other (sunrise-pop-mark))
        (sunrise-change-window)
        (setq other (or other
                        (if (file-exists-p (concat sunrise-other-directory this))
                            this
                          (file-name-nondirectory this))))))
    (setq this (concat default-directory this)
          other (concat sunrise-other-directory other))
    (list fun this other)))

(defun sunrise-pop-mark ()
  "Pop the first mark in the current Dired buffer."
  (let ((result nil))
    (condition-case description
      (save-excursion
        (goto-char (point-min))
        (dired-next-marked-file 1)
        (setq result (dired-get-filename t t))
        (dired-unmark 1))
      (error (message (cadr description))))
    result))

;;; ============================================================================
;;; File search & analysis functions:

(defun sunrise-process-kill ()
  "Kill the process running in the current buffer (if any)."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
         (condition-case nil
             (delete-process proc)
           (error nil)))))

(defvar sunrise-process-map (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map sunrise-virtual-mode-map)
                         (define-key map "\C-c\C-k" 'sunrise-process-kill)
                         map)
  "Local map used in Sunrise panes during find and locate operations.")

(defun sunrise-find-decorate-buffer (find-items)
  "Provide details on `sunrise-find' execution in the current buffer.
If the current find operation is done only in selected files and directories,
modify the info line of the buffer to reflect this. Additionally, display an
appropriate message in the minibuffer."
  (rename-uniquely)
  (when find-items
    (let ((items-len (length find-items))
          (max-items-len (window-width))
          (inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 1)
      (when (re-search-forward "find \." nil t)
        (if (> items-len max-items-len)
            (setq find-items
                  (concat (substring find-items 0 max-items-len) " ...")))
        (replace-match (format "find %s" find-items)))))
  (sunrise-beginning-of-buffer)
  (sunrise-highlight)
  (hl-line-mode 1)
  (message (propertize "Sunrise find (C-c C-k to kill)"
                       'face 'minibuffer-prompt)))

(defun sunrise-find-apply (fun pattern)
  "Helper function for functions `sunrise-find', `sunrise-find-name' and `sunrise-find-grep'."
  (let* ((suffix (if (eq 'w32 window-system) " {} ;" " \\{\\} \\;"))
         (find-ls-option
          (cons
           (concat "-exec ls -d " sunrise-virtual-listing-switches suffix)
           "ls -ld"))
         (sunrise-find-items (sunrise-quote-marked)) (dir))
    (when sunrise-find-items
      (if (not (y-or-n-p "Find in marked items only? "))
          (setq sunrise-find-items nil)
        (setq dir (directory-file-name (expand-file-name default-directory)))
        (add-to-list 'file-name-handler-alist (cons dir 'sunrise-multifind-handler))))
    (sunrise-save-aspect
     (sunrise-alternate-buffer (apply fun (list default-directory pattern)))
     (sunrise-virtual-mode)
     (use-local-map sunrise-process-map)
     (sunrise-keep-buffer))
    (run-with-idle-timer 0.01 nil 'sunrise-find-decorate-buffer sunrise-find-items)))

(defun sunrise-find (pattern)
  "Run `find-dired' passing the current directory as first parameter."
  (interactive "sRun find (with args): ")
  (sunrise-find-apply 'find-dired pattern))

(defun sunrise-find-name (pattern)
  "Run `find-name-dired' passing the current directory as first parameter."
  (interactive "sFind name pattern: ")
  (sunrise-find-apply 'find-name-dired pattern))

(defun sunrise-find-grep (pattern)
  "Run `find-grep-dired' passing the current directory as first
parameter. Called with prefix asks for additional grep options."
  (interactive "sFind files containing pattern: ")
  (let ((find-grep-options
         (if current-prefix-arg
             (concat find-grep-options
                     " "
                     (read-string "Additional Grep Options: "))
         find-grep-options)))
    (sunrise-find-apply 'find-grep-dired pattern)))

(defadvice find-dired-sentinel
  (after sunrise-advice-find-dired-sentinel (proc state))
  "If the current find operation was launched inside the Sunrise
Commander, create a new backup buffer on operation completion or
abort."
  (with-current-buffer (process-buffer proc)
    (when (eq 'sunrise-virtual-mode major-mode)
      (sunrise-backup-buffer))))
(ad-activate 'find-dired-sentinel)

(defadvice find-dired-filter
  (around sunrise-advice-find-dired-filter (proc string))
  "Disable the \"non-foolproof\" padding mechanism in `find-dired-filter' that
breaks Dired when using ls options that omit some columns (like g or G). Defined
by the Sunrise Commander."
  (if (and (eq 'sunrise-virtual-mode major-mode)
           (or (string-match "g" sunrise-virtual-listing-switches)
               (string-match "G" sunrise-virtual-listing-switches)))
      (let ((find-ls-option nil)) ad-do-it)
    ad-do-it))
(ad-activate 'find-dired-filter)

(defun sunrise-multifind-handler (operation &rest args)
  "Magic file name handler for manipulating the command executed by `find-dired'
when the user requests to perform the find operation on all currently marked
items (as opposed to the current default directory). Removes itself from the
`inhibit-file-name-handlers' every time it's executed."
  (let ((inhibit-file-name-handlers
         (cons 'sunrise-multifind-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (when (eq operation 'shell-command)
      (setq file-name-handler-alist
            (rassq-delete-all 'sunrise-multifind-handler file-name-handler-alist))
      (when sunrise-find-items
        (setcar args (replace-regexp-in-string
                      "find \." (format "find %s" sunrise-find-items) (car args)))))
    (apply operation args)))

(defvar sunrise-as-pending nil
  "Buffer-local variable used by async search operations to keep
partial process output between consecutive batches of data.")

(defun sunrise-as-filter (as-buffer &optional as-filter)
  "Return a filter function for an async search process."
  `(lambda (process output)
     (let ((inhibit-read-only t)
           (beg (point-max))
           (as-filter (or (quote ,as-filter) #'identity))
           (entries))

       (setq output (concat sunrise-as-pending output)
             entries (split-string output "[\r\n]" t))

       (set (make-local-variable 'sunrise-as-pending) "")
       (unless (string-match "[\r\n]$" output)
         (setq sunrise-as-pending (car (last entries))))

       (set-buffer ,as-buffer)
       (save-excursion
         (mapc (lambda (x)
                 (when (and (apply as-filter (list x))
                            (not (eq x sunrise-as-pending)))
                   (setq x (replace-regexp-in-string "\\./" "" x))
                   (goto-char (point-max))
                   (insert-char 32 2)
                   (insert-directory x sunrise-virtual-listing-switches nil nil)))
               entries)
         (sunrise-display-attributes beg (point-at-eol) sunrise-show-file-attributes)))))

(defun sunrise-as-sentinel (as-buffer as-command)
  "Return a sentinel function for an async search process.
Used to notify about the termination status of the process."
  `(lambda (process status)
     (let ((inhibit-read-only t))
       (set-buffer ,as-buffer)
       (goto-char (point-max))
       (insert "\n " ,as-command " " status)
       (forward-char -1)
       (insert " at " (substring (current-time-string) 0 19))
       (forward-char 1))
     (sunrise-beginning-of-buffer)
     (sunrise-highlight)
     (hl-line-mode 1)))

(defun sunrise-as-prompt (as-command)
  "Display the message that appears when an async search process is launched."
  (message (propertize (format "Sunrise %s (C-c C-k to kill)" as-command)
                       'face 'minibuffer-prompt)))

(defun sunrise-as-search (as-label as-command as-filter &rest as-args)
  "Launch an asyncronous search operation.

AS-LABEL is a name to use for displaying in messages etc.

AS-COMMAND is the path to the search command to invoke.

AS-FILTER is an optional filter to test every entry returned by
the search process - only those entries for which this filter
returns non-nil will be included in the result.

AS-ARGS are all additional arguments needed to execute the
operation.

Please note that this facility executes its processes directly,
without the intermediation of a shell, so spaces as separators
are not supported in any of the arguments."

  (let ((as-buffer (create-file-buffer (format "*Sunrise %s*" as-command)))
        (as-process-args
         (append (list (format "Async %s" as-label) nil as-command) as-args))
        (as-process nil))
    (sunrise-save-aspect
     (sunrise-alternate-buffer (switch-to-buffer as-buffer))
     (insert "  " default-directory ":") (newline)
     (insert (format " Results of: %s %s" as-command
                     (substring (format "%s" as-args) 1 -1)))
     (newline)
     (sunrise-virtual-mode)
     (set-process-filter
      (setq as-process (apply 'start-process as-process-args))
      (sunrise-as-filter as-buffer as-filter))
     (set-process-sentinel as-process (sunrise-as-sentinel as-buffer as-command))
     (set-process-buffer as-process as-buffer)
     (use-local-map sunrise-process-map)
     (run-with-idle-timer 0.01 nil 'sunrise-as-prompt as-label))))

(defun sunrise-async-grep (as-input)
  "Launch a grep asynchronous search operation. If any entries
have been explicitly selected in the current pane ask the user
whether to run the grep only for these entries, otherwise run it
in the current directory. If called with prefix ask for
additional grep options."
  (interactive "sFind files containing: ")
  (let* ((default-grep-options "-rl")
         (opts (if current-prefix-arg
                   (concat default-grep-options " "
                           (read-string "Additional Grep Options: "))
                 default-grep-options))
         (options (split-string opts " " t))
         (marked (sunrise-get-marked-files))
         (target
          (if (and marked (y-or-n-p "Grep in marked items only? "))
              marked
            '("."))))
    (cl-labels ((fl (&rest args) (sunrise-flatlist args)))
      (apply 'sunrise-as-search
             (fl "Grep" sunrise-grep-command nil options as-input target)))))

(defun sunrise-grep ()
  "Run grep asynchronously and display the results in Sunrise virtual mode."
  (interactive)
  (if sunrise-recursive-grep-supported
      (call-interactively 'sunrise-async-grep)
    (call-interactively 'sunrise-find-grep)))

(defvar locate-command)
(autoload 'locate-prompt-for-search-string "locate")
(defun sunrise-locate (as-input &optional _filter _arg)
  "Run locate asynchronously and display the results in Sunrise virtual mode."
  (interactive
   (list (locate-prompt-for-search-string) nil current-prefix-arg))
  (sunrise-as-search "Locate" "locate" #'file-exists-p as-input))

(defun sunrise-multi-occur (string)
  "Execute `multi-occur' on all marked files. Note this command needs to visit
first all the selected files."
  (interactive "sSearch in selected files for occurrences of: ")
  (let ((regular-files (delq nil (mapcar (lambda (x)
                                           (and (file-regular-p x) x))
                                         (dired-get-marked-files)))))
    (if (not regular-files)
        (error "Sunrise: no regular files to search")
      (sunrise-quit)
      (multi-occur (mapcar 'find-file regular-files) string)
      (other-window 1))))

(defun sunrise-flatten-branch (&optional mode)
  "Display a flat view of the items contained in the current directory and all
its subdirectories, sub-subdirectories and so on (recursively) in the active
pane."
  (interactive "cFlatten branch showing: (E)verything, (D)irectories,\
 (N)on-directories or (F)iles only?")
  (if (and mode (>= mode 97)) (setq mode (- mode 32)))
  (case mode
    (?E (sunrise-find-name "*"))
    (?D (sunrise-find "-type d"))
    (?N (sunrise-find "-not -type d"))
    (?F (sunrise-find "-type f"))))

(defun sunrise-prune-paths (regexp)
  "Kill all lines (only the lines) in the current pane matching REGEXP."
  (interactive "sPrune paths matching: ")
  (save-excursion
    (sunrise-beginning-of-buffer)
    (while (if (string-match regexp (dired-get-filename t))
               (dired-kill-line)
             (dired-next-line 1)))))

(defun sunrise-fuzzy-narrow ()
  "Interactively narrow contents of the current pane using fuzzy matching:
  * press Delete or Backspace to revert the buffer to its previous state
  * press Return, C-n or C-p to exit and accept the current narrowed state
  * press Esc or C-g to abort the operation and revert the buffer
  * use ! to prefix characters that should NOT appear beyond a given position.
  Once narrowed and accepted, you can restore the original contents of the pane
  by pressing g (`revert-buffer')."
  (interactive)
  (when sunrise-running
    (sunrise-beginning-of-buffer)
    (dired-change-marks ?* ?\t)
    (let ((stack nil) (filter "") (regex "") (next-char nil) (inhibit-quit t))
      (cl-labels ((read-next (f) (read-char (concat "Fuzzy narrow: " f))))
        (setq next-char (read-next filter))
        (sunrise-backup-buffer)
        (while next-char
          (case next-char
            ((?\e ?\C-g) (setq next-char nil) (sunrise-revert-buffer))
            (?\C-n (setq next-char nil) (sunrise-beginning-of-buffer))
            (?\C-p (setq next-char nil) (sunrise-end-of-buffer))
            ((?\n ?\r) (setq next-char nil))
            ((?\b ?\d)
             (revert-buffer)
             (setq stack (cdr stack) filter (caar stack) regex (cdar stack))
             (unless stack (setq next-char nil)))
            (t
             (setq filter (concat filter (char-to-string next-char)))
             (if (not (eq next-char sunrise-fuzzy-negation-character))
                 (setq next-char (char-to-string next-char)
                       regex (if (string= "" regex) ".*" regex)
                       regex (concat regex (regexp-quote next-char) ".*"))
               (setq next-char (char-to-string (read-next filter))
                     filter (concat filter next-char)
                     regex (replace-regexp-in-string "\\.\\*\\'" "" regex)
                     regex (concat regex "[^"(regexp-quote next-char)"]*")
                     regex (replace-regexp-in-string "\\]\\*\\[\\^" "" regex)))
             (setq stack (cons (cons filter regex) stack))))
          (when next-char
            (dired-mark-files-regexp (concat "^" regex "$"))
            (dired-toggle-marks)
            (dired-do-kill-lines)
            (setq next-char (read-next filter)))))
      (dired-change-marks ?\t ?*))))

(defun sunrise-recent-files ()
  "Display the history of recent files in Sunrise virtual mode."
  (interactive)
  (if (not (featurep 'recentf))
      (error "ERROR: Feature recentf not available!"))

  (sunrise-save-aspect
   (let ((dired-actual-switches dired-listing-switches))
     (sunrise-switch-to-clean-buffer "*Recent Files*")
     (insert "Recently Visited Files: \n")
     (dolist (file recentf-list)
       (condition-case nil
           (insert-directory file sunrise-virtual-listing-switches nil nil)
         (error (ignore))))
     (sunrise-virtual-mode)
     (sunrise-keep-buffer))))

(defun sunrise-recent-directories ()
  "Display the history of directories recently visited in the current pane."
  (interactive)
  (sunrise-save-aspect
   (let ((hist (cdr (assoc sunrise-selected-window sunrise-history-registry)))
         (dired-actual-switches dired-listing-switches)
         (pane-name (capitalize (symbol-name sunrise-selected-window)))
         (switches))
     (sunrise-switch-to-clean-buffer (format "*%s Pane History*" pane-name))
     (insert (concat "Recent Directories in " pane-name " Pane: \n"))
     (dolist (dir hist)
       (condition-case nil
           (case (sunrise-history-entry-type dir)
             (tramp
              (insert (concat "d......... 0 0000-00-00 " dir))
              (newline))
             (local
              (setq switches (concat sunrise-virtual-listing-switches " -d")
                    dir (sunrise-chop ?/ (expand-file-name dir)))
              (insert-directory dir switches nil nil))
             (t (ignore)))
         (error (ignore))))
     (sunrise-virtual-mode))))

(defun sunrise-switch-to-clean-buffer (name)
  (sunrise-alternate-buffer (switch-to-buffer name))
  (erase-buffer))

(defun sunrise-pure-virtual (&optional passive)
  "Create a new empty buffer in Sunrise VIRTUAL mode.
If the optional argument PASSIVE is non-nil, creates the virtual
buffer in the passive pane."
  (interactive "P")
  (if passive
      (progn
        (sunrise-synchronize-panes)
        (sunrise-in-other (sunrise-pure-virtual nil)))
    (sunrise-save-aspect
     (let* ((dir (directory-file-name (dired-current-directory)))
            (buff (generate-new-buffer-name (buffer-name (current-buffer)))))
       (sunrise-alternate-buffer (switch-to-buffer buff))
       (goto-char (point-min))
       (insert "  " dir ":")(newline)
       (insert " Pure VIRTUAL buffer: ")(newline)
       (sunrise-virtual-mode)
       (sunrise-keep-buffer)))))

(defun sunrise-dired-do-apply (dired-fun)
  "Helper function for implementing `sunrise-do-query-replace-regexp' and Co."
  (let ((buff (current-buffer)) (orig sunrise-restore-buffer))
    (condition-case nil
        (progn
          (sunrise-quit)
          (switch-to-buffer buff)
          (call-interactively dired-fun)
          (replace-buffer-in-windows buff)
          (sunrise-bury-panes))
      (quit
       (when orig (switch-to-buffer orig))
       (sunrise)))))

(defun sunrise-do-query-replace-regexp ()
  "Force Sunrise to quit before executing `dired-do-query-replace-regexp'."
  (interactive)
  (sunrise-dired-do-apply 'dired-do-query-replace-regexp))

(defun sunrise-do-search ()
  "Force Sunrise to quit before executing `dired-do-search'."
  (interactive)
  (sunrise-dired-do-apply 'dired-do-search))

(defun sunrise-sticky-isearch-prompt ()
  "Display the message that appears when a sticky search is launched."
  (message (propertize "Sunrise sticky I-search (C-g to exit): "
                       'face 'minibuffer-prompt)))

(defvar sunrise-sticky-isearch-commands
  '(nil
    ("\C-o" . dired-omit-mode)
    ("\M-a" . sunrise-beginning-of-buffer)
    ("\M-e" . sunrise-end-of-buffer)
    ("\C-v" . scroll-up-command)
    ("\M-v" . (lambda () (interactive) (scroll-up-command '-)))
    ("\C-g" . (lambda () (interactive) (save-excursion (isearch-abort))))
  ) "Keybindings installed in `isearch-mode' during a sticky search.")

(defun sunrise-sticky-isearch-remap-commands (&optional restore)
  "Remap `isearch-mode-map' commands using `sunrise-sticky-isearch-commands'.
Replace the bindings in our table with the previous ones from `isearch-mode-map'
so we can restore them when the current sticky search operation finishes."
  (when (eq restore (car sunrise-sticky-isearch-commands))
    (setcar sunrise-sticky-isearch-commands (not restore))
    (mapc (lambda (entry)
            (let* ((binding (car entry))
                   (old-command (lookup-key isearch-mode-map binding))
                   (new-command (cdr entry)))
              (define-key isearch-mode-map binding new-command)
              (setcdr entry old-command)))
          (cdr sunrise-sticky-isearch-commands))))

(defun sunrise-sticky-isearch (&optional backward)
  "Concatenate Isearch operations to allow fast file system navigation.
Search continues until C-g is pressed (to abort) or Return is
pressed on a regular file (to end the operation and visit that
file)."
  (set (make-local-variable 'search-nonincremental-instead) nil)
  (add-hook 'isearch-mode-end-hook 'sunrise-sticky-post-isearch)
  (sunrise-sticky-isearch-remap-commands)
  (if backward
      (isearch-backward nil t)
    (isearch-forward nil t))
  (run-hooks 'sunrise-refresh-hook)
  (run-with-idle-timer 0.01 nil 'sunrise-sticky-isearch-prompt))

(defun sunrise-sticky-isearch-forward ()
  "Start a sticky forward search in the current pane."
  (interactive)
  (sunrise-sticky-isearch))

(defun sunrise-sticky-isearch-backward ()
  "Start a sticky backward search in the current pane."
  (interactive)
  (sunrise-sticky-isearch t))

(defun sunrise-sticky-post-isearch ()
  "`isearch-mode-end-hook' function for Sunrise sticky Isearch operations."
  (and
   (dired-get-filename nil t)
   (let* ((filename (expand-file-name (dired-get-filename nil t)))
          (is-dir (or (file-directory-p filename)
                      (sunrise-avfs-dir filename)
                      (sunrise-virtual-directory-p filename))))
     (cond ((or isearch-mode-end-hook-quit (not is-dir))
            (progn
              (remove-hook 'isearch-mode-end-hook 'sunrise-sticky-post-isearch)
              (kill-local-variable 'search-nonincremental-instead)
              (sunrise-sticky-isearch-remap-commands t)
              (isearch-done)
              (if isearch-mode-end-hook-quit
                  (run-hooks 'sunrise-refresh-hook)
                (sunrise-find-file filename))))
           (t
            (progn
              (sunrise-find-file filename)
              (set (make-local-variable 'search-nonincremental-instead) nil)
              (isearch-forward nil t)
              (run-with-idle-timer 0.01 nil 'sunrise-sticky-isearch-prompt)))))))

(defun sunrise-show-files-info (&optional deref-symlinks)
  "Enhanced version of `dired-show-file-type' from dired‐aux.
If at most one item is marked, print the filetype of the current
item according to the \"file\" command, including its size in bytes.
If more than one item is marked, print the total size in
bytes (calculated recursively) of all marked items."
  (interactive "P")
  (message "Calculating total size of selection... (C-g to abort)")
  (let* ((selection (dired-get-marked-files t))
         (size (sunrise-size-format (sunrise-files-size selection)))
         (items (length selection)) (label) (regex))
    (if (>= 1 items)
        (progn
          (setq selection (car selection)
                label (file-name-nondirectory selection)
                regex (concat "^.*" label "[:;]")
                label (concat label ":"))
          (dired-show-file-type selection deref-symlinks)
          (message
           "%s (%s bytes)"
           (replace-regexp-in-string regex label (current-message)) size))
      (message "%s bytes in %d selected items" size items))
    (sit-for 0.5)))

(eval-when-compile
  (defsubst sunrise-size-attr (file)
    "Helper function for `sunrise-files-size'."
    (float (or (nth 7 (file-attributes file)) 0))))

(defun sunrise-files-size (files)
  "Recursively calculate the total size of all FILES.
FILES should be a list of paths."
  (let ((result 0))
    (mapc
     (lambda (x) (setq result (+ x result)))
     (mapcar (lambda (f) (cond ((string-match "\\.\\./?$" f) 0)
                               ((string-match "\\./?$" f) (sunrise-size-attr f))
                               ((file-symlink-p f) (sunrise-size-attr f))
                               ((file-directory-p f) (sunrise-directory-size f))
                               (t (float (sunrise-size-attr f)))))
             files))
    result))

(defun sunrise-directory-size (directory)
  "Recursively calculate the total size of the given DIRECTORY."
  (sunrise-files-size (directory-files directory t nil t)))

(defun sunrise-size-format (size)
  "Return integer representation of SIZE (a float) as a string.
Uses comma as the thousands separator."
  (let* ((num (replace-regexp-in-string "\\..*$" "" (number-to-string size)))
         (digits (reverse (split-string num "" t)))
         result)
    (dotimes (n (length digits))
      (when (and (< 0 n) (zerop (% n 3)))
        (setq result (concat "," result)))
      (setq result (concat (pop digits) result)))
    result))

;;; ============================================================================
;;; TI (Terminal Integration) and CLEX (Command Line EXpansion) functions:

;;;###autoload
(defun sunrise-term (&optional cd newterm program)
  "Run terminal in a new buffer or switch to an existing one.
If the optional argument CD is non-nil, directory is changed to
the current one in the active pane. A non-nil NEWTERM argument
forces the creation of a new terminal. If PROGRAM is provided
and exists in `exec-path', then it will be used instead of the
default `sunrise-terminal-program'."
  (interactive)
  (let ((aterm (car sunrise-ti-openterms)))
    (if (and (null program)
             (or (eq major-mode 'eshell-mode)
                 (and (buffer-live-p aterm)
                      (with-current-buffer aterm
                        (eq major-mode 'eshell-mode)))))
        (setq program "eshell")
      (setq program (or program sunrise-terminal-program))))
  (if (memq major-mode '(sunrise-mode sunrise-virtual-mode sunrise-tree-mode))
      (hl-line-mode 1))
  (if (string= program "eshell")
      (sunrise-term-eshell cd newterm)
    (sunrise-term-extern cd newterm program)))

;;;###autoload
(defun sunrise-term-cd ()
  "Run terminal in a new buffer or switch to an existing one.
cd's to the current directory of the active pane."
  (interactive)
  (sunrise-term t))

;;;###autoload
(defun sunrise-term-cd-newterm ()
  "Open a NEW terminal (don't switch to an existing one).
cd's to the current directory of the active pane."
  (interactive)
  (sunrise-term t t))

;;;###autoload
(defun sunrise-term-cd-program (&optional program)
  "Open a NEW terminal using PROGRAM as the shell."
  (interactive "sShell program to use: ")
  (sunrise-term t t program))

(defmacro sunrise-term-excursion (cd newterm form &optional is-external)
  "Take care of the common mechanics of launching or switching to a terminal.
Helper macro."
  `(let* ((start-buffer (current-buffer))
          (new-term (or (null sunrise-ti-openterms) ,newterm))
          (next-buffer (or (cadr (memq start-buffer sunrise-ti-openterms))
                           (car sunrise-ti-openterms)))
          (new-name) (is-line-mode))
     (sunrise-select-viewer-window t)
     (if (not new-term)
         ;;don't switch anywhere else if we're in a term and we want only to cd:
         (unless (and ,cd (memq (current-buffer) sunrise-ti-openterms))
           (switch-to-buffer next-buffer))
       (when next-buffer
         (with-current-buffer next-buffer
           (setq is-line-mode (and (boundp 'sunrise-term-line-minor-mode)
                                   (symbol-value 'sunrise-term-line-minor-mode)))))
       ,form
       (if ,is-external (sunrise-term-char-mode))
       (if is-line-mode (sunrise-term-line-mode))
       (when (memq (current-buffer) sunrise-ti-openterms)
         (rename-uniquely)
         (setq new-name (buffer-name))
         ,form)
       (when new-name
         (message "Sunrise: previous terminal renamed to %s" new-name))
       (push (current-buffer) sunrise-ti-openterms))))

(defun sunrise-term-line-mode ()
  "Switch the current terminal to line mode.
Apply additional Sunrise keybindings for terminal integration."
  (interactive)
  (term-line-mode)
  (sunrise-term-line-minor-mode 1))

(defun sunrise-term-char-mode ()
  "Switch the current terminal to character mode.
Bind C-j and C-k to Sunrise terminal integration commands."
  (interactive)
  (term-char-mode)
  (sunrise-term-line-minor-mode 0)
  (sunrise-term-char-minor-mode 1))

(defun sunrise-term-extern (&optional cd newterm program)
  "Implementation of `sunrise-term' for external terminal programs.
See `sunrise-term' for a description of the arguments."
  (let* ((program (if program (executable-find program)))
         (program (or program sunrise-terminal-program))
         (dir (expand-file-name (sunrise-choose-cd-target)))
        (aterm (car sunrise-ti-openterms))
        (cd (or cd (null sunrise-ti-openterms)))
        (line-mode (if (buffer-live-p aterm)
                       (with-current-buffer aterm (term-in-line-mode)))))
    (sunrise-term-excursion cd newterm (term program) t)
    (sunrise-term-char-mode)
    (when (or line-mode (term-in-line-mode))
      (sunrise-term-line-mode))
    (when cd
      (term-send-raw-string
       (concat "cd " (shell-quote-wildcard-pattern dir) "
")))))

(defun sunrise-term-eshell (&optional cd newterm)
  "Implementation of `sunrise-term' when using `eshell'."
  (let ((dir (expand-file-name (sunrise-choose-cd-target)))
        (cd (or cd (null sunrise-ti-openterms))))
    (sunrise-term-excursion cd newterm (eshell))
    (when cd
      (insert (concat "cd " (shell-quote-wildcard-pattern dir)))
      (eshell-send-input))
    (sunrise-term-line-mode)))

(defmacro sunrise-ti (form)
  "Evaluate FORM in the context of the selected pane.
Helper macro for implementing terminal integration in Sunrise."
  `(when sunrise-running
     (sunrise-select-window sunrise-selected-window)
     (hl-line-unhighlight)
     (unwind-protect
         ,form
       (when sunrise-running
         (sunrise-select-viewer-window)))))

(defun sunrise-ti-previous-line ()
  "Move one line backward on active pane from the terminal window."
  (interactive)
  (sunrise-ti (forward-line -1)))

(defun sunrise-ti-next-line ()
  "Move one line forward on active pane from the terminal window."
  (interactive)
  (sunrise-ti (forward-line 1)))

(defun sunrise-ti-select ()
  "Run `dired-advertised-find-file' on active pane from the terminal window."
  (interactive)
  (sunrise-ti (sunrise-advertised-find-file)))

(defun sunrise-ti-mark ()
  "Run `dired-mark' on active pane from the terminal window."
  (interactive)
  (sunrise-ti (dired-mark 1)))

(defun sunrise-ti-unmark ()
  "Run `dired-unmark-backward' on active pane from the terminal window."
  (interactive)
  (sunrise-ti (dired-unmark-backward 1)))

(defun sunrise-ti-prev-subdir (&optional count)
  "Run `dired-prev-subdir' on active pane from the terminal window."
  (interactive "P")
  (let ((count (or count 1)))
    (sunrise-ti (sunrise-dired-prev-subdir count))))

(defun sunrise-ti-unmark-all-marks ()
  "Remove all marks on active pane from the terminal window."
  (interactive)
  (sunrise-ti (dired-unmark-all-marks)))

(defun sunrise-ti-change-window ()
  "Switch focus to the currently active pane."
  (interactive)
  (sunrise-select-window sunrise-selected-window))

(defun sunrise-ti-change-pane ()
  "Change selection of active pane to passive one."
  (interactive)
  (sunrise-ti (sunrise-change-window)))

(add-hook
 'kill-buffer-hook
 (defun sunrise-ti-cleanup-openterms ()
   "Remove the current buffer from the list of open terminals."
   (setq sunrise-ti-openterms (delete (current-buffer) sunrise-ti-openterms))))

(defun sunrise-ti-revert-buffer ()
  "Refresh the currently active pane."
  (interactive)
  (let ((dir default-directory))
    (if (not (sunrise-equal-dirs dir sunrise-this-directory))
        (sunrise-ti (sunrise-goto-dir dir))
      (sunrise-ti (sunrise-revert-buffer)))))

(defun sunrise-ti-lock-panes ()
  "Resize and lock the panes at standard position from the command line."
  (interactive)
  (sunrise-ti (sunrise-lock-panes)))

(defun sunrise-ti-min-lock-panes ()
  "Minimize the panes from the command line."
  (interactive)
  (sunrise-ti (sunrise-min-lock-panes)))

(defun sunrise-ti-max-lock-panes ()
  "Maximize the panes from the command line."
  (interactive)
  (sunrise-ti (sunrise-max-lock-panes)))

(defmacro sunrise-clex (pane form)
  "Evaluate FORM in the context of PANE.
Helper macro for implementing command line expansion in Sunrise."
  `(progn
     (setq pane (if (atom pane) pane (eval pane)))
     (with-current-buffer (symbol-value (sunrise-symbol ,pane 'buffer))
       ,form)))

(defun sunrise-clex-marked (pane)
  "Return a string containing the list of marked files in PANE."
  (sunrise-clex
   pane
   (mapconcat 'shell-quote-wildcard-pattern (dired-get-marked-files) " ")))

(defun sunrise-clex-file (pane)
  "Return the file currently selected in PANE."
  (sunrise-clex
   pane
   (concat (shell-quote-wildcard-pattern (dired-get-filename)) " ")))

(defun sunrise-clex-marked-nodir (pane)
  "Return a list of basenames of all the files currently marked in PANE."
  (sunrise-clex
   pane
   (mapconcat 'shell-quote-wildcard-pattern
              (dired-get-marked-files 'no-dir) " ")))

(defun sunrise-clex-dir (pane)
  "Return the current directory of the given pane."
  (sunrise-clex
   pane
   (concat (shell-quote-wildcard-pattern default-directory) " ")))

(defun sunrise-clex-start ()
  "Start a new CLEX operation.
Puts `sunrise-clex-commit' into local `after-change-functions'."
  (interactive)
  (if sunrise-clex-on
      (progn
        (setq sunrise-clex-on nil)
        (delete-overlay sunrise-clex-hotchar-overlay))
    (insert-char ?% 1)
    (when sunrise-running
      (add-hook 'after-change-functions 'sunrise-clex-commit nil t)
      (setq sunrise-clex-on t)
      (setq sunrise-clex-hotchar-overlay (make-overlay (point) (1- (point))))
      (overlay-put sunrise-clex-hotchar-overlay 'face 'sunrise-clex-hotchar-face)
      (message
       "Sunrise: CLEX is now ON for keys: m f n d a p M F N D A P %%"))))

(defun sunrise-clex-commit (&optional _beg _end _range)
  "Commit the current CLEX operation (if any).
This function is added to the local `after-change-functions' list
by `sunrise-clex-start'."
  (interactive)
  (when sunrise-clex-on
    (setq sunrise-clex-on nil)
    (delete-overlay sunrise-clex-hotchar-overlay)
    (let* ((xchar (char-before))
           (expansion (case xchar
                        (?m (sunrise-clex-marked       'left))
                        (?f (sunrise-clex-file         'left))
                        (?n (sunrise-clex-marked-nodir 'left))
                        (?d (sunrise-clex-dir          'left))
                        (?M (sunrise-clex-marked       'right))
                        (?F (sunrise-clex-file         'right))
                        (?N (sunrise-clex-marked-nodir 'right))
                        (?D (sunrise-clex-dir          'right))
                        (?a (sunrise-clex-marked       '(sunrise-this)))
                        (?A (sunrise-clex-dir          '(sunrise-this)))
                        (?p (sunrise-clex-marked       '(sunrise-other)))
                        (?P (sunrise-clex-dir          '(sunrise-other)))
                        (t nil))))
      (when expansion
        (delete-char -2)
        (insert expansion)))))

(define-minor-mode sunrise-term-char-minor-mode
  "Sunrise Commander terminal add-on for character (raw) mode."
  nil nil
  '(("\C-c\C-j" . sunrise-term-line-mode)
    ("\C-c\C-k" . sunrise-term-char-mode)
    ("\C-c\t"   . sunrise-ti-change-window)
    ("\C-ct"    . sunrise-term)
    ("\C-cT"    . sunrise-term-cd)
    ("\C-c\C-t" . sunrise-term-cd-newterm)
    ("\C-c\M-t" . sunrise-term-cd-program)
    ("\C-c;"    . sunrise-follow-viewer)
    ("\C-c\\"   . sunrise-ti-lock-panes)
    ("\C-c{"    . sunrise-ti-min-lock-panes)
    ("\C-c}"    . sunrise-ti-max-lock-panes)))

(define-minor-mode sunrise-term-line-minor-mode
  "Sunrise Commander terminal add-on for line (cooked) mode."
  nil nil
  '(([M-up]        . sunrise-ti-previous-line)
    ([A-up]        . sunrise-ti-previous-line)
    ("\M-P"        . sunrise-ti-previous-line)
    ([M-down]      . sunrise-ti-next-line)
    ([A-down]      . sunrise-ti-next-line)
    ("\M-N"        . sunrise-ti-next-line)
    ("\M-\C-m"     . sunrise-ti-select)
    ("\C-\M-j"     . sunrise-ti-select)
    ([M-return]    . sunrise-ti-select)
    ([S-M-return]  . sunrise-ti-select)
    ("\M-M"        . sunrise-ti-mark)
    ([M-backspace] . sunrise-ti-unmark)
    ("\M-\d"       . sunrise-ti-unmark)
    ("\M-J"        . sunrise-ti-prev-subdir)
    ("\M-U"        . sunrise-ti-unmark-all-marks)
    ([C-tab]       . sunrise-ti-change-window)
    ([M-tab]       . sunrise-ti-change-pane)
    ("\C-c\t"      . sunrise-ti-change-window)
    ("\C-ct"       . sunrise-term)
    ("\C-cT"       . sunrise-term-cd)
    ("\C-c\C-t"    . sunrise-term-cd-newterm)
    ("\C-c\M-t"    . sunrise-term-cd-program)
    ("\C-c;"       . sunrise-follow-viewer)
    ("\M-\S-g"     . sunrise-ti-revert-buffer)
    ("%"           . sunrise-clex-start)
    ("\t"          . term-dynamic-complete)
    ("\C-c\\"      . sunrise-ti-lock-panes)
    ("\C-c{"       . sunrise-ti-min-lock-panes)
    ("\C-c}"       . sunrise-ti-max-lock-panes))
  :group 'sunrise)

(defadvice term-sentinel (around sunrise-advice-term-sentinel (proc msg) activate)
  "Take care of killing Sunrise Commander terminal buffers on exit."
  (if (and (or sunrise-term-char-minor-mode sunrise-term-line-minor-mode)
           sunrise-terminal-kill-buffer-on-exit
           (memq (process-status proc) '(signal exit)))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (bury-buffer buffer)
        (kill-buffer buffer))
    ad-do-it))

;;; ============================================================================
;;; Desktop support:

(defun sunrise-pure-virtual-p (&optional buffer)
  "Return t if BUFFER (or the current buffer if nil) is purely virtual.
Purely virtual means it is not attached to any directory or any
file in the file system."
  (with-current-buffer (if (bufferp buffer) buffer (current-buffer))
    (not (or (eq 'sunrise-mode major-mode)
             (and (eq 'sunrise-virtual-mode major-mode)
                  buffer-file-truename
                  (file-exists-p buffer-file-truename))))))

(defun sunrise-desktop-save-buffer (desktop-dir)
  "Return the additional data for saving a Sunrise buffer to a desktop file."
  (unless (sunrise-pure-virtual-p)
    (let* ((side (if (eq (current-buffer) sunrise-left-buffer) 'left 'right))
           (sorting-order (or (get side 'sorting-order) "NAME"))
           (sorting-reverse (get side 'sorting-reverse)))
      (apply
       'append
       (delq nil
             (list
              (if (eq major-mode 'sunrise-virtual-mode)
                  (list 'dirs buffer-file-truename)
                (cons 'dirs (dired-desktop-buffer-misc-data desktop-dir)))
              (cons side t)
              (if sorting-order (cons 'sorting-order sorting-order))
              (if sorting-reverse (cons 'sorting-reverse sorting-reverse))
              (if (eq major-mode 'sunrise-virtual-mode) (cons 'virtual t))))
       (mapcar (lambda (fun)
                 (funcall fun desktop-dir))
               sunrise-desktop-save-handlers)))))

(defun sunrise-desktop-restore-buffer (desktop-buffer-file-name
                                  desktop-buffer-name
                                  desktop-buffer-misc)
  "Restore a Sunrise (normal or VIRTUAL) buffer from its desktop file data."
  (let* ((sunrise-running t)
         (misc-data (cdr (assoc 'dirs desktop-buffer-misc)))
         (is-virtual (assoc 'virtual desktop-buffer-misc))
         (buffer
          (if (not is-virtual)
              (with-current-buffer
                  (dired-restore-desktop-buffer desktop-buffer-file-name
                                                desktop-buffer-name
                                                misc-data)
                (sunrise-mode)
                (current-buffer))
            (desktop-restore-file-buffer (car misc-data)
                                         desktop-buffer-name
                                         misc-data))))
    (with-current-buffer buffer
      (when is-virtual (set-visited-file-name nil t))
      (mapc (lambda (side)
              (when (cdr (assq side desktop-buffer-misc))
                (set (sunrise-symbol side 'buffer) buffer)
                (set (sunrise-symbol side 'directory) default-directory)
                (sunrise-desktop-sort buffer side desktop-buffer-misc)))
            '(left right))
      (mapc (lambda (fun)
              (funcall fun
                       desktop-buffer-file-name
                       desktop-buffer-name
                       desktop-buffer-misc))
            sunrise-desktop-restore-handlers))
    buffer))

(defun sunrise-desktop-sort (buffer side desktop-buffer-misc)
  "Restore the sorting order in BUFFER to be displayed in SIDE.
Use the data in DESKTOP-BUFFER-MISC to obtain all pertinent
details."
  (with-current-buffer buffer
    (let ((sunrise-selected-window side)
          (sorting-order (cdr (assoc 'sorting-order desktop-buffer-misc)))
          (sorting-reverse (cdr (assoc 'sorting-reverse desktop-buffer-misc))))
      (when sorting-order
        (condition-case nil
            (funcall (intern (format "sunrise-sort-by-%s" (downcase sorting-order))))
          (error (ignore))))
      (when sorting-reverse (sunrise-reverse-pane)))))

(defun sunrise-reset-state ()
  "Reset some environment variables that control the Sunrise behavior.
Used for desktop support."
  (setq sunrise-left-directory "~/" sunrise-right-directory "~/"
        sunrise-this-directory "~/" sunrise-other-directory "~/")
  (if sunrise-running (sunrise-quit))
  nil)

;; This registers the previous functions in the desktop framework:
(add-to-list 'desktop-buffer-mode-handlers
             '(sunrise-mode . sunrise-desktop-restore-buffer))

;; This initializes (and sometimes starts) Sunrise after desktop restoration:
(add-hook 'desktop-after-read-hook
          (defun sunrise-desktop-after-read-function ()
            (unless (assoc 'sunrise-running desktop-globals-to-clear)
              (add-to-list 'desktop-globals-to-clear
                           '(sunrise-running . (sunrise-reset-state))))
            (when (and (buffer-live-p sunrise-left-buffer)
                       (get-buffer-window sunrise-left-buffer))
              (sunrise-setup-windows)
              (sunrise-highlight)
              (setq sunrise-current-frame (window-frame (selected-window))
                    sunrise-running t))))

;;; ============================================================================
;;; Miscellaneous functions:

(defun sunrise-buffer-files (buffer-or-name)
  "Return the list of all file names currently displayed in the given buffer."
  (with-current-buffer buffer-or-name
    (save-excursion
      (let ((result nil))
        (sunrise-beginning-of-buffer)
        (while (not (eobp))
          (setq result (cons (dired-get-filename t t) result))
          (forward-line 1))
        (reverse result)))))

(defun sunrise-keep-buffer (&optional side)
  "Keep the currently displayed buffer in SIDE (left or right) window.
Keeps it there even if it does not belong to the panel's history
ring. If SIDE is nil, use the value of `sunrise-selected-window'
instead. Useful for maintaining the contents of the pane during
layout switching."
  (let* ((side (or side sunrise-selected-window))
         (window (symbol-value (sunrise-symbol side 'window))))
    (set (sunrise-symbol side 'buffer) (window-buffer window))))

(defun sunrise-scrollable-viewer (buffer)
  "Set the `other-window-scroll-buffer' variable to BUFFER.
Doing so allows to scroll the given buffer directly from the active pane."
  (setq other-window-scroll-buffer buffer)
  (if buffer
      (message "QUICK VIEW: Press C-e/C-y to scroll, Space/M-Space to page, and C-u v (or C-u o) to dismiss")))

(defun sunrise-describe-mode ()
  "Call `describe-mode' and make the resulting buffer C-M-v scrollable."
  (interactive)
  (describe-mode)
  (sunrise-scrollable-viewer (get-buffer "*Help*"))
  (sunrise-select-window sunrise-selected-window))

(defun sunrise-equal-dirs (dir1 dir2)
  "Return non-nil if the two paths DIR1 and DIR2 represent the same directory."
  (string= (expand-file-name (concat (directory-file-name dir1) "/"))
           (expand-file-name (concat (directory-file-name dir2) "/"))))

(defun sunrise-summary ()
  "Summarize basic Sunrise commands and show recent Dired errors."
  (interactive)
  (dired-why)
  (message "C-opy, R-ename, K-lone, D-elete, v-iew, e-X-ecute, Ff-ollow, \
Jj-ump, q-uit, m-ark, u-nmark, h-elp"))

(defun sunrise-restore-point-if-same-buffer ()
  "Synchronize point position if the same buffer is displayed in both panes."
  (let ((this-win)(other-win)(point))
    (when (and (eq sunrise-left-buffer sunrise-right-buffer)
               (window-live-p (setq other-win (sunrise-other 'window))))
      (setq this-win (selected-window))
      (setq point (point))
      (select-window other-win)
      (goto-char point)
      (select-window this-win))))

(defun sunrise-mark-toggle ()
  "Toggle the mark on the current file or directory."
  (interactive)
  (when (dired-get-filename t t)
    (if (eq ?  (char-after (line-beginning-position)))
        (dired-mark 1)
      (dired-unmark 1))))

(defun sunrise-assoc-key (name alist test)
  "Return the key in ALIST matched by NAME according to TEST."
  (let (head (tail alist) found)
    (while (and tail (not found))
      (setq head (caar tail)
            found (and (apply test (list head name)) head)
            tail (cdr tail)))
    found))

(defun sunrise-get-marked-files ()
  "Return current pane's *explicitly* selected entries, or nil if
no entries have been explicitly selected."
  (let ((marked))
    (condition-case err
        (setq marked (dired-get-marked-files t nil nil t))
      (error (unless (string= "No file on this line" (cadr err))
               (signal (car err) (cdr err)))))
    (unless (< (length marked) 2)
      (if (eq t (car marked)) (setq marked (cdr marked)))
      marked)))

(defun sunrise-quote-marked ()
  "Return current pane's explicitly selected entries quoted and
space-separated as a string, or nil if no entries have been
explicitly selected."
  (let ((marked (sunrise-get-marked-files)))
    (when marked
      (format "\"%s\"" (mapconcat 'identity marked "\" \"")))))

(defun sunrise-fix-listing-switches()
  "Work around a bug in Dired that makes `dired-move-to-filename' misbehave
when any of the options -p or -F is used with ls."
  (mapc (lambda (sym)
          (let ((val (replace-regexp-in-string "\\(?:^\\| \\)-[pF]*\\(?: \\|$\\)" " " (symbol-value sym))))
            (while (string-match "\\(?:^\\| \\)-[^- ]*[pF]" val)
              (setq val (replace-regexp-in-string "\\(\\(?:^\\| \\)-[^- ]*\\)[pF]\\([^ ]*\\)" "\\1\\2" val)))
            (set sym val)))
        '(sunrise-listing-switches sunrise-virtual-listing-switches))
  (remove-hook 'sunrise-init-hook 'sunrise-fix-listing-switches))
(add-hook 'sunrise-init-hook 'sunrise-fix-listing-switches)

(defun sunrise-chop (char path)
  "Remove all trailing instances of character CHAR from the string PATH."
  (while (and (< 1 (length path))
              (eq (string-to-char (substring path -1)) char))
    (setq path (substring path 0 -1)))
  path)

(defun sunrise-flatlist (in &optional out rev)
  "Flatten the nesting in an arbitrary list of values."
  (cond
   ((and (null in) rev) out)
   ((null in) (nreverse out))
   (t
    (let ((head (car in)) (tail (cdr in)))
      (if (atom head)
          (sunrise-flatlist tail (cons head out) rev)
        (sunrise-flatlist tail (append (sunrise-flatlist head nil t) out) rev))))))

;;; ============================================================================
;;; Advice

(defun sunrise-ad-enable (regexp &optional function)
  "Put all or FUNCTION-specific advice matching REGEXP into effect.
If provided, only update FUNCTION itself, otherwise all functions
with advice matching REGEXP."
  (if function
      (progn (ad-enable-advice function 'any regexp)
             (ad-activate function))
    (ad-enable-regexp regexp)
    (ad-activate-regexp regexp)))

(defun sunrise-ad-disable (regexp &optional function)
  "Stop all FUNCTION-specific advice matching REGEXP from taking effect.
If provided, only update FUNCTION itself, otherwise all functions
with advice matching REGEXP."
  (if function
      (progn (ad-disable-advice function 'any regexp)
             (ad-update function))
    (ad-disable-regexp regexp)
    (ad-update-regexp regexp)))

(defun sunrise-unload-function ()
  (sunrise-ad-disable "^sunrise-advice-"))

;;; ============================================================================
;;; Font-Lock colors & styles:

(defmacro sunrise-rainbow (symbol spec regexp)
  `(progn
     (defface ,symbol '((t ,spec)) "Sunrise rainbow face" :group 'sunrise)
     ,@(mapcar (lambda (m)
                 `(font-lock-add-keywords ',m '((,regexp 1 ',symbol))))
               '(sunrise-mode sunrise-virtual-mode))))

(sunrise-rainbow sunrise-html-face              (:foreground "DarkOliveGreen")        "\\(^[^!].[^d].*\\.x?html?$\\)")
(sunrise-rainbow sunrise-xml-face               (:foreground "DarkGreen")             "\\(^[^!].[^d].*\\.\\(xml\\|xsd\\|xslt?\\|wsdl\\)$\\)")
(sunrise-rainbow sunrise-log-face               (:foreground "brown")                 "\\(^[^!].[^d].*\\.log$\\)")
(sunrise-rainbow sunrise-compressed-face        (:foreground "magenta")               "\\(^[^!].[^d].*\\.\\(zip\\|bz2\\|t?[gx]z\\|[zZ]\\|[jwers]?ar\\|xpi\\|apk\\|xz\\)$\\)")
(sunrise-rainbow sunrise-packaged-face          (:foreground "DarkMagenta")           "\\(^[^!].[^d].*\\.\\(deb\\|rpm\\)$\\)")
(sunrise-rainbow sunrise-encrypted-face         (:foreground "DarkOrange1")           "\\(^[^!].[^d].*\\.\\(gpg\\|pgp\\)$\\)")

(sunrise-rainbow sunrise-directory-face         (:inherit dired-directory :bold t)    "\\(^[^!].d.*\\)")
(sunrise-rainbow sunrise-symlink-face           (:inherit dired-symlink :italic t)    "\\(^[^!].l.*[^/]$\\)")
(sunrise-rainbow sunrise-symlink-directory-face (:inherit dired-directory :italic t)  "\\(^[^!].l.*/$\\)")
(sunrise-rainbow sunrise-alt-marked-dir-face    (:foreground "DeepPink" :bold t)      "\\(^[^ *!D].d.*$\\)")
(sunrise-rainbow sunrise-alt-marked-file-face   (:foreground "DeepPink")              "\\(^[^ *!D].[^d].*$\\)")
(sunrise-rainbow sunrise-marked-dir-face        (:inherit dired-marked)               "\\(^[*!D].d.*$\\)")
(sunrise-rainbow sunrise-marked-file-face       (:inherit dired-marked :bold nil)     "\\(^[*!D].[^d].*$\\)")
(sunrise-rainbow sunrise-broken-link-face       (:inherit dired-warning :italic t)    "\\(^[!].l.*$\\)")

(provide 'sunrise)

;;; sunrise.el ends here
