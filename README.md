# The Sunrise Commander

Welcome to the Git repository of the Sunrise Commander, the orthodox file
manager for GNU Emacs.

The author of the Sunrise Commander is José Alfredo Romero Latouche. Maintenance
is currently done by volunteers. The official repository is hosted at
[sunrise-commander/sunrise-commander](https://github.com/sunrise-commander/sunrise-commander)
on GitHub. If you wish to contribute, please fork the repository, commit your
work to the fork and send us a pull request.

Read on for a terse user's guide. The [Emacs Wiki
page](http://www.emacswiki.org/emacs/Sunrise_Commander) gives a more complete
tour and many tips from users.

Happy hacking!

## User's Guide

**The Sunrise Commander** is a double-pane file manager for Emacs. It's built atop of Dired and takes advantage of all its power, but also provides many handy features of its own:

 * Sunrise is implemented as a derived major mode confined inside the pane buffers, so its buffers and Dired ones can live together without easymenu or viper to avoid key binding collisions.
 * It automatically closes unused buffers and tries to never keep open more than the one or two used to display the panes, though this behavior may be disabled if desired.
 * Each pane has its own history stack: press <kbd>M-y</kbd> / <kbd>M-u</kbd> for moving backwards / forwards in the history of directories.
 * Press <kbd>M-t</kbd> to swap (transpose) the panes.
 * Press <kbd>C-=</kbd> for "smart" file comparison using `ediff`. It compares together the first two files marked on each pane or, if no files have been marked, it assumes that the second pane contains a file with the same name as the selected one and tries to compare these two. You can also mark whole lists of files to be compared and then just press C-= for comparing the next pair.
 * Press <kbd>=</kbd> for fast "smart" file comparison -- like above, but using regular diff.
 * Press <kbd>C-M-=</kbd> for directory comparison (by date/size/contents of files).
 * Press <kbd>C-c C-s</kbd> to change the layout of the panes (horizontal/vertical/top)
 * Press <kbd>C-c /</kbd> to interactively refine the contents of the current pane using fuzzy (a.k.a. flex) matching, then:
   * press <kbd>Delete</kbd> or <kbd>Backspace</kbd> to revert the buffer to its previous state
   * press <kbd>Return</kbd>, <kbd>C-n</kbd> or <kbd>C-p</kbd> to exit and accept the current narrowed state
   * press <kbd>Esc</kbd> or <kbd>C-g</kbd> to abort the operation and revert the buffer
   * use <kbd>!</kbd> to prefix characters that should NOT appear after a given position
   * Once narrowed and accepted, you can restore the original contents of the pane by pressing <kbd>g</kbd> (`revert-buffer`).
 * Sticky search: press <kbd>C-c s</kbd> to launch an interactive search that will remain active from directory to directory, until you hit a regular file or press <kbd>C-g</kbd>
 * Press <kbd>C-x C-q</kbd> to put the current pane in Editable Dired mode (allows to edit the pane as if it were a regular file -- press <kbd>C-c C-c</kbd> to commit your changes to the filesystem, or <kbd>C-c C-k</kbd> to abort).
 * Press <kbd>y</kbd> to recursively calculate the total size (in bytes) of all files and directories currently selected/marked in the active pane.
 * Sunrise VIRTUAL mode integrates dired-virtual mode to Sunrise, allowing to capture grep, find and locate results in regular files and to use them later as if they were directories with all the Dired and Sunrise operations at your fingertips.
 * The results of the following operations are displayed in VIRTUAL mode:
    * `find-name-dired` (press <kbd>C-c C-n</kbd>),
    * `find-dired`      (press <kbd>C-c C-f</kbd>),
    * `grep`            (press <kbd>C-c C-g</kbd>),
    * `locate`          (press <kbd>C-c C-l</kbd>),
    * list all recently visited files (press <kbd>C-c C-r</kbd> -- requires `recentf`),
    * list all directories in active pane's history ring (press <kbd>C-c C-d</kbd>).
 * Supports [AVFS](http://avf.sourceforge.net/) for transparent navigation inside compressed archives (`*.zip`, `*.tgz`, `*.tar.bz2`, `*.deb`, etc. etc.) You need to have AVFS with coda or fuse installed and running on your system for this to work, though.
 * Opening terminals directly from Sunrise:
    * Press <kbd>C-c C-t</kbd> to inconditionally open a new terminal into the currently selected directory in the active pane.
    * Use <kbd>C-c t</kbd> to switch to the last opened terminal, or (when already inside a terminal) to cycle through all open terminals.
    * Press <kbd>C-c T</kbd> to switch to the last opened terminal and change directory to the one in the current directory.
    * Press <kbd>C-c M-t</kbd> to be prompted for a program name, and then open a new terminal using that program into the currently selected directory (`eshell` is a valid value; if no program can be found with the given name then the value of `sunrise-terminal-program` is used instead).
 * Terminal integration and Command line expansion: integrates tightly with `eshell` and `term-mode` to allow interaction between terminal emulators in line mode (<kbd>C-c C-j</kbd>) and the panes: the most important navigation commands (up, down, mark, unmark, go to parent dir) can be executed on the active pane directly from the terminal by pressing the usual keys with Meta: <kbd>M-up</kbd>, <kbd>M-down</kbd>, etc. Additionally, the following substitutions are automagically performed in `eshell` and `term-line-mode`:
    * `%f` - expands to the currently selected file in the left pane
    * `%F` - expands to the currently selected file in the right pane
    * `%m` - expands to the list of paths of all marked files in the left pane
    * `%M` - expands to the list of paths of all marked files in the right pane
    * `%n` - expands to the list of names of all marked files in the left pane
    * `%N` - expands to the list of names of all marked files in the right pane
    * `%d` - expands to the current directory in the left pane
    * `%D` - expands to the current directory in the right pane
    * `%a` - expands to the list of paths of all marked files in the active pane
    * `%A` - expands to the current directory in the active pane
    * `%p` - expands to the list of paths of all marked files in the passive pane
    * `%P` - expands to the current directory in the passive pane
 * Cloning of complete directory trees: press <kbd>K</kbd> to clone the selected files and directories into the passive pane. Cloning is a more general operation than copying, in which all directories are recursively created with the same names and structures at the destination, while what happens to the files within them depends on the option you choose:
    * `"(F)ile System of..."` clones the FS structure of paths in a VIRTUAL pane,
    * `"(D)irectories only"` ignores all files, copies only directories,
    * `"(C)opies"` performs a regular recursive copy of all files and dirs,
    * `"(H)ardlinks"` makes every new file a (hard) link to the original one
    * `"(S)ymlinks"` creates absolute symbolic links for all files in the tree,
    * `"(R)elative symlinks”` creates relative symbolic links.
 * Passive navigation: the usual navigation keys (n, p, Return, U, ;) combined with Meta allow to move across the passive pane without actually having to switch to it.
 * Synchronized navigation: press <kbd>C-c C-z</kbd> to enable/disable synchronized navigation. In this mode, the passive navigation keys (<kbd>M-n</kbd>, <kbd>M-p</kbd>, <kbd>M-RET</kbd>, etc.) operate on both panes simultaneously. I've found this quite useful for comparing hierarchically small to medium-sized directory trees (for large to very large directory trees one needs something on the lines of `diff -r` though).
 * And much more -- press <kbd>?</kbd> while in Sunrise mode for basic help, or <kbd>h</kbd> for a complete list of all keybindings available (use <kbd>C-e</kbd> and <kbd>C-y</kbd> to scroll).

There is no help window like in MC, but if you really miss it, just get and install the sunrise-buttons extension.

A lot of this code was once adapted from Kevin Burton's mc.el, but it has evolved considerably since then. Another part (the code for file copying and renaming) derives originally from the Dired extensions written by Kurt Nørmark for [LAML](http://www.cs.aau.dk/~normark/scheme/distribution/laml/).

It's written on GNU Emacs 25 on Linux and tested on GNU Emacs 22, 23, 24 and 25 for Linux and on EmacsW32 (version 23) for Windows. I have also received feedback from users reporting it works OK on the Mac. It does not work either on GNU Emacs 21 or XEmacs -- please drop me a line if you would like to help porting it. All contributions and/or bug reports will be very welcome.

For more details on the file manager, several available extensions and many cool tips & tricks visit http://www.emacswiki.org/emacs/Sunrise_Commander

## Installation and Usage

 1. Put `sunrise.el` somewhere in your Emacs `load-path`.
 2. Add a `(require 'sunrise)` to your `.emacs` file.
 3. Choose some unused extension for files to be opened in Sunrise VIRTUAL mode and add it to `auto-mode-alist`, e.g. if you want to name your virtual directories like `*.svrm` just add to your `.emacs` file a line like the following:
       (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sunrise-virtual-mode))
 4. Evaluate the new lines, or reload your `.emacs` file, or restart Emacs.
 5. Type <kbd>M-x sunrise</kbd> to invoke the Sunrise Commander (or much better: bind the function to your favorite key combination). The command `sunrise-cd` invokes Sunrise and automatically selects the current file wherever it is in the filesystem. Type h at any moment for information on available key bindings.
 6. Type <kbd>M-x customize-group [RET] sunrise [RET]</kbd> to customize options, fonts and colors (activate AVFS support here, too).
 7. Enjoy 😊
