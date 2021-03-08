;;; sunrise-loop.el --- Background file operations for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 27 Jun 2008
;; Version: 3
;; Package-Requires: ((emacs "24.4"))
;; Keywords: files, sunrise commander, background copy rename move
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

;; This extension adds to the Sunrise Commander the capability of performing
;; copy and rename operations in the background. It provides prefixable
;; drop-in replacements for the `sunrise-do-copy' and `sunrise-do-rename'
;; commands and uses them to redefine their bindings in the `sunrise-mode-map'
;; keymap. When invoked the usual way (by pressing C or R), these new
;; functions work exactly as the old ones, i.e. they simply pass the control
;; flow to the logic already provided by Sunrise, but when prefixed (e.g. by
;; pressing C-u C or C-u R) they launch a separate Elisp intepreter in the
;; background, delegate to it the execution of all further operations and
;; return immediately, so the Emacs UI remains fully responsive while any
;; potentially long-running copy or move tasks can be let alone to eventually
;; reach their completion in the background.

;; After all requested actions have been performed, the background interpreter
;; remains active for a short period of time (30 seconds by default, but it
;; can be customized), after which it shuts down automatically.

;; At any moment you can abort all tasks scheduled and under execution and
;; force the background interpreter to shut down by invoking the
;; `sunrise-loop-stop' command (M-x sunrise-loop-stop).

;; If you need to debug something or are just curious about how this extension
;; works, you can set the variable `sunrise-loop-debug' to t to have the
;; interpreter launched in debug mode. In this mode all input and output of
;; background operations are sent to a buffer named *SUNRISE-LOOP*. To return
;; to normal mode set `sunrise-loop-debug' back to nil and use
;; `sunrise-loop-stop' to kill the currently running interpreter.

;; The extension disables itself and tries to do its best to keep out of the
;; way when working with remote directories through FTP (e.g. when using
;; ange-ftp), since in these cases the execution of file transfers in the
;; background should be managed directly by the FTP client.

;; It was written on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-loop) expression to your .emacs file somewhere
;; after the (require 'sunrise) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart
;; Emacs.

;; 4) The next time you need to copy of move any big files, just prefix the
;; appropriate command with C-u.

;; 5) Enjoy ;-)

;; 6) You can use `unload-feature' to get rid of the provided functionality
;; completely.

;;; Code:

(require 'sunrise)

(defcustom sunrise-loop-debug nil
  "Activate debug mode in the Sunrise Loop extension.
When set, the background elisp interpreter is launched in such a
way that all background input and output are sent to a buffer
named *SUNRISE LOOP* and automatic lifecycle management is
disabled (i.e. you have to kill the interpreter manually using
sunrise-loop-stop to get rid of it)."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-loop-timeout 30
  "Number of seconds to wait while idle before shutting down the interpreter.
After executing one or more operations in the background, the
Sunrise Loop Elisp interpreter will be killed automatically after
this amount of time."
  :group 'sunrise
  :type 'integer)

(defcustom sunrise-loop-use-popups t
  "When non-nil, display pop-up notification when execution queue is emptied."
  :group 'sunrise
  :type 'boolean)

(defvar sunrise-loop-process nil)
(defvar sunrise-loop-timer nil)
(defvar sunrise-loop-scope nil)
(defvar sunrise-loop-queue nil)

(defun sunrise-loop-start ()
  "Launch and initiate a new background Elisp interpreter.
The new interpreter runs in batch mode and inherits all functions
from the Sunrise Commander (sunrise.el) and from this
file."
  (let ((process-connection-type nil)
        (sunrise-main (symbol-file 'sunrise-mode))
        (sunrise-loop (symbol-file 'sunrise-loop-cmd-loop))
        (emacs (concat invocation-directory invocation-name)))
    (setq sunrise-loop-process (start-process
                                "Sunrise-Loop"
                                (if sunrise-loop-debug "*SUNRISE-LOOP*" nil)
                                emacs
                                "-batch" "-q" "-no-site-file"
                                "-l" sunrise-main "-l" sunrise-loop
                                "-eval" "(sunrise-loop-cmd-loop)"))
    (sunrise-loop-enqueue `(setq load-path (quote ,load-path)))
    (sunrise-loop-enqueue '(require 'sunrise))
    (if sunrise-loop-debug
        (sunrise-loop-enqueue '(setq sunrise-loop-debug t))
      (set-process-filter sunrise-loop-process 'sunrise-loop-filter))
    (setq sunrise-loop-queue nil)))

(defun sunrise-loop-disable-timer ()
  "Disable the automatic shutdown timer.
This is done every time we send a new task to the background
interpreter, lest it gets nuked before completing its queue."
  (if sunrise-loop-timer
      (progn
        (cancel-timer sunrise-loop-timer)
        (setq sunrise-loop-timer nil))))

(defun sunrise-loop-enable-timer ()
  "Enable the automatic shutdown timer.
This is done every time we receive confirmation from the
background interpreter that all the tasks delegated to it have
been completed. Once this function is executed, if no new tasks
are enqueued before `sunrise-loop-timeout' seconds, the interpreter is
killed."
  (sunrise-loop-disable-timer)
  (setq sunrise-loop-timer
        (run-with-timer sunrise-loop-timeout nil 'sunrise-loop-stop)))

(defun sunrise-loop-stop (&optional interrupt)
  "Shut down the background Elisp interpreter and clean up after it.

If INTERRUPT is non-nil, force an immediate shutdown."
  (interactive "p")
  (sunrise-loop-disable-timer)
  (when sunrise-loop-queue
    (cond (interrupt
           (sunrise-loop-notify
            "Aborted. Some operations may remain unfinished.")
           (setq sunrise-loop-queue nil))
          (t (sunrise-loop-enable-timer))))
  (unless sunrise-loop-queue
    (delete-process sunrise-loop-process)
    (setq sunrise-loop-process nil)))

(defun sunrise-loop-notify (string)
  "Show message STRING to notify the user about an event."
  (if (and window-system sunrise-loop-use-popups)
      (x-popup-dialog t (list string '("OK")) t)
    (message (concat "[[" string "]]"))))

(defun sunrise-loop-filter (_process output)
  "Process filter for the background interpreter.

OUTPUT is partial output from the interpreter."
  (mapc (lambda (line)
          (cond ((string-match "^\\[\\[\\*\\([^\]\*]+\\)\\*\\]\\]$" line)
                 (sunrise-loop-notify (match-string 1 line)))

                ((and (or (string-match "^\\[\\[" line)
                          (string-match "^Sunrise Loop: " line))
                      (< 0 (length line)))
                 (message "%s" line))

                ((eq ?^ (string-to-char line))
                 (let ((command (substring line 1)))
                   (when (string= command (car sunrise-loop-queue))
                     (pop sunrise-loop-queue)
                     (sunrise-loop-enable-timer)
                     (unless sunrise-loop-queue
                       (sunrise-loop-notify "Background job finished!")))))
                (t nil)))
        (split-string output "\n")))

(defun sunrise-loop-enqueue (form)
  "Delegate evaluation of FORM to the background interpreter.
If no such interpreter is currently running, launches a new one."
  (sunrise-loop-disable-timer)
  (unless sunrise-loop-process
    (sunrise-loop-start))
  (let ((command (prin1-to-string form)))
    (setq sunrise-loop-queue (append sunrise-loop-queue (list (md5 command))))
    (process-send-string sunrise-loop-process command)
    (process-send-string sunrise-loop-process "\n")))

(defun sunrise-loop-cmd-loop ()
  "Main execution loop for the background Elisp interpreter."
  (sunrise-ad-disable "^sunrise-loop-")
  (defun read-char nil ?y) ;; Always answer "yes" to any prompt
  (let ((command) (signature))
    (while t
      (setq command (read))
      (setq signature (md5 (prin1-to-string command)))
      (condition-case description
          (progn
            (if sunrise-loop-debug
                (message "%s" (concat "[[Executing in background: "
                                      (prin1-to-string command) "]]")))
            (eval command)
            (message "[[Command successfully invoked in background]]"))
        (error (message "%s" (concat "[[*ERROR IN BACKGROUND JOB: "
                                     (prin1-to-string description) "*]]"))))
      (message "^%s" signature))))

(defun sunrise-loop-applicable-p ()
  "Return non-nil if an operation is suitable for the background interpreter."
  (and (null (string-match "^/ftp:" dired-directory))
       (null (string-match "^/ftp:" sunrise-other-directory))))

(defun sunrise-loop-do-copy (&optional arg)
  "Drop-in prefixable replacement for the `sunrise-do-copy' command.

When invoked with a prefix argument ARG, sets a flag that is used
later by advice to decide whether to delegate further copy
operations to the background interpreter."
  (interactive "P")
  (if (and arg (sunrise-loop-applicable-p))
      (let ((sunrise-loop-scope t))
        (sunrise-do-copy))
    (sunrise-do-copy)))

(defun sunrise-loop-do-clone (&optional arg)
  "Drop-in prefixable replacement for the `sunrise-do-clone' command.

When invoked with a prefix argument ARG, sets a flag that is used
later by advice to decide whether to delegate further copy
operations to the background interpreter."
  (interactive "P")
  (if (and arg (sunrise-loop-applicable-p))
      (let ((sunrise-loop-scope t))
        (call-interactively 'sunrise-do-clone))
    (call-interactively 'sunrise-do-clone)))

(defun sunrise-loop-do-rename (&optional arg)
  "Drop-in prefixable replacement for the `sunrise-do-rename' command.

When invoked with a prefix argument ARG, sets a flag that is used
later by advice to decide whether to delegate further rename
operations to the background interpreter."
  (interactive "P")
  (if (and arg (sunrise-loop-applicable-p))
      (let ((sunrise-loop-scope t))
        (sunrise-do-rename))
    (sunrise-do-rename)))

(defadvice sunrise-progress-prompt (around sunrise-loop-advice-progress-prompt
                                           activate)
  "Display \"Sunrise Loop\" instead of \"Sunrise\" in the prompt."
  (setq ad-return-value
        (concat (if sunrise-loop-scope "Sunrise Loop: " "Sunrise: ")
                (ad-get-arg 0)
                "...")))

(defadvice y-or-n-p (before sunrise-loop-advice-y-or-n-p activate)
  "Modify all confirmation request messages inside a loop scope."
  (when sunrise-loop-scope
    (setq (ad-get-arg 0)
          (replace-regexp-in-string
           "\?" " in the background? (overwrites ALWAYS!)" (ad-get-arg 0)))))

(defadvice dired-mark-read-file-name
    (before sunrise-loop-advice-dired-mark-read-file-name
            (prompt dir op-symbol arg files &optional default)
            activate)
  "Modify all queries from Dired inside a loop scope."
  (if sunrise-loop-scope
      (setq prompt (replace-regexp-in-string
                    "^\\([^ ]+\\) ?\\(.*\\)"
                    "\\1 (in background - overwrites ALWAYS!) \\2" prompt))))

(defadvice dired-create-files
    (around sunrise-loop-advice-dired-create-files
            (file-creator operation fn-list name-constructor
                          &optional marker-char)
            activate)
  "Delegate `dired-do-copy' ops in a loop to background interpreter."
  (if sunrise-loop-scope
      (with-no-warnings
        (sunrise-loop-enqueue
         `(let ((target ,target))       ; cf. `dired-do-create-files'
            (dired-create-files (function ,file-creator)
                                ,operation
                                (quote ,fn-list)
                                ,name-constructor nil))))
    ad-do-it))

(defadvice sunrise-clone-files
    (around sunrise-loop-advice-clone-files
            (file-path-list
             target-dir
             clone-op
             progress
             &optional do-overwrite)
            activate)
  "Delegate `sunrise-do-copy' ops in a loop to background interpreter."
  (if sunrise-loop-scope
      (sunrise-loop-enqueue
       `(sunrise-clone-files
         (quote ,file-path-list) ,target-dir #',clone-op ',progress 'ALWAYS))
    ad-do-it))

(defadvice sunrise-move-files
    (around sunrise-loop-advice-move-files
            (file-path-list target-dir progress &optional do-overwrite)
            activate)
  "Delegate `sunrise-do-rename' ops in a loop to background interpreter."
  (if sunrise-loop-scope
      (sunrise-loop-enqueue
       `(sunrise-move-files
         (quote ,file-path-list) ,target-dir ',progress 'ALWAYS))
    ad-do-it))

(define-key sunrise-mode-map "C" 'sunrise-loop-do-copy)
(define-key sunrise-mode-map "K" 'sunrise-loop-do-clone)
(define-key sunrise-mode-map "R" 'sunrise-loop-do-rename)

(defun sunrise-loop-unload-function ()
  "Unload the Sunrise Commander loop extension."
  (sunrise-ad-disable "^sunrise-loop-")
  (define-key sunrise-mode-map "C" 'sunrise-do-copy)
  (define-key sunrise-mode-map "K" 'sunrise-do-clone)
  (define-key sunrise-mode-map "R" 'sunrise-do-rename))

(provide 'sunrise-loop)

;;; sunrise-loop.el ends here
