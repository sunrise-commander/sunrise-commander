;;; sunrise-checkpoint-old.el --- Backward-compatible checkpoints for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 28 Dec 2009
;; Version: 1
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Keywords: files, sunrise commander, old checkpoints
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

;; Beginning with version 4 of the Sunrise Commander, checkpoints were redefined
;; to be a special form of bookmarks. Unfortunately, creating bookmarks with
;; custom handlers isn't supported in the version of bookmarks.el distributed
;; with Emacs 22, so if you use Sunrise checkpoints and you don't want to update
;; your bookmarks.el, just add this extension to your .emacs.el to get back the
;; original functionality.

;; This extension was written on GNU Emacs 23 on Linux, and tested on GNU Emacs
;; 22 and 23 for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'. (Optionally) compile
;; it.

;; 2) Enjoy ;-) -- Sunrise should pick the correct extension automatically. On
;; Emacs 23 it will look for sunrise-checkpoint, while on Emacs 22 it'll try
;; to load sunrise-checkpoint-old. Only if you *really* want to use the old
;; extensions with a more recent version of bookmarks.el than the one bundled
;; with Emacs 22 you may add a new (require 'sunrise-checkpoint-old) to your
;; .emacs file somewhere after (require 'sunrise).

;;; Code:

(require 'sunrise)

(defvar sunrise-checkpoint-registry '(("~" "~/" "~/"))
  "Registry of currently defined checkpoints.")

(defun sunrise-checkpoint-save (&optional name)
  "Save the current Sunrise pane directories under NAME for later restoring."
  (interactive "sCheckpoint name to save? ")
  (let ((my-cell (assoc-string name sunrise-checkpoint-registry)))
    (sunrise-save-directories)
    (if (null my-cell)
        (setq sunrise-checkpoint-registry
              (cons (cons name (list sunrise-left-directory sunrise-right-directory))
                    sunrise-checkpoint-registry))
      (setcdr my-cell (list sunrise-left-directory sunrise-right-directory)))
  (message "%s" (concat "Checkpoint \"" name "\" saved"))))

(defun sunrise-checkpoint-restore (&optional name)
  "Restore a checkpoint previously saved under NAME."
  (interactive "sCheckpoint name to restore? " )
  (let* ((cp-list (assoc-string name sunrise-checkpoint-registry))
         (dirs-list (cdr cp-list)))
    (unless cp-list
      (error (concat "No such checkpoint: " name)))
    (if (eq sunrise-selected-window 'right)
        (setq dirs-list (reverse dirs-list)))
    (mapc (lambda (x) (sunrise-goto-dir x) (sunrise-change-window)) dirs-list)))

(defun sunrise-checkpoint-handler (&optional arg)
  "Dummy function for compatilibity with the new checkpoints interface."
  (ignore arg))

(provide 'sunrise-checkpoint-old)

;;; sunrise-checkpoint-old.el ends here
