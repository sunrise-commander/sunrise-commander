;;; sunrise-checkpoint.el --- Checkpoint bookmarks for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 29 Dec 2009
;; Version: 1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, sunrise commander, checkpoints, bookmarks
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

;; Beginning with version 4 of the Sunrise Commander, checkpoints were
;; redefined to be a special form of bookmarks. Unfortunately, the differences
;; between the bookmarks frameworks in Emacs 22 and Emacs 23 are so big that
;; including this code directly in sunrise.el would make it incompatible with
;; Emacs 22. For this reason both versions of checkpoints are now provided as
;; dynamically loaded extensions, so that you can decide which of them to use.
;; To be sure, this is the version I intend to further develop, as it has a
;; richer set of functions and integrates more nicely to the rest of Emacs.
;; The other one is deprecated and will eventually disappear once Emacs 23+
;; becomes the "stable" release.

;; This extension was written and tested on GNU Emacs 23 on Linux.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'. (Optionally) compile
;; it.

;; 2) Enjoy ;-)

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'bookmark)

(require 'sunrise)

(defun sunrise-checkpoint-save (&optional _arg)
  "Create a new checkpoint bookmark to save the location of both panes."
  (interactive "p")
  (sunrise-save-directories)
  (let ((bookmark-make-record-function 'sunrise-checkpoint-make-record))
    (call-interactively 'bookmark-set)))

(defun sunrise-checkpoint-restore (&optional _arg)
  "Call `bookmark-jump' interactively."
  (interactive "p")
  (call-interactively 'bookmark-jump)
  (sunrise-history-push default-directory)
  (sunrise-in-other (sunrise-history-push default-directory)))

(defun sunrise-checkpoint-make-record ()
  "Generate a the bookmark record for a new checkpoint."
  `((filename . ,(format "Sunrise Checkpoint: %s | %s"
                         sunrise-left-directory sunrise-right-directory))
    (sunrise-directories . (,sunrise-left-directory
                            ,sunrise-right-directory))
    (handler . sunrise-checkpoint-handler)))

(defun sunrise-checkpoint-handler (&optional bookmark)
  "Handler for a checkpoint BOOKMARK."
  (sunrise-ensure-running)
  (sunrise-select-window 'left)
  (let ((dirs (cdr (assq 'sunrise-directories (cdr bookmark))))
        (missing '()))
    (mapc (lambda (x)
            (if (file-directory-p x)
                (sunrise-save-aspect (dired x) (sunrise-bookmark-jump))
              (setq missing (cons sunrise-selected-window missing)))
            (sunrise-change-window))
          dirs)
    (if missing (sunrise-checkpoint-relocate bookmark (reverse missing)))))

(defun sunrise-checkpoint-relocate (bookmark &optional sides)
  "Handle relocation of checkpoint BOOKMARK to SIDES."
  (interactive (list (bookmark-completing-read "Bookmark to relocate")))
  (let* ((sides (or sides '(left right)))
         (name (car bookmark))
         (dirs (assq 'sunrise-directories (cdr bookmark)))
         (relocs (mapcar
                  (lambda (x)
                    (read-directory-name
                     (format "Relocate %s [%s] to: " name (symbol-name x))))
                  sides))
         (result (cond ((< 1 (length relocs)) relocs)
                       ((eq 'right (car sides))
                        (list (cadr dirs) (car relocs)))
                       (t (list (car relocs) (caddr dirs))))))
    (setcdr dirs result)
    (bookmark-set-filename
     bookmark (apply 'format "Sunrise Checkpoint: %s | %s" result)))
  (bookmark-save)
  (sunrise-checkpoint-handler bookmark))

(defadvice bookmark-relocate
    (around sunrise-checkpoint-advice-bookmark-relocate (bookmark))
  "Advice for bookmark relocation."
  (let ((bmk (bookmark-get-bookmark bookmark)))
    (if (assq 'sunrise-directories bmk)
        (sunrise-checkpoint-relocate bmk)
      ad-do-it)))
(ad-activate 'bookmark-relocate)

(defun sunrise-checkpoint-unload-function ()
  "Unload the Sunrise Commander checkpoint extension."
  (sunrise-ad-disable "^sunrise-checkpoint-"))

(provide 'sunrise-checkpoint)

;;; sunrise-checkpoint.el ends here
