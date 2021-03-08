;;; sunrise-popviewer.el --- Floating viewer window for the Sunrise Commander -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012 José Alfredo Romero Latouche.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: José Alfredo Romero Latouche <escherdragon@gmail.com>
;;      Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero Latouche <escherdragon@gmail.com>
;; Created: 20 Aug 2008
;; Version: 3
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, sunrise commander, windows, accessibility, viewer
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

;; This extension advises several Sunrise Commander functions in order to make
;; the viewer window "float", i.e. instead of having a dedicated window
;; sitting under the panes all the time, a new frame is displayed whenever the
;; user requests to view a file (by pressing "o" or "v") or to open a command
;; line in the current directory.

;; WARNING: This code and the Buttons extension (sunrise-buttons) do NOT mix
;; together, if you're using the Buttons extension remove it first from your
;; .emacs file.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) If you are currently using the Buttons extension (sunrise-buttons),
;; remove it first from your .emacs file.

;; 2) Add a (require 'sunrise-popviewer) expression to your .emacs file
;; somewhere after the (require 'sunrise) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart
;; Emacs.

;; 4) Use `sunrise-popviewer-mode' to toggle the functionality.

;; 5) The next time you invoke the Sunrise Commander, only two panes will be
;; displayed. If you press o (or v) on a file inside any of them, it will be
;; displayed in a new frame. If you press C-c t to open a terminal in the
;; current directory, it'll be opened in a new frame too.

;; 6) Enjoy ;-)

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'sunrise)

(defcustom sunrise-popviewer-enabled nil
  "Whether the popviewer extension should be active at startup."
  :group 'sunrise
  :type 'boolean)

(defcustom sunrise-popviewer-style 'dedicated-frames
  "Determines the way frames are used for quick viewing files:

* Single Frame: reuse the same frame whenever possible.
* Single Dedicated Frame: reuse frame, close when last buffer is killed.
* Multiple Frames: new frame for every new file (or terminal) displayed.
* Dedicated Frames: new frame, close it whenever its buffer is killed."
  :group 'sunrise
  :type '(choice
          (const single-frame)
          (const single-dedicated-frame)
          (const multiple-frames)
          (const dedicated-frames)))

(defcustom sunrise-popviewer-select-viewer-action nil
  "Alternative function for selecting a viewer window."
  :group 'sunrise
  :type 'function)

(defvar sunrise-popviewer-frame-name "Sunrise Viewer"
  "Name of the frame being currently used as the viewer.")

(defun sunrise-popviewer-setup-windows ()
  "`sunrise-setup-windows' replacement for `sunrise-popviewer-mode'."
  (interactive)
  (bury-buffer)
  (delete-other-windows)

  (cl-case sunrise-window-split-style
    (horizontal (split-window-horizontally))
    (vertical   (split-window-vertically))
    (top        (ignore))
    (t          (error "Sunrise: don't know how to split this window: %s"
                       sunrise-window-split-style)))

  (sunrise-setup-visible-panes)
  (sunrise-select-window sunrise-selected-window)
  (sunrise-restore-panes-width)
  (setq other-window-scroll-buffer nil)
  (run-hooks 'sunrise-start-hook))

(defadvice sunrise-setup-windows
    (around sunrise-popviewer-advice-setup-windows)
  "Set up the Sunrise window configuration (two windows in `sunrise-mode')."
  (sunrise-popviewer-setup-windows))

(defun sunrise-popviewer-get-frame ()
  "Return the frame being currently used as the viewer, if any."
  (cdr (assoc sunrise-popviewer-frame-name (make-frame-names-alist))))

(defun sunrise-popviewer-pop-frame ()
  "Bring forward the viewer frame, create a new one if necessary."
  (let* ((vframe (sunrise-popviewer-get-frame)) (target-frame))
    (when vframe
      (select-frame vframe)
      (if (memq sunrise-popviewer-style
                '(single-frame single-dedicated-frame))
          (setq target-frame vframe)
        (set-frame-name (buffer-name))))
    (unless target-frame
      (setq other-window-scroll-buffer nil)
      (setq target-frame
            (make-frame `((name . ,sunrise-popviewer-frame-name)))))
    (select-frame target-frame)
    (raise-frame)))

(defun sunrise-popviewer-dedicate-frame ()
  "Take care of dedicating the current window as to its frame, if necessary."
  (let ((vframe (sunrise-popviewer-get-frame)))
    (when vframe
      (select-frame vframe)
      (set-window-dedicated-p
       (frame-first-window vframe)
       (memq sunrise-popviewer-style
             '(single-dedicated-frame dedicated-frames))))
    (add-hook
     'kill-buffer-hook
     (lambda () (sunrise-select-window sunrise-selected-window))
     t t)))

(defun sunrise-popviewer-quick-view (&optional arg)
  "Quickly view the currently selected item.

* Regular files are opened in a separate frame.
* Directories are visited in the passive pane.
* Symbolic link targets are visited in the passive pane.

The prefix argument ARG is as for `sunrise-quick-view'."
  (interactive "P")
  (setq
   other-window-scroll-buffer
   (let ((other-window-scroll-buffer
          (if (memq sunrise-popviewer-style
                    '(single-frame single-dedicated-frame))
              other-window-scroll-buffer
            nil)))
     (sunrise-quick-view arg)
     (sunrise-popviewer-dedicate-frame)
     other-window-scroll-buffer)))

(defadvice sunrise-term
    (around sunrise-popviewer-advice-term (&optional cd newterm program))
  "Make terminal windows dedicated when using multiple viewers."
  (let ((sunrise-popviewer-style (if (or newterm program)
                                     sunrise-popviewer-style
                                   'single-frame)))
    ad-do-it)
  (sunrise-popviewer-dedicate-frame))

(defun sunrise-popviewer-select-viewer-window ()
  "Popviewer replacement for `sunrise-select-viewer-window'."
  (interactive)
  (cond (sunrise-popviewer-select-viewer-action
         (funcall sunrise-popviewer-select-viewer-action))
        ((null window-system) (other-window 1))
        (t (sunrise-popviewer-pop-frame))))

(defadvice sunrise-select-viewer-window
    (around sunrise-popviewer-advice-select-viewer-window)
  "Try to select a window that is not a SC pane in a separate frame."
  (sunrise-popviewer-select-viewer-window))

(defadvice sunrise-cd
    (around sunrise-popviewer-advice-cd (&optional norestore))
  "Redefine `sunrise-cd' not to disable Sunrise in PopViewer mode."
  (if (sunrise-running-p)
      (sunrise-popviewer-setup-windows)
    ad-do-it))

;;;###autoload
(define-minor-mode sunrise-popviewer-mode "Use an alternative viewer window."
  :global t
  :group 'sunrise
  :lighter ""
  (let ((hookfun (if sunrise-popviewer-mode 'remove-hook 'add-hook))
        (adfun (if sunrise-popviewer-mode
                   'sunrise-ad-enable
                 'sunrise-ad-disable))

        (viewerfun (if sunrise-popviewer-mode
                       'sunrise-popviewer-select-viewer-window
                     'sunrise-select-viewer-window))

        (quickviewfun (if sunrise-popviewer-mode
                          'sunrise-popviewer-quick-view
                        'sunrise-quick-view))

        (panelockfun (if sunrise-popviewer-mode
                         'sunrise-popviewer-setup-windows
                       'sunrise-lock-panes)))

    (funcall hookfun 'window-size-change-functions 'sunrise-lock-window)
    (define-key sunrise-mode-map "o" quickviewfun)
    (define-key sunrise-mode-map "v" quickviewfun)
    (define-key sunrise-mode-map "\C-c\t" viewerfun)
    (define-key sunrise-mode-map [(control tab)] viewerfun)
    (define-key sunrise-mode-map "\\" panelockfun)
    (funcall adfun "^sunrise-popviewer-")
    (when (sunrise-running-p) (sunrise-setup-windows))))

(defun sunrise-popviewer-unload-function ()
  "Unload the Sunrise Commander popup viewer extension."
  (sunrise-popviewer-mode -1)
  (sunrise-ad-disable "^sunrise-popviewer-"))

(sunrise-popviewer-mode (if sunrise-popviewer-enabled 1 -1))

(provide 'sunrise-popviewer)

;;; sunrise-popviewer.el ends here
