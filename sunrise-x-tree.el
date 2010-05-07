;;; sunrise-x-tree.el --- Tree View for the Sunrise Commander File Manager.

;; Copyright (C) 2010 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;; Keywords: Sunrise Commander Emacs File Manager Directories Tree Navigation

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

(require 'sunrise-commander)
(require 'tree-widget)
(eval-when-compile (require 'desktop))

(defcustom sr-tree-explosion-ratio 3
  "Maximum number of directory levels for the sr-tree-explode-branch function to
  open recursively each time it's evaluated."
  :group 'sunrise
  :type 'integer)

(defvar sr-tree-root nil
  "Root widget of the current tree view.")

(defvar sr-tree-open-paths nil
  "List of paths to all the directories open in the current tree view.")

(defvar sr-tree-cursor nil
  "Cons cell in which the CAR contains the label, and the CDR contains the file
  path to the selected directory in the current tree view.")

(defvar sr-tree-mode-map (make-sparse-keymap)
  "Keymap for the Sunrise Commander Tree View.")

;;; ============================================================================
;;; GUI Management functions:

(define-widget 'sr-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs  'sr-tree-expand-dir
  :has-children   t)

(defun sr-tree-focus-widget ()
  "Move point to the first button widget in the current line (if any)."
  (interactive)
  (beginning-of-line)
  (unless (get-char-property (point) 'button)
    (while (not (or (eolp) (get-char-property (point) 'button)))
      (forward-char))))

(defun sr-tree-get-button ()
  "Return the first button widget in the current line (if any)."
  (sr-tree-focus-widget)
  (get-char-property (point) 'button))

(defun sr-tree-get-branch ()
  "Return the first tree widget in the current line (if any)."
  (widget-get (sr-tree-get-button) :parent))

(defun sr-tree-refresh-dir (widget &rest ignore)
  "Refresh WIDGET parent (or own) tree children. IGNORE other arguments."
  (let ((tree (if (tree-widget-p widget)
                  widget
                (widget-get widget :parent))))
    (widget-put tree :args nil) ;; Clear the tree children cache.
    (widget-value-set tree (widget-value tree))) ;; Redraw the tree node.
  (if (fboundp 'sr-tabs-refresh)
      (sr-tabs-refresh))
  (sr-highlight))

(defun sr-tree-refresh-branch (&optional prefix)
  "Revert the currently selected branch in the directory tree. If no branch is
  selected, then select the root node and revert the whole tree. If PREFIX is
  not nil, close first all open subdirectories in the tree."
  (interactive "P")
  (if prefix
      (setq sr-tree-open-paths nil))
  (let ((button (sr-tree-get-button)))
    (unless button
      (sr-tree-beginning-of-buffer)
      (setq button (sr-tree-get-button)))
    (sr-tree-refresh-dir button)))

(defun sr-tree-revert-buffer (&optional ignore-auto noconfirm)
  "Revert the current Sunrise Tree View buffer."
  (interactive)
  (sr-tree-refresh-branch))

(defun sr-tree-widget (e &optional open)
  "Return a widget to display directory E. With a non-nil optional argument
  OPEN, display the widget as open from the start."
  (let ((is-open (or open (member e sr-tree-open-paths)))
        (tag (replace-regexp-in-string "/?$" "" e)))
    (setq tag (file-name-as-directory (file-name-nondirectory tag)))
    `(sr-tree-dir-widget
      :open ,is-open
      :node (push-button
             :tag ,tag
             :format "%[%t%]\n"
             :notify sr-tree-refresh-dir)
      :path ,e)))

(defun sr-tree-path-line (&optional path)
  "Transform PATH into a suitable "
  (let ((path (expand-file-name (or path (cdr sr-tree-cursor) ""))))
    (replace-regexp-in-string "/?$" "" path)))

(defun sr-tree-highlight ()
  (save-excursion
    (let ((path (or (cdr sr-tree-cursor) "-"))
          (inhibit-read-only t))
      (goto-char (point-min))
      (kill-line)
      (widget-insert (sr-tree-path-line nil) " ")
      (sr-highlight))))

(defun sr-tree-list (dir)
  "Return the list of entries in DIR. Place directories first."
  (let ((entries (directory-files dir 'full)) dirs entry rel-entry)
    (while entries
      (setq entry   (car entries)
            rel-entry (file-relative-name entry (concat entry "/.."))
            entries (cdr entries))
      (if (and (file-directory-p entry)
               (not (string= (substring entry -1) "."))
               (or (not dired-omit-mode)
                   (not (eq ?. (string-to-char rel-entry)))))
          (setq dirs (cons entry dirs))))
    (nreverse dirs)))

(defun sr-tree-expand-dir (tree)
  "Return TREE widget children. Reuse :args cache if exists."
  (or (widget-get tree :args)
      (let ((dir (widget-get tree :path)))
        (message "Reading directory '%s'..." dir)
        (condition-case err
            (prog1
                (mapcar 'sr-tree-widget (sr-tree-list dir))
              (message "Reading directory '%s'...done" dir))
          (error
           (message "%s" (error-message-string err))
           nil)))))

(defun sr-tree-register-path (widget)
  (let ((path (sr-tree-path-line (widget-get widget :path))))
    (setq sr-tree-open-paths
          (if (widget-get widget :open)
              (cons path sr-tree-open-paths)
            (delete path sr-tree-open-paths)))))
(add-hook 'tree-widget-after-toggle-functions 'sr-tree-register-path)

(defun sr-tree-build (root)
  (interactive "DRoot: ")
  (cd (setq root (expand-file-name root)))
  (let ((inhibit-read-only t)
        (all (overlay-lists)))
    (erase-buffer)
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all))
    (tree-widget-set-theme "folder")
    (widget-insert (format "%s \n\n" (sr-tree-path-line root)))
    (set
     (make-local-variable 'sr-tree-root)
     (widget-create (sr-tree-widget root t)))
    (widget-setup)
    (if sr-tree-cursor
        (sr-tree-search-cursor)
      (sr-tree-beginning-of-buffer))
    (sr-tree-refresh-branch)
    (sr-tree-register-path sr-tree-root)))

(defun sr-tree-goto-dir (root &optional keep-state)
  (interactive)
  (unless keep-state
    (setq sr-tree-cursor nil sr-tree-open-paths nil))
  (setq root (expand-file-name root))
  (sr-tree-build root)
  (sr-keep-buffer)
  (sr-history-push root))

;;; ============================================================================
;;; GUI interaction functions:

(defun sr-tree-toggle-branch (&optional action)
  (interactive)
  (let ((is-open (widget-get (sr-tree-get-branch) :open)))
    (when (or (and is-open (eq action 'close))
              (and (not is-open) (eq action 'open))
              (null action))
      (sr-tree-focus-widget)
      (widget-button-press (point)))))

(defun sr-tree-open-branch ()
  (interactive)
  (if (widget-get (sr-tree-get-branch) :open)
      (sr-tree-advertised-find-file)
    (sr-tree-toggle-branch 'open)))

(defun sr-tree-explode-branch (&optional level branch)
  (interactive)
  (unless (or level branch)
    (recenter (truncate (/ (window-body-height) 10.0))))
  (let ((level (or level sr-tree-explosion-ratio))
        (branch (or branch (sr-tree-get-branch)))
        args)
    (when (< 0 level)
      (unless (widget-get branch :open)
        (setq level (1- level))
        (funcall #'tree-widget-action branch))
      (dolist (child (cdr (widget-get branch :children)))
        (sr-tree-explode-branch level child)))))

(defun sr-tree-close-branch ()
  (interactive)
  (sr-tree-toggle-branch 'close))

(defun sr-tree-get-cursor ()
  (let* ((cursor-node (sr-tree-get-button))
         (cursor-tree (if cursor-node (widget-get cursor-node :parent)))
         (cursor-node (widget-get cursor-node :node))
         (cursor-tag (widget-get cursor-node :tag))
         (cursor-path (sr-tree-path-line (widget-get cursor-tree :path))))
    (and cursor-tag cursor-path (cons cursor-tag cursor-path))))

(defun sr-tree-update-cursor ()
  (setq sr-tree-cursor (sr-tree-get-cursor))
  (when sr-tree-cursor
    (setq sr-this-directory (cdr sr-tree-cursor))
    (sr-tree-highlight)
    (cd (cdr sr-tree-cursor))
    (when (and (featurep 'sunrise-x-modeline)
               (not (eq mode-line-format (default-value 'mode-line-format))))
      (if (fboundp 'sr-modeline-refresh)
          (sr-modeline-refresh))
      (force-mode-line-update))
    (when sr-synchronized
      (sr-tree-advertised-find-file-other))))

(defun sr-tree-next-line ()
  (interactive)
  (forward-line)
  (sr-tree-update-cursor))

(defun sr-tree-previous-line ()
  (interactive)
  (forward-line -1)
  (sr-tree-update-cursor))

(defun sr-tree-scroll-down (&optional arg)
  (interactive)
  (scroll-down arg)
  (sr-tree-update-cursor))

(defun sr-tree-scroll-up (&optional arg)
  (interactive)
  (scroll-up arg)
  (sr-tree-update-cursor))

(defun sr-tree-search-cursor (&optional init-cursor recursing)
  (let ((cursor (or init-cursor sr-tree-cursor)) new-cursor)
    (if (null cursor)
        (sr-tree-beginning-of-buffer)
      (unless recursing (goto-char (point-min)))
      (when (search-forward (car cursor) (point-max) t)
        (setq new-cursor (sr-tree-get-cursor))
        (if (and new-cursor (not (sr-equal-dirs (cdr cursor) (cdr new-cursor))))
            (progn
              (sr-tree-next-line)
              (sr-tree-search-cursor cursor t))
          (sr-tree-update-cursor))))))

(defun sr-tree-omit-mode (&optional force)
  (interactive)
  (setq dired-omit-mode (or force (not dired-omit-mode)))
  (revert-buffer))

(defun sr-tree-beginning-of-buffer ()
  (interactive)
  (goto-char (widget-get sr-tree-root :from))
  (sr-tree-update-cursor))

(defun sr-tree-end-of-buffer ()
  (interactive)
  (forward-sentence)
  (sr-tree-update-cursor))

(defun sr-tree-isearch-forward (&optional prefix)
  (interactive "P")
  (if prefix
      (add-hook 'isearch-mode-end-hook 'sr-tree-post-isearch))
  (isearch-forward nil t))

(defun sr-tree-isearch-backward (&optional prefix)
  (interactive "P")
  (if prefix
      (add-hook 'isearch-mode-end-hook 'sr-tree-post-isearch))
  (isearch-backward nil t))

(defun sr-tree-post-isearch ()
  (if isearch-mode-end-hook-quit
      (remove-hook 'isearch-mode-end-hook 'sr-tree-post-isearch)
    (progn
      (sr-tree-open-branch)
      (recenter (truncate (/ (window-body-height) 10.0)))
      (if (widget-get (sr-tree-get-branch) :args)
          (isearch-forward nil t)
        (remove-hook 'isearch-mode-end-hook 'sr-tree-post-isearch)))))

(defun sr-tree-handle-mouse-event (e handler)
  (when (and e (eq major-mode 'sr-tree-mode))
    (mouse-set-point e)
    (when (sr-tree-get-button)
      (sr-tree-update-cursor)
      (funcall handler)
      t)))

(defun sr-tree-mouse-advertised-find-file (e)
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-advertised-find-file))

(defun sr-tree-mouse-toggle-branch (e)
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-toggle-branch))

(defun sr-tree-mouse-focus-branch (e)
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-focus-branch))

(defun sr-tree-mouse-unfocus-branch (e)
  (interactive "e")
  (or (sr-tree-handle-mouse-event e 'sr-tree-unfocus-branch)
      (sr-tree-unfocus-branch)))

(defun sr-tree-mouse-explode-branch (e)
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-explode-branch))

;;; ============================================================================
;;; Tree Widget adapter functions:

(defun sr-tree-widget-forward (arg)
  (interactive "p")
  (if (eq major-mode 'sr-tree-mode)
      (sr-change-window)
    (widget-forward arg)))

(defun sr-tree-widget-button-press (pos &optional event)
  (interactive "@d")
  (if (eq major-mode 'sr-tree-mode)
      (sr-tree-open-branch)
    (widget-button-press pos event)))

(defun sr-tree-widget-button-click (event)
  (interactive "e")
  (unless (eq major-mode 'sr-tree-mode)
    (tree-widget-button-click event)))

;;; ============================================================================
;;; File system navigation functions:

(defun sr-tree-prev-subdir (&optional keep-state)
  (interactive)
  (let* ((branch (sr-tree-get-branch))
         (parent (widget-get branch :parent)))
    (cond
     ((tree-widget-p parent)
      (goto-char (widget-get parent :from))
      (sr-tree-update-cursor))

     ((sr-equal-dirs default-directory "/")
      (ignore))

     ((eq branch sr-tree-root)
      (sr-tree-goto-dir ".." keep-state)
      (sr-tree-beginning-of-buffer)))))

(defun sr-tree-collapse-branch ()
  (interactive)
  (let ((branch (sr-tree-get-branch)))
    (if (widget-get branch :open)
        (sr-tree-close-branch)
      (sr-tree-prev-subdir)
      (unless (eq (sr-tree-get-branch) sr-tree-root)
        (sr-tree-close-branch)))))

(defun sr-tree-focus-branch ()
  (interactive)
  (unless (eq (sr-tree-get-branch) sr-tree-root)
    (sr-tree-goto-dir (cdr sr-tree-cursor) t)))

(defun sr-tree-unfocus-branch ()
  (interactive)
  (let ((sr-tree-cursor sr-tree-cursor))
    (unless (eq (sr-tree-get-branch) sr-tree-root)
      (sr-tree-beginning-of-buffer))
    (sr-tree-prev-subdir t))
  (sr-tree-search-cursor)
  (recenter))

(defun sr-tree-advertised-find-file ()
  (interactive)
  (let ((sr-goto-dir-function nil))
    (sr-save-aspect
     (sr-alternate-buffer
      (sr-goto-dir (cdr sr-tree-cursor))))))

(defun sr-tree-advertised-find-file-other ()
  (interactive)
  (let ((target (cdr sr-tree-cursor))
        (sr-synchronized nil)
        (sr-goto-dir-function nil))
    (sr-in-other
     (progn (sr-goto-dir target)
            (sr-highlight 'sr-passive-path-face)))))

(defun sr-tree-sync ()
  (interactive)
  (unless sr-synchronized
    (sr-tree-advertised-find-file-other))
  (sr-sync)
  (sr-force-passive-highlight))

(defun sr-tree-dismiss ()
  (interactive)
  (let ((target (widget-get sr-tree-root :path))
        (sr-goto-dir-function nil))
    (sr-save-aspect
     (sr-alternate-buffer (sr-goto-dir target)))))

;;; ============================================================================
;;; File system manipulation functions:

(defmacro sr-tree-adapt-dired-command (command)
  `(let ((ad-redefinition-action 'accept))
     (ad-deactivate 'dired-get-filename)
     (flet
      ((dired-get-filename (&optional localp no-error)
                           (cdr sr-tree-cursor))
       (dired-show-file-type (file &optional deref-symlinks)
                             (message
                              "%s: directory"
                              (directory-file-name (car sr-tree-cursor)))))
       ,command)
     (ad-activate 'dired-get-filename)))

(defun sr-tree-do-copy ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-copy)))

(defun sr-tree-do-clone ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-clone)))

(defun sr-tree-do-symlink ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-symlink)))

(defun sr-tree-do-relsymlink ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-relsymlink)))

(defun sr-tree-do-rename ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-rename))
  (unless (file-exists-p (cdr sr-tree-cursor))
    (sr-tree-prev-subdir)
    (sr-tree-refresh-branch)))

(defun sr-tree-do-delete ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-delete))
  (unless (file-exists-p (cdr sr-tree-cursor))
    (sr-tree-prev-subdir)
    (sr-tree-refresh-branch)))

(defun sr-tree-show-files-info ()
  (interactive)
  (sr-tree-adapt-dired-command (sr-show-files-info)))

(define-derived-mode sr-tree-mode nil "Sunrise Tree View"
  "Tree view for the Sunrise Commander file manager."
  :group 'sunrise
  (set-keymap-parent sr-tree-mode-map sr-mode-map)
  (mapc 'make-local-variable '(sr-tree-open-paths
                               sr-tree-cursor
                               isearch-mode-end-hook
                               desktop-save-buffer
                               revert-buffer-function
                               sr-goto-dir-function))
  (add-hook 'isearch-mode-end-hook 'sr-tree-update-cursor)
  (setq desktop-save-buffer    'sr-tree-desktop-save-buffer
        revert-buffer-function 'sr-tree-revert-buffer
        sr-goto-dir-function   'sr-tree-goto-dir)
  (setq dired-omit-mode t)
  (unless sr-tree-root
    (sr-tree-build default-directory)))

(defun sr-tree-menu-init ()
  "Initializes the Sunrise Tree extension menu."

  (unless (lookup-key sr-mode-map [menu-bar Sunrise])
    (define-key sr-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Tree View")))
    (define-key sr-mode-map [menu-bar Sunrise tree-view]
      (cons "Tree View" menu-map))
    (define-key menu-map [enable-tree-view] '("enable" . sr-tree-view)))

  (define-key sr-tree-mode-map [menu-bar Sunrise]
    (cons "Sunrise" (make-sparse-keymap)))
  (let ((menu-map (make-sparse-keymap "Tree View")))
    (define-key sr-tree-mode-map [menu-bar Sunrise tree-view]
      (cons "Tree View" menu-map))
    (define-key menu-map [enable-tree-view] nil)
    (define-key menu-map [disable-tree-view] '("dismiss" . sr-tree-dismiss)))

  (remove-hook 'sr-start-hook 'sr-tree-menu-init)
  (unintern 'sr-tree-menu-init))

(add-hook 'sr-start-hook 'sr-tree-menu-init)

;;; ============================================================================
;;; Sunrise Tree View keybindings:

(define-key sr-tree-mode-map "\C-m" 'sr-tree-open-branch)
(define-key sr-tree-mode-map " " 'sr-tree-open-branch)
(define-key sr-tree-mode-map "\C-o" 'sr-tree-omit-mode)
(define-key sr-tree-mode-map "n" 'sr-tree-next-line)
(define-key sr-tree-mode-map "p" 'sr-tree-previous-line)
(define-key sr-tree-mode-map "\t" 'sr-change-window)
(define-key sr-tree-mode-map "g" 'sr-tree-refresh-branch)
(define-key sr-tree-mode-map "J" 'sr-tree-prev-subdir)
(define-key sr-tree-mode-map "j" 'sr-tree-build)
(define-key sr-tree-mode-map "f" 'sr-tree-advertised-find-file)
(define-key sr-tree-mode-map "v" 'sr-tree-advertised-find-file-other)
(define-key sr-tree-mode-map "\M-a" 'sr-tree-beginning-of-buffer)
(define-key sr-tree-mode-map "\M-e" 'sr-tree-end-of-buffer)
(define-key sr-tree-mode-map "\C-s" 'sr-tree-isearch-forward)
(define-key sr-tree-mode-map "\C-r" 'sr-tree-isearch-backward)
(define-key sr-tree-mode-map "\C-c\C-c" 'sr-tree-dismiss)
(define-key sr-tree-mode-map "\C-cf" 'sr-tree-focus-branch)
(define-key sr-tree-mode-map "\C-cu" 'sr-tree-unfocus-branch)
(define-key sr-tree-mode-map "\C-c\C-m" 'sr-tree-explode-branch)

(define-key sr-tree-mode-map "C" 'sr-tree-do-copy)
(define-key sr-tree-mode-map "K" 'sr-tree-do-clone)
(define-key sr-tree-mode-map "R" 'sr-tree-do-rename)
(define-key sr-tree-mode-map "D" 'sr-tree-do-delete)
(define-key sr-tree-mode-map "S" 'sr-tree-do-symlink)
(define-key sr-tree-mode-map "Y" 'sr-tree-do-relsymlink)
(define-key sr-tree-mode-map "y" 'sr-tree-show-files-info)

(define-key sr-tree-mode-map [up] 'sr-tree-previous-line)
(define-key sr-tree-mode-map [down] 'sr-tree-next-line)
(define-key sr-tree-mode-map [left] 'sr-tree-collapse-branch)
(define-key sr-tree-mode-map [right] 'sr-tree-open-branch)
(define-key sr-tree-mode-map [next] 'sr-tree-scroll-up)
(define-key sr-tree-mode-map [prior] 'sr-tree-scroll-down)
(define-key sr-tree-mode-map [backspace] 'sr-tree-collapse-branch)
(define-key sr-tree-mode-map [(control return)] 'sr-tree-explode-branch)
(define-key sr-tree-mode-map [(shift return)] 'sr-tree-focus-branch)
(define-key sr-tree-mode-map [(meta return)] 'sr-tree-unfocus-branch)
(define-key sr-tree-mode-map [(control right)] 'sr-tree-explode-branch)
(define-key sr-tree-mode-map [(shift right)] 'sr-tree-focus-branch)
(define-key sr-tree-mode-map [(control left)] 'sr-tree-collapse-branch)
(define-key sr-tree-mode-map [(shift left)] 'sr-tree-unfocus-branch)

(define-key sr-tree-mode-map "\C-q" 'sr-tree-sync)

(dotimes (n 10)
  (define-key sr-tree-mode-map (number-to-string n) 'digit-argument))

(mapc (lambda (x)
        (define-key sr-tree-mode-map x nil))
      '("Q" "F" "A" "k" "s" "\C-c/"))

(define-key tree-widget-button-keymap "\t" 'sr-tree-widget-forward)
(define-key tree-widget-button-keymap "\C-m" 'sr-tree-widget-button-press)
(define-key tree-widget-button-keymap [down-mouse-1] 'sr-tree-widget-button-click)
(define-key tree-widget-button-keymap [mouse-1] 'tree-widget-button-click)
(define-key tree-widget-button-keymap [double-mouse-1] 'sr-tree-mouse-advertised-find-file)
(define-key tree-widget-button-keymap [C-mouse-1] 'sr-tree-mouse-explode-branch)

(define-key sr-tree-mode-map [mouse-1] 'sr-tree-mouse-toggle-branch)
(define-key sr-tree-mode-map [mouse-2] (lambda ()
                                         (interactive)
                                         (call-interactively 'mouse-set-point)
                                         (sr-advertised-find-file)
                                         (sr-tree-update-cursor)))
(define-key sr-tree-mode-map [double-mouse-1] 'sr-tree-mouse-advertised-find-file)
(define-key sr-tree-mode-map [S-mouse-1] 'sr-tree-mouse-focus-branch)
(define-key sr-tree-mode-map [M-mouse-1] 'sr-tree-mouse-unfocus-branch)
(define-key sr-tree-mode-map [C-mouse-1] 'sr-tree-mouse-explode-branch)

(define-key sr-tree-mode-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key sr-tree-mode-map (kbd "<M-down-mouse-1>") 'ignore)
(define-key sr-tree-mode-map (kbd "<C-down-mouse-1>") 'ignore)

;;; ============================================================================
;;; Sunrise Core integration:

(defun sr-tree-view (&optional desktop-mode)
  "Display a tree of entries in current directory."
  (interactive)
  (let ((default-directory (or (dired-default-directory) default-directory))
        (omit dired-omit-mode))
    (sr-save-aspect
     (if desktop-mode
         (switch-to-buffer (generate-new-buffer "Sunrise Tree"))
       (sr-alternate-buffer
        (switch-to-buffer (generate-new-buffer "Sunrise Tree"))))
     (sr-tree-mode))
    (if (fboundp 'sr-modeline-setup)
        (sr-modeline-setup))
    (if (fboundp 'sr-tabs-engage)
        (sr-tabs-engage))
    (sr-force-passive-highlight)))

(defun sr-tree-mouse-view (e)
  (interactive "e")
  (sr-mouse-change-window e)
  (sr-tree-view))

(defun sr-tree-mouse-dismiss (e)
  (interactive "e")
  (sr-mouse-change-window e)
  (sr-tree-dismiss))

(define-key sr-mode-map "\C-t " 'sr-tree-view)
(define-key sr-mode-map "\C-t\C-m" 'sr-tree-view)
(define-key sr-tree-mode-map "\C-t " 'sr-tree-dismiss)
(define-key sr-tree-mode-map "\C-t\C-m" 'sr-tree-dismiss)
(define-key sr-mode-map [(shift meta down)] 'sr-tree-view)
(define-key sr-tree-mode-map [(shift meta down)] 'sr-tree-dismiss)
(define-key sr-mode-map (kbd "<M-S-down-mouse-1>") 'sr-tree-mouse-view)
(define-key sr-tree-mode-map (kbd "<M-S-down-mouse-1>") 'sr-tree-mouse-dismiss)

;;; ============================================================================
;;; Sunrise Buttons integration:

(defvar sr-tree-button-commands
  '((sr-do-copy             . sr-tree-do-copy)
    (sr-do-clone            . sr-tree-do-clone)
    (sr-do-symlink          . sr-tree-do-symlink)
    (sr-do-relsymlink       . sr-tree-do-relsymlink)
    (sr-do-rename           . sr-tree-do-rename)
    (sr-do-delete           . sr-tree-do-delete)
    (sr-change-window       . sr-change-window)
    (sr-synchronize-panes   . sr-synchronize-panes)
    (sr-sync                . sr-sync)
    (sr-beginning-of-buffer . sr-tree-beginning-of-buffer)
    (sr-end-of-buffer       . sr-tree-end-of-buffer)
    (sr-term                . sr-term)
    (sr-describe-mode       . sr-describe-mode)
    (sr-transpose-panes     . sr-transpose-panes)))

(defun sr-tree-button-command (command)
  (let ((translation (cdr (assq command sr-tree-button-commands))))
    (if translation
        (call-interactively translation)
      (sr-tree-dismiss)
      (call-interactively command))))

;;; ============================================================================
;;; Desktop support:

(defun sr-tree-desktop-save-buffer (desktop-dirname)
  "Returns the additional data for saving a sunrise tree buffer to a desktop
  file."
  (append `((root . ,(widget-get sr-tree-root :path))
            (open-paths . ,sr-tree-open-paths)
            (cursor ,sr-tree-cursor)
            (omit-mode . ,dired-omit-mode))
          (if (eq (current-buffer) sr-left-buffer) '((left . t)))
          (if (eq (current-buffer) sr-right-buffer) '((right . t)))
          (if (fboundp 'sr-tabs-desktop-save-buffer)
              (sr-tabs-desktop-save-buffer desktop-dirname))))

(defun sr-tree-desktop-restore-buffer (desktop-buffer-file-name
                                       desktop-buffer-name
                                       desktop-buffer-misc)
  "Restores a sunrise tree buffer from a description in a desktop file."
  (sr-tree-view t)
  (setq sr-tree-open-paths (cdr (assoc 'open-paths desktop-buffer-misc)))
  (setq dired-omit-mode (cdr (assoc 'omit-mode desktop-buffer-misc)))
  (setq sr-tree-cursor (cadr (assoc 'cursor desktop-buffer-misc)))
  (sr-tree-build (cdr (assoc 'root desktop-buffer-misc)))
  (sr-tree-update-cursor)
  (mapc (lambda (side)
          (when (cdr (assoc side desktop-buffer-misc))
            (set (sr-symbol side 'buffer) (current-buffer))
            (set (sr-symbol side 'directory) (cdr sr-tree-cursor))))
        '(left right))
  (if (fboundp 'sr-tabs-desktop-restore-buffer)
      (sr-tabs-desktop-restore-buffer desktop-buffer-file-name
                                      desktop-buffer-name
                                      desktop-buffer-misc)))

(add-to-list 'desktop-buffer-mode-handlers
             '(sr-tree-mode . sr-tree-desktop-restore-buffer)) 

(provide 'sunrise-x-tree)

;;; sunrise-x-tree.el ends here
