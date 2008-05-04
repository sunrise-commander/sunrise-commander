(require 'sunrise-commander)

(defcustom sr-mirror-keep-backups t
  "Flag that indicates whether backup files are to be kept whenever the mirror
  of a read-only archive is modified and committed."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-mirror-pack-commands-alist
  '(
    ("\\.zip$" .                    "zip -r   %f *")
    ("\\.jar$" .                    "jar cvf  %f *")
    ("\\.\\(?:tar\\.gz\\|tgz\\)$" . "tar cvzf %f *")
    ("\\.tar\\.bz2$" .              "tar cvjf %f *")
   )
  "List of shell commands to repack the contents of the current mirror area into
  a compressed archive of the appropriate type. Use %f as a placeholder for  the
  name of the resulting archive."
  :group 'sunrise
  :type 'alist)

(defvar sr-mirror-home nil)

(define-key sr-mode-map "\C-c\C-b" 'sr-mirror-toggle)

(defun sr-mirror-enable ()
  "Enables  sunrise  mirror  support by setting the sr-mirror-home variable to a
  non-nil value and activating all advice necessary for mirror operations.  This
  method is called every time a new mirror area is created."
  (if sr-mirror-home
      (ignore)
    (progn
      (setq sr-mirror-home (concat sr-avfs-root "#mirror#/"))
      (ad-activate 'sr-copy-files)
      (ad-activate 'make-directory)
      (ad-activate 'save-buffer))))

(defun sr-mirror-disable ()
  "Disables   sunrise   mirror  support  by  resetting  the  sr-mirror-home  and
  deactivating all advice used in mirror operations. This method is called after
  the last mirror area in the current mirror home is closed."
  (if sr-mirror-home
      (progn
        (setq sr-mirror-home nil)
        (ad-deactivate 'sr-copy-files)
        (ad-deactivate 'make-directory)
        (ad-deactivate 'save-buffer))))

(defun sr-mirror-open ()
  "Uses  funionfs to create a writeable filesystem overlay over the AVFS virtual
  fs of the selected compressed archive and displays it in the current pane. The
  result  is  a  mirror  of  the  contents of the original archive that is fully
  writeable."
  (interactive)
  (let* ((path (dired-get-filename))
         (virtual (sr-avfs-dir path))
         (fname (file-name-nondirectory path))
         (base) (mirror) (overlay) (command))

    (if (null (assoc-default fname sr-mirror-pack-commands-alist 'string-match))
        (error (concat "Sunrise: sorry, no packer was registered for " fname)))
    (if (null virtual)
        (error (concat "Sunrise: sorry, don't know how to mirror " path)))

    (sr-mirror-enable)
    (if (not (file-exists-p sr-mirror-home))
        (make-directory sr-mirror-home))

    (setq base (sr-mirror-mangle path))
    (setq mirror (concat sr-mirror-home base))
    (setq overlay (concat sr-mirror-home "." base))
    (if (not (file-directory-p mirror))
        (progn
          (make-directory mirror)
          (make-directory overlay)
          (setq command (concat "cd ~; funionfs " overlay " " mirror " -o dirs=" virtual "=ro"))
          (shell-command-to-string command)))
    (sr-goto-dir mirror)
    t ))

(defun sr-mirror-close ()
  "Destroys  the  current mirror area by unmounting and deleting the directories
  it was built upon. Tries to automatically repack the mirror and substitute the
  original  archive  with  a  new  one  containing the modifications made to the
  mirror."
  (interactive)
  (if (null sr-mirror-home)
      (error (concat "Sunrise: sorry, can't mirror " (dired-get-filename)))

    (let ((here (dired-current-directory))
          (pos) (mirror) (overlay))
      (if (sr-overlapping-paths-p sr-mirror-home here)
          (progn
            (setq pos (string-match "\\(?:/\\|$\\)" here (length sr-mirror-home)))
            (setq mirror (substring here (length sr-mirror-home) pos))
            (setq overlay (concat "." mirror ))
            (sr-follow-file (sr-mirror-demangle mirror))
            (sr-mirror-commit mirror overlay)
            (sr-mirror-unmount mirror overlay))
        (error (concat "Sunrise: sorry, that's not a mirror area: " here)))
      
      (if (null (directory-files sr-mirror-home nil "^[^.]"))
          (sr-mirror-disable))
      
      t)))

(defun sr-mirror-commit (mirror overlay)
  "Commits  all  modifications  made  to  the  given mirror in the given overlay
  directory by replacing the original compressed archive with a  new  one  built
  with the current content of the mirror. Keeps a backup of the original archive
  if the sr-mirror-backup variable is not nil (it is by default)."
  (if (and (sr-mirror-files (concat sr-mirror-home overlay))
           (y-or-n-p "Sunrise: commit changes in mirror? "))
      (condition-case err
          (let ((repacked (sr-mirror-repack mirror))
                (target (dired-get-filename)))
            (if sr-mirror-keep-backups
                (rename-file target (concat target ".bak") 1))
            (rename-file repacked (dired-current-directory) t))
        (error
         (progn
           (setq err (second err))
           (if (not (yes-or-no-p (concat err ". OK to continue? ")))
               (error err)))))))

(defun sr-mirror-unmount (mirror overlay)
  "Unmounts  and  deletes  all directories used for mirroring a given compressed
  archive."
  (let* ((command (concat "fusermount -u " sr-mirror-home mirror))
         (err (shell-command-to-string command)))
    (if (or (null err) (string= err ""))
        (progn
          (dired-delete-file (concat sr-mirror-home mirror) 'always)
          (dired-delete-file (concat sr-mirror-home overlay) 'always)
          (sr-revert-buffer))
      (error (concat "Sunrise: Error unmounting mirror: " err)))))

(defun sr-mirror-toggle ()
  "Opens  a new mirror area or destroys the current one, depending on the actual
  context."
  (interactive)
  (let ((open-ok) (close-ok) (err-msg))
    (condition-case err1
        (setq open-ok (sr-mirror-open))
      (error (condition-case err2
                 (progn
                   (setq close-ok (sr-mirror-close))
                   (setq err-msg (second err1)))
               (error
                  (setq err-msg (second err2))) )) )
    (if (and (not open-ok) (not close-ok))
        (error err-msg))))

(defun sr-mirror-repack (mirror)
  "Tries  to repack the given mirror. On success returns a string containing the
  full path to the newly packed archive, on failure throws an error."
  (let* ((target-home (concat sr-mirror-home ".repacked/"))
         (target (replace-regexp-in-string
                  "/?$" ""
                  (car (last (split-string mirror "+")))))
         (files (directory-files (concat sr-mirror-home mirror)))
         (command (assoc-default mirror sr-mirror-pack-commands-alist 'string-match)))

    (if (null command)
        (error (concat "Sunrise: sorry, don't know how to repack " mirror)))

    (if (not (file-exists-p target-home))
        (make-directory target-home))
    (setq target (concat target-home target))
    (setq command (replace-regexp-in-string "%f" target command))
    (setq command (concat "cd " sr-mirror-home mirror "; " command))
    (shell-command-to-string command)
    target))

(defun sr-mirror-mangle (path)
  "Transforms  the  given  filesystem  path  into  a  string  that  can  be used
  internally as the name of a new mirror area."
  (if (equalp ?/ (string-to-char path))
      (setq path (substring path 1)))
  (replace-regexp-in-string
   "/" "+"
   (replace-regexp-in-string "\\+" "{+}" path)))

(defun sr-mirror-demangle (path)
  "Does  the  opposite of sr-mirror-mangle, ie. transforms the given mirror area
  name into a regular filesystem path."
  (concat "/"
          (replace-regexp-in-string
           "{\\+}" "+" (replace-regexp-in-string
                        "\\+\\([^}]\\)" "/\\1" path))))

(defun sr-mirror-files (directory)
  "Returns a list with the names of files and directories that can be considered
  as mirror modifications inside an overlay directory."
  (if (not (file-directory-p directory))
      (ignore)
    (let ((files (directory-files directory)))
      (mapcar (lambda (x) (setq files (delete x files)))
              '("." ".." "._funionfs_control~"))
      files)))

(defun sr-mirror-overlay-redir (dirname &optional force-root)
  "Analyses  the  given  directory  path  and rewrites it (if necessary) to play
  nicely with the mirror fs the given path  belongs  to.  If  the  path  is  not
  contained in a mirror fs then it is returned unmodified."
  (if (null sr-avfs-root)
      dirname
    (let ((xpdir (expand-file-name dirname))
          (mirror) (pos) (target))
      (if (sr-overlapping-paths-p sr-mirror-home xpdir)
          (progn
            (setq mirror (substring xpdir (length sr-mirror-home)))
            (setq pos (string-match "/\\|$" mirror))
            (if pos
                (progn
                  (setq target (replace-regexp-in-string "^/" "" (substring mirror pos)))
                  (setq mirror (substring mirror 0 pos))))
            (if (and target
                     (or (> (length target) 0) force-root)
                     (not (equal ?. (string-to-char mirror)))) 
                (concat sr-mirror-home "." mirror "/" target)
              dirname))
        dirname))))

;; This redirects all sr-copy operations to the right path under the overlay
;; directory:
(defadvice sr-copy-files
  (around sr-mirror-advice-sr-copy-files
          (file-path-list target-dir &optional do-overwrite))
  (let ((orig target-dir))
    (setq target-dir (sr-mirror-overlay-redir target-dir t))
    (if (> (length target-dir) (length orig))
        (make-directory target-dir))
    ad-do-it))

;; This redirects directory creation operations to the right path under the
;; overlay directory:
(defadvice make-directory
  (around sr-mirror-advice-make-directory (dirname &optional parents))
  (setq dirname (sr-mirror-overlay-redir dirname))
  (setq parents t)
  ad-do-it)

;; This creates all the subdirectories needed (and sets their permissions) in
;; order to make possible the redirection of buffer saving operations to the
;; right path under the overlay directory:
(defadvice save-buffer
  (around sr-mirror-advice-save-buffer (&optional args))
  (let* ((orig (buffer-file-name))
         (target (sr-mirror-overlay-redir orig)))
    (if (> (length target) (length orig))
        (let ((default-directory "~/")
              (target-dir (file-name-directory target)))
          (make-directory target-dir)
          (shell-command-to-string (concat dired-chmod-program " a+x " target-dir))
          (write-file target nil))
      ad-do-it)))

;; This toggles the read-only flag in all buffers opened inside a mirror area,
;; so they are always writeable by default:
(defun sr-mirror-toggle-read-only ()
  (if sr-mirror-home
      (let* ((orig (buffer-file-name))
             (target (sr-mirror-overlay-redir orig)))
        (if (> (length target) (length orig))
            (toggle-read-only -1)))))
(add-hook 'find-file-hook 'sr-mirror-toggle-read-only)

(provide 'sunrise-x-mirror)
