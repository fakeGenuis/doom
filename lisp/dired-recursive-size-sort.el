;;; dired-recursive-size-sort.el -*- lexical-binding: t; -*-

;; `ls --sort=size' compares the size of the directory *file* (a few KB on
;; ext4/btrfs), so a folder full of videos sorts next to an empty one.  Dired
;; sorts entirely through `ls' and has no Lisp hook to take over, so once it
;; has inserted a listing, measure every entry with `du' and reorder the lines
;; with the built-in `sort-subr' -- the primitive `sort-lines' is built on.
;;
;; Loaded lazily from `config.org' when the first Dired buffer appears; the
;; hook below then only acts on buffers whose listing switches ask for a size
;; sort, i.e. Dirvish's `s'/`S' quicksort keys and Dired's own `C-u s'.

;;; Code:

(require 'dired)

(defun +my/dired-sort-by-recursive-size ()
  "Sort the current Dired buffer by the disk usage of each entry.
Directories are measured with `du', so they sort by the size of their
whole tree instead of by the size of the directory file itself.  Acts
only when the listing switches ask for a size sort, so Dirvish's `s'/`S'
quicksort keys and Dired's own `C-u s' drive it."
  (when (and (stringp dired-directory) (not dired-sort-inhibit)
             (not (or (cdr dired-subdir-alist) (file-remote-p default-directory)))
             (string-match-p "\\(?:\\`\\| \\)-[^- ]*S\\|--sort=size"
                             dired-actual-switches))
    (let* ((switches dired-actual-switches)
           (rev (string-match-p "\\(?:\\`\\| \\)-[^- ]*r\\|--reverse" switches))
           (dirs-first (string-match-p "--group-directories-first" switches))
           (beg (save-excursion
                  (goto-char (or (next-single-property-change (point-min) 'dired-filename)
                                 (point-max)))
                  (line-beginning-position)))
           (names (save-excursion
                    (goto-char beg)
                    (let (names)
                      (while (not (eobp))
                        (when-let* ((name (ignore-errors (dired-get-filename nil t))))
                          (push (directory-file-name name) names))
                        (forward-line 1))
                      names)))
           (usage (when names ; `du' prints one "SIZE<TAB>FILE" line per argument
                    (mapcar (lambda (line)
                              (let* ((fields (split-string line "\t"))
                                     (file (cadr fields)))
                                (cons file (car fields))))
                            (apply #'process-lines-handling-status
                                   "du" #'ignore "-sb" "--" names))))
           (inhibit-read-only t))
      (save-restriction
        (narrow-to-region beg (point-max))
        (goto-char (point-min))
        (sort-subr nil #'forward-line #'end-of-line
                   (lambda ()            ; key: [DIRECTORY? BYTES]
                     (let ((name (ignore-errors (dired-get-filename nil t))))
                       (vector (if (and dirs-first
                                        (not (and name (file-directory-p name))))
                                   1 0)
                               (string-to-number
                                (or (cdr (assoc (and name (directory-file-name name)) usage))
                                    "")))))
                   nil
                   (lambda (a b)         ; biggest first, unless --reverse
                     (if (= (aref a 0) (aref b 0))
                         (if rev (< (aref a 1) (aref b 1))
                           (> (aref a 1) (aref b 1)))
                       (< (aref a 0) (aref b 0)))))))))

(add-hook 'dired-after-readin-hook #'+my/dired-sort-by-recursive-size)

(provide 'dired-recursive-size-sort)
;;; dired-recursive-size-sort.el ends here
