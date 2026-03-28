;;; org-babel-inline-results.el -*- lexical-binding: t; -*-

(after! org
  (defun my/org-babel-show-current-src-block ()
    "Ensure the current src block is visible."
    (save-excursion
      (when-let ((begin (org-babel-where-is-src-block-head)))
        (goto-char begin)
        (when-let ((element (org-element-at-point)))
          (when (org-element-type-p element 'src-block)
            (org-fold-hide-block-toggle 'off t element))))))

  (defun my/org-babel-execute-src-block-preserve-visibility (fn &rest args)
    "Execute current src block without triggering unrelated block reveals."
    (my/org-babel-show-current-src-block)
    ;; Org fold's fragility check runs in `after-change-functions' and may
    ;; reveal folded blocks after Babel edits nearby text.  Disable only that
    ;; reveal logic while keeping the rest of execution unchanged.
    (org-fold-core-ignore-fragility-checks
      (apply fn args)))

  (defun my/org-babel-insert-result-preserve-visibility (fn &rest args)
    "Insert Babel results without expanding unrelated folded blocks."
    (my/org-babel-show-current-src-block)
    (org-fold-core-ignore-fragility-checks
      (apply fn args)))

  (defun my/org-babel-current-result-overlay ()
    "Return the hidden overlay for the current Babel result, if any."
    (let ((case-fold-search t))
      (save-excursion
        (forward-line 0)
        (when (re-search-forward org-babel-result-regexp nil t)
          (let ((start (line-end-position)))
            (seq-find
             (lambda (overlay)
               (eq (overlay-get overlay 'invisible) 'org-babel-hide-result))
             (overlays-at start)))))))

  (defun my/org-babel-show-current-result ()
    "Expand the current Babel result if it is hidden."
    (when-let ((overlay (my/org-babel-current-result-overlay)))
      (setq org-babel-hide-result-overlays
            (delq overlay org-babel-hide-result-overlays))
      (delete-overlay overlay)))

  (defun my/org-babel-hide-current-result ()
    "Hide the current Babel result."
    (let* ((start (line-end-position))
           (end (progn
                  (forward-line 1)
                  (while (looking-at org-babel-multi-line-header-regexp)
                    (forward-line 1))
                  (goto-char (1- (org-babel-result-end)))
                  (point))))
      (when (> end start)
        (let ((ov (make-overlay start end)))
          (overlay-put ov 'invisible 'org-babel-hide-result)
          (overlay-put ov 'evaporate t)
          ;; Keep hidden results searchable and auto-expand them when needed.
          (overlay-put
           ov 'isearch-open-invisible
           (lambda (overlay)
             (setq org-babel-hide-result-overlays
                   (delq overlay org-babel-hide-result-overlays))
             (delete-overlay overlay)))
          (push ov org-babel-hide-result-overlays)))))

  (defun my/org-babel-hide-result-toggle (&optional force)
    "Toggle the visibility of the current result as `#+RESULTS: [...]'.

This keeps using Org's built-in result overlay list and invisibility
spec so TAB cycling, clear operations, and result refreshes continue
to work with the normal Babel machinery.

When FORCE is symbol `off', unconditionally display the result.
Otherwise, when FORCE is non-nil, unconditionally hide the result."
    (interactive)
    (save-excursion
      (forward-line 0)
      (let ((case-fold-search t))
        (unless (re-search-forward org-babel-result-regexp nil t)
          (error "Not looking at a result line")))
      (if (my/org-babel-current-result-overlay)
          (when (or (not force) (eq force 'off))
           (my/org-babel-show-current-result))
        (when (not (eq force 'off))
          (my/org-babel-hide-current-result)))))

  (defun my/org-babel-show-result-before-execute (&rest _)
    "Expand the current result before executing the source block."
    (save-excursion
      (when-let ((result-pos (org-babel-where-is-src-block-result nil nil)))
        (goto-char result-pos)
        (my/org-babel-show-current-result))))

  (advice-add 'org-babel-hide-result-toggle :override
              #'my/org-babel-hide-result-toggle)
  (advice-add 'org-babel-execute-src-block :around
              #'my/org-babel-execute-src-block-preserve-visibility)
  (advice-add 'org-babel-insert-result :around
              #'my/org-babel-insert-result-preserve-visibility)
  (advice-add 'org-babel-execute-src-block :before
              #'my/org-babel-show-result-before-execute)

  ;; Fold results on file open, but rely on Org's own fold/show commands.
  (add-hook 'org-mode-hook #'org-babel-result-hide-all))

  ;; (map! :map org-mode-map
  ;;       :localleader
  ;;       :desc "Toggle inline result"
  ;;       "v" #'org-babel-hide-result-toggle))

(provide 'org-babel-inline-results)
