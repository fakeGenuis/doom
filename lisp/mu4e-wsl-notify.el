;;; mu4e-wsl-notify.el --- Windows notifications for mu4e -*- lexical-binding: t; -*-

;; The first query establishes a silent baseline.  Later queries notify about
;; unseen message IDs, showing up to three messages in a single toast.

(require 'json)
(require 'seq)
(require 'subr-x)

(defvar +my/mu4e-notified-ids (make-hash-table :test #'equal)
  "Message IDs already seen during this Emacs session.")
(defvar +my/mu4e-notification-initialized nil)
(defvar +my/mu4e-notification-query-process nil)

(defun +my/wsl-notify (title body)
  "Send TITLE and BODY to Windows asynchronously via PowerShell."
  (let* ((powershell (or (executable-find "powershell.exe")
                         (user-error "Windows PowerShell is unavailable")))
         (payload (base64-encode-string
                   (encode-coding-string
                    (json-serialize
                     `(:title ,title :body ,body
                       :distro ,(or (getenv "WSL_DISTRO_NAME") "")))
                    'utf-8)
                   t))
         (script (concat
                  (format "$Data = [Text.Encoding]::UTF8.GetString([Convert]::FromBase64String('%s')) | ConvertFrom-Json\n"
                          payload)
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "scripts/wsl-notify.ps1" doom-user-dir))
                    (buffer-string)))))
    (make-process
     :name "mu4e-wsl-notify"
     :buffer (get-buffer-create "*mu4e-wsl-notify*")
     :connection-type 'pipe
     :noquery t
     :command (list powershell "-NoLogo" "-NoProfile" "-NonInteractive"
                    "-WindowStyle" "Hidden" "-EncodedCommand"
                    (base64-encode-string
                     (encode-coding-string script 'utf-16le) t))
     :sentinel
     (lambda (process _event)
       (when (and (memq (process-status process) '(exit signal))
                  (/= (process-exit-status process) 0))
         (message "Windows mail notification failed; see *mu4e-wsl-notify*"))))))

(defun +my/mu4e-notification-text (text fallback)
  "Normalize whitespace in TEXT, returning FALLBACK when empty."
  (let ((text (string-trim (replace-regexp-in-string "[[:space:]]+" " " (or text "")))))
    (if (string-empty-p text) fallback text)))

(defun +my/mu4e-notify-messages (messages)
  "Notify about unseen MESSAGES, using the first result as a silent baseline."
  (let (new)
    (dolist (mail messages)
      (let ((id (+my/mu4e-notification-text
                 (plist-get mail :message-id) (plist-get mail :path))))
        (when (and id (not (gethash id +my/mu4e-notified-ids)))
          (puthash id t +my/mu4e-notified-ids)
          (push mail new))))
    (setq new (nreverse new))
    (when (and +my/mu4e-notification-initialized new)
      (let ((entries
             (mapcar
              (lambda (mail)
                (let ((from (car (plist-get mail :from))))
                  (cons (+my/mu4e-notification-text
                         (plist-get from :name)
                         (+my/mu4e-notification-text (plist-get from :email) "Unknown sender"))
                        (+my/mu4e-notification-text (plist-get mail :subject) "(no subject)"))))
              (seq-take new 3))))
        (+my/wsl-notify
         (mapconcat #'identity (delete-dups (mapcar #'car entries)) ", ")
         (if (= (length new) 1)
             (cdar entries)
           (concat
            (mapconcat (lambda (entry) (format "%s: %s" (car entry) (cdr entry)))
                       entries "\n")
            (if (> (length new) 3)
                (format "\n... and %d more" (- (length new) 3)) ""))))))
    (setq +my/mu4e-notification-initialized t)))

(defun +my/mu4e-notification-query-finished (process _event)
  "Handle PROCESS results and repeat once if a refresh arrived meanwhile."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (unwind-protect
          (condition-case err
              (pcase (cons (process-status process) (process-exit-status process))
                (`(exit . 0)
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (+my/mu4e-notify-messages
                    (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil))))
                ;; mu exits with status 2 when there are no matches.
                (`(exit . 2) (+my/mu4e-notify-messages nil))
                (_ (message "Mail notification query failed; see *mu4e-wsl-notify*")))
            (error (message "Mail notification failed: %s" (error-message-string err))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))
    (when (process-get process 'rerun)
      (+my/mu4e-wsl-notify))))

(defun +my/mu4e-wsl-notify (&optional _)
  "Find new unread messages independently of the unread-count delta."
  (if (process-live-p +my/mu4e-notification-query-process)
      (process-put +my/mu4e-notification-query-process 'rerun t)
    (when-let* ((bookmark (mu4e-bookmark-favorite))
                (query (plist-get bookmark :query)))
      (setq +my/mu4e-notification-query-process
            (make-process
             :name "mu4e-notification-query"
             :buffer (generate-new-buffer " *mu4e-notification-query*")
             :stderr (get-buffer-create "*mu4e-wsl-notify*")
             :coding 'utf-8-unix
             :connection-type 'pipe
             :noquery t
             :command
             (append (list mu4e-mu-binary "find" "--format=json2"
                           "--skip-dups" "--sortfield=date" "--reverse")
                     (when mu4e-mu-home (list "--muhome" mu4e-mu-home))
                     (list (funcall mu4e-query-rewrite-function
                                    (format "(%s) AND flag:unread" query))))
             :sentinel #'+my/mu4e-notification-query-finished)))))

(defun +my/mu4e-test-notification ()
  "Send a test Windows notification without fetching or sending mail."
  (interactive)
  (+my/wsl-notify "Example Sender"
                  "Your mail notifications are working."))

(provide 'mu4e-wsl-notify)
;;; mu4e-wsl-notify.el ends here
