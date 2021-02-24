;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "name"
      user-mail-address "***REMOVED***")



(if (equal (display-pixel-width) 3840)
    (progn
      (add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))
      (set-face-attribute 'default t :font "Fira Code-8"))
  (progn
    (add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
    (set-face-attribute 'default t :font "JetBrains Mono-8"))
  )

(setq doom-theme 'doom-snazzy)

(setq display-line-numbers-type 'relative)

(after! org
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/gtd/inbox.org"
                           ;;"~/org/gtd/todo.org"
                           "~/org/gtd/projects.org"))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/gtd/inbox.org" "Inbox")
                               "* TODO %i%?")
                              ("s" "Someday" entry
                               (file+headline "~/org/gtd/inbox.org" "Someday")
                               "* HOLD %i%? \n %U")
                              ("r" "Readings" entry
                               (file+headline "~/org/gtd/inbox.org" "Readings")
                               "* PROJ %i%? \n %U")))
  (setq org-log-done 'time)
  ;;(setq org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w@)" "HOLD(h)" "|" "DONE(d!)" "KILL(k@)")
  ;;                          (sequence "[ ](T)" "[-](S)" "[?](W@)" "|" "[x](D)")))
  ;;(setq org-modules '(org-habit))
  )

(use-package treemacs
  :config
  (progn
    (setq treemacs-width 17))
  ;;(treemacs-resize-icons 11)
  )

;;(after! ivy
;;  ;; Causes open buffers and recentf to be combined in ivy-switch-buffer
;;  (progn
;;    (setq ivy-posframe-display-functions-alist
;;        '((complete-symbol . ivy-posframe-display-at-point)
;;          (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
;;          (t               . ivy-posframe-display-at-window-center))
;;        ivy-posframe-width (frame-width))
;;    (ivy-posframe-mode 1)
;;    )
;;)

(setq leetcode-prefer-language "cpp")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/Coding/leetcode")

(use-package mu4e
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-compose-signature-auto-include nil)
  (mu4e-drafts-folder "/***REMOVED***/Drafts")
  (mu4e-get-mail-command "mbsync -c ~/.config/isync/***REMOVED***-mbsyncrc -a")
  (mu4e-maildir "~/.mail")
  (mu4e-refile-folder "/***REMOVED***/Archive")
  (mu4e-sent-folder "/***REMOVED***/Sent Items")
  (mu4e-maildir-shortcuts
   '(("/***REMOVED***/Inbox" . ?i)
     ("/***REMOVED***/Drafts" . ?D)
     ("/***REMOVED***/Sent Items" . ?s)
     ("/***REMOVED***/Notifications" . ?n)
     ("/***REMOVED***/Junk E-mail" . ?j)
     ("/***REMOVED***/Virus Items" . ?v)))
  (mu4e-trash-folder "/***REMOVED***/Trash")
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t))
;; enable inline images
(setq mu4e-view-show-images t)
;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

(use-package mu4e-alert
    :after mu4e
    :hook ((after-init . mu4e-alert-enable-mode-line-display)
           (after-init . mu4e-alert-enable-notifications))
    :config (mu4e-alert-set-default-style 'libnotify))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
