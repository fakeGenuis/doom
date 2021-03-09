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

(setq show-paren-style 'expression)
(electric-pair-mode 1)

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

(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (set-face-attribute 'mode-line nil :family "JetBrains Mono" :height 135)
  (set-face-attribute 'mode-line-inactive nil :family "JetBrains Mono" :height 135)
  (setq inhibit-compacting-font-caches t
        doom-modeline-height 1
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-buffer-encoding nil))

(use-package treemacs
  :config
  (progn
    (setq treemacs-width 17))
  ;;(treemacs-resize-icons 11)
  )

(setq leetcode-prefer-language "cpp")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/Coding/leetcode")

(use-package mu4e
  ;;:ensure nil
  :custom
  (mu4e-update-interval 300)
  (mu4e-change-filenames-when-moving t)
  (mu4e-maildir "~/.mail")
  (mu4e-get-mail-command "mbsync -c ~/.config/isync/***REMOVED***-mbsyncrc -a")
  ;;(mu4e-get-mail-command "mbsync -a")

  (mu4e-drafts-folder "/***REMOVED***/Drafts")
  (mu4e-refile-folder "/***REMOVED***/Archive")
  (mu4e-sent-folder "/***REMOVED***/Sent Items")
  (mu4e-trash-folder "/***REMOVED***/Trash")
  (mu4e-attachment-dir "~/Downloads")

  (mu4e-compose-signature-auto-include nil)
  (mu4e-maildir-shortcuts
   '(("/***REMOVED***/Inbox" . ?i)
     ("/***REMOVED***/Drafts" . ?D)
     ("/***REMOVED***/Sent Items" . ?s)
     ("/***REMOVED***/Notifications" . ?n)
     ("/***REMOVED***/Junk E-mail" . ?j)
     ("/***REMOVED***/Virus Items" . ?v)))
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t))
;; enable inline images
(setq mu4e-view-show-images t)
;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

(require 'tramp)
(add-to-list 'tramp-methods
'("yadm"
  (tramp-login-program "yadm")
  (tramp-login-args (("enter")))))
  ;;(tramp-login-env (("SHELL") ("/bin/bash")))
  ;;(tramp-remote-shell "/bin/bash")
  ;;(tramp-remote-shell-args ("-c"))))
