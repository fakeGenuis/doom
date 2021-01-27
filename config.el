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
                           "~/org/gtd/todo.org"
                           "~/org/gtd/projects.org"))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/gtd/inbox.org" "Inbox")
                               "* TODO %i%?")
                              ("s" "Someday" entry
                               (file+headline "~/org/gtd/inbox.org" "Someday")
                               "* HOLD %i%? \n %U")))
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

(after! ivy
  ;; Causes open buffers and recentf to be combined in ivy-switch-buffer
  (progn
    (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
          (t               . ivy-posframe-display-at-window-center)))
    (ivy-posframe-mode 1)
    )
)

(setq leetcode-prefer-language "cpp")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/Coding/leetcode")

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
