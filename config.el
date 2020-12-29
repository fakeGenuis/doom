;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "name"
      user-mail-address "***REMOVED***")

(if (equal (display-pixel-width) 3840)
    (setq doom-font (font-spec :family "Inconsolata" :size 48)
          doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 40))
  (setq doom-font (font-spec :family "Inconsolata")
        doom-variable-pitch-font (font-spec :family "Inconsolata")))

(setq doom-theme 'doom-snazzy)

(setq display-line-numbers-type 'relative)

(setq show-paren-style 'expression)
(electric-pair-mode 1)

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

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (setq doom-modeline-height 55)
  (setq doom-modeline-height 1.2)
  (set-face-attribute 'mode-line nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :height 130)
  )

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
