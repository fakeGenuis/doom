;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "name"
      user-mail-address "***REMOVED***")



(if (equal (display-pixel-width) 3840)
    (progn
      (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
      (set-face-attribute 'default t :font "JetBrains Mono-14"))
  (progn
    (add-to-list 'default-frame-alist '(font . "JetBrains Mono"))
    (set-face-attribute 'default t :font "JetBrains Mono"))
  )

;;(if (equal (display-pixel-width) 3840)
;;    (setq doom-font (font-spec :family "Inconsolata" :size 48)
;;          doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 40))
;;  (setq doom-font (font-spec :family "Inconsolata")
;;        doom-variable-pitch-font (font-spec :family "Inconsolata")))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(setq doom-theme 'doom-snazzy)

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.75))

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

(after! ivy
  ;; Causes open buffers and recentf to be combined in ivy-switch-buffer
  (setq ivy-posframe-display-functions-alist
      '((complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
        (t               . ivy-posframe-display-at-window-center)))
)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
