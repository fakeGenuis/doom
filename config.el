;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "name"
      user-mail-address "***REMOVED***")

(use-package! doom-modeline
  ;;:ensure t
  :hook (after-init . doom-modeline-mode)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be displayed.
  (setq doom-modeline-window-width-limit fill-column
        doom-modeline-enable-word-count t
        )

  )
(if (equal (display-pixel-width) 3840)
    (custom-set-faces
     '(mode-line ((t (:family "Comic Shanns" :height 160))))
     '(mode-line-inactive ((t (:family "Comic Shanns" :height 160)))))
  (custom-set-faces
   '(mode-line ((t (:family "Comic Shanns" :height 120))))
   '(mode-line-inactive ((t (:family "Comic Shanns" :height 120)))))
)
(defun my-doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (if (equal (display-pixel-width) 3840)
      26 24))
(advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

(if (equal (display-pixel-width) 3840)
    (setq doom-font (font-spec :family "Fira Code Nerd Font Mono" :size 38)
          doom-big-font (font-spec :family "Inconsolata Go Nerd Font Mono" :size 50)
          doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 32))
  (setq doom-font (font-spec :family "Fira Code" :size 24)
        doom-variable-pitch-font (font-spec :family "Ubuntu Mono")))

(setq doom-theme 'doom-dracula)
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9))

(setq display-line-numbers-type nil)

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
  :custom
  ;;(org-src-window-setup 'split-window-right)
  ;;(setq org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w@)" "HOLD(h)" "|" "DONE(d!)" "KILL(k@)")
  ;;                          (sequence "[ ](T)" "[-](S)" "[?](W@)" "|" "[x](D)")))
  ;;(setq org-modules '(org-habit))
  (setq org-display-inline-images t
        org-image-actual-width 400
        org-startup-with-inline-images t
  )
)

(eval-after-load 'latex
  '(setq LaTeX-clean-intermediate-suffixes (delete "\\.synctex\\.gz"  LaTeX-clean-intermediate-suffixes)
         LaTeX-clean-intermediate-suffixes (append LaTeX-clean-intermediate-suffixes (list "\\.dvi" "\\.fdb_latexmk"))
         Tex-clean-confirm nil))
(use-package! math-preview
  :custom
  (math-preview-marks '(("\\begin{equation}" . "\\end{equation}")
                        ("\\begin{equation*}" . "\\end{equation*}")
                        ("\\[" . "\\]")
                        ("\\(" . "\\)")
                        ("$$" . "$$")
                        ("$" . "$")))
  (math-preview-preprocess-functions '((lambda (s)
                                         (concat "{\\color{white}" s "}"))))
  )
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)

(use-package! treemacs
  :config
  (progn
    (setq treemacs-width 17))
  ;;(treemacs-resize-icons 11)
  )
(dolist (face '(treemacs-root-face
                treemacs-directory-face
                treemacs-directory-collapsed-face
                treemacs-file-face
                treemacs-tags-face))
  (set-face-attribute face nil :family "Comic Mono" :height 140))

(setq leetcode-prefer-language "cpp")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/Coding/leetcode")

(use-package mu4e
  ;;:ensure nil
  :defer 20
  :custom
  ;;(mu4e-update-interval 300)
  (mu4e-change-filenames-when-moving t)
  (mu4e-maildir "~/.mail")
  (mu4e-attachment-dir "~/Downloads")

  ;; enable inline images
  (mu4e-view-show-images t)
  ;; every new email composition gets its own frame!
  (mu4e-compose-in-new-frame t)

  ;;(mu4e-compose-signature-auto-include nil)

  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-prefer-html t)

  ;;(setq mu4e-context-policy 'pick-first)
  ;;(setq mu4e-compose-context-policy nil)
)

(with-eval-after-load 'mu4e
 (setq mu4e-get-mail-command "all_proxy='socks5://127.0.0.1:1089' mbsync -c ~/.config/isync/***REMOVED***-mbsyncrc -c ~/.config/isync/***REMOVED***-mbsyncrc -a")
 (setq mu4e-contexts
        `(
         ,(make-mu4e-context
          :name "private"
          :enter-func (lambda () (mu4e-message "Entering context private"))
          :leave-func (lambda () (mu4e-message "Leaving context private"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) "***REMOVED***")))
          :vars '((user-mail-address . "***REMOVED***")
                  (user-full-name . "name")
                  (mu4e-sent-folder . "/***REMOVED***/[***REMOVED***]/Sent Mail")
                  (mu4e-trash-folder . "/***REMOVED***/[***REMOVED***]/Bin")
                  (mu4e-compose-signature . (concat "name\n" "From Emacs\n"))
                  (mu4e-compose-format-flowed . t)
                  (mu4e-maildir-shortcuts . ( ("/***REMOVED***/INBOX"            . ?i)
                                             ("/***REMOVED***/[***REMOVED***]/All Mail"  . ?a)
                                              ("/***REMOVED***/[***REMOVED***]/Sent Mail" . ?s)
                                              ("/***REMOVED***/[***REMOVED***]/Starred"   . ?r)
                                              ("/***REMOVED***/[***REMOVED***]/Bin"       . ?t)
                                              ("/***REMOVED***/[***REMOVED***]/Spam"   . ?v)
                                              ))))
         ,(make-mu4e-context
          :name "work"
          :enter-func (lambda () (mu4e-message "Entering context work"))
          :leave-func (lambda () (mu4e-message "Leaving context work"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) "***REMOVED***")))
          :vars '((user-mail-address . "***REMOVED***")
                  (user-full-name . "name")
                  (mu4e-drafts-folder . "/***REMOVED***/Drafts")
                  (mu4e-refile-folder . "/***REMOVED***/Archive")
                  (mu4e-sent-folder . "/***REMOVED***/Sent Items")
                  (mu4e-trash-folder . "/***REMOVED***/Trash")
                  (mu4e-compose-signature . (concat "name\n" "From Emacs\n"))
                  (mu4e-compose-format-flowed . t)
                  (mu4e-maildir-shortcuts . ( ("/***REMOVED***/Inbox"            . ?i)
                                              ("/***REMOVED***/Drafts" . ?D)
                                              ("/***REMOVED***/Sent Items" . ?s)
                                              ("/***REMOVED***/Notifications" . ?n)
                                              ("/***REMOVED***/Junk E-mail" . ?j)
                                              ))))
         ))
 )

(require 'tramp)
(use-package! tramp
  :config
  (setenv "SHELL" "/bin/bash")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/bash")))
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-args ("-c"))))
  )

(after! keycast
  (define-minor-mode keycast-mode
    ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update))))
(add-to-list 'global-mode-string '("" mode-line-keycast))
(keycast-mode) ;; or run keycast-mode by demand

(use-package! vterm
  :config
  (setq vterm-shell "/usr/bin/fish")
  )
