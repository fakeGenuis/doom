;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "name"
      user-mail-address "***REMOVED***")

;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Toggle transparency"    "T" #'toggle-transparency
       ))

(if (equal (display-pixel-width) 3840)
    (setq doom-font (font-spec :family "agave Nerd Font" :size 43)
          doom-big-font (font-spec :family "mononoki Nerd Font" :size 50)
          doom-unicode-font (font-spec :family "FuraCode Nerd Font" :size 29)
          doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 34))
  (if (equal (display-pixel-height) 1600)
      (setq doom-font (font-spec :family "agave Nerd Font" :size 36)
            doom-big-font (font-spec :family "mononoki Nerd Font" :size 48)
            doom-unicode-font (font-spec :family "mononoki Nerd Font" :size 24)
            doom-variable-pitch-font (font-spec :family "WenQuanYi Micro Hei"))
      (setq doom-font (font-spec :family "agave Nerd Font" :size 32)
            doom-big-font (font-spec :family "mononoki Nerd Font" :size 48)
            doom-unicode-font (font-spec :family "mononoki Nerd Font" :size 24)
            doom-variable-pitch-font (font-spec :family "WenQuanYi Micro Hei" :size 26))
      )
    )

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "agave Nerd Font" 43)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Gothic SC" :size 34)))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp) (equal (display-pixel-width) 3840))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

;(setq doom-theme 'doom-palenight)
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        )
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be displayed.
  (setq doom-modeline-window-width-limit fill-column
        doom-modeline-enable-word-count t
        )

  )
(if (equal (display-pixel-width) 3840)
    (custom-set-faces
     '(mode-line ((t (:family "Comic Shanns" :height 140))))
     '(mode-line-inactive ((t (:family "Comic Shanns" :height 140)))))
  (custom-set-faces
   '(mode-line ((t (:family "Comic Shanns" :height 120))))
   '(mode-line-inactive ((t (:family "Comic Shanns" :height 120)))))
)
;https://github.com/seagle0128/doom-modeline/issues/187
(defun my/doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (if (equal (display-pixel-width) 3840)
      26 24))
(advice-add #'doom-modeline--font-height :override #'my/doom-modeline--font-height)

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9))

(setq display-line-numbers-type nil)

(setq show-paren-style 'expression)

(after! yasnippet
  :config
  ;(setq +file-templates-dir "~/.config/doom/templates/")
  (set-file-template! "/leetcode/.+\\.cpp$"
    ;:when +file-templates-in-emacs-dirs-p
    :trigger "__leetcode.cpp" :mode 'c++-mode)
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
(setq-default enable-local-variables t)
;(setq-default Tex-master (concat (projectile-project-root) "main.tex"))

(use-package! evil-tex
  :when (featurep! :editor evil +everywhere)
  :config
  (setq evil-tex-include-newlines-in-envs nil
        evil-tex-select-newlines-with-envs nil)
  )

(use-package org
  :init
  (setq org-directory "~/org/")
  (defvar co/org-agenda-directory (expand-file-name "agenda" org-directory))
  (defun co/org-agenda-file-paths (path)
    (if (listp path)
        (mapcar (lambda (x) (expand-file-name (concat x ".org") co/org-agenda-directory)) path)
      (expand-file-name (concat path ".org") co/org-agenda-directory)))

  (setq org-agenda-files (co/org-agenda-file-paths '("todo" "habits" "journal")))

  :config
  (setq org-ellipsis " ▾"
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-image-actual-width 400
        org-startup-with-inline-images t
        org-refile-targets '(("archive.org" :maxlevel . 1)))
  ;(org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ; display inline images
  (org-display-inline-images)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("publish" . ?P)
       ("batch" . ?b)
       ("idea" . ?i)))
  ;https://stackoverflow.com/a/50875921
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp ,(co/org-agenda-file-paths "todos") "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("tc" "Task from note" entry (file+olp ,(co/org-agenda-file-paths "todos") "Inbox")
           "* TODO [%a] %?\n  %U\n  %i" :empty-lines 1)
      ("ts" "Someday" entry (file+olp ,(co/org-agenda-file-paths "todos") "Someday")
           "* HOLD %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("tr" "Readings" entry (file+olp ,(co/org-agenda-file-paths "todos") "Readings")
           "* PROJ %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree ,(co/org-agenda-file-paths "journal"))
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline ,(co/org-agenda-file-paths "journal") "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
)

(use-package org-roam
  ;:custom
  ;(org-roam-directory (file-truename "~/org/roam"))
  :custom
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?\n[%<%Y-%m-%d %H:%M>]\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (require 'org-roam-dailies)
  )

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
 (setq mu4e-get-mail-command "proxychains -q mbsync -c ~/.config/isync/***REMOVED***-mbsyncrc -a && proxychains -q mbsync -c ~/.config/isync/***REMOVED***-mbsyncrc -a")
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

;(require 'tramp)
(use-package! tramp
  :config
  ;(setenv "SHELL" "/bin/bash")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-args ("-c"))
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 ;(tramp-login-env (("SHELL") ("/bin/bash")))
                 ))
  (defun yadm-status ()
    (interactive)
    (magit-status "/yadm::"))
  (map! :leader
        (:prefix "g"
         :desc "yadm-status" "a" #'yadm-status)
        )
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

(use-package elfeed
  :config
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  :bind (:map elfeed-search-mode-map
              ("A" . bjm/elfeed-show-all)
              ;("E" . bjm/elfeed-show-emacs)
              ("m" . elfeed-toggle-star)
              ;("D" . bjm/elfeed-show-daily)
              ("q" . bjm/elfeed-save-db-and-bury))
  )

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))
  )
(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-size 0.5)
  )

(defun bjm/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;(evil-define-key 'normal elfeed-show-mode-map
;  (kbd "J") 'elfeed-goodies/split-show-next
;  (kbd "K") 'elfeed-goodies/split-show-prev)
;(evil-define-key 'normal elfeed-search-mode-map
;  (kbd "J") 'elfeed-goodies/split-show-next
;  (kbd "K") 'elfeed-goodies/split-show-prev)

(use-package ranger
  :config
  (ranger-override-dired-mode t)
  (setq ranger-modify-header t
        ;ranger-cleanup-eagerly t
        ranger-cleanup-on-disable t
        ranger-return-to-ranger t
        ranger-excluded-extensions '("mkv" "iso" "mp4" "ipynb")
        ranger-max-preview-size 3
        ranger-dont-show-binary t
        ranger-footer-delay 0.2
        ranger-preview-delay 0.040
        )
  ;TODO change =ranger-pop-eshell= to vterm
;(setq helm-descbinds-window-style 'same-window)
)
(use-package dired
  :config
  ;https://github.com/jtbm37/all-the-icons-dired/pull/39/
  (setq all-the-icons-dired-monochrome nil)
  )

;https://docs.projectile.mx/projectile/configuration.html
(use-package! projectile
  :config
  (setq projectile-file-exists-remote-cache-expire (* 10 60)
        projectile-track-known-projects-automatically nil
        projectile-auto-discover nil)
  )

(use-package! tldr
  :config
  (setq tldr-enabled-categories '("common" "linux" "osx" "sunos"))
  )

(use-package! vterm
  :config
  (setq vterm-shell "/usr/bin/fish"
        vterm-buffer-name-string "vterm %s"
        vterm-kill-buffer-on-exit t)
  )
;(use-package multi-vterm)
(use-package vterm-toggle
  :config

  ;; you can cd to the directory where your previous buffer file exists
  ;; after you have toggle to the vterm buffer with `vterm-toggle'.
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
  ;Switch to previous vterm buffer
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)
)
(map! :leader
      (:prefix-map ("o" . "open")
       (:when (featurep! :term vterm)
        :desc "Toggle vterm popup here"    "." #'vterm-toggle-cd
        )
       ))

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))
