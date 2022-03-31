;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "name"
      user-mail-address "***REMOVED***")

(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))

(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

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

;; (display-pixel-height) error in daemon mode
                                        ;(add-to-list 'face-font-rescale-alist '("agave Nerd Font" . 1.2))
                                        ;(add-to-list 'face-font-rescale-alist '("Sarasa Gothic SC" . 1.2))

(setq +my/scale-factor
      (/ (string-to-number (shell-command-to-string "xdpyinfo | grep dimension | awk '{print $2}' | cut -d'x' -f2")) 720.0))

(setq +my/is-laptop
      (eq (shell-command "ls /sys/class/power_supply | grep BAT &>/dev/null") 0))

(defun +my/font-size(size)
     (ceiling (* (if +my/is-laptop 1.15 1) size +my/scale-factor)))

(setq doom-font (font-spec :family "UbuntuMono Nerd Font" :size (+my/font-size 14))
      ;; big font mode resize serif-font and variable-pitch-font also
      doom-big-font (font-spec :family "Mononoki Nerd Font Mono" :size (+my/font-size 17))
      doom-serif-font (font-spec :family "Source Serif Pro" :size (+my/font-size 13))
      doom-unicode-font (font-spec :family "FuraCode Nerd Font" :size (+my/font-size 11))
      doom-variable-pitch-font (font-spec :family "Sarasa Gothic SC" :size (+my/font-size 9)))

;; TODO resize cjk font with =C +=
(defun +my/cjk-font(font-size)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Sarasa Gothic SC" :size (+my/font-size font-size)))))

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "UbuntuMono Nerd Font" (+my/font-size 14))) ;; 11 13 17 19 23
        (set-face-attribute 'mode-line nil :family "Comic Shanns" :height (+ 80 (+my/font-size 20)))
        (set-face-attribute 'mode-line-inactive nil :family "Comic Shanns" :height (+ 80 (+my/font-size 20)))
        (+my/cjk-font 11))))


(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

;;https://emacs.stackexchange.com/a/47092
;; DONE restore ligature after exit big font mode
(add-hook 'doom-big-font-mode-hook
          (lambda ()
            (if doom-big-font-mode
                (progn
                  (add-hook 'doom-big-font-mode-hook #'+my/better-font)
                  (+ligatures-init-fira-font-h))
              (progn
                (remove-hook 'doom-big-font-mode-hook #'+my/better-font)
                (+ligatures-init-fira-font-h)))))

(add-hook 'writeroom-mode-enable-hook (lambda () (+my/cjk-font 17)))
(add-hook 'writeroom-mode-disable-hook (lambda () (+my/cjk-font 11)))

;(setq doom-theme 'doom-palenight)
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)  ; if nil, italics is universally disabled

  (load-theme 'doom-dracula t)

  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;(doom-themes-treemacs-config)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be displayed.
  (setq doom-modeline-window-width-limit fill-column
        doom-modeline-enable-word-count t
        doom-modeline-workspace-name t)
  )
;(if (equal (display-pixel-width) 3840)
;    (custom-set-faces
;     '(mode-line ((t (:family "Comic Shanns" :height 140))))
;     '(mode-line-inactive ((t (:family "Comic Shanns" :height 140)))))
;  (custom-set-faces
;   '(mode-line ((t (:family "Comic Shanns" :height 120))))
;   '(mode-line-inactive ((t (:family "Comic Shanns" :height 120)))))
;)

;(setq doom-modeline-height 1)
;(set-face-attribute 'mode-line nil :family "Comic Shanns" :height (+ 80 (+my/font-size 20)))
;(set-face-attribute 'mode-line-inactive nil :family "Comic Shanns" :height (+ 80 (+my/font-size 20)))
;https://github.com/seagle0128/doom-modeline/issues/187
(defun +my/doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (+ 20 (+my/font-size 2))
  ;(if (equal (display-pixel-width) 3840)
  ;    26 24)
)
(advice-add #'doom-modeline--font-height :override #'+my/doom-modeline--font-height)

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9)
  (setq all-the-icons-dired-mode t)
  )

(setq display-line-numbers-type nil)

(setq show-paren-style 'expression)

(eval-after-load 'latex
  '(setq LaTeX-clean-intermediate-suffixes (delete "\\.synctex\\.gz"  LaTeX-clean-intermediate-suffixes)
         LaTeX-clean-intermediate-suffixes (append LaTeX-clean-intermediate-suffixes (list "\\.dvi" "\\.fdb_latexmk"))
         Tex-clean-confirm nil))
(use-package! math-preview
  :config
  (setq math-preview-marks '(("\\begin{equation}" . "\\end{equation}")
                        ("\\begin{equation*}" . "\\end{equation*}")
                        ("\\[" . "\\]")
                        ("\\(" . "\\)")
                        ("$$" . "$$")
                        ("$" . "$")))
  (setq math-preview-preprocess-functions '((lambda (s)
                                         (concat "{\\color{white}" s "}"))))
  )
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
(setq-default enable-local-variables t)
;(setq-default Tex-master (concat (projectile-project-root) "main.tex"))

;(defun +my/latex-mode-setup ()
;  (setq-local company-backends
;              (append '((company-dabbrev company-yasnippet company-ispell))
;                      company-backends)))
;(add-hook 'LaTeX-mode-hook '+my/latex-mode-setup)

;(defun +my/lsp-init-company-backends-h ()
;  (when (and lsp-completion-mode (not TeX-mode-p))
;    (set (make-local-variable 'company-backends)
;         (cons +lsp-company-backends
;               (remove +lsp-company-backends
;                       (remq 'company-capf company-backends))))))
;
;(advice-add #'+lsp-init-company-backends-h :override #'+my/lsp-init-company-backends-h)

(setq-hook! 'LaTeX-mode-hook +lsp-company-backends '(:separate company-capf company-yasnippet company-dabbrev))

(use-package! evil-tex
  :when (featurep! :editor evil +everywhere)
  :config
  (setq evil-tex-include-newlines-in-envs nil
        evil-tex-select-newlines-with-envs nil)
  )

(after! yasnippet
  :config
  ;(setq +file-templates-dir "~/.config/doom/templates/")
  (set-file-template! "/leetcode/.+\\.cpp$"
    ;:when +file-templates-in-emacs-dirs-p
    :trigger "__leetcode.cpp" :mode 'c++-mode)
  )

(use-package! org
  :init
  (setq org-directory "~/org/")
  (defvar co/org-agenda-directory (expand-file-name "agenda" org-directory))
  (defun co/org-agenda-file-paths (path)
    (if (listp path)
        (mapcar (lambda (x) (expand-file-name (concat x ".org") co/org-agenda-directory)) path)
      (expand-file-name (concat path ".org") co/org-agenda-directory)))
  ;; https://emacs.stackexchange.com/a/63793
  (defun org-copy-link-url ()
    (interactive)
    (kill-new (org-element-property :raw-link (org-element-context))))

  ;; :bind (:map org-mode-map
  ;;        :localleader
  ;;        "y" #'org-copy-link-url)
  :custom
  (org-agenda-files (co/org-agenda-file-paths '("todos" "habits" "journal")))
  (org-ellipsis "⤵")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-image-actual-width 400)
  (org-startup-with-inline-images t)
  (org-refile-targets '(("archive.org" :maxlevel . 1)))
  (org-tag-alist
   '((:startgroup)
     ;; Put mutually exclusive tags here
     (:endgroup)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("publish" . ?P)
     ("batch" . ?b)
     ("idea" . ?i)))

  :config
  (map! :map org-mode-map
        :localleader
        "y" #'org-copy-link-url)
  (load "~/.config/doom/org-capture-templates.el")
  ;;https://stackoverflow.com/a/50875921
  ;;https://github.com/daviwil/emacs-from-scratch/blob/c55d0f5e309f7ed8ffa3c00bc35c75937a5184e4/init.el
  (use-package org-habit
    :custom
    (org-habit-graph-column 60)
    :config
    (add-to-list 'org-modules 'org-habit))


  ;;(org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ;; display inline images
  (org-display-inline-images)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/org/roam")
        org-roam-v2-ack t)
  :custom
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?\n[%<%Y-%m-%d %H:%M>]\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config
  (require 'org-roam-dailies)
  )

(use-package! org-pandoc-import :after org)

(use-package! org-noter
  :after org
  :config
  (org-noter-set-doc-split-fraction '(0.75 . 0.25))
  )

(defun +my/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

; last update was 5 years ago
(use-package! wolfram-mode
  :config
  (autoload 'wolfram-mode "wolfram-mode" nil t)
  (autoload 'run-wolfram "wolfram-mode" nil t)
  (setq wolfram-program "/usr/local/bin/wolfram")
  (+my/append-to-list 'auto-mode-alist '(("\.m$" . wolfram-mode)
                                         ("\.wl$" . wolfram-mode)
                                         ("\.wls$" . wolfram-mode)
                                          ))
  (setq wolfram-path "~/.Mathematica/Applications")
  )

(after! lsp
  (add-to-list 'lsp-language-id-configuration '(wolfram-mode . "Mathematica"))

  (lsp-register-client
   (make-lsp-client :language-id 'wolfram
                    :new-connection (lsp-tcp-server-command
                                     (lambda (port)
                                       `("wolfram" ;; or "wolframscript"
                                         "-script" ;; or "-file"
                                         "~/softwares/lsp-wl/init.wls"
                                         ,(concat
                                           "--socket="
                                           (number-to-string port)
                                           ))))
                    :major-modes '(wolfram-mode)
                    :server-id 'lsp-wl
                    ))
)

(use-package! treemacs
  :when (featurep! :ui treemacs)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-width 17)
  (setq treemacs-project-follow-cleanup t)
  ;(setq treemacs-user-mode-line-format t)
        ;https://github.com/hlissner/doom-emacs/issues/1551
        ;doom-themes-treemacs-enable-variable-pitch nil

  ;https://github.com/Alexander-Miller/treemacs/issues/486
  ;; (dolist (face '(treemacs-root-face
  ;;                 treemacs-git-unmodified-face
  ;;                 treemacs-git-modified-face
  ;;                 treemacs-git-renamed-face
  ;;                 treemacs-git-ignored-face
  ;;                 treemacs-git-untracked-face
  ;;                 treemacs-git-added-face
  ;;                 treemacs-git-conflict-face
  ;;                 treemacs-directory-face
  ;;                 treemacs-directory-collapsed-face
  ;;                 treemacs-file-face
  ;;                 treemacs-tags-face))
  ;;  (set-face-attribute face nil :family "Comic Shanns" :height (+ 80 (+my/font-size 20))))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'extended))

(use-package treemacs-all-the-icons
   :after (treemacs all-the-icons))

(load "~/.config/doom/mu4e.el")

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

(use-package! elfeed
  :when (featurep! :app rss)
  :bind (:map elfeed-search-mode-map
              ("A" . bjm/elfeed-show-all)
              ;("E" . bjm/elfeed-show-emacs)
              ("m" . elfeed-toggle-star)
              ;("D" . bjm/elfeed-show-daily)
              ("q" . bjm/elfeed-save-db-and-bury))
  :config
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
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
)

(map! :leader
      (:prefix-map ("o" . "open")
       (:when (featurep! :app rss)
        :desc "elfeed"    "e" #'elfeed
        )
       ))

(use-package! elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))
  )

(use-package! elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-size 0.5)
  )

(use-package! dired
  :config
  ;https://github.com/jtbm37/all-the-icons-dired/pull/39/
  (setq all-the-icons-dired-monochrome nil)
  )

(use-package! ranger
  :when (featurep! :emacs dired +ranger)
  :after dired
  :custom
  (ranger-cleanup-eagerly t)
  (ranger-modify-header t)
  (ranger-cleanup-on-disable t)
  (ranger-return-to-ranger t)
  ; aviod noisy lsp root request when browsing
  (ranger-show-literal t)
  (ranger-excluded-extensions '("mkv" "iso" "mp4" "ipynb"))
  (ranger-max-preview-size 10)
  (ranger-dont-show-binary t)
  (ranger-footer-delay 0.2)
  (ranger-preview-delay 0.04)
  :config
  (ranger-override-dired-mode t)
  ;TODO change =ranger-pop-eshell= to vterm
  ;(setq helm-descbinds-window-style 'same-window)
  (map! :leader
        (:prefix-map ("o" . "open")
          :desc "ranger"    "r" #'ranger
          :desc "REPL"    "R" #'+eval/open-repl-other-window))
  )

;https://docs.projectile.mx/projectile/configuration.html
(use-package! projectile
  :config
  (setq projectile-file-exists-remote-cache-expire (* 10 60)
        projectile-track-known-projects-automatically nil
        projectile-auto-discover nil)
  ;(setq projectile-file-exists-local-cache-expire (* 5 60))
  )

(use-package! tldr
  :config
  (setq tldr-enabled-categories '("common" "linux" "osx" "sunos"))
  )

(use-package! vterm
  :custom
  (vterm-shell "/usr/bin/fish")
  (vterm-buffer-name-string "vterm %s")
  (vterm-kill-buffer-on-exit t)
  )

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(use-package pomm
  :commands (pomm))
