;;;; init.el -*- lexical-binding: t; -*-

;; "I am Gandalf, and Gandalf means me"
(setq user-full-name "Alex Combas"
      user-email-address "alex@flyingcastle.net")

(setq auth-sources '("~/.authinfo.gpg"))

(setq inhibit-startup-message t ; turn off emacs startup file
      ;; as opposed to an audio-beep
      visible-bell t
      ;;save clipboard strings to kill ring before replacing
      save-interprogram-paste-before-kill t
      ;; C-h a search for command using all apropos features
      apropos-do-all t
      ;; middle button paste to point in buffer
      mouse-yank-at-point t
      ;; single .[SPC] ends a sentence instead of .[SPC][SPC]
      sentence-end-double-space nil
      ;; prevent outdated source code from being loaded
      load-prefer-newer t
      ;; performance enhancers
      read-process-output-max (* 1024 1024)
      ;; turn off paren delay
      show-paren-delay 0
      ;; as opposed to doing it another way
      backup-by-copying t
      ;; do not create lockfiles, allows simultaneous editing
      create-lockfiles nil
      ;; kill the entire line, including /n
      kill-whole-line t
      ;; make display 100 characters wide
      set-fill-column 100
      ;; use python3
      python-shell-interpreter "ipython3"
      ;; needed for ipython to run properly
      python-shell-interpreter-args "--simple-prompt -i"
      ;; use ssh
      tramp-default-method "ssh"
      ;; set sbcl for common lisp
      inferior-lisp-program (executable-find "sbcl")
      ;; note: this is the linux trash, not the windows trash
      delete-by-moving-to-trash t
)

;; Keep custom settings separate
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; remove pointless ui features
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(pixel-scroll-precision-mode)
(display-time-mode 1)
(show-paren-mode)

;; set initial emacs window size
(when window-system
  (set-frame-size (selected-frame) 100 45))

;; turn on fringes, left 10, right 0
(set-fringe-mode '(10 . 0))

;; persistant save place in file between sessions
(save-place-mode 1)

;; default fonts
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; I prefer y or n, to yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Doom-emacs cons trick
;; disable gcs during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; turn on gcs after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; all defun inside init.el start with ac
(defun ac-defer-garbage-collection-h ()
  ;; Set gcs value to highest fixnum value
  (setq gc-cons-threshold most-positive-fixnum))

(defun ac-restore-garbage-collection-h ()
  ;; Set gcs to 16mb
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216
                          gc-cons-percentage 0.1))))

;; turn off gcs when entering minibuffer
(add-hook 'minibuffer-setup-hook #'ac-defer-garbage-collection-h)

;; turn on gcs after exiting minibuffer
(add-hook 'minibuffer-exit-hook #'ac-restore-garbage-collection-h)

(defun ac-display-startup-time ()
  ;; Display load-time and gcs amount in scratch buffer
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; set initial screens
(setq initial-scratch-message (ac-display-startup-time))
(add-hook 'after-init-hook 'org-agenda-list)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default nuke-trailing-whitespace-p t)

;; Cleanup whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook
          (lambda() (delete-trailing-whitespace)))

(defun ac-remove-dos-eol ()
  "Do not show ^M in files containing mixed line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'ac-remove-dos-eol)

;; Set all-the-things to UTF-8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRGIN))

;; Setup Use Package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Better history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save/" t)))

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/"))
      backup-by-copying 1
      delete-old-versions -1
      version-control t
      vc-make-backup-files t)

;; Turn on line/column numbers everywhere
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; then turn them off for some modes
(dolist (mode '(org-mode-hook
                dired-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurance of CHAR." t)

(defun ac-insert-date()
  ;; Make inserting the date easier C-c i d
  (interactive)
  (insert (format-time-string "%x")))

(defun ac-insert-time()
  ;; Make inserting the time easier C-c i t
  (interactive)
  (insert (format-time-string "%X")))

(defun ac-reload-init ()
  ;; Make reloading ~/.emacs.d/init.el easier <f6>
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; hot keys
(global-set-key (kbd "<f6>") 'ac-reload-init)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c l") 'org-store-line)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-x w") 'elfeed)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c i d") 'ac-insert-date)
(global-set-key (kbd "C-c i t") 'ac-insert-time)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-material-dark t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package langtool
  :ensure t
  :bind
  (("C-c v" . 'langtool-check) ; starts the checker
   ("C-c c" . 'langtool-correct-buffer) ; corrects the buffer
   ("C-c x" . 'langtool-check-done)) ; ends the checker
  :config
  (setq langtool-language-tool-jar
        "~/Source/LanguageTool-5.6/languagetool-commandline.jar")
  (setq langtool-default-language "en-CA")
  (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8")))

;; Turn on company-mode globally, then off for some modes
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-backends '((company-capf company-dabbrev-code company-ispell)))
  ;; Show 5 completions max, default 10
  (setq company-tooltip-limit 5)
  (setq company-tooltip-flip-when-above t)
  (setq company-global-modes '(not erc-mode
                                   message-mode
                                   eshell-mode
                                   term-mode
                                   dired-mode
                                   org-mode)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; nov ebook reader
(use-package nov
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-text-width 100)
  (setq visual-fill-column-center-text t)
  :hook
  (nov-mode . visual-line-mode)
  (nov-mode . visual-fill-column-mode)
  )

;; elfeed news reader feeds
(setq elfeed-feeds
      '(("https://planet.emacslife.com/atom.xml" planet emacs)
        ("https://planet.lisp.org/rss20.xml" planet lisp)
        ("https://planet.gnu.org/rss20.xml" planet gnu)
        ("https://planet.ubuntu.com/rss20.xml" planet ubuntu)
        ("news://comp.lang.lisp" lisp)
        ("https://nedroid.com/feed/" webcomic)
        ("https://xkcd.com/rss.xml" webcomic)
        ("https://smbc-comics.com/rss" webcomic)
        ("https://www.qwantz.com/rssfeed.php" webcomic)))

;; info colors
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; for editing .yml files
(use-package yaml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; fixed-with for code, variable-width for text
(use-package mixed-pitch
  :ensure t
  :hook (text-mode . mixed-pitch-mode))

;; use dired-single for dired
(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   (:map dired-mode-map ("f" . dired-single-buffer))
   (:map dired-mode-map ("b" . dired-single-up-directory)))
  :custom ((dired-listing-switches "-agho
  --group-directories-first")))

;; use ! to call xdg-open on any file in dired
(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-guess-shell-alist-user '(("" "xdg-open"))))

;; use H to hide dot files when in dired
(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("H" . dired-hide-dotfiles-mode)))

;; nicer icons for dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; loads when dired loads, remaps to use dired-single
(defun ac-dired-init ()
  (interactive)
  (define-key dired-mode-map [remap dired-find-file]
              'dired-single-buffer)
  (define-key dired-mode-map [remap
                              dired-mousej-find-file-other-window]
              'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory] ' dired-single-up-directory))

;; turn dired into a single buffer applicaiton
(use-package dired-single
  :ensure t
  :hook
  (dired-mode . ac-dired-init))

;; Org mode configuration start
(use-package org
  :ensure t
  :hook
  (org-mode . font-lock-mode)
  (org-mode . visual-line-mode)
  :config
  (setq org-cycle-separator-lines -1)
  (setq org-startup-indented t)
  (setq org-startup-folded nil)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-use-effective-time t)
  (setq org-agenda-files '("~/Org/Tasks.org"
                           "~/Org/Habits.org"))
  (setq org-tag-persistent-alist '(("@errand" . ?e)
                                   ("@home" . ?h)
                                   ("@work" . ?w)
                                   ("@note" . ?n)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-refile-targets '(("Archive.org" :maxlevel . 1)
                             ("Tasks.org" :maxlevel . 1)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("IN-PROGRESS" :foreground "orange" :weight bold)
                ("DONE" :foreground "green" :weight bold)
                ("CANCELED" :foreground "green" :weight bold)))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

(use-package visual-fill-column
  :ensure t
  :config
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-enable-sensible-window-split t)
  (setq-default visual-fill-column-fringes-outside-margins t)
  :hook
  (org-mode . visual-fill-column-mode)
  (visual-line-mode . visual-fill-column-mode))

(use-package org-variable-pitch
  :ensure t
  :hook (org-mode . variable-pitch-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sqlite . t)
     (sql . t)
     (latex . t)
     (shell . t))))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

;; use ALT-o when in the minibuffer for extra commands
(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'councel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :config (all-the-icons-ivy-rich-mode 1))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package all-the-icons
  :ensure t)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-callable)
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap describe-key] . helpful-key)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-popup
  :ensure t
  :after magit)

(use-package forge
  :ensure t
  :after magit)

(use-package no-littering
  :ensure t)

(use-package writeroom-mode
  :ensure t)

(use-package palimpsest
  :ensure t
  :hook (text-mode . palimpsest-mode))

(use-package treemacs
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind
  (("M-o" . ace-window)
   ("M-O" . ace-swap-window)))

(use-package eterm-256color
  :ensure t
  :hook (term-mode . eterm-256color-mode))

(defun ac-configure-eshell()
  ;; Personal Eshell setttings
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions
               'eshell-truncate-buffer)
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :ensure t
  :after eshell)

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . ac-configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'powerline))

(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-fancy slime-company))
  (setq slime-lisp-implementation '((sbcl ("sbcl")))
        slime-default-lisp 'sbcl
        slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
  (erc-autojoin-timing 'indent)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  (erc-interpret-mirc-color t)
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (add-to-list 'erc-modules 'image)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package erc-hl-nicks
  :ensure t
  :after erc)

(use-package erc-image
  :ensure t
  :after erc)

(use-package emojify
  :ensure t
  :after erc
  :config (global-emojify-mode))
