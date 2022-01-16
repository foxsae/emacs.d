;;;; init.el -*- lexical-binding: t; -*-

;; Keep custom settings separate
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(setq user-full-name "Alex Combas"
      user-email-address "alex@flyingcastle.net")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

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

;; remove pointless ui features
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(pixel-scroll-precision-mode)
(display-time-mode 1)

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

;; use Doom-emacs cons improvements for speed-up
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after
  ;; will enjoy the benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216
                          gc-cons-percentage 0.1))))

;; turn off garbage collection while in the mini-buffer
;; but restore it afterwards
(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; Display startup time and gcs in initial scratch msg
(defun m/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(setq initial-scratch-message (m/display-startup-time))

(setq-default indicate-buffer-boundaries 'left)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default nuke-trailing-whitespace-p t)

;; Cleanup whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook
          (lambda() (delete-trailing-whitespace)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
  ;;(package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; M-x auto-package-update-now
(use-package auto-package-update
  :custom
  (auto-package-update-interval 28)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))

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

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f1>") 'vterm)
(global-set-key (kbd "C-c l") 'org-store-line)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-x w") 'elfeed)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))(
global-set-key (kbd "<f6>") 'reload-init)

;; Turn on company-mode globally, then off for some modes
(add-hook 'after-init-hook 'global-company-mode)
;; minimal backends
(setq company-backends '((company-capf company-dabbrev-code company-ispell)))
;; Show 5 completions max, default 10
(setq company-tooltip-limit 5)
(setq company-tooltip-flip-when-above t)
(setq company-global-modes '(not erc-mode
                                 message-mode
                                 eshell-mode
                                 vterm-mode
                                 term-mode
                                 dired-mode
                                 org-mode))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; fixed-with for code, variable-width for text
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(use-package dired
  :ensure nil
  :init
  :bind (("C-x C-j" . dired-jump)
         (:map dired-mode-map ("f" . dired-single-buffer))
         (:map dired-mode-map ("b" . dired-single-up-directory)))
  :custom
  ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("H" . dired-hide-dotfiles-mode)))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-single
  :ensure nil
  :bind
  (([remap dired-find-file] . dired-single-buffer)
   ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
   ([remap dired-up-directory] . dired-single-up-directory)))

;; Org mode configuration start
(use-package org
  :hook
  (org-mode . flyspell-mode)
  (org-mode . font-lock-mode)
  (org-mode . visual-line-mode))

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
              ("CANCELED" :foreground "green" :weight bold))))


(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill
  :init
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(use-package org-variable-pitch
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

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'councel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package lsp-ivy)

(use-package which-key
  :diminish which-key-mode
  :config (which-key-setup-side-window-right-bottom)
  :init (which-key-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(show-paren-mode)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-lsp t))

(use-package doom-themes
  :init
  (load-theme 'doom-material-dark t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package all-the-icons)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-callable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-popup
  :after magit)

(use-package forge
  :after magit)

(use-package multiple-cursors
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package no-littering)

(use-package writeroom-mode)

(use-package palimpsest)
(add-hook 'text-mode-hook 'palimpsest-mode)

(use-package dap-mode
  :config
  (require 'dap-python))

(use-package eldoc
  :diminish eldoc-mode
  :hook (prog-mode . eldoc-mode))

(use-package treemacs)
(use-package lsp-treemacs)

(use-package ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window)))

(use-package python-mode
  :ensure nil
  :config
  (require 'dap-python)
  :custom
  (dap-python-debugger 'debugpy))

(use-package pyvenv
  :after python-mode
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  :config
  (pyvenv-mode 1))

(use-package lsp-pyright
  :after python-mode
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright) (lsp))))

(use-package vterm
  :custom (vterm-kill-buffer-on-exit t)
  :config
  ;; Set this to match your custom shell prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string t))

;; Counsel-yank-pop in vterm
(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
                   (lambda (str) (vterm-send-string str t))))
          (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun m/configure-eshell()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions
               'eshell-truncate-buffer)
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . m/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package slime
  :ensure nil
  :config
  (slime-setup '(slime-fancy slime-company))
  (setq slime-lisp-implementation '((sbcl ("sbcl")))
        slime-default-lisp 'sbcl
        slime-contribs '(slime-fancy)))

(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion
                'slime-company-just-one-space))
