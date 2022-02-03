;;;; init.el -*- lexical-binding: t; -*-

;; "I am Gandalf, and Gandalf means me"
(setq-default user-full-name "Alex Combas")
(setq-default user-email-address "alex@flyingcastle.net")

(setq auth-sources '("~/.authinfo.gpg"))

;; Doom garbage collection trick, disables gcs during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; turn off startup message in favour of custom setting
(setq inhibit-startup-message t)
;; as opposed to an audio-beep
(setq visible-bell t)
;;save clipboard strings to kill ring before replacing
(setq save-interprogram-paste-before-kill t)
;; C-h a search for command using all apropos features
(setq apropos-do-all t)
;; middle button paste to point in buffer
(setq mouse-yank-at-point t)
;; single .[SPC] ends a sentence instead of .[SPC][SPC]
(setq sentence-end-double-space nil)
;; prevent outdated source code from being loaded
(setq load-prefer-newer t)
;; performance enhancers
(setq read-process-output-max (* 1024 1024))
;; turn off paren delay
(setq show-paren-delay 0)
;; as opposed to doing it another way
(setq backup-by-copying t)
;; do not create lockfiles, allows simultaneous editing
(setq create-lockfiles nil)
;; kill the entire line, including /n
(setq kill-whole-line t)
;; use ipython3 as python shell
(setq python-shell-interpreter "ipython3")
;; flags needed for ipython to run properly
(setq python-shell-interpreter-args "--simple-prompt -i")
;; use ssh for tramp
(setq tramp-default-method "ssh")
;; set SBCL for common lisp
(setq inferior-lisp-program (executable-find "sbcl"))
;; note: this is the linux trash, not the windows trash, I think...
(setq delete-by-moving-to-trash t)
;; make cursor the width of the character
(setq x-stretch-cursor t)
;; turn off load average
(setq display-time-default-load-average nil)

;; Keep custom settings separate
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Disable/Enable various ui elements
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(display-time-mode 1)
(show-paren-mode)

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

;; revert file automatically if it changes on disk
(global-auto-revert-mode 1)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default nuke-trailing-whitespace-p t)

;; Cleanup white space
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(defun ac-remove-dos-eol ()
  "Do not show ^M in files containing mixed line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'ac-remove-dos-eol)

;; Set environment to Canadian English, eh?
(set-locale-environment "en_CA.UTF-8")

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

;; Better history settings
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; put auto-save files into Emacs directory
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; put backup files into Emacs directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(setq backup-by-copying 1)
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

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
                vterm-mode-hook
                pdf-view-mode
                nov-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurance of CHAR." t)

(defun ac-insert-date()
  "Make inserting the date easier C-c i d"
  (interactive)
  (insert (format-time-string "%x")))

(defun ac-insert-time()
  "Make inserting the time easier C-c i t"
  (interactive)
  (insert (format-time-string "%X")))

(defun ac-reload-init ()
  "Make reloading ~/.emacs.d/init.el easier <f6>"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; get that good scroll on
(pixel-scroll-precision-mode)
(use-package good-scroll
  :ensure t
  :config
  (good-scroll-mode 1)
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen))

;; use the sexy doom modeline theme
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; and a nice dark doom theme
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-material-dark t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; spell and grammar check tool
(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar
        "~/Source/LanguageTool-5.6/languagetool-commandline.jar")
  (setq langtool-default-language "en-CA")
  (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8")))

;; more spell checking
(use-package flyspell
  :ensure nil
  :config (setq ispell-program-name "aspell")
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

;; even more spell checking
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; yet even more spell checking
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

;; Good auto completion once it is setup properly
(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq company-show-quick-access 'left)
  (setq company-backends '((company-capf company-dabbrev-code)))
  (setq company-tooltip-flip-when-above t)
  :custom
  ;;use M-[asdfg] keys for quick completions
  (company-quick-access-keys '("a" "s" "d" "f" "g"))
  (company-quick-access-modifier 'meta)
  :custom-face
  ;; set the face manually to fixed-width
  (company-tooltip ((t (:family "Source Code Pro"))))
  :hook (prog-mode . company-mode))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  :bind
  (:map pdf-view-mode-map
        ("g" . pdf-view-first-page)
        ("G" . pdf-view-last-page)
        ("n" . pdf-view-next-page)
        ("p" . pdf-view-previous-page)
        ("e" . pdf-view-goto-page)
        ("u" . pdf-view-revert-buffer)
        ("s" . pdf-occur)
        ("al" . pdf-annot-list-annotations)
        ("at" . pdf-annot-add-text-annotation)
        ("ad" . pdf-annot-delete)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "cmark"))

;; nov ebook reader
(use-package nov
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-text-width 100)
  (setq visual-fill-column-center-text t)
  :hook
  (nov-mode . visual-line-mode)
  (nov-mode . visual-fill-column-mode))

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

;; make info documentation more colourful
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; for editing mode for .yml files
(use-package yaml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; use dired-single for dired
(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("f" . dired-single-buffer)
        ("b" . dired-single-up-directory))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :hook (dired-mode . auto-revert-mode)) ;auto refresh on file change

;; use ! to call `xdg-open` on any file, this opens the file
;; with the default OS application: eg. vlc for movies, lollypop for
;; music, etc.
(use-package dired-x
  :ensure nil
  :after dired
  :config
  ;; with these commands Emacs doesn't freeze when calling an external
  ;; application plus the application will continue to run even if
  ;; Emacs is closed.
  (setq dired-guess-shell-alist-user '(("" "setsid -w nohup xdg-open &"))))

;; hide dot files by default, H to toggle visibility
(use-package dired-hide-dotfiles
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("H" . dired-hide-dotfiles-mode))
  :hook (dired-mode . dired-hide-dotfiles-mode))

;; turn dired into a single buffer application
(use-package dired-single
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ([remap dired-find-file] . dired-single-buffer)
        ([remap dired-mouse-find-file-other-window] .
         dired-single-buffer-mouse)
        ([remap dired-mouse-find-file] . dired-single-buffer-mouse)
        ([remap dired-up-directory] . dired-single-up-directory)))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/Projects/"))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

(use-package rg
  :ensure t)

;; Org mode configuration start
(use-package org
  :ensure t
  :bind (:map org-mode-map ("C-c s" . org-schedule))
  :config
  (setq org-startup-indented t) ; all headings are indented
  (setq org-log-done 'time) ; add time when task in complete
  (setq org-use-effective-time t) ; calcualte midnight properly
  (setq org-src-fontify-natively t) ; native highlighting
  (setq org-src-tab-acts-natively t) ; tab works as expected
  (setq org-agenda-files '("~/Org"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
        '((sequence "IN-PROGRESS" "TODO" "REMINDER" "|" "CANCELLED" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("IN-PROGRESS" :foreground "orange" :weight bold)
          ("REMINDER" :foreground "pink" :weight bold)
          ("DONE" :foreground "green" :weight bold)
          ("CANCELED" :foreground "green" :weight bold)))
  :hook
  (org-mode . font-lock-mode) ; toggle syntax highlighting
  (org-mode . visual-line-mode)) ; toggle visual line editing

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/Org/Agenda.org" "Tasks")
         "** TODO %?\n %i\n %a")
        ("j" "Journal Entry" entry
         (file+olp+datetree "~/Org/Journal.org")
         "**** %i%?\n")))

(use-package org-bullets
  :ensure t
  :config (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸"))
  :hook (org-mode . org-bullets-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (python . t)
     (sqlite . t)
     (latex . t)
     (shell . t))))

;; wrap lines at column-width rather than at windows edge
(use-package visual-fill-column
  :ensure t
  :config
  (setq-default fill-coumn 90)
  (setq-default fill-column-width 90)
  (setq-default visual-fill-column-width 90)
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-enable-sensible-window-split t)
  (setq-default visual-fill-column-fringes-outside-margins t)
  :hook (visual-line-mode . visual-fill-column-mode))

;; fixed-with for code, variable-width for text
;; turn on auto-fill-mode and  mixed-pitch-mode
(use-package mixed-pitch
  :ensure t
  :hook
  ((org-mode text-mode prog-mode) . auto-fill-mode)
  ((org-mode text-mode) . mixed-pitch-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 10))

;; use ALT-o when in the minibuffer for extra commands
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config (ivy-rich-mode t))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-ivy-rich
  :ensure t
  :config (all-the-icons-ivy-rich-mode t))

;; nicer icons for dired
(use-package all-the-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode t))

(use-package helpful
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-popup
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
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(use-package eterm-256color
  :ensure t
  :hook (term-mode . eterm-256color-mode))

(defun ac-configure-eshell()
  "Set eshell history to 10k, also ignore duplication"
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000)
  (setq eshell-buffer-maximum-lines 10000)
  (setq eshell-hist-ignoredups t)
  (setq eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :ensure nil
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t))
  (eshell-git-prompt-use-theme 'powerline)
  :hook (eshell-first-time-mode . ac-configure-eshell))

(use-package eshell-git-prompt
  :ensure t
  :after eshell)

;; load local version of Common Lisp Hyper Spec
(load "~/quicklisp/clhs-use-local.el" t)

(use-package slime
  :ensure t
  :init (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy slime-company slime-quicklisp slime-asdf))
  :hook
  (slime-mode . slime-company)
  (slime-mode . (lambda ()
                  (load (expand-file-name
                         "~/quicklisp/slime-helper.el"))
                  (add-to-list 'slime-contribs 'slime-fancy)
                  (add-to-list 'slime-contribs 'inferior-lisp))))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(use-package emojify
  :ensure t
  :after erc
  :config (global-emojify-mode))

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook))

(use-package puni
  :ensure t
  :hook ((prog-mode sgml-mode nxml-mode tex-mode) . puni-mode))

;; credit to Greg Own for the rg snippet which modifies grep-find to use
;; ripgrep, by default it checks the top level directory if inside a
;; git repository or else the pwd.
(use-package grep
  :ensure nil
  :config
  (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

;; tb-keycast-mode setup, not in melpa yet
(add-to-list 'load-path "~/Source/tb-keycast")
(require 'tb-keycast)
(setq tab-bar-format nil)
(setq tb-keycast-status-align 'left)
(setq tb-keycast-status-min-width 0)
(set-face-attribute 'tb-keycast-face nil
                    :background "#3e3e3e"
                    :foreground "#88FFAA"
                    :slant 'normal
                    :weight 'bold)
(setq tb-keycast-ignore '(minibuffer-cmd))

;; hot keys
(global-set-key (kbd "<f5>") 'ac-reload-init)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'counsel-org-capture)
(global-set-key (kbd "C-c w") 'elfeed)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c i d") 'ac-insert-date)
(global-set-key (kbd "C-c i t") 'ac-insert-time)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)
(global-set-key (kbd "C-h F") 'helpful-function)
(global-set-key (kbd "C-h C") 'helpful-command)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'counsel-rg)
(global-set-key (kbd "C-c C-g") 'grep-find) ; powered by ripgrep
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-c h") 'langtool-check) ; starts the checker
(global-set-key (kbd "C-c j")  'langtool-correct-buffer) ; corrects the buffer
(global-set-key (kbd "C-c k") 'langtool-check-done) ; ends the checker

;; turn off suspend functionality
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

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
