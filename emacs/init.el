;; Don't show the splash screen
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell nil)          ; Don't flash when the bell rings

;; Turn off some unneeded UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)  ; Leave this one on if you're a beginner!

(global-tab-line-mode t) ; a tab for each buffer
(setq tab-line-new-button-show nil)  ;; do not show add-new button
(setq tab-line-close-button-show nil)  ;; do not show close button

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs_backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; setup TRAMP
(setq tramp-default-method "ssh")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; setup exec-path per usare ag
(setq exec-path (append exec-path '("/usr/bin")))

;; Initialize package sources
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; Bootstrap `use-package`
 (unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))
(require 'use-package)

;; installa tutti i pacchetti che non sono presenti
;;(setq use-package-always-ensure t)

;; setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(column-number-mode)
;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; (use-package command-log-mode)
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
)

(use-package indent-bars
  :ensure t
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer

(use-package all-the-icons)

(use-package htmlize
  :custom (org-html-htmlize-output-type 'css))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(use-package solarized-theme
  :config (load-theme 'solarized-dark t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "<f9>") 'neotree-toggle))

(use-package notmuch
  :ensure t
  :defer t)

(global-set-key (kbd "C-x 2") 'ic/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'ic/split-and-follow-vertically)

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  )

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )

(use-package vterm
    :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.html\\'" . web-mode)))

(use-package eglot
  :ensure t
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; hideshow
(add-hook 'python-mode-hook     'hs-minor-mode)

;(use-package treesit-auto
  ;:custom
  ;(treesit-auto-install 'prompt)
  ;:config
  ;(treesit-auto-add-to-auto-mode-alist 'all)
  ;(global-treesit-auto-mode))

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t))

;; outline-indent
(use-package outline-indent
  :ensure t
  :straight (outline-indent
             :type git
             :host github
             :repo "jamescherti/outline-indent.el")
  :custom
  (outline-indent-ellipsis " ▼ "))

(use-package casual-dired
  :ensure t
  :straight (casual-dired
             :type git
             :host github
             :repo "/kickingvegas/casual-dired")
  :bind (:map dired-mode-map
              ("C-t" . #'casual-dired-tmenu)
              ("s" . #'casual-dired-sort-by-tmenu)
              ("/" . #'casual-dired-search-replace-tmenu)))

(use-package pdf-tools
   :ensure t
   :pin manual
   :mode  ("\\.pdf\\'" . pdf-view-mode)
   :config
   (setq-default pdf-view-display-size 'fit-width)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   (setq pdf-annot-activate-created-annotations t "automatically annotate highlights")
   (pdf-tools-install :no-query)
   (require 'pdf-occur))

(use-package tab-line
  :ensure t
  :bind
  (("C-<iso-lefttab>" . tab-line-switch-to-prev-tab)
   ("C-<tab>" . tab-line-switch-to-next-tab)))

; tab line customization
(require 'powerline)
(defvar my/tab-height 22)
(defvar my/tab-left (powerline-bar-right 'tab-line nil my/tab-height))
(defvar my/tab-right (powerline-bar-left nil 'tab-line my/tab-height))

(defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
  (powerline-render (list my/tab-left
                          (format " %s  " (buffer-name buffer))
                          my/tab-right)))
(setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)

(set-face-attribute 'tab-line nil ;; background behind tabs
      :background "gray40"
      :foreground "gray60" :distant-foreground "gray50"
      :family "Fira Sans Condensed" :height 1.0 :box nil)
;; (set-face-attribute 'tab-line-tab nil ;; active tab in another window
;;       :inherit 'tab-line
;;       :foreground "gray70" :background "gray90" :box nil)
;; (set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
;;       :background "#008081" :foreground "white" :box nil)
;; (set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
;;       :background "gray80" :foreground "black" :box nil)
;; (set-face-attribute 'tab-line-highlight nil ;; mouseover
;;       :background "white" :foreground 'unspecified)
(set-face-attribute 'tab-line-tab-modified nil ;; tab con modifiche
         :foreground "red" :weight 'normal)


(add-hook 'python-mode-hook #'outline-indent-minor-mode)
(add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

(defun my-c-mode-common-hook ()
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (setq c-basic-offset 4
        c-indent-level 4
        c-default-style "stroustrup")
  (setq indent-tabs-mode t)
  )

(use-package c-ts-mode
 :if (treesit-language-available-p 'c)
 :custom
 (c-ts-mode-indent-offset 4)
 (c-ts-mode-indent-style "stroustrup")
 :init
 ;; Remap the standard C/C++ modes
 (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
 (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
 (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

 ;(c-set-offset 'substatement-open 0)
 ;;; other customizations can go here

 ;(setq c++-tab-always-indent t)
 ;(setq c-basic-offset 4)                  ;; Default is 2
 ;(setq c-indent-level 4)                  ;; Default is 2

 ;(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 ;(setq tab-width 4)
 ;(setq indent-tabs-mode t)  ; use spaces only if nil
 ;)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; vterm toggle
(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)

(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)

;; switcha ad ultimo buffer usato con f1
(global-set-key (kbd "<f1>")  'mode-line-other-buffer)

(global-set-key (kbd "<f7>") 'org-clock-in)
(global-set-key (kbd "<f8>") 'org-clock-out)

;; Evidenzia linea corrente
(hl-line-mode 1)
;; Evita blink cursore
(blink-cursor-mode -1)

;; Font Fira code
(defvar dimensione-font-default 140)
(set-face-attribute 'default nil :font "Fira Code Retina" :height dimensione-font-default)

;; Abilita ricordo file recenti
(recentf-mode 1)

;; Abilita fido mode per ricerca nei buffer
(fido-vertical-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(defun split-and-vterm()
	(interactive)
	(split-window-right)
	(other-window -1)
	(vterm)
)
(global-set-key (kbd "<f4>") 'split-and-vterm)

(defun my-org-insert-date ()
   (interactive)
   (org-insert-heading)
   (insert (format-time-string "%d %B")))

(global-set-key (kbd "C-c _") #'my-org-insert-date)

(defun switch-scuola-persp ()
  (interactive)
  (persp-state-load "~/persp/scuola.save"))

(global-set-key (kbd "C-c s") #'switch-scuola-persp)

(defun switch-lavoro-persp ()
  (interactive)
  (persp-state-load "~/persp/lavoro.save"))

(global-set-key (kbd "C-c l") #'switch-lavoro-persp)

(defun copy-line (&optional arg)
    "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables.  This function works by temporarily making the
buffer read-only."
    (interactive "P")
    (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-line arg)))
;; optional key binding
(global-set-key "\C-c\C-k" 'copy-line)

(require 'compile)
 (add-hook 'c-mode-hook
           (lambda ()
	     (unless (file-exists-p "Makefile")
	       (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -c -o %s.o %s %s %s"
                              (or (getenv "CC") "gcc")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-pedantic -Wall -g")
			      file))))))

; config org mode per eseguire codice
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(custom-set-variables
 ; custom-set-variables was added by Custom.
 ; If you edit it by hand, you could mess it up, so be careful.
 ; Your init file should contain only one such instance.
 ; If there is more than one, they won't work right.
 '(package-selected-packages '(all-the-icons use-package)))
(custom-set-faces
 ; custom-set-faces was added by Custom.
 ; If you edit it by hand, you could mess it up, so be careful.
 ; Your init file should contain only one such instance.
 ; If there is more than one, they won't work right.
 )
