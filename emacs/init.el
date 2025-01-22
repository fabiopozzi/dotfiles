(setq user-emacs-directory "~/.config/emacs/")
(setq default-directory "~/")

;; Use UTF-8 everywhere
(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

;; Store customize settings in their own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Put backup files somewhere else
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq c-default-style "k&r"
      c-basic-offset 4)

;; Visual settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-splash-screen t
      ring-bell-function 'ignore
      display-time-default-load-average nil
      use-dialog-box nil
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      )
(setq-default show-trailing-whitespace t)

(global-tab-line-mode t) ; a tab for each buffer
(column-number-mode)
;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; show trailing whitespace
;;(setq-default show-trailing-whitespace t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Set up package management. I like the use-package macro (which is
;; part of emacs 29) and am trying to use regular package archives
;; rather than something like straight / elpaca.
(require 'package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 20)
        ("MELPA"        . 15)
        ("ORG"          . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))
(package-initialize)

(use-package all-the-icons)

(use-package casual
  :ensure t
  )

(use-package chatgpt-shell
  :ensure t)
(setq chatgpt-shell-anthropic-key "la-mia-chiave-segreta")
(setq chatgpt-shell-model-version "claude-3-5-sonnet-20241022")

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
)

(use-package eglot
  :ensure t
  :defer t
  :hook ((c-mode . eglot-ensure)
         (c-mode . hs-minor-mode))
  :config
        (add-to-list 'eglot-server-programs '(c-mode "clangd"))
  )

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t))

(use-package indent-bars
  :ensure t
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package notmuch
  :ensure t
  :defer t)

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tab-line
  :ensure t
  :bind
  (("C-<iso-lefttab>" . tab-line-switch-to-prev-tab)
   ("C-<tab>" . tab-line-switch-to-next-tab)))

(use-package vterm
    :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.html\\'" . web-mode)))

(use-package solarized-theme
  :config (load-theme 'solarized-dark t))

(use-package emacs
  :config
  (load-theme 'solarized-dark)
)

;; Font Fira code
(defvar dimensione-font-default 140)
(set-face-attribute 'default nil :font "Fira Code Retina" :height dimensione-font-default)

;; hideshow
(add-hook 'python-mode-hook     'hs-minor-mode)

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


(setq c-basic-offset 4)
(setq c-mode-indent-offset 4)
(setq tab-width 4)

(use-package cc-mode
   :config
   (add-hook 'c-mode-common-hook
    (lambda ()
        (c-set-style "k&r")
        (setq indent-tabs-mode nil)
        (setq c-basic-offset 4)
        (setq c-mode-indent-offset 4)
        (setq tab-width 4)
       ))
   (add-hook 'c-mode-common-hook
             (lambda ()
               (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -o %s %s %s %s"
                              (or (getenv "CC") "gcc")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-pedantic -Wall -g")
                              file)))))
  :bind
    ("C-+" . 'hs-show-all)
    ("C-_" . 'hs-hide-all)
    ("C-=" . 'hs-show-block)
    ("C--" . 'hs-hide-block)
  )

(defun switch-scuola-persp ()
  (interactive)
  (persp-state-load "~/persp/scuola.save"))

(global-set-key (kbd "C-c s") #'switch-scuola-persp)

(defun switch-lavoro-persp ()
  (interactive)
  (persp-state-load "~/persp/lavoro.save"))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; keybindings
(global-set-key (kbd "C-c l") #'switch-lavoro-persp)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(global-set-key (kbd "C-%") #'forward-or-backward-sexp)
