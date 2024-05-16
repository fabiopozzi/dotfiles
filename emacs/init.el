;; Don't show the splash screen
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell nil)          ; Don't flash when the bell rings

;; Turn off some unneeded UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)  ; Leave this one on if you're a beginner!

;(global-tab-line-mode t) ; a tab for each buffer
;(setq tab-line-new-button-show nil)  ;; do not show add-new button
;(setq tab-line-close-button-show nil)  ;; do not show close button


;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs_backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old


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
;;(package-initialize)

;; Bootstrap `use-package`
 (unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))
(require 'use-package)

;; installa tutti i pacchetti che non sono presenti
(setq use-package-always-ensure t)

(column-number-mode)
;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; indentation settings
(setq-default indent-tabs-mode nil)
(setq tab-width 4) ; tabs larghi 4
(setq c-default-style "k&r"
	c-basic-offset 4)

;; (use-package command-log-mode)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package all-the-icons)

(use-package htmlize
  :custom (org-html-htmlize-output-type 'css)
  )

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(evil-set-undo-system 'undo-redo)

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "<f9>") 'neotree-toggle))

(global-set-key (kbd "C-x 2") 'ic/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'ic/split-and-follow-vertically)

;; custom evil keybindings

;; set leader key in tutti gli stati
(evil-set-leader nil (kbd "SPC"))
;; tasti vari
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b") 'list-buffers)
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit-status)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

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

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; switcha ad ultimo buffer usato con f1
(global-set-key (kbd "<f1>")  'mode-line-other-buffer)
;; switch buffer using f2-f3
;; (global-set-key (kbd "<f2>") 'previous-buffer)
;; (global-set-key (kbd "<f3>") 'next-buffer)

(global-set-key (kbd "<f7>") 'org-clock-in)
(global-set-key (kbd "<f8>") 'org-clock-out)

;; Evidenzia linea corrente
(hl-line-mode 1)
;; Evita blink cursore
(blink-cursor-mode -1)

;; Font Fira code
(defvar dimensione-font-default 160)
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

(global-set-key (kbd "C-c .") #'switch-scuola-persp)

(defun switch-lavoro-persp ()
  (interactive)
  (persp-state-load "~/persp/lavoro.save"))

(global-set-key (kbd "C-c ,") #'switch-lavoro-persp)

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
