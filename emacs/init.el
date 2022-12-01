;; Don't show the splash screen
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell nil)          ; Don't flash when the bell rings

;; Turn off some unneeded UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode 1)  ; Leave this one on if you're a beginner!

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;(setq use-package-always-ensure t)

(column-number-mode)
;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Load the Modus Vivendi dark theme
(load-theme 'modus-vivendi t)

;;(use-package command-log-mode)

;;(use-package all-the-icons)

;;(use-package doom-modeline
;;  :init (doom-modeline-mode 1)
;;  :custom ((doom-modeline-height 15)))

;;(use-package doom-themes
;;  :init (load-theme 'doom-dracula t))

;;(use-package rainbow-delimiters
;;  :hook (prog-mode . rainbow-delimiters-mode))

;;(use-package evil
;;  :init
;;  (setq evil-want-integration t)
;;  (setq evil-want-keybinding nil)
;;  (setq evil-want-C-u-scroll t)
;;  (setq evil-want-C-i-jump nil)
;;  :config
;;  (evil-mode 1)
;;  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
;;  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;  (evil-set-initial-state 'messages-buffer-mode 'normal)
;;  (evil-set-initial-state 'dashboard-mode 'normal))

;;(use-package evil-collection
;;  :after evil
;;  :config
;;  (evil-collection-init))

;;(use-package hydra)

;;(defhydra hydra-text-scale (:timeout 4)
;;  "scale text"
;;  ("j" text-scale-increase "in")
;;  ("k" text-scale-decrease "out")
;;  ("f" nil "finished" :exit t))

;; Evidenzia linea corrente
(hl-line-mode 1)
;; Evita blink cursore
(blink-cursor-mode -1)

;; Abilita ricordo file recenti
(recentf-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

