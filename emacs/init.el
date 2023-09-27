;; Don't show the splash screen
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell nil)          ; Don't flash when the bell rings

;; Turn off some unneeded UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode 1)  ; Leave this one on if you're a beginner!

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

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

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

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "<f9>") 'neotree-toggle))

(defvar ic/elfeed-external-mode-map (make-sparse-keymap))
(define-minor-mode ic/elfeed-external-mode "A minor mode to add external modes `showing` elfeed entry content" (use-local-map ic/elfeed-external-mode-map))

;;;###autoload
(defun ic/split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;;;###autoload
(defun ic/split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'ic/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'ic/split-and-follow-vertically)

(use-package elfeed
  :defer t
  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs) 
  (evil-set-initial-state 'elfeed-show-mode 'emacs) 
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml"))
  :bind (:map elfeed-search-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("C-<tab>" . ic/elfeed-external-next-entry)
              ("E" . ic/elfeed-enqueue-media-url)
              ("Q" . ic/elfeed-save-and-quit)
              ("r" . ic/elfeed-mark-as-read)
              ("R" . ic/elfeed-mark-all-as-read)
           :map elfeed-show-mode-map
              ("C-<tab>" . ic/elfeed-external-next-entry)
           :map ic/elfeed-external-mode-map
           ("C-<tab>" . ic/elfeed-external-next-entry)))
;;
;; External mode functions
;;

;;;###autoload
(defun ic/elfeed-external-buffer-p (buf)
  "Returns non-nil if BUF has enabled the ic/elfeed-external-mode."
  (with-current-buffer buf
    (and (boundp 'ic/elfeed-external-mode) ic/elfeed-external-mode)))

;;;###autoload
(defun ic/elfeed-external-buffer-list ()
  "List all buffers that have the elfeed-external-mode enabled."
  (seq-filter (lambda (b) (and (not (eq (elfeed-search-buffer) b)) (ic/elfeed-external-buffer-p b))) (buffer-list)))

;;;###autoload
(defun ic/elfeed-current-buffer-external ()
  (interactive)
  "Returns non-nil if BUF has enabled the ic/elfeed-external-mode."
  (let ((result (and (boundp 'ic/elfeed-external-mode) ic/elfeed-external-mode))
        (name (buffer-name (current-buffer))))
        result))

;;;###autoload
(defun ic/elfeed-delete-non-search-windows ()
  (interactive)
  "Delete all elfeed non search buffers."
  ;; External
  (condition-case nil
      (ic/elfeed-delete-external-windows)
    (error nil))

  ;; Show
  (condition-case nil
      (ic/elfeed-delete-show-windows)
    (error nil)))

;;;###autoload
(defun ic/elfeed-delete-external-windows ()
  (mapcar (lambda (w) (when w (delete-window w))) (mapcar (lambda (b) (get-buffer-window b 'visible)) (ic/elfeed-external-buffer-list))))

;;;###autoload
(defun ic/elfeed-show-buffer-list ()
  "List all buffers that have teh elfeed-show-mode enabled."
  (seq-filter (lambda (b) (ic/elfeed-show-buffer-p b)) (buffer-list)))

;;;###autoload
(defun ic/elfeed-delete-show-windows ()
  (interactive)
  "List all buffers that have the elfeed-show-mode enabled."
  (mapcar (lambda (w) (when w (delete-window w))) (mapcar (lambda (b) (get-buffer-window b 'visible)) (ic/elfeed-show-buffer-list))))

;;;###autoload
(defun ic/mark-current-as-read ()
  (interactive)
  "Mark current entry as read."
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current)
    (elfeed-db-save-safe)))

(defun ic/elfeed-show-buffer-p (buf)
  "Returns non-nil if BUF has enabled the ic/elfeed-external-mode."
  (with-current-buffer buf (equal major-mode 'elfeed-show-mode)))


;;;###autoload
(defun ic/elfeed-jump-to-search(&optional visited)
  (interactive)
  "Jump to a elfeed-search window. VISITED is an optional list with windows already visited."
  (let* ((visited (or visited '()))
         (buffer (current-buffer))
         (name (buffer-name buffer))
         (window (frame-selected-window))
         (found (equal "*elfeed-search*" name))
         (current-buffer-selected (equal (get-buffer-window buffer) window)))

    (cond
     ((and found current-buffer-selected) t)
     ((member name visited) nil)
     (t (progn (other-window 1)
               (ic/elfeed-jump-to-search (add-to-list 'visited name))))))) 

;;;###autoload
(defun ic/elfeed-external-next-entry (&optional visited)
  (interactive)
  "Closes external elfeed windows and moves to next entry."
  (ic/elfeed-delete-non-search-windows)
  (ic/elfeed-jump-to-search)
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current))
  (next-line)
  (ic/elfeed-open-in-dwim (elfeed-search-selected :ignore-region)))
                              
;;; credits: https://emacs.stackexchange.com/questions/15033/how-to-mark-current-line-and-move-cursor-to-next-line
;;;###autoload
(defun ic/mark-line (&optional arg)
  "Select the current line and move the cursor by ARG lines IF no region is selected.
If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (let ((lines (or arg 1)))
    (when (not (use-region-p))
      (forward-line 0)
      (set-mark-command nil))
    (forward-line lines)))

;;;###autoload
(defun ic/elfeed-mark-as-read ()
  "Mark all items in the elfeed buffer as read."
  (interactive)
  (ic/mark-line 0)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun ic/elfeed-mark-all-as-read ()
  "Mark all items in the elfeed buffer as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun ic/elfeed-save-and-quit ()
  "Wrapper to save the elfeed db to disk before quiting"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;;
;; Window placement and behavior
;;
(defadvice elfeed-search-show-entry (around elfeed-search-show-entry-around activate)
  "Open entries in a new buffer below."
  (ic/mark-current-as-read)
  (ic/elfeed-delete-show-windows)
  (ic/split-and-follow-vertically)
  ad-do-it)

(defun ic/elfeed-kill-external-buffer-and-window (&optional buffer-or-name)
  "Kill the github issues window and buffer.  Return t if grep window was found."
  (if (or (derived-mode-p 'elfeed-show-mode) (ic/elfeed-current-buffer-external))
        (progn
          (kill-buffer-and-window)
          t)
        nil))


;; elfeed-kill-buffer should use (kill-current-buffer) vs (kill-buffer (current-buffer))  to trigger listeners
;; Let's use an advice to fix that.
(advice-add #'elfeed-kill-buffer :around #'kill-current-buffer)


(global-set-key (kbd "C-x w") 'elfeed)
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

(defun my/org-insert-current-time-as-inactive-time-stamp ()
   (interactive)
   (org-insert-heading)
   (insert (format-time-string "%d %B")))

(define-key org-mode-map (kbd "C-c _") #'my/org-insert-current-time-as-inactive-time-stamp)

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
