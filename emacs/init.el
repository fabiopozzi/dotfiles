(setq user-emacs-directory "~/.config/emacs/")
(setq default-directory "~/")

;; Use UTF-8 everywhere
(set-language-environment    "UTF-8")
(setopt locale-coding-system   'utf-8)
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
(setq  tab-line-exclude-modes nil)
(column-number-mode)
;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)


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
        (add-to-list 'eglot-server-programs '(web-mode "vls"))
  )

(use-package htmlize
  :custom (org-html-htmlize-output-type 'css))

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t))

(use-package indent-bars
  :ensure t
  :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer

(use-package org-cliplink
  :ensure t)

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

;; minibuffer completion
(use-package vertico
  :custom
  (vertico-count 5)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode)
  (vertico-flat-mode))

;; completion using corfu (da configurare)
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-fold-search nil dabbrev-case-replace nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package emacs
  :config
  (load-theme 'solarized-dark)
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)
)

(use-package vue-mode
    :ensure t)

(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

(defvar dimensione-font-default 160)
(set-face-attribute 'default nil :font "Inconsolata" :height dimensione-font-default)

;; hideshow
(add-hook 'python-mode-hook     'hs-minor-mode)

(setq-default c-tab-always-indent 'complete)
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
  :bind
    ("C-+" . 'hs-show-all)
    ("C-_" . 'hs-hide-all)
    ("C-=" . 'hs-show-block)
    ("C--" . 'hs-hide-block)
  )

(defun compile-with-gcc ()
  "Compile current buffer file with gcc."
  (interactive)
  (let ((filename (buffer-file-name))
        (compile-command nil))
    (if filename
        (if (string-match "\\.c$" filename)
            (progn
              (setq compile-command
                    (concat "gcc -Wall -o "
                            (file-name-sans-extension filename)
                            " "
                            filename))
              (compile compile-command))
          (message "Not a C file!"))
      (message "Buffer is not visiting a file!"))))

;; Bind the function to a key (optional)
(global-set-key (kbd "C-c C-g") 'compile-with-gcc)

(defun run-in-vterm ()
  "Run the binary corresponding to the current C file in vterm."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (string-match "\\.c$" filename)
            (let ((binary-name (file-name-sans-extension filename)))
              (if (file-exists-p binary-name)
                  (progn
                    ;; Ensure vterm is loaded
                    (require 'vterm)
                    ;; Create new vterm if none exists
                    (unless (get-buffer "*vterm*")
                      (vterm))
                    ;; Switch to vterm buffer
                    (switch-to-buffer "*vterm*")
                    ;; Send the command to execute the binary
                    (vterm-send-string (concat "./"
                                             (file-name-nondirectory binary-name)))
                    (vterm-send-return))
                (message "Binary not found! Compile the file first.")))
          (message "Not a C file!"))
      (message "Buffer is not visiting a file!"))))

;; Bind the function to a key (optional)
(global-set-key (kbd "C-c C-r") 'run-in-vterm)

;(defun compile-and-run ()
;  "Compile the current C file and run it in vterm if compilation succeeds."
;  (interactive)
;  (let ((compilation-buffer (compile-with-gcc)))
;    (set-process-sentinel 
;     (get-buffer-process compilation-buffer)
;     (lambda (process event)
;       (when (string-match "finished" event)
;         (run-in-vterm))))))
;
;; Bind the combined function to a key
;(global-set-key (kbd "C-c C-b") 'compile-and-run)

(setq org-todo-keywords
      '((sequence "TODO" "WIP" "ATTESA" "|" "DONE" "FATTO" "DELEGATO")))

(defun switch-scuola-persp ()
  (interactive)
  (persp-state-load "~/persp/scuola.save"))

(defun switch-lavoro-persp ()
  (interactive)
  (persp-state-load "~/persp/lavoro.save"))

(defun switch-tecno-persp ()
  (interactive)
  (persp-state-load "~/persp/tecnomedica.save"))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun create-frame-with-new-perspective ()
  "Create a new frame and switch to a new perspective in it."
  (interactive)
  ;; Crea un nuovo frame
  (let ((new-frame (make-frame)))
    ;; Seleziona il nuovo frame
    (select-frame new-frame)
    ;; Crea una nuova prospettiva
    (let ((persp-name (read-string "Enter perspective name: ")))
      (persp-switch persp-name))
    ;; Opzionalmente, puoi aggiungere qui altre personalizzazioni del frame
    (message "Created new frame with perspective: %s"
             (persp-current-name))))

;; Binding della funzione a una combinazione di tasti (opzionale)
(global-set-key (kbd "C-c n") 'create-frame-with-new-perspective)

;; keybindings
(global-set-key (kbd "C-c s") #'switch-scuola-persp)
(global-set-key (kbd "C-c l") #'switch-lavoro-persp)
(global-set-key (kbd "C-c t") #'switch-tecno-persp)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(global-set-key (kbd "C-5") #'forward-or-backward-sexp)
(global-set-key (kbd "s-/") 'completion-at-point)
(global-set-key (kbd "<f2>") 'org-clock-in)
(global-set-key (kbd "<f3>") 'org-clock-out)
(global-set-key (kbd "C-x p i") 'org-cliplink)

;; dired
(setq dired-dwim-target t)  ; suggest other visible dired buffer

(setq org-capture-templates
   '(("K" "Cliplink capture task" entry (file+headline "~/Nextcloud/org/bookmarks.org" "Bookmarks")
      "* %(org-cliplink-capture) \n  DATA: %t\n" :empty-lines 1)))

;; Scorciatoia per attivare org-capture
(global-set-key (kbd "<f9>") 'org-capture)


;; newsticker
(setq newsticker-url-list '(
  ("Emacs-news" "https://sachachua.com/blog/category/emacs-news/feed/")
))

(defun my/close-newsticker ()
    "Kill all tree-view related buffers."
    (kill-buffer "*Newsticker List*")
    (kill-buffer "*Newsticker Item*")
    (kill-buffer "*Newsticker Tree*"))

(advice-add 'newsticker-treeview-quit :after 'my/close-newsticker)

;; solarized colors
;; (defun set-tab-line-solarized ()
;;   (let ((active-bg "#002b36")
;;         (active-fg "#93a1a1")
;;         (inactive-bg "#073642")
;;         (inactive-fg "#586e75")
;;         (separator-color "#093642"))

;; Custom function to set specific colors
(defun set-tab-line-custom-colors ()
  "Set custom colors for tab-line elements."
  (let ((active-bg "#2b2b2b")
        (active-fg "#e6e6e6")
        (inactive-bg "#1a1a1a")
        (inactive-fg "#808080")
        (separator-color "#404040"))

    (set-face-attribute 'tab-line nil
                       :background inactive-bg
                       :foreground inactive-fg
                       :height 1.0
                       :box `(:line-width 1 :color ,separator-color))

    (set-face-attribute 'tab-line-tab nil
                       :inherit 'tab-line
                       :background active-bg
                       :foreground active-fg
                       :box `(:line-width 1 :color ,separator-color))

    (set-face-attribute 'tab-line-tab-inactive nil
                       :inherit 'tab-line
                       :background inactive-bg
                       :foreground inactive-fg
                       :box `(:line-width 1 :color ,separator-color))

    (set-face-attribute 'tab-line-tab-current nil
                       :inherit 'tab-line-tab
                       :background "#0404bf"
                       :foreground active-fg
                       :bold t)

    (set-face-attribute 'tab-line-highlight nil
                       :background "#505050"
                       :foreground active-fg)))

;; Call the custom function
(set-tab-line-custom-colors)




