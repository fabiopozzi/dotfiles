;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fabio Pozzi"
      user-mail-address "fabio@fabiopozzi.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
 (setq doom-font (font-spec :family "monospace" :size 20 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-dark+)
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/org/")

(setq org-journal-dir "~/Documents/org/journal")

;; default file for org capture
(setq org-default-notes-file (concat org-directory "/TODO.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; abilita menu-bar
(menu-bar-mode 1)

;; Prova ad evidenziare le parti di codice latex inserite
(setq org-highlight-latex-and-related '(latex script entities))

(setq org-todo-keywords
      '((sequence "TODO" "PROGRESS" "TEST" "|" "DONE" "RIMANDATO")))

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) https://stackoverflow.com/a/8836107/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

(setq company-global-modes '(not org-mode))

(setq org-emphasis-alist
  '(("*" (bold :background "black" :foreground "Orange" ))
    ("/" (italic :background "black" :foreground "green"))
    ("_" underline (:background "black"))
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))

;; indentazione con tabs per file c/c++
(setq-hook! '(c-mode-hook c++-mode-hook) indent-tabs-mode t)
(setq-default c-default-style "linux"
              c-basic-offset 4
              tab-width 4)

(add-hook 'python-mode-hook
    (lambda ()
       (setq indent-tabs-mode nil)
       (setq tab-width 4)))

;; latex settings
(setq +latex-viewers '(evince))

;; clangd
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(add-to-list 'exec-path "/home/fabio/.local/bin")

(setq initial-frame-alist '((top . 0) (left . 0) (width . 161) (height . 64)))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
