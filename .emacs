;; melpa setup
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Documents/personale/org/work.org"
                             "~/Documents/personale/org/casa.org"))

;; setup c indentation
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

;; display whitespace
(global-whitespace-mode 1)

(set-default-font "inconsolata-18")
;; add custom themes path and load gruvbox theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

