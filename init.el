(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; elpy
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(package-initialize)

;; setup.el provides a macro for configuration patterns
;; it makes package installation and config nice and tidy!
;; https://www.emacswiki.org/emacs/SetupEl
(use-package setup
  :ensure t)

;;;;;;;;;;;;;;;
;; INTERFACE ;;
;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (global-visual-line-mode)
  (save-place-mode 1)
  (global-display-line-numbers-mode)
  (blink-cursor-mode 0)
  :custom
  ;; keep track of saved places in ~/.emacs.d/places
  (save-place-file (concat user-emacs-directory "places"))
  (inhibit-startup-screen t)
  (visible-bell t)
  (display-line-numbers-type 'absolute) 
  ;; Don't use hard tabs
  (indent-tabs-mode nil)
  (tab-width 4)
  ;; shell scripts
  (sh-basic-offset 2)
  (sh-indentation 2)
  ;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
  (backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
  (auto-save-default nil))

;; Don't show line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq def-font "CaskaydiaCove Nerd Font Propo")
;; (setq def-font "JetBrainsMono Nerd Font Propo")
;; (setq def-font "Iosevka Extended")

(setq-default line-spacing 0.1)

(defun my-font (size)
  (format "%s %i" def-font size))

;; Set font
(add-to-list 'default-frame-alist `(font . ,(my-font 13)))

(use-package zenburn-theme
  :ensure t
  :defer nil
  :config
  (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package evil
;;   :ensure t
;;   :defer nil
;;   :custom
;;   (evil-undo-system 'undo-redo)
;;   (evil-bigword "^ \t\r\n")
;;   (evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; (use-package god-mode
;;   :ensure t
;;   :defer nil)

;; (use-package evil-god-state
;;   :ensure t
;;   :defer nil
;;   :config
;;   (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
;;   ;; (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
;;   ;; (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
;;   (evil-define-key 'god global-map [escape] 'evil-god-state-bail))


(use-package dired
  :ensure nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init ;; Open dired folders in same buffer
  (put 'dired-find-alternate-file 'disabled nil))

;; DOOM MODELINE
(use-package doom-modeline
  :ensure t
  :defer nil
  :custom
  (doom-modeline-height 28)
  :config
  (doom-modeline-mode 1))

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :ensure t
  :defer nil
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package which-key
  :ensure t
  :defer nil
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package golden-ratio
  :ensure t
  :defer nil
  :config
  (golden-ratio-mode 1))

(use-package projectile
  :ensure t
  :defer nil
  :custom
  (projectile-project-search-path '("~/src/" "~/work/" "~/.config"))
  (projectile-enable-caching 'persistent)
  (projectile-known-projects-file "~/.config/emacs/projectile-bookmarks.eld")
  (projectile-completion-system 'ido)
  :init
  (projectile-mode 1)
  :bind
  ("<f5>" . projectile--find-file)
  ("<f6>" . projectile-switch-project))

;; Expand-region - expand selection
(use-package expand-region
  :ensure t
  :defer nil
  :bind ("C-=" . er/expand-region))

(use-package ido
  :ensure nil
  :defer nil
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-file-extensions-order '(".clj" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  :init
  (ido-mode 1))

(use-package ido-completing-read+
  :ensure t
  :defer nil)

(use-package flx-ido
  :ensure t
  :defer nil
  :custom
  ;; disable ido faces to see flx highlights.
  (ido-enable-flex-matching t)
  (ido-use-faces nil)
  :init
  (flx-ido-mode 1)
)

(use-package amx
  :ensure t
  :defer nil
  :init
  (amx-mode))

;; (use-package smex
;;   :ensure t
;;   :defer nil
;;   :bind (("M-x" . smex))
;;   :config (smex-initialize))

;;;;;;;;;;;;;;;;
;; DEVLOPMENT ;;
;;;;;;;;;;;;;;;;

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

;; (use-package eglot
;;   :ensure nil
;;   :defer nil
;;   :bind (("C-S-i" . eglot-format-buffer))
;;   :hook ((
;;           clojure-ts-mode
;;           clojure-mode
;;           clojurec-mode
;;           clojurescript-mode
;;           elixir-ts-mode
;;           elixir-mode
;;           haskell-mode
;;           java-mode scala-mode)
;;          . eglot-ensure)
;;   :config
;;   (setq eglot-server-programs
;;       '((clojure-ts-mode . ("clojure-lsp"))))
;;   :custom
;;   (eglot-autoshutdown t)
;;   (eglot-extend-to-xref nil)
;;   (eglot-stay-out-of '(yasnippet))
;;   (eglot-confirm-server-initiated-edits nil)
;;   (setq-default eglot-workspace-configuration
;;                 '((haskell
;;                    (plugin
;;                     (stan
;;                      (globalOn . :json-false)))))))

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((clojure-ts-mode
          clojure-mode
          clojurec-mode
          clojurescript-mode
          elixir-ts-mode) . lsp)
  :commands tsp
  :bind (:map lsp-mode-map
              ("C-S-i" . lsp-format-buffer))
  :custom
  (lsp-lens-enable nil)
  (lsp-lens-place-position 'above-line)
  (lsp-modeline-code-actions-segments '(count icon name))
  )

;; (use-package lsp-ui
;;   :ensure t
;;   :defer t
;;   :custom
;;   ;; (lsp-ui-sideline-show-diagnostics t)
;;   (lsp-ui-sideline-show-hover t)
;;   ;; (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-sideline-update-mode "line")
;;   ;; (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-show-with-mouse t))

 
;; Magit - git 
(use-package magit
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t)

;; Autocompletion
(use-package corfu
  :ensure t
  :defer nil
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode))

;; ;; Paredit
(use-package paredit
  :ensure t
  :defer t 
  :hook (emacs-lisp-mode
	     eval-expression-minibuffer-setup
	     ielm-mode
	     lisp-mode
	     lisp-interaction-mode
	     scheme-mode
         cojure-mode
         clojure-ts-mode
         cider-repl-mode
         go-ts-mode
         go-mode)
  :config
  (show-paren-mode 1))

(use-package electric-pair-mode
  :ensure nil
  :defer t
  :hook
  (prog-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t 
  :hook (prog-mode))

(use-package mhtml-mode
  :ensure t
  :defer t 
  :bind
  ("C-c d" . html-div)
  ("C-c s" . html-span))


;; (use-package flymake
;;   :bind
;;   ("M-n" . flymake-goto-next-error)
;;   ("M-p" . flymake-goto-prev-error))

(use-package flycheck
  :ensure t
  :defer nil
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t
  :defer t)

;;;;;;;;;;;;
;; CLOJURE ;;
;;;;;;;;;;;;

(use-package clojure-ts-mode
  :ensure t
  :hook (clojure-ts-mode . subword-mode)
  (clojure-ts-mode . paredit-mode)
  :custom
  (clojure-ts-toplevel-inside-comment-form t)
  :config
  (require 'flycheck-clj-kondo))

(use-package clj-refactor
  :ensure t
  :config 
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook
  (clojure-ts-mode . clj-refactor-mode))

;; CIDER is a whole interactive development environment for
;; Clojure. There is a ton of functionality here, so be sure
;; to check out the excellent documentation at
;; https://docs.cider.mx/cider/index.html
(use-package cider
  :defer t 
  :bind
  ("C-c u" . cider-user-ns)
  ("C-M-r" . cider-refresh)
  :custom
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-history-file "~/.config/emacs/cider-history")
  (cider-repl-wrap-history t)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-result-prefix "\n"))


;; hydra provides a nice looking menu for commands
;; to see what's available, use M-x and the prefix cider-hydra
;; https://github.com/clojure-emacs/cider-hydra
(use-package cider-hydra
  :defer t 
  :hook
  (clojure-ts-mode . cider-hydra-mode))

;; additional refactorings for CIDER
;; e.g. add missing libspec, extract function, destructure keys
;; https://github.com/clojure-emacs/clj-refactor.el
(use-package clj-refactor
  :ensure t
  :defer t 
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook
  (clojure-ts-mode . clj-refactor-mode))

;; Use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;;;;;;;;;;;;;
;; HASKELL ;;
;;;;;;;;;;;;;

(use-package lsp-haskell
  :ensure t
  :defer t
  )

(use-package haskell-mode
  :ensure t
  :defer t
  :hook
  ((haskell-mode haskell-literate-mode) . lsp)
  :defer t)

;;;;;;;;;;;
;; GLEAM ;;
;;;;;;;;;;;

;; (use-package gleam-mode
;;   :ensure t
;;   :load-path "~/.config/emacs/gleam-mode"
;;   :bind (:map gleam-mode-map
;;               ("C-S-i" . gleam-format)))

;; (add-hook 'gleam-mode-hook
;;           (lambda () (add-hook 'before-save-hook 'gleam-format nil t)))

;;;;;;;;;
;; NIX ;;
;;;;;;;;;

(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

;;;;;;;;;;;;;
;; CUSTOMS ;;
;;;;;;;;;;;;;


;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
;; This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)

;; Временно, для диагностики:
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.nix-profile/bin") ":/run/current-system/sw/bin"))
(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(add-to-list 'exec-path "/run/current-system/sw/bin")


;; CUSTOMS
(setq custom-file "custom.el")
(load custom-file)

;; terminal
(setup (:package eat))

;; Bash completion
(use-package bash-completion
  :ensure t
  :defer t
  :config
  (bash-completion-setup))


;; KEYBINDINGS
(global-set-key (kbd "M-/") 'hippie-expand)

;; dd equivalent
(defun kill-current-line (&optional n)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))

(global-set-key (kbd "C-S-k") 'kill-current-line)

;; NEWLINE
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun begin-of-line-and-indented-new-line ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(global-set-key (kbd "C-M-j") 'switch-to-buffer)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "<C-return>") 'end-of-line-and-indented-new-line)
(global-set-key (kbd "<C-S-return>") 'begin-of-line-and-indented-new-line)

(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "<M-return>") 'cider-pprint-eval-last-sexp)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; M is set to CMD (much easier)
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-t-enable-clipboard t)

(add-hook 'org-mode-hook  'org-indent-mode)

(global-set-key (kbd "M-o") 'other-window)

;; LATEX
 ;; Org Export Settings
      (use-package org
        :custom
        (org-export-with-drawers nil)
        (org-export-with-todo-keywords nil)
        (org-export-with-broken-links t)
        (org-export-with-toc nil)
        (org-export-with-smart-quotes t)
        (org-export-date-timestamp-format "%d %B %Y"))

;; LaTeX PDF Export settings
  (use-package ox-latex
    :ensure nil
    :demand t
    :custom
    ;; Multiple LaTeX passes for bibliographies
    (org-latex-pdf-process
     '("pdflatex -interaction nonstopmode -output-directory %o %f"
       "bibtex %b"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    ;; Clean temporary files after export
    (org-latex-logfiles-extensions
     (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
             "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
             "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
             "tex" "bcf"))))
