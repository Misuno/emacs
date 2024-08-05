
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
  (setq inhibit-startup-screen t)
  (setq visible-bell t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (global-visual-line-mode)
  (save-place-mode 1)
  (setq display-line-numbers-type 't) 
  (global-display-line-numbers-mode)
  (blink-cursor-mode 0)
  
  ;; When you visit a file, point goes to the last place where it
  ;; was when you previously visited the same file.
  ;; http://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)
  :custom
  ;; keep track of saved places in ~/.emacs.d/places
  (save-place-file (concat user-emacs-directory "places"))

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

(setq-default line-spacing 0.2)


;; Don't show line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq def-font "CaskaydiaCove Nerd Font")
;; (setq def-font "Iosevka Nerd Font Propo")

(defun my-font (size)
  (format "%s %i" def-font size))

;; Set font
(add-to-list 'default-frame-alist `(font . ,(my-font 12)))

(use-package modus-themes
  :ensure t
  :defer nil
  :bind
  ([f4] . modus-themes-toggle)
  
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-disable-other-themes t)
  (modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-vivendi-tinted :no-confirm))


;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))



(use-package solaire-mode
  :ensure t
  :defer nil 
  :config
  (solaire-global-mode +1))



;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :ensure t
  :defer nil
  :config
  (global-undo-tree-mode t))

(use-package evil
  :ensure t
  :defer nil
  :custom
  (evil-undo-system 'undo-tree)
  (evil-bigword "^ \t\r\n")
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

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

(use-package undo-fu
  :ensure t 
  :defer nil)

;; NERD ICONS
(use-package nerd-icons
  :custom
  (nerd-icons-font-family def-font))


;; DOOM MODELINE
(use-package doom-modeline
  :custom
  (doom-modeline-height 28)
  :config
  (doom-modeline-mode 1))

;; This assumes you've installed the package via MELPA.
(use-package ligature
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
  :defer t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  ("<f5>" . projectile--find-file)
  ("C-M-p" . projectile-command-map))

;; Expand-region - expand selection
(use-package expand-region
  :defer t 
  :bind ("C-=" . er/expand-region))


;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Enable vertico-multiform
(vertico-multiform-mode)

;; Configure the display per command.
;; Use a buffer with indices for imenu
;; and a flat (Ido-like) menu for M-x.
(setq vertico-multiform-commands
      '((execute-extended-command unobtrusive)))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult

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
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package eglot
  :bind (("C-S-i" . eglot-format-buffer))
  :hook ((
          ;; clojure-ts-mode
          ;; clojure-mode
          ;; clojurec-mode
          ;; clojurescript-mode
          ;; elixir-ts-mode
          ;; elixir-mode
          haskell-mode
          java-mode scala-mode)
         . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
        '(clojure-ts-mode . ("clojure-lsp")))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref nil)
  (eglot-stay-out-of '(yasnippet))
  (eglot-confirm-server-initiated-edits nil)
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false)))))))


(setup (:package lsp-mode lsp-ui lsp-ivy lsp-treemacs)
  (:hook lsp-enable-which-key-integration)
  (:bind "M-<f7>" lsp-find-references))

(use-package lsp-mode
  :hook ((clojure-ts-mode
          clojure-ts-mode
          clojure-mode
          clojurec-mode
          clojurescript-mode
          elixir-ts-mode) . lsp)
  :commands lsp
  :bind (:map lsp-mode-map
              ("C-S-i" . lsp-format-buffer))
  :custom
  (lsp-lens-place-position 'above-line)
  (lsp-modeline-code-actions-segments '(count icon name)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nextls" "--stdio" ))
                    :multi-root t
                    ;; :initialization-options '(:experimental (:completions (:enable t))) ;; Enable the experimental completion mode
                    :activation-fn (lsp-activate-on "elixir")
                    :server-id 'next-ls)))



(use-package lsp-ui
  :custom
  ;; (lsp-ui-sideline-show-diagnostics t)
  ;; (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode "line")
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse t))

 
;; Magit - git 
(use-package magit
  :defer t)

;; Company - auto completion 
(use-package company
  :defer t 
  :hook (cider-mode
         prog-mode
	     cider-repl-mode)
  :custom
  (company-global-modes '(not org-mode))
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.1)))
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (completion-ignore-case t))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))


(use-package yasnippet
  :defer t 
  :hook
  (prog-mdoe . yas-minor-mode)
  :config
  (yas-global-mode 1))

;; Paredit
(use-package paredit
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
  :hook
  (prog-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :defer t 
  :hook (prog-mode))

(use-package mhtml-mode
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
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

;;;;;;;;;;;;
;; COJURE ;;
;;;;;;;;;;;;

(use-package clojure-ts-mode
  :defer t 
  :mode "\\.clj\\'"
  :hook (clojure-ts-mode . subword-mode)
  (clojure-ts-mode . paredit-mode)
  :custom
  (clojure-ts-toplevel-inside-comment-form t)
  :config
  (require 'flycheck-clj-kondo))

(setup (:package clj-refactor)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (:hook-into clojure-ts-mode))

(setup (:package cider-hydra)
  (:hook-into clojure-ts-mode))


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
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-repl-wrap-history t))


;; hydra provides a nice looking menu for commands
;; to see what's available, use M-x and the prefix cider-hydra
;; https://github.com/clojure-emacs/cider-hydra
(use-package cider-hydra
  :defer t 
  :hook
  (clojure-ts-mode))

;; additional refactorings for CIDER
;; e.g. add missing libspec, extract function, destructure keys
;; https://github.com/clojure-emacs/clj-refactor.el
(use-package clj-refactor
  :defer t 
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook
  (clojure-ts-mode))

;; Use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

;;;;;;;;;;;;;
;; HASKELL ;;
;;;;;;;;;;;;;

(use-package lsp-haskell)

(use-package haskell-mode
  :hook
  ((haskell-mode haskell-literate-mode) . lsp)
  :defer t)

;;;;;;;;;;;
;; GLEAM ;;
;;;;;;;;;;;

(use-package gleam-mode
  :load-path "~/.config/emacs/gleam-mode"
  :bind (:map gleam-mode-map
              ("C-S-i" . gleam-format)))

(add-hook 'gleam-mode-hook
          (lambda () (add-hook 'before-save-hook 'gleam-format nil t)))



;;;;;;;;;;;;;
;; CUSTOMS ;;
;;;;;;;;;;;;;


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; CUSTOMS
(setq custom-file "custom.el")
(load custom-file)

;; terminal
(setup (:package eat))

;; Bash completion
(use-package bash-completion
  :config
  (bash-completion-setup))


;; KEYBINDINGS
(global-set-key (kbd "M-/") 'hippie-expand)


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

(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)
(global-set-key (kbd "<C-M-return>") 'begin-of-line-and-indented-new-line)

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
