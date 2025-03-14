
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
  (visible-bell nil)
  (display-line-numbers-type 'relative) 
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

(setq-default line-spacing 0.3)


;; Don't show line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq def-font "CaskaydiaMono Nerd Font Propo")
;; (setq def-font "Iosevka Nerd Font Propo")

(defun my-font (size)
  (format "%s %i" def-font size))

;; Set font
(add-to-list 'default-frame-alist `(font . ,(my-font 16)))

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

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
  :defer t
  :config
  (projectile-mode +1)
  :bind
  ("<f5>" . projectile--find-file)
  ("C-M-p" . projectile-command-map))

;; Expand-region - expand selection
(use-package expand-region
  :ensure t
  :defer nil
  :bind ("C-=" . er/expand-region))

;; (use-package ido
;;   :ensure t
;;   :defer nil
;;   :custom
;;   (ido-enable-flex-matching t)
;;   (ido-everywhere t)
;;   (ido-file-extensions-order '(".clj" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;;   :config
;;   (ido-mode 1))

;; (use-package smex
;;   :ensure t
;;   :defer nil
;;   :bind (("M-x" . smex))
;;   :config (smex-initialize))

(use-package counsel
  :ensure t
  :defer nil
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package posframe
  :ensure t
  :defer nil)

(use-package ivy-posframe
     :ensure t
     :after ivy
     :config
     (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
     (ivy-posframe-mode 1))

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
  :ensure nil
  :defer t
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

(use-package lsp-mode
  :ensure t
  :defer t
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
  (lsp-lens-enable nil)
  (lsp-lens-place-position 'above-line)
  (lsp-modeline-code-actions-segments '(count icon name)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nextls" "--stdio" ))
                    :multi-root t
                    ;; :initialization-options '(:experimental (:completions (:enable t))) ;; Enable the experimental completion mode
                    :activation-fn (lsp-activate-on "elixir")
                    :server-id 'next-ls)))



;; (use-package lsp-ui
;;   :ensure t
;;   :defer t
;;   :custom
;;   ;; (lsp-ui-sideline-show-diagnostics t)
;;   ;; (lsp-ui-sideline-show-hover t)
;;   ;; (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-sideline-update-mode "line")
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-show-with-mouse t))

 
;; Magit - git 
(use-package magit
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t)

;; ;; Company - auto completion 
;; (use-package company
;;   :ensure t
;;   :defer t 
;;   :hook (cider-mode
;;          prog-mode
;; 	     cider-repl-mode)
;;   :custom
;;   (company-global-modes '(not org-mode))
;;   (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.1)))
;;   (company-minimum-prefix-length 1)
;;   (company-require-match nil)
;;   (completion-ignore-case t))

;; ;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (use-package yasnippet
;;   :ensure t
;;   :defer t 
;;   :hook
;;   (prog-mdoe . yas-minor-mode)
;;   :config
;;   (yas-global-mode 1))

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
;; COJURE ;;
;;;;;;;;;;;;

(use-package clojure-ts-mode
  :ensure t
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
  (cider-repl-history-file "~/.emacs.d/cider-history")
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
  :ensure t
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
  :ensure t
  :defer t
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
