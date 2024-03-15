(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; elpy
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)
;; setup.el provides a macro for configuration patterns
;; it makes package installation and config nice and tidy!
;; https://www.emacswiki.org/emacs/SetupEl
(use-package setup
  :ensure t)

;;;;;;;;;;;;;;;
;; INTERFACE ;;
;;;;;;;;;;;;;;;

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-visual-line-mode)
(save-place-mode 1)
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)
(blink-cursor-mode 0)


;; Don't show line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq def-font "Iosevka ss16")

(defun my-font (size)
  (format "%s %i" def-font size))

;; Set font
(add-to-list 'default-frame-alist `(font . ,(my-font 13)))

;; Set theme

;; (setup (:package atom-one-dark-theme)
;;   (load-theme 'atom-one-dark t))

;; (setup (:package monokai-pro-theme)
;;   (load-theme 'monokai-pro t))

(setup (:package spacemacs-theme)
  (load-theme 'spacemacs-dark t))

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; shell scripts
(setq-default sh-basic-offset 2
              sh-indentation 2)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; NERD ICONS
(setup (:package nerd-icons)
  ;; (:option nerd-icons-font-family "Iosevka ss16")
  )

;; DOOM MODELINE
(setup (:package doom-modeline)
  (:option doom-modeline-height 24)
  (doom-modeline-mode 1))

;; Fira code font 
;; (setup (:package fira-code-mode)
;;   (:option fira-code-mode-disabled-ligatures '("#(" "[]" "x")) ; ligatures you don't want
;;   (:hook-into prog-mode)
;;   (fira-code-mode-set-font)
;;   (global-fira-code-mode))                                         ; mode to enable fira-code-mode in

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :ensure t
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
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setup (:package which-key)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setup (:package projectile)
  (projectile-mode +1)
  (:bind "C-c f" projectile--find-file)
  ;; Recommended keymap prefix on macOS
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  ;; (define-key projectile-mode-map (kbd "C-M-p") 'projectile-command-map)
  )

(setup (:package ido)
  (:option ido-enable-flex-matching t
           ido-everywhere nil
           ido-use-filename-at-point 'guess
           ido-create-new-buffer 'always
           ido-file-extensions-order '(".clj" ".go" ".edn" ".templ" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")
           ido-ignore-extensions t)
  (setq-default confirm-nonexistent-file-or-buffer nil)
  (ido-mode 1))

(setup (:package smex)
  (:global "M-x" smex
           "<menu>" smex
           "M-X" smex-major-mode-commands)
  (smex-initialize))

(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command 
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

;; (defadvice ido-set-matches-1 (around ido-smex-acronym-matches activate)
;;   "Filters ITEMS by setting acronynms first."
;;   (if (and (fboundp 'smex-already-running) (smex-already-running) (> (length ido-text) 1))

;;       ;; We use a hash table for the matches, <type> => <list of items>, where
;;       ;; <type> can be one of (e.g. `ido-text' is "ff"):
;;       ;; - strict: strict acronym match (i.e. "^f[^-]*-f[^-]*$");
;;       ;; - relaxed: for relaxed match (i.e. "^f[^-]*-f[^-]*");
;;       ;; - start: the text start with (i.e. "^ff.*");
;;       ;; - contains: the text contains (i.e. ".*ff.*");
;;       (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
;;             (matches (make-hash-table :test 'eq)))

;;         ;; Filtering
;;         (dolist (item items)
;;           (let ((key))
;;             (cond
;;              ;; strict match
;;              ((string-match (concat regex "[^-]*$") item)
;;               (setq key 'strict))

;;              ;; relaxed match
;;              ((string-match regex item)
;;               (setq key 'relaxed))

;;              ;; text that start with ido-text
;;              ((string-match (concat "^" ido-text) item)
;;               (setq key 'start))

;;              ;; text that contains ido-text
;;              ((string-match ido-text item)
;;               (setq key 'contains)))

;;             (when key
;;               ;; We have a winner! Update its list.
;;               (let ((list (gethash key matches ())))
;;                 (puthash key (push item list) matches)))))

;;         ;; Finally, we can order and return the results
;;         (setq ad-return-value (append (gethash 'strict matches)
;;                                       (gethash 'relaxed matches)
;;                                       (gethash 'start matches)
;;                                       (gethash 'contains matches))))

;;     ;; ...else, run the original ido-set-matches-1
;;     ad-do-it))

;; Expand-region - expand selection
(setup (:package expand-region)
  (:global "C-=" er/expand-region))

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
        ("C-S-b"   . treemacs)
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

;;;;;;;;;;;;;;;;
;; DEVLOPMENT ;;
;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-diagnostics-provider :flymake)
  :commands
  (lsp))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; (use-package eglot
;;   :ensure t
;;   :hook (((java-mode scala-mode go-ts-mode)
;;           . eglot-ensure)
;;          ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
;;   :preface
;;   (defun eglot-disable-in-cider ()
;;     (when (eglot-managed-p)
;;       (if (bound-and-true-p cider-mode)
;;           (progn
;;             (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
;;             (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
;;         (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
;;         (add-hook 'xref-backend-functions 'eglot-xref-backend nil t)))))

;; Magit - git 
(setup (:package magit))

;; optionally if you want to use debugger
;; Enabling only some features
(setq dap-auto-configure-features '(sessions locals controls tooltip))


(use-package dape
  :ensure t
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; :preface
  ;; (setq dape-key-prefix "\C-x\C-a")
  ;;
  ;; May also need to set/change gud (gdb-mi) key prefix
  ;; (setq gud-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  ;; :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )


;; Company - auto 
(setup (:package company)
  (:hook-into cider-mode
	          cider-repl-mode)
  (:option company-global-modes '(not org-mode)
           company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3))
           company-minimum-prefix-length 1
           company-require-match nil
           completion-ignore-case t) 
  (add-hook 'after-init-hook 'global-company-mode))

(setup (:package yasnippet)
  (add-hook 'prog-mdoe-hook 'yas-minor-mode)
  (yas-global-mode 1))

;; Paredit
(setup (:package paredit)
  (:hook-into emacs-lisp-mode
	          eval-expression-minibuffer-setup
	          ielm-mode
	          lisp-mode
	          lisp-interaction-mode
	          scheme-mode
              cojure-mode
              clojure-ts-mode
              cider-repl-mode
              go-ts-mode
              go-mode
              prog-mode))
(show-paren-mode 1)

;; Rainbow delimiters
(setup (:package rainbow-delimiters)
  (:hook-into prog-mode))

(setup (:package mhtml-mode)
  (:bind "C-c d" html-div
         "C-c s" html-span))


;;;;;;;;
;; GO ;;
;;;;;;;;

(require 'dap-dlv-go)

(use-package go-ts-mode
  :ensure t
  :mode "\\.go\\'"
  :preface
  (defun vd/go-lsp-start()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred)
    )
  :bind
  (:map go-ts-mode-map
        ("C-c g b" . go-dap-setup)
        ("C-c g h" . go-root-setup)
        ("C-c g t" . dap-breakpoint-toggle)
        ("C-c g a" . treesit-beginning-of-defun)
        ("C-c g e" . treesit-end-of-defun)
        ("C-c g i" . prog-indent-sexp)
        ("RET"     . newline-and-indent)
        ("M-RET"   . newline)
        )
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.local/bin")
  (setq lsp-go-analyses '(
                          (nilness . t)
                          (shadow . t)
                          (unusedwrite . t)
                          (fieldalignment . t)
                          (escape . t)
                          )
        lsp-go-codelenses '(
                            (test . t)
                            (tidy . t)
                            (upgrade_dependency . t)
                            (vendor . t)
                            (gc_details . t)
                            (run_govulncheck . t)
                            )
        )
  :hook
  (go-ts-mode . vd/go-lsp-start)
  (go-ts-mode . electric-pair-mode))

(use-package godoctor
  :ensure t
)



;;;;;;;;;;;;
;; COJURE ;;
;;;;;;;;;;;;
;; clojure-mode is (naturally) the major mode for editing
;; Clojure and ClojureScript. subword-mode allows words
;; in camel case to be treated as separate words for
;; movement and editing commands.
;; https://github.com/clojure-emacs/clojure-mode
;; subword-mode is useful for working with camel-case tokens,
;; like names of Java classes (e.g. JavaClassName)
(setup (:package clojure-mode)
  (:hook subword-mode
         paredit-mode
         lsp
         lsp-lens-hide))

;; CIDER is a whole interactive development environment for
;; Clojure. There is a ton of functionality here, so be sure
;; to check out the excellent documentation at
;; https://docs.cider.mx/cider/index.html
(setup (:package cider)
  (:bind "C-c u" cider-user-ns
         "C-M-r" cider-refresh)
  (:option cider-show-error-buffer t
           cider-auto-select-error-buffer t
           cider-repl-history-file "~/.emacs.d/cider-history"
           cider-repl-pop-to-buffer-on-connect t
           cider-repl-wrap-history t))

;; company provides auto-completion for CIDER
;; see https://docs.cider.mx/cider/usage/code_completion.html
(setup (:package company)
  (:hook-into cider-mode
	            cider-repl-mode))

;; hydra provides a nice looking menu for commands
;; to see what's available, use M-x and the prefix cider-hydra
;; https://github.com/clojure-emacs/cider-hydra
(setup (:package cider-hydra)
  (:hook-into clojure-mode))

;; additional refactorings for CIDER
;; e.g. add missing libspec, extract function, destructure keys
;; https://github.com/clojure-emacs/clj-refactor.el
(setup (:package clj-refactor)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (:hook-into clojure-mode))

;; enable paredit in your REPL
(setup cider-repl-mode
  (:hook paredit-mode))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

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
(setup (:package bash-completion)
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

(global-set-key (kbd "C-S-i") 'lsp-format-buffer)

(desktop-save-mode 1)

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
