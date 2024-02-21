(use-package magit)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package lsp-treemacs)

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil))

(use-package flycheck
  :defer 2
  :diminish
  ;; :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay 2)
  (flycheck-stylelintrc "~/.stylelintrc.json")
  (flycheck-highlighting-mode 'symbols))


(setup (:package eglot))

(use-package company)
(setq company-global-modes '(not org-mode))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

(setup (:package yasnippet))
(yas-global-mode 1)

;; Paredit
(setup (:package paredit)
  (:hook-into emacs-lisp-mode
	            eval-expression-minibuffer-setup
	            ielm-mode
	            lisp-mode
	            lisp-interaction-mode
	            scheme-mode))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'clojure-mode-hook          'enable-paredit-mode)
(add-hook 'cider-repl-mode            'enable-paredit-mode)

(show-paren-mode 1)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

