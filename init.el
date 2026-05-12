(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; elpy
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)

;; custom files
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

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
  (auto-save-default nil)

  ;; --VERTICO--
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Don't show line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (setq def-font "CaskaydiaCove Nerd Font Propo")
;; (setq def-font "JetBrainsMono Nerd Font Propo")w
(setq def-font "Iosevka Nerd Font")

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

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  ;; Использовать именно nerd-icons (так как у тебя Iosevka Nerd Font)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  ;; Высота панели (подбери под себя)
  (doom-modeline-height 25)
  ;; Отображать ли номер колонки
  (doom-modeline-column-zero-based t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :defer nil
  :custom
  (evil-undo-system 'undo-redo)
  (evil-bigword "^ \t\r\n")
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package god-mode
  :ensure t
  :defer nil)

(use-package evil-god-state
  :ensure t
  :defer nil
  :config
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  ;; (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
  ;; (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

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
  ;; (projectile-completion-system ':init
  (projectile-mode 1)
  :bind
  ("<f5>" . projectile--find-file)
  ("<f6>" . projectile-switch-project))

;; Expand-region - expand selection
(use-package expand-region
  :ensure t
  :defer nil
  :bind ("C-=" . er/expand-region))

(use-package vertico
  :ensure t
  :defer nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  (setopt vertico-cycle t))

(use-package vertico-multiform
  :ensure nil
  :hook (after-init . vertico-multiform-mode)
  :init
  )

(use-package orderless
  :ensure t
  :custom
  ;; Настраиваем orderless как основной стиль автодополнения
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Для файлов оставляем partial-completion, чтобы работали пути вроде /u/s/l -> /usr/share/local
  (completion-category-overrides '((file (styles partial-completion basic))))
  ) 

(use-package consult
  :ensure t
  :bind (("C-x C-b"   . consult-buffer)      ;; Умный поиск по буферам, закладкам и недавним файлам
         ("C-x r b" . consult-bookmark)    ;; Поиск по закладкам
         ("M-y"     . consult-yank-pop)    ;; Поиск по истории буфера обмена (kill-ring)
         ("C-s"     . consult-line)        ;; Поиск по текущему файлу (замена isearch)
         ("M-g g"   . consult-goto-line)   ;; Переход к строке с превью
         ("M-g o"   . consult-outline)     ;; Поиск по заголовкам (отлично работает в Org/Markdown)
         ("M-s r"   . consult-ripgrep))    ;; Быстрый поиск по содержимому файлов в проекте (нужен установленный ripgrep)
  :config
  ;; Заменяем стандартный регистр поиска на регистр consult
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format))

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
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (gleam "https://github.com/gleam-lang/tree-sitter-gleam")))

 ;;;;;;;;;;;;;;;;
;; DIAGNOSTICS;;
;;;;;;;;;;;;;;;;

;; Встроенный линтер (замена Flycheck)
(use-package flymake
  :ensure nil ;; Встроен в Emacs
  :defer t
  :bind (:map flymake-mode-map
              ;; Быстрая навигация по ошибкам
              ("M-n"   . flymake-goto-next-error)
              ("M-p"   . flymake-goto-prev-error)
              ;; Открыть красивый список всех ошибок в проекте (аналог flycheck-list-errors)
              ("C-c e" . flymake-show-project-diagnostics))
  :custom
  ;; Скрывать счетчики в mode-line, если ошибок нет (делает строку чище)
  (flymake-suppress-zero-counters t)
  ;; Показывать индикаторы ошибок на левом поле
  (flymake-fringe-indicator-position 'left-fringe))

;;;;;;;;;;;;;;;;
;; LSP (EGLOT) ;
;;;;;;;;;;;;;;;;

;; Встроенный LSP-клиент (замена lsp-mode)
(use-package eglot
  :ensure nil ;; Встроен в Emacs 29+
  :defer t
  :hook ((clojure-ts-mode
          clojure-mode
          clojurescript-mode
          elixir-ts-mode
          haskell-mode
          haskell-literate-mode
          gleam-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ;; Твоя привычная кнопка для форматирования
              ("C-S-i" . eglot-format-buffer)
              ;; Дополнительные полезные биндинги
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :custom
  ;; Автоматически выключать сервер, когда закрыт последний файл проекта
  (eglot-autoshutdown t)
  ;; Интеграция со стандартным механизмом поиска ссылок/определений Emacs (xref)
  (eglot-extend-to-xref t)
  ;; Задержка (в секундах) перед отправкой изменений на сервер. 
  ;; Не дает Emacs тормозить при быстром наборе текста.
  (eglot-send-changes-idle-time 0.5)
  ;; Отключаем запись логов общения с сервером для максимальной производительности
  (eglot-events-buffer-size 0))

;; Опционально: Отключаем всплывающую документацию Eglot (eldoc) во время набора текста,
;; если она сильно мельтешит в минибуфере. Документация будет показываться только 
;; если курсор задержится на функции.
(setq eldoc-idle-delay 0.5)

;; Magit - git 
(use-package magit
  :ensure t
  :bind ("C-c m" . magit)
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
  (show-paren-mode 1)
  (add-hook 'paredit-mode-hook
            (lambda () (electric-pair-local-mode -1))))

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

;;;;;;;;;;;;
;; CLOJURE ;;
;;;;;;;;;;;;

(use-package clojure-ts-mode
  :ensure t
  :hook
  (clojure-ts-mode . subword-mode)
  (clojure-ts-mode . paredit-mode)
  :custom
  (clojure-ts-toplevel-inside-comment-form t)
  :config)

;; CIDER is a whole interactive development environment for
;; Clojure. There is a ton of functionality here, so be sure
;; to check out the excellent documentation at
;; https://docs.cider.mx/cider/index.html
(use-package cider
  :defer t 
  :bind
  ("C-c u" . cider-user-ns)
  ("C-M-r" . cider-refresh)
  :custom (cider-show-error-buffer t)
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

(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode haskell-literate-mode) . lsp))

;;;;;;;;;;;
;; GLEAM ;;
;;;;;;;;;;;
;; (use-package gleam-mode
;; :ensure nil ;; nil, потому что загружаешь из локальной папки
;;   :load-path "~/.config/emacs/gleam-mode"
;;   :mode "\\.gleam\\'"
;;   :config
;;   ;; Включаем интеграцию с Tree-sitter при запуске режима
;;   (add-hook 'gleam-mode-hook 
;;             (lambda ()
;;               (when (treesit-ready-p 'gleam)
;;                 (treesit-parser-create 'gleam)))))

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
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; terminal
;; Bash completion
(use-package bash-completion
  :ensure t
  :defer t
  :config
  (bash-completion-setup))


;; KEYBINDINGS

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
(global-set-key (kbd "M-o") 'other-window)

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

;; LATEX
;; Org Export Settings
(use-package org
  :ensure nil
  :defer t
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
  :defer t
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


