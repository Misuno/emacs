;; INTERFACE
;; (use-package modus-themes
;;   :ensure t)
;; (load-theme 'modus-vivendi-tritanopia t)

;; (setup (:package sourcerer-theme))
;; (load-theme 'sourcerer t)

(add-hook 'org-mode-hook  'org-indent-mode)

;; (use-package spacemacs-theme
;;   :ensure t)
;; (load-theme 'spacemacs-dark t)

;; Fira code font 
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "x")) ; ligatures you don't want
  :config
  (fira-code-mode-set-font)
  (global-fira-code-mode)
  :hook prog-mode)                                         ; mode to enable fira-code-mode in

(set-face-attribute 'default nil :height 130)
(set-face-attribute 'default t :font "Iosevka NFM")
;; (add-to-list 'default-frame-alist '(font . "Fira Code 12" ));; (add-to-list 'default-frame-alist '(font . "JetBrains mono 12" ))

;; DOOM MODELINE
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 28)))


(menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

(tooltip-mode -1)
(set-fringe-mode 10)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-visual-line-mode)


;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Line numbes
(global-display-line-numbers-mode)


(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; treemacs is a tree layout file explorer
;; https://github.com/Alexander-Miller/treemacs
(setup (:package treemacs treemacs-projectile treemacs-magit)
  (:global "M-0" treemacs-select-window
           "M-o" ace-window ;; treemacs brings ace-window as a dependency
           "C-S-b" treemacs))

(treemacs)

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map)
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(setup (:package projectile)
  (projectile-mode +1)
  (:bind "s-p" projectile-command-map
         "C-c p" projectile-command-map))

;; counsel-projectile integrates projectile with
;; counsel's browse-and-select UI
(setup (:package counsel-projectile))

;; shell scripts
(setq-default sh-basic-offset 2
              sh-indentation 2)


;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; NERD ICONS
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))


;;;;;;;;;;
;; IVY ;;
;;;;;;;;

(use-package ivy
  :diminish
  :bind (
	 ;; ("C-s" . swiper)
         :map ivy-minibuffer-map
         ;; ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(setq ivy-initial-inputs-alist nil)

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))


(use-package helpful
  :bind (
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))


(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
