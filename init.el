;;; Mu setup

(add-to-list 'load-path "~/.emacs.d/my")

(load "path.el")

(load "packages.el")

;; CUSTOMS
(setq custom-file "custom.el")
(load custom-file)

(load "interface.el")

;; DEVELOPMENT
(load "dev_common.el")

;;;;;;;;
;; GO ;;
;;;;;;;;
(load "go_setup.el")

;;;;;;;;;;;;;;
;; CLOJURE ;;
;;;;;;;;;;;;
(load "clojure_setup.el")

;;;;;;;;;;;;;;
;; FLUTTER ;;
;;;;;;;;;;;;
(load "flutter_setup.el")

;;;;;;;;
;; F# ;;
;;;;;;;;
(load "fsharp_setup.el")

;; OTHER

;; terminal
(setup (:package eat))

;; Bash completion
(use-package bash-completion)
(bash-completion-setup)


;; KEYBINDINGS
(global-set-key (kbd "M-/") 'hippie-expand)


;; NEWLINE
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<C-j>") 'end-of-line-and-indented-new-line)


(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "<M-return>") 'cider-pprint-eval-last-sexp)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; M is set to CMD (much easier)
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-t-enable-clipboard t)

(global-set-key (kbd "C-S-i") 'lsp-format-buffer)
