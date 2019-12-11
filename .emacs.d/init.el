;; Load custom emacs code
(load "~/.emacs.d/snippets/xah.el")
;; Global keys for some useful xah commands
(global-set-key (kbd "<f8>") 'xah-open-file-fast)

;; Unicode
(prefer-coding-system 'utf-8-unix)

;; MELPA Repository
;; ==================
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Display column numbers
(setq column-number-mode t)

;; Use js2-mode for editing JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; For elpy
(elpy-enable)

;; Set ace-window global hotkey
(global-set-key (kbd "M-o") 'ace-window)

;; Evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(global-set-key (kbd "<f6>") 'evil-local-mode) ;; Local mode hotkey
(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd "q") nil)

(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)

(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

;; Leader-like functionality using ,(comma)
;; =============================================================================
;; Binding some of the same keys I use with (neo)vim.
(require 'neotree)
(define-key evil-normal-state-map (kbd ",n") 'neotree-toggle)
;; Bookmarks
(define-key evil-normal-state-map (kbd ",rb") 'bookmark-jump)
(define-key evil-normal-state-map (kbd ",rm") 'bookmark-set)

;; Some modes to not start in normal mode
(cl-loop for (mode . state)
      in '((inferior-emacs-lisp-mode . emacs)
        (elfeed-show-mode . emacs) ;; Elfeed
	(nrepl-mode . insert)
	(pylookup-mode . emacs)
	(comint-mode . normal)
	(shell-mode . insert)
	(git-commit-mode . insert)
	(git-rebase-mode . emacs)
	(term-mode . emacs)
	(help-mode . emacs)
	(helm-grep-mode . emacs)
	(grep-mode . emacs)
	(bc-menu-mode . emacs)
	(magit-branch-manager-mode . emacs)
	(rdictcc-buffer-mode . emacs)
	(dired-mode . emacs)
	(wdired-mode . normal))
    do (evil-set-initial-state mode state))

(require 'powerline)
(powerline-center-evil-theme)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 121 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" default)))
 '(package-selected-packages
   (quote
    (neotree yaml-mode powerline powershell go-mode slime markdown-mode magit json-mode js2-mode evil elpy cyberpunk-theme clojure-mode ace-window)))
 '(tool-bar-mode nil))
