(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-blue))
 '(custom-safe-themes
   '("76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" default))
 '(package-selected-packages
   '(magit company color-theme-sanityinc-tomorrow exec-path-from-shell nixfmt zenburn-theme nix-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; default all use-package invocations to ensure by default
;; ie install if not already installed
(setq use-package-always-ensure t)

;; host-specific
(when (string= system-name "nixos" )
  (load "server")
  (unless (server-running-p) (server-start))

  ;; font
  (add-to-list 'default-frame-alist
	       '(font . "DejaVu Sans Mono-14")))

(when (string= system-name "dad" )
  (setq dired-use-ls-dired nil))
;;  (set-frame-font "ComicShannsMono Nerd Font 14" nil t)

;; copy shell path to .app
;;  (when (memq window-system '(mac ns x))
;;    (exec-path-from-shell-initialize)))

(setq-default explicit-shell-file-name "/run/current-system/sw/bin/zsh")
(setq-default shell-file-name "/run/current-system/sw/bin/zsh")

;; gui config
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(electric-indent-mode 1)
(setq initial-buffer-choice "~/")

;; keybinds
;; org mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; packages without specific config
(use-package magit)

;; company
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
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
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))
;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; nix-mode config
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

;; pretty on save
;; elisp
(defun autoindent-indent-whole-buffer ()
  (indent-region (point-min) (point-max)))

(defvar autoindent-modes-list '(emacs-lisp-mode lisp-mode)
  "Modes on which to auto-indent after save.")

(defun autoindent-save-hook ()
  (when (member major-mode autoindent-modes-list)
    (autoindent-indent-whole-buffer)))

(add-hook 'before-save-hook #'autoindent-save-hook)

;; nix
(add-hook 'nix-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'nix-format-buffer nil t)))

;; custom macros

;; kill invisible buffers
(defun kill-background-buffers ()
  "Kill all buffers not currently visible in any window."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (get-buffer-window buffer 'visible)
      (kill-buffer buffer))))

(global-set-key (kbd "C-c k") 'kill-background-buffers)
