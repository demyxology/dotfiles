;;; ...  -*- lexical-binding: t -*-
;; gc tuning for startup
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; mepla
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

;; config
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-hl-line-mode t)
(setq select-enable-clipboard t)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(delete-selection-mode 1)
(set-frame-font "Berkeley Mono-14" nil t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq default-directory (expand-file-name "~/"))
(setq command-line-default-directory (expand-file-name "~/"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key y-or-n-p-map (kbd "RET") 'act)
(unless (assoc "xterm-ghostty" term-file-aliases)
  (add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-256color")))
(electric-pair-mode 1)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; keybinds
;; text manipulation
(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "C-,") 'duplicate-line)
(global-set-key (kbd "C-.") 'kill-whole-line)
(global-set-key (kbd "C-c ,") 'copy-from-above-command)
(global-set-key (kbd "C-c t") 'transpose-lines)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c e") 'load-file)

;; navigation
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "M-g b") 'beginning-of-defun)
(global-set-key (kbd "M-g e") 'end-of-defun)

;; window management
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)

;; development
(global-set-key (kbd "C-c c") 'compile)

;; lsp
(global-set-key (kbd "C-c r") 'eglot-rename)
(global-set-key (kbd "C-c h") 'eldoc)
(global-set-key (kbd "C-c d") 'xref-find-definitions)
(global-set-key (kbd "C-c x") 'xref-find-references)

;; use-package bootstrap & packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package orderless
  :custom (completion-styles '(orderless basic)))
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 2))
(use-package which-key
  :config
  (which-key-mode))
(use-package magit)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
(use-package helpful)
(use-package ace-window
  :bind (("M-o" . ace-window)))
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nixpkgs-fmt)
(add-hook 'nix-mode-hook 'nix-nixfmt-bin)
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))
(use-package rcirc
  :config
  (add-to-list 'rcirc-server-alist
               '("irc.oftc.net"
                 :channels ("#cat-v"))))
(use-package flycheck
  :init (global-flycheck-mode))
(use-package nerd-icons)
(load-file "~/.emacs.d/acme-mouse.el")

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-items '((recents  . 5)))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-preselect 'prompt)
  :config
  (setq completion-auto-help t)
  (setq completion-show-inline-help t)
  (setq completion-auto-select 'completion-at-point))

(use-package tree-sitter
  :hook (prog-mode . tree-sitter-mode)
  :config
  (use-package tree-sitter-langs))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; LSP
(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (setq eglot-sync-connect nil)
  (setq eglot-connect-timeout 30)
  (setq eglot-report-progress t)
  (setq eglot-extend-to-xref t)
  (setq-default eglot-workspace-configuration
                '((:c :formatting (:indentWidth 4))
                  (:cpp :formatting (:indentWidth 4))
                  (:c :indexing (:trackReferences t))
                  (:cpp :indexing (:trackReferences t))))

(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

;; LSP tuning
(setq read-process-output-max (* 1024 1024))

;; galaxy scrolling
(defun galaxy-brain-scroll-amount ()
  (let* ((window-ypos (nth 1 (window-edges)))
         (screen-ypos (cddr (mouse-position)))
         (amount (max 1
                      (- screen-ypos (if (null header-line-format)
                                         window-ypos
                                       (1+ window-ypos))))))
    amount))

(defun my-mouse-wheel-scroll (event)
  (interactive "e")
  (let ((mouse-wheel-scroll-amount (list (galaxy-brain-scroll-amount))))
    (mwheel-scroll event)))

(global-set-key [wheel-up] 'my-mouse-wheel-scroll)
(global-set-key [wheel-down] 'my-mouse-wheel-scroll)
