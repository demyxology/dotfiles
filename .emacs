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
;; (setq scroll-conservatively 100)
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

;; keybinds
;; text manipulation
(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "C-c r") 'align-regexp)
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
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c =") 'balance-windows)

;; development
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c c") 'compile)

;; lsp
(global-set-key (kbd "C-c d") 'lsp-find-definition)
(global-set-key (kbd "C-c r") 'lsp-find-references)

;; evil keybinds
;; (evil-define-key 'visual 'global (kbd "ga") 'align)

;; use-package bootstrap & packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package marginalia
  :config (marginalia-mode))
(use-package orderless
  :custom (completion-styles '(orderless basic)))
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 2))
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package which-key
  :config
  (which-key-mode))
(use-package magit)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
(use-package helpful)
(use-package ace-window
  :bind (("M-o" . ace-window)))
(use-package smartparens
  :hook (prog-mode . smartparens-mode))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package dired-sidebar)
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
;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
(use-package rcirc
  :config
  (add-to-list 'rcirc-server-alist
               '("irc.oftc.net"
                 :channels ("#cat-v"))))
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-clang-args '("--header-insertion=never"
                              "--clang-tidy"
                              "--completion-style=detailed")))
(use-package nerd-icons)
(load-file "~/.emacs.d/acme-mouse.el")

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

;; evil
;; (use-package evil
;;   :init      ;; tweak evil's configuration before loading it

;;   (setq evil-want-keybinding nil)
;;   (setq evil-vsplit-window-right t)
;;   (setq evil-split-window-below t)
;;   (setq evil-undo-system 'undo-redo)
;;   :config    ;; tweak evil after loading it
;;   (evil-mode 1)
;;   (setcdr evil-insert-state-map nil)
;;   (define-key evil-insert-state-map [escape] 'evil-normal-state) 
;;   ;; example: make j/k move by visual lines, not actual lines
;;   (evil-define-key 'normal 'global
;;     "j" 'evil-next-visual-line
;;     "k" 'evil-previous-visual-line))

;; ;; For consistent evil keybindings across the rest of Emacs
;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; ;; Enable surround functionality (like vim-surround)
;; (use-package evil-surround
;;   :after evil
;;   :config
;;   (global-evil-surround-mode 1))

;; ;; Enable commenting operators (like gcc to comment lines)
;; (use-package evil-commentary
;;   :after evil
;;   :config
;;   (evil-commentary-mode))

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

;; consult
(use-package consult
  :bind (
    ;; C-x bindings in global map
    ("C-x b" . consult-buffer)                  ; Enhanced buffer switching
    ("C-x 4 b" . consult-buffer-other-window)   ; Buffer in other window
    ("C-x r b" . consult-bookmark)              ; Bookmarks
    ("C-x p b" . consult-project-buffer)        ; Project buffer
    
    ;; Search related
    ("C-s" . consult-line)                      ; Search current buffer
    ("M-s g" . consult-grep)                    ; Search with grep
    ("M-s r" . consult-ripgrep)                 ; Search with ripgrep
    ("M-s l" . consult-line)                    ; Line search
    ("M-s f" . consult-find)                    ; Find file
    ("M-s i" . consult-imenu)                   ; Jump to symbol
    
    ;; Register access
    ("M-g i" . consult-imenu)                   ; Jump to heading/function
    ("M-g o" . consult-outline)                 ; Jump to outline
    ("M-g m" . consult-mark)                    ; Jump to mark
    ("M-g k" . consult-global-mark)             ; Jump to global mark
    
    ;; More convenient M-y
    ("M-y" . consult-yank-pop)                  ; Enhanced kill ring
    
    ;; Multi-file search
    ("M-s e" . consult-isearch-history)         ; Search history
    
    ;; Isearch integration
    :map isearch-mode-map
    ("M-e" . consult-isearch-history)           ; Search history
    ("M-s e" . consult-isearch-history)         ; Search history
    ("M-s l" . consult-line))                   ; Search line
  
  :config
  ;; Configure preview behavior
  (setq consult-preview-key 'any)           ; Preview on any key
  (setq consult-project-function nil))

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
(use-package lsp-mode
  :commands lsp
  :hook ((c-mode . lsp)
         (c++-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-modeline-diagnostics-enable t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-doc-winum-ignore nil
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-diagnostic-max-lines 5
        lsp-ui-sideline-diagnostic-max-line-length 128
        lsp-ui-peek-enable t))

;; Make sure lsp-ui hooks into lsp-mode
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

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
