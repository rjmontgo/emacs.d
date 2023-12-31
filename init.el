;;* init.el

;;** Turning off silly defaults
(setq backup-directory-alist '(("." . "~/.cache/emacs/backup")))
(setq backup-by-copying t)

;;** use-package init/config
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;** UI tweaks
;;*** disable all the crap I don't need
(setq visible-bell nil) ; don't ever beep at me
(setq inhibit-startup-message t)
(setq scroll-margin 10)
(set-fringe-mode 10)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;*** Line Numbering
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers in certain modes
(dolist (mode '(org-mode-hook
		term-shell-hook
		eshell-mode-hook
		Info-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;*** theme-ing
;;(use-package nano-theme)
;;(use-package nano-modeline)
(use-package hc-zenburn-theme)
;;(setq nano-modeline-position 'nano-modeline-footer)
;;(nano-modeline-prog-mode t)
;;(load-theme 'nano-dark t)
(load-theme 'hc-zenburn t)


;;*** Set fonts
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 110)
;;(set-face-foreground 'font-lock-string-face nano-dark-popout)

;;*** Rainbow Delims
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;** Emacs Keybindings
(global-set-key (kbd "M-o") 'other-window)

;;** Packages
;;*** Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;*** Diminish (hide minor modes)
(use-package diminish)
;;*** Ivy (Emacs minibuffer Completion)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;;*** Counsel
(use-package counsel
  :bind (("C-x b" . counsel-switch-buffer)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))

;;*** Projectile

;;*** org mode
(defun xy/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(defun xy/org-font-setup ()
  ;; replace hypens with bullets
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "∙"))))))

  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
 		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "FiraCode Nerd Font Mono" :weight 'regular :height (cdr face)))

  ;; Make certain things like code blocks fixed pitch in org
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package org
  :hook (org-mode . xy/org-mode-setup)
  :config
  (setq org-ellipsis " ⏷")
  (xy/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
  ;;  :custom
  ;;(org-bullets-bullet-list '("⊙" "○" "⏺" "○" "⏺" "○" "⏺")))

(defun xy/org-mode-visual-fill ()
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((org-mode . xy/org-mode-visual-fill)
	 (help-mode . xy/org-mode-visual-fill)
	 (Info-mode . xy/org-mode-visual-fill)))


;;*** guix-emacs
(use-package guix)
;;*** racket mode
(use-package racket-mode)
;;*** schemin'
(use-package geiser)
(use-package geiser-racket)
(use-package geiser-guile)

;;** lsp
(defun xy/lsp-mode-setup ()
  (setq lsp-header-line-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode xy/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . lsp-deferred))

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;*** Tree-Sitter
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
<<<<<<< HEAD
   '(evil sicp visual-fill-column org-bullets racket-mode guix emacs-guix hc-zenburn-theme zenburn-theme zenburn ivy-rich which-key rainbow-delimiters nano-modeline diminish counsel ivy nano-theme)))
=======
   '(treesit company-box company lsp-mode geiser-guile geiser-racket visual-fill-column org-bullets racket-mode guix emacs-guix hc-zenburn-theme zenburn-theme zenburn ivy-rich which-key rainbow-delimiters nano-modeline diminish counsel ivy nano-theme)))
>>>>>>> da5cf476d7e39a9809a492158038d449db26e2e3
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
