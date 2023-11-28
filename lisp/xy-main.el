;;* Main
;; Configure the base of emacs and elisp editting
;; provide general packages.

;;* Default overrides
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; enable some good ui things
(setq scroll-margin 10)
(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		leetcode--problem-detail-mode-hook
		nov-mode-hook
		Info-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; set a decent default font
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 120)

;; disable backup files
(setq backup-directory-alist '(("." . "~/.cache/emacs/backup")))
(setq backup-by-copying t)


;;; some decent default keybinds
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)

;;; setup OS X homebrew bin directory
(add-to-list 'exec-path "/opt/homebrew/bin" t)

;;* General Emacs Packages
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.8))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-x b" . counsel-switch-buffer)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))


;;* Utilities/simple config
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;* Themeing
(use-package monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro t))


(provide 'xy-main)
