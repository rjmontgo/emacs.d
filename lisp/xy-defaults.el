;;; disable certain emacs defaults

;; disable annoying ui things
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq visible-bell nil)
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
		term-shell-hook
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

(provide 'xy-defaults)

