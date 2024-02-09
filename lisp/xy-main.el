;;* Main
;; Configure the base of emacs and elisp editting
;; provide general packages.

;;* Default overrides
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(global-prettify-symbols-mode t)

;; Disable visible bell in darwin
(when (eq system-type 'darwin)
  (progn (setq visible-bell nil)
         (setq ring-bell-function 'ignore)))

;; enable some good ui things
(setq scroll-margin 10)
(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
                shell-mode-hook
		leetcode--problem-detail-mode-hook
		nov-mode-hook
                vterm-mode-hook
		Info-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; set a decent default font
(set-face-attribute 'default nil :font "Iosevka Comfy:style=Regular" :height 120)

;; disable backup files
(setq backup-directory-alist '(("." . "~/.cache/emacs/backup")))
(setq backup-by-copying t)



;;; some decent default keybinds
(defvar xy/keymap (make-keymap))
(define-minor-mode xy/keys-mode
  "Minor mode to prevent other modes from clobbering my keybinds"
  :init-value t
  :global t
  :keymap xy/keymap)

(add-to-list 'emulation-mode-map-alists
             `((xy/keys-mode . ,xy/keymap)))

(define-key xy/keymap (kbd "M-o") 'other-window)
(define-key xy/keymap (kbd "C-w") 'backward-kill-word)
(define-key xy/keymap (kbd "C-c C-w") 'kill-region)

;;; setup OS X homebrew bin directory
(add-to-list 'exec-path "/nix/var/nix/profiles/default/bin/" t)
(add-to-list 'exec-path "/Users/rob/.nix-profile/bin/" t)
(add-to-list 'exec-path "/opt/homebrew/bin" t)


;; eshell config
(setq eshell-prompt-function
      (lambda ()
        (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground "#98BB6C"))
                (if (eq nil (getenv "IN_NIX_SHELL"))
                    (propertize " 󰘧" 'face `(:foreground "#7E9CD8"))
                  (propertize " 󱄅" 'face `(:foreground "#A3D4D5")))
                (propertize " " 'face `(:foreground "#DCD7BA")))))

(setq eshell-prompt-regexp "^[^#$]* [󰘧󱄅] ")

(use-package direnv
  :diminish)

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode))

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
(use-package kanagawa-theme)

(use-package modus-themes)

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox-light t))

;;* setup the password store
(use-package password-store)
(setq epa-pinentry-mode 'loopback)

(use-package notmuch)

(provide 'xy-main)
