;;; -*- lexical-binding: t -*-

;; backup folder

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; init lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; bootstrap
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'xy-use-package)
(require 'xy-defaults)

;; gui config
(require 'xy-themes)
(require 'xy-emacs-completion)

;; lsp
(require 'xy-eglot)
(require 'xy-corfu)
(require 'xy-elixir)
(require 'xy-treesitter)

;; org
(require 'xy-org)

;; misc
(require 'xy-nov)
