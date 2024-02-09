;;* Setup

;;** Tree-Sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(clojure "https://github.com/sogaiu/tree-sitter-clojure")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(java "https://github.com/tree-sitter/tree-sitter-java")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(java-mode . java-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (js-mode . js-ts-mode)
        (javascript-mode . js-ts-mode)))

(use-package mhtml-mode
  :config
  (keymap-local-unset "M-o"))

;;** LSP config
;; eglot may generate a lot of output so let it have a highger threshold than 4kb
(setq read-process-output-max (* 1024 1024))

(use-package corfu
  :hook
  ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(3.0 . 1.0))
  :init
  (global-corfu-mode))

;;*** Clojure
(use-package clojure-ts-mode
  :mode "\\.clj\\'")
(use-package cider)

;;*** go
(defun go-mode-setup ()
  (interactive)
  (setq go-ts-mode-indent-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(use-package go-ts-mode)

;;*** JS/TS/TSX
(setq js-indent-level 2)

;;*** Lisp
(use-package paredit
  :diminish
  :hook (emacs-lisp-mode clojure-ts-mode scheme-mode lisp-mode))

;;*** Smart Parens
(defun lispy-smartparens ()
  "Disables some features of smart parens that are annoying
in lisp code like '' or \"\""
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem))

(use-package smartparens-mode
  :ensure smartparens
  :hook (go-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode sly-mrepl-mode)
  :config
  (add-hook 'sly-mrepl-mode-hook #'lispy-smartparens)
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode lisp-interaction-mode))

;;*** Nix
(use-package nix-mode)

;;** Magit (Forge)
(use-package magit
  :ensure t)

(use-package geiser
  :diminish
  :config
  (setq geiser-active-implementations '(guile)))
(use-package geiser-guile
  :diminish)
(use-package sly
  :diminish
  :config
  (setq inferior-lisp-program "sbcl"))



(provide 'xy-prog)

