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

;;** LSP config
;; eglot may generate a lot of output so let it have a highger threshold than 4kb
(setq read-process-output-max (* 1024 1024))

(defun xy--show-focus-unfocus-lsp-ui-doc ()
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (if (frame-focus-state (lsp-ui-doc--get-frame))
          (lsp-ui-doc-unfocus-frame)
        (lsp-ui-doc-focus-frame))
    (lsp-ui-doc-glance)))

(use-package lsp-mode
  :config

  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-icons-enable t)
  ;; :hook (typescript-ts-mode
  ;;        go-ts-mode
  ;;        js-ts-mode
  ;;        java-ts-mode
  ;;        python-ts-mode)
  :bind (("C-c C-k" . xy--show-focus-unfocus-lsp-ui-doc)
         ("C-c C-d" . lsp-find-definition))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config (lsp-ui-doc-mode t)
  :custom
  (lsp-ui-doc-position 'at-point "Set Doc Position to Point"))

(use-package markdown-mode) ; used to render eldoc box properly

(use-package eldoc-box
  :diminish
  :config
  ;; quiet eldoc in the minibuffer
  (setq eldoc-echo-area-use-multiline-p nil))


;;*** Clojure
(use-package clojure-ts-mode
  :mode "\\.clj\\'")

(use-package window-stool
  :init
  (unless (package-installed-p 'window-stool)
    (package-vc-install "https://github.com/JasZhe/window-stool")))

(defun go-mode-setup ()
  (interactive)
  (setq go-ts-mode-indent-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

;;*** go
(use-package go-ts-mode)

;;*** JS/TS/TSX
(setq js-indent-level 2)

;;*** Lisp
(use-package paredit
  :diminish
  :hook (emacs-lisp-mode clojure-ts-mode scheme-mode lisp-mode))

(use-package cider)


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

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

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


