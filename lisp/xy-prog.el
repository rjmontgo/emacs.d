;;* Setup
;; - tree-sitter
;; - eglot (lsp)
;; - various language modes
;; - completion via corfu
;; - magit (forge?)

;;** Tree-Sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(clojure "https://github.com/sogaiu/tree-sitter-clojure")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
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
	(java-mode . java-ts-mode)))

;;** Eglot/LSP config
;; eglot may generate a lot of output so let it have a highger threshold than 4kb
(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :config
  ;; tell eglot to chill out
  (setq eglot-report-progress nil))

(use-package markdown-mode) ; used to render eldoc box properly 

(use-package eldoc-box
  :config
  ;; quiet eldoc in the minibuffer
  (setq eldoc-echo-area-use-multiline-p nil)
  :bind
  (("C-c C-k" . eldoc-box-help-at-point)))

;;** LSP Mode config
(use-package lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-icons-enable nil))
(use-package company)
(use-package lsp-ui)

;;*** Java/lsp /non-eglot work
;;; no config needed at this time
(add-to-list 'exec-path "/etc/profiles/per-user/rob/bin/java")
(use-package lsp-java
  :hook (java-ts-mode . lsp-mode))
;;*** Clojure
(use-package clojure-ts-mode
  :mode "\\.clj\\'")

;;*** Lisp
(use-package paredit
  :hook (emacs-lisp-mode clojure-ts-mode))

;;*** Nix
(use-package nix-mode)



;;** Corfu/Completion
(use-package corfu
  :init
  (setq corfu-auto nil)
  :config
  (setq corfu-auto-delay 0.2)
  :bind
  ("C-c C-c" . completion-at-point)
  :hook
  ((elisp-mode . corfu-mode)
   (eglot-managed-mode . corfu-mode)))

;;** Magit (Forge)
(use-package magit
  :ensure t)


(provide 'xy-prog)
