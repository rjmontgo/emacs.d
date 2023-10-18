
(use-package elixir-mode)

(add-hook 'elixir-mode-hook 'eglot-ensure)

(add-to-list 'eglot-server-programs '(elixir-mode "/Users/rob/.local/bin/elixir-ls/language_server.sh"))

(provide 'xy-elixir)
