(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

(provide 'xy-python)
