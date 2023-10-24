
;; eglot may generate a lot of output so let it have a highger threshold than 4kb
(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :bind
  (("C-c C-c" . completion-at-point)))


(use-package markdown-mode)

(use-package eldoc-box
  :bind (("C-c C-k" . eldoc-box-help-at-point)))


(provide 'xy-eglot)

