
;; eglot may generate a lot of output so let it have a highger threshold than 4kb
(setq read-process-output-max (* 1024 1024))

(use-package eglot)

(use-package markdown-mode)

;; enable eldoc box popup on hover
(use-package eldoc-box
  :hook
  (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(provide 'xy-eglot)
