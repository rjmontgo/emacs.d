

(use-package corfu
  :init
  (setq corfu-auto t)
  :config
  (setq corfu-auto-delay 0.2)
  :bind
  ("C-c C-c" . completion-at-point)
  :hook
  ((elisp-mode . corfu-mode)
   (eglot-managed-mode . corfu-mode)))


(provide 'xy-corfu)
