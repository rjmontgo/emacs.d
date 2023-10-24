

(use-package corfu
  :init
  (setq corfu-auto t)
  :config
  (setq corfu-auto-delay 0.2)
  :hook
  ((elisp-mode . corfu-mode)
   (eglot-managed-mode . corfu-mode)))

(provide 'xy-corfu)
