
(defun xy/org-setup ()
  (org-indent-mode))

(use-package org
  :hook
  (org-mode . xy/org-setup)
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd/index.org" "INBOX")
           "* %?\n%i\n%a"))))

(provide 'xy-org)
