
(defun xy/org-setup ()
  (org-indent-mode))

(use-package org
  :hook
  (org-mode . xy/org-setup))

(provide 'xy-org)
