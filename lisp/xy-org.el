(defun xy/org-setup ()
  (org-indent-mode))

(defun org-jump-to-todo ()
  "Jump to the file where the todo template is stored"
  (interactive)
  (org-capture '(4) "t"))

(defun org-capture-todo ()
  "Quick capture a todo"
  (interactive)
  (org-capture nil "t"))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c C-t" . org-jump-to-todo)
         ("C-c t" . org-capture-todo))
  :hook
  (org-mode . xy/org-setup)
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-capture-bookmark nil)
  (setq org-agenda-files '("~/org/gtd.org"))
  (setq org-confirm-babel-evaluate nil)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
           "* %?\n%i\n%a")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))

(provide 'xy-org)
