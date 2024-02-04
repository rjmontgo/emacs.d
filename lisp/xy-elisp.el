;;* Elisp functions

;;** send an org-safe link for the current file:line to the kill ring
(defun xy--check-project-path ()
  (if (project-current)
      (file-name-directory (directory-file-name (project-root (project-current))))
    "~/"))

(defun xy--kill-new-org-link ()
  (interactive)
  (save-restriction
    (widen)
    (let ((target-dir (xy--check-project-path))
          (full-path (buffer-file-name))
          (line-number (line-number-at-pos)))
      (kill-new (format "[[file:%s::%d][%s:L%d]]" full-path line-number (file-relative-name full-path target-dir) (line-number-at-pos))))))

(defun xy/tmp ()
  (interactive)
  (message (buffer-file-name)))

(defun xy--kill-new-github-link ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((current-branch (car (vc-git-branches)))
           (root (project-root (project-current)))
           (file-path (file-relative-name (buffer-file-name) root))
           (line-number (line-number-at-pos))
           (str (vc-git-repository-url root))
           (github-url-root (when (string-match "\\(.*\\):\\(.*\\).git" str)
                              (match-string 2 str))))
      (kill-new
       (format "https://github.com/%s/blob/%s/%s#L%s" github-url-root current-branch file-path line-number)))))

(provide 'xy-elisp)
