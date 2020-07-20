;;; lisp/init-utils.el -*- lexical-binding: t; -*-

(defun poly-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defun +utils/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, prompt for a known project to search from."
  (interactive "P")
  (let* ((disabled-command-function nil)
         (default-directory
           (if arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects
                                    nil t nil nil (poly-project-root))
                 (user-error "There are no known projects"))
             default-directory)))
    (call-interactively
        #'projectile-ripgrep
     ;;; (cond ((featurep! :completion ivy)  #'+ivy/project-search)
     ;;;       ((featurep! :completion helm) #'+helm/project-search)
     ;;;      (#'projectile-ripgrep))
     )))

(provide 'init-utils)
;;; init-utils.el ends here