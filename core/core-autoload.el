;;; core/core-autoload.el -*- lexical-binding: t; -*-

(defvar poly-autoload-file (concat poly-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")

(defun poly-load-autoload ()
  "Load `poly-autoload-file'."
  (if (file-exists-p poly-autoload-file)
      (load poly-autoload-file)
    (progn
      (poly/generate-autoload-file)
      (load poly-autoload-file))))

(defun poly/generate-autoload-file ()
  "Extract autload file from each star to `poly-autoload-file'."
  (interactive)
  (let ((autoload-file-list
	 (file-expand-wildcards
          (expand-file-name "*.el" poly-autoload-dir))))
    (dolist (file (reverse autoload-file-list))
      (message
       (cond ((update-file-autoloads file t poly-autoload-file)
              "Nothing in %s")
             (t "Scanned %s"))
       (file-relative-name file user-emacs-directory)))
    ))

(provide 'core-autoload)
;;; core/core-autoload.el ends here
