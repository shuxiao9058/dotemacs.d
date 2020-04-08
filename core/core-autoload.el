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

;; module &optional module-enabled-p
(defun poly/generate-autoloads-autodefs (file buffer)
  (with-temp-buffer
    (insert-file-contents file)
    (while (re-search-forward "^;;;###autodef *\\([^\n]+\\)?\n" nil t)
      (let* ((standard-output buffer)
             (form    (read (current-buffer)))
             (altform (match-string 1))
             (definer (car-safe form))
             (symbol  (poly-unquote (cadr form))))

        ; (cond ((and (not module-enabled-p) altform)
        ;        (print (read altform)))
        ;       ((memq definer '(defun defmacro cl-defun cl-defmacro))
        ;        (if module-enabled-p
        ;            (print (make-autoload form file))
        ;          (cl-destructuring-bind (_ _ arglist &rest body) form
        ;            (print
        ;             (if altform
        ;                 (read altform)
        ;               (append
        ;                (list (pcase definer
        ;                        (`defun 'defmacro)
        ;                        (`cl-defun `cl-defmacro)
        ;                        (_ type))
        ;                      symbol arglist
        ;                      (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
        ;                              module
        ;                              (if (stringp (car body))
        ;                                  (pop body)
        ;                                "No documentation.")))
        ;                (cl-loop for arg in arglist
        ;                         if (and (symbolp arg)
        ;                                 (not (keywordp arg))
        ;                                 (not (memq arg cl--lambda-list-keywords)))
        ;                         collect arg into syms
        ;                         else if (listp arg)
        ;                         collect (car arg) into syms
        ;                         finally return (if syms `((ignore ,@syms)))))))))
        ;        (print `(put ',symbol 'doom-module ',module)))
        ;       ((eq definer 'defalias)
        ;        (cl-destructuring-bind (_ _ target &optional docstring) form
        ;          (unless module-enabled-p
        ;            (setq target #'ignore
        ;                  docstring
        ;                  (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
        ;                          module docstring)))
        ;          (print `(put ',symbol 'doom-module ',module))
        ;          (print `(defalias ',symbol #',(doom-unquote target) ,docstring))))
        ;       (module-enabled-p (print form)))

        ))))

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
       (file-relative-name file user-emacs-directory)))))

(provide 'core-autoload)
;;; core/core-autoload.el ends here
