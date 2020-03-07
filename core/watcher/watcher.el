;;; core/watcher/watcher.el   -*- lexical-binding: t; -*-

(defvar watcher-init-errors nil
  "List of errors that occurred during watcher initialization.")

(defvar watcher-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, enable watcher debugging features")

(defvar watcher-wrapper-alist
  '((:after . after!) (:ignore . ignore!) (:disable . ignore!))
  "Aliases for wrappers.
Each element of this is of the form (KEY . ALIAS).")

(eval-and-compile
  (defun watcher:wrap-form (wrappers form)
    "Wraps FORM with each wrapper in WRAPPERS.

WRAPPERS are a list of forms to wrap around FORM.

Example:
(watcher:wrap-form '((if (+ 1 1)) (unless (null 1))) '(+ 1 1))

=> (unless (null 1) (if (+ 1 1) (+ 1 1)))
"
    (declare (pure t) (side-effect-free t))
    (if (consp wrappers)
        (watcher:wrap-form (cdr wrappers)
                    (append (car wrappers)
                            (list form)))
      form)))

(eval-and-compile
  (defun watcher:switches-to-wrappers (switches)
    "Convert SWITCHES to WRAPPERS.

SWITCHES is a list of symbols representing org switches. WRAPPERS is an alist in
which each element is of the form (WRAPPER-FN ARGS).

Example:

(watcher:switches-to-wrappers '(:after evil :if watcher-debug-mode))

=> '((after! evil) (if watcher-debug-mode))
"
    (mapcar
     (lambda (pair)
       (let ((key (car pair)) (value (cdr pair)))
         (cons (or (alist-get key watcher:wrapper-alist)
                   (thread-first key
                     (symbol-name)
                     (substring 1)
                     (intern)))
               value)))
     switches)))

(defmacro watcher:elisp-block (path wrappers &rest body)
  "Eval BODY wrapped with WRAPPERS, reporting any errors.
This macro catches and report any errors in the resulting form (of BODY and
WRAPPERS) and pushes them to `watcher-init-errors'. PATH is a the list of headings
\(as strings) in an org mode file that."
  (declare (indent 2))
  (when (string-match-p "disabled"
                        (or (apply #'concat (mapcar #'cadr path)) ""))
    (push '(:disable) wrappers))
  (setq wrappers (watcher:switches-to-wrappers wrappers))
  `(condition-case init-error
       ,(watcher:wrap-form wrappers `(progn ,@body))
     (error
      (let ((path (string-join (nreverse ',(mapcar #'car path)) "->")))
        (push (cons init-error path) watcher-init-errors)
        (message "%S error for %S in source block at %S"
                 (car init-error)
                 (cdr init-error)
                 path)))))

(watcher:elisp-block (("watcher:log" nil) ("pre-package-installation macros" nil) ("Initialize" nil) ("Watcher" ":watcher:")) nil
  (defmacro watcher:log (format-string &rest args)
    "Log to *Messages* if `watcher-debug-mode' is on.
  Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
  Accepts the same arguments as `message'."
    `(when watcher-debug-mode
       (let ((inhibit-message (active-minibuffer-window)))
         (message
          (concat (propertize "watcher " 'face 'font-lock-comment-face)
                  ,format-string)
          ,@args)))))

(defmacro watcher:try-load (module)
  "Try to load the given module, logging an error if unable to load"
  `(condition-case ex
       (require ,module)
     ('error
      (message "watcher: Unable to load [%s] module: %s" ,module ex))))

(provide 'watcher)
;;; watcher.el ends here
