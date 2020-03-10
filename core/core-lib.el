;;; core-lib.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-expand(filename)
  (load(expand-file-name filename)))

;; attempt to load a feature/library, failing silently
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if 'require' fails."
  `(require ,feature ,file 'noerror))



;{Ensure Executables};
;; Add any executables that must be found
(defun ensure-executable (exec)
  (unless (executable-find exec)
    (message (concat exec " not found in exec-path"))))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

;;; timestamps in *Messages*
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
                  (if (not (bolp))
                      (newline))
                  (insert (current-time-microseconds)))
        )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'core-lib)
;;; core-lib.el ends here
