;;; core-lib.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-expand(filename)
  (load(expand-file-name filename)))

;; attempt to load a feature/library, failing silently
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

;; (defmacro watcher:try-load (module)
;;   "Try to load the given module, logging an error if unable to load"
;;   `(condition-case ex
;;        (require ,module)
;;      ('error
;;       (message "watcher: Unable to load [%s] module: %s" ,module ex))))

(defun eval-after-load-all (my-features form)
  "Run FORM after all MY-FEATURES are loaded.
See `eval-after-load' for the possible formats of FORM."
  (if (null my-features)
      (if (functionp form)
	  (funcall form)
	(eval form))
    (eval-after-load (car my-features)
      `(lambda ()
	 (eval-after-load-all
	  (quote ,(cdr my-features))
	  (quote ,form))))))


(cl-defmacro after-load (pkgs &body body)
  "Waits until all packages are loaded before evaluating body.
Example:
(after-load (ivy counsel projectile)
  (do-stuff))
Expands to:
(with-eval-after-load \"ivy\"
  (with-eval-after-load \"counsel\"
    (with-eval-after-load \"projectile\"
      ...)))"
  (declare (indent 1))
  (if pkgs
      `(with-eval-after-load ,(symbol-name (car pkgs))
         (after-load ,(cdr pkgs) ,@body))
    `(progn ,@body)))

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

;;{Ensure Executables};
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

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

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

;;
;;; Public library

(defun poly-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;; (defun poly-region-active-p ()
;;   "Return non-nil if selection is active.
;; Detects evil visual mode as well."
;;   (declare (side-effect-free t))
;;   (or (use-region-p)
;;       (and (bound-and-true-p evil-local-mode)
;;            (evil-visual-state-p))))


(defun poly-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defmacro poly-log (format-string &rest args)
  "Log to *Messages* if `poly-debug-mode' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same arguments as `message'."
  `(when poly-debug-mode
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "POLY " 'face 'font-lock-comment-face)
                 ;; (when (bound-and-true-p poly--current-module)
                 ;;   (propertize
                 ;;    (format "[%s/%s] "
                 ;;            (poly-keyword-name (car poly--current-module))
                 ;;            (cdr poly--current-module))
                 ;;    'face 'warning))
                 format-string)
        ,@args))))

;; ;;
;; ;; Growl (Mac OS X only)
;; ;;
;; (defun growl-notify (message &optional title)
;;   "Display a Growl MESSAGE. The optional TITLE's default value is \"Emacs\"."
;;   (interactive "Message: ")
;;   (let ((g-title (if (and title (not (eq title ""))) title "Emacs")))
;;     (shell-command
;;      (concat
;;       "growlnotify"
;;       " --image /Applications/MacPorts/EmacsMac.app/Contents/Resources/Emacs.icns"
;;       " --title " (shell-quote-argument g-title)
;;       " --message " (shell-quote-argument message)))))

(defun terminal-notify (message &optional title)
  "Display a Notify MESSAGE. The optional TITLE's default value is \"Emacs\"."
  (interactive "Message: ")
  (let ((g-title (if (and title (not (eq title ""))) title "Emacs")))
    (shell-command
     (concat
      "terminal-notifier "
      " -ignoreDnD "
      ;; " -appIcon /Applications/MacPorts/EmacsMac.app/Contents/Resources/Emacs.icns"
      " -title " (shell-quote-argument g-title)
      " -sender " (shell-quote-argument "org.gnu.Emacs")
      " -message " (shell-quote-argument message)))))

(provide 'core-lib)
;;; core-lib.el ends here
