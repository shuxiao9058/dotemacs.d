;;; lisp/init-comp.el -*- lexical-binding: t; -*-

(defun jeff/native-comp-path (path)
  (when (native-comp?)
    (let ((comp-always-compile t))
      (native-compile-async path t nil))))

(defun jeff/native-comp-elpa ()
  (interactive)
  (jeff/native-comp-path "~/.emacs.d/elpa/")
  t)

(defun jeff/native-comp-emacs-base ()
  (interactive)
  (jeff/native-comp-path "/usr/local/share/emacs/28.0.50/lisp/")
  t)

(defun jeff/native-comp-all ()
  (interactive)
  (jeff/native-comp-emacs-base)
  (jeff/native-comp-elpa)
  t)

;;; init-comp.el ends here
