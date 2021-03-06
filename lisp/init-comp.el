;;; lisp/init-comp.el -*- lexical-binding: t; -*-

(defun native-comp? ()
  (and (functionp 'native-comp-available-p)
       (native-comp-available-p)))

(defun jeff/native-comp-path (path)
  (when (native-comp?)
    (let ((comp-always-compile t))
      (native-compile-async path t nil))))

(defun jeff/native-comp-elpa ()
  (interactive)
  ;; (jeff/native-comp-path "~/.emacs.d/elpa/")
  (jeff/native-comp-path (expand-file-name ".local/straight/build" user-emacs-directory))
  t)

(defun jeff/native-comp-emacs-base ()
  (interactive)
  (jeff/native-comp-path "/usr/local/share/emacs/28.0.50/lisp/")
  (jeff/native-comp-path (expand-file-name "lisp/" user-emacs-directory))
  t)

(defun jeff/native-comp-all ()
  (interactive)
  (jeff/native-comp-emacs-base)
  (jeff/native-comp-elpa)
  t)

(provide 'init-comp)
;;; init-comp.el ends here
