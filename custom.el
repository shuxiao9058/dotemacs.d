;;; custom.el -*- lexical-binding: t; -*-

(setq make-backup-files nil
      inhibit-startup-screen t
      whitespace-line-column 100
      default-directory (expand-file-name "workspace/" "~")
      vagrant-vagrantfile (expand-file-name "vagrant/Vagrantfile" default-directory)
      ;; Wrapping
      truncate-lines t
      truncate-partial-width-windows 50
      whitespace-style '(face trailing lines-tail))

;;; lisp
(setq ;; lisp-body-indent   2
 lisp-indent-function  'common-lisp-indent-function)
;;; custom.el ends here
