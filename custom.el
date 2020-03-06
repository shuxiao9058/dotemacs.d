;;; custom.el -*- lexical-binding: t; -*-

(setq make-backup-files nil
      inhibit-startup-screen t
      whitespace-line-column 100
      default-directory (expand-file-name "workspace/" "~")
      vagrant-vagrantfile (expand-file-name "vagrant/Vagrantfile" default-directory)
      ;; Wrapping
      truncate-lines t
      truncate-partial-width-windows 50
      ; evil-want-C-u-scroll nil
 whitespace-style '(face trailing lines-tail))

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; custom.el ends here