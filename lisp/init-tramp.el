;; lisp/init-tramp.el -*- lexical-binding: t; -*-

(use-package tramp
  :straight (:type built-in)
  :ensure t
  :custom
  (tramp-default-method "ssh")
  (remote-file-name-inhibit-cache t)
  :config
  (customize-set-variable
   'tramp-ssh-controlmaster-options
   (concat
    " -o ControlPath=~/.ssh/ControlMaster/master-%%r@%%h:%%p "
    " -o ControlMaster=auto -o ControlPersist=yes")
   )
  (add-to-list 'tramp-default-user-alist '("ssh" "10\.181\.24\.12" "jy09901"))
  (setq tramp-verbose 6)
  (setq tramp-default-user "jy09901"
	tramp-default-host "10\.181\.24\.12"))

(use-package password-cache
  :straight (:type built-in)
  :ensure nil
  :custom
  ;; Never expire passwords
  (password-cache-expiry nil))

(use-package tramp-sh
  :straight (:type built-in)
  :ensure nil
  :custom
  ;; Use out-of-band method for big files
  (tramp-copy-size-limit (* 0.5 1024 1024))
  :config
  ;; Use the PATH from the remote
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package tramp-gvfs
  :straight (:type built-in)
  :ensure nil
  :if (not IS-MAC)
  ;; :after tramp-ftp
  :config
  ;; ;; Prefer gvfs for FTP
  ;; (add-to-list 'tramp-gvfs-methods "ftp")
  (add-to-list 'tramp-gvfs-methods "dav")
  (add-to-list 'tramp-gvfs-methods "davs"))

(provide 'init-tramp)
;;; init-tramp.el ends here
