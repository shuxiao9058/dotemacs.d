
;; load the whole configuration
(load (expand-file-name "early-init.el" user-emacs-directory))
(load (expand-file-name "init.el" user-emacs-directory))

;; pdump every packages we marked
(dolist (pkg  poly-pdump-packages)
  (require pkg))

;; backup load-path, restore when startup with dump
(setq poly-pdump-load-path load-path)

;; (princ poly-pdump-load-path)

;; dump image
;; (dump-emacs-portable (expand-file-name "emacs.pdmp" user-emacs-directory))
