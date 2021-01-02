
; ;;; Ensure every installed package is loaded.
; (dolist (package package-selected-packages)
;   (princ package)
;   ;; (message package)
;   ;; (require package)
;   )

; ;;; straight.el
; (setq
;  straight-recipes-gnu-elpa-use-mirror    t
;  straight-repository-branch              "develop"
;  straight-vc-git-default-clone-depth     1
;  ;; straight-enable-use-package-integration nil
;  straight-check-for-modifications        '(find-when-checking)
;  straight-use-package-by-default  t
;  )
; (defvar bootstrap-version)

; (let ((bootstrap-file
;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;       (bootstrap-version 5))
;   (unless (file-exists-p bootstrap-file)
;     (with-current-buffer
;         (url-retrieve-synchronously
;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;          'silent 'inhibit-cookies)
;       (goto-char (point-max))
;       (eval-print-last-sexp)))
;   (load bootstrap-file nil 'nomessage))

; (setq weiss-dumped-load-path load-path
;       weiss-dumped-p t)

; (dolist (package  '(
;                     use-package
;                     company
;                     ivy
;                     counsel
;                     org
;                     which-key
;                     swiper
;                     ivy-prescient
; 		    dracula-theme
;                     doom-modeline
;                     hydra
;                     ))
;   (require package))

; ;; (load-theme 'doom-one-light-theme t t)
; (dump-emacs-portable "~/.emacs.d/emacs.pdmp")
