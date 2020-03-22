;;; init.el -*- lexical-binding: t; -*-
;;

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Load the heart of Poly Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; (add-to-list 'dynamic-library-alist
;;     '(erime（expand-file-name poly-local-dir "lib/liberime.so"))

;; lisp
(add-to-list 'load-path
	     (concat user-emacs-directory "lisp"))

;;; core
(require 'init-env)
(require 'init-ivy)
(require 'init-ui) ;; ui
(require 'init-recentf)
;;; (require 'init-evil)
;; case show line numbers
(require 'init-linum) 

;;; tools
(require 'init-wakatime)
(require 'init-keyfreq)
(require 'init-format)
(require 'init-rime)
(require 'init-vterm)
;; (require 'init-dired)
(require 'init-search)
(require 'init-telega)

;;; editor
(require 'init-markdown)

;;; programming
(require 'init-projectile)
(require 'init-company)
(require 'init-lsp)
(require 'init-go)
(require 'init-lua)
(require 'init-lisp)
(require 'init-web)

;;; functions
(require 'init-utils)

;;; magit
(require 'init-vc)

;; hydra
(require 'init-hydra)

;;; keybinds
;;; early load with general in core
(require 'init-keybinds)

;; Load customization file now
(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file nil t)))

;;; init.el ends here
