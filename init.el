;;; init.el -*- lexical-binding: t; -*-
;;

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Load the heart of Poly Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; lisp
(add-to-list 'load-path
	     (concat user-emacs-directory "lisp"))
;; themes
(add-to-list 'load-path
	     (expand-file-name "themes" user-emacs-directory))

(defconst my-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d")

(defconst my-site-lisp-dir (concat my-emacs-d "site-lisp")
  "Directory of site-lisp")

(defconst my-lisp-dir (concat my-emacs-d "lisp")
  "Directory of lisp.")

(defun my-vc-merge-p ()
  "Use Emacs for git merge only?"
  (boundp 'startup-now))

(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not (my-vc-merge-p)))
    (load (file-truename (format "%s/%s" my-lisp-dir pkg)) t t)))

(require 'init-meow)

;;; core
(require 'init-benchmarking)
(require 'init-env)
;; (require 'init-comp)
;; (require 'init-ivy)
;; (require 'init-icomplete)
;; (require 'init-selectrum)
(require 'init-vertico)
(require 'init-ui) ;; ui
(require 'init-recentf)
;; case show line numbers
(require 'init-linum)
(require 'init-tab-bar)
(require 'init-gpg)
(require 'init-activity-watch)
;; (require 'init-desktop)

;; (unless window-system
;;   (require 'init-iterm2))

;; tools
(require 'init-keyfreq)
(require 'init-wakatime)
(require 'init-vterm)
(require 'init-dired)
(require 'init-rime)
(require 'init-search)
(require 'init-telega)
(require 'init-screencast)
;; (require 'init-media)
(require 'init-tramp)
(require 'init-beancount)

;; editor
(require 'init-markdown)
(require 'init-pair)
(require 'init-org)
(require 'init-org-mind) ;; mindmap
(require 'init-clock)
(require 'init-pomo)
(require 'init-blog)
(require 'init-org-roam)

;; programming
(require 'init-flycheck)
(require 'init-proto)
(require 'init-projectile)
(require 'init-perspective)
(require 'init-company)
(require-init 'init-tabnine-capf)
(require-init 'company-tabnine)
;; (require 'init-tabnine)

;; language server protocol
;; (setq with-company-lsp t)
;; (setq with-company-lsp nil)
(if poly-use-lsp-mode
    (require 'init-lsp)
  (require 'init-eglot))

(require 'init-tridactylrc)
(require 'init-go)
;; (with-eval-after-load 'doom-modeline
;;   (require 'init-go))
(require 'init-python)
(require 'init-nginx)
(require 'init-lua)
(require 'init-lisp)
(require 'init-perl)
(require 'init-web)
;; (require 'init-rest)
(require 'init-cc)
(require 'init-nix)
;; (require 'init-fish)
(require 'init-docker)
(require 'init-yaml)
(require 'init-java)
(require 'init-clojure)
(require 'init-format)
(require 'init-lookup)

;; functions
(require 'init-utils)
(require 'init-session)

;; magit
(require 'init-vc)

;; hydra
(require 'init-hydra)

;; mail
;; (require 'init-mail)
(require 'init-gnus)

(require 'init-pdump)

(require 'init-misc)

;; Load customization file now
(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file nil t)))

;; (let ((personal-file (expand-file-name ".private.el" user-emacs-directory)))
;;   (when (file-exists-p private-file)
;;     (load private-file nil t)))

;;; init.el ends here
(put 'scroll-left 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-messenger:use-magit-popup t)
 '(lsp-go-gopls-server-args
   '("-debug" "127.0.0.1:3000" "-logfile=/tmp/gopls-emacs.log") nil nil "Customized with use-package lsp-mode")
 '(safe-local-variable-values '((org-auto-add-ids-to-headlines-in-file . t)))
 '(session-use-package t)
 '(smtpmail-smtp-server "smtp.google.com" t)
 '(smtpmail-smtp-service 25 t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((use-package) (use-package))))


;; For profiling
;; (profiler-start 'cpu)
;; (profiler-report)
;; (profiler-stop)

;; (frame-parameter nil 'font)

;; (princ (json-encode (font-family-list)))

;; (ash 7 1)

;; (ash 7 -1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
