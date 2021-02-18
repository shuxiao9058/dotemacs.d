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

;;; core
;; (require 'init-env)
(require 'init-comp)
(require 'init-ivy)
(require 'init-ui) ;; ui
(require 'init-recentf)
;; case show line numbers
(require 'init-linum)

;; (unless window-system
;;   (require 'init-iterm2))

;; ;; tools
(require 'init-wakatime)
(require 'init-keyfreq)
(require 'init-rime)
;; (require 'init-vterm)
(require 'init-dired)
(require 'init-search)
(require 'init-telega)
(require 'init-screencast)
;; (require 'init-media)

;; editor
(require 'init-markdown)
(require 'init-pair)
(require 'init-org)
(require 'init-org-roam)

;; programming
(require 'init-flycheck)
(require 'init-proto)
(require 'init-projectile)
(require 'init-company)

;; language server protocol
(setq with-company-lsp t)
(if with-company-lsp
    (require 'init-lsp)
  (require 'init-eglot))

(require 'init-go)
(require 'init-python)
(require 'init-lua)
(require 'init-lisp)
(require 'init-web)
(require 'init-cc)
(require 'init-nix)
(require 'init-fish)
(require 'init-docker)
(require 'init-format)

;; functions
(require 'init-utils)
(require 'init-session)

;; magit
(require 'init-vc)

;; hydra
(require 'init-hydra)

;; keybindings
;; early load with general in core
(eval-after-load 'general
  `(progn
     (require 'init-keybindings)))

;; Load customization file now
(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file nil t)))
;;; init.el ends here
(put 'scroll-left 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-messenger:use-magit-popup t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe-cursor ((t (:background "#268bd2")))))

;; For profiling
;; (profiler-start 'cpu)
;; (profiler-report)
;; (profiler-stop)
