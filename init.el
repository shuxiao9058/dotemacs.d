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

(require 'init-meow)

;;; core
;; (require 'init-env)
(require 'init-comp)
;; (require 'init-ivy)
(require 'init-icomplete)
(require 'init-ui) ;; ui
(require 'init-recentf)
;; case show line numbers
(require 'init-linum)

(unless window-system
  (require 'init-iterm2))

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
(require 'init-perspective)
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
(require 'init-yaml)
(require 'init-java)
(require 'init-format)

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

;; (require 'init-misc)

;; keybindings
;; early load with general in core
;; (eval-after-load 'general
;;   `(progn
;;      (require 'init-keybindings)))

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
 '(helm-minibuffer-history-key "M-p")
 ;; '(lsp-go-gopls-server-args
 ;; '("-debug" "127.0.0.1:3000" "-logfile=/tmp/gopls-emacs.log") nil nil "Customized with use-package lsp-mode")
 ;; '(lsp-go-gopls-server-path "/usr/local/bin/gopls" nil nil "Customized with use-package lsp-mode")
 '(smtpmail-smtp-server "smtp.google.com" t)
 '(smtpmail-smtp-service 25 t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types
   '((emacs)
     (emacs)
     (emacs)
     (emacs)
     (emacs)
     (emacs)
     (emacs)
     (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:foreground "#50fa7b"))))
 '(highlight-indentation-face ((t (:inherit default :foreground "#878787"))))
 '(hl-line ((t (:background "DodgerBlue4"))))
 '(icomplete-first-match ((t (:inherit mode-line-emphasis))))
 '(ivy-posframe-cursor ((t (:background "#268bd2"))))
 '(mode-line-buffer-id ((t (:foreground "Light Blue"))))
 '(orderless-match-face-0 ((t (:inherit font-lock-type-face :weight bold))))
 '(orderless-match-face-1 ((t (:inherit error :weight bold))))
 '(orderless-match-face-2 ((t (:inherit font-lock-string-face :weight bold))))
 '(orderless-match-face-3 ((t (:inherit font-lock-keyword-face :weight bold)))))

;; For profiling
;; (profiler-start 'cpu)
;; (profiler-report)
;; (profiler-stop)
