
;;; init.el -*- lexical-binding: t; -*-
;;

;; ;; A big contributor to startup times is garbage collection. We up the gc
;; ;; threshold to temporarily prevent it from running, then reset it later by
;; ;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
;; (setq gc-cons-threshold most-positive-fixnum)

;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;;;;; Startup optimizations

;;;;;; Set garbage collection threshold

;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

; (setq max-lisp-eval-depth 32000		; (def 300, 20x)
;       max-specpdl-size 32000			; (def 600, 10x)
;       undo-limit 400000			; min at GC time (def 20,000)
;       undo-strong-limit 500000		; max at GC time (def 30,000)
;       undo-tree-visualizer-timestamps t	; can "go back in time" more easily with them
;       )

;; (setq gc-cons-threshold-original gc-cons-threshold)
;; (setq gc-cons-threshold (* 1024 1024 100))

;; (let ((file-name-handler-alist nil))
;;   ;; temporarily increase garbage collection threshold and turn off file name handler regexp
;;   (setq gc-cons-threshold-original gc-cons-threshold)
;;   (setq gc-cons-threshold (* 1024 1024 100))
;;   ;; (require 'org)
;;   ;; (setq vc-follow-symlinks t)                  ; Just do it
;;   ;; (org-babel-load-file (expand-file-name "settings.org"
;;   ;;                                        user-emacs-directory))

;;   (run-with-idle-timer
;;    5 nil
;;    (lambda ()
;;      (setq gc-cons-threshold gc-cons-threshold-original)
;;      (makunbound 'gc-cons-threshold-original)))
;;   )

;;;;;; Set file-name-handler-alist


;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

;; (setq file-name-handler-alist-original file-name-handler-alist)
;; (setq file-name-handler-alist nil)

;;;;;; Set deferred timer to reset them

;; (run-with-idle-timer
;;  5 nil
;;  (lambda ()
;;    (setq gc-cons-threshold gc-cons-threshold-original)
;;    (setq file-name-handler-alist file-name-handler-alist-original)
;;    (makunbound 'gc-cons-threshold-original)
;;    (makunbound 'file-name-handler-alist-original)
;;    (message "gc-cons-threshold and file-name-handler-alist restored")))

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Load the heart of Poly Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

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

(unless window-system
  (require 'init-iterm2))

;; tools
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

;; programming
;; (require 'init-flycheck)
(require 'init-proto)
(require 'init-projectile)
(require 'init-company)
(require 'init-lsp)
(require 'init-go)
(require 'init-python)
(require 'init-lua)
(require 'init-lisp)
(require 'init-web)
(require 'init-cc)
(require 'init-nix)
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
 )
