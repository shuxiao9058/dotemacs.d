
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

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;;;;;; Set file-name-handler-alist

;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;; Set deferred timer to reset them

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))


;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

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
;; (require 'init-lsp)
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
