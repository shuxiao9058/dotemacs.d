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

;; Load customization file now
(load (expand-file-name "custom.el" user-emacs-directory) nil t)

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Load the heart of Poly Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; lisp
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; core
(require 'init-env)
(require 'init-ivy)
(require 'init-ui)
(require 'init-recentf)
;;; (require 'init-evil)

;;; tools
(require 'init-wakatime)
(require 'init-keyfreq)
(require 'init-format)
(require 'init-rime)
(require 'init-vterm)
;; (require 'init-dired)

;;; programming
(require 'init-projectile)
(require 'init-vc)
(require 'init-lsp)
(require 'init-go)

;;; keybinds
(require 'init-keybinds)

;;; init.el ends here
