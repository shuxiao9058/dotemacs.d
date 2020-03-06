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

(add-to-list 'load-path (concat user-emacs-directory "core"))

(require 'core)
(require 'core-def)
(require 'core-packages)

;; lisp
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'init-evil)
(require 'init-ui)
(require 'init-vc)
(require 'init-lsp)

;;; tools
(require 'init-wakatime)
(require 'init-keyfreq)
(require 'init-format)
(require 'init-rime)

;; (require 'init-lsp)
;; ; (require 'init-ivy)

;;; init.el ends here
