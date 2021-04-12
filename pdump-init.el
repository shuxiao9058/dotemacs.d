;; Restore the load path
(setq load-path poly-pdump-load-path)

;;; Load the heart of Poly Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; lisp
(add-to-list 'load-path
	     (concat user-emacs-directory "lisp"))

(require 'core-ui)
(require 'core-font)
