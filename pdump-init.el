;; ;; For profiling
;; (profiler-start 'cpu)

;; Restore the load path
(setq load-path poly-pdump-load-path)

;; ;;; Load the heart of Poly Emacs
;; (load (concat user-emacs-directory "core/core")
;;       nil 'nomessage)

;; ;; lisp
;; (add-to-list 'load-path
;; 	     (concat user-emacs-directory "lisp"))

;; (require 'core-ui)
(require 'core-frame)
(require 'core-font)

(with-eval-after-load 'doom-modeline
  (require 'init-go))
(require 'init-org-roam)

;; fix env issue
;; PATH variable, etc
(when IS-MAC
  (require 'init-env)
  (exec-path-from-shell-initialize)
  )

;; These two modes are disabled in pdump
(global-font-lock-mode t)
(transient-mark-mode t)

;; (profiler-report)
;; (profiler-stop)
