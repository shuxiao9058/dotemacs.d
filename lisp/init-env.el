;;; lisp/init-env.el -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :straight t
  :demand t
  :if IS-MAC
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)

  ;; ;; TODO remove old path locations
  ;; (setq exec-path (cons "/opt/local/bin" exec-path))
  ;; (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))

  ;; (setq exec-path (cons "/usr/local/opt/make/libexec/gnubin" exec-path))
  ;; (setenv "PATH" (concat "/usr/local/opt/make/libexec/gnubin:" (getenv "PATH")))

  ;; (setq exec-path (cons "/usr/local/opt/texinfo/bin" exec-path))
  ;; (setenv "PATH" (concat "/usr/local/opt/texinfo/bin:" (getenv "PATH")))

  ;; (setq exec-path (cons "/opt/local/gopath/bin" exec-path))
  ;; (setenv "PATH" (concat "/opt/local/gopath/bin:" (getenv "PATH")))
  )

(provide 'init-env)

;;; init-env.el ends here
