;;; lisp/init-python.el -*- lexical-binding: t; -*-

;; (use-package conda
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :init
;;   (conda-env-initialize-interactive-shells)
;;   ;; if you want eshell support, include:
;;   (conda-env-initialize-eshell)
;;   ;; if you want auto-activation (see below for details), include:
;;   (conda-env-autoactivate-mode t)
;;   :config
;;   ;; WARNING: do NOT set `conda-anaconda-home' instead, as it breaks automatic discovery and use of
;;   ;; Conda environments. It's annoyingly brittle, but using the appropriate conda environment
;;   ;; depends on having this set and NOT `conda-anaconda-home'.
;;   (setq
;;    conda-anaconda-home (expand-file-name "/usr/local/anaconda3/")
;;    conda-env-home-directory (expand-file-name "/usr/local/anaconda3/") ;; as in previous example; not required
;;    conda-env-subdirectory "envs")
;;   ;; display current conda env in the mode line
;;   (add-to-list 'mode-line-misc-info
;;                '(:eval (if conda-env-current-name
;;                            (format " «%s»"
;;                                    (truncate-string-to-width
;;                                     conda-env-current-name
;;                                     15 nil nil "…"))
;;                          "")) t)
;;   )

(setq python-indent-offset 4)

(use-package ein
  :straight t
  :ensure t
  :defer t
  :commands (ein:notebooklist-open ein:notebooklist-login)
  :custom
  (ein:use-smartrep t)
  :config
  (cond
   ((eq system-type 'darwin) (setq ein:console-args '("--gui=osx" "--matplotlib=osx" "--colors=Linux")))
   ((eq system-type 'gnu/linux) (setq ein:console-args '("--gui=gtk3" "--matplotlib=gtk3" "--colors=Linux"))))

  (setq ein:query-timeout 1000)

  (defun load-ein ()
    (ein:notebooklist-load)
    (interactive)
    (ein:notebooklist-open))
  )

(provide 'init-python)
;;; init-python.el ends here
