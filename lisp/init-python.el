;;; lisp/init-python.el -*- lexical-binding: t; -*-

(use-package conda
  :straight t
  :ensure t
  :init
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  :config
  (setq
   conda-anaconda-home (expand-file-name "/usr/local/anaconda3/")
   conda-env-home-directory (expand-file-name "/usr/local/anaconda3/") ;; as in previous example; not required
   conda-env-subdirectory "envs"))

(setq python-indent-offset 4)

(provide 'init-python)
;;; init-python.el ends here
