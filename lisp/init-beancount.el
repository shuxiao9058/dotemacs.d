;;; lisp/init-beancount.el -*- lexical-binding: t; -*-

(use-package beancount
  :straight (beancount
	     :host github
	     :repo "beancount/beancount-mode")
  :mode (("\\.beancount$" . beancount-mode)
	 ("\\.bean$" . beancount-mode))
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  :config
  (add-hook 'beancount-mode-hook
	    (lambda () (setq-local electric-indent-chars nil)))
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)
  )

(provide 'init-beancount)
;;; init-beancount.el ends here
