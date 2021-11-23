;;; lisp/init-nginx.el -*- lexical-binding: t; -*-

(use-package nginx-mode
    :straight t
    :ensure t
    :mode (("nginx\\.conf\\'" . nginx-mode)
	   ;; ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
           ("/nginx/.+\\.conf\\'" . nginx-mode))
    :custom
    (nginx-indent-level 4)
    (nginx-indent-tabs-mode nil))

(provide 'init-nginx)
;;; init-nginx.el ends here
