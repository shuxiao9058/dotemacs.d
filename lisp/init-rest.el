;;; lisp/init-rest.el -*- lexical-binding: t; -*-

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package restclient-test
  :straight t
  :diminish
  :hook (restclient-mode . restclient-test-mode))

;; (use-package company-restclient
;;   :straight t
;;   :defines company-backends
;;   :init (add-to-list 'company-backends 'company-restclient))

(provide 'init-rest)
;;; init-rest.el ends here
