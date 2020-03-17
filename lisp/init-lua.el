;;; lisp/init-lua.el -*- lexical-binding: t; -*-

(use-package lua-mode
    :straight t
    :after company
    :custom
    (lua-indent-level tab-width)
    (lua-indent-string-contents t)
    :config
    (setq-local company-backends
                (let ((b #'company-tabnine))
                  (cons b (remove b company-backends))))
    )

;; (use-package company-lua
;;     :straight t
;;     ;; :defines company-backends
;;     :config
;;     (add-to-list 'company-backends 'company-lua)
;;     ;; (cl-pushnew 'company-lua company-backends)
;;     )

(provide 'init-lua)
;;; init-lua.el ends here
