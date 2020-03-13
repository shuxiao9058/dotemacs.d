;;; lisp/init-lua.el -*- lexical-binding: t; -*-

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level tab-width)
  (lua-indent-string-contents t)
  :config
  (add-hook 'lua-mode-hook #'lsp-deferred)
  )

;; (use-package company-lua
;;     :straight t
;;     ;; :defines company-backends
;;     :config
;;     (add-to-list 'company-backends 'company-lua)
;;     ;; (cl-pushnew 'company-lua company-backends)
;;     )

; (eval-after-load 'format-all
    
;     )

(provide 'init-lua)
;;; init-lua.el ends here
