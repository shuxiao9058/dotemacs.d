;;; lisp/init-prog.el -*- lexical-binding: t; -*-

(use-package yasnippet
  :straight t
  :ensure t
  :diminish yas-global-mode
  :commands yas-global-mode
  :hook (after-init . yas-global-mode)
  ;; :config
  ;; (add-to-list 'yas-snippet-dirs
  ;; 		 (expand-file-name "snippets" poly-etc-dir))
  ;; make company break completion
  ;; (setq company-continue-commands (-snoc company-continue-commands 'yas-insert-snippet))
  )

(use-package yasnippet-snippets
  :straight t
  :ensure t
  :after yasnippet)


(use-package java-snippets
  :straight t
  :defer t
  :after yasnippet)

(use-package javadoc-lookup
  :straight t)

(provide 'init-prog)
;;; init-prog.el ends here
