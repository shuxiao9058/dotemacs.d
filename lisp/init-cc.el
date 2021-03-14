;;; lisp/init-cc.el -*- lexical-binding: t; -*-

;;; C/C++
(use-package cc-mode
  :straight t
  :ensure t
  :mode (
	 ("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
	 ("\\.cxx\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
	 ("\\.x\\'" . objc-mode)
	 ("\\.xm\\'" . objc-mode)
	 ("\\.m\\'" . objc-mode)
	 ("\\.mm\\'" . objc-mode)

	 ;; ("\\.c" . c-mode)
         ;; ("\\.h" . c-mode)
         ;; ("\\.cpp" . c++-mode)
         ;; ("\\.hpp" . c++-mode)
	 ;; ("\\.h\\(h\\|xx\\|pp\\)\\'" . c++-mode)
         ;; ("\\.tpp\\'" . c++-mode)
	 )
  :custom
  (c-offsets-alist '((inline-open           . 0)
                     (brace-list-open       . 0)
                     (inextern-lang         . 0)
                     (statement-case-open   . 4)
                     (access-label          . -)
                     (case-label            . 0)
                     (member-init-intro     . +)
                     (topmost-intro         . 0)
                     (inlambda              . 0) ;; better indentation for lambda
                     (innamespace           . 0) ;; no indentation after namespace
                     (arglist-cont-nonempty . +)))
  ;; :config
  ;; (with-eval-after-load 'lsp-mode
  ;;   (setq lsp-clients-clangd-args
  ;;         '("-j=2"
  ;;           "--background-index"
  ;;           "--clang-tidy"
  ;;           "--completion-style=bundled"
  ;;           "--pch-storage=memory"
  ;;           "--suggest-missing-includes")))
  )

(use-package modern-cpp-font-lock
  :straight t
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))


(use-package cmake-mode
  :straight t
  :ensure t
  :defines (company-backends)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends 'company-cmake)))

;; (use-package clang-format
;;   :straight t
;;   :defer t
;;   ;; :load-path "site-lisp"
;;   :commands (clang-format-buffer)
;;   ;; :config
;;   ;; (setq clang-format-style-option "file")
;;   ;; ;; (bind-key "C-c <down>" 'clang-format-buffer c-mode-base-map)
;;   ;; :bind (:map c-mode-base-map
;;   ;;             ("C-c <down>" . clang-format-buffer)
;;   ;;             )
;;   )

(use-package cpp-auto-include
  :straight   (cpp-auto-include
               :host github
               :repo "emacsorphanage/cpp-auto-include")
  :commands (cpp-auto-include)
  )

(provide 'init-cc)
;;; init-cc.el ends here
