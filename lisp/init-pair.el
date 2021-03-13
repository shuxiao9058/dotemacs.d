;;; lisp/init-pair.el -*- lexical-binding: t; -*-

;; (use-package paredit
;;     :straight (
;; 	       :type git
;; 	       :host github
;; 	       :repo "emacsmirror/paredit")
;;     :ensure t
;;     :defer t
;;     :hook ((scheme-mode
;; 	    emacs-lisp-mode lisp-mode ielm-mode
;; 	    clojure-mode cider-repl-mode
;; 	    cask-mode) . paredit-mode)
;;     :commands paredit-mode enable-paredit-mode
;;     :config
;;     (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;     )

;; electric-pair-mode or smartparens-mode

;; keeps our parentheses balanced and allows for easy manipulation
(use-package smartparens
  :straight t
  :ensure t
  :diminish smartparens-mode
  :hook  (after-init . smartparens-global-mode)
  :custom
  (sp-base-key-bindings 'sp)
  (sp-show-pair-from-inside t)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol nil)
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (require 'smartparens-config)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  ;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

  (smartparens-strict-mode nil)
  )

(provide 'init-pair)
;;; init-pair.el ends here
