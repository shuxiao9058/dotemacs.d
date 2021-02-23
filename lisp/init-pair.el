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
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  )

(use-package evil-smartparens
  :straight t
  :ensure t
  :diminish evil-smartparens-mode
  :after (evil smartparens)
  :hook (((clojure-mode lisp-mode scheme-mode emacs-lisp-mode) . evil-smartparens-mode)
	 (smartparens-strict-mode . evil-smartparens-mode)
	 (smartparens-enabled-hook . evil-smartparens-mode))
  :config
  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  ;; (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  ;; (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
  ;; (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-pair "'" "'" :actions '(wrap insert))


  (sp-use-smartparens-bindings)
  ;; (sp-use-paredit-bindings)
  ;; Make evil-mc cooperate with smartparens better
  (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
    (unless (memq (car sp--mc/cursor-specific-vars) vars)
      (setcdr (assq :default evil-mc-cursor-variables)
              (append vars sp--mc/cursor-specific-vars))))
  )

(provide 'init-pair)
;;; init-pair.el ends here
