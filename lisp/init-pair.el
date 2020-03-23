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
    :config
    (require 'smartparens-config)
    )

(use-package evil-smartparens
    :straight t
    :ensure t
    :diminish evil-smartparens-mode
    :after smartparens
    :hook ((clojure-mode lisp-mode scheme-mode emacs-lisp-mode) . evil-smartparens-mode)
    :config
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars))))
    )

(provide 'init-pair)
;;; init-pair.el ends here
