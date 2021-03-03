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

(use-package evil-smartparens
  :straight t
  :ensure t
  :diminish evil-smartparens-mode
  :after (evil smartparens)
  :hook (((clojure-mode lisp-mode scheme-mode emacs-lisp-mode) . evil-smartparens-mode)
	 ;; (smartparens-strict-mode . evil-smartparens-mode)
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

  ;; Disable single quote matching for lisp modes
  (sp-with-modes sp--lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it serve as
    ;; hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
    (sp-local-pair "`" nil
		   :skip-match (lambda (ms mb me)
				 (cond
				  ((equal ms "'")
				   (or (sp--org-skip-markup ms mb me)
				       (not (sp-point-in-string-or-comment))))
				  (t (not (sp-point-in-string-or-comment)))))))

  :general
  (:keymaps 'evil-smartparens-mode-map
	    "s" nil)
  (:states '(normal visual motion)
	   "zj" #'sp-backward-sexp
	   "z;" #'sp-forward-sexp
	   "zk" #'sp-down-sexp
	   "zl" #'sp-up-sexp

	   "zK" #'sp-end-of-sexp
	   "zL" #'sp-beginning-of-sexp

	   "zJ" #'sp-backward-sexp-end
	   "z:" #'sp-forward-sexp-end)
  (:states 'normal
	   "zst" #'sp-transpose-sexp

	   "zsu" #'sp-unwrap-sexp
	   "zsb" #'sp-backward-unwrap-sexp

	   "zfj" #'sp-backward-slurp-sexp
	   "zfJ" #'sp-backward-barf-sexp

	   "zf;" #'sp-forward-slurp-sexp
	   "zf:" #'sp-forward-barf-sexp

	   "zgn" #'sp-add-to-previous-sexp
	   "zgp" #'sp-add-to-next-sexp

	   "zsw" #'sp-swap-enclosing-sexp
	   "zss" #'sp-splice-sexp
	   "zsd;" #'sp-splice-sexp-killing-forward
	   "zsdj" #'sp-splice-sexp-killing-backward
	   "zsda" #'sp-splice-sexp-killing-around

	   "zsc" #'sp-convolute-sexp

	   "zsj" #'sp-absorb-sexp
	   "zs;" #'sp-emit-sexp

	   "zsJ" #'sp-extract-before-sexp
	   "zs:" #'sp-extract-after-sexp

	   "zsy" #'sp-split-sexp
	   "zsY" #'sp-splice-sexp)
  )

(provide 'init-pair)
;;; init-pair.el ends here
