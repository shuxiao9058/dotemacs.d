;;; lisp/init-pair.el -*- lexical-binding: t; -*-

(use-package paredit
    :straight t
    ;; :straight (:type git
    ;; 		   :host github
    ;; 		   :repo "emacsmirror/paredit")
    :ensure t
    :defer t
    :hook ((;; scheme-mode
	    ;; emacs-lisp-mode lisp-mode ielm-mode
	    clojure-mode cider-repl-mode
	    ;; cask-mode
	    ) . paredit-mode)
    :commands paredit-mode enable-paredit-mode
    ;; :config
    ;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    )

;; electric-pair-mode or smartparens-mode

;; keeps our parentheses balanced and allows for easy manipulation
(use-package smartparens
    :straight t
    :ensure t
    :diminish smartparens-mode
    :commands
    smartparens-strict-mode
    smartparens-mode
    sp-restrict-to-pairs-interactive
    sp-local-pair
    :hook  (after-init . smartparens-global-mode)
    :init
    (setq sp-interactive-dwim t)
    :custom
    (sp-base-key-bindings 'sp)
    ;; (sp-show-pair-from-inside t)
    ;; (sp-autoskip-closing-pair 'always)
    ;; (sp-hybrid-kill-entire-symbol nil)
    ;; (sp-autowrap-region nil)
    :config
    (show-smartparens-global-mode t)
    ;; (smartparens-global-mode t)
    (require 'smartparens-config)
    (sp-use-smartparens-bindings)
    ;; Only use smartparens in web-mode
    (with-eval-after-load 'smartparens
      (setq web-mode-enable-auto-pairing nil)
      (sp-local-pair 'web-mode "<% " " %>")
      (sp-local-pair 'web-mode "{ " " }")
      (sp-local-pair 'web-mode "<%= "  " %>")
      (sp-local-pair 'web-mode "<%# "  " %>")
      (sp-local-pair 'web-mode "<%$ "  " %>")
      (sp-local-pair 'web-mode "<%@ "  " %>")
      (sp-local-pair 'web-mode "<%: "  " %>")
      (sp-local-pair 'web-mode "{{ "  " }}")
      (sp-local-pair 'web-mode "{% "  " %}")
      (sp-local-pair 'web-mode "{%- "  " %}")
      (sp-local-pair 'web-mode "{# "  " #}"))

    (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
    (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
    (sp-pair "{" "}" :wrap "C-{")


    ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
    (bind-key "C-<left>" nil smartparens-mode-map)
    (bind-key "C-<right>" nil smartparens-mode-map)

    (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
    (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)

    ;; ;; fix conflict with move frame left/right side
    ;; (unbind-key "C-M-<left>" smartparens-mode-map)
    ;; (unbind-key "C-M-<right>" smartparens-mode-map)
    )

(use-package rainbow-mode
    :straight t
    :diminish rainbow-mode
    :commands rainbow-mode)

(use-package rainbow-delimiters
    :straight t
    :ensure t
    :diminish rainbow-delimiters-mode
    :commands rainbow-delimiters-mode
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

;; (smartparens-strict-mode nil)

;; (use-package autopair
;;     :straight (autopair :host github :repo "joaotavora/autopair")
;;     :hook
;;     ((python-mode go-mode rust-mode scala-mode emacs-lisp-mode php-mode web-mode org-mode prog-mode) . autopair-mode))

(provide 'init-pair)
;;; init-pair.el ends here
