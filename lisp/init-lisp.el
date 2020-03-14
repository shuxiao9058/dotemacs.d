;;; lisp/init-lisp.el -*- lexical-binding: t; -*-

(use-package paredit
    :straight (
               :type git
	       :host github
	       :repo "emacsmirror/paredit")
    :ensure t
    :hook ((scheme-mode
            emacs-lisp-mode lisp-mode ielm-mode
            clojure-mode cider-repl-mode
            cask-mode) . paredit-mode)
    )

(use-package lisp-mode
    :straight nil
    :after paredit
    :ensure nil
    :defer t
    :config
    (defun init-lisp-mode ()
	(setq lisp-body-indent 2)
	(show-paren-mode t)
	(setq show-paren-delay 0)
	(make-variable-buffer-local 'show-paren-style)
	(setq show-paren-style 'parenthesis) ; or parenthesis/expression
	(enable-paredit-mode)
	(setq abbrev-mode t)
	(setq lisp-indent-function 'common-lisp-indent-function))
    :hook
    (lisp-mode . init-lisp-mode)
    (emacs-lisp-mode . init-lisp-mode))

(provide 'init-lisp)
;;; init-lisp.el ends here
