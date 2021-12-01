;;; lisp/init-perl.el -*- lexical-binding: t; -*-

(use-package cperl-mode
    :straight t
    :ensure t
    :interpreter "perl"
    :preface
    ;;; cperl-mode is preferred to perl-mode
    ;;; "Brevity is the soul of wit" <foo at acm.org>
    (defalias 'perl-mode 'cperl-mode)
    :mode
    (("\\.pl$" . cperl-mode)
     ("\\.pm$" . cperl-mode)
     ("\\.psgi$" . cperl-mode)
     ("Makefile\\.PL$" . cperl-mode)
     ("/t/.+\\.t$" . cperl-mode))
    :init
    (add-hook 'cperl-mode-hook
              (lambda ()
		(auto-fill-mode -1)
		(setq-local require-final-newline nil)
		(setq indent-tabs-mode nil)))
    :config
    ;; comprehensively switch from perl-mode to cperl-mode
    ;; (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
    ;; (add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))

    ;; (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
    ;; (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
    ;; (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
    ;; (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

    (setq cperl-hairy t)          ; Turns on most of cperl options.
    (setq cperl-invalid-face nil) ; Turns off underscore for trailing whitespace.
    (setq cperl-indent-level 4)
    )

(provide 'init-perl)
;;; init-perl.el ends here
