;;; lisp/init-lisp.el -*- lexical-binding: t; -*-

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

;; ;; SLIMEのロード
;; (use-package slime
;;   :straight slime-company
;;   ;; :ensure-system-package (sbcl clisp)
;;   :hook ((lisp-mode . slime-mode)
;; 	 (slime-repl-mode
;; 	  . (lambda () (add-to-list
;; 			'company-backends
;; 			'(company-slime company-dabbrev-code)))))
;;   :config
;;   (setq inferior-lisp-program "clisp")
;;   ;; (slime-setup '(slime-fancy slime-company))
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (slime-setup '(slime-fancy slime-company))
;;   (defun slime-space\\skk-insert (origfun &rest arglist)
;;     "skkの変換(スペース)がslime-spaceに食われてしまうのを回避"
;;     (apply (cond (skk-henkan-mode
;; 		  ;; skk-henkan-mode中であれば(▽▼の時)skk-insertへ処理を投げる
;; 		  #'skk-insert)
;; 		 (t
;; 		  ;; それ以外は通常のslime-space
;; 		  origfun))
;; 	   arglist))
;;   ;; (advice-add 'slime-space :around #'slime-space\\skk-insert)
;;   (advice-add 'slime-autodoc-space :around #'slime-space\\skk-insert))

(use-package profiler
  :straight t
  :defer t
  :config
  :general
  (nmap :keymaps '(profiler-report-mode-map)
    (kbd "<tab>") 'profiler-report-toggle-entry
    (kbd "<backtab>")  'profiler-report-toggle-entry)
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
