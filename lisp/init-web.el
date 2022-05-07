;;; lisp/init-web.el -*- lexical-binding: t; -*-

;; ** vuejs
(use-package vue-mode
  :straight t
  :commands (vue-mode)
  :mode "\\.vue"
  ;; :config
  ;; (set-face-background 'mmm-default-submode-face nil)
  )



;; 设置缩进级别空格数
(defvar-local my/web-mode-offset 2)

(defun my/current-buffer-suffix()
  "Return suffix of current buffer."
  (nth 0 (cdr (split-string (buffer-name) "\\."))))

(use-package web-mode
  :straight t
  :hook
  (web-mode . (lambda()
		(if (string= (my/current-buffer-suffix) "vue")
		    (setq web-mode-style-padding 0
			  web-mode-script-padding 0))

		;; 设置缩进级别
		(setq web-mode-markup-indent-offset my/web-mode-offset)
		(setq web-mode-css-indent-offset my/web-mode-offset)
		(setq web-mode-code-indent-offset my/web-mode-offset)
		(setq web-mode-attr-indent-offset my/web-mode-offset)))
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.vue\\'" . web-mode)
	 ("\\.jinja\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html$" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-comment-style 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-comment-keywords t)
  (web-mode-enable-current-element-highlight t)
  ;; (js2-basic-offset my/web-mode-offset)
  ;; (js-indent-level my/web-mode-offset)
  ;; (company-tooltip-align-annotations t)
  ;; (sgml-basic-offset my/web-mode-offset)
  )



;; TypeScript
(defun my/setup-tide-mode ()
  "Setup tide mode used in \\<keymap\\>>."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :straight t
  :commands tide-setup
  :hook
  (before-save . tide-format-before-save)
  (typescript-mode . setup-tide-mode)
  :after (web-mode company)
  :custom
  (typescript-indent-level 2)
  (tide-format-options '(:indentSize 2 :tabSize 2)))

;; (use-package tide
;;     :straight t
;;     :hook
;;     (before-save . tide-format-before-save)
;;     (typescript-mode . setup-tide-mode)
;;     (web-mode .
;;               (lambda ()
;;		(when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                   (my/setup-tide-mode))))
;;     :config
;;     (flycheck-add-mode 'typescript-tslint 'web-mode)
;;     :after (web-mode flycheck company))


(use-package company-web
  :straight t)


(use-package typescript-mode
  :straight t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :custom
  (typescript-indent-level 2)
  :hook
  ((typescript-mode . subword-mode)
   (typescript-mode . lsp)
   (typescript-mode . (lambda ()
			(require 'tide)
			(tide-setup))))
  :mode
  ("\\.tsx?\\'" . typescript-tsx-mode))

(use-package prettier-js
  :straight t
  :commands (prettier-js-mode prettier)
  :custom
  (prettier-target-mode "js-mode")
  (prettier-js-args
   '("--trailing-comma" "all" "--single-quote" "--semi" "--arrow-parens" "always"))
  :hook ((js-mode . prettier-js-mode)
	 (typescript-mode . prettier-js-mode)
	 (web-mode . prettier-js-mode)))


(use-package js
  :straight (:type built-in)
  :mode ("\\.js$" . js-mode)
  :hook
  ((js-mode . lsp)
   (js-mode . (lambda ()
		(require 'tide)
		(tide-setup)))))

(use-package tagedit
  :straight t
  :ensure t
  :commands tagedit-mode
  :config
  (tagedit-add-paredit-like-keybindings)
  ;; (add-hook 'web-mode-hook 'tagedit-mode)
  :hook
  (((sgml-mode html-mode) . tagedit-mode)))

(provide 'init-web)
;;; init-web.el ends here
