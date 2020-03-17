;;; lisp/init-company.el -*- lexical-binding: t; -*-

(use-package company
    :straight t
    :commands company-mode
    :hook (((prog-mode latex-mode) . company-mode)
	   (after-init . global-company-mode))
    :bind (:map company-active-map
		;; ([return] . nil)
		;; ("RET" . nil)
		("TAB" . company-select-next)
		([tab] . company-select-next)
		("S-TAB" . company-select-previous)
		([backtab] . company-select-previous)
		("C-j" . company-complete-selection))
    :custom
    (company-minimum-prefix-length 1)
    (company-tooltip-align-annotations t)
    (company-begin-commands '(self-insert-command))
    (company-require-match 'never)
    ;; Don't use company in the following modes
    (company-global-modes '(not shell-mode eaf-mode))
    ;; Trigger completion immediately.
    (company-idle-delay 0.2)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (company-show-numbers t)
    :config
    ;; TODO: should we remove the existing backends?
  (make-variable-buffer-local 'company-backends)

    (global-company-mode +1)
    (defun smarter-yas-expand-next-field-complete ()
      "Try to `yas-expand' and `yas-next-field' at current cursor position.
If failed try to complete the common part with `company-complete-common'"
      (interactive)
      (if yas-minor-mode
          (let ((old-point (point))
		(old-tick (buffer-chars-modified-tick)))
            (yas-expand)
            (when (and (eq old-point (point))
		       (eq old-tick (buffer-chars-modified-tick)))
              (ignore-errors (yas-next-field))
              (when (and (eq old-point (point))
			 (eq old-tick (buffer-chars-modified-tick)))
		(company-complete-common))))
	(company-complete-common)))
    ;; :config
    ;; (setq company-idle-delay 0 ; default 0.5
    ;;       company-show-numbers t
    ;;       company-minimum-prefix-length 2 ; default 4
    ;;       company-tooltip-limit 10
    ;;       company-auto-complete-chars nil
    ;;       company-tooltip-align-annotations t
    ;;       company-selection-wrap-around t
    ;;       company-quickhelp-delay nil
    ;;       company-require-match 'never
    ;;       company-dabbrev-downcase nil
    ;;       company-dabbrev-ignore-case nil
    ;;       company-dabbrev-other-buffers t)
    )


(use-package company-posframe
    :straight t
    :after company
    :config
    (company-posframe-mode 1)
    )

(use-package company-flx
    :straight t
    :after company
    :config
    (company-flx-mode 1)
    (setq company-flx-limit 256)
    )

(use-package company-prescient
    :straight t
    :disabled
    :ensure t
    :after company
    :config (company-prescient-mode))



(use-package company-quickhelp
    :straight t
    :after company
    :config
    (company-quickhelp-mode))

(use-package company-tabnine
    :straight t
    :commands company-tabnine-start-process
    :ensure t
    :custom
    (company-tabnine-max-num-results 9)
    :init
    (setq company-tabnine-no-continue nil)
    ;; (setq company-tabnine-log-file-path "/tmp/TabNine.log")
    (setq company-tabnine-executable-args
          '("--client" "emacs" "--log-level" "Error" "--log-file-path" "/tmp/TabNine.log"))
    :config
     (setq-default company-backends
                (cons #'company-tabnine company-backends))
    ;; ;; Enable TabNine on default
    ;; (add-to-list 'company-backends #'company-tabnine)

    ;; Integrate company-tabnine with lsp-mode
    (defun company//sort-by-tabnine (candidates)
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
          candidates
	(let ((candidates-table (make-hash-table :test #'equal))
              candidates-lsp
              candidates-tabnine)
	  (dolist (candidate candidates)
            (if (eq (get-text-property 0 'company-backend candidate)
		    'company-tabnine)
		(unless (gethash candidate candidates-table)
		  (push candidate candidates-tabnine))
              (push candidate candidates-lsp)
              (puthash candidate t candidates-table)))
	  (setq candidates-lsp (nreverse candidates-lsp))
	  (setq candidates-tabnine (nreverse candidates-tabnine))
	  (nconc (seq-take candidates-tabnine 3)
		 (seq-take candidates-lsp 6)))))

    (add-hook 'lsp-after-open-hook
              (lambda ()
		(setq company-tabnine-max-num-results 3)
		(add-to-list 'company-transformers 'company//sort-by-tabnine t)
		(add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))

    ;; ;; https://github.com/hlissner/doom-emacs/issues/1268
    ;;   ;; (cl-pushnew 'company-tabnine (default-value 'company-backends))
    ;;   ;; https://github.com/TommyX12/company-tabnine
    ;;   ;; workaround for company-flx-mode and other transformers
    ;;   ;; company-transformers or plugins that use it (such as company-flx-mode) can interfere with TabNine's sorting.
    ;;   (setq company-tabnine--disable-next-transform nil)
    ;;   (defun my-company--transform-candidates (func &rest args)
    ;;     (if (not company-tabnine--disable-next-transform)
    ;;         (apply func args)
    ;;       (setq company-tabnine--disable-next-transform nil)
    ;;       (car args)))

    ;;   (defun my-company-tabnine (func &rest args)
    ;;     (when (eq (car args) 'candidates)
    ;;       (setq company-tabnine--disable-next-transform t))
    ;;     (apply func args))

    ;;   (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
    ;;   (advice-add #'company-tabnine :around #'my-company-tabnine)
    ;;   ; (set-company-backend! 'prog-mode
    ;;   ;   'company-tabnine 'company-capf 'company-yasnippet)
    )


(provide 'init-company)
;;; init-company.el ends here
