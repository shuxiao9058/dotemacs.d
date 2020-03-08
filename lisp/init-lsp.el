;;; lisp/init-lsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((java-mode python-mode go-mode scala-mode
		    js-mode js2-mode typescript-mode web-mode) . lsp)
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ;; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :config
  ;; 注册emmy-lua-lsp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (list
                                                           "/usr/bin/java"
                                                           "-cp"
                                                           (expand-file-name  "bin/EmmyLua-LS-all.jar" poly-local-dir)
                                                           "com.tang.vscode.MainKt"))
                    :major-modes '(lua-mode)
                    :server-id 'emmy-lua
                    :priority 2
                    :notification-handlers
                    (lsp-ht
                     ("emmy/progressReport" 'ignore))
                    ))

  (setq lsp-eldoc-hook '(lsp-document-highlight))

  ;; cancel warning
  (advice-add 'lsp-warn
              :around (lambda (orig-func &rest r)
                        (message (apply #'format-message r))))
  )

(use-package lsp-haskell
  :straight t)

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :hook ((lsp-mode-hook . lisp-ui-mode))
  ; :bind (:map lsp-ui-mode-map
  ;             ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ;             ([remap xref-find-references] . lsp-ui-peek-find-references)
  ;             ("C-c u" . lsp-ui-imenu))
  :general
  (nmap
    :keymaps 'lsp-ui-mode-map
    [remap evil-goto-definition] #'lsp-ui-peek-find-definitions
    "gD" #'lsp-ui-peek-find-references)
(general-def
    :keymaps 'lsp-ui-peek-mode-map
    "C-j" 'lsp-ui-peek--select-next
    "C-k" 'lsp-ui-peek--select-prev)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature nil)
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 10000)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-background (doom-color 'base4))
  (lsp-ui-doc-border (doom-color 'fg))
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if IS-GUI
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  )

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :straight t
  :commands company-mode
  :hook (((prog-mode latex-mode) . company-mode)
	        (after-init . global-company-mode))
  :bind (:map company-active-map
              ([return] . nil)
              ("RET" . nil)
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
					; (unless *clangd* (delete 'company-clang company-backends))
  (global-company-mode 1)
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

(use-package company-lsp
  :straight t
  :after company
  :custom (company-lsp-cache-candidates 'auto)
  :config
  (setq
   syntax-checking-enable-by-default t
   lsp-highlight-symbol-at-point nil
   lsp-enable-codeaction nil
   lsp-log-io nil
   lsp-enable-xref t
   lsp-auto-guess-root t
   lsp-diagnostic-package :flymake
   lsp-enable-indentation t
   lsp-enable-completion-at-point t
   lsp-enable-eldoc nil
   lsp-response-timeout 1000
   lsp-file-watch-threshold 150000
   )

  (add-to-list 'lsp-file-watch-ignored "build")


  (setq-default company-frontends
                '(;; company-tng-frontend
                  company-pseudo-tooltip-frontend
                  ;; company-preview-frontend
                  company-echo-metadata-frontend))
  )

(use-package company-quickhelp
  :straight t
  :after company
  :config
  (company-quickhelp-mode))

(use-package company-tabnine
  :straight t
  :commands company-tabnine-start-process
  :ensure t
  :init
  (setq company-tabnine-no-continue nil)
  ;; (setq company-tabnine-log-file-path "/tmp/TabNine.log")
  (setq company-tabnine-executable-args
        '("--client" "emacs" "--log-level" "Error" "--log-file-path" "/tmp/TabNine.log"))
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

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

;; ** vuejs
(use-package vue-mode
  :straight t
  :commands (vue-mode)
  :mode "\\.vue"
  :config
  (set-face-background 'mmm-default-submode-face nil))


(use-package zeal-at-point
  :straight t
  :when (and IS-LINUX (display-graphic-p))
  :defer t)

;;;; disable annoying notifications
(defcustom message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                        "^Ispell process killed$"
                                        ".+expected selector or type assertion, found.+"
                                        ".+expected identifier on left side.+"
                                        "^LSP ::.+"
                                        ".+and \d{1,10} more errors.+"
                                        "Wrote "
                                        "Liberime: start with shared dir" ;;; liberime
                                        )
  "filter formatted message string to remove noisy messages"
  :type '(list string)
  :group 'general)

(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (if (and (stringp formatted-string)
               (cl-some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
          (let ((inhibit-read-only t))
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (insert formatted-string "\n")))
        (progn
          (ad-set-args 0 `("%s" ,formatted-string))
          ad-do-it)))))

(provide 'init-lsp)
;;; init-lsp ends here
