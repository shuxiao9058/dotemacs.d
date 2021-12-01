;;; lisp/init-lua.el -*- lexical-binding: t; -*-

(use-package lua-mode
    :straight t
    :ensure t
    :defer t
    :after (company company-tabnine)
    :custom
    (lua-indent-level tab-width)
    (lua-indent-string-contents t)
    ;; :hook (cua-mode . lua-mode)
    :interpreter (("lua" . lua-mode)
		  ("nse" . lua-mode)
		  )
    :mode (("\\.lua$" . lua-mode) ("\\.nse$" . lua-mode))
    :config
    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    ;; (defun lsp-lua-install-save-hooks ()
    ;;   (add-hook 'before-save-hook #'lsp-format-buffer t t))
    ;; (with-eval-after-load 'lsp
    ;;   (add-hook 'lua-mode-hook #'lsp-lua-install-save-hooks))
    ;; :general
    ;; (nvmap :keymaps 'lua-mode-map
    ;;   "TAB" #'lua-goto-forward
    ;;   "C-o" #'lua-goto-backward)
    )

(provide 'init-lua)
;;; init-lua.el ends here
