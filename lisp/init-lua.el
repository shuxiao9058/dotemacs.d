;;; lisp/init-lua.el -*- lexical-binding: t; -*-

(use-package lua-mode
  :straight t
  :ensure t
  :defer t
  :after (company company-tabnine)
  :custom
  (lua-indent-level tab-width)
  (lua-indent-string-contents t)
  :hook (cua-mode . lua-mode)
  :interpreter ("lua" . lua-mode)
  :mode ("\\.lua$" . lua-mode)
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  ;; :general
  ;; (nvmap :keymaps 'lua-mode-map
  ;;   "TAB" #'lua-goto-forward
  ;;   "C-o" #'lua-goto-backward)
  )

(provide 'init-lua)
;;; init-lua.el ends here
