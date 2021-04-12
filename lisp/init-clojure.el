;;; lisp/init-clojure.el -*- lexical-binding: t; -*-

(use-package clojure-mode
  :straight t
  :commands (clojurescript-mode)
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package clojure-mode-extra-font-locking
  :straight t
  :ensure t
  :after clojure-mode)

(use-package cider
  :straight t
  :ensure t
  :commands cider-mode
  :config
  (setq nrepl-popup-stacktraces nil)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package flycheck-clojure
  :straight t
  :ensure t
  :commands clojure-mode
  :config
  (flycheck-clojure-setup))

(provide 'init-clojure)
;;; init-clojure.el ends here
