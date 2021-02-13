;;; lisp/init-fish.el -*- lexical-binding: t; -*-

(use-package fish-mode
  :straight t
  :ensure t
  :commands fish-mode
  :when (executable-find "fish")
  )

(use-package fish-completion
  :straight t
  :ensure t
  :when (executable-find "fish")
  :commands fish-completion-mode turn-on-fish-completion-mode
  :config
  (global-fish-completion-mode)
  :hook (eshell-mode . turn-on-fish-completion-mode)
  )

;; Terminal
(use-package eshell
  :straight t
  :ensure t
  :preface
  (defun jae--setup-eshell ()
    (setenv "TERM" "emacs"))
  :hook (eshell-mode . turn-on-fish-completion-mode))

(use-package esh-autosuggest
  :straight t
  :commands esh-autosuggest-mode
  :preface
  (defun jae--setup-company-eshell-autosuggest ()
    "Fish-like autosuggestion in Eshell"
    (setq-local company-backends '(company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-if-just-one-frontend))
    (setq-local company-idle-delay 0.5))
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package company-pcomplete
  :straight t
  :disabled
  :after (eshell company)
  :commands company-pcomplete
  :preface
  (defun jae--setup-pcomplete ()
    "It's surprisingly hard to bind keys within eshell-mode"
    (setq company-backends '(company-pcomplete))
    (bind-key "<tab>" 'company-complete eshell-mode-map))
  :hook (eshell-mode . jae--setup-eshell))

(provide 'init-fish)
;;; init-fish.el ends here
