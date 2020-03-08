;;; core-packages.el -*- lexical-binding: t; -*-

;;; core packages here
(use-package general
  :straight t
  ;; :after evil
  )
;;
;;; Packages
(use-package which-key
  :straight t
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 4
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

(use-package hydra
  :straight t
  :init
  (setq hydra-if-helpful t))

;;; magit
(use-package hide-mode-line
    :straight t
    :commands (hide-mode-line-mode))

(provide 'core-packages)