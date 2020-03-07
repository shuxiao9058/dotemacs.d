;;; lisp/init-ivy.el -*- lexical-binding: t; -*-

;; generic completion front-end
(use-package ivy
  :straight t
  :delight
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-count-format "(%d/%d ")
  (ivy-mode t))

(provide 'init-ivy)
;;; init-ivy.el ends here