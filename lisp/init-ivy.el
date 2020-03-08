;;; lisp/init-ivy.el -*- lexical-binding: t; -*-

;; generic completion front-end
(use-package ivy
  :straight t
  :after general
  :commands (ivy-switch-buffer)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-count-format "(%d/%d ")
  (ivy-mode t))

(use-package flyspell-correct-ivy
  :straight t
  :after ivy
  :commands (flyspell-correct-word-generic)
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :general
  (:keymaps '(flyspell-mode-map)
        :states '(normal visual)
        "zs" 'flyspell-correct-word-generic
        "z=" 'flyspell-buffer))

(provide 'init-ivy)
;;; init-ivy.el ends here