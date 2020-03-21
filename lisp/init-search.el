;;; lisp/init-search.el -*- lexical-binding: t; -*-

(use-package color-rg
    :straight (color-rg
	       :host github
	       :repo "manateelazycat/color-rg"
               :files (:defaults "color-rg.el"))
    :demand t
    :after counsel
    :defer t
    :commands (color-rg-search-input color-rg-search-symbol color-rg-search-project color-rg-search-symbol-with-type color-rg-search-project-with-type)
    :custom
    ;; `color-rg' do not kill any buffer
    (color-rg-kill-temp-buffer-p nil)
    :config
    (add-to-list 'evil-emacs-state-modes 'color-rg-mode)
    )

(provide 'init-search)
;;; init-search.el ends here
