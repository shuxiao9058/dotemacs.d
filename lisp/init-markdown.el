;;; lisp/init-markdown.el -*- lexical-binding: t; -*-

(use-package markdown-mode
    :straight t
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-command "multimarkdown")
    (markdown-enable-math t)
    (markdown-enable-wiki-links t)
    (markdown-asymmetric-header t)
    (markdown-italic-underscore t)
    (markdown-fontify-code-blocks-natively t)
    (markdown-fontify-code-block-default-mode t)
    (markdown-make-gfm-checkboxes-buttons t)
    (markdown-gfm-uppercase-checkbox t)
    )

(use-package markdown-toc
    :straight t
    :commands markdown-toc-generate-toc)

(use-package edit-indirect
    :straight t)

(use-package grip-mode
    :straight t
    :bind (:map markdown-mode-command-map
                ("g" . grip-mode)))

(use-package mmm-mode
    :straight t
    :hook (markdown-mode . mmm-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here
