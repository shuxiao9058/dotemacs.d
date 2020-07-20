;;; lisp/init-web.el -*- lexical-binding: t; -*-

;; ** vuejs
(use-package vue-mode
    :straight t
    :commands (vue-mode)
    :mode "\\.vue"
    ;; :config
    ;; (set-face-background 'mmm-default-submode-face nil)
    )

(provide 'init-web)
;;; init-web.el ends here