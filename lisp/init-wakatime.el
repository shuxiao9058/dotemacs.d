;;; lisp/init-wakatime.el -*- lexical-binding: t; -*-

(use-package wakatime-mode
    :straight t
    :init
    (setq +wakatime-hide-filenames t)
    (when IS-MAC
      (setq wakatime-cli-path "/usr/local/bin/wakatime"))
    :hook ((org-mode . wakatime-mode)
           (prog-mode . wakatime-mode))
    ;; :if (not IS-AARCH64)
    :config
    (global-wakatime-mode +1))

(provide 'init-wakatime)

;;; init-wakatime.el ends here
