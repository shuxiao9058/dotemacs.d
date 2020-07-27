;;; lisp/init-wakatime.el -*- lexical-binding: t; -*-

(use-package wakatime-mode
  :straight t
  :init
  (setq +wakatime-hide-filenames t)
  (when IS-MAC
    (setq wakatime-cli-path "/usr/local/bin/wakatime"
          wakatime-python-bin "/usr/local/anaconda3/bin/python3"))
  :hook ((org-mode . wakatime-mode)
         (prog-mode . wakatime-mode))
  :config
  (global-wakatime-mode +1)
  )

(provide 'init-wakatime)

;;; init-wakatime.el ends here
