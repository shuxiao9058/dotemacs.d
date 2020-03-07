;;; lisp/init-wakatime.el -*- lexical-binding: t; -*-

(use-package wakatime-mode
  :straight t
  ;; :after-call pre-command-hook
  :init (setq +wakatime-hide-filenames t)
  :hook ((org-mode . wakatime-mode)
         (prog-mode . wakatime-mode))
  :custom ((wakatime-cli-path "/usr/local/bin/wakatime")
           (wakatime-python-bin "/usr/local/anaconda3/bin/python3"))
  :config
  (global-wakatime-mode +1)
  )

(provide 'init-wakatime)

;;; init-wakatime.el ends here