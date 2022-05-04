;;; lisp/init-csv.el -*- lexical-binding: t; -*-

(use-package csv-mode
  :straight t
  :demand t
  :ensure t
  :config
  (setq csv-align-max-width 100))

(provide 'init-csv)
;;; init-csv.el ends here
