;;; lisp/init-dart.el -*- lexical-binding: t; -*-

(use-package dart-mode
  :straight t
  )

(use-package flutter
  :straight t
  :after dart-mode
  :bind (:map evil-normal-state-map
              ("SPC r" . #'flutter-run-or-hot-reload)))

(provide 'init-dart)
;;; init-dart.el ends here
