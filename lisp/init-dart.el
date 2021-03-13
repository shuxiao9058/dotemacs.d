;;; lisp/init-dart.el -*- lexical-binding: t; -*-

(use-package dart-mode
  :straight t
  )

(use-package flutter
  :straight t
  :after dart-mode
  )

(provide 'init-dart)
;;; init-dart.el ends here
