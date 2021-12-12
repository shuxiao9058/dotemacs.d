;;; lisp/init-window.el -*- lexical-binding: t; -*-

;; (use-package switch-window
;;     :straight t
;;     ;; :when (featurep! +switch-window)
;;     :defer t
;;     :init
;;     (global-set-key [remap other-window] #'switch-window)
;;     :config
;;     (setq switch-window-shortcut-style 'qwerty
;;           switch-window-qwerty-shortcuts '("a" "s" "d" "f" "g" "h" "j" "k" "l")))

(use-package ace-window
  :straight t
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'global
        aw-background t))

(use-package windswap
  :straight t
  ;; https://github.com/amnn/windswap
  ;; windswap-left|right|up|down
  :commands (windswap-up windswap-down windswap-left windswap-right))

(provide 'init-window)
;;; init-window.el ends here
