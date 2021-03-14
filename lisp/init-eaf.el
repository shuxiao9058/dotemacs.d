;;; lisp/init-eaf.el -*- lexical-binding: t; -*-

(use-package epc
  :straight t
  :defer t
  :ensure t)
(use-package ctable
  :straight t
  :defer t
  :ensure t)

(use-package deferred
  :straight t
  :defer t
  :ensure t)

(use-package s
  :straight t
  :defer t
  :ensure t)

(use-package eaf
  :straight (eaf :type git :fetcher github :repo "manateelazycat/emacs-application-framework" :files (:defaults "app" "core" "*.py"))
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

(provide 'init-eaf)
;;; init-eaf.el ends here
