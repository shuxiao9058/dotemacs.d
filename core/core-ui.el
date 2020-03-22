;;; core/core-ui.el -*- lexical-binding: t; -*-

;; Clear Window clutter and set up the look and feel
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
(when (and (fboundp 'horizontal-scroll-bar-mode) (not (eq horizontal-scroll-bar-mode -1)))
  (scroll-bar-mode -1))

(when (and (fboundp 'use-file-dialog) (not (eq use-file-dialog -1)))
  (use-file-dialog -1))
(when (and (fboundp 'use-dialog-box) (not (eq use-dialog-box -1)))
  (use-dialog-box -1))
(when (and (fboundp 'blink-cursor-mode) (not (eq blink-cursor-mode -1)))
  (blink-cursor-mode -1))

;;; very long line performence optimizy
(global-so-long-mode t)

;;; highlight current line
(global-hl-line-mode)

;; auto maximized frame
(when (and IS-MAC (display-graphic-p))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(when IS-MAC
  (setq-default ns-use-thin-smoothing t
                ns-use-fullscreen-animation t
                ns-use-native-fullscreen t
                frame-resize-pixelwise t
                ns-use-proxy-icon t
                ns-use-mwheel-momentum t
                ns-use-mwheel-acceleration t
                mac-command-modifier 'super
                mac-option-modifier  'meta
                )
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (add-hook 'window-setup-hook #'toggle-frame-maximized)
  )

(provide 'core-ui)
;;; core-ui.el ends here