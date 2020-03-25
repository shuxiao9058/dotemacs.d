;;; core/core-ui.el -*- lexical-binding: t; -*-

;;; font

;; (when (member "Source Code Pro" (font-family-list))
;;   (add-to-list 'initial-frame-alist '(font . "Source Code Pro"))
;;   (add-to-list 'default-frame-alist '(font . "Source Code Pro")))

;; (set-face-attribute 'default nil :family "Source Code Pro" :height 120)

(when IS-GUI
  (let ((monaco-font "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
	(sourcode-font "-apple-Source Code Pro-*-normal-normal-*-13-*-*-*-m-0-iso10646-1")
	(firacode-font "-*-Fira Code-*-*-*-*-13-*-*-*-*-*-*-*")
	(sarasa-font "-*-Sarasa Mono SC-*-*-*-*-14-*-*-*-*-*-*-*")
	)
    ;; (set-face-attribute 'default nil :font sourcode-font)
    ;; (set-face-attribute 'default nil :font monaco-font)
    (set-face-attribute 'default nil :font sarasa-font)
    )
  )

;; 等宽: Source Code Pro 13 + STkaiti 16
;; (setq face-font-rescale-alist `(("STkaiti" . ,(/ 16.0 13))))
;; (set-fontset-font t 'han (font-spec :family "STkaiti"))

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

;; ;; tooltips in echo-aera
;; (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
;;   (tooltip-mode -1))

(when EMACS27+
  ;; very long line performence optimizy
  (global-so-long-mode t))

;;; highlight current line
(global-hl-line-mode)

;; auto maximized frame
(when (and IS-MAC IS-GUI)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

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

;; Symbol’s value as variable is void: mouse-wheel-down-event
(when (require 'mwheel nil 'noerror)
  ;; scroll two lines at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount
        '(1                                 ; 一次只滚动 1 行
          ((shift) . 2)                     ; 按住 Shift 滚动 2 行
          ((control). 3))                   ; 按住 Ctrl 滚动 3 行
        mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
        mouse-wheel-follow-mouse 't ;; scroll window under mouse
        scroll-preserve-screen-position 't ;; 鼠标滚动的时候保持光标在屏幕中的位置不变
        scroll-step 2) ;; keyboard scroll two lines at a time
  (mouse-wheel-mode t))

;; Hide the mouse while typing:
(setq make-pointer-invisible t)

;; Display visited file's path in the frame title
;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      `((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'core-ui)
;;; core-ui.el ends here
