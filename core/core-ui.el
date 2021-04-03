;;; core/core-ui.el -*- lexical-binding: t; -*-

(use-package doom-themes
  :straight t
  :custom-face
  (icomplete-first-match ((t (:inherit mode-line-emphasis))))
  (mode-line-buffer-id ((t (:foreground "Light Blue"))))
  ;; (font-lock-variable-name-face ((t (:foreground "#50fa7b"))))
  ;; (highlight-indentation-face ((t (:inherit default :foreground "#878787"))))
  ;; (hl-line ((t (:background "DodgerBlue4"))))
  (orderless-match-face-0 ((t (:inherit font-lock-type-face :weight bold))))
  (orderless-match-face-1 ((t (:inherit error :weight bold))))
  (orderless-match-face-2 ((t (:inherit font-lock-string-face :weight bold))))
  (orderless-match-face-3 ((t (:inherit font-lock-keyword-face :weight bold))))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(defalias 'yes-or-no-p 'y-or-n-p)

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

(when EMACS27+
  ;; very long line performence optimizy
  (global-so-long-mode t))

;;; highlight current line
(global-hl-line-mode)

;; auto maximized frame
(when (and IS-MAC IS-GUI)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(when IS-MAC
  (setq frame-resize-pixelwise t
	mac-command-modifier 'super
	mac-option-modifier  'meta)
  (when (featurep 'ns)
    (setq ns-use-thin-smoothing t
          ns-use-fullscreen-animation t
          ns-use-native-fullscreen t
          ns-use-proxy-icon t
          ns-use-mwheel-momentum t
          ns-use-mwheel-acceleration t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  ;; (add-hook 'window-setup-hook #'toggle-frame-maximized)
  )

(setq inhibit-splash-screen nil)

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
  (mouse-wheel-mode t)
  ;; Better scrolling with mouse wheel/trackpad.
  (unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
    (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
    (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
    (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
    (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
    (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
    (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))
  )

;; Hide the mouse while typing:
(setq make-pointer-invisible t)

;; Display visited file's path in the frame title
;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      `((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; emoji
(when IS-MAC
  (if (version< "27.0" emacs-version)
      (set-fontset-font
       "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
    (set-fontset-font
     t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))

;; Activate winner mode for quickly changing window sizes, etc
(when (fboundp 'winner-mode)
  (winner-mode 1))

(provide 'core-ui)
;;; core-ui.el ends here
