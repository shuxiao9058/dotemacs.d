;;; lisp/init-ui.el -*- lexical-binding: t; -*-

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

;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; auto maximized frame
(when (and IS-MAC (display-graphic-p))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

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

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-dracula t)
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
    :straight t
    :init
     (doom-modeline-mode +1)
    :config
 (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-persp-name nil
        doom-modeline-lsp nil
        doom-modeline-github nil ;; Whether display github notifications or not. Requires `ghub+` package.
        ;; The interval of checking github.
        doom-modeline-github-interval (* 30 60)
        doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-checker-simple-format t
        )

  ;; Define your custom doom-modeline
  (doom-modeline-def-modeline 'my-simple-line
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

  ;; Add to `doom-modeline-mode-hook` or other hooks
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
    )

(use-package awesome-tab
  :straight (:host github
    :repo "manateelazycat/awesome-tab"
    )
  ;; :after-call after-find-file dired-initial-position-hook
  :commands (awesome-tab-build-ivy-source awesome-tab-select-visible-tab awesome-tab-mode)
  :init
  (setq awesometab-hide-tabs-hooks
        '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook))
  (setq awesome-tab-style 'alternate)
  ;; Make hydra compatible with awesome-tab
  (with-eval-after-load 'hydra
    (defun zenith/lv-window (fun)
      (with-selected-window (funcall fun)
        (setq-local header-line-format nil))
      lv-wnd)

    (advice-add 'lv-window :around 'zenith/lv-window))
  (dotimes (i 10)
    (general-define-key (concat "M-" (int-to-string i)) #'awesome-tab-select-visible-tab))
  :config
  (awesome-tab-mode +1)
  )

(provide 'init-ui)

;;; init-ui.el ends here