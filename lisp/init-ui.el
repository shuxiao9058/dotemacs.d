;;; lisp/init-ui.el -*- lexical-binding: t; -*-

;; ;; tooltips in echo-aera
;; (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
;;   (tooltip-mode -1))

;; ;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(use-package doom-themes
    :straight t
    :ensure t
    :custom
    ;; Global settings (defaults)
    (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
    (doom-themes-enable-italic t) ; if nil, italics is universally disabled
    :config
    (progn
      ;; (load-theme 'doom-one t)
      (load-theme 'doom-dracula t)
      ;; (load-theme 'doom-vibrant t)

      ;; Enable flashing mode-line on errors
      (doom-themes-visual-bell-config)

      ;; Enable custom neotree theme (all-the-icons must be installed!)
      (doom-themes-neotree-config)
      ;; or for treemacs users
      (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
      (doom-themes-treemacs-config)
      ;; Corrects (and improves) org-mode's native fontification.
      (doom-themes-org-config))
    )

(use-package doom-modeline
    :straight t
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon nil)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    (doom-modeline-major-mode-color-icon nil)
    (doom-modeline-persp-name nil)
    (doom-modeline-lsp nil)
    ;; Whether display github notifications or not. Requires `ghub+` package.
    (doom-modeline-github nil)
    ;; The interval of checking github.
    (doom-modeline-github-interval (* 30 60))
    (doom-modeline-height 25)
    (doom-modeline-bar-width 3)
    (doom-modeline-checker-simple-format t)
    :hook (after-init . doom-modeline-init)
    :config
    (progn
      ;; Define your custom doom-modeline
      (doom-modeline-def-modeline 'my-simple-line
	  '(bar input-method matches buffer-info remote-host buffer-position parrot selection-info)
	'(objed-state misc-info persp-name lsp minor-modes indent-info buffer-encoding major-mode process vcs checker))

      ;; Add to `doom-modeline-mode-hook` or other hooks
      (add-hook 'doom-modeline-mode-hook
		(lambda() (doom-modeline-set-modeline 'my-simple-line 'default)))
      )
    ;; (doom-modeline-mode +1)
    )

(use-package awesome-tab
    :straight (awesome-tab
	       :type git
	       :host github
	       :repo "manateelazycat/awesome-tab"
	       )
    :commands (awesome-tab-mode)
    :ensure t
    :custom
    ;; (awesome-tab-style 'alternate)
    (awesome-tab-show-tab-index t)
    (awesometab-hide-tabs-hooks
     '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook))
    (awesome-tab-hide-tab-function 'my-awesome-tab-hide-tab)
    :init
    (defun my-awesome-tab-hide-tab (x)
      (let ((name (format "%s" x)))
	(or
	 (string-prefix-p "*NeoTree*" name)
	 (string-prefix-p "*Ilist*" name)
	 (string-prefix-p "*epc" name)
	 (string-prefix-p "*helm" name)
	 (string-prefix-p "*Compile-Log*" name)
	 (string-prefix-p "*lsp" name)
	 (and (string-prefix-p "magit" name)
              (not (file-name-extensionname)))
	 )))

    ;; winum users can use `winum-select-window-by-number' directly.
    (defun my-select-window-by-number (win-id)
      "Use `ace-window' to select the window by using window index.
WIN-ID : Window index."
      (let ((wnd (nth (- win-id 1) (aw-window-list))))
	(if wnd
            (aw-switch-to-window wnd)
	  (message "No such window."))))

    (defun my-select-window ()
      (interactive)
      (let* ((event last-input-event)
             (key (make-vector 1 event))
             (key-desc (key-description key)))
	(my-select-window-by-number
	 (string-to-number (car (nreverse (split-string key-desc "-")))))))

    (set-face-attribute
     'header-line nil
     :box nil)
    (awesome-tab-mode t)
    )

(use-package rainbow-delimiters
    :straight t
    :ensure t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
    :straight t
    :hook (prog-mode . highlight-indent-guides-mode)
    :if IS-GUI
    :config
    ;; (setq highlight-indent-guides-method 'column)
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive 'top)
    (setq highlight-indent-guides-auto-enabled t)
    )

;; copied from +spacemacs/spacemacs-editing-visual
(use-package highlight-parentheses
    :straight t
    :hook (prog-mode . highlight-parentheses-mode)
    :init
    (setq hl-paren-delay 0.2)
    (setq hl-paren-colors
	  '("SpringGreen3" "IndianRed1" "IndianRed3" "IndianRed4"))
    :config
    (set-face-attribute 'hl-paren-face nil :weight 'bold)
    (custom-set-faces '(show-paren-match ((t (:foreground "SpringGreen1" :underline t)))))
    )

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Dashboard settings ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package dashboard
;;     :straight t
;;     :ensure t
;;     :functions (all-the-icons-faicon
;; 		all-the-icons-material
;; 		open-custom-file
;; 		persp-get-buffer-or-nil
;; 		persp-load-state-from-file
;; 		persp-switch-to-buffer
;; 		winner-undo
;; 		widget-forward)
;;     :init
;;     (setq dahboard-banner-logo-title "")
;;     ;;(setq dashboard-startup-banner "~/Imágenes/logo.png")
;;     (setq dashboard-center-content t)
;;     (setq dashboard-items '((recents . 5)
;; 			    (projects . 5)
;; 			    (agenda . 5)))
;;     :hook
;;     (dashboard-mode . (lambda () (linum-mode -1)))
;;     :config
;;     (setq dashboard-set-init-info t
;; 	  dashboard-set-file-icons t
;; 	  dashboard-set-heading-icons t
;; 	  dashboard-heading-icons '((recents . "file-text")
;; 				    (bookmarks . "bookmark")
;; 				    (agenda . "calendar")
;; 				    (projects . "file-directory")
;; 				    (registers . "database"))
;; 	  dashboard-set-navigator t
;; 	  dashboard-navigator-buttons
;; 	  `(((,(when (display-graphic-p)
;; 		 (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
;;                "Settings" "Opens settings file"
;;                (lambda (&rest _) (config-file)))
;;              (,(when (display-graphic-p)
;; 		 (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
;;                "Update" "Update Emacs Configuration to the latest version"
;;                (lambda (&rest _) (update-config)))
;; 	     (,(when (display-graphic-p)
;; 		 (all-the-icons-material "info" :height 1.35 :v-adjust -0.24))
;;                "Personal File" "Opens the personal config file"
;;                (lambda (&rest _) (personal-file))))))
;;     (dashboard-setup-startup-hook))

(use-package all-the-icons
    :straight t
    :if (or IS-GUI (daemonp))
    ;; :init
    ;; (setq all-the-icons-scale-factor 0.8)
    )

(use-package all-the-icons-dired
    :straight t
    :after (all-the-icons dired)
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
    :straight t
    :after (all-the-icons ibuffer)
    :config
    (all-the-icons-ibuffer-mode t))

;; (use-package all-the-icons-ivy
;;     :straight t
;;     :after (all-the-icons ivy)
;;     :config
;;     (add-to-list 'all-the-icons-ivy-file-commands #'counsel-buffer-or-recentf)
;;     (add-to-list 'all-the-icons-ivy-file-commands #'counsel-ibuffer)
;;     (add-to-list 'all-the-icons-ivy-file-commands #'counsel-fzf)
;;     (all-the-icons-ivy-setup)

;;     (with-eval-after-load 'counsel-projectile
;;       (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile)
;;       (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-switch-project)
;;       (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-find-file)
;;       (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-find-dir)
;;       (all-the-icons-ivy-setup)))

(use-package all-the-icons-ivy-rich
    :straight t
    :ensure t
    :after (all-the-icons ivy)
    :init (all-the-icons-ivy-rich-mode 1)
    :custom
    (all-the-icons-ivy-rich-icon-size 0.9)
    )

(use-package ivy-rich
    :straight t
    :ensure t
    :after (counsel ivy)
    :init (ivy-rich-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
