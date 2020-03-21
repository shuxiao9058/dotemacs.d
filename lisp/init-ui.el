;;; lisp/init-ui.el -*- lexical-binding: t; -*-

;; ;; tooltips in echo-aera
;; (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
;;   (tooltip-mode -1))

;; ;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


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
    (doom-modeline-mode +1)
    )

(use-package awesome-tab
    :straight (awesome-tab :type git
			   :host github
			   :repo "manateelazycat/awesome-tab"
			   )
    :commands (awesome-tab-mode)
    :ensure t
    :init
    ;;(awesometab-hide-tabs-hooks
    ;;      '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook))
    (setq awesome-tab-style 'alternate)
    :config
    (awesome-tab-mode t)
    ;; (awesome-tab-build-ivy-source)
    )

(use-package rainbow-delimiters
    :straight t
    :ensure t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
    :if (or (display-graphic-p) (daemonp))
    ;; :init
    ;; (setq all-the-icons-scale-factor 0.8)
    )


;; (use-package all-the-icons-dired
;;     :straight t
;;     :after (all-the-icons dired)
;;     :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-ibuffer
;;     :straight t
;;     :after (all-the-icons ibuffer)
;;     :config
;;     (all-the-icons-ibuffer-mode t))

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

(provide 'init-ui)

;;; init-ui.el ends here
