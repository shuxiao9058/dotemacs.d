;;; lisp/init-ui.el -*- lexical-binding: t; -*-

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

;; (use-package doom-themes
;;   :straight t
;;   :ensure t
;;   ;; :after treemacs
;;   :custom
;;   ;; Global settings (defaults)
;;   (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
;;   (doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   :config
;;   ;; (load-theme 'doom-one t)
;;   (load-theme 'doom-dracula t)
;;   ;; (load-theme 'doom-vibrant t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   )


(use-package ace-window
  :straight t
  :ensure t
  :defer t
  :after general
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  ("C-x o"  #'ace-window))

(use-package doom-modeline
  :straight t
  :custom
  ;; (doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
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
  (doom-modeline-mode +1)
  )

(use-package treemacs
  :straight t
  ;; :after hl-line-mode
  :custom
  (treemacs-collapse-dirs                 (if (executable-find "python") 3 0))
  (treemacs-deferred-git-apply-delay      0.5)
  (treemacs-display-in-side-window        t)
  (treemacs-eldoc-display                 nil)
  (treemacs-file-event-delay              5000)
  (treemacs-file-follow-delay             0.2)
  (treemacs-follow-after-init             t)
  (treemacs-git-command-pipe              "")
  (treemacs-git-integration                t)
  (treemacs-goto-tag-strategy             'refetch-index)
  (treemacs-indentation                   2)
  (treemacs-indentation-string            " ")
  (treemacs-is-never-other-window         t)
  (treemacs-max-git-entries               5000)
  (treemacs-no-png-images                 t)
  (treemacs-no-delete-other-windows       t)
  (treemacs-project-follow-cleanup        t)
  (treemacs-persist-file                  (expand-file-name "/treemacs-persist" poly-cache-dir))
  (treemacs-recenter-distance             0.1)
  (treemacs-recenter-after-file-follow    nil)
  (treemacs-recenter-after-tag-follow     nil)
  (treemacs-recenter-after-project-jump   'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs--icon-size 12)
  (treemacs-silent-refresh t)
  (treemacs-follow-mode t)
  (treemacs-show-cursor                   t)
  (treemacs-show-hidden-files             t)
  (treemacs-silent-filewatch              t)
  (treemacs-sorting                       'alphabetic-desc)
  (treemacs-space-between-root-nodes      t)
  (treemacs-tag-follow-cleanup            t)
  (treemacs-tag-follow-delay              1.5)
  (treemacs-width                         28)
  (doom-treemacs-use-generic-icons nil)
  :defines winum-keymap
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :init
  (with-eval-after-load 'winum
    (bind-key (kbd "M-9") #'treemacs-select-window winum-keymap))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
	       (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
        (vector #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111)))
  :bind (([f8]        . treemacs)
         ("C-`"       . treemacs-select-window)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
	 :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  )

(use-package treemacs-projectile
  :straight t
  :ensure t
  :after (treemacs projectile)
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :ensure t)

;; (use-package awesome-tab
;;   :straight (awesome-tab
;; 	     :type git
;; 	     :host github
;; 	     :repo "manateelazycat/awesome-tab")
;;   ;; :commands (awesome-tab-mode)
;;   :ensure t
;;   :custom
;;   (awesome-tab-style 'bar)
;;   (awesome-tab-show-tab-index t)
;;   (awesome-tab-display-icon nil)
;;   (awesometab-hide-tabs-hooks
;;    '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook))
;;   ;; :init
;;   ;; (awesome-tab-mode t)
;;   :config
;;   (defun awesome-tab-hide-tab-function (x)
;;     "Awesome tab hide tab function (X)."
;;     (let ((name (format "%s" x)))
;;       (and
;;        (not (string-prefix-p "*epc" name))
;;        (not (string-prefix-p "*helm" name))
;;        (not (string-prefix-p "*Compile-Log*" name))
;;        (not (string-prefix-p "*lsp" name))
;;        (not (and (string-prefix-p "magit" name)
;; 		 (not (file-name-extension name))))
;;        )))
;;   ;;set shortcut for buffer switch
;;   (dotimes (tabnum 10)
;;     (global-set-key
;;      (kbd (concat "M-" (number-to-string tabnum)))
;;      'awesome-tab-select-visible-tab))
;;   :general
;;   ("M-j" #'awesome-tab-ace-jump)
;;   ("M-h" #'awesome-tab-backward)
;;   ("M-l" #'awesome-tab-forward)
;;   ("<M-right>" #'awesome-tab-forward)
;;   ("<M-left>" #'awesome-tab-backward)
;;   ("<M-down>" #'awesome-tab-forward-group)
;;   ("<M-up>" #'awesome-tab-backward-group)
;;   ("M-z" #'awesome-tab-switch-group)
;;   )

(use-package rainbow-delimiters
  :straight t
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :delight highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-auto-enabled nil
        ;; default is \x2502 but it is very slow on Mac
        highlight-indent-guides-character ?\xFFE8
        highlight-indent-guides-responsive 'top)
  ;; :config
  ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(use-package display-fill-column-indicator
  :if EMACS27+
  :straight nil
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)))

;; copied from +spacemacs/spacemacs-editing-visual
(use-package highlight-parentheses
  :straight t
  :hook (prog-mode . highlight-parentheses-mode)
  :init
  (setq hl-paren-delay 0.2)
  (setq hl-paren-colors
	'("SpringGreen3" "IndianRed1" "IndianRed3" "IndianRed4"))
  ;; :config
  ;; (set-face-attribute 'hl-paren-face nil :weight 'bold)
  ;; (custom-set-faces '(show-paren-match ((t (:foreground "SpringGreen1" :underline t)))))
  )

;; (use-package dashboard
;;   :straight t
;;   :ensure t
;;   :functions (
;; 	      ;; all-the-icons-faicon
;; 	      ;; all-the-icons-material
;; 	      open-custom-file
;; 	      persp-get-buffer-or-nil
;; 	      persp-load-state-from-file
;; 	      persp-switch-to-buffer
;; 	      winner-undo
;; 	      widget-forward)
;;   :demand
;;   :diminish (dashboard-mode page-break-lines-mode)
;;   :custom
;;   (dashboard-center-content t)
;;   (dashboard-items '((recents . 5)
;; 		     (projects . 5)
;; 		     (agenda . 5)))
;;   (dashboard-set-init-info t)
;;   (dashboard-set-file-icons nil)
;;   (dashboard-set-heading-icons nil)
;;   ;; (dashboard-heading-icons '((recents . "file-text")
;;   ;; 			     (bookmarks . "bookmark")
;;   ;; 			     (agenda . "calendar")
;;   ;; 			     (projects . "file-directory")
;;   ;; 			     (registers . "database")))
;;   (dashboard-set-navigator t)
;;   (initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
;;   ;; (dashboard-navigator-buttons
;;   ;;  `(((,(when (display-graphic-p)
;;   ;; 	  (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
;;   ;;      "Settings" "Opens settings file"
;;   ;;      (lambda (&rest _) (config-file)))
;;   ;;     (,(when (display-graphic-p)
;;   ;; 	  (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
;;   ;;      "Update" "Update Emacs Configuration to the latest version"
;;   ;;      (lambda (&rest _) (update-config)))
;;   ;;     (,(when (display-graphic-p)
;;   ;; 	  (all-the-icons-material "info" :height 1.35 :v-adjust -0.24))
;;   ;;      "Personal File" "Opens the personal config file"
;;   ;;      (lambda (&rest _) (personal-file))))))
;;   ;; (dashboard-banner-logo-title "Close the world. Open the nExt.")
;;   ;; (dashboard-startup-banner (expand-file-name "images/KEC_Dark_BK_Small.png" user-emacs-directory))
;;   ;; (dashboard-startup-banner "~/Imágenes/logo.png")
;;   ;; (setq dahboard-banner-logo-title "")
;;   :hook
;;   (dashboard-mode . (lambda () (linum-mode -1)))
;;   :config
;;   (dashboard-setup-startup-hook)
;;   ;; Open Dashboard function
;;   (defun open-dashboard ()
;;     "Open the *dashboard* buffer and jump to the first widget."
;;     (interactive)
;;     (if (get-buffer dashboard-buffer-name)
;;         (kill-buffer dashboard-buffer-name))
;;     (dashboard-insert-startupify-lists)
;;     (switch-to-buffer dashboard-buffer-name)
;;     (goto-char (point-min))
;;     (delete-other-windows))
;;   :general
;;   (:keymaps 'dashboard-mode-map
;; 	    "n"  #'dashboard-next-line
;; 	    "p"  #'dashboard-previous-line
;; 	    "N"  #'dashboard-next-section
;; 	    "F"  #'dashboard-previous-section)
;;   )

;; (use-package all-the-icons
;;   :straight t
;;   :if (or IS-GUI (daemonp))
;;   ;; :init
;;   ;; (setq all-the-icons-scale-factor 0.8)
;;   )

;; (use-package all-the-icons-dired
;;   :straight t
;;   :if (or IS-GUI (daemonp))
;;   :after (all-the-icons dired)
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-ibuffer
;;   :straight t
;;   :if (or IS-GUI (daemonp))
;;   :after (all-the-icons ibuffer)
;;   :config
;;   (all-the-icons-ibuffer-mode t))

;; (use-package all-the-icons-ivy
;;   :straight t
;;   :after (all-the-icons ivy)
;;   :if (or IS-GUI (daemonp))
;;   :config
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-buffer-or-recentf)
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-ibuffer)
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-fzf)
;;   (all-the-icons-ivy-setup)

;; (with-eval-after-load 'counsel-projectile
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile)
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-switch-project)
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-find-file)
;;   (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-find-dir)
;;   (all-the-icons-ivy-setup)))

;; (use-package all-the-icons-ivy-rich
;;   :straight t
;;   :ensure t
;;   :after (all-the-icons ivy)
;;   :if (or IS-GUI (daemonp))
;;   :init (all-the-icons-ivy-rich-mode 1)
;;   :custom
;;   (all-the-icons-ivy-rich-icon-size 0.9)
;;   )

(use-package ivy-rich
  :straight t
  :ensure t
  :after (counsel ivy)
  :custom
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :if (or IS-GUI (daemonp))
  :init (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

;; growl notify
(use-package alert
  :straight (alert
	     :host github
	     :repo "jwiegley/alert")
  :commands (alert)
  :custom
  (alert-default-style 'osx-notifier)
  )

;; ;; github style emoji input
;; (use-package company-emoji
;;   :config
;;   (add-to-list 'company-backends 'company-emoji))

(provide 'init-ui)
;;; init-ui.el ends here
