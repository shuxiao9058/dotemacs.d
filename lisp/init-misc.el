;;; lisp/init-misc.el -*- lexical-binding: t; -*-

;; (defun my-nov-font-setup ()
;;   (face-remap-add-relative 'variable-pitch :family "Liberation Serif"))


;; This page lists all the customizations: https://depp.brause.cc/nov.el/
;; (use-package nov
;;     ;; :straight t
;;     :after org
;;     :straight (:no-native-compile t :type git :host nil :repo "https://depp.brause.cc/nov.el.git")
;;     :mode (("\\.epub\\'" . nov-mode))
;;     :config
;;     (progn
;;       ;; Change the font face
;;       (add-hook 'nov-mode-hook
;; 		(lambda ()
;;                   (face-remap-add-relative
;;                    'variable-pitch
;;                    :family "opendyslexic"
;;                    :height 2.3)))

;;       (setq nov-text-width 120)

;;       ;; (evil-set-initial-state 'nov-mode 'normal)

;;       ;; (general-define-key
;;       ;;  :keymaps 'nov-mode-map
;;       ;;  :states 'normal
;;       ;;  "g" 'nov-render-document
;;       ;;  "n" 'nov-next-document
;;       ;;  "(point)" 'nov-previous-document
;;       ;;  "t" 'nov-goto-toc
;;       ;;  "l" 'nov-history-back
;;       ;;  "r" 'nov-history-forward
;;       ;;  "RET" 'nov-browse-url
;;       ;;  "c" 'nov-copy-url
;;       ;;  ;;"<follow-link>" 'mouse-face
;;       ;;  ;;"<mouse-2>" 'nov-browse-url
;;       ;;  "TAB" 'shr-next-link
;;       ;;  "M-TAB" 'shr-previous-link
;;       ;;  "<backtab>" 'shr-previous-link
;;       ;;  ;; Toggle normal emacs font rendering
;;       ;;  "f" (lambda ()
;;       ;;        (interactive)
;;       ;;        (setq nov-variable-pitch (not nov-variable-pitch))
;;       ;;        (nov-render-document)))
;;       )
;;     )

;; (use-package bookmark+
;;     :straight t)

;; ;;; nov.el for epub ebooks reading
;; (use-package nov
;;   :straight t
;;   ;; :straight (nov :type git
;;   ;;  :files ("*.el")
;;   ;;                :host nil :repo "https://depp.brause.cc/nov.el.git")
;;   :after org
;;   :ensure t
;;   ;; :commands nov-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;;   ;; :hook
;;   ;; (nov-mode . 'my-nov-font)
;;   )

;; (defun log-complete-time-elapsed()
;;   (interactive)
;;   (let ((time (current-time)))
;;     (company-complete-common)
;;     (message "%.03fms"
;;              (* 1000 (float-time (time-since time)))))
;;   )

(use-package conf-mode
  :straight nil
  :mode
  (("\\.ini\\’" . conf-mode)))


;; Consult without consultation fees
(use-package consult
  :straight t
  :ensure t
  :demand t
  :after selectrum
  :defer 1
  ;; :custom
  ;; (consult-preview-key nil)
  ;; (consult-project-root-function #'projectile-project-root)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init (bind-key "TAB"
                  (lambda ()
                    (interactive)
                    (isearch-exit)
                    (consult-line isearch-string))
                  isearch-mode-map)
  :config
  (require 'consult)
  (require 'consult-imenu)
  (with-eval-after-load 'org
    (require 'consult-org))
  (declare-function consult--customize-set "consult")
  (progn
    (setq consult-project-root-function #'vc-root-dir)
    (consult-customize
     consult-ripgrep consult-grep
     consult-buffer consult-recent-file
     :preview-key (kbd "M-."))

    ;; Disable consult-buffer project-related capabilities as
    ;; they are very slow in TRAMP.
    (setq consult-buffer-sources
          (delq 'consult--source-project-buffer
                (delq 'consult--source-project-file consult-buffer-sources)))

    (setq consult--source-hidden-buffer
          (plist-put consult--source-hidden-buffer :narrow ?h)))
  :bind (
	 ("M-s f" . consult-line)
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g r" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
	 ([remap yank-pop] . consult-yank-from-kill-ring)
	 ([remap switch-to-buffer] . consult-buffer)
	 ([remap goto-line] . consult-goto-line)))

(use-package deadgrep
  :ensure t
  :commands (deadgrep--read-search-term)
  :bind (("C-c s" . deadgrep)))

(use-package consult-lsp
  :straight t
  :ensure t
  :after (consult lsp))

(use-package embark
  :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (("C-c o" . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package savehist
  :straight t
  :config
  (savehist-mode 1))

(use-package docker-compose-mode
  :straight t)

;; (booleanp 2)

;; (count-loop (i 0 10)
;; 	    (message "i: %d" i)
;; 	    )

;; (defun where())
(provide 'init-misc)
;;; init-misc.el ends here
