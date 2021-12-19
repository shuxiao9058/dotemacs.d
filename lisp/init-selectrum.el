;;; lisp/init-selectrum.el -*- lexical-binding: t; -*-



;; (defun my/selectrum-recentf-open-files ()
;;   "Open `recent-list' item in a new buffer.
;;   The user's $HOME directory is abbreviated as a tilde."
;;   (interactive)
;;   (let ((files (mapcar 'abbreviate-file-name recentf-list)))
;;     (find-file (completing-read "Find recent file: " files nil t))))

;; (defun my/selectrum-yank-kill-ring ()
;;   "Insert the selected `kill-ring' item directly at point.
;; When region is active, `delete-region'.
;; Sorting of the `kill-ring' is disabled.  Items appear as they
;; normally would when calling `yank' followed by `yank-pop'."
;;   (interactive)
;;   (let* ((selectrum-should-sort-p nil)
;; 	 (kills (delete-dups kill-ring))
;; 	 (toinsert (completing-read "Yank from kill ring: " kills nil t)))
;;     (when (and toinsert (use-region-p))
;;       ;; the currently highlighted section is to be replaced by the yank
;;       (delete-region (region-beginning) (region-end)))
;;     (insert toinsert)))

;;;; Orderless
;; ordering of narrowed candidates
(use-package orderless
  :straight t
  :after selectrum
  :config
  (setq completion-styles '(orderless))
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)))

(use-package selectrum
  :straight t
  :hook (after-init . selectrum-mode)
  :custom
  (selectrum-fix-vertical-window-height t)
  (selectrum-extend-current-candidate-highlight t)
  (selectrum-count-style 'current/matches)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-refine-candidates-function #'orderless-filter)
  ;; (selectrum-num-candidates-displayed 15)
  (selectrum-max-window-height 15)
  :config
  (selectrum-mode t)
  :bind (;; ([remap yank-pop] . my/selectrum-yank-kill-ring)
	 :map selectrum-minibuffer-map
	 ;; ("DEL" . selectrum-backward-kill-sexp)
	 ("<S-backspace>" . selectrum-backward-kill-sexp)
	 ("<down>" . selectrum-next-candidate)
	 ("C-j"  .  selectrum-next-candidate)
	 ("<up>" .  selectrum-previous-candidate)
	 ("C-k"  .  selectrum-previous-candidate)
	 ("<backtab>" . selectrum-previous-candidate)
	 ))

(use-package marginalia
  :straight t
  :after selectrum
  :ensure t
  :demand t
  :defer 1
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light))
  :init
  (setq marginalia-command-categories
        '((imenu . imenu)
	  (projectile-find-file . project-file)
	  ;; (projectile-find-file . project)
          (projectile-find-dir . project-file)
	  ;; (projectile-find-dir . project)
	  ;; (projectile-switch-project . project)
          (projectile-switch-project . file)
	  (projectile-switch-open-project . file)
	  (projectile-recentf . project-file)
	  (projectile-display-buffer . project-buffer)
	  (projectile-switch-to-buffer . project-buffer)
	  ;; (projectile-commander . project)
	  ))
  :bind (;; ("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode)

  ;; Display more annotations - e.g. docstring with M-x
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
	      (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

;; -----------------------------------------------------------------------------
;; Marginalia doesn't remember the this-command when switching projects using
;; projectile, since it uses multiple minibuffers. In order to classify project
;; completions properly, we keep track of when we're in the process of switching
;; projects and make sure to return the correct category

(defvar c/switching-project? nil)
(defun c/projectile-before-switch-project ()
  (setq c/switching-project? t))
(defun c/projectile-after-switch-project ()
  (setq c/switching-project? nil))

(after-load (projectile marginalia)
  (add-hook 'projectile-before-switch-project-hook #'c/projectile-before-switch-project)
  (add-hook 'projectile-after-switch-project-hook #'c/projectile-after-switch-project)

  (advice-add 'marginalia-classify-by-prompt :around
	      (lambda (orig-fun &rest args)
                (if c/switching-project?
		    'project
                  (apply orig-fun args)))))


(use-package all-the-icons-completion
  :straight t
  :after marginalia
  :commands all-the-icons-completion-marginalia-setup
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (selectrum-prescient-mode t)
  (prescient-persist-mode))

;; (use-package consult-org
;;     :straight t
;;     :after (consult org))

(provide 'init-selectrum)
;;; init-selectrum.el ends here
