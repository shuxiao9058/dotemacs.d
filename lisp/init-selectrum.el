;;; lisp/init-selectrum.el -*- lexical-binding: t; -*-

(defun my/selectrum-recentf-open-files ()
  "Open `recent-list' item in a new buffer.
  The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

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
    :bind (;; ("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))
    :config
    (marginalia-mode))

(use-package selectrum-prescient
    :straight t
    :after selectrum
    :config
    (selectrum-prescient-mode t)
    (prescient-persist-mode))

(use-package deadgrep
    :ensure t
    :commands (deadgrep--read-search-term)
    :bind (("C-c s" . deadgrep)))

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
    ( require 'consult)
    (require 'consult-imenu)
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

(use-package consult-org
    :straight t
    :after (consult org))

(use-package consult-lsp
    :straight t
    :ensure t
    :after (consult lsp))

(use-package embark
    :straight t
    :bind (("C-c o" . embark-act)
           :map minibuffer-local-map
           ("M-o"   . embark-act)))

(use-package embark-consult
    :straight t
    :after (embark consult))

(provide 'init-selectrum)
;;; init-selectrum.el ends here
