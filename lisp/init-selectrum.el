;;; lisp/init-selectrum.el -*- lexical-binding: t; -*-

(defun my/selectrum-recentf-open-files ()
  "Open `recent-list' item in a new buffer.
  The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(defun my/selectrum-yank-kill-ring ()
  "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.
Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
  (interactive)
  (let* ((selectrum-should-sort-p nil)
	 (kills (delete-dups kill-ring))
	 (toinsert (completing-read "Yank from kill ring: " kills nil t)))
    (when (and toinsert (use-region-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert toinsert)))

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
    :bind (([remap yank-pop] . my/selectrum-yank-kill-ring)
	   :map selectrum-minibuffer-map
	   ("DEL" . selectrum-backward-kill-sexp)
	   ("<down>" . selectrum-next-candidate)
	   ("C-j"  .  selectrum-next-candidate)
	   ("<up>" .  selectrum-previous-candidate)
	   ("C-k"  .  selectrum-previous-candidate)
	   ("<backtab>" . selectrum-previous-candidate)
	   ))

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

(provide 'init-selectrum)
;;; init-selectrum.el ends here
