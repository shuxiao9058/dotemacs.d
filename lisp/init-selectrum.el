;;; lisp/init-selectrum.el -*- lexical-binding: t; -*-

(defun my/selectrum-recentf-open-files ()
  "Open `recent-list' item in a new buffer.
  The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Open recentf entry: " files nil t))))

;;; kill ring

(defun konix/kill-ring-insert ()
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (toinsert (completing-read "Yank : "
                                    (delete-dups kill-ring))))
    (when (and toinsert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert toinsert)))

(global-set-key (kbd "C-c C-y") #'konix/kill-ring-insert)

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
  :bind (:map selectrum-minibuffer-map
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
