;;; lisp/init-tab-bar.el -*- lexical-binding: t; -*-

;; Tab bar represents a named persistent window configuration.
(use-package tab-bar
  :straight nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-close-tab-select 'recent)
  ;; Start a new tab with the current buffer.
  (tab-bar-new-tab-choice t)
  (tab-bar-new-tab-to 'right)
  (tab-bar-position nil)
  ;; Keep tab-bar hidden.
  (tab-bar-show t)
  (tab-bar-tab-hints t)
  ;; (tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-border 0)
  (tab-bar-close-button nil)
  (tab-bar-back-button nil)
  (tab-bar-new-button nil)
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-tab-name-format-function '+tab-bar-tab-format-function)
  :config
  ;; Enable `tab-bar-mode' by default.
  (tab-bar-mode t)
  (global-tab-line-mode -1)
  ;; Check `winner-mode' that keeps track of layout changes.
  (tab-bar-history-mode -1)

  (defun my/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
			(tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
	     (tab-bar-switch-to-tab (completing-read "Select tab: " tabs))))))

  (defun +tab-bar-switch-project ()
    "Switch to project in a new tab, project name will be used as tab name.
No tab will created if the command is cancelled."
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'project-switch-project)
            (when-let ((proj (project-current)))
              (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
              (setq succ t)))
	(unless succ
          (tab-bar-close-tab)))))

  (defun +tab-bar-tab-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (concat
       (propertize (concat
                    " "
                    (alist-get 'name tab)
                    " ")
                   'face
                   (funcall tab-bar-tab-face-function tab))
       " ")))

  (global-set-key (kbd "C-x t .") #'tab-bar-rename-tab)
  (global-set-key (kbd "C-x t l") #'+tab-bar-switch-project)

  (tab-bar-mode 1)
  :bind
  ("H-t" . an/hydra-window-management/body)
  (("C-x t t" . my/tab-bar-select-tab-dwim)
   ("s-t" . my/tab-bar-select-tab-dwim)
   ;; Add alias for C-tab.
   ("<s-tab>" . tab-next)
   ;; Add alias for C-S-tab.
   ("<C-s-tab>" . tab-previous))
  ;; monkey-with-hammer.png
  ("M-1" .  (lambda () (interactive) (tab-bar-select-tab 1)))
  ("M-2" .  (lambda () (interactive) (tab-bar-select-tab 2)))
  ("M-3" .  (lambda () (interactive) (tab-bar-select-tab 3)))
  ("M-4" .  (lambda () (interactive) (tab-bar-select-tab 4)))
  ("M-5" .  (lambda () (interactive) (tab-bar-select-tab 5)))
  ("M-6" .  (lambda () (interactive) (tab-bar-select-tab 6)))
  ("M-7" .  (lambda () (interactive) (tab-bar-select-tab 7)))
  ("M-8" .  (lambda () (interactive) (tab-bar-select-tab 8)))
  ("M-9" .  (lambda () (interactive) (tab-bar-select-tab 9))))

;; use project name as default tab name
(defun toy/set-tab-name-default ()
  (interactive)
  (if (buffer-file-name (current-buffer))
      (let ((proj-name (projectile-project-name)))
	(unless (or (= (length proj-name) 0) (string= proj-name "-"))
	  ;; (message proj-name)
	  (tab-bar-rename-tab proj-name)))))

(advice-add 'tab-bar-new-tab :after (lambda (&rest x) (toy/set-tab-name-default)))
(advice-add 'tab-bar-tab-name-format-function :after (lambda (&rest x) (toy/set-tab-name-default)))
(add-hook 'window-setup-hook #'toy/set-tab-name-default)

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
