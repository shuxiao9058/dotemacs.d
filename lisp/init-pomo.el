;;; lisp/init-pomo.el -*- lexical-binding: t; -*-

(setq my-org-pomodoro-start-scpt (expand-file-name "bin/Vitamin-R/Start.applescript" poly-local-dir))
(setq my-org-pomodoro-abort-scpt (expand-file-name "bin/Vitamin-R/Abort.applescript" poly-local-dir))
(setq my-org-pomodoro-stop-scpt (expand-file-name "bin/Vitamin-R/Stop.applescript" poly-local-dir))

(defun my-get-vitamin-r-rate-option (choice)
  "my org-pomodoro finish"
  (interactive
   (let ((completion-ignore-case  t)
	 ;; (orderless-matching-styles '()
	 ;; )

	 ;; (orderless-matching-styles nil)
	 (orderless-matching-styles '(orderless-strict-initialism))
	 (completion-styles '(orderless))
	 )
     (list (completing-read "How is you feeling: "
			    '("I felt distracted" "My focus was good" "I felt highly focused" "I experienced \"flow\"") nil t))))
  choice)

(defun my-start-vitamin-r()
  (let ((my-todo-title org-clock-heading))
    ;; (my-todo-tags (org-get-tags-at)))
    (princ my-todo-title)
    (process-lines "osascript" my-org-pomodoro-start-scpt my-todo-title))
  )

(defun my-stop-vitamin-r()
  (interactive)
  (let ((my-todo-title org-clock-heading)
	(my-rate-time-slice-choice (call-interactively 'my-get-vitamin-r-rate-option))
	(my-pomodoro-count (number-to-string (mod org-pomodoro-count org-pomodoro-long-break-frequency)))
	)
    (process-lines "osascript" my-org-pomodoro-stop-scpt
		   my-todo-title my-rate-time-slice-choice my-pomodoro-count))
  )

(defun my-abort-vitamin-r()
  (let ((my-todo-title org-clock-heading))
    (process-lines "osascript" my-org-pomodoro-abort-scpt my-todo-title))
  )

(use-package org-pomodoro
  :straight t
  :ensure t
  :after org
  :commands org-pomodoro
  :custom
  (org-pomodoro-format "Pomo %s")
  (org-pomodoro-short-break-format "Break %s")
  (org-pomodoro-long-break-format "Long break %s")
  (org-pomodoro-start-sound-p t)
  (org-pomodoro-ticking-sound-p t)
  (org-pomodoro-ticking-sound-states '(:pomodoro))
  (org-pomodoro-ticking-frequency 1)
  :config
  (add-hook 'org-pomodoro-started-hook 'my-start-vitamin-r)
  (add-hook 'org-pomodoro-finished-hook 'my-stop-vitamin-r)
  (add-hook 'org-pomodoro-killed-hook 'my-abort-vitamin-r)
  :bind
  (("C-c C-x C-p" . org-pomodoro)
   :map org-mode-map
   ("C-c C-x C-p" . org-pomodoro)))

(provide 'init-pomo)
;;; init-pomo.el ends here
