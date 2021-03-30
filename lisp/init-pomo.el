;;; lisp/init-pomo.el -*- lexical-binding: t; -*-

(defun my-stop-vitamin-r (choice)
  "my org-pomodoro finish"
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "How is you feeling: "
			    '("I felt distracted" "My focus was good" "I felt highly focused" "I experienced \"flow\"") nil t))))
  ;; (list (completing-read "How is you feeling: "
  ;;                        '(("1" . "Cow") ("2" . "Rabbit") ("3" . "Dog")) nil t)))
  ;; (message "You chose `%s'" choice)
  choice)

;; (setq my-choice (call-interactively 'my-stop-vitamin-r))

;; (message my-choice)

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
  (setq my-org-pomodoro-start-scpt (expand-file-name "bin/Vitamin-R/Start.applescript" poly-local-dir))
  (setq my-org-pomodoro-abort-scpt (expand-file-name "bin/Vitamin-R/Abort.applescript" poly-local-dir))
  (setq my-org-pomodoro-stop-scpt (expand-file-name "bin/Vitamin-R/Stop.applescript" poly-local-dir))
  (add-hook 'org-pomodoro-started-hook
	    (lambda()
	      (let ((my-todo-title org-clock-heading)
		    )
		;; (my-todo-tags (org-get-tags-at)))
		(princ my-todo-title)
		(process-lines "osascript" my-org-pomodoro-start-scpt my-todo-title)
		))
	    )

  (add-hook 'org-pomodoro-killed-hook
	    (lambda()
	      (let ((my-todo-title org-clock-heading)
		    )
		(process-lines "osascript" my-org-pomodoro-abort-scpt my-todo-title)
		))
	    )
  (add-hook 'org-pomodoro-finished-hook
	    (lambda ()
	      (let ((my-todo-title org-clock-heading)
		    (my-rate-time-slice-choice (call-interactively 'my-stop-vitamin-r)))
		(process-lines "osascript" my-org-pomodoro-stop-scpt my-todo-title my-rate-time-slice-choice)
		)))
  :bind
  (("C-c C-x C-p" . org-pomodoro)
   :map org-mode-map
   ("C-c C-x C-p" . org-pomodoro)))

(provide 'init-pomo)
;;; init-pomo.el ends here
