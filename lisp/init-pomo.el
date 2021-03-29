;;; lisp/init-pomo.el -*- lexical-binding: t; -*-

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
  (add-hook 'org-pomodoro-started-hook
	    (lambda()
	      (do-applescript "tell application \"JustFocus\" \nlaunch\nstart pomodoro\nend tell")))
  (add-hook 'org-pomodoro-finished-hook
	    (lambda ()
	      (do-applescript "tell application \"JustFocus\" \nstop\nend tell")
	      (when (org-clock-is-active)
		(if (org-pomodoro-active-p)
		    (org-pomodoro)
      		  ;; todo: subtract idle time (right not the loss is small, like 3 min)
		  ;; something like org-clockout nil t (- (org-current-time) idle-time)
		  (org-clock-out))
		)))
  :bind
  (("C-c C-x C-p" . org-pomodoro)
   :map org-mode-map
   ("C-c C-x C-p" . org-pomodoro)))

(provide 'init-pomo)
;;; init-pomo.el ends here
