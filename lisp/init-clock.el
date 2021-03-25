;;; lisp/init-clock.el -*- lexical-binding: t; -*-

(use-package org-clock
  :straight nil
  ;; ensure we always run org-clock-persistence-insinuate below
  :demand t
  :after (org alert)
  :custom
  (org-clock-persist 'history)
  (org-clock-persist-file (expand-file-name "org-clock-save.el" poly-cache-dir))
  (org-clock-sound t)
  (org-clock-in-resume t)
  (org-clock-idle-time 10)
  (org-clock-into-drawer t)
  (org-clock-out-when-done t)
  (org-clock-history-length 20)
  (org-clock-mode-line-total 'today)
  (org-clock-display-default-range 'thisweek)
  (org-clock-in-switch-to-state "NEXT")
  (org-clock-out-switch-to-state "WAIT")
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-clock-clocked-in-display 'both)
  :config
  (org-clock-persistence-insinuate)
  (setq org-show-notification-handler
	'(lambda (m)
	   (let ((ring-bell-function nil))
	     (org-clock-play-sound org-clock-sound)
	     (alert m :timeout 1200 :title "Org Clock Notify" :severity 'high))))
  :bind
  ("C-c C-x C-i" . org-clock-in)
  ("C-c C-x C-o" . org-clock-out)
  ("C-c C-x C-x" . org-clock-in-last)
  )

(use-package org-clock-budget
  :straight (org-clock-budget
             :host github
             :repo "Fuco1/org-clock-budget"
             )
  :commands (org-clock-budget-report)
  :init
  (defun my-buffer-face-mode-org-clock-budget ()
    "Sets a fixed width (monospace) font in current buffer"
    (interactive)
    ;; (setq buffer-face-mode-face '(:family "Iosevka" :height 1.0))
    (buffer-face-mode)
    (setq-local line-spacing nil))
  :config
  (add-hook 'org-clock-budget-report-mode-hook (lambda()
						 (progn
						   (toggle-truncate-lines 1)
						   (my-buffer-face-mode-org-clock-budget)
						   )
						 ))
  )

;; (use-package secretaria
;;   :straight t
;;   :after (alert f s)
;;   :custom
;;   (secretaria-clocked-task-save-file
;;    (expand-file-name "secretaria-clocked-task" poly-cache-dir))
;;   ;; remind me about every 10 minutes
;;   (secretaria-clocked-in-reminder-every-minutes 10)
;;   (secretaria-today-unknown-time-appt-remind-every 10)
;;   :config
;;   ;; use this for getting a reminder every 30 minutes of those tasks scheduled
;;   ;; for today and which have no time of day defined.
;;   (add-hook 'after-init-hook #'secretaria-unknown-time-always-remind-me))

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

(use-package org-mru-clock
  :straight t
  :after (org org-clock)
  :bind (("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-k" . org-mru-clock-in))
  :custom
  (org-mru-clock-how-many 100)
  (org-mru-clock-keep-formatting t)
  (org-mru-clock-predicate nil))

(defvar poly/previously-clocking '())

(defun poly/temporarily-clock-out ()
  (interactive)
  (save-window-excursion
    (if (org-clocking-p)
        (progn
          (org-clock-goto)
          (add-to-list 'poly/previously-clocking
		       (cons org-clock-heading (org-id-get-create)))
          (org-clock-out))
      (message "No clock is active now.")
      )))

(defun poly/mark-clocking-task-as-todo-and-clock-out ()
  (interactive)
  (save-window-excursion
    (if (org-clocking-p)
        (progn
          (org-clock-goto)
          (add-to-list 'poly/previously-clocking
		       (cons org-clock-heading (org-id-get-create)))
          (org-todo 'todo)
          (org-clock-out)
          (save-buffer))
      (message "No clock is active now."))))

(defun poly/resume-previous-clock (id)
  (interactive)
  (save-window-excursion
    (org-id-goto id)
    (org-clock-in)
    (poly/remove-previous-clock id)))

(defun poly/remove-previous-clock (id)
  (when (not (null id))
    (setq poly/previously-clocking
          (--remove (s-equals? id (cdr it)) poly/previously-clocking))))

(defun poly/mark-clocking-task-as-done ()
  "Find current clocking task and mark it as done."
  (interactive)
  (save-window-excursion
    (if (org-clocking-p)
        (progn
          (org-clock-goto)
          (org-todo 'done)
          (poly/remove-previous-clock (org-id-get))
          (save-buffer))
      (message "No clock is active now."))))

(bind-key "C-c C-x C-d" 'poly/mark-clocking-task-as-done)
(bind-key "C-c C-x C-d" 'poly/mark-clocking-task-as-done org-mode-map)

(provide 'init-clock)
;;; init-clock.el ends here
