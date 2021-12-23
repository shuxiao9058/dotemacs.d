;;; lisp/init-pomo.el -*- lexical-binding: t; -*-

(setq my-org-pomodoro-start-scpt (expand-file-name "bin/Vitamin-R/Start.applescript" poly-local-dir))
(setq my-org-pomodoro-abort-scpt (expand-file-name "bin/Vitamin-R/Abort.applescript" poly-local-dir))
(setq my-org-pomodoro-stop-scpt (expand-file-name "bin/Vitamin-R/Stop.applescript" poly-local-dir))
(setq my-org-pomodoro-hide-scpt (expand-file-name "bin/Vitamin-R/Hide.applescript" poly-local-dir))

(defun my-get-vitamin-r-rate-option (choice)
  "my org-pomodoro finish"
  (interactive
   (let ((completion-ignore-case  t)
	 (orderless-matching-styles '(orderless-strict-initialism))
	 (completion-styles '(orderless))
	 )
     (list (completing-read "How is you feeling: "
			    '("I felt distracted" "My focus was good" "I felt highly focused" "I experienced \"flow\"") nil t))))
  choice)

(defun my-start-vitamin-r()
  (when (org-clock-is-active)
    (let ((my-todo-title org-clock-heading)
	  (my-todo-tags-list (org-get-tags org-clock-marker)))
      (setq my-todo-tags
	    (if my-todo-tags-list
		(mapconcat 'identity my-todo-tags-list ",") ""))
      (princ my-todo-tags)
      (if my-todo-title
	  (process-lines "/usr/bin/osascript" my-org-pomodoro-start-scpt my-todo-title my-todo-tags)))))

(defun my-stop-vitamin-r()
  (interactive)
  (process-lines "/usr/bin/osascript" my-org-pomodoro-hide-scpt)
  (let ((my-todo-title org-clock-heading)
	(my-rate-time-slice-choice (call-interactively 'my-get-vitamin-r-rate-option))
	(my-pomodoro-count (number-to-string (mod org-pomodoro-count org-pomodoro-long-break-frequency))))
    (process-lines "/usr/bin/osascript" my-org-pomodoro-stop-scpt
		   my-todo-title my-rate-time-slice-choice my-pomodoro-count)
    )
  )

;; (my-stop-vitamin-r)
(defun my-abort-vitamin-r()
  (let ((my-todo-title org-clock-heading))
    (process-lines "/usr/bin/osascript" my-org-pomodoro-abort-scpt my-todo-title)))

(defun poly/org-pomodoro-clocking-info()
  (interactive)
  (when (org-pomodoro-active-p)
    (let ((s (cl-case org-pomodoro-state
               (:pomodoro
		(propertize org-pomodoro-format 'face 'org-pomodoro-mode-line))
               (:overtime
		(propertize org-pomodoro-overtime-format
                            'face 'org-pomodoro-mode-line-overtime))
               (:short-break
		(propertize org-pomodoro-short-break-format
                            'face 'org-pomodoro-mode-line-break))
               (:long-break
		(propertize org-pomodoro-long-break-format
                            'face 'org-pomodoro-mode-line-break))))
	  (clock-is-active (org-clock-is-active)))
      (let ((pomo-status (when (and (org-pomodoro-active-p) (> (length s) 0))
			   (string-trim (concat "[" (format s (org-pomodoro-format-seconds)) "] "))))
	    (pomo-state (string-trim (format s ""))))
	(list pomo-state pomo-status (string-trim (org-clock-get-clock-string)))))))

(defun poly/org-clock-info()
  (when (org-clock-is-active)
    (let ((clock-string (string-trim (org-clock-get-clock-string)))
	  (current-clock-time
	   (let ((currently-clocked-time
		  (floor (org-time-convert-to-integer
			  (time-since org-clock-start-time)) 60)))
	     (org-duration-from-minutes currently-clocked-time))))
      (list nil (concat "[" current-clock-time "]") clock-string)))
  )

(defun poly/org-clock-tick(&optional event)
  (interactive)
  (if (not event)
      (let* ((clock-info
	      (cond((org-pomodoro-active-p) (poly/org-pomodoro-clocking-info))
		   ((org-clock-is-active) (poly/org-clock-info))
		   (t (list nil nil "No Task"))))
	     (state (car clock-info))
	     (status (nth 1 clock-info))
	     (clock-string (nth 2 clock-info)))
	(if clock-string
            (tl/open-hammerspoon-url "Clocking" "status" status "state" state "clockString" clock-string)
	  (tl/open-hammerspoon-url "Clocking" "status" status "state" state)))
    (cond ((string= event "org-clock-out")
	   (unless (org-pomodoro-active-p)
	     (tl/open-hammerspoon-url "Clocking" "status" "No Task")))
	  ((string= event "org-pomodoro-killed-or-break-finished")
	   (tl/open-hammerspoon-url "Clocking" "status" "No Task")))))

;; (poly/org-clock-tick)

(use-package org-pomodoro
  :straight t
  :ensure t
  :after org
  :commands org-pomodoro
  :custom
  (org-pomodoro-format "Pomo %s")
  (org-pomodoro-short-break-format "Break %s")
  (org-pomodoro-long-break-format "Long break %s")
  (org-pomodoro-long-break-length 10)
  (org-pomodoro-start-sound-p nil)
  (org-pomodoro-ticking-sound-p nil)
  (org-pomodoro-ticking-sound-states '(:pomodoro))
  (org-pomodoro-ticking-frequency 5)
  :config
  (add-hook 'org-pomodoro-finished-hook
	    (lambda () (terminal-notify "A pomodoro is finished, take a break !!!" "Pomo")))
  (add-hook 'org-pomodoro-short-break-finished-hook
            (lambda () (terminal-notify "A short break done, ready a new pomodoro !!!" "Pomo")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda () (terminal-notify "A long break done, ready a new pomodoro !!!" "Pomo")))
  ;; (setq org-pomodoro-tick-hook nil)

  (add-hook 'org-pomodoro-tick-hook #'poly/org-clock-tick)
  (add-hook 'org-clock-out-hook (lambda() (poly/org-clock-tick "org-clock-out")))
  (add-hook 'org-pomodoro-break-finished-hook (lambda() (poly/org-clock-tick "org-pomodoro-killed-or-break-finished")))
  (add-hook 'org-pomodoro-killed-hook (lambda() (poly/org-clock-tick "org-pomodoro-killed-or-break-finished")))

  ;; (advice-remove 'org-clock-update-mode-line #'tl/update-hammerspoon-org-clock-bar)
  (advice-add 'org-clock-update-mode-line :after #'poly/org-clock-tick)
  ;; (advice-add 'org-pomodoro-kill :after (lambda() (poly/org-clock-tick "org-pomodoro-killed-or-break-finished")))
  ;; (add-hook 'org-pomodoro-started-hook 'my-start-vitamin-r)
  ;; (add-hook 'org-pomodoro-finished-hook 'my-stop-vitamin-r)
  ;; (add-hook 'org-pomodoro-killed-hook 'my-abort-vitamin-r)
  :bind
  (("C-c C-x C-p" . org-pomodoro)
   :map org-mode-map
   ("C-c C-x C-p" . org-pomodoro)))

(provide 'init-pomo)
;;; init-pomo.el ends here
