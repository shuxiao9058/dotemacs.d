;; -*- coding: utf-8; lexical-binding: t; -*-

(defun should-use-minimum-resource ()
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\)\.js" buffer-file-name)))

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list
      '(eshell-mode
        shell-mode
        profiler-report-mode
        ffip-diff-mode
        dictionary-mode
        erc-mode
        dired-mode
        help-mode
        text-mode
        fundamental-mode
        jabber-roster-mode
        jabber-chat-mode
        inferior-js-mode
        inferior-python-mode
        ivy-occur-grep-mode ; better performance
        ivy-occur-mode ; better performance
        twittering-mode
        compilation-mode
        weibo-timeline-mode
        woman-mode
        Info-mode
        calc-mode
        calc-trail-mode
        comint-mode
        gnus-group-mode
        gud-mode
        org-mode
        vc-git-log-edit-mode
        log-edit-mode
	vterm-mode
        term-mode
        w3m-mode
        speedbar-mode
        gnus-summary-mode
        gnus-article-mode
	company-mode
	magit-status-mode
	messages-buffer-mode
        calendar-mode
	telega-chat-mode
	telega-root-mode))

(cond
 ;; ((fbounp 'linum-mode))
 (nil;; (fboundp 'global-display-line-numbers-mode)
  (defun display-line-numbers-mode-hook-setup ()
    (setq display-line-numbers (if (or (memq major-mode linum-mode-inhibit-modes-list)
                                       ;; don't show line number for certain file extensions
                                       (should-use-minimum-resource))
                                   nil
                                 t)))
  (add-hook 'display-line-numbers-mode-hook 'display-line-numbers-mode-hook-setup)
  (global-display-line-numbers-mode t))
 (t
  (global-linum-mode t)

  (defadvice linum-on (around linum-on-inhibit-for-modes)
    "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))
  (ad-activate 'linum-on)

  ;; update line number every second so `linum-mode' won't slow down Emacs
  ;; @see https://lists.gnu.org/archive/html/bug-gnu-emacs/2013-04/msg00577.html
  ;; package like `nlinum-mode' has better performance but `git-gutter' is dependent
  ;; on `linum-mode'.
  ;; So we have to use `linum-mode'.
  (setq linum-delay t)
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 2 nil #'linum-update-current))))

(provide 'init-linum)
;;; init-linum.el ends here
