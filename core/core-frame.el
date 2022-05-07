;;; core/core-frame.el -*- lexical-binding: t; -*-

;; frame configuration can not pdump

;; ;; Display visited file's path in the frame title
;; ;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
;; (setq frame-title-format
;;       `((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))

(setq frame-title-format '(multiple-frames "%b"
					   ("" invocation-name "@" system-name)))

;; Activate winner mode for quickly changing window sizes, etc
(when (fboundp 'winner-mode)
  (winner-mode 1))

(provide 'core-frame)
;;; core-frame.el ends here
