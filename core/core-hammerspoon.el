
;;; core/core-hammerspoon.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; https://github.com/deftsp/.emacs.d/blob/fe38ec59ae630c5b88df9d10f40e33a2159113fb/lisp/50hammerspoon.el
;; (require 'dash)

(defun tl/open-hammerspoon-url (event &rest params)
  (let ((len (length params))
        (url (concat "hammerspoon://" event)))
    (when (> len 0)
      (if (zerop (% len 2))
          (let ((querys (--reduce (format "%s&%s" acc it)
                                  (-map (lambda (l)
                                          (format "%s=%s" (url-encode-url (car l)) (url-encode-url (cadr l)))
					  ;; (format "%s=%s" ( car l) ( cadr l))
					  )
                                        (-partition-all 2 params)))))
            (setq url (concat url "?" querys)))
        (error "illegal hammerspoon params")))
    ;; (print url)
    (tl/with-suppress-message "Shell command succeeded with"
      (shell-command (format "open -g \"%s\""
			     ;; (url-encode-url url)
			     url
			     ;; (url-encode-url url)
			     )))))

(defun tl/notify-hammerspoon-did-init ()
  (tl/open-hammerspoon-url "emacs_did_init"))

(add-hook 'after-init-hook #'tl/notify-hammerspoon-did-init t)

(defun tl/notify-hammerspoon-did-kill ()
  (tl/open-hammerspoon-url "emacs_did_kill"))

(add-hook 'kill-emacs-hook #'tl/notify-hammerspoon-did-kill t)

(provide 'core-hammerspoon)
;;; core-hammerspoon.el ends here
