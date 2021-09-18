;;; lisp/init-gpg.el -*- lexical-binding: t; -*-

;; enable EasyPG handling
;; gpg-agent confuses epa when getting passphrase
(defun my-squash-gpg (&rest ignored-frame)
  "Kill any GPG_AGENT_INFO in our environment."
  (setenv "GPG_AGENT_INFO" nil))

(use-package epa-file
    :straight (:type built-in)
    :pdump nil
    :ensure t
    ;; :if (string-match "socrates" (system-name))
    :commands epa-file-enable
    ;; :init (epa-file-enable)
    ;; :custom
    :config
    (setq     epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'\\|\\.asc"
	      ;; (epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
	      epa-file-cache-passphrase-for-symmetric-encryption t
	      epa-file-select-keys nil
	      epg-gpg-program "/opt/local/bin/gpg2")
    (add-hook 'after-make-frame-functions 'my-squash-gpg t)
    (my-squash-gpg)
    (epa-file-name-regexp-update)
    (epa-file-enable))

(use-package auth-source-pass
    :straight (:type built-in)
    :ensure t
    ;; :if (file-exists-p "~/.password-store")
    :config (auth-source-pass-enable))

;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
;; (epa-file-name-regexp-update)
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)   ;; cache passphrase
;; (setq epa-file-select-keys nil) ;; If non-nil, always asks user to select recipients.

;; (setf epa-pinentry-mode 'loopback)
;; (setq epg-gpg-program "/usr/local/bin/gpg2")

(use-package pinentry
    :straight t
    ;; :pdump nil
    :config
    (pinentry-start)
    (setq epa-pinentry-mode 'loopback))

(provide 'init-gpg)

;;; init-gpg.el ends here
