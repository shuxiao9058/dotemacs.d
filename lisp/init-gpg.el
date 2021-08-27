;;; lisp/init-gpg.el -*- lexical-binding: t; -*-

;; enable EasyPG handling
;; gpg-agent confuses epa when getting passphrase
(defun my-squash-gpg (&rest ignored-frame)
  "Kill any GPG_AGENT_INFO in our environment."
  (setenv "GPG_AGENT_INFO" nil))

(use-package epa-file
  :straight nil
  :pdump nil
  ;; :if (string-match "socrates" (system-name))
  :commands epa-file-enable
  :init (epa-file-enable)
  :custom
  (epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'\\|\\.asc")
  ;; (epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  (epa-file-cache-passphrase-for-symmetric-encryption t)
  (epa-file-select-keys nil)
  (epg-gpg-program "/usr/local/bin/gpg2")
  :config
  (progn
    (add-hook 'after-make-frame-functions 'my-squash-gpg t)
    (my-squash-gpg)
    (epa-file-enable)
    (epa-file-name-regexp-update)))

(use-package auth-source-pass
  :ensure t
  :if (file-exists-p "~/.password-store")
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
  :config
  (progn
    (pinentry-start)
    (setq epa-pinentry-mode 'loopback)))

(provide 'init-gpg)

;;; init-gpg.el ends here
