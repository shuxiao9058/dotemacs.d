;;; lisp/init-desktop.el -*- lexical-binding: t; -*-


(defun sanityinc/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)

(defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

(use-package desktop
  ;; :defer 2
  :straight nil
  :init
  (setq desktop-save t
        desktop-load-locked-desktop t)
  (setq desktop-path (list user-emacs-directory))
  :custom
  (desktop-auto-save-timeout 600)
  :config

  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
  ;; Save a bunch of variables to the desktop file.
  ;; For lists, specify the length of the maximal saved data too.
  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
	'((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (ivy-history              . 100)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))

  ;; http://emacs.stackexchange.com/a/20036/115
  ;; fix warning upon restoring desktop save file
  (setq desktop-restore-frames nil)

  (defun rag/bury-star-buffers ()
    "Bury all star buffers."
    (mapc (lambda (buf)
            (when (string-match-p "\\`\\*.*\\*\\'" (buffer-name buf))
              (bury-buffer buf)))
          (buffer-list)))
  (add-hook 'desktop-after-read-hook #'rag/bury-star-buffers)

  (defun rag/restore-last-saved-desktop ()
    "Enable `desktop-save-mode' and restore the last saved desktop."
    (interactive)
    (setq desktop-path (list user-emacs-directory))
    (desktop-save-mode 1)
    (desktop-read))

  ;; (desktop-save-mode 0)
  (desktop-save-mode 1)
  ;; Add a hook when emacs is closed to we reset the desktop
  ;; modification time (in this way the user does not get a warning
  ;; message about desktop modifications)
  (add-hook 'kill-emacs-hook
            (lambda ()
              ;; Reset desktop modification time so the user is not bothered
              (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
  :bind (("<S-f2>" . desktop-save-in-desktop-dir)
         ("<C-f2>" . rag/restore-last-saved-desktop))
  )

(use-package session
  :straight t
  :custom
  (session-save-file (locate-user-emacs-file ".session"))
  (session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (session-save-file-coding-system 'utf-8)
  :hook (after-init . session-initialize))


;; Restore histories and registers after saving
(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(provide 'init-desktop)
;;; init-desktop.el ends here
