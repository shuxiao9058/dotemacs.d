;;; lisp/init-session

(defvar desktop-base-file-name)
(defvar desktop-dirname)
(defvar desktop-restore-eager)
(defvar desktop-file-modtime)

;;
;;; Helpers

(defun poly-session-file (&optional name)
  "TODO"
  (cond ((require 'persp-mode nil t)
         (expand-file-name (or name persp-auto-save-fname) persp-save-dir))
        ((require 'desktop nil t)
         (if name
             (expand-file-name name (file-name-directory (desktop-full-file-name)))
           (desktop-full-file-name)))
        ((error "No session backend available"))))

(defun poly-save-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (poly-session-file))))
  (cond ((require 'persp-mode nil t)
         (unless persp-mode (persp-mode +1))
         (setq persp-auto-save-opt 0)
         (persp-save-state-to-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (let ((frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                              frameset-filter-alist))
               (desktop-base-file-name (file-name-nondirectory file))
               (desktop-dirname (file-name-directory file))
               (desktop-restore-eager t)
               desktop-file-modtime)
           (make-directory desktop-dirname t)
           ;; Prevents confirmation prompts
           (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t))))
        ((error "No session backend to save session with"))))

(defun poly-load-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (poly-session-file))))
  (message "Attempting to load %s" file)
  (cond ((not (file-readable-p file))
         (message "No session file at %S to read from" file))
        ((require 'persp-mode nil t)
         (unless persp-mode
           (persp-mode +1))
         (let ((allowed (persp-list-persp-names-in-file file)))
           (cl-loop for name being the hash-keys of *persp-hash*
              unless (member name allowed)
              do (persp-kill name))
           (persp-load-state-from-file file)))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))


;;
;;; Command line switch

(defun poly-restore-session-handler (&rest _)
  "TODO"
  (add-hook 'window-setup-hook #'poly-load-session 'append))

;;
;;; Commands

(defun poly/quickload-session ()
  "TODO"
  (interactive)
  (message "Restoring session...")
  (poly-load-session)
  (message "Session restored. Welcome back."))

(defun poly/quicksave-session ()
  "TODO"
  (interactive)
  (message "Saving session")
  (poly-save-session)
  (message "Saving session...DONE"))

(defun poly/load-session (file)
  "TODO"
  (interactive
   (let ((session-file (poly-session-file)))
     (list (or (read-file-name "Session to restore: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file)
                               t)
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Loading '%s' session" file)
  (poly-load-session file)
  (message "Session restored. Welcome back."))

(defun poly/save-session (file)
  "TODO"
  (interactive
   (let ((session-file (poly-session-file)))
     (list (or (read-file-name "Save session to: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Saving '%s' session" file)
  (poly-save-session file))


(defun poly/restart-and-restore (&optional debug)
  "TODO"
  (interactive "P")
  (setq poly-autosave-session nil)
  (poly/quicksave-session)
  (restart-emacs
   (append (if debug (list "--debug-init"))
           (when (boundp 'chemacs-current-emacs-profile)
             (list "--with-profile" chemacs-current-emacs-profile))
           (list "--restore"))))

(provide 'init-session)
