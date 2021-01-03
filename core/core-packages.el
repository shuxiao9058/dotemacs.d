;;; core/core-packages.el -*- lexical-binding: t; -*-

;; (use-package server ; built-in
;;   :straight nil
;;   ;; :if (display-graphic-p)
;;   :defer 1
;;   :init
;;   ;; (if IS-WINDOWS
;;   ;;     (progn
;;   ;; 	(setq server-use-tcp t)
;;   ;; 	(setq server-use-socket nil)
;;   ;; 	)
;;   ;;   (setq server-use-tcp nil)
;;   ;;   (setq server-use-socket t))

;;   ;; (setq server-auth-dir  (expand-file-name "emacs-server" poly-cache-dir))
;;   ;; (setq server-socket-dir (expand-file-name "emacs-server" poly-cache-dir))
;;   ;; (setq server-name (expand-file-name "emacs-server-file" server-socket-dir))

;;   ;; (unless (file-exists-p server-auth-dir)
;;   ;;   (make-directory server-auth-dir))
;;   ;; (unless (or (not server-socket-dir) (file-exists-p server-socket-dir))
;;   ;;   (make-directory server-socket-dir))

;;   (defadvice server-ensure-safe-dir
;;       (around my-around-server-ensure-safe-dir activate)
;;     "Ignores any errors raised from server-ensure-safe-dir"
;;     (ignore-errors ad-do-it))
;;   :config
;;   (unless (server-running-p)
;;     (server-start))
;;   )

(use-package autorevert
  :straight nil
  ;; :hook (dired-mode . auto-revert-mode)
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-verbose nil))

;; (use-package undo-fu
;;   :straight (undo-fu :host gitlab
;; 		     :repo "ideasman42/emacs-undo-fu"
;; 		     :files ("undo-fu.el"))
;;   )

(use-package undo-tree
  :if IS-MAC
  :straight (:local-repo  "~/.emacs.d/site-lisp/undo-tree")
  :ensure t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist (list (cons ".*" (expand-file-name "undo-tree-history" poly-cache-dir))))
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-lazy-drawing 1000)
  :config
  (global-undo-tree-mode)
  )

(use-package hide-mode-line
  :straight t
  :commands (hide-mode-line-mode))

(use-package xclip
  :straight t
  :if IS-LINUX
  :ensure t
  :custom
  (xclip-method 'xclip)
  :config
  (xclip-mode +1)
  (xterm-mouse-mode +1)
  )

(use-package clipetty
  :straight t
  :ensure t
  :hook (after-init . global-clipetty-mode))

(use-package pbcopy
  :straight t
  ;; :if (and IS-MAC (not IS-GUI))
  :if IS-MAC
  :init (turn-on-pbcopy)
  )

;; (use-package posframe
;;   :straight (posframe
;;     :host github
;; 		      :repo "tumashu/posframe"
;; 		      :files ("posframe.el"))
;;   :ensure t)

(use-package restart-emacs
  :straight t
  :ensure t)

;; Sorting and filtering
(use-package prescient
  :straight t)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(use-package gcmh
  :straight t
  :custom
  (gcmh-verbose             nil)
  ;; (gcmh-lows-cons-threshold #x800000)
  (gcmh-high-cons-threshold most-positive-fixnum)
  ;; (gc-cons-percentage 0.1)
  (gcmh-idle-delay 10)
  ;; :hook ((focus-out  . gcmh-idle-garbage-collect)
  ;; 	   (pre-command . (lambda()(gcmh-mode +1)))
  ;; 	   )
  :config
  (setq gc-cons-percentage 0.6)
  (when (not noninteractive)
    (gcmh-mode +1)
    (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect)
    )
  ;; (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  ;; (add-hook 'pre-command-hook (lambda ()(gcmh-mode +1)))
  )

;;;; disable annoying notifications
(defcustom message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                        "^Ispell process killed$"
                                        ".+expected selector or type assertion, found.+"
                                        ".+expected identifier on left side.+"
                                        "^LSP ::.+"
                                        ".+and \d{1,10} more errors.+"
                                        "Wrote "
                                        "Liberime: start with shared dir" ;;; liberime
					".+Starting new Ispell process.+" ;;; ispell
					"Package cl is deprecated"
					"Loading[\s\w\/\.-]+\(module\).+"
					;; "Loading[\w\/\d\W]+\(module\).+" ;;; module load
					"For information about GNU Emacs and the GNU system.+"
                                        )
  "filter formatted message string to remove noisy messages"
  :type '(list string)
  :group 'general)

(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (if (and (stringp formatted-string)
               (cl-some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
	  (let ((inhibit-read-only t))
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (insert formatted-string "\n")))
	(progn
	  (ad-set-args 0 `("%s" ,formatted-string))
	  ad-do-it)))))

(provide 'core-packages)
;;; core-packages.el ends here
