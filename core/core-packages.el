;;; core/core-packages.el -*- lexical-binding: t; -*-

(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
	    (lambda ()
	      (advice-remove #'tty-run-terminal-initialization #'ignore)
	      (tty-run-terminal-initialization (selected-frame) nil t))))

(use-package server ; built-in
  :straight nil
  :defer 1
  :init
  (if IS-WINDOWS
      (progn
	(setq server-use-tcp t)
	(setq server-use-socket nil))
    (setq server-use-tcp nil)
    (setq server-use-socket t))

  (defadvice server-ensure-safe-dir
      (around my-around-server-ensure-safe-dir activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it))
  :config
  (unless (server-running-p)
    (server-start)))

;; (use-package files
;;   :straight nil
;;   :ensure t
;;   :init
;;   (setq make-backup-files nil
;;         enable-local-variables :all
;;         create-lockfiles nil
;;         auto-save-default nil
;;         auto-save-list-file-prefix nil
;;         save-silently t
;;         confirm-kill-processes nil
;;         find-file-suppress-same-file-warnings t))


(use-package autorevert
  :straight nil
  :diminish auto-revert-mode
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

;;; Undo-Fu
;; trying another undo package
;; https://gitlab.com/ideasman42/emacs-undo-fu
(use-package undo-fu
  :straight (undo-fu
	     :host gitlab
	     :repo "ideasman42/emacs-undo-fu"
	     :files ("undo-fu.el"))
  :ensure t
  :demand t
  :custom
  ;; Store more undo history to prevent loss of data
  (undo-limit 400000)
  (undo-strong-limit 3000000)
  (undo-outer-limit 3000000))

;; persistent undo across sessions
(use-package undo-fu-session
  :straight t
  :after undo-fu
  :demand t
  :custom
  (undo-fu-session-file-limit nil)
  (undo-fu-session-directory (expand-file-name "undo-fu-session" poly-cache-dir))
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
:config
(with-eval-after-load 'undo
  (global-undo-fu-session-mode))

(use-package undo-tree
  :disabled
  :straight (:type git :host nil :repo "http://www.dr-qubit.org/git/undo-tree.git")
  ;; :disabled
  ;; :if IS-MAC
  :commands global-undo-tree-mode
  ;; Pull package directly from maintainer, the elpa package is behind.
  ;; :straight (:local-repo  "~/.emacs.d/site-lisp/undo-tree")
  :demand
  :delight
  :ensure t
  :custom
  ;; supposedly causes errors in undo read
  ;; see https://emacs.stackexchange.com/a/34214/11934
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  ;; (undo-tree-history-directory-alist (list (cons ".*" (expand-file-name "undo-tree-history" poly-cache-dir))))
  ;; ;; stop littering - set undo directory
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-history" poly-cache-dir))))
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-lazy-drawing 1000)
  :config
  (global-undo-tree-mode))

(use-package hide-mode-line
  :straight t
  :commands (hide-mode-line-mode))

;; (use-package xclip
;;   :straight t
;;   ;; :if IS-LINUX
;;   :ensure t
;;   :custom
;;   (xclip-method 'xclip)
;;   :config
;;   (xclip-mode +1)
;;   (xterm-mouse-mode +1)
;;   )

(use-package clipetty
  :straight t
  :ensure t
  :hook (after-init . global-clipetty-mode)
  )

(use-package pbcopy
  :straight t
  :if IS-MAC
  :init (turn-on-pbcopy))

(use-package reveal-in-osx-finder
  :straight t
  :if IS-MAC
  :commands reveal-in-osx-finder
  :bind ("C-c z" . reveal-in-osx-finder))

;; (use-package posframe
;;   :straight (posframe
;; 	     :host github
;; 	     :repo "tumashu/posframe"
;; 	     :files ("posframe.el"))
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
  ;; (gcmh-lows
  ;; -cons-threshold #x800000)
  (gcmh-high-cons-threshold most-positive-fixnum)
  ;; (gc-cons-percentage 0.1)
  (gcmh-idle-delay 10)
  :config
  (setq gc-cons-percentage 0.6)
  (when (not noninteractive)
    (gcmh-mode +1)
    (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect)
    ))

(use-package command-log-mode
  :straight t
  :ensure t
  :config
  (global-command-log-mode))

(use-package transient
  :straight t
  :bind
  (:map transient-map
	([escape] . transient-quit-one)
	("q" . transient-quit-one)))

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C-<" . mc/mark-next-like-this)
	 ("C->" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; required by core-hammerspoon
(use-package dash
  :straight t)

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
					".+search-failed.+"
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

;; Michael Hoffman at the comment of
;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html

(defalias 'tl/message-orig (symbol-function 'message))

;; Unfortunately this isn't re-entrant, so if you stack uses of
;; with-suppress-message I think only the innermost regexes will still be
;; suppressed. The this-fn of noflet would be nice but I use this very early in
;; my emacs startup so I wouldn't necessarily have access to it.
(defmacro tl/with-suppress-message (regex &rest body)
  "Suppress any `message' starting with REGEX when executing BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message)
              (lambda (format-string &rest args)
                (unless (string-match-p ,regex format-string)
                  (apply 'tl/message-orig format-string args)))))
     ,@body))

;; enable winner-mode
(winner-mode 1)

(provide 'core-packages)
;;; core-packages.el ends here
