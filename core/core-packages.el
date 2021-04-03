;;; core/core-packages.el -*- lexical-binding: t; -*-

;; ;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;; ;; reason; inexplicably doubling startup time for terminal Emacs. Keeping
;; ;; it disabled will have nasty side-effects, so we simply delay it until
;; ;; later in the startup process and, for some reason, it runs much faster
;; ;; when it does.
;; (unless (daemonp)
;;   (defun doom-init-tty-h ()
;;     (advice-remove #'tty-run-terminal-initialization #'ignore)
;;     (tty-run-terminal-initialization (selected-frame) nil t))
;;   (advice-add #'tty-run-terminal-initialization :override #'ignore)
;;   (add-hook 'window-setup-hook #'doom-init-tty-h))

;; (require 'faces)
;; (setq xterm-extra-capabilities nil)


(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
	    (lambda ()
	      (advice-remove #'tty-run-terminal-initialization #'ignore)
	      (tty-run-terminal-initialization (selected-frame) nil t))))

(use-package server ; built-in
  :straight nil
  ;; :if (display-graphic-p)
  :defer 1
  :init
  (if IS-WINDOWS
      (progn
	(setq server-use-tcp t)
	(setq server-use-socket nil)
	)
    (setq server-use-tcp nil)
    (setq server-use-socket t))

  ;; (setq server-auth-dir  (expand-file-name "emacs-server" poly-cache-dir))
  ;; (setq server-socket-dir (expand-file-name "emacs-server" poly-cache-dir))
  ;; (setq server-name (expand-file-name "emacs-server-file" server-socket-dir))

  ;; (unless (file-exists-p server-auth-dir)
  ;;   (make-directory server-auth-dir))
  ;; (unless (or (not server-socket-dir) (file-exists-p server-socket-dir))
  ;;   (make-directory server-socket-dir))

  (defadvice server-ensure-safe-dir
      (around my-around-server-ensure-safe-dir activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it))
  :config
  (unless (server-running-p)
    (server-start))
  )

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
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-verbose nil))

;;; Undo-Fu
;; trying another undo package
;; https://gitlab.com/ideasman42/emacs-undo-fu
(use-package undo-fu
  :straight (undo-fu
	     :host gitlab
	     :repo "ideasman42/emacs-undo-fu"
	     :files ("undo-fu.el"))
  ;; :after evil
  :ensure t
  ;; :disabled
  :demand t
  ;; :init
  ;; ;; `evil' activates undo-tree, so we must pre-emptively disable it.
  ;; (eval-after-load 'undo-tree
  ;;   (global-undo-tree-mode -1))
  ;; :general
  ;; (:states '(normal)
  ;; 	   "u" 'undo-only
  ;; 	   "\C-r" 'undo-fu-only-redo)
  ;; (:states '(normal insert motion emacs)
  ;; 	   "s-z" 'undo-fu-only-undo
  ;; 	   "s-Z" 'undo-fu-only-redo)
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
  (global-undo-tree-mode)
  )

(use-package hide-mode-line
  :straight t
  :commands (hide-mode-line-mode))

(use-package exec-path-from-shell
  :straight t
  :ensure t
  :if (and IS-MAC IS-GUI)
  :config
  (when (file-executable-p "/usr/local/bin/fish")
    (setq exec-path-from-shell-shell-name "/usr/local/bin/fish"
          exec-path-from-shell-debug nil))

  ;; (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables
	'("PATH"
	  "PYENV_ROOT"
	  "JAVA_HOME"
	  "GOPATH"
	  "GOINSECURE"
	  "GOINSECURE"
	  "SDKMAN_DIR"))

  (setenv "GOPROXY" "")

  (exec-path-from-shell-initialize)

  (if (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
      (progn
	(message "Native comp is available")
	;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
	;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
	(add-to-list 'exec-path (concat invocation-directory "bin") t)
	(setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
				       (when (getenv "LIBRARY_PATH")
					 ":")
				       ;; This is where Homebrew puts gcc libraries.
				       (car (file-expand-wildcards
					     (expand-file-name "/usr/local/lib/gcc/10")))))
	(setenv "DYLD_LIBRARY_PATH" (concat (getenv "DYLD_LIBRARY_PATH")
					    (when (getenv "DYLD_LIBRARY_PATH") ":")
					    ;; This is where Homebrew puts gcc libraries.
					    (car (file-expand-wildcards
						  (expand-file-name "/usr/local/lib/gcc/10")))))
	;; Only set after LIBRARY_PATH can find gcc libraries.
	(setq comp-deferred-compilation t))
    (message "Native comp is *not* available"))
  )

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
  ;; :after evil
  :ensure t
  :hook (after-init . global-clipetty-mode)
  ;; :config
  ;; ;; copy to system clipboard with `:copy`
  ;; (evil-ex-define-cmd "copy" #'clipetty-kill-ring-save)
  )

(use-package pbcopy
  :straight t
  :if IS-MAC
  ;; :if IS-MAC
  :init (turn-on-pbcopy)
  )

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

(use-package command-log-mode
  :straight t
  :ensure t
  ;; :config
  ;; (global-command-log-mode)
  )

;; ;; Keep ~/.emacs.d clean
;; (use-package no-littering
;;   :straight t
;;   :ensure t
;;   :demand t)

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
