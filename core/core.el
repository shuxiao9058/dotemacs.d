;;; core/core.el --- the heart of the beast -*- lexical-binding: t; -*-

(when (version< emacs-version "26")
  (error "Detected Emacs %s. only supports Emacs 26 and higher"
         emacs-version))

;; Ensure `poly-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(unless noninteractive
  (setq file-name-handler-alist nil))

(require 'core-variables)

;;; cua-mode for colum-editing
;; windows style keybind C-x, C-v, cut paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands

;;;
;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'core-ui)

;; *** watcher
(add-to-list 'load-path
	     (expand-file-name "watcher" (file-name-directory load-file-name)))
(require 'watcher)

;; Load the bare necessities
(watcher:try-load 'core-lib)

(defvar poly-initialize-core-p nil
  "whether initialization core")


;; ;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; ;; collect; staving off the collector while the user is working.
;; (when  (not noninteractive)
;;   (setq gc-cons-percentage 0.6)
;;   ;; (add-transient-hook! 'pre-command-hook (gcmh-mode +1))
;;   (with-eval-after-load 'gcmh
;;     (setq gcmh-idle-delay 10
;;           gcmh-high-cons-threshold 16777216
;;           gcmh-verbose doom-debug-mode  ; 16mb
;;           gc-cons-percentage 0.1)
;;     (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)))

(defun poly-initialize-core ()
  "Load Poly's core files for an interactive session."
  (if poly-initialize-core-p
      nil
    (require 'core-straight)
    (require 'core-keybindings)
    (require 'core-packages)
    (require 'core-evil)
    (setq poly-initialize-core-p t)
    (when (not noninteractive)
      (add-to-list 'command-switch-alist (cons "--restore" #'poly-restore-session-handler))
      )
    )
  )

(poly-initialize-core)

(provide 'core)
;;; core.el ends here
