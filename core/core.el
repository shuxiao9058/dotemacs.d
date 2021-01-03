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

;; ;; Disable splash screen
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message nil)

;; Disable startup-screen and message
(setq inhibit-startup-screen t)

;; Makes *scratch* empty.
(setq initial-scratch-message nil)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; ;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;;;
;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Load the bare necessities
(require 'core-lib)

;; REVIEW Fixes 'void-variable tab-prefix-map' errors caused by packages that
;;        prematurely use this variable before it was introduced. Remove this in
;;        a year.
(unless (boundp 'tab-prefix-map)
  (defvar tab-prefix-map (make-sparse-keymap)))

(defvar poly-initialize-core-p nil
  "whether initialization core")

(defun poly-initialize-core ()
  "Load Poly's core files for an interactive session."
  (if poly-initialize-core-p
      nil
    (require 'core-straight)
    (require 'core-ui)
    (require 'core-keybindings)
    (require 'core-packages)
    (require 'core-evil)
    (require 'core-autoload)
    (poly-load-autoload)
    ;; (when (not noninteractive)
    ;;   (add-to-list 'command-switch-alist (cons "--restore" #'poly-restore-session-handler))
    ;;   )
    )
  (setq poly-initialize-core-p t)
  )

(poly-initialize-core)

(provide 'core)
;;; core.el ends here
