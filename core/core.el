;;; core/core.el --- the heart of the beast -*- lexical-binding: t; -*-

(when (version< emacs-version "26")
  (error "Detected Emacs %s. only supports Emacs 26 and higher"
         emacs-version))

;; Ensure `poly-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

(defalias 'yes-or-no-p 'y-or-n-p)

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(unless noninteractive
  (setq file-name-handler-alist nil))

(require 'core-variables)

;; ;; Disable splash screen
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message nil)

;; Disable startup-screen and message
(setq inhibit-startup-screen t)

;; Makes *scratch* empty.
(setq initial-scratch-message nil)

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

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
    (require 'core-font)
    (require 'core-keybindings)
    (require 'core-packages)
    (require 'core-hammerspoon)
    ;; (require 'core-autoload)
    ;; (poly-load-autoload)
    ;; (when (not noninteractive)
    ;;   (add-to-list 'command-switch-alist (cons "--restore" #'poly-restore-session-handler))
    ;;   )
    )
  (setq poly-initialize-core-p t)
  )

(poly-initialize-core)

(provide 'core)
;;; core.el ends here
