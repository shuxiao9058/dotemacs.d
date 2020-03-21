;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

(when (version< emacs-version "27")
  (error "Detected Emacs %s. only supports Emacs 27 and higher"
         emacs-version))

;; Ensure `poly-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(unless noninteractive
  (setq file-name-handler-alist nil))

(require 'core-variables)

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

(defun poly-initialize-core ()
  "Load Poly's core files for an interactive session."
  (if  poly-initialize-core-p
      nil
    (require 'core-straight)
    (require 'core-packages)
    (setq poly-initialize-core-p t)
    )
  )

(poly-initialize-core)

(provide 'core)
