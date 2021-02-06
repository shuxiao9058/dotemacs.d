;;; core/core-variables.el -*- lexical-binding: t; -*-

(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-GUI (display-graphic-p))
(defconst IS-CONSOLE (not IS-GUI))

;;; Directories/files
(defconst poly-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst poly-core-dir (concat poly-emacs-dir "core/")
  "The root directory of Poly's core files. Must end with a slash.")

(defconst poly-site-lisp-dir (concat poly-emacs-dir "site-lisp/"))

(defconst poly-local-dir
  (let ((localdir (getenv "POLYLOCALDIR")))
    (if localdir
	(expand-file-name (file-name-as-directory localdir))
      (expand-file-name ".local/" poly-emacs-dir)))
  "Root directory for local storage.
Use this as a storage location for this system's installation of Doom Emacs.
These files should not be shared across systems. By default, it is used by
`doom-etc-dir' and `doom-cache-dir'. Must end with a slash.")

(defconst poly-etc-dir (concat poly-local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst poly-cache-dir (concat poly-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defconst poly-autoload-dir (expand-file-name "autoload/" poly-emacs-dir)
  "Directory for autoload files.")


(defconst poly/using-native-comp (fboundp 'native-comp-available-p)
  ;; for native comp branch
  )

;; (when (boundp 'comp-deferred-compilation)
;;   (setq comp-deferred-compilation t))

(setq x-select-enable-clipboard           t
      x-select-enable-primary             t
      save-interprogram-paste-before-kill t
      apropos-do-all                      t
      mouse-yank-at-point                 t
      require-final-newline               t)


(provide 'core-variables)
;;; core-variables.el ends here
