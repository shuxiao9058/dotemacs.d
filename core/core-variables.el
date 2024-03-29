;;; core/core-variables.el -*- lexical-binding: t; -*-

(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-GUI (display-graphic-p))
(defconst IS-CONSOLE (not IS-GUI))

(defun IS-AARCH64 ()
  (string-match-p "aarch64-apple-darwin.+" system-configuration))

(defconst IS-AARCH64
  (if (IS-AARCH64) t nil)
  "Whether apple arm chip")

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

(defvar poly-pdump-load-path nil
  "The load-path backup before dump.
This variable is non-nil when emacs is started with dump file.")

(defvar poly-pdump-packages '(cl-lib)
  "A list of package names to dump.")

(defun poly-pdump-packages (&rest pkgs)
  "Mark pkgs should be dumped."
  (dolist (pkg pkgs)
    ;; (push pkg poly-pdump-packages)
    (cl-pushnew pkg poly-pdump-packages :test #'string=)))

(defvar poly-use-package-always-pdump t
  "always set :pdump to use-package"
  )

(defvar poly-use-lsp-mode nil
  "use lsp-mode for completion"
  )

(defvar poly-use-company nil
  "use company for completion"
  )

;; (defconst poly/using-native-comp (fboundp 'native-comp-available-p)
;;   ;; for native comp branch
;;   )
(defconst poly/using-native-comp
  (and poly-enable-native-comp (fboundp 'native-comp-available-p))
  ;; native compilation
  )

;; (when (boundp 'comp-deferred-compilation)
;;   (setq comp-deferred-compilation t))

(setq x-select-enable-clipboard           t
      x-select-enable-primary             t
      save-interprogram-paste-before-kill t
      apropos-do-all                      t
      mouse-yank-at-point                 t
      require-final-newline               t
      ;; silence ad-handle-definition about advised functions getting redefined
      ad-redefinition-action 'accept)

(provide 'core-variables)
;;; core-variables.el ends here
