;;; core/core-variables.el -*- lexical-binding: t; -*-

(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Symbol’s function definition is void: if-let
(require 'subr-x)

;;; Directories/files
(defconst poly-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst poly-core-dir (concat poly-emacs-dir "core/")
  "The root directory of Poly's core files. Must end with a slash.")

(defconst poly-modules-dir (concat poly-emacs-dir "modules/")
  "The root directory for Poly's modules. Must end with a slash.")

(defconst poly-local-dir
  (if-let (localdir (getenv "POLYLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (concat poly-emacs-dir ".local/"))
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

(defconst poly-docs-dir (concat poly-emacs-dir "docs/")
  "Where Doom's documentation files are stored. Must end with a slash.")

(defconst poly-private-dir
  (if-let (polydir (getenv "POLYDIR"))
      (expand-file-name (file-name-as-directory polydir))
    (or (let ((xdgdir
               (expand-file-name "poly/"
                                 (or (getenv "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.poly.d/"))
  "Where your private configuration is placed.

Defaults to ~/.config/poly, ~/.poly.d or the value of the DOOMDIR envvar;
whichever is found first. Must end in a slash.")

(defconst poly-autoload-file (concat poly-local-dir "autoloads.el")
  "Where `poly-reload-core-autoloads' stores its core autoloads.

This file is responsible for informing Emacs where to find all of Doom's
autoloaded core functions (in core/autoload/*.el).")

(defconst poly-package-autoload-file (concat poly-local-dir "autoloads.pkg.el")
  "Where `poly-reload-package-autoloads' stores its package autoloads.

This file is compiled from the autoloads files of all installed packages
combined.")

(provide 'core-variables)

;;; core-variables.el ends here