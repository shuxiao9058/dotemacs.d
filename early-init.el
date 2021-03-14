;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; ;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)

(when (boundp 'comp-eln-load-path)
  (let ((eln-cache-dir (expand-file-name ".local/cache/eln-cache/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))
    (setcar comp-eln-load-path eln-cache-dir)
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete"))))

(if (and (fboundp 'native-comp-available-p) (native-comp-available-p) (fboundp 'json-serialize))
    (progn
      ;; Prevent compilation of this package
      (require 'comp)
      (setq comp-deferred-compilation t
            package-native-compile t
	    ;; native-comp settings per
	    ;; https://github.com/shshkn/emacs.d/blob/master/docs/nativecomp.md
            comp-speed 2
	    comp-async-report-warnings-errors nil
	    comp-verbose 0
	    comp-async-jobs-number 2
	    )

      ;; https://github.com/raxod502/straight.el/issues/680
      (setq comp-deferred-compilation-deny-list
	    '("init\\.el$"
	      "xterm\\.el$"
	      "^.+evil-pkg\\.el$"
	      "markdown-toc-pkg\\.el$"
	      ".+-pkg\\.el$"
	      "\\(?:[^a-z]+-autoloads\\.el$\\)"))
      (setq comp-deferred-compilation-black-list
            '(xterm "/xterm\\.el$" "/xterm\\.el.gz$" "/evil-collection-vterm\\.el\\'" "/mu4e.*\\.el$"))
					; (add-to-list 'comp-deferred-compilation-deny-list "init\\.el$")
      ;; (native--compile-async '("~/.emacs.d/lisp/" "~/.emacs.d/themes/" "~/.emacs.d/modules/" "~/.emacs.d/local-config.el") t)
      )
  (message "Not support native-comp"))

;;; Automatic Optimization
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))
(setq file-name-handler-alist-original file-name-handler-alist)
(setq inhibit-compacting-font-caches nil)
(setq file-name-handler-alist nil)
;; (run-with-idle-timer 5 t #'garbage-collect)
(run-with-idle-timer 5 nil (lambda ()
                             (setq gc-cons-threshold gc-cons-threshold-original)
                             (setq file-name-handler-alist file-name-handler-alist-original)
                             (makunbound 'gc-cons-threshold-original)
                             (makunbound 'file-name-handler-alist-original)))


;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; Disable Emacs 27's automatic package.el initialization before the init.el
;; file is loaded. I use straight.el instead of package.el.
(setq package-enable-at-startup nil)


;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; (setq warning-minimum-level :emergency)
;; (setq debug-on-error nil)

;; (setq warning-minimum-level :debug)
;; (setq debug-on-error t)

;; (setq stack-trace-on-error t)

;; (setq debug-on-entry t)

;; (debug-on-entry 'tty-find-type)

;; (tty-run-terminal-initialization (selected-frame) "xterm-256color")
