;;; lisp/init-env.el -*- lexical-binding: t; -*-

;; (use-package exec-path-from-shell
;;   :straight t
;;   :demand t
;;   :if IS-MAC
;;   ;; :init
;;   ;; (setq exec-path-from-shell-check-startup-files nil)
;;   :config
;;   (exec-path-from-shell-initialize)

;;   ;; ;; TODO remove old path locations
;;   ;; (setq exec-path (cons "/opt/local/bin" exec-path))
;;   ;; (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
;;   )

;; (use-package exec-path-from-shell
;;     :straight t
;;     :ensure t
;;     :if IS-MAC
;;     :config
;;     (exec-path-from-shell-initialize)

;;     ;; (if (and (fboundp 'native-comp-available-p)
;;     ;; 	   (native-comp-available-p))
;;     ;;     (progn
;;     ;; 	(message "Native comp is available")
;;     ;; 	;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
;;     ;; 	;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
;;     ;; 	(add-to-list 'exec-path (concat invocation-directory "bin") t)
;;     ;; 	(setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
;;     ;; 				       (when (getenv "LIBRARY_PATH")
;;     ;; 					 ":")
;;     ;; 				       ;; This is where Homebrew puts gcc libraries.
;;     ;; 				       (car (file-expand-wildcards
;;     ;; 					     (expand-file-name "/usr/local/opt/gcc/lib/gcc/10")))))
;;     ;; 	(setenv "DYLD_LIBRARY_PATH" (concat (getenv "DYLD_LIBRARY_PATH")
;;     ;; 					    (when (getenv "DYLD_LIBRARY_PATH") ":")
;;     ;; 					    ;; This is where Homebrew puts gcc libraries.
;;     ;; 					    (car (file-expand-wildcards
;;     ;; 						  (expand-file-name "/usr/local/opt/gcc/lib/gcc/10")))))
;;     ;; 	;; Only set after LIBRARY_PATH can find gcc libraries.
;;     ;; 	(setq comp-deferred-compilation t))
;;     ;;   (message "Native comp is *not* available"))
;;     )

(use-package exec-path-from-shell
    :straight t
    :ensure t
    :if IS-MAC
    :config
    ;; (setq shell-default-term-shell "/bin/zsh")
    ;; (setq exec-path-from-shell-shell-name "/bin/zsh")
    ;; ;; (setq exec-path-from-shell-shell-name "/run/current-system/sw/bin/zsh")
    ;; (setq exec-path-from-shell-arguments '("-l"))
    ;; (when (file-executable-p "/usr/local/bin/fish")
    ;;   (setq exec-path-from-shell-shell-name "/usr/local/bin/fish"
    ;;         exec-path-from-shell-debug nil))

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

    ;; Load path from zsh login shell
    (when (or IS-LINUX IS-MAC)
      (defvar zsh-executable  "/bin/zsh")
      ;; (defvar zsh-executable  "/usr/bin/env zsh")
      (let* ((zshpath (shell-command-to-string
                       (concat zsh-executable " -lc 'printenv PATH'")))
             (pathlst (split-string zshpath ":")))
	(setq exec-path pathlst)
	(setq eshell-path-env zshpath)
	;; (princ zshpath)
	(setenv "PATH" zshpath))

      ;; use zsh as default shell
      (setenv "SHELL" "zsh"))

    (exec-path-from-shell-initialize)

    ;; (if (and (fboundp 'native-comp-available-p)
    ;;       (native-comp-available-p))
    ;;  (progn
    ;;    (message "Native comp is available")
    ;;    ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
    ;;    ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
    ;;    (add-to-list 'exec-path (concat invocation-directory "bin") t)
    ;;    (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
    ;;                   (when (getenv "LIBRARY_PATH")
    ;;                     ":")
    ;;                   ;; This is where Homebrew puts gcc libraries.
    ;;                   (car (file-expand-wildcards
    ;;                         (expand-file-name "/usr/local/lib/gcc/10")))))
    ;;    (setenv "DYLD_LIBRARY_PATH" (concat (getenv "DYLD_LIBRARY_PATH")
    ;;                        (when (getenv "DYLD_LIBRARY_PATH") ":")
    ;;                        ;; This is where Homebrew puts gcc libraries.
    ;;                        (car (file-expand-wildcards
    ;;                          (expand-file-name "/usr/local/lib/gcc/10")))))
    ;;    ;; Only set after LIBRARY_PATH can find gcc libraries.
    ;;    (setq comp-deferred-compilation t))
    ;;   (message "Native comp is *not* available"))
    )

;; ;; Load path from zsh login shell
;; (when (or IS-LINUX IS-MAC)
;;   (defvar zsh-executable  "/run/current-system/sw/bin/zsh")
;;   ;; (defvar zsh-executable  "/usr/bin/env zsh")
;;   (let* ((zshpath (shell-command-to-string
;;                    (concat zsh-executable " -lc 'printenv PATH'")))
;;          (pathlst (split-string zshpath ":")))
;;     (setq exec-path pathlst)
;;     (setq eshell-path-env zshpath)
;;     (princ zshpath)
;;     (setenv "PATH" zshpath))

;;   ;; use zsh as default shell
;;   (setenv "SHELL" "zsh"))


(provide 'init-env)

;;; init-env.el ends here
