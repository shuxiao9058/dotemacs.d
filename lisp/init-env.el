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


(use-package exec-path-from-shell
  :straight t
  :ensure t
  :if IS-MAC
  :config
  (exec-path-from-shell-initialize)

  ;; ;; gccemacs setup
  ;; (when (and (fboundp 'native-comp-available-p)
  ;;            (native-comp-available-p))
  ;;   ;; (add-to-list 'exec-path (expand-file-name "/usr/local/opt/gccemacs/bin"))

  ;;   (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
  ;;                                  (when (getenv "LIBRARY_PATH")
  ;;                                    ":")
  ;;                                  (car (file-expand-wildcards
  ;;                                        (expand-file-name "/usr/local/opt/libgccjit/lib/gcc/10")))))
  ;;   ;; Only set after LIBRARY_PATH can find gcc libraries.
  ;;   (setq comp-deferred-compilation t))

  ;; (if (and (fboundp 'native-comp-available-p)
  ;;          (native-comp-available-p))
  ;;     (progn
  ;;       (message "Native comp is available")
  ;;       ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
  ;;       ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
  ;;       (add-to-list 'exec-path (concat invocation-directory "bin") t)

  ;;       ; (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
  ;;       ;                                (when (getenv "LIBRARY_PATH")
  ;;       ;                                  ":")
  ;;       ;                                ;; This is where Homebrew puts gcc libraries.
  ;;       ;                                (car (file-expand-wildcards
  ;;       ;                                      (expand-file-name "~/homebrew/opt/gcc/lib/gcc/*")))))
  ;;       ;; Only set after LIBRARY_PATH can find gcc libraries.
  ;;       (setq comp-deferred-compilation t))
  ;;   (message "Native comp is *not* available"))
  )

(provide 'init-env)

;;; init-env.el ends here
