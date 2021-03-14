;;; lisp/init-dired.el -*- lexical-binding: t; -*-

(use-package dired
  :straight nil
  :custom
  (dired-auto-revert-buffer t)  ; don't prompt to revert; just do it
  (dired-dwim-target t)  ; suggest a target for moving/copying intelligently
  (dired-hide-details-hide-symlink-targets nil)
  ;; Always copy/delete recursively
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)
  ;; Where to store image caches
  (image-dired-dir (concat poly-cache-dir "image-dired/"))
  (image-dired-db-file (concat image-dired-dir "db.el"))
  (image-dired-gallery-dir (concat image-dired-dir "gallery/"))
  (image-dired-temp-image-file (concat image-dired-dir "temp-image"))
  (image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  ;; Screens are larger nowadays, we can afford slightly larger thumbnails
  (image-dired-thumb-size 150)

  :config
  (let ((args (list "-aBhl" "--group-directories-first")))
    (when IS-BSD
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support --group-directories-first
        (setq args (delete "--group-directories-first" args))))
    (setq dired-listing-switches (string-join args " ")))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)
  ;; :general
  ;; ;; Restore these keybinds, since the blacklisted/overwritten gr/gR will
  ;; ;; undo them:
  ;; (nmap :keymaps 'dired-mode-map
  ;;       "gr" #'revert-buffer)
  ;; (:states '(nomal global)
  ;; 	     :keymaps 'dired-mode-map
  ;; 	     ;; Kill all dired buffers on q
  ;; 	     ;; "q" 'flyspell-correct-word-generic
  ;; 	     ;; To be consistent with ivy/helm+wgrep integration
  ;; 	     "C-c C-e" #'wdired-change-to-wdired-mode)
  )

(provide 'init-dired)
;;; init-dired.el ends here
