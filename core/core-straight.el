;;; core/core-straight.el -*- lexical-binding: t; -*-

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `straight.el'
(setq straight-base-dir poly-local-dir
      straight-repository-branch "develop"
      straight-use-package-by-default t
      straight-cache-autoloads nil ; we already do this, and better.
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom refresh' instant (once everything set up), which is much
      ;; nicer UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration t
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb with half Doom's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This
      ;; imposes an issue with packages that require their git history for
      ;; certain things to work (like magit and org), but we can deal with that
      ;; when we cross that bridge.
      straight-vc-git-default-clone-depth 1
      ;; Prefix declarations are unneeded bulk added to our autoloads file. Best
      ;; we just don't have to deal with them at all.
      autoload-compute-prefixes nil
      straight-fix-org t
      straight-enable-use-package-integration t
      ;; straight-check-for-modifications '(find-when-checking check-on-save)
      )

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" poly-local-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-demand t)

(defvar poly-autoload-file (concat poly-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")
  
(defun poly-load-autoload ()
  "Load `poly-autoload-file'."
  (if (file-exists-p poly-autoload-file)
    (load poly-autoload-file)
  (progn
    (poly/generate-autoload-file)
    (load poly-autoload-file))))

(defun poly/generate-autoload-file ()
  "Extract autload file from each star to `poly-autoload-file'."
  (interactive)  
  (let ((autoload-file-list
       (file-expand-wildcards
        (expand-file-name "*.el" poly-autoload-dir))))
    (dolist (file (reverse autoload-file-list))
      (message
       (cond ((update-file-autoloads file t poly-autoload-file)
          "Nothing in %s")
         (t "Scanned %s"))
       (file-relative-name file user-emacs-directory)))
      ))

(provide 'core-straight)
;;; core/core-straight.el ends here
