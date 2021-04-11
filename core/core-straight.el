;;; core/core-straight.el -*- lexical-binding: t; -*-

(with-eval-after-load 'use-package-core
  (when (and (boundp 'use-package-keywords)
             (listp use-package-keywords))
    (princ "set use-package-keywords")
    (setq use-package-keywords (remq :pdump use-package-keywords))
    (add-to-list 'use-package-keywords :pdump t)))

(defun use-package-normalize/:pdump (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((and (listp arg) (keywordp (car arg))) arg)
       ((symbolp arg) (symbol-name arg))
       (t
        (use-package-error
         ":pdump wants a bool value"))))))

(defun use-package-handler/:pdump (name _keyword arg rest state)
  (when state
    (poly-pdump-packages `,name)))

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `straight.el'
(setq straight-base-dir poly-local-dir
      straight-repository-branch "develop"
      straight-use-package-by-default t
      straight-cache-autoloads t ;; we already do this, and better.
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
      straight-fix-flycheck t
      straight-enable-use-package-integration t
      straight-disable-native-compile nil

      ;; Tell straight.el about the profiles we are going to be using.
      straight-profiles
      '((nil . "default.el")
	;; Packages which are pinned to a specific commit.
	(pinned . "pinned.el"))
      ;; ;; Set package.el variables just in case to avoid polluting
      ;; ;; the root directory.
      ;; package-enable-at-startup nil
      ;; straight-check-for-modifications '(find-when-checking check-on-save)
      use-package-always-demand t
      )

(eval-when-compile
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
    ;; catch emacs updates that have native compiled leftovers
    ;; Credits: https://github.com/raxod502/straight.el/643/issues
    (unless (catch 'emacs-version-changed
	      (load bootstrap-file nil 'nomessage))
      ;; remove historian-save-file
      ;; try fix (void-variable _args)
      (when (boundp 'historian-save-file)
	(delete-directory (file-truename (expand-file-name (car historian-save-file))) t)
	)
      (when (boundp 'comp-eln-load-path)
	;; remove leftovers
	(when (y-or-n-p (format "Delete '%s'? " (car comp-eln-load-path)))
	  (delete-directory (file-truename (expand-file-name (car comp-eln-load-path))) t))
	;; try loading again
	(load bootstrap-file nil 'nomessage))))
  (straight-use-package 'use-package))

(straight-override-recipe
 '(semi :host github :repo "wanderlust/semi" :branch "semi-1_14-wl"))

(straight-override-recipe
 '(flim :host github :repo "wanderlust/flim" :branch "flim-1_14-wl"
	:files ("*.texi" "*.el" (:exclude "md5-dl.el"
					  "md5-el.el" "mel-b-dl.el" "sha1-dl.el"
					  "smtpmail.el") "flim-pkg.el")))
(straight-override-recipe
 '(apel :host github :repo "wanderlust/apel" :branch "apel-wl"))
(straight-override-recipe
 '(wanderlust :host github :repo "wanderlust/wanderlust" :branch "master"))

(straight-override-recipe
 '(org :type git :host github :repo "emacsmirror/org" :no-build t))

;; after straight's install procedure you will need to add straight-x.el and load the required commands.
(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")

(provide 'core-straight)
;;; core/core-straight.el ends here
