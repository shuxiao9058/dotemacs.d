;;; core/core-straight.el -*- lexical-binding: t; -*-

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `straight.el'
(setq straight-base-dir poly-local-dir
      straight-repository-branch "master"
      ;; straight-use-package-by-default t
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
      straight-fix-org t
      straight-enable-use-package-integration t
      straight-disable-native-compilation nil

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
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  )


(straight-override-recipe
 '(org :type git :host github :repo "emacsmirror/org" :no-build t))

;; after straight's install procedure you will need to add straight-x.el and load the required commands.
(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")

;; ;; pinned some package version
;; (let ((straight-current-profile 'pinned))
;;   (straight-use-package 'org-plus-contrib)
;;   (straight-use-package 'org)

;;   ;; Pin org-mode version.
;;   (add-to-list 'straight-x-pinned-packages
;;                ;; '("org" . "924308a150ab82014b69c46c04d1ab71e874a2e6")
;;                )
;;   )

(provide 'core-straight)
;;; core/core-straight.el ends here
