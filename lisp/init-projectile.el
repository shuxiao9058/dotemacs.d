;;; lisp/init-projectile.el -*- lexical-binding: t; -*-

(use-package project
  :straight nil
  :custom
  (project-vc-ignores
   '("vendor/" "*.elc" "*.a"
     "tmp" "dist" "coverage"
     ".idea" ".vscode"
     ".ensime_cache" ".eunit"
     ".git" ".hg" ".fslckout"
     "_FOSSIL_" ".bzr" "_darcs"
     ".tox" ".svn"
     ".stack-work" ".ccls-cache" ".cache" ".clangd")
   '(".log" ".vs" "node_modules"))
  )

(use-package ag
  :straight t
  :ensure t
  :commands (ag ag-regexp ag-project)
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  )


(defcustom find-project-ignore-dir
  '("/usr")
  ""
  :type 'list)


(defun my/projectile-ignored-project-function(project-root)
  (member t (mapcar
             #'(lambda (dir)
                 (string-prefix-p dir project-root)
                 )
             find-project-ignore-dir
             )))


(defun projectile-selection-at-point ()
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun projectile-deadgrep (search-term)
  (interactive (list (deadgrep--read-search-term)))
  (let ((deadgrep-project-root-function #'projectile-project-root))
    (deadgrep search-term)))

(use-package projectile
  :straight t
  :commands projectile-global-mode
  :after rg
  :delight '(:eval (format " [prj: %s]" (projectile-project-name)))
  ;; :init
  ;; (when (executable-find "rg")
  ;;   (setq-default projectile-generic-command "rg --files --hidden"))
  ;; :delight
  :preface
  :custom
  (projectile-indexing-method 'hybrid)
  (projectile-completion-system 'default)
  (projectile-ignored-project-function #'my/projectile-ignored-project-function)
  (projectile-enable-caching t)
  (projectile-sort-order 'modification-time)
  ;; (projectile-search-in-file-rg  (lambda () (projectile-dired) (projectile-commander)))
  ;; (projectile-switch-project-action  (lambda () (projectile-dired) (projectile-commander)))
  ;; :preface
  ;; (defun projectile-rg ()
  ;;   "Run ripgrep in projectile."
  ;;   (interactive)
  ;;   (counsel-rg "" (projectile-project-root))
  ;;   )
  :config
  (defun poly/switch-project-action ()
    (interactive)
    (if (magit-git-dir)
	(magit-status)
      (projectile-find-file)))

  (defun my/projectile-dynamic-change-index-method()
    (when (projectile-project-p)
      (if (eq (projectile-project-vcs) 'none)
          (setq projectile-indexing-method 'native)
	(setq projectile-indexing-method 'hybrid))))

  (add-hook 'find-file-hook #'my/projectile-dynamic-change-index-method)
  (add-hook 'dired-mode-hook #'my/projectile-dynamic-change-index-method)

  (defun reload-dir-locals-for-project ()
    "For every buffer with the same `projectile-project-root' as the
current buffer's, reload dir-locals."
    (interactive)
    (dolist (buffer (projectile-project-buffer-names))
      (with-current-buffer buffer
	(reload-dir-locals-for-curent-buffer))))


  ;; `ibuffer-projectile'
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))))

  (my/projectile-ignored-project-function "/usr/bin")
  (my/projectile-ignored-project-function "vendor")

  ;; (setq projectile-switch-project-action #'poly/switch-project-action)


  (defun my/projectile-custom-switch-action()
    (my/projectile-dynamic-change-index-method)
    (projectile-find-file))

  (setq projectile-switch-project-action #'my/projectile-custom-switch-action)

  (setq projectile-cache-file (expand-file-name "projectile.cache" poly-cache-dir)
	projectile-known-projects-file (concat poly-cache-dir "projectile-bookmarks.eld"))
  (setq projectile-globally-ignored-directories '(".idea"
                                                  ".ensime_cache"
                                                  ".eunit"
                                                  ".git"
                                                  ".hg"
                                                  ".fslckout"
                                                  "_FOSSIL_"
                                                  ".bzr"
                                                  "_darcs"
                                                  "target"
                                                  ".tox"
                                                  ".settings"
                                                  ".svn"
                                                  ".github"
                                                  ".metals"
                                                  ".bloop"
                                                  ".ccls-cache"
                                                  ".stack-work")
        projectile-globally-ignored-file-suffixes '("*.pyc" "*.class" "*.project" "*.jar"))
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (dolist (extfs projectile-globally-ignored-file-suffixes)
              (setq rg-cmd (format "%s -g '!%s'" rg-cmd extfs)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))
  (projectile-global-mode)
  )

(defun poly/find-file()
  "my find file"
  (interactive)
  (if (and (bound-and-true-p projectile-mode) (not (eq (projectile-project-vcs) 'none)))
      (projectile-find-file)
    (call-interactively #'find-file)
    )
  :bind(
      :map projectile-command-map
      ("s s" . projectile-deadgrep)
    ))

;; (use-package counsel-projectile
;;   :straight t
;;   :commands counsel-projectile-mode
;;   :after (counsel projectile)
;;   :config
;;   (counsel-projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
