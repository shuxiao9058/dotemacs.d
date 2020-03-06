;;; core-packages.el -*- lexical-binding: t; -*-

;; void-function lexical-let
(eval-when-compile (require 'cl))

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `straight.el'
(setq straight-base-dir poly-local-dir
      straight-repository-branch "develop"
      straight-use-package-by-default  t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
    :straight t)

;; used to provide spacemacs like transient states
(use-package hydra
  :straight t
  :init
  (setq hydra-if-helpful t))

(use-package which-key
  :straight t
  :defer .1
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config (which-key-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; replaces isearch
(use-package swiper
  :straight t
  :commands swiper
  :after ivy)

;; generic completion front-end
(use-package ivy
  :straight t
  :delight
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-count-format "(%d/%d ")
  (ivy-mode t))

;; provide completion functions that use ivy
(use-package counsel
  :straight t
  :after ivy
  :demand t
  :delight
  :config
  (counsel-mode t))

;; use projectile to interact with projects
(use-package projectile
  :straight t
  :commands projectile-global-mode
  :delight
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" poly-cache-dir)
        projectile-known-projects-file (concat poly-cache-dir "projectile-bookmarks.eld")
        projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :commands counsel-projectile-mode
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(provide 'core-packages)