;;; lisp/init-keybindings.el -*- lexical-binding: t; -*-

;; ;; NOTE SPC u replaces C-u as the universal argument.

;; ;; Minibuffer
;; (define-key! evil-ex-completion-map
;;   "C-a" #'evil-beginning-of-line
;;   "C-b" #'evil-backward-char
;;   "C-s" (if (featurep! :completion ivy)
;;             #'counsel-minibuffer-history
;;           #'helm-minibuffer-history))

;; (define-key! :keymaps +default-minibuffer-maps
;;   [escape] #'abort-recursive-edit
;;   "C-a"    #'move-beginning-of-line
;;   "C-r"    #'evil-paste-from-register
;;   "C-u"    #'evil-delete-back-to-indentation
;;   "C-v"    #'yank
;;   "C-w"    #'doom/delete-backward-word
;;   "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
;;   ;; Scrolling lines
;;   "C-j"    #'next-line
;;   "C-k"    #'previous-line
;;   "C-S-j"  #'scroll-up-command
;;   "C-S-k"  #'scroll-down-command)

;; (define-key! read-expression-map
;;   "C-j" #'next-line-or-history-element
;;   "C-k" #'previous-line-or-history-element)

;;
;;; General + leader/localleader keys
(use-package general
  :straight t
  :config
  (progn ;; let's go crazy
    ;; *** leader key
    (general-define-key
     :states '(normal visual insert emacs)
     :prefix "SPC"
     :non-normal-prefix "M-m"
     "TAB" '(evil-prev-buffer :which-key "prev buffer")
     "`"   '(evil-next-buffer :which-key "next buffer")
     "SPC" '(counsel-M-x :which-key "M-x")
					;; "SPC" '(amb:find-file-dwim :which-key "find you a file")
     "." '((lambda ()(interactive)(dired ".")) :which-key "current directory")
     "U" '(undo-tree-visualize :which-key "undo-tree")

     "b" '(:ignore t :which-key "buffers")
					;; "bb" '(helm-buffers-list :which-key "buffers list")
     "bd" '(kill-buffer :which-key "kill buffer")
     "bs" '((lambda ()(interactive)(switch-to-buffer "*scratch*")) :which-key "scratch buffer")

     ;;<leader> c --- code
     "c" '(:wk "Code")
     "cC" '(compile :wk "Compile")
     "cc" '(recompile :wk "Recompile")

     "e" '(:ignore t :which-key "emacs/init")
					;; "ef" '(amb:edit-init-file :which-key "edit init.el")
     "eR" '((lambda ()(interactive)(load-file user-init-file)) :which-key "reload init.el")

     ;;<leader> f --- file
     "f" '(:ignore t :which-key "Files")
     "fr" '(recentf-open-files :wk "Recent files")
     "fR" '(projectile-recentf :wk "Recent project files")
     "fs" '(save-buffer :wk "Save buffer")
     "fS" '(save-some-buffer :wk "Save some buffers")

     ;;<leader> s --- search
     "s" '(:wk "Search")
     "sb" '(swiper :wk "Search Buffer")
     "sf" '(locate :wk "Locate file")
     "si" '(imenu :wk "Jump to symbol")
     "sL" '(ffap-menu :wk "Jump to link")
     "sj" '(evil-show-jumps :wk "Jump list")
     "sm" '(evil-show-marks :wk "Jump to mark")
     "ss" '(swiper-isearch :wk "Search Buffer")
     "sS" '(swiper-isearch-thing-at-point :wk "Search Buffer for thing at point")

       ;;; <leader> q --- quit/session
     "q" '(:ignore t :which-key "quit/session")
     "qf" '(delete-frame :wk "Delete frame")
     "qK" '(save-buffers-kill-emacs :wk "Kill Emacs (and daemon)")
     "qQ" '(kill-emacs :wk "Quit Emacs")
     "qq" '(save-buffers-kill-terminal :wk "Quit Emacs")

     ;;<leader> g --- versioning
     "g" '(:wk "Git/Versioning")
     "gR" '(vc-revert :wk "Git revert file")
     "g;" '(with-editor-finish :wk "With Editor Finish")
     "gl" '(with-editor-cancel :wk "With Editor Cancel")
     "g/" '(magit-dispatch :wk "Magit dispatch")
     "gg" '(magit-status :wk "Magit status")
     "gx" '(magit-file-delete :wk "Magit file delete")
     "gB" '(magit-blame-addition :wk "Magit blame")
     "gC" '(magit-clone :wk "Magit clone")
     "gF" '(magit-fetch :wk "Magit fetch")
     "gL" '(magit-log :wk "Magit buffer log")
     "gS" '(magit-stage-file :wk "Git stage file")
     "gU" '(magit-unstage-file :wk "Git unstage file")
     "gf" '(:wk "Find")
     "gvff" '(magit-find-file :wk "Find file")
     "gvfg" '(magit-find-git-config-file :wk "Find gitconfig file")
     "gvfc" '(magit-show-commit :wk "Find commit")
     "gl" '(:wk "List")
     "gvlr" '(magit-list-repositories :wk "List repositories")
     "gvls" '(magit-list-submodules :wk "List submodules")
     "gc" '(:wk "Create")
     "gcr" '(magit-init :wk "Initialize repo")
     "gcR" '(magit-clone :wk "Clone repo")
     "gcc" '(magit-commit-create :wk "Commit")
     "gcf" '(magit-commit-fixup :wk "Fixup")



     )
    )
  )

;;
;;; Packages
(use-package which-key
  :straight t
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

(use-package hydra
  :straight t
  :init
  (setq hydra-if-helpful t))

(provide 'init-keybinds)
;;; init-keybinds.el ends here
