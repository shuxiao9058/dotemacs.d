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
  ;; Removing emacs state from non-normal list allow the use of SPC
 ;; (delete 'emacs general-non-normal-states)

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

      ;;<leader> j --- Buffer
  "b" '(:ignore t :wk "buffers")
  "bb" '(ivy-switch-buffer :wk "Switch Buffer")
  "bo" '(other-buffer :wk "Other Buffer")
  "bk" '(kill-buffer :wk "Kill Buffer")
  "bs" '(save-buffer :wk "Save Buffer")
  "bl" '(list-buffers :wk "List Buffers")
  "bx" '((lambda ()(interactive)(switch-to-buffer "*scratch*")) :wk "scratch buffer")

     ;;<leader> c --- code
     "c" '(:wk "Code")
     "cC" '(compile :wk "Compile")
     "cc" '(recompile :wk "Recompile")

     "e" '(:ignore t :which-key "emacs/init")
					;; "ef" '(amb:edit-init-file :which-key "edit init.el")
     "eR" '((lambda ()(interactive)(load-file user-init-file)) :which-key "reload init.el")

     ;;<leader> f --- file
     "f" '(:ignore t :which-key "Files")
     "ff" '(counsel-find-file :wk "Find file")
     "fr" '(counsel-recentf :wk "Recent files")
     "fL" '(counsel-locate :wk "File Locate")
     "fR" '(projectile-recentf :wk "Recent project files")
     "fs" '(save-buffer :wk "Save buffer")
     "fS" '(save-some-buffer :wk "Save some buffers")

     ;;; <leader> p --- project
     "p" '(:ignore t :which-key "project")
     "pp" '(counsel-projectile-switch-project :wk "Switch Project")
     "pf" '(counsel-projectile-find-file :wk "Find file in project")
     "pb" '(counsel-projectile-switch-to-buffer :wk "Switch buffer in project")
     "p*" '((lambda () (interactive) (counsel-git-grep (current-word))) :wk "Git grep current word")
     "p/" '(counsel-git-grep :wk "Git grep")

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
     "g" '(:ignore t :wk "Git")
     "g/" '(magit-dispatch :wk "Magit dispatch")
     "g'" '(forge-dispatch :wk "Forge dispatch")
     "gb" '(magit-branch-checkout :wk "Magit switch branch")
     "gg" '(magit-status :wk "Magit status")
     "gR" '(vc-revert :wk "Git revert file")
     "gD" '(magit-file-delete :wk "Magit file delete")
     "gB" '(magit-blame-addition :wk "Magit blame")
     "gC" '(magit-clone :wk "Magit clone")
     "gF" '(magit-fetch :wk "Magit fetch")
     "gL" '(magit-log :wk "Magit buffer log")
     "gS" '(magit-stage-file :wk "Git stage file")
     "gU" '(magit-unstage-file :wk "Git unstage file")

      ;;; <leader> gf --- git find
     "gf" '(:ignore t :wk "Find")
     "gff" '(magit-find-file :wk "Find file")
     "gfg" '(magit-find-git-config-file :wk "Find gitconfig file")
     "gfc" '(magit-show-commit :wk "Find commit")
     
     ;;; <leader> gl --- git list
     "gl" '(:ignore t :wk "List")
     "glr" '(magit-list-repositories :wk "List repositories")
     "gls" '(magit-list-submodules :wk "List submodules")

     ;;; <leader> gc --- git create
     "gc" '(:ignore t :wk "Create")
     "gcr" '(magit-init :wk "Initialize repo")
     "gcR" '(magit-clone :wk "Clone repo")
     "gcc" '(magit-commit-create :wk "Commit")
     "gcf" '(magit-commit-fixup :wk "Fixup")
     "gcb" '(magit-branch-and-checkout :wk "Branch")
     "gci" '(forge-create-issue :wk "Issue")
     "gcp" '(forge-create-pullreq :wk "Pull request")

      ;;; <leader> i --- insert
     "i" '(:ignore t :wk "Insert")
     "iy" '(counsel-yank-pop :wk "From clipboard")

  ;;<leader> n --- open
  "o" '(:wk "Open/Toggle")
  "ob" '(browse-url-of-file :wk "Browser")
  "of" '(make-frame :wk "New frame")
  "o-" '(dired-jump :wk "Dired")
  "oe" '(eshell :wk "Open eshell here")
  "ot" '(treemacs :wk "Treemacs")

   ;;<leader> w --- windows
    "w" '(:wk "Windows")
  "w'" '(windmove-right :wk "Move to Right Window")
  "w;" '(windmove-down :wk "Move to Bottom Window")
  "wl" '(windmove-up :wk "Move to Top Window")
  "wk" '(windmove-left :wk "Move to Left Window")
  "wo" '(other-window :wk "Move to Other Window")
  "wj" '(kill-buffer-and-window :wk "Kill current Window")
  "wJ" '(delete-other-windows :wk "Kill all other windows")
  "w+" '(evil-window-increase-height :wk "Increase window height")
  "w-" '(evil-window-decrease-height :wk "Decrease window height")
  "w>" '(evil-window-increase-width :wk "Increase window width")
  "w<" '(evil-window-decrease-width :wk "Decrease window width")
  "w=" '(balance-windows :wk "Balance Windows")
  "ws" '(evil-window-split :wk "Split Horizontally")
  "wv" '(evil-window-vsplit :wk "Split Vertically")

     ;;<leader> m --- multiple cursors
    "m" '(:wk "Multiple Cursors")
    ;;<leader> h --- help
    "h" '(:keymap help-map :wk "Help")
    ;;<leader> x --- C-x compatibility
    "x" '(:keymap ctl-x-map :wk "C-x")
    ;;<leader> @ --- LSP-mode Keymap
    "y" '(:keymap lsp-command-map :package lsp-mode :wk "LSP")
     )


;;; minibuffer keymap
(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    ivy-minibuffer-map
    ivy-switch-buffer-map)
  "A list of all the keymaps used for the minibuffer.")

(general-define-key :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-b"    #'backward-word
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'evil-delete-backward-word
  "C-z"    (lambda (ignore-errors (call-interactively #'undo)))
  ;; Scrolling lines
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

  ;; (general-define-key :keymaps :keymaps '(read-expression-map)
  ;;   "C-j" #'next-line-or-history-element
  ;;   "C-k" #'previous-line-or-history-element)

(evil-normalize-keymaps)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
