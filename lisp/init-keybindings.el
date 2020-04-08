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


;; (general-define-key :keymaps 'flyspell-mode-map
;; 		    :states '(normal visual)
;; 		    "zs" #'flyspell-correct-word-generic
;; 		    "z=" #'flyspell-buffer)

;;; help-map
(general-define-key :keymaps 'help-map
		    ;; new keybinds
		    "'"    #'describe-char
		    "C-k"  #'describe-key-briefly
		    "C-l"  #'describe-language-environment
		    "C-m"  #'info-emacs-manual

		    ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
		    ;; <leader> h prefix. It's already on ? and F1 anyway.
		    "C-h"  nil
		    ;; replacement keybinds
		    ;; replaces `info-emacs-manual' b/c it's on C-m now
		    "r"    nil
		    ;; make `describe-bindings' available under the b prefix which it previously
		    ;; occupied. Add more binding related commands under that prefix as well
		    "b"    nil
		    "bb"   #'describe-bindings
		    "bi"   #'which-key-show-minor-mode-keymap
		    "bm"   #'which-key-show-major-mode
		    "bt"   #'which-key-show-top-level
		    "bf"   #'which-key-show-full-keymap
		    "bk"   #'which-key-show-keymap
		    ;; replaces `apropos-documentation' b/c `apropos' covers this
		    "d"    nil
		    ;; replaces `apropos-command'
		    "a"    #'apropos
		    "A"    #'apropos-documentation
		    ;; replaces `describe-copying' b/c not useful
		    "C-c"  #'describe-coding-system
		    ;; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
		    "F"    #'describe-face
		    ;; replaces `view-hello-file' b/c annoying
		    "h"    nil
		    ;; replaces `describe-package' b/c redundant w/ `doom/help-packages'
		    "P"    #'find-library
		    )

;; M-x
(general-define-key "M-x" #'counsel-M-x)

;; *** leader key
(leader-def
    "TAB" '(evil-prev-buffer :which-key "prev buffer")
  "`"   '(evil-next-buffer :which-key "next buffer")
  "SPC" '(projectile-find-file :wk "Find file in project")
  "RET" '(bookmark-jump :wk "Jump to bookmark")

  "." '(find-file :wk "Find file")
  "," '(persp-switch-to-buffer :wk "Switch workspace buffer")
  "<" '(switch-to-buffer :wk "Switch buffer")
  "`" '(evil-switch-to-windows-last-buffer  :wk "Switch to last buffer")
  "'" '(ivy-resume :wk "Resume last search")
  ;; "U" '(undo-tree-visualize :which-key "undo-tree")

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
  "cc" '(compile :wk "Compile")
  "cC" '(recompile :wk "Recompile")
  ;; "cd" '(+lookup/definition :wk "Jump to definition")
  ;; "cD" '(+lookup/references :wk "Jump to references")
  "cf" '(+format/region-or-buffer :wk "Format buffer/region")
  "cF" '(+format/lsp-format-region-or-buffer :wk "LSP Format buffer/region")
  "ci" '(lsp-organize-imports :wk "LSP Organize imports")

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
  "sd" '(color-rg-search-input :wk "Search current directory")
  "sf" '(locate :wk "Locate file")
  "si" '(imenu :wk "Jump to symbol")
  "sL" '(ffap-menu :wk "Jump to link")
  "sj" '(evil-show-jumps :wk "Jump list")
  "sp" '(color-rg-search-input-in-project :wk "Search project")
  ;; "sp" '(+utils:search-project :wk "Search project")
  "sm" '(evil-show-marks :wk "Jump to mark")
  "ss" '(swiper-isearch :wk "Search Buffer")
  "sS" '(swiper-isearch-thing-at-point :wk "Search Buffer for thing at point")

                    ;;; <leader> q --- quit/session
  "q" '(:ignore t :which-key "quit/session")
  "qf" '(delete-frame :wk "Delete frame")
  "qK" '(save-buffers-kill-emacs :wk "Kill Emacs (and daemon)")
  "qq" '(save-buffers-kill-terminal :wk "Quit Emacs")
  "qQ" '(evil-quit-all-with-error-code :wk "Quit Emacs without saving")
  "qs" '(poly/quicksave-session :wk "Quick save current session")
  "ql" '(poly/quickload-session :wk "Restore last session")
  "qS" '(poly/save-session :wk "Save session to file")
  "qL" '(poly/load-session :wk "Restore session from file")
  "qr" '(poly/restart-and-restore :wk "Restart & restore Emacs")
  "qR" '(restart-emacs :wk "Restart Emacs")

  ;;<leader> g --- versioning
  "g" '(:ignore t :wk "Git")
  "g/" '(magit-dispatch :wk "Magit dispatch")
  "g'" '(forge-dispatch :wk "Forge dispatch")
  "gb" '(magit-branch-checkout :wk "Magit switch branch")
  "gg" '(magit-status :wk "Magit status")
  ;; "gg" '((lambda () (interactive)
  ;; 	     (when (and buffer-file-name (buffer-modified-p))
  ;; 	       (save-buffer))
  ;; 	     ;; (save-buffer-if-dirty)
  ;; 	     (magit-status-and-focus-unstaged)
  ;; 	     ) :wk "Magit status")
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
  "oT" '(telega :wk "Telega")

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
  ;; replaces `apropos-command'
					; "a"    #'apropos
					; "A"    #'apropos-documentation

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


(with-eval-after-load 'general
  (general-define-key
   "TAB" 'company-indent-or-complete-common))

;; Close transient with esc/q
(general-define-key :keymaps 'transient-map
		    [escape] #'transient-quit-one
		    "q" #'transient-quit-one
		    )

;; start of company
(eval-after-load 'comint
  `(progn
     (general-define-key :keymaps 'comint-mode-map
			 "TAB" #'company-complete
			 [tab] #'company-complete)
     )
  )
;; ends of company

;; sart of ivy
(eval-after-load 'ivy
  `(progn
     (general-define-key :keymaps 'ivy-minibuffer-map
			 "C-SPC" #'ivy-call-and-recenter  ; preview file
			 "C-l"   #'ivy-alt-done
			 "C-v"   #'yank
			 )
     ))

(eval-after-load 'counsel
  `(progn
     (general-define-key :keymaps 'counsel-ag-map
			 "C-SPC"    #'ivy-call-and-recenter ; preview
			 "C-l"      #'ivy-done
          ;;; [C-return] #'+ivy/git-grep-other-window-action
			 )
     )
  )
;; end of ivy

(general-define-key :keymaps 'read-expression-map
		    "C-j" #'next-line-or-history-element
		    "C-k" #'previous-line-or-history-element)

(provide 'init-keybindings)
;;; init-keybindings.el ends here