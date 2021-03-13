;;; lisp/init-icomplete.el -*- lexical-binding: t; -*-

;; This is a completion style, it's a back-end for completion and is used by a
;; front-end that provides a completion UI.
;; TODO: check intergration with company:
;; https://github.com/oantolin/orderless#company
(use-package orderless
  :straight t
  :ensure t
  :after icomplete
  :custom
  (orderless-component-separator " +")
  (orderless-matching-styles
   '(;; The characters of the component should appear in that order in the
     ;; candidate, but not necessarily consecutively. This maps 'abc' to
     ;; 'a.*b.*c'.
     orderless-flex
     ;; orderless-initialism = each character of the component should
     ;; appear as the beginning of a word in the candidate, in order. This
     ;; maps 'abc' to '\<a.*\<b.*\c'.
     ;; orderless-strict-initialism = like initialism but only allow
     ;; non-letters in between the matched words. 'fb' would match
     ;; 'foo-bar' but not 'foo-qux-bar'.
     ;; orderless-strict-leading-initialism = like strict-initialism but
     ;; require the first initial to match the candidate’s first word. 'bb'
     ;; would match 'bar-baz' but not 'foo-bar-baz'.
     ;; orderless-strict-full-initialism = like strict-initialism but
     ;; require the first initial to match the candidate’s first word. 'bb'
     ;; would match 'bar-baz' but not 'foo-bar-baz'.
     orderless-strict-leading-initialism
     ;; The component is treated as a regexp that must match somewhere in
     ;; the candidate.
     orderless-regexp
     ;; The component is split at word endings and each piece must match at
     ;; a word boundary in the candidate, occurring in that order.
     orderless-prefixes
     ;; The component is treated as a literal string that must occur in the
     ;; candidate.
     orderless-literal))
  )

;; ;; '=' at the end of a component will make this component match as a literal.
;; (defun my/orderless-literal-dispatcher (pattern _index _total)
;;   (when (string-suffix-p "=" pattern)
;;     `(orderless-literal . ,(substring pattern 0 -1))))

;; ;; ',' at the end of a component will make this component match as a strict
;; ;; leading initialism.
;; (defun my/orderless-initialism-dispatcher (pattern _index _total)
;;   (when (string-suffix-p "," pattern)
;;     `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

;; (setq orderless-style-dispatchers
;;       '(my/orderless-literal-dispatcher
;;         my/orderless-initialism-dispatcher))

(use-package minibuffer
  :straight nil
  :custom
  ;; minibuffer prompt 只读，且不允许光标进入其中
  (minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  ;; Shorten "(default ...)" to "[...]".
  ;; (minibuffer-eldef-shorten-default t)
  :config
  ;; Make "unimportant" part of filename in minibuffer visually less noticeable.
  (file-name-shadow-mode t)
  ;; Show recursion depth in minibuffer (related to
  ;; enable-recursive-minibuffers).
  (minibuffer-depth-indicate-mode t)
  ;; Show default value in minibuffer prompt only when it's applicable.
  (minibuffer-electric-default-mode t)

  (defun my/focus-minibuffer ()
    "Focus the active minibuffer.
Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
    (interactive)
    (let ((minibuffer (active-minibuffer-window)))
      (when minibuffer
        (select-window minibuffer))))

  (defun my/focus-minibuffer-or-completions ()
    "Focus the active minibuffer or the \\*Completions\\*.
If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.
The continuous switch is essentially the same as running
`my/focus-minibuffer' and `switch-to-completions' in
succession."
    (interactive)
    (let* ((minibuffer (active-minibuffer-window))
           (completions (get-buffer-window "*Completions*")))
      (cond ((and minibuffer
                  (not (minibufferp)))
             (select-window minibuffer nil))
            ((and completions
                  (not (eq (selected-window)
                           completions)))
             (select-window completions nil)))))

  (defun my/describe-symbol-at-point (&optional arg)
    "Get help (documentation) for the symbol at point.
With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
    (interactive "P")
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol)))
    (when arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

  (defun my/completions-kill-save-symbol ()
    "Add symbol-at-point to the kill ring.
Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
    (interactive)
    (kill-new (thing-at-point 'symbol)))
  ;; :bind
  ;; (("s-f" . find-file)
  ;;  ("s-F" . find-file-other-window)
  ;;  ("s-d" . dired)
  ;;  ("s-D" . dired-other-window)
  ;;  ("s-b" . switch-to-buffer)
  ;;  ("s-B" . switch-to-buffer-other-window)
  ;;  ("s-h" . my/describe-symbol-at-point)
  ;;  ("s-H" . (lambda ()
  ;;             (interactive)
  ;;             (my/describe-symbol-at-point '(4))))
  ;;  ("s-v" . my/focus-minibuffer-or-completions)
  ;;  (:map minibuffer-local-completion-map
  ;;        ;; Expand the current candidate and then exit the session, if
  ;;        ;; possible.
  ;;        ("<return>" . minibuffer-force-complete-and-exit)
  ;;        ;; Insert the minibuffer content as it is exactly (without
  ;;        ;; expansion) and exit the session.
  ;;        ("C-j" . exit-minibuffer))
  ;; (:map completion-list-mode-map
  ;;       ("n" . next-line)
  ;;       ("p" . previous-line)
  ;;       ("f" . next-completion)
  ;;       ("b" . previous-completion)
  ;;       ("h" . my/describe-symbol-at-point)
  ;;       ("w" . my/completions-kill-save-symbol)
  ;;       ("M-v" . my/focus-minibuffer))
  ;; )
  ;; (:map minibuffer-local-completion-map
  ;; 	;; Space should never complete. Use it for orderless group.
  ;; 	("SPC" . nil))
  )

(use-package icomplete-vertical
  :straight (icomplete-vertical :type git
  				:host github
  				:repo "oantolin/icomplete-vertical"
  				:branch "master")
  :ensure t
  :demand t
  :after minibuffer
  ;; :hook ((after-init . icomplete-mode)
  ;;        (after-init . icomplete-vertical-mode))
  :custom
  ;; (icomplete-vertical-prospects-height (/ (window-height) 6))
  (icomplete-vertical-prospects-height 15)
  ;; Ignore case when reading a buffer name.
  (read-buffer-completion-ignore-case t)
  ;; Ignore case when reading a file name.
  (read-file-name-completion-ignore-case t)
  ;; ;; Layout of *Completions* buffer.
  ;; (completions-format 'vertical)
  ;; Start something in the minibuffer, switch to another window, call
  ;; minibuffer again, run commands and then move back to the original
  ;; minibuffer.
  (enable-recursive-minibuffers t)
  ;; Accept short answers to questions.
  (read-answer-short t)
  ;; Resize minibuffer and echo area to fit the text inside.
  (resize-mini-windows t)

  ;; completion-styles try to match candidates using one style at a time moving
  ;; from the first to the last until something is matched. orderless replaces
  ;; all the built in completion styles apart from partial-completion.
  ;; partial-completion = allows to navigate to a filesystem path like ~/.l/s/fo
  ;; for ~/.local/share/fonts.
  (completion-styles '(orderless partial-completion substring flex))
  (completion-category-overrides '((file (styles basic substring))))
  ;; ;; (setq completion-category-defaults nil)
  ;; ;; ;; Cycling is used if there aren't more candidates than this number.
  ;; ;; (completion-cycle-threshold 3)
  (completion-flex-nospace nil)
  (completion-pcm-complete-word-inserts-delimiters t)
  ;; ;; Characters treated as word delimiters for completion.
  (completion-pcm-word-delimiters "-_./:| ")
  ;; disable display two buffer *Completions* and minibuffer
  (completion-show-help nil)
  (completion-auto-help nil)
  (completion-ignore-case t)
  (completion-cycle-threshold t)
  (icomplete-delay-completions-threshold 100)
  ;; (icomplete-max-delay-chars 0)
  (icomplete-compute-delay 0)
  (icomplete-show-matches-on-no-input t)
  ;; Hide common prefix from completion candidates.
  (icomplete-hide-common-prefix nil)
  ;; ;; Max number of lines to use in minibuffer.
  ;; (icomplete-prospects-height 2)
  ;; ;; Candidate separator.
  ;; ;; (icomplete-separator "\n")
  ;; ;; (icomplete-separator (propertize " ┆ " 'face 'shadow))
  (icomplete-with-completion-tables t)
  ;; ;; Do not use Icomplete in non-mini buffers (use company in these buffers).
  (icomplete-in-buffer nil)
  ;; (icomplete-tidy-shadowed-file-names t)
  ;; (icomplete-prospects-height 2)
  :config
  (when (require 'orderless nil t)
    (setq completion-styles (cons 'orderless completion-styles)) ;把orderless放到completion-styles 开头
    ;; 默认按空格开隔的每个关键字支持regexp/literal/initialism 3种算法
    (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism ))
    (defun without-if-$! (pattern _index _total)
      (when (or (string-prefix-p "$" pattern) ;如果以! 或$ 开头，则表示否定，即不包含此关键字
		(string-prefix-p "!" pattern))
	`(orderless-without-literal . ,(substring pattern 1))))
    (defun flex-if-comma (pattern _index _total) ;如果以逗号结尾，则以flex 算法匹配此组件
      (when (string-suffix-p "," pattern)
	`(orderless-flex . ,(substring pattern 0 -1))))
    (defun literal-if-= (pattern _index _total) ;如果以=结尾，则以literal  算法匹配此关键字
      (when (or (string-suffix-p "=" pattern)
		(string-suffix-p "-" pattern)
		(string-suffix-p ";" pattern))
	`(orderless-literal . ,(substring pattern 0 -1))))
    (setq orderless-style-dispatchers '(literal-if-= flex-if-comma without-if-$!)))

  (advice-add 'icomplete-vertical-minibuffer-teardown
              :after #'visual-line-mode)
  (icomplete-mode 1)
  (icomplete-vertical-mode 1)
  (defun my/icomplete-recentf ()
    "Open `recent-list' item in a new buffer.
  The user's $HOME directory is abbreviated as a tilde."
    (interactive)
    (icomplete-vertical-do ()
      (let ((files (mapcar 'abbreviate-file-name recentf-list)))
        (find-file
         (completing-read "Open recentf entry: " files nil t)))))

  (defun my/icomplete-yank-kill-ring ()
    "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.
Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
    (interactive)
    (let ((kills                    ;; do not sort items
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
          (:separator 'dotted-line :height (/ (window-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))
  :bind (
	 ([remap yank-pop] . my/icomplete-yank-kill-ring)
	 :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle)
	 ("C-n"  . icomplete-forward-completions)
	 ("C-j"  . icomplete-forward-completions)
	 ("M-n"  . icomplete-forward-completions)

	 ("<up>" . icomplete-backward-completions)
	 ("C-p"  . icomplete-backward-completions)
	 ("C-k"  . icomplete-backward-completions)
	 ("M-p"  . icomplete-backward-completions)
	 ;; "C-v"  #'icomplete-vertical-toggle
	 ;; "<tab>"  #'icomplete-forward-completions
	 ("<tab>"  . icomplete-force-complete)
	 ;; "M-TAB" #'icomplete-force-complete
	 ("<return>" . icomplete-force-complete-and-exit)
	 ;; "<tab>" #'icomplete-force-complete
	 ;; "C-j" #'icomplete-force-complete
	 ("<left>"  . icomplete-backward-completions)
	 ("<right>" . icomplete-forward-completions)
	 ("<backtab>" . icomplete-backward-completions)
	 ("C-l" . icomplete-fido-backward-updir)
	 ("DEL" . icomplete-fido-backward-updir)

	 ;; "RET" #'icomplete-fido-ret
	 ;; "C-m" #'icomplete-fido-ret
	 ("C-s" . icomplete-forward-completions)
	 ("C-r" . icomplete-backward-completions)
	 ("C-." . next-history-element)
	 ;; "C-l" #'icomplete-fido-backward-updir
					; ("C-e" . (lambda(&optional argv)(interactive)(if (eolp) (call-interactively #'icomplete-fido-exit) (end-of-line))))
	 ([remap next-line] . icomplete-forward-completions)
	 ;; [remap minibuffer-complete] #'icomplete-backward-completions
	 ;; ([remap minibuffer-complete-and-exit] . icomplete-ret)
	 )
  )

					; (use-package restricto
					;   :after minibuffer
					;   :straight (:host github :repo "oantolin/restricto")

					;   :bind (:map minibuffer-local-completion-map
					; 	      ("SPC"   . restricto-narrow)
					; 	      ("S-SPC" . restricto-widen))
					;   :config
					;   (restricto-mode))

;; swiper is a buffer search interface using ivy.
(use-package swiper
  :straight t
  :commands swiper
  ;; :after ivy
  )

(use-package ripgrep
  :straight t
  )

;; (use-package wgrep-ag
;;   :straight t
;;   )

;; rg: like ag, but faster (and rustier)
(use-package rg
  :straight t
  ;; :hook (rg-mode . wgrep-ag-setup)
  :custom
  ;; (rg-custom-type-aliases
  ;;  '(("clojure" . "*.clj *.cljs *.cljc *.cljx *.edn"))
  (rg-group-result t)
  (rg-show-columns t)
  (rg-ignore-case 'smart)
  (rg-show-header t)
  :config
  (rg-define-search bl/rg-regexp-project
    :query ask
    :format regexp
    :files current
    :dir project
    :confirm never)
  )

(provide 'init-icomplete)
;;; init-icomplete.el ends here
