;;; lisp/init-corfu.el -*- lexical-binding: t; -*-

(use-package corfu
  :straight (
	     :files (:defaults "extensions/*")
	     :includes (corfu-history))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
	      ("C-n" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
	      ("C-p" . corfu-previous)
              ("<backtab>" . corfu-previous)
	      ;; ("SPC" . corfu-move-to-minibuffer)
              ;; ("<space>" . corfu-move-to-minibuffer)
	      ([remap completion-at-point] . corfu-next))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match t)
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; :hook ((prog-mode . corfu-mode)
  ;; 	 (org-mode . corfu-mode))
  :init
  (global-corfu-mode))


(unless (display-graphic-p)
  (use-package popon
    :straight (popon
	       :type git
	       :repo "https://codeberg.org/akib/emacs-popon.git"))
  (use-package corfu-popup
    :straight (corfu-popup
	       :type git
	       :repo "https://codeberg.org/akib/emacs-corfu-popup.git")
    :init
    (corfu-popup-mode +1)))

;; Icon support
(use-package kind-icon
  :ensure t
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icon t)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir (expand-file-name "svg-lib" poly-cache-dir))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-mapping
        '((array "a" :icon "code-brackets" :face font-lock-type-face)
          (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
          (class "c" :icon "video-input-component" :face font-lock-type-face) ;
          (color "#" :icon "palette" :face success) ;
          (constant "co" :icon "square-circle" :face font-lock-constant-face) ;
          (constructor "cn" :icon "cube-outline" :face font-lock-function-name-face) ;
          (enum-member "em" :icon "format-align-right" :face font-lock-builtin-face) ;
          (enum "e" :icon "server" :face font-lock-builtin-face) ;
          (event "ev" :icon "zip-box-outline" :face font-lock-warning-face) ;
          (field "fd" :icon "tag" :face font-lock-variable-name-face) ;
          (file "f" :icon "file-document-outline" :face font-lock-string-face) ;
          (folder "d" :icon "folder" :face font-lock-doc-face) ;
          (interface "if" :icon "share-variant" :face font-lock-type-face) ;
          (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face) ;
          (macro "mc" :icon "lambda" :face font-lock-keyword-face)
          (method "m" :icon "cube-outline" :face font-lock-function-name-face) ;
          (function "f" :icon "cube-outline" :face font-lock-function-name-face) ;
          (module "{" :icon "view-module" :face font-lock-preprocessor-face) ;
          (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
          (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face) ;
          (param "pa" :icon "tag" :face default)
          (property "pr" :icon "wrench" :face font-lock-variable-name-face) ;
          (reference "rf" :icon "collections-bookmark" :face font-lock-variable-name-face) ;
          (snippet "S" :icon "format-align-center" :face font-lock-string-face) ;
          (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
          (struct "%" :icon "video-input-component" :face font-lock-variable-name-face) ;
          (text "tx" :icon "format-text" :face shadow)
          (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
          (unit "u" :icon "ruler-square" :face shadow)
          (value "v" :icon "format-align-right" :face font-lock-builtin-face) ;
          (variable "va" :icon "tag" :face font-lock-variable-name-face)
          (t "." :icon "file-find" :face shadow)))
  )

;; A few more useful configurations...
(use-package emacs
  :init
  (setq compilation-scroll-output 'first-error)
  (setq auto-revert-check-vc-info t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package corfu-history
  :after corfu
  :init (corfu-history-mode))

;; Completion At Point Extensions made for `corfu'
(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
  :hook ((prog-mode . my/set-basic-capf)
         (org-mode . my/set-basic-capf)
         ((lsp-completion-mode eglot-managed-mode) . my/set-lsp-capf))
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  ;; (setq cape-dict-file "/usr/share/dict/words")
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))
(define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)


(use-package corfu-doc
  :ensure t
  :straight t
  :config
  ;;hook
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  ;;bind
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))

;; Configure Tempel
(use-package tempel
  :straight t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package tabnine-capf
  :after cape
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

;; https://github.com/50ways2sayhard/.emacs.d/blob/1158200665431cc336c868ad1f9ecb43c249fc31/elisp/init-complete.el
(defun my/convert-super-capf (arg-capf)
  (list
   #'cape-file
   (cape-capf-buster
    (cape-super-capf
     arg-capf
     #'tabnine-completion-at-point
     ;; #'tempel-expand
     )
    )
   ;; #'cape-dabbrev
   ))

(defun my/set-basic-capf ()
  (setq completion-category-defaults nil)
  (setq-local completion-at-point-functions (my/convert-super-capf (car completion-at-point-functions))))

(defun my/set-lsp-capf ()
  (setq completion-category-defaults nil)
  (setq-local completion-at-point-functions (my/convert-super-capf (if poly-use-lsp-mode
                                                                       #'lsp-completion-at-point
								     #'eglot-completion-at-point))))

(provide 'init-corfu)
;;; init-corfu.el ends here
