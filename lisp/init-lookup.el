;;; lisp/init-lookup.el -*- lexical-binding: t; -*-

(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")



(defvar +lookup-references-functions
  '(+lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions ()
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-dictionary-prefer-offline (featurep! +offline)
  "If non-nil, look up dictionaries online.

Setting this to nil will force it to use offline backends, which may be less
than perfect, but available without an internet connection.

Used by `+lookup/dictionary-definition' and `+lookup/synonyms'.

For `+lookup/dictionary-definition', this is ignored on Mac, where Emacs users
Dictionary.app behind the scenes to get definitions.")


;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)


(eval-after-load 'xref
    `(progn
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
 ;;; (defadvice! +lookup--projectile-find-tag-a (orig-fn)
 ;;;    :around #'projectile-find-tag
 ;;;    (let ((xref-backend-functions '(etags--xref-backend t)))
 ;;;     (funcall orig-fn)))

  ;; ;; Use `better-jumper' instead of xref's marker stack
  ;; (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)

  (use-package ivy-xref
    :straight t
    :config
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    ;; (set-popup-rule! "^\\*xref\\*$" :ignore t))
    )

;;
;;; dumb-jump

(use-package dumb-jump
  :straight t
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector
        'ivy)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))



; ;;
; ;;; Dash docset integration

; (use-package! dash-docs
;     :straight t
;   :defer t
;   :init
;   (add-hook '+lookup-documentation-functions #'+lookup-dash-docsets-backend-fn)
;   :config
;   (setq dash-docs-enable-debugging doom-debug-mode
;         dash-docs-docsets-path (concat poly-etc-dir "docsets/")
;         dash-docs-min-length 2
;         dash-docs-browser-func #'eww)

;   ;; Before `gnutls' is loaded, `gnutls-algorithm-priority' is treated as a
;   ;; lexical variable, which breaks `+lookup*fix-gnutls-error'
;   (defvar gnutls-algorithm-priority)
; ;   (defadvice! +lookup--fix-gnutls-error-a (orig-fn url)
; ;     "Fixes integer-or-marker-p errors emitted from Emacs' url library,
; ; particularly, the `url-retrieve-synchronously' call in
; ; `dash-docs-read-json-from-url'. This is part of a systemic issue with Emacs 26's
; ; networking library (fixed in Emacs 27+, apparently).

; ; See https://github.com/magit/ghub/issues/81"
; ;     :around #'dash-docs-read-json-from-url
; ;     (let ((gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
; ;       (funcall orig-fn url)))

; )

; (use-package counsel-dash
;    :straight t)

; (use-package zeal-at-point
;     :straight t
;     :when (and IS-LINUX (display-graphic-p))
;     :defer t)

(provide 'init-lookup)
;;; init-lookup.el ends here
