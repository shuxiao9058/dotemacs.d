;;; core/core-keybinds.el -*- lexical-binding: t; -*-



;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).

(defvar poly-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar poly-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar poly-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar poly-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")

(defvar poly-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")


;;
;;; Keybind settings

(when IS-MAC
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar poly-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `poly/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun poly/escape ()
  "Run `poly-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'poly-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'poly/escape)


;;
;;; General + leader/localleader keys

(use-package general
  :straight t
  :init
  ;; Convenience aliases
  (defalias 'define-key! #'general-def)
  (defalias 'unmap! #'general-unbind))

;; HACK `map!' uses this instead of `define-leader-key!' because it consumes
;; 20-30% more startup time, so we reimplement it ourselves.
(defmacro poly--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (poly-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key poly-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t poly-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t poly-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (cons `(after! which-key ,@(nreverse wkforms))
           (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.

See `poly-leader-key' and `poly-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'poly-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `poly-localleader-key' and `poly-localleader-alt-key' to change the
localleader prefix."
  (if (featurep 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix poly-localleader-key
        :non-normal-prefix poly-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix poly-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Doom's startup time!
(define-prefix-command 'poly/leader 'poly-leader-map)
(define-key poly-leader-map [override-state] 'all)

;; Bind `poly-leader-key' and `poly-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'poly-after-init-modules-hook
  (defun poly-init-leader-keys-h ()
    "Bind `poly-leader-key' and `poly-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal poly-leader-alt-key "C-c")
                   (set-keymap-parent poly-leader-map mode-specific-map))
                  ((equal poly-leader-alt-key "C-x")
                   (set-keymap-parent poly-leader-map ctl-x-map)))
            (define-key map (kbd poly-leader-alt-key) 'poly/leader))
        (evil-define-key* '(normal visual motion) map (kbd poly-leader-key) 'poly/leader)
        (evil-define-key* '(emacs insert) map (kbd poly-leader-alt-key) 'poly/leader))
      (general-override-mode +1))))


;;
;;; Packages

(use-package which-key
  :straight t
  :defer 1
  :after pre-command-hook
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
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements poly-leader-key "<leader>")
  (which-key-add-key-based-replacements poly-localleader-key "<localleader>")

  (which-key-mode +1))


;;
;;; `map!' macro

(defvar poly-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun poly--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`poly-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l poly-evil-state-alist)) collect it
           else do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :prefix-map   'lisp-indent-function 'defun)

;; specials
(defvar poly--map-forms nil)
(defvar poly--map-fn nil)
(defvar poly--map-batch-forms nil)
(defvar poly--map-state '(:dummy t))
(defvar poly--map-parent-state nil)
(defvar poly--map-evil-p nil)
(after! evil (setq poly--map-evil-p t))

(defun poly--map-process (rest)
  (let ((poly--map-fn poly--map-fn)
        poly--map-state
        poly--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (poly--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (poly--map-commit)
                  (setq poly--map-fn 'poly--define-leader-key))
                 (:localleader
                  (poly--map-commit)
                  (setq poly--map-fn 'define-localleader-key!))
                 (:after
                  (poly--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (poly--map-set :keymaps `(quote ,(poly-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (poly-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (poly--map-nested (list (intern (poly-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (poly-enlist (pop rest))
                    (let ((keymap (intern (format "poly-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            poly--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (poly-enlist (pop rest))
                    (poly--map-set (if poly--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          poly--map-forms)))
                 (_
                  (condition-case _
                      (poly--map-def (pop rest) (pop rest)
                                     (poly--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((poly--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (poly--map-commit)
    (macroexp-progn (nreverse (delq nil poly--map-forms)))))

(defun poly--map-append-keys (prop)
  (let ((a (plist-get poly--map-parent-state prop))
        (b (plist-get poly--map-state prop)))
    (if (and a b)
        `(general--concat nil ,a ,b)
      (or a b))))

(defun poly--map-nested (wrapper rest)
  (poly--map-commit)
  (let ((poly--map-parent-state (poly--map-state)))
    (push (if wrapper
              (append wrapper (list (poly--map-process rest)))
            (poly--map-process rest))
          poly--map-forms)))

(defun poly--map-set (prop &optional value)
  (unless (equal (plist-get poly--map-state prop) value)
    (poly--map-commit))
  (setq poly--map-state (plist-put poly--map-state prop value)))

(defun poly--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (poly-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state poly--map-batch-forms)))
  t)

(defun poly--map-commit ()
  (when poly--map-batch-forms
    (cl-loop with attrs = (poly--map-state)
             for (state . defs) in poly--map-batch-forms
             if (or poly--map-evil-p (not state))
             collect `(,(or poly--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) poly--map-forms))
    (setq poly--map-batch-forms nil)))

(defun poly--map-state ()
  (let ((plist
         (append (list :prefix (poly--map-append-keys :prefix)
                       :infix  (poly--map-append-keys :infix)
                       :keymaps
                       (append (plist-get poly--map-parent-state :keymaps)
                               (plist-get poly--map-state :keymaps)))
                 poly--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

Properties
  :leader [...]                   an alias for (:prefix poly-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds."
  (poly--map-process rest))

(provide 'core-keybinds)

;;; core-keybinds.el ends here