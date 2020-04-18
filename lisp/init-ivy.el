;;; lisp/init-ivy.el -*- lexical-binding: t; -*-

;; generic completion front-end
(use-package ivy
  :straight t
  ;; :after general
  :commands (ivy-switch-buffer ivy-occur ivy-help ivy-mode)
  :init
  (progn
    (declare-function ivy-completing-read "ivy")
    (setq completing-read-function #'ivy-completing-read))
  :preface
  (progn
    (defun config-ivy--ignore-errors (f &rest args)
      (ignore-errors
        (apply f args)))
    (defun config-ivy-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun config-ivy-with-empty-ivy-extra-directories (f &rest args)
      (let ((ivy-extra-directories nil))
        (apply f args)))

    ;; Define a command to pop open eshell for the candidate.

    (defun config-ivy-eshell-action ()
      "Open eshell at the target."
      (interactive)
      (ivy-exit-with-action
       (lambda (candidate)
         (let ((default-directory
                 (cond
                  ((f-dir-p candidate) candidate)
                  ((f-file-p candidate) (f-dirname candidate))
                  (t
                   (user-error "No available eshell action for candidate")))))
           (eshell t)))))

    ;; Define a command for entering wgrep straight from ivy results.

    (defun config-ivy-occur-then-wgrep ()
      "Shortcut for calling `ivy-occur' then activating wgrep."
      (interactive)
      (noflet
       ;; HACK: Run the original exit callback, then assume the occur buffer is
       ;; being displayed and change to wgrep.
       ((ivy-exit-with-action
         (action)
         (funcall this-fn (lambda (&rest args)
                            (apply action args)
                            (ivy-wgrep-change-to-wgrep-mode)))))
       (ivy-occur)))
    )
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  ;; Do not show extra directories when finding files.
  (ivy-extra-directories '("."))
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-magic-slash-non-match-action nil)
  (ivy-height 20)
  :config
  (advice-add #'counsel-find-file :around #'config-ivy-with-empty-ivy-extra-directories)
  (advice-add 'ivy--queue-exhibit :around #'config-ivy--ignore-errors)

  ;; Increase the maximum number of candidates that will be sorted
  ;; using `flx'. The default is 200, which means `flx' is almost
  ;; never used. Setting it too high (e.g. 10000) causes lag. This
  ;; seems to be a good compromise (for example, @PythonNut uses it,
  ;; see [1]).
  ;;
  ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
  (setq ivy-flx-limit 2000)
  (ivy-mode)
  )


(use-package ivy-prescient
  :straight t
  :ensure t
  :after ivy
  :config (ivy-prescient-mode))

;; (use-package flyspell-correct-ivy
;;     :straight t
;;     :after ivy
;;     :commands (flyspell-correct-word-generic)
;;     :hook
;;     (text-mode . flyspell-mode)
;;     (prog-mode . flyspell-prog-mode)
;;     )

;; (use-package ivy-posframe
;;     :if IS-GUI
;;     :hook
;;     (ivy-mode . ivy-posframe-mode)
;;     :custom
;;     (ivy-posframe-size-function 'cpm/ivy-posframe-size)
;;     (ivy-posframe-parameters ;; `((min-height . 16)
;;      ;;   (internal-border-width . 10)
;;      ;;   (internal-border-width . 10)
;;      ;;   ;; (foreground-color . "#00afef")
;;      ;;   )
;;      '((left-fringe . 0)
;;        (right-fringe . 0)
;;        (internal-border-width . 12))
;;      )
;;     (ivy-posframe-display-functions-alist
;;      '((swiper          . ivy-posframe-display-at-frame-top-center)
;;        (swiper-isearch  . ivy-posframe-display-at-frame-top-center)
;;        (complete-symbol . ivy-posframe-display-at-point)
;;        (counsel-M-x     . ivy-posframe-display-at-frame-center)
;;        (t               . ivy-posframe-display-at-frame-center)))
;;     :custom-face
;;     (ivy-posframe-cursor ((t (:background "#268bd2"))))
;;     :config
;;     (set-face-attribute 'ivy-current-match nil :underline t)
;;     ;; (defun reset-posframe-size(frame)
;;     ;;   ;; TODO use curr-width-4, but frame can be the pos-frame itself
;;     ;;   (let ((curr-width (frame-width frame)))
;;     ;;     (setq ivy-posframe-width (min curr-width 90))
;;     ;;     (setq ivy-posframe-min-width (min curr-width 90))
;;     ;;     )
;;     ;;   )
;;     ;; (add-to-list 'window-size-change-functions 'reset-posframe-size)
;;     (ivy-posframe-mode))

;; (defun cpm/ivy-posframe-size ()
;;   (list
;;    :min-height ivy-height
;;    :min-width (round (* (frame-width) 0.72))))

(provide 'init-ivy)
;;; init-ivy.el ends here
