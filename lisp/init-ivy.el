;;; lisp/init-ivy.el -*- lexical-binding: t; -*-

;; generic completion front-end
(use-package ivy
    :straight t
    :after general
    :commands (ivy-switch-buffer)
    :config
    (setq ivy-use-virtual-buffers t
          ivy-wrap t
          ivy-display-style 'fancy
          ivy-count-format "(%d/%d ")
    (ivy-mode t))


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

; (use-package ivy-posframe
;   :if IS-GUI
;     :hook
;   (ivy-mode . ivy-posframe-mode)
;   :custom
;   (ivy-posframe-size-function 'cpm/ivy-posframe-size)
;   (ivy-posframe-parameters ;; `((min-height . 16)
;    ;;   (internal-border-width . 10)
;    ;;   (internal-border-width . 10)
;    ;;   ;; (foreground-color . "#00afef")
;    ;;   )
;    '((left-fringe . 0)
;      (right-fringe . 0)
;      (internal-border-width . 12))
;    )
;   (ivy-posframe-display-functions-alist
;    '((swiper          . ivy-posframe-display-at-frame-top-center)
;      (swiper-isearch  . ivy-posframe-display-at-frame-top-center)
;      (complete-symbol . ivy-posframe-display-at-point)
;      (counsel-M-x     . ivy-posframe-display-at-frame-center)
;      (t               . ivy-posframe-display-at-frame-center)))
;   :custom-face
;   (ivy-posframe-cursor ((t (:background "#268bd2"))))
;   :config
;   (set-face-attribute 'ivy-current-match nil :underline t)
;   ;; (defun reset-posframe-size(frame)
;   ;;   ;; TODO use curr-width-4, but frame can be the pos-frame itself
;   ;;   (let ((curr-width (frame-width frame)))
;   ;;     (setq ivy-posframe-width (min curr-width 90))
;   ;;     (setq ivy-posframe-min-width (min curr-width 90))
;   ;;     )
;   ;;   )
;   ;; (add-to-list 'window-size-change-functions 'reset-posframe-size)
;   (ivy-posframe-mode))

; (defun cpm/ivy-posframe-size ()
;   (list
;    :min-height ivy-height
;    :min-width (round (* (frame-width) 0.72))))

(provide 'init-ivy)
;;; init-ivy.el ends here
