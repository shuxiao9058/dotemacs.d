;;; lisp/init-misc.el -*- lexical-binding: t; -*-

;; (defun my-nov-font-setup ()
;;   (face-remap-add-relative 'variable-pitch :family "Liberation Serif"))


;; This page lists all the customizations: https://depp.brause.cc/nov.el/
;; (use-package nov
;;     ;; :straight t
;;     :after org
;;     :straight (:no-native-compile t :type git :host nil :repo "https://depp.brause.cc/nov.el.git")
;;     :mode (("\\.epub\\'" . nov-mode))
;;     :config
;;     (progn
;;       ;; Change the font face
;;       (add-hook 'nov-mode-hook
;; 		(lambda ()
;;                   (face-remap-add-relative
;;                    'variable-pitch
;;                    :family "opendyslexic"
;;                    :height 2.3)))

;;       (setq nov-text-width 120)

;;       ;; (evil-set-initial-state 'nov-mode 'normal)

;;       ;; (general-define-key
;;       ;;  :keymaps 'nov-mode-map
;;       ;;  :states 'normal
;;       ;;  "g" 'nov-render-document
;;       ;;  "n" 'nov-next-document
;;       ;;  "(point)" 'nov-previous-document
;;       ;;  "t" 'nov-goto-toc
;;       ;;  "l" 'nov-history-back
;;       ;;  "r" 'nov-history-forward
;;       ;;  "RET" 'nov-browse-url
;;       ;;  "c" 'nov-copy-url
;;       ;;  ;;"<follow-link>" 'mouse-face
;;       ;;  ;;"<mouse-2>" 'nov-browse-url
;;       ;;  "TAB" 'shr-next-link
;;       ;;  "M-TAB" 'shr-previous-link
;;       ;;  "<backtab>" 'shr-previous-link
;;       ;;  ;; Toggle normal emacs font rendering
;;       ;;  "f" (lambda ()
;;       ;;        (interactive)
;;       ;;        (setq nov-variable-pitch (not nov-variable-pitch))
;;       ;;        (nov-render-document)))
;;       )
;;     )

;; (use-package bookmark+
;;     :straight t)

;; ;;; nov.el for epub ebooks reading
;; (use-package nov
;;   :straight t
;;   ;; :straight (nov :type git
;;   ;;  :files ("*.el")
;;   ;;                :host nil :repo "https://depp.brause.cc/nov.el.git")
;;   :after org
;;   :ensure t
;;   ;; :commands nov-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;;   ;; :hook
;;   ;; (nov-mode . 'my-nov-font)
;;   )

(defun log-complete-time-elapsed()
  (interactive)
  (let ((time (current-time)))
    (company-complete-common)
    (message "%.03fms"
             (* 1000 (float-time (time-since time)))))
  )

(use-package conf-mode
    :straight nil
    :mode
    (("\\.ini\\’" . conf-mode)))

(provide 'init-misc)
;;; init-misc.el ends here
