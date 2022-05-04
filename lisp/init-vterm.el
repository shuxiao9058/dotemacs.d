;;; lisp/init-vterm.el -*- lexical-binding: t; -*-

(use-package vterm
  :straight (vterm :type git :flavor melpa
                   :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
                   :host github :repo "akermu/emacs-libvterm"
 		   :no-native-compile t
                   )
  :demand
  :commands (vterm ds/vterm)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-ignore-blink-cursor nil)
  :init
  (setq vterm-shell "zsh")
  (defun ds/vterm-send-C-x ()
    (interactive)
    (vterm-send "C-x"))
  (defun ds/vterm-send-C-z ()
    (interactive)
    (vterm-send "C-z"))
  :bind (:map vterm-mode-map
              ("C-c t" . 'vterm-copy-mode)
              ("C-x C-x" . 'ds/vterm-send-C-x)
	      ("C-z" . vterm-send-C-z)
	      :map vterm-copy-mode-map
              ("C-c t" . 'vterm-copy-mode))
  ;; :after ds-theme
  ;; :config
  ;; :general
  ;; (:keymaps 'vterm-mode-map
  ;;           [escape] #'vterm--self-insert
  ;;           [return] #'vterm--self-insert
  ;;           "p" #'vterm-yank
  ;;           "u" #'vterm-undo
  ;;           "C-y" #'vterm-yank
  ;;           "M-n" #'vterm-send-down
  ;;           "M-p" #'vterm-send-up
  ;;           "M-y" #'vterm-yank-pop
  ;;           "M-/" #'vterm-send-tab
  ;;           )
  :config
  (setq vterm-always-compile-module t)
  (define-key vterm-mode-map (kbd "C-h") 'vterm-send-C-h)
  (define-key vterm-mode-map (kbd "C-z") 'vterm-send-C-z)
  (defun vterm-send-meta-left ()
    "Send `M-<left>' to the libvterm."
    (interactive)
    (vterm-send-key "<left>" nil t))

  (defun vterm-send-meta-right ()
    "Send `M-<right>' to the libvterm."
    (interactive)
    (vterm-send-key "<right>" nil t))
  (define-key vterm-mode-map (kbd "M-<left>") 'vterm-send-meta-left)
  (define-key vterm-mode-map (kbd "M-<right>") 'vterm-send-meta-right)
  (setq vterm-keymap-exceptions (remove "C-h" vterm-keymap-exceptions))
  ;; (defun vterm-send-C-k-and-kill ()
  ;;   "Send `C-k' to libvterm, and put content in kill-ring."
  ;;   (interactive)
  ;;   (kill-ring-save (point) (vterm-end-of-line))
  ;;   (vterm-send-key "k" nil nil t))
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t)))
  (defun ds/vterm (&optional name)
    (interactive "MName: ")
    (if (< 0 (length name))
        (if (get-buffer name)
            (switch-to-buffer name)
          (vterm name))
      (vterm)))
  )

(use-package vterm-toggle
  :straight t
  :when (memq window-system '(mac ns x))
  :bind (([f8] . vterm-toggle)
         ([f9] . vterm-compile)
         :map vterm-mode-map
         ([f8] . vterm-toggle)
         ([(control return)] . vterm-toggle-insert-cd))
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (defvar vterm-compile-buffer nil)
  (defun vterm-compile ()
    "Compile the program including the current buffer in `vterm'."
    (interactive)
    (let* ((command (eval compile-command))
           (w (vterm-toggle--get-window)))
      (setq compile-command (compilation-read-command command))
      (let ((vterm-toggle-use-dedicated-buffer t)
            (vterm-toggle--vterm-dedicated-buffer (if w (vterm-toggle-hide)
                                                    vterm-compile-buffer)))
        (with-current-buffer (vterm-toggle-cd)
          (setq vterm-compile-buffer (current-buffer))
          (rename-buffer "*vterm compilation*")
          (compilation-shell-minor-mode 1)
          (vterm-send-M-w)
          (vterm-send-string compile-command t)
          (vterm-send-return))))))

;; (use-package multi-libvterm
;;   :straight (multi-libvterm :type git :host github :repo "suonlight/multi-libvterm")
;;   :demand
;;   :after projectile
;;   :init
;;   (defvar ds/multi-libvterm-map (make-sparse-keymap)
;;     "Keymap for multi-libvterm commands.")
;;   (defun ds/multi-libvterm-create (name)
;;     "Create a vterm buffer and set it's name to NAME."
;;     (interactive "sName: ")
;;     (let* ((bufname (if (< 0 (length name)) (concat "*vterminal<" name ">*")))
;;            (existing-buf (get-buffer bufname)))
;;       (if (buffer-live-p existing-buf)
;;           (switch-to-buffer existing-buf)
;;         (progn (multi-libvterm bufname)
;;                (if bufname (rename-buffer bufname))))))
;;   (defun ds/multi-libvterm-dedicated-solo ()
;;     "Open the multi-libvterm-dedicated buffer and make it the only window in the frame."
;;     (interactive)
;;     (multi-libvterm-dedicated-close)
;;     (multi-libvterm-dedicated-open)
;;     (delete-other-windows))
;;   :commands (multi-libvterm
;;              multi-libvterm-next
;;              multi-libvterm-prev
;;              multi-libvterm-dedicated-toggle
;;              multi-libvterm-dedicated-open
;;              multi-libvterm-dedicated-close
;;              multi-libvterm-projectile
;;              ds/multi-libvterm-create
;;              ds/multi-libvterm-dedicated-solo)
;;   :bind-keymap ("C-c C-s" . ds/multi-libvterm-map)
;;   :bind (:map projectile-command-map
;;               ("x s" . multi-libvterm-projectile)
;;               :map ds/multi-libvterm-map
;;               ("C-s" . ds/multi-libvterm-create)
;;               ("n" . multi-libvterm-next)
;;               ("p" . multi-libvterm-prev)))


;; (use-package shell-mode
;;   :straight nil)

(provide 'init-vterm)
;;; init-vterm.el ends here
