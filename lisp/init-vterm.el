;;; lisp/init-vterm.el -*- lexical-binding: t; -*-

(use-package vterm
  :straight (vterm :type git :flavor melpa
                   :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
                   :host github :repo "akermu/emacs-libvterm"
                   ;; :fork (:host github :repo "paulbdavis/emacs-libvterm")
                   )
  :demand
  :commands (vterm ds/vterm)
  :custom ((vterm-max-scrollback 10000))
  ; :init
  ; (defun ds/vterm-send-C-x ()
  ;   (interactive)
  ;   (vterm-send "C-x"))
  ; :bind (:map vterm-mode-map
  ;             ("C-c t" . 'vterm-copy-mode)
  ;             ("C-x C-x" . 'ds/vterm-send-C-x)
  ;             :map vterm-copy-mode-map
  ;             ("C-c t" . 'vterm-copy-mode))
  ; :after ds-theme
  ; :config
  ; (defun ds/vterm (&optional name)
  ;   (interactive "MName: ")
  ;   (if (< 0 (length name))
  ;       (if (get-buffer name)
  ;           (switch-to-buffer name)
  ;         (vterm name))
  ;     (vterm)))
  :general
   (:keymaps 'vterm-mode-map
          [escape] #'vterm--self-insert
          [return] #'vterm--self-insert
          "p" #'vterm-yank
          "u" #'vterm-undo
          "C-y" #'vterm-yank
          "M-n" #'vterm-send-down
          "M-p" #'vterm-send-up
          "M-y" #'vterm-yank-pop
          "M-/" #'vterm-send-tab
          )
    )

; (use-package multi-libvterm
;   :straight (multi-libvterm :type git :host github :repo "suonlight/multi-libvterm")
;   :demand
;   :after projectile
;   :init
;   (defvar ds/multi-libvterm-map (make-sparse-keymap)
;     "Keymap for multi-libvterm commands.")
;   (defun ds/multi-libvterm-create (name)
;     "Create a vterm buffer and set it's name to NAME."
;     (interactive "sName: ")
;     (let* ((bufname (if (< 0 (length name)) (concat "*vterminal<" name ">*")))
;            (existing-buf (get-buffer bufname)))
;       (if (buffer-live-p existing-buf)
;           (switch-to-buffer existing-buf)
;         (progn (multi-libvterm bufname)
;                (if bufname (rename-buffer bufname))))))
;   (defun ds/multi-libvterm-dedicated-solo ()
;     "Open the multi-libvterm-dedicated buffer and make it the only window in the frame."
;     (interactive)
;     (multi-libvterm-dedicated-close)
;     (multi-libvterm-dedicated-open)
;     (delete-other-windows))
;   :commands (multi-libvterm
;              multi-libvterm-next
;              multi-libvterm-prev
;              multi-libvterm-dedicated-toggle
;              multi-libvterm-dedicated-open
;              multi-libvterm-dedicated-close
;              multi-libvterm-projectile
;              ds/multi-libvterm-create
;              ds/multi-libvterm-dedicated-solo)
;   :bind-keymap ("C-c C-s" . ds/multi-libvterm-map)
;   :bind (:map projectile-command-map
;               ("x s" . multi-libvterm-projectile)
;               :map ds/multi-libvterm-map
;               ("C-s" . ds/multi-libvterm-create)
;               ("n" . multi-libvterm-next)
;               ("p" . multi-libvterm-prev)))

(provide 'init-vterm)
;;; init-vterm.el ends here
