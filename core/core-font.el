;;; core/core-font.el -*- lexical-binding: t; -*-

;; (defvar +font-family "Roboto Mono")
(defvar +font-family "Fira Code")
(defvar +ufont-family "Sarasa Gothic SC")
;; (defvar +ufont-family "PingFang SC") ;; chinese font
(defvar +fixed-pitch-family "Sarasa Gothic SC")
(defvar +variable-pitch-family "Sarasa Gothic SC")
(defvar +font-size 12)

(defun poly/font-exist-p (fontname)
  "test if this font is exist or not."
  (when (and fontname (not (string= fontname "")))
    (when (x-list-fonts fontname) t)))

;; (defun font-installed-p (font-name)
;;   "Check if font with FONT-NAME is available."
;;   (find-font (font-spec :name font-name)))

;; (modify-frame-parameters nil `((ns-appearance . ,(frame-parameter nil 'background-mode))))

(catch 'out-loop-tag
  (dolist (font '("Fira Code"))
    (when (poly/font-exist-p font)
      (let ((frame-font (format "%s-%d" font +font-size)))
	(message frame-font)
	(set-frame-font frame-font))
      (throw 'out-loop-tag t))))

;; (defun +load-base-font ()
;;   (let* ((font-spec (format "%s-%d" +font-family +font-size))
;;          (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
;;          (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
;;     (add-to-list 'default-frame-alist `(font . ,font-spec))
;;     (set-face-attribute 'variable-pitch nil :font variable-pitch-font-spec)
;;     (set-face-attribute 'fixed-pitch nil :font fixed-pitch-font-spec)))

(defun +load-ext-font (&optional frame)
  "set unicode font"
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter frame 'font)
     charset
     (font-spec :family +ufont-family))))

(defun +load-font ()
  (interactive)
  (let* ((font-spec (format "%s-%d" +font-family +font-size))
         (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
         (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'variable-pitch nil :font variable-pitch-font-spec)
    (set-face-attribute 'fixed-pitch nil :font fixed-pitch-font-spec))
  (+load-ext-font))

(defun poly/init-ui (&optional frame)
  (when IS-GUI
    ;; update transparent titlebar textcolor wrt themes
    (modify-frame-parameters frame `((ns-appearance . ,(frame-parameter frame 'background-mode))))
    (+load-font) t))

(poly/init-ui)


;;; Changing font sizes
(use-package default-text-scale
  :straight t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease))
  :hook (after-init . default-text-scale-mode))

;; setting fonts
(use-package faces
  :straight nil
  :custom-face
  (variable-pitch
   ((t (:font ,(font-xlfd-name (font-spec :family "Fira Code"
                                          :foundry "Outline"
                                          ;; If we do not specify this, Emacs
                                          ;; selects ascii and we miss accents
                                          :registry "iso10646-1"))
              :slant normal :weight normal
              :height 110 :width normal))))
  (default
    ((t (:font ,(font-xlfd-name (font-spec :family "Fira Code"
                                           :foundry "Outline"
                                           ;; If we do not specify this, Emacs
                                           ;; selects ascii and we miss accents
                                           :registry "iso10646-1"))
               :slant normal :weight normal
               :height 110 :width normal))))
  (fixed-pitch
   ((t (:inherit default))))

  :config
  (defun colawithsauce/set-unicode-fonts(&optional frame)
    "Setting unicode-char fonts for emacs."
    ;; Use Noto for everything to get consistent view
    ;; Chinese, simplified
    (set-fontset-font t 'unicode (font-spec :family "Sarasa Mono Slab SC" :height 110 :weight 'semi-bold) frame)
    ;; Symbols, including b/w emoji
    (set-fontset-font t 'symbol "Apple Color Emoji" frame 'prepend))
  (add-hook 'after-make-frame-functions #'colawithsauce/set-unicode-fonts)

  (colawithsauce/set-unicode-fonts nil)

  ;; 中文字体
  ;; Rescale to restrict font into same height.
  (add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.9))
  (add-to-list 'face-font-rescale-alist '("Sarasa Mono Slab SC" . 0.88))

  ;; Disable font rescale in variable-pitch mode
  (defun colawithsauce/disable-rescale-maybe ()
    "remove stuffs in `face-font-scale-alist' when in buffer-face-mode."
    (if buffer-face-mode
        (setq-local face-font-rescale-alist nil)
      (setq-local face-font-rescale-alist '(("Sarasa Mono Slab SC" . 0.88) ("Apple Color Emoji" . 0.9) ("-cdac$" . 1.3)))))
  (add-hook 'buffer-face-mode-hook #'colawithsauce/disable-rescale-maybe))

(provide 'core-font)
