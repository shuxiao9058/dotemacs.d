;;; core/core-font.el -*- lexical-binding: t; -*-

;; (defvar +font-family "Roboto Mono")
(defvar +font-family "Fira Code")
(defvar +ufont-family "Sarasa Gothic SC")
;; (defvar +ufont-family "PingFang SC") ;; chinese font
(defvar +fixed-pitch-family "Sarasa Gothic SC")
(defvar +variable-pitch-family "Sarasa Gothic SC")
(defvar +font-size 10)
;; (defvar +ufont-size 10)

(defun poly/font-exist-p (fontname)
  "test if this font is exist or not."
  (when (and fontname (not (string= fontname "")))
    (when (x-list-fonts fontname) t)))

;; (defun font-installed-p (font-name)
;;   "Check if font with FONT-NAME is available."
;;   (find-font (font-spec :name font-name)))

;; (modify-frame-parameters nil `((ns-appearance . ,(frame-parameter nil 'background-mode))))

;; (catch 'out-loop-tag
;;   (dolist (font '("Fira Code"))
;;     (when (poly/font-exist-p font)
;;       (let ((frame-font (format "%s-%d" font +font-size)))
;; 	(message frame-font)
;; 	(set-frame-font frame-font))
;;       (throw 'out-loop-tag t))))

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
    (princ (format "charset: %s, +ufont-family: %s\n" charset +ufont-family))
    (set-fontset-font
     ;; t
     (frame-parameter frame 'font)
     charset
     (font-spec :family +ufont-family
		:size +font-size
		;; :height 110
		:width 'normal
		;; :weight 'semi-bold
		) frame)))

;; (frame-parameter nil 'font)

;; (defun colawithsauce/set-unicode-fonts(&optional frame)
;;   "Setting unicode-char fonts for emacs."
;;   ;; Use Noto for everything to get consistent view
;;   ;; Chinese, simplified
;;   (set-fontset-font t 'unicode (font-spec :family "Sarasa Mono Slab SC" :height 110 :weight 'semi-bold) frame)
;;   ;; Symbols, including b/w emoji
;;   (set-fontset-font t 'symbol "Apple Color Emoji" frame 'prepend))
;; (add-hook 'after-make-frame-functions #'colawithsauce/set-unicode-fonts)
;; (colawithsauce/set-unicode-fonts nil)

(defun +load-font (&optional frame)
  (interactive)
  (setq english-fonts `(,+font-family))
  (setq chinese-fonts `(,+ufont-family))
  (set-face-attribute 'default nil :font
		      (format "%s-%d" (car english-fonts) 12)
                      ;; (format "%s:pixelsize=%d" (car english-fonts) 12)
		      )
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family +ufont-family
				 ;; (car chinese-fonts)
				 :size 12)))
  (set-face-attribute 'variable-pitch nil :font (format "%s-%d" +ufont-family 12))
  (set-face-attribute 'fixed-pitch nil :font (format "%s-%d" +ufont-family 12))

  (setq face-font-rescale-alist `((,+ufont-family . 1.2)))
  ;; emoji
  (when IS-MAC
    (if (version< "27.0" emacs-version)
        (set-fontset-font
         "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
      (set-fontset-font
       t 'symbol (font-spec :family "Apple Color Emoji" :size 13) nil 'prepend)))

  ;; (let* ((font-spec (format "%s-%d" +font-family +font-size))
  ;;        (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
  ;;        (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
  ;;   (set-fontset-font nil nil (font-spec :family +font-family
  ;; 					 :size +font-size
  ;; 					 :width 'normal
  ;; 					 ) frame)
  ;;   ;; (set-frame-font font-spec frame)
  ;;   (set-face-attribute 'variable-pitch nil :font variable-pitch-font-spec)
  ;;   (set-face-attribute 'fixed-pitch nil :font fixed-pitch-font-spec))
  ;; (+load-ext-font)
  )

;; (set-face-attribute 'default nil :height 100)
;; 该值的单位是1 / 10pt，因此100等于10pt，依此类推。

(+load-font nil)

(add-hook 'after-make-frame-functions #'+load-font)

(custom-set-faces
 '(telega-entity-type-pre ((t :inherit 'fixed-pitch :family nil))))

(add-hook 'telega-root-mode-hook '+load-font)

;; ;; Sarasa Mono SC can make font align correctly,
;; ;; even with mixed Chinese and English
;; (when (member +ufont-family (font-family-list))
;;   (make-face 'telega-align-by-sarasa)
;;   (set-face-font 'telega-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
;;   (dolist (hook '(telga-chat-mode-hook telega-root-mode-hook))
;;     (add-hook hook (lambda()
;;  		     (buffer-face-set 'telega-align-by-sarasa)))))

;; (defun poly/init-ui (&optional frame)
;;   (when IS-GUI
;;     ;; update transparent titlebar textcolor wrt themes
;;     (modify-frame-parameters frame `((ns-appearance . ,(frame-parameter frame 'background-mode))))
;;     (+load-font) t))

;; (poly/init-ui)

;; ;;; Changing font sizes
;; (use-package default-text-scale
;;   :straight t
;;   :bind (("C-M-=" . default-text-scale-increase)
;;          ("C-M--" . default-text-scale-decrease))
;;   :hook (after-init . default-text-scale-mode))

;; ;; 中文字体
;; ;; Rescale to restrict font into same height.
;; (add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.9))
;; (add-to-list 'face-font-rescale-alist '("Sarasa Mono Slab SC" . 0.88))


;; ;; Disable font rescale in variable-pitch mode
;; (defun colawithsauce/disable-rescale-maybe ()
;;   "remove stuffs in `face-font-scale-alist' when in buffer-face-mode."
;;   (if buffer-face-mode
;;       (setq-local face-font-rescale-alist nil)
;;     (setq-local face-font-rescale-alist '(("Sarasa Mono Slab SC" . 0.88) ("Apple Color Emoji" . 0.9) ("-cdac$" . 1.3)))))
;; (add-hook 'buffer-face-mode-hook #'colawithsauce/disable-rescale-maybe)

;; ;; emoji
;; (when IS-MAC
;;   (if (version< "27.0" emacs-version)
;;       (set-fontset-font
;;        "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
;;     (set-fontset-font
;;      t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))

;; (font-at 123)
(provide 'core-font)
;;; core-font.el ends here
