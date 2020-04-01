;;; lisp/init-screencast.el -*- lexical-binding: t; -*-


;; Screencast
;; brew install gifsicle
(use-package gif-screencast
    :straight t
  :init
  (when IS-MAC
    (setq gif-screencast-args '("-x")
          gif-screencast-cropping-program "mogrify"
          gif-screencast-capture-format "ppm"
          ;; gif-screencast-output-directory "~/Downloads"
          )
    (let ((tmp (expand-file-name "~/gif-screencast")))
      (setq gif-screencast-screenshot-directory (concat tmp "/sceenshot")
            gif-screencast-output-directory  tmp
            )
      ))
  :config
  (advice-add
   #'gif-screencast--cropping-region
   :around
   (lambda (oldfun &rest r)
     (apply #'format "%dx%d+%d+%d"
            (mapcar
             (lambda (x) (* 2 (string-to-number x)))
             (split-string (apply oldfun r) "[+x]")))))
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop)))


(provide 'init-screencast)
;;; init-screencast.el ends here