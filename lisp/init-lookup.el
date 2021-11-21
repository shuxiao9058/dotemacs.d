;;; lisp/init-lookup.el -*- lexical-binding: t; -*-

;;
;;; dumb-jump
(use-package dumb-jump
    :straight t
    :commands dumb-jump-result-follow
    :config
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package counsel-dash
    :straight t
    :commands (counsel-dash counsel-dash-at-point counsel-dash-install-docset)
    :custom
    (counsel-dash-enable-debugging nil)
    (dash-docs-common-docsets '("Django"  "Go" "Flask" "NumPy" "Pandas" "Scala" "Rust" "Python 3" "Java_SE17"))
    :init
    ;; Language Hooks
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
    (add-hook 'scala-mode-hook (lambda () (setq-local counsel-dash-docsets '("Scala" "Akka" "Play_Scala" "Java"))))
    (add-hook 'java-mode-hook (lambda () (setq-local counsel-dash-docsets '("Java" "Play_Java"))))
    (add-hook 'rust-mode-hook (lambda () (setq-local counsel-dash-docsets '("Rust"))))
    (add-hook 'clojure-mode-hook (lambda () (setq-local counsel-dash-docsets '("Clojure"))))
    (add-hook 'haskell-mode-hook (lambda () (setq-local counsel-dash-docsets '("Haskell"))))
    (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))
    (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
    (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
    (add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript"))))
    (add-hook 'js-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript"))))
    (add-hook 'go-mode-hook (lambda () (setq-local counsel-dash-docsets '("Go"))))
    (add-hook 'lua-mode-hook (lambda () (setq-local counsel-dash-docsets '("Lua"))))
    (add-hook 'html-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "Javascript"))))
    (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python 3"))))
    ;; :bind (("C-h C-d" . counsel-dash))
    :bind
    (("C-c d" . counsel-dash-at-point)))

(use-package dash-docs
    :straight t
    ;; :preface
    ;; (defun yc/eww-dash-doc (url)
    ;;   "View dash doc with `eww' in dedicated buffer."
    ;;   (interactive)

    ;;   (with-current-buffer (get-buffer-create "*eww-dash-doc*")
    ;;     (eww-mode)
    ;;     (eww url)))

    :custom
    (dash-docs-enable-debugging nil)
    (dash-docs-docsets-path
     (let ((original-dash-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
       (if (and IS-MAC
		(file-directory-p original-dash-path))
           original-dash-path
	 (expand-file-name "~/Documents/dash-docsets"))))
    ;; (dash-docs-browser-func 'yc/eww-dash-doc)
    :config
    (setq
     dash-docs-common-docsets (dash-docs-installed-docsets)))

;; (use-package zeal-at-point
;;     :straight t
;;     :when (and IS-LINUX (display-graphic-p))
;;     :defer t)

(provide 'init-lookup)
;;; init-lookup.el ends here
