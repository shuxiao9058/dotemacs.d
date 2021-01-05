;;; lisp/init-format.el -*- lexical-binding: t; -*-

(use-package format-all
  :straight t
  ;; :ensure t
  :hook ((
	  lua-mode
	  go-mode
	  python-mode
	  ;; cc-mode
	  c-mode
	  c++-mode
	  ;; clang-mode
	  elisp-mode
	  emacs-lisp-mode
	  markdown-mode
	  ;; nix-mode
	  ;;  objc-mode
	  ;;  swift-mode
	  ;;  typescript-mode
	  ;;  web-mode
	  ) . format-all-mode)
  :commands (format-all-mode
	     format-all-buffer)
  :init
  (defconst format-all--system-type
    (cl-case system-type
      (windows-nt 'windows)
      (cygwin     'windows)
      (darwin     'macos)
      (gnu/linux  'linux)
      (berkeley-unix
       (save-match-data
	 (let ((case-fold-search t))
	   (cond ((string-match "freebsd" system-configuration) 'freebsd)
		 ((string-match "openbsd" system-configuration) 'openbsd)
		 ((string-match "netbsd"  system-configuration) 'netbsd))))))
    "Current operating system according to the format-all package.")

  ;; (defun format-all--resolve-system (choices)
  ;;   "Get first choice matching `format-all--system-type' from CHOICES."
  ;;   (cl-dolist (choice choices)
  ;;     (cond ((atom choice)
  ;; 	     (cl-return choice))
  ;; 	    ((eql format-all--system-type (car choice))
  ;; 	     (cl-return (cadr choice))))))

  :config
  (define-format-all-formatter lua-format
    (:executable "lua-format")
    (:install (macos ""))
    (:languages "Lua")
    (:format (format-all--buffer-easy executable "-i" "-c" (expand-file-name "~/.config/lua-format/config.yaml") "--"))
    )

  ;; (define-format-all-formatter clang-format
  ;;   (:executable "clang-format")
  ;;   (:install
  ;;    (macos "brew install clang-format")
  ;;    (windows "scoop install llvm"))
  ;;   (:languages "C" "C++" "Java" "Objective-C" "Protocol Buffer")
  ;;   ;; (:modes c-mode c++-mode java-mode protobuf-mode  objc-mode)
  ;;   ;; (:format (format-all--buffer-easy executable ("-assume-filename=%S" (or buffer-file-name mode-result "")) "-style=file"))
  ;;   (:format
  ;;    (format-all--buffer-easy
  ;;     executable
  ;;     (concat "-assume-filename="
  ;; 	      (or (buffer-file-name)
  ;; 		  (cdr (assoc language
  ;; 			      '(("C"               . ".c")
  ;; 				("C++"             . ".cpp")
  ;; 				("Java"            . ".java")
  ;; 				("Objective-C"     . ".m")
  ;; 				("Protocol Buffer" . ".proto"))))))))
  ;;   )

  (define-format-all-formatter goimports-gofmt
    (:executable "/bin/sh")
    (:install
     (macos "brew install go")
     (windows "scoop install go")
     "go get golang.org/x/tools/cmd/goimports")
    (:languages "Go")
    (:format (format-all--buffer-easy executable "-c" "goimports | gofmt -s")))

  (eval-after-load 'format-all
    (dolist (hook '(lua-mode-hook
		    go-mode-hook
		    python-mode-hook
		    cc-mode
		    c-mode-hook
		    c++-mode-hook
		    clang-mode
		    emacs-lisp-mode-hook
		    markdown-mode-hook))
      (add-hook hook 'format-all-ensure-formatter)))

  ;; ;; (define-format-all-formatter goimports
  ;; ;;   (:executable "goimports")
  ;; ;;   (:install (macos ""))
  ;; ;;   (:languages "Go")
  ;; ;;   (:format (format-all--buffer-easy executable))
  ;; ;;   )
  ;; :general
  ;; (nvmap :keymaps '(format-all-buffer-mode-map)
  ;;   "==" #'format-all-buffer)
  )

(provide 'init-format)
;;; init-format.el ends here
