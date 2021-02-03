;;; lisp/init-format.el -*- lexical-binding: t; -*-

(use-package format-all
  :straight t
  ;; :ensure t
  :hook ((
	  fish-mode
	  prog-mode
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
  ;; :custom
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

  (defun format-all--resolve-system (choices)
    "Get first choice matching `format-all--system-type' from CHOICES."
    (cl-dolist (choice choices)
      (cond ((atom choice)
	     (cl-return choice))
	    ((eql format-all--system-type (car choice))
	     (cl-return (cadr choice))))))
  :config
  (define-format-all-formatter cpp-lua-format
    (:executable "lua-format")
    (:install (macos ""))
    (:languages "Lua")
    (:format (format-all--buffer-easy executable "-i" "-c" (expand-file-name "~/.config/lua-format/config.yaml") "--"))
    )

  (define-format-all-formatter my-clang-format
    (:executable "clang-format")
    (:install
     (macos "brew install clang-format")
     (windows "scoop install llvm"))
    (:languages "C" "C++" "Java" "Objective-C" "Protocol Buffer")
    ;; (:modes c-mode c++-mode java-mode protobuf-mode  objc-mode)
    (:format
     (format-all--buffer-easy
      executable
      (concat "-assume-filename="
	      (or (buffer-file-name)
		  (cdr (assoc language
			      '(("C"               . ".c")
				("C++"             . ".cpp")
				("Java"            . ".java")
				("Objective-C"     . ".m")
				("Objective-C" . ".x")
				("Objective-C" . ".xm")
				("Objective-C" . ".m")
				("Objective-C" . ".mm")
				("Protocol Buffer" . ".proto")))))))))

  (define-format-all-formatter goimports-gofmt
    (:executable "/bin/sh")
    (:install
     (macos "brew install go")
     (windows "scoop install go")
     "go get golang.org/x/tools/cmd/goimports")
    (:languages "Go")
    (:format (format-all--buffer-easy executable "-c" "goimports | gofmt -s")))


  (define-format-all-formatter goimports
    (:executable "goimports")
    (:install (macos ""))
    (:languages "Go")
    (:format (format-all--buffer-easy executable))
    )

  (eval-after-load 'format-all
    (dolist (hook '(lua-mode-hook
		    go-mode-hook
		    python-mode-hook
		    cc-mode-hook
		    c-mode-hook
		    c++-mode-hook
		    clang-mode-hook
		    objc-mode-hook
		    emacs-lisp-mode-hook
		    markdown-mode-hook
		    fish-mode-hook
		    ))
      (add-hook hook 'format-all-ensure-formatter)))

  (setq-default format-all-formatters
		'(("Go" goimports)
		  ("Lua" cpp-lua-format)
		  ("C" my-clang-format)
		  ("C++" my-clang-format)
		  ("Protocol Buffer" my-clang-format)
		  ("SQL" pgformatter)
		  ;; ("CSS" prettier)
		  ;; ("HTML" prettier)
		  ;; ("Dockerfile" dockfmt)
		  ;; ("Shell" shfmt)
		  ;; ("Markdown" prettier)
		  ;; ("Nix" nixfmt)
		  ;; ("Emacs Lisp" emacs-lisp)
		  ;; ("YAML" prettier)
		  ))
  )

(provide 'init-format)
;;; init-format.el ends here
