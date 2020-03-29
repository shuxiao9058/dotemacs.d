;;; lisp/init-format.el -*- lexical-binding: t; -*-

(use-package format-all
    :straight t
    :ensure t
    ;; :hook ((
    ;;         lua-mode
    ;;         go-mode
    ;;         ;; clang-mode
    ;;         elisp-mode
    ;;         emacs-lisp-mode
    ;; 	    markdown-mode
    ;;         ;;  objc-mode
    ;;         ;;  swift-mode
    ;;         ;;  typescript-mode
    ;;         ;;  web-mode
    ;;         ) . format-all-mode)
    :commands (
               format-all-mode
               format-all-buffer)
    :config
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

    (define-format-all-formatter lua-format
	(:executable "lua-format")
      (:install (macos ""))
      (:languages "Lua")
      (:format (format-all--buffer-easy executable  "-c" (expand-file-name "~/.config/lua-format/lua-format.style") "-i" "--"))
      )

    (define-format-all-formatter clang-format
	(:executable "clang-format")
      (:install
       (macos "brew install clang-format")
       (windows "scoop install llvm"))
      (:languages "C" "C++" "Java" "Objective-C" "Protocol Buffer")
      ;; (:modes c-mode c++-mode java-mode protobuf-mode  objc-mode)
      ;; (:format (format-all--buffer-easy executable ("-assume-filename=%S" (or buffer-file-name mode-result "")) "-style=file"))
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
				  ("Protocol Buffer" . ".proto"))))))))
      )

    (define-format-all-formatter goimports
	(:executable "goimports")
      (:install (macos ""))
      (:languages "Go")
      (:format (format-all--buffer-easy executable))
      )
    )

(defvar +format-region-p nil
  "Is non-nil if currently reformatting a selected region, rather than the whole
buffer.")

;;
;;; Commands
(defalias '+format/buffer #'format-all-buffer)

(defun +format/region (beg end)
  "Runs the active formatter on the lines within BEG and END.
WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (save-restriction
    (narrow-to-region beg end)
    (let ((+format-region-p t))
      (+format/buffer))))

(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'+format/region
     #'+format/buffer)))

(defun +format/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (unless (bound-and-true-p lsp-mode)
    (user-error "Not in an LSP buffer"))
  (call-interactively
   (if (poly-region-active-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

(provide 'init-format)
;;; init-format.el ends here
