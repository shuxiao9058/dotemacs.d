;;; custom.el -*- lexical-binding: t; -*-

(setq debug-on-error t)

(setq make-backup-files nil
      inhibit-startup-screen t
      whitespace-line-column 100
      default-directory (expand-file-name "workspace/" "~")
      vagrant-vagrantfile (expand-file-name "vagrant/Vagrantfile" default-directory)
      ;; Wrapping
      truncate-lines t
      truncate-partial-width-windows 50
      ;; evil-want-C-u-scroll nil
      whitespace-style '(face trailing lines-tail))



;;; lisp
(setq ;; lisp-body-indent   2
 lisp-indent-function  'common-lisp-indent-function)


;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; disable annoying notifications
(defcustom message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                        "^Ispell process killed$"
                                        ".+expected selector or type assertion, found.+"
                                        ".+expected identifier on left side.+"
                                        "^LSP ::.+"
                                        ".+and \d{1,10} more errors.+"
                                        "Wrote "
                                        "Liberime: start with shared dir" ;;; liberime
					".+Starting new Ispell process.+" ;;; ispell
					"Package cl is deprecated"
					"Loading[\s\w\/\.-]+\(module\).+"
					;; "Loading[\w\/\d\W]+\(module\).+" ;;; module load
					"For information about GNU Emacs and the GNU system.+"
                                        )
  "filter formatted message string to remove noisy messages"
  :type '(list string)
  :group 'general)

(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (if (and (stringp formatted-string)
	       (cl-some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
	  (let ((inhibit-read-only t))
	    (with-current-buffer "*Messages*"
	      (goto-char (point-max))
	      (insert formatted-string "\n")))
	(progn
	  (ad-set-args 0 `("%s" ,formatted-string))
	  ad-do-it)))))

;;; custom.el ends here
