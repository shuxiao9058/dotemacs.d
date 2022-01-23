;;; lisp/init-tabnine.el -*- lexical-binding: t; -*-

(defun tabnine//company-box-icons--tabnine (candidate)
  (when (eq (get-text-property 0 'company-backend candidate)
            'company-tabnine)
    'Reference))

(defun tabnine//sort-by-tabnine (candidates)
  "The first two candidates will be from company-lsp, the following two
candidates will be from company-tabnine, others keeping their own origin order."
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))

(use-package company-tabnine
  ;; :straight (:local-repo "/Users/jiya/workspace/company-tabnine")
  :straight t
  ;; :straight (company-tabnine
  ;; 	     :host github
  ;; 	     :repo "karta0807913/company-tabnine"
  ;; 	     ;; :branch "master"
  ;; 	     ;; :files (:defaults "contrib" "etc" "server" "Makefile")
  ;; 	     )
  :commands company-tabnine-start-process
  :ensure t
  :after company
  :custom
  (company-tabnine-binaries-folder (expand-file-name ".TabNine" "~"))
  (company-tabnine-log-file-path "/tmp/TabNine.log")
  (company-tabnine-wait 0.25)
  (company-tabnine-max-num-results 5)
  ;; (company-tabnine-max-num-results 4)
  (company-tabnine-no-continue t)
  :config
  (setq company-tabnine-executable-args (list "--log-level" "Debug"))
  (setq company-backends
	;; '(company-tabnine)
	'(;; company-tabnine
	  (company-tabnine :with company-capf :separate)
	  company-capf
	  company-dabbrev-code
	  (company-files          ; files & directory
	   company-keywords       ; keywords
	   )
	  (company-abbrev company-dabbrev)
	  )
	)
  ;; (setq-default company-backends
  ;; 		'(company-tabnine
  ;; 		  ;; (company-tabnine :with company-capf :separate)
  ;; 		  company-capf
  ;; 		  company-dabbrev-code
  ;; 		  (company-files          ; files & directory
  ;; 		   company-keywords       ; keywords
  ;; 		   )
  ;; 		  (company-abbrev company-dabbrev)))
  (when (> 9 company-tabnine-max-num-results)
    ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (add-to-list 'company-transformers 'tabnine//sort-by-tabnine t)
    )
  ;; workaround for company-flx-mode and other transformers
  (setq company-tabnine--disable-next-transform nil)
  (defun my-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
	(apply func args)
      (setq company-tabnine--disable-next-transform nil)
      (car args)))

  (defun my-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
      (setq company-tabnine--disable-next-transform t))
    (apply func args))

  (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  (advice-add #'company-tabnine :around #'my-company-tabnine)

  (with-eval-after-load 'company
    (setq-default company-backends
		  '(company-tabnine
		    ;; (company-tabnine :with company-capf :separate)
		    ;; company-capf
		    company-dabbrev-code
		    (company-files          ; files & directory
		     company-keywords       ; keywords
		     )
		    (company-abbrev company-dabbrev))))
  )

(provide 'init-tabnine)
;;; init-tabnine.el ends here
