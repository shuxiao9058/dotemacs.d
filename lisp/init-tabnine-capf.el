(defcustom company-tabnine-capf-threshold 60
  "the tabnine threshold for sorting two different backend"
  :group 'company-tabnine-capf
  :type 'integer)

(setq company-tabnine-binaries-folder (expand-file-name ".TabNine" "~")
      company-tabnine-log-file-path "/tmp/TabNine.log"
      company-tabnine-wait 0.25
      company-tabnine-max-num-results 5
      ;; (company-tabnine-max-num-results 4)
      company-tabnine-no-continue t)
;; (setq company-tabnine-binaries-folder "~/.emacs.d/.cache/tabnine")

(defun company-tabnine-capf--annotation (candidate)
  "read annotation from candidate
company-tabnine's annotation is stored in text properties, so smart!
so if annotation cannot read from properties, just put into company-capf"
  (let ((annotation (get-text-property 0 'annotation candidate)))
    (if annotation
        annotation
      (company-capf--annotation candidate))))

(defun company-tabnine-capf--post-completion (candidate)
  "call post completion and return nil"
  (if (and candidate (get-text-property 0 'old_suffix candidate))
      (company-tabnine--post-completion candidate)
    (company--capf-post-completion candidate)))

(defun pp-hash (table)
  (let ((data (nthcdr 2 (nbutlast
                         (split-string (pp-to-string table) "[()]")
                         2))))
    (princ (concat "(" (car data) ")"))))


(defun company-tabnine-capf--mix-candidates (&optional tabnine-result capf-result)
  ;; (if (symbolp tabnine-result)
  ;;     (message "tabnine-result is symbol"))

  ;; (message "type of tabnine-result is: %s, type of capf-result: %s"
  ;; 	   (type-of tabnine-result) (type-of capf-result))

  ;; (if (symbolp capf-result)
  ;;     (message "capf-result is symbol"))
  ;; (let ((tabnine-result-str  (json-serialize (if (symbolp tabnine-result)
  ;; 						 (symbol-value tabnine-result)
  ;; 					       tabnine-result)
  ;;                                            :null-object nil
  ;;                                            :false-object json-false))
  ;; 	(capf-result-str  (json-serialize (if (symbolp capf-result)
  ;; 					      (symbol-value capf-result)
  ;; 					    capf-result)
  ;;                                         :null-object nil
  ;;                                         :false-object json-false))
  ;; 	)
  ;;   (message "tabnine-result-str is: %s, capf-result-str is: %s" tabnine-result-str capf-result-str)
  ;;   )

  ;; (let ((tabnine-result-length (length tabnine-result))
  ;; 	(capf-result-length (length capf-result)))
  ;;   (if (eq tabnine-result-length 0)
  ;; 	(message "tabnine-result is empty")
  ;;     (message "tabnine-restult length is: %d" tabnine-result-length))

  ;;   (if (eq capf-result-length 0)
  ;; 	(message "capf-result is empty")
  ;;     (message "capf-result-length is: %d" capf-result-length)))

  (let ((tabnine-length (length tabnine-result))
	(capf-length (length capf-result)))
    (if (or (> tabnine-length 0) (> capf-length 0))
	(cond
	 ((eq tabnine-length 0) capf-result)
	 ((eq capf-length 0)
	  ;; (message "is tabnine-result, length of tabnine-result: %d" tabnine-length)
	  tabnine-result)
	 ((> (company-tabnine-capf--extract-tabnine-confidence (car tabnine-result)) company-tabnine-capf-threshold)
          ;; (message "confidence is large than threshold")
          `(,(car tabnine-result) ,@(company-tabnine-capf--mix-candidates (cdr tabnine-result) capf-result)))
	 (t
          ;; (message "face to the last cond")
          `(,@capf-result ,@tabnine-result))))))

;; (defun lsp-completion--sort-completions (completions)
;;   "Sort COMPLETIONS."
;;   (message "type completions is: %s" (type-of completions))
;;   (when (hash-table-p completions)
;;     (let ((items (gethash "items" completions)))
;;       (message "type of items is: %s" (type-of items))
;;       (sort
;;        items
;;        (-lambda ((&CompletionItem :sort-text? sort-text-left :label label-left)
;; 		 (&CompletionItem :sort-text? sort-text-right :label label-right))
;; 	 (if (equal sort-text-left sort-text-right)
;;              (string-lessp label-left label-right)
;; 	   (string-lessp sort-text-left sort-text-right)))
;;        )
;;       (puthash "items" items completions)
;;       completions
;;       )
;;     ;; (pp-hash completions)
;;     ;; (message "completions is hash-table")
;;     )
;;   ;; (sort
;;   ;;  completions
;;   ;;  (-lambda ((&CompletionItem :sort-text? sort-text-left :label label-left)
;;   ;;            (&CompletionItem :sort-text? sort-text-right :label label-right))
;;   ;;    (if (equal sort-text-left sort-text-right)
;;   ;;        (string-lessp label-left label-right)
;;   ;;      (string-lessp sort-text-left sort-text-right)))
;;   ;;  )
;;   )


(defun lsp-completion--sort-completions (completions)
  "Sort COMPLETIONS."
  (message "type completions is: %s" (type-of completions))
  (when (hash-table-p completions)
    (let ((items (gethash "items" completions)))
      (message "type of items is: %s" (type-of items))
      ;; dump items
      (dotimes (index (length items))
	;; (message "type of index is: %s" (type-of index))
	(let*((item (aref items index))
	      (type-of-item (type-of item))
	      json-content
	      )
	  (message "type of item is: %s" type-of-item)
	  (pp-hash item)
	  (setq json-content (json-serialize item
					     :null-object nil
					     :false-object json-false))
	  (if json-content
	      (message json-content)
	    )
	  ;; (cl-print-object item nil)
	  ;; (cond
	  ;;  ((string= type-of-item "integer") (message "item is: %d" item))
	  ;;  )
	  )
	;; (cond
	;;  ())
	;; (let ((item (aref completions index))
	;;       json-content)
	;;   (cl-print-object item nil)
	;;   (message "item: %d" index)
	;;   (setq json-content (json-serialize item
	;; 				     :null-object nil
	;; 				     :false-object json-false))
	;;   (message "json-content: %s" json-content)
	;;   )
	;; (message "number: %d" number)
	)
      )
    )

  (when (vectorp completions)
    (progn
      (message "completions is vector")
      ;; (cl-print-object completions nil)
      ;; (dotimes (index (length completions))
      ;; 	(let ((item (aref completions index))
      ;; 	      json-content)
      ;; 	  (cl-print-object item nil)
      ;; 	  (message "item: %d" index)
      ;; 	  (setq json-content (json-serialize item
      ;; 					     :null-object nil
      ;; 					     :false-object json-false))
      ;; 	  (message "json-content: %s" json-content)
      ;; 	  )
      ;; 	;; (message "number: %d" number)
      ;; 	)
      ;; (mapcar (lambda (x) (format "aa: %s" x)) completions)
      )
    )
  (sort
   completions
   (-lambda ((&CompletionItem :sort-text? sort-text-left :label label-left)
	     (&CompletionItem :sort-text? sort-text-right :label label-right))
     (if (equal sort-text-left sort-text-right)
         (string-lessp label-left label-right)
       (string-lessp sort-text-left sort-text-right)))))

;; (mapcar 'list '(a b c d))
;; (mapcar 'princ '(a b c d))

;; (mapcar (lambda (x) (format "%s" x)) '(a b c))

;; (sort (list 'a 'b) #'<)
;; (sort '(9 5 2 -1 5 3 8 7 4) (lambda (x y) (< x y)))

(defun company-tabnine-capf--candidates (prefix)
  "combine and sort the company-tabnine and the company-capf results.
if company-tabnine's confidence is greater then `company-tabnine-capf-threshold',
tabnine's candidate have greater position then others."
  (let ((tabnine-result (company-tabnine--candidates prefix))
        (capf-result (company-capf--candidates prefix))
        tmp-result)
    (setq tmp-result
	  (company-tabnine-capf--mix-candidates tabnine-result capf-result))
    ;; (if tmp-result
    ;; 	;; (progn
    ;;     ;;   (message "tmp-result is not nil")
    ;;     ;;   (message "type of tmp-result is: %s" (type-of tmp-result))
    ;;     ;;   ;; (dotimes (index (length tmp-result))
    ;;     ;;   ;;   (let* ((item (nth index tmp-result))
    ;;     ;;   ;;          (type-of-item (type-of item))
    ;; 	;;   ;; 	   json-content
    ;; 	;;   ;; 	   )
    ;;     ;;   ;;     (cl-print-object item nil)
    ;;     ;;   ;;     (message "item: %d, type-of item is: %s" index (type-of item))
    ;;     ;;   ;;     (cond
    ;;     ;;   ;;      ((string= type-of-item "string") (message "item is: %s" item)
    ;;     ;;   ;;   	(setq json-content (json-serialize item
    ;;     ;;   ;;   					   :null-object nil
    ;;     ;;   ;;   					   :false-object json-false))
    ;;     ;;   ;;   	(message "json-content is: " json-content))
    ;;     ;;   ;;      (t (message "unknow type of item, type: %s" type-of-item))
    ;;     ;;   ;;      ))
    ;; 	;;   ;;   ;; (let ((item (aref tmp-result index))
    ;; 	;;   ;;   ;;       json-content)
    ;; 	;;   ;;   ;;   (cl-print-object item nil)
    ;; 	;;   ;;   ;;   (message "item: %d" index)
    ;; 	;;   ;;   ;;   (setq json-content (json-serialize item
    ;; 	;;   ;;   ;;              :null-object nil
    ;; 	;;   ;;   ;;              :false-object json-false))
    ;; 	;;   ;;   ;;   (message "json-content: %s" json-content)
    ;; 	;;   ;;   ;;   )
    ;;     ;;   ;;   ;; (message "number: %d" number)
    ;;     ;;   ;;   )
    ;;     ;;   )
    ;; 	;; (message "tmp-result is nil")
    ;; 	)
    ;; (message "type of tmp-result is: %s" (type-of tmp-result))
    ;; (when (hash-table-p tmp-result)
    ;;   (pp-hash tmp-result)
    ;;   (message "hash-table"))
    ;; (princ tmp-result)
    ;; (when (symbolp tmp-result))
    tmp-result)
  )


;; (cons 'a (cons 'b '()))
;; (list 'a 'b)

(defun company-tabnine-capf--extract-tabnine-confidence (candidate)
  "extract integer from company-tabnine's candidate"
  (string-to-number (get-text-property 0 'annotation candidate)))

(defun company-tabnine-capf--meta (candidate)
  "return meta data for candidate"
  (let ((tabnine-meta (company-tabnine--meta candidate)))
    (if tabnine-meta
        tabnine-meta
      (company-capf 'meta candidate))))

;;;###autoload
(defun company-tabnine-capf (command &rest args)
  "a company backend for combine tabnine and capf"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tabnine-capf))
    (prefix
     (let ((company-tabnine-result (apply 'company-tabnine `(,command ,@args)))
           (company-capf-result      (apply 'company-capf `(,command ,@args)))
           )
       (or company-tabnine-result company-capf-result)
       )
     ;; (apply 'company-tabnine `(,command ,@args))
     ;; (apply 'company-capf `(,command ,@args))
     )
    (meta (company-tabnine-capf--meta (car args)))
    (annotation (company-tabnine-capf--annotation (car args)))
    (sorted t)
    (no-cache t)
    (kind (apply 'company-capf `(,command ,@args)))
    (post-completion (company-tabnine-capf--post-completion (car args)))
    (candidates (company-tabnine-capf--candidates (car args)))))

;;;###autoload
(defun toggle-company-tabnine-capf ()
  "toggle company-tabnine-capf backend"
  (interactive)
  (when (not (file-exists-p company-tabnine-binaries-folder))
    (company-tabnine-install-binary))
  (if (memq 'company-tabnine-capf company-backends)
      (progn
        (setq company-backends (remove 'company-tabnine-capf company-backends))
        (message "company-tabnine-capf disabled"))
    (add-to-list 'company-backends 'company-tabnine-capf)
    (message "company-tabnine-capf enabled!")))

(provide 'init-tabnine-capf)
