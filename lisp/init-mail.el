;;; lisp/init-mail.el -*- lexical-binding: t; -*-

;; (use-package notmuch
;;   :straight t
;;   :after gnus
;;   :ensure nil
;;   :config
;;   (setq mail-user-agent 'message-user-agent)
;;   )

;; (add-hook 'wl-folder-mode-hook (lambda () (evil-emacs-state +1)) t)
;; (add-hook 'wl-summary-mode-hook (lambda () (evil-emacs-state +1)) t)

(use-package wanderlust
  :straight t
  :ensure t
  :no-require t
  :commands (wl wl-other-frame wl-draft)
  :init
  (setq wl-address-file (expand-file-name "wl/addresses.wl" poly-etc-dir)
	wl-temporary-file-directory (expand-file-name "wl" poly-cache-dir)
	wl-init-file (expand-file-name "wl/wl.el" poly-etc-dir)
	wl-folders-file (expand-file-name "wl/folders.wl" poly-etc-dir))

  ;; Wanderlust
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

  ;; Configure Wanderlust as the default mail composer. This enables you
  ;; to run Wanderlust using the C-x m key chord.
  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  ;; :custom
  ;; (wl-auto-select-next 'unread)
  ;; (wl-summary-width nil)
  ;; (wl-summary-weekday-name-lang "en")
  ;; (wl-summary-showto-folder-regexp ".Sent.*")
  ;; (wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s")
  ;; (wl-message-mode-line-format (propertize "%f" 'face 'powerline-active1))
  ;; ;; Summary threads
  ;; (wl-thread-insert-opened t)
  ;; (wl-thread-open-reading-thread t)
  ;; (wl-stay-folder-window t)
  ;; ;; (wl-message-ignored-field-list '("."))
  ;; ;; (wl-message-visible-field-list
  ;; ;;  '("^\\(To\\|Cc\\):"
  ;; ;;    "^Subject:"
  ;; ;;    "^\\(From\\|Reply-To\\):"
  ;; ;;    "^\\(Posted\\|Date\\):"
  ;; ;;    "^Organization:"
  ;; ;;    "^X-\\(Face\\(-[0-9]+\\)?\\|Weather\\|Fortune\\|Now-Playing\\):"))
  ;; ;; (wl-message-sort-field-list
  ;; ;;  (append wl-message-sort-field-list
  ;; ;; 	   '("^Reply-To" "^Posted" "^Date" "^Organization")))
  :config
  (setq mail-user-agent 'wl-user-agent
        pgg-scheme 'gpg
        mime-edit-split-message nil)
  (when (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

  ;; (setq wl-message-id-domain wl-local-domain)

  (add-hook 'wl-folder-mode-hook #'evil-emacs-state)
  (add-hook 'wl-summary-mode-hook #'evil-emacs-state)

  (add-hook 'mime-edit-mode-hook #'auto-fill-mode)

  ;; ;;; w3m octet configuration for handling attachments
  ;; (setq org-mime-library 'semi)
  ;; (setq default-mime-charset-for-write 'utf-8)  ;'iso-8859-1)
  ;; (setq default-mime-charset           'utf-8)  ;'iso-8859-1)



  ;; ;; directory
  ;; (setq ;; elmo-msgdb-directory (expand-file-name "elmo/" doom-local-dir)
  ;;  ;; elmo-cache-directory (expand-file-name  "cache/" elmo-msgdb-directory)
  ;;  ;; wl-temporary-file-directory (expand-file-name "tmp/" elmo-msgdb-directory)
  ;;  ;; elmo-passwd-alist-file-name (expand-file-name "wl-passwd.gpg" elmo-msgdb-directory)

  ;;  ;; elmo-imap4-use-modified-utf7 t
  ;;  ;; elmo-imap4-default-authenticate-type 'plain
  ;;  ;; elmo-imap4-default-stream-type 'ssl
  ;;  elmo-passwd-storage-type 'alist
  ;;  ;; elmo-passwd-storage-type 'auth-source
  ;;  )

  ;; (setq
  ;;  mime-save-directory (expand-file-name "Downloads" wl-temporary-file-directory))

  ;; 	;; ;; All system folders (draft, trash, spam, etc) are placed in the
  ;; 	;; ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
  ;; 	;; wl-default-folder "%inbox"
  ;; 	;; wl-draft-folder   "%[Gmail]/Drafts"
  ;; 	;; wl-trash-folder   "%[Gmail]/Trash"
  ;; 	;; ;; The below is not necessary when you send mail through Gmail's SMTP server,
  ;; 	;; ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
  ;; 	;; wl-fcc            "%[Gmail]/Sent"

  ;; 	;; Mark sent messages as read (sent messages get sent back to you and
  ;; 	;; placed in the folder specified by wl-fcc)
  ;; 	wl-fcc-force-as-read    t

  ;; 	;; For auto-completing foldernames
  ;; 	wl-default-spec "%")

  ;; ;; 利用 Org-mode 发送 html 邮件
  ;; (setq org-mime-library 'semi)


  ;; ;; ;; Visible Headers
  ;; ;; (setq wl-message-visible-field-list
  ;; ;;       (append mime-view-visible-field-list
  ;; ;;               '("^Subject:" "^From:" "^To:" "^Cc:" "^Bcc:"
  ;; ;;                 "^X-Mailer:" "^X-Newsreader:" "^User-Agent:"
  ;; ;;                 "^X-Face:" "^X-Mail-Count:" "^X-ML-COUNT:")))

  ;; ;; ;; Invisible Headers
  ;; ;; (setq wl-message-ignored-field-list
  ;; ;;       (append mime-view-ignored-field-list
  ;; ;;               '(".*Received:" ".*Path:" ".*Id:" "^References:"
  ;; ;;                 "^Replied:" "^Errors-To:"
  ;; ;;                 "^Lines:" "^Sender:" ".*Host:" "^Xref:"
  ;; ;;                 "^Content-Type:" "^Content-Transfer-Encoding:"
  ;; ;;                 "^Precedence:"
  ;; ;;                 "^Status:" "^X-VM-.*:"
  ;; ;;                 "^X-Info:" "^X-PGP" "^X-Face-Version:"
  ;; ;;                 "^X-UIDL:" "^X-Dispatcher:"
  ;; ;;                 "^MIME-Version:" "^X-ML" "^Message-I.:"
  ;; ;;                 "^Delivered-To:" "^Mailing-List:"
  ;; ;;                 "^ML-Name:" "^Reply-To:" "Date:"
  ;; ;;                 "^X-Loop" "^X-List-Help:"
  ;; ;;                 "^X-Trace:" "^X-Complaints-To:"
  ;; ;;                 "^Received-SPF:" "^Message-ID:"
  ;; ;;                 "^MIME-Version:" "^Content-Transfer-Encoding:"
  ;; ;;                 "^Authentication-Results:"
  ;; ;;                 "^X-Priority:" "^X-MSMail-Priority:"
  ;; ;;                 "^X-Mailer:" "^X-MimeOLE:")))


  ;; ;; ;; ..but these five
  ;; ;; (setq wl-message-visible-field-list
  ;; ;;       '("^To:"
  ;; ;;         "^Cc:"
  ;; ;;         "^From:"
  ;; ;;         "^Subject:"
  ;; ;;         "^Date:"))

  ;; ;; (setq wl-message-ignored-field-list
  ;; ;;     '(".")
  ;; ;;     wl-message-visible-field-list
  ;; ;;     '("^\\(To\\|Cc\\):"
  ;; ;;       "^Subject:"
  ;; ;;       "^\\(From\\|Reply-To\\):"
  ;; ;;       "^\\(Posted\\|Date\\):"
  ;; ;;       "^Organization:"
  ;; ;;       "^X-\\(Face\\(-[0-9]+\\)?\\|Weather\\|Fortune\\|Now-Playing\\):")
  ;; ;;     wl-message-sort-field-list
  ;; ;;     (append wl-message-sort-field-list
  ;; ;;             '("^Reply-To" "^Posted" "^Date" "^Organization")))

  ;; ;; (setq wl-quicksearch-folder "%[Gmail]/All Mail")

  ;; ;; ;; This is needed to allow msmtp to do its magic:
  ;; ;; (setq message-sendmail-f-is-evil 't)

  ;; ;; ;; bbdb
  ;; ;;   (require 'bbdb-wl)
  ;; ;;   (bbdb-wl-setup)
  ;; ;;   (setq bbdb-use-pop-up nil)
  ;; ;;   (setq bbdb/mail-auto-create-p nil)
  ;; ;;   (setq signature-use-bbdb nil)
  ;; ;;   (setq wl-summary-from-function 'bbdb-wl-from-func)
  ;; ;;   (setq bbdb-always-add-addresses nil)
  ;; ;;   (setq bbdb-offer-save 'yes)
  ;; ;;   (setq bbdb-quiet-about-name-mismatches 0)

  ;; ;; send
  ;; (setq wl-draft-send-mail-function 'wl-draft-send-mail-with-sendmail)
  ;; ;; (setq message-sendmail-extra-arguments '("-a" "account"))
  ;; (setq mail-specify-envelope-from t
  ;; 	message-sendmail-envelope-from 'header
  ;; 	sendmail-program "/usr/local/bin/msmtp"
  ;; 	message-sendmail-f-is-evil 't)

  ;; (setq wl-summary-line-format-spec-alist
  ;;       (append wl-summary-line-format-spec-alist
  ;;               '((?@ (wl-summary-line-attached)))))


  ;; ;; Open ~/.wl in emacs lisp mode.
  ;; (add-to-list 'auto-mode-alist '("\.wl$" . emacs-lisp-mode))

  ;; (add-hook 'mime-view-mode-hook
  ;; 	    #'(lambda () (setq show-trailing-whitespace nil)))
  ;; (if (boundp 'mail-user-agent)
  ;;     (setq mail-user-agent 'wl-user-agent))
  ;; (if (fboundp 'define-mail-user-agent)
  ;;     (define-mail-user-agent
  ;; 	'wl-user-agent
  ;; 	'wl-user-agent-compose
  ;; 	'wl-draft-send
  ;; 	'wl-draft-kill
  ;; 	'mail-send-hook))
  )

;; (eval-after-load 'mime-edit
;;   (setq mime-file-types
;; 	  '(
;; 	    ("\\.pdf$" "application" "pdf"
;; 	     nil "base64" "attachment"
;; 	     (("filename" . file)))
;; 	    ("\\.sxw$" "application" "vnd.sun.xml.writer"
;; 	     nil "base64" "attachment"
;; 	     (("filename" . file)))
;; 	    ("\\.sxc$" "application" "vnd.sun.xml.calc"
;; 	     nil "base64" "attachment"
;; 	     (("filename" . file)))
;; 	    ("\\.xls$" "application" "vnd.ms-excel"
;; 	     nil "base64" "attachment"
;; 	     (("filename" . file)))

;; 	    ("\\.lisp$" "application" "octet-stream"
;; 	     (("type" . "common-lisp")))

;; 	    ("\\.cl$" "application" "octet-stream"
;; 	     (("type" . "common-lisp")))

;; 	    ("\\.java$" "application" "octet-stream"
;; 	     (("type" . "java"))
;; 	     "7bit" "attachment"
;; 	     (("filename" . file)))
;; 	    )
;; 	  ))


;;   (autoload 'elmo-split "elmo-split" "Split messages on the folder." t)

;;   (add-hook 'wl-draft-mode-hook 'turn-on-flyspell)
;; 					;(add-hook 'wl-draft-mode-hook 'turn-on-auto-fill)
;;   (add-hook 'wl-draft-mode-hook 'visual-line-mode)



;;   (defun mail-to-address-at-point ()
;;     ;; Based on a defun by Kevin Rodgers <kevinr@ihs.com>
;;     "*Edit a new mail message to the address at point."
;;     (interactive)
;;     (let ((url-at-point (substring (thing-at-point 'url) 7)))
;;       (if (string-match email-regexp url-at-point)
;; 	  (compose-mail url-at-point nil nil nil 'switch-to-buffer-other-frame)
;; 	(message "Bad email address"))))

;;   ;; summary
;;   (setq wl-auto-select-next 'unread
;; 	wl-summary-auto-sync-marks nil
;; 	wl-summary-width nil
;; 	wl-summary-fix-timezone nil
;; 	wl-summary-weekday-name-lang "en"
;; 	wl-summary-showto-folder-regexp ".Sent.*"
;; 	wl-summary-line-format "%T%P%Y-%M-%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
;; 	wl-message-mode-line-format (propertize "%f" 'face 'powerline-active1)
;; 	wl-thread-insert-opened t
;; 	wl-thread-open-reading-thread t)


;;   (setq mm-text-html-renderer 'shr
;;         mime-shr-blocked-images nil
;;         mime-view-text/html-previewer 'shr
;;         mime-shr-blocked-images nil
;;         mime-setup-enable-inline-image t
;;         )


;;   ;; ;; inline image
;;   ;; (setq mime-w3m-safe-url-regexp nil
;;   ;; mime-w3m-display-inline-images t)
;;   ;; (setq mime-edit-split-message nil)
;;   )

;; (use-package! semi
;; 	      :hook ((mime-view-mode-hook . user--mime-view-mode-hook)
;; 		     (mime-edit-mode-hook . user--mime-edit-mode-hook))
;; 	      :config
;; 	      (setq mm-text-html-renderer 'shr
;; 		    ;; ;; inline image
;; 		    ;; mime-w3m-safe-url-regexp nil
;; 		    ;; mime-w3m-display-inline-images t

;; 		    ;; w3m-display-inline-image t
;; 		    mm-inline-text-html-with-images t
;; 		    ;; mm-w3m-safe-url-regexp nil
;; 		    gnus-inhibit-images nil

;; 		    ;; ;; Don't split large mails.
;; 		    ;; mime-edit-split-message nil
;; 		    ;; Decrypt encrypted emails automatically.
;; 		    mime-pgp-decrypt-when-preview t
;; 		    ;; MIME type priorities.
;; 		    mime-view-type-subtype-score-alist
;; 		    '(((text . plain) . 4)
;; 		      ((text . enriched) . 3)
;; 		      ((text . html) . 2)
;; 		      ((text . richtext) . 1)))
;; 	      )

;; ;; (defun user--semi-w3m-config ()
;; ;;   "Configure SEMI to use w3m."
;; ;;   (autoload 'mime-w3m-preview-text/html "mime-w3m")

;; ;;   ;; Use w3m to view HTML mail.
;; ;;   (ctree-set-calist-strictly
;; ;;    'mime-preview-condition
;; ;;    '((type . text)
;; ;;      (subtype . html)
;; ;;      (body . visible)
;; ;;      (body-presentation-method . mime-w3m-preview-text/html)))

;; ;;   )

;; ;; (with-eval-after-load 'mime-view
;; ;;   (cond

;; ;;    ;; ((feature-p 'mime-shr) (user--semi-shr-config))
;; ;;    ;; ((feature-p 'emacs-w3m) (user--semi-w3m-config)))
;; ;;   )
;; ;; )

;; ;; (use-package! w3m
;; ;;   ;; :config
;; ;;   )

;; 					; ;; BBDB integration
;; 					; (use-package! bbdb
;; 					;   :init
;; 					;   :config
;; 					;   (require 'bbdb-wl)
;; 					;   (bbdb-wl-setup)
;; 					;   ; (setq bbdb-use-pop-up nil)
;; 					;   ; (setq bbdb/mail-auto-create-p nil)
;; 					;   ; (setq signature-use-bbdb nil)
;; 					;   ; (setq wl-summary-from-function 'bbdb-wl-from-func)
;; 					;   ; (setq bbdb-always-add-addresses nil)
;; 					;   ; (setq bbdb-offer-save 'yes)
;; 					;   ; (setq bbdb-quiet-about-name-mismatches 0)


;; 					;   ; ;; want to complete based on email-only, full name, aka, mail-alias
;; 					;   ; ;; Show completions as: full name <email>  if it's a primary email
;; 					;   ; ;;                  or: email (full name) if it's not primary
;; 					;   ; ;;                  or: aka (full name) for all AKAs
;; 					;   ; ;;                  or: mail-alias [N recipients]
;; 					;   ; ;; In each case, set a property linked to the text to insert.

;; 					;   ; (defun cl-bbdb-alt-address (r a)
;; 					;   ;   (concat a " (" (bbdb-record-name r) ")"))

;; 					;   ; (defun cl-bbdb-address-props (r &optional s e)
;; 					;   ;   (let ((a (bbdb-dwim-net-address r e)))
;; 					;   ;     (unless s (setq s a))
;; 					;   ;     (set-text-properties 0 (length s) (list 'address a) s)
;; 					;   ;     s))

;; 					;   ; (defun cl-bbdb-alt-props (r e)
;; 					;   ;   (cl-bbdb-address-props r (cl-bbdb-alt-address r e) e))

;; 					;   ; (defun cl-bbdb-aka-props (r a)
;; 					;   ;   (cl-bbdb-address-props r (cl-bbdb-alt-address r a)))

;; 					;   ; (defun cl-bbdb-completions ()
;; 					;   ;   (let (alias-map choices)
;; 					;   ;     (dolist (r (bbdb-records))
;; 					;   ;       (when (bbdb-record-net r)
;; 					;   ;         (setq choices (cons (cl-bbdb-address-props r) choices))
;; 					;   ;         (dolist (e (cdr (bbdb-record-net r)))
;; 					;   ;           (setq choices (cons (cl-bbdb-alt-props r e) choices)))
;; 					;   ;         (dolist (a (bbdb-record-aka r))
;; 					;   ;           (setq choices (cons (cl-bbdb-aka-props r a) choices)))
;; 					;   ;         (let ((aliases (cdr (assq 'mail-alias (bbdb-record-raw-notes r)))))
;; 					;   ;           (dolist (a (and aliases (split-string aliases ", ")))
;; 					;   ;             (let ((binding (assoc a alias-map))
;; 					;   ;                   (e (bbdb-dwim-net-address r)))
;; 					;   ;               (if binding
;; 					;   ;                   (setcdr binding (cons e (cdr binding)))
;; 					;   ;                 (setq alias-map (cons (cons a (list e)) alias-map))))))))
;; 					;   ;     ;; Now process the aliases
;; 					;   ;     (dolist (binding alias-map)
;; 					;   ;       (let ((s (format "%s (%d recipients)" (car binding)
;; 					;   ;                        (length (cdr binding))))
;; 					;   ;             (es (mapconcat 'identity (cdr binding) ", ")))
;; 					;   ;         (set-text-properties 0 (length s) (list 'address es) s)
;; 					;   ;         (setq choices (cons s choices))))
;; 					;   ;     choices))

;; 					;   ; (defun cl-ido-bbdb-complete ()
;; 					;   ; (interactive)
;; 					;   ; (let* ((end (point))
;; 					;   ;        (beg (save-excursion
;; 					;   ;               (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
;; 					;   ;               (goto-char (match-end 0))
;; 					;   ;               (point)))
;; 					;   ;        (orig (buffer-substring beg end))
;; 					;   ;        (typed (downcase orig))
;; 					;   ;        (pattern (bbdb-string-trim typed)))
;; 					;   ;   (let ((s (ido-completing-read "Recipient: "
;; 					;   ;                                 (cl-bbdb-completions)
;; 					;   ;                                 nil nil pattern)))
;; 					;   ;     (delete-region beg end)
;; 					;   ;     (insert (get-text-property 0 'address s)))))
;; 					;   )


(provide 'init-mail)
;;; init-mail.el ends here
