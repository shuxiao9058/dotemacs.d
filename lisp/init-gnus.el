;;; lisp/init-gnus.el -*- lexical-binding: t; -*-

(use-package gnus
  :straight (:type built-in)
  :ensure t
  ;; :straight (nnhackernews :type built-in)
  ;; :straight (nnreddit :type built-in)
  ;; :bind (("C-c m" . 'gnus))
  ;; :bind (:map gnus-article-mode-map
  ;;             ("o" . gnus-mime-copy-part)
  ;;             :map gnus-topic-mode-map
  ;;             ("<tab>" . gnus-topic-select-group))
  :commands gnus
  :hook
  (gnus-select-group-hook . gnus-group-set-timestamp)
  (gnus-summary-exit-hook . gnus-topic-sort-groups-by-alphabet)
  (gnus-summary-exit-hook . gnus-group-sort-groups-by-rank)
  (gnus-group-mode . gnus-topic-mode)
  ((gnus-browse-mode gnus-server-mode gnus-group-mode gnus-summary-mode) . hl-line-mode)
  (gnus-started-hook . gnus-group-list-all-groups)
  :custom
  (gnus-use-cache t)
  (gnus-use-scoring nil)
  (gnus-keep-backlog 10)
  (gnus-suppress-duplicates t)
  (gnus-novice-user nil)
  (gnus-expert-user t)
  (gnus-interactive-exit 'quiet)
  (gnus-dbus-close-on-sleep t)
  (gnus-use-cross-reference nil)
  (gnus-inhibit-startup-message nil)
  (gnus-select-method '(nnmaildir "" (directory "~/Maildir/")))
  ;; (gnus-home-directory (expand-file-name "gnus/" poly-cache-dir))
  ;; (gnus-select-method '(nnfolder ""))
  ;; (gnus-secondary-select-methods
  ;;  '(
  ;;    (nnfolder "")
  ;;    ;; (nnmaildir "Gmail"
  ;;    ;; 		(directory "~/Mail/"))
  ;;    )
  ;;  )
  ;; (gnus-select-method
  ;;  '(nnmaildir "MyMail"
  ;;              (directory "~/Mail/")))
  ;; (gnus-secondary-select-methods nil)
  ;; (gnus-select-method '(nnnil))
  ;; (gnus-secondary-select-methods
  ;;  '((nnmaildir "MyMail"
  ;;               (directory "~/Mail/"))))
  (gnus-secondary-select-methods
   '((nnmaildir ""
                (directory "~/Maildir/"))))
  ;; Render HTML content using gnus-w3m
  (mm-text-html-renderer 'gnus-w3m)
  (gnus-inhibit-images nil);; Keep images displayed
  (gnus-blocked-images nil)
  :config
  (auto-image-file-mode t)
  (setq  user-mail-address "shuxiao9058@gmail.com"
	 send-mail-function 'smtpmail-send-it
	 starttls-gnutls-program "gnutls-cli"
	 starttls-use-gnutls t
	 smtpmail-stream-type 'starttls
	 ;; smtpmail-stream-type  'ssl
	 smtpmail-smtp-server "smtp.gmail.com"
	 smtpmail-smtp-service 587
	 ;; smtpmail-smtp-service 465
	 )

  (setq ;; mail-sources
   mail-sources '((maildir :path "~/Mail/" :subdirs ("cur" "new")))
   ;; '((maildir :path "~/Mail/Inbox/")
   ;;   (maildir :path "~/Mail/archive/"))
   )
  (setq group-name-map '(
			 ;; 	 ("nnmaildir+OldEmail:INBOX" . "Gmail-Inbox")
			 ;; ("nnmaildir+Work:INBOX" . "Work-Inbox")
			 ;; ("nnmaildir+Work:Archive" . "Work-Archive")
			 ;; ("nnmaildir+Work:Backlog" . "Work-Backlog")
			 ;; ("nnmaildir+Work:Sent Mail" . "Work-Sent")
			 ;; ("nnmaildir+Work:org-archive" . "Work-Org-Archive")
			 ;; ("nnmaildir+Gmail:INBOX" . "Gmail: Inbox")
			 ("Inbox" . "Gmail: Inbox")
			 ("QQMail" . "Gmail: QQMail")
			 ))
  ;; (setq gnus-group-line-format "%ue%uM %S%p[%5t][%L]\t%P%5y:%B%(%uG%)%O\n")


  ;; You need to replace this key ID with your own key ID!
  (setq mml2015-signers '("FC6BDB92CD5BEB22")
	mml2015-encrypt-to-self t)
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  (setq mm-inline-text-html-with-images t)

  (require 'browse-url)
  ;; (require 'gnus-dired)
  ;; (require 'gnus-topic)


  (setq nnmail-expiry-wait 30)
  (setq mm-encrypt-option 'guided)
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t)

  ;; gnus article
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.3)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  ;; (setq gnus-visible-headers
  ;;       '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
  ;;         "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
  ;;         "^X-Mailer:"))
  ;; (setq gnus-sorted-header-list gnus-visible-headers)

  ;; Gnus group
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-list-groups-with-ticked-articles nil)
  )

(use-package gnus-group
  :straight (:type built-in)
  ;; :straight nil
  :after gnus
  :ensure t
  :custom
  (gnus-group-sort-function 'gnus-group-sort-by-server)
  (gnus-level-subscribed 6)
  (gnus-level-unsubscribed 7)
  (gnus-level-zombie 8)
  (gnus-activate-level 2)
  (gnus-list-groups-with-ticked-articles nil)
  (gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (gnus-group-mode-line-format "%%b")
  :config
  ;; (setq gnus-group-sort-function
  ;;       '((gnus-group-sort-by-unread)
  ;;         (gnus-group-sort-by-alphabet)
  ;;         (gnus-group-sort-by-rank)))
  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5))

  (define-key gnus-group-mode-map
    ;; list all the subscribed groups even they contain zero un-read messages
    (kbd "o") 'my-gnus-group-list-subscribed-groups)
  :hook ((gnus-group-mode-hook . hl-line-mode)
         (gnus-select-group-hook . gnus-group-set-timestamp))
  :bind (:map gnus-group-mode-map
	      ("M-n" . gnus-topic-goto-next-topic)
	      ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-topic
  :straight (:type built-in)
  :after (gnus gnus-group)
  :ensure nil
  :config
  (setq gnus-topic-display-empty-topics t)
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  )

(use-package gnus-async
  :straight (:type built-in)
  :after gnus
  :ensure nil
  :defer t
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

(use-package gnus-sum
  :straight (:type built-in)
  :after gnus
  :defer t
  :ensure nil
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-save-duplicate-list t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-summary-gather-subject-limit 'fuzzy)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  (setq gnus-face-1 'gnus-header-content)
  (setq gnus-face-2 'gnus-header-from)
  (setq gnus-face-3 'gnus-header-subject)
  (setq gnus-summary-line-format "%U%R  %1{%-16,16&user-date;%}  %2{%-25,25f%}  %3{%B%S%}\n")
  (setq gnus-summary-mode-line-format "%p")
  ;; (setq gnus-sum-thread-tree-false-root ""
  ;;       gnus-sum-thread-tree-indent " "
  ;;       gnus-sum-thread-tree-leaf-with-other "├► "
  ;;       gnus-sum-thread-tree-root ""
  ;;       gnus-sum-thread-tree-single-leaf "╰► "
  ;;       gnus-sum-thread-tree-vertical "│")
  (setq gnus-sum-thread-tree-false-root "─┬> ")
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-single-indent "")
  (setq gnus-sum-thread-tree-leaf-with-other "├─> ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "└─> ")
  (setq gnus-sum-thread-tree-vertical "│")
  )

(use-package gnus-dired
  :straight (:type built-in)
  :after gnus
  :hook (dired-mode . gnus-dired-mode)
  )

(use-package gnus-art
  :straight (:type built-in)
  :after gnus
  :config
  (setq
   gnus-article-browse-delete-temp 'ask
   gnus-article-over-scroll nil
   gnus-article-show-cursor t
   gnus-article-sort-functions
   '((not gnus-article-sort-by-number)
     (not gnus-article-sort-by-date))
   gnus-article-truncate-lines nil
   gnus-html-frame-width 80
   gnus-html-image-automatic-caching t
   gnus-inhibit-images t
   gnus-max-image-proportion 0.7
   gnus-treat-display-smileys nil
   gnus-article-mode-line-format "%G %S %m"
   gnus-visible-headers
   '("^From:" "^Subject:" "^To:" "^Cc:" "^Newsgroups:" "^Date:"
     "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
     "^X-Mailer:")
   gnus-sorted-header-list gnus-visible-headers
   )
  :hook (gnus-article-mode-hook . (lambda () (setq-local fill-column 80)))
  )

(provide 'init-gnus)
;;; init-gnus.el ends here
