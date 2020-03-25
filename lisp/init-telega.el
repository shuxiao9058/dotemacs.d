;;; lisp/init-telega.el -*- lexical-binding: t; -*-

(use-package telega
    :straight (telega
	       :host github
	       :repo "zevlg/telega.el"
	       :branch "master"
	       :files (:defaults "etc" "server" "Makefile")
	       )
    :commands (telega)
    :defer t
    :init
    (unless IS-GUI (setq telega-use-images nil))
    :custom
    (telega-proxies
     (list
      '(:server "127.0.0.1" :port 6153 :enable t
        :type (:@type "proxyTypeSocks5"))))
    ;; (telega-chat-button-width 28)
    ;; (telega-chat-fill-column 47)
    ;; (telega-root-fill-column 48)
    :config
    (progn
      ;; NOTE: Fix mode line by resetting width
      (doom-modeline--set-char-widths doom-modeline-rhs-icons-alist)
      (add-hook 'telega-chat-mode-hook
		(lambda ()
		  (set (make-local-variable 'company-backends)
		       (append '(telega-company-emoji
				 telega-company-username
				 telega-company-hashtag)
			       (when (telega-chat-bot-p telega-chatbuf--chat)
				 '(telega-company-botcmd))))
		  (company-mode 1)))

      ;; Sarasa Mono SC can make font align correctly,
      ;; even with mixed Chinese and English
      (when (member "Sarasa Mono SC" (font-family-list))
	(make-face 'telega-align-by-sarasa)
	(set-face-font 'telega-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
	(dolist (hook '(telga-chat-mode-hook telega-root-mode-hook))
	  (add-hook hook (lambda()
			   (buffer-face-set 'telega-align-by-sarasa)))))

      (with-eval-after-load 'all-the-icons
	(add-to-list 'all-the-icons-mode-icon-alist
		     '(telega-root-mode all-the-icons-fileicon "telegram"
		       :heigt 1.0
		       :v-adjust -0.2
		       :face all-the-icons-yellow))
	(add-to-list 'all-the-icons-mode-icon-alist
		     '(telega-chat-mode all-the-icons-fileicon "telegram"
		       :heigt 1.0
		       :v-adjust -0.2
		       :face all-the-icons-blue)))
      ;; ;; D-Bus notifications
      ;; (telega-notifications-mode 1)
      )
    )

(provide 'init-telega)
;;; init-telega.el ends here
