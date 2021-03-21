;;; lisp/init-telega.el -*- lexical-binding: t; -*-

(use-package telega
  :straight (telega
	     :host github
	     :repo "zevlg/telega.el"
	     :branch "releases"
	     ;; :branch "master"
	     :files (:defaults "contrib" "etc" "server" "Makefile")
	     )
  :commands (telega)
  :defer t
  :custom
  (telega-server-libs-prefix "/opt/td")
  (telega-use-images t)
  (telega-proxies
   (list
    '(:server "127.0.0.1" :port 6153 :enable t
	      :type (:@type "proxyTypeSocks5"))))
  (telega-directory (expand-file-name "telega" poly-cache-dir))
  :config
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
  )

(provide 'init-telega)
;;; init-telega.el ends here
