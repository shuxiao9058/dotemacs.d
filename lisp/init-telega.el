;;; lisp/init-telega.el -*- lexical-binding: t; -*-

(use-package telega
  :straight (telega
	     :host github
	     :repo "zevlg/telega.el"
	     :branch "master"
	     :files (:defaults "contrib" "etc" "server" "Makefile"))
  :commands (telega)
  :defer t
  :custom
  ;; (telega-symbol-reply "↫")
  (telega-root-show-avatars nil)
  ;; (telega-user-show-avatars nil)
  ;; (telega-avatar-factors-alist '((1 . (0.8 . 0.1))
  ;; 				 (2 . (0.8 . 0.1))))
  (telega-animation-play-inline nil)
  (telega-server-libs-prefix "/usr/local")
  (telega-use-images t)
  ;; (telega-proxies
  ;;  (list
  ;;   '(:server "127.0.0.1" :port 6153 :enable nil
  ;; 	:type (:@type "proxyTypeSocks5"))))
  (telega-directory (expand-file-name "telega" poly-cache-dir))
  :config
  ;; show previews for photo/video webpages
  (advice-add #'telega-ins--webpage :before-while
	      (lambda (msg &rest args)
                (let ((ht (telega--tl-get msg :content :web_page :type)))
                  (-contains? '("video" "photo") ht))))

  (add-hook 'telega-chat-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends)
		   (append '(telega-company-emoji
			     telega-company-username
			     telega-company-hashtag)
			   (when (telega-chat-bot-p telega-chatbuf--chat)
			     '(telega-company-botcmd))))
	      (company-mode 1)))
  (unbind-key (kbd "k") telega-msg-button-map)  ;; delete marked or at point (doubled with d)
  (unbind-key (kbd "e") telega-msg-button-map)  ;; msg-edit
  (define-key telega-msg-button-map (kbd "E") 'telega-msg-edit)
  (unbind-key (kbd "n") telega-msg-button-map)  ;; button-forward (seems to not differ from next link)
  (unbind-key (kbd "l") telega-msg-button-map)  ;; redisplay
  (unbind-key (kbd "h") telega-chat-button-map) ;; info (doubled with i)
  )

(provide 'init-telega)
;;; init-telega.el ends here
