;;; core/core-packages.el -*- lexical-binding: t; -*-

(use-package server ; built-in
    :straight nil
    :if (display-graphic-p)
    :defer 1
    :config
    (unless (server-running-p)
      (server-start)))

(use-package autorevert
    :straight nil
    ;; :blackout t
    ;; :hook
    :hook (dired-mode . auto-revert-mode)
    :diminish auto-revert-mode
    :config
    (global-auto-revert-mode +1)
    :custom
    (auto-revert-verbose nil))

(use-package undo-tree
    :straight (undo-tree :host github
			 :repo "emacsmirror/undo-tree"
			 :files ("undo-tree.el"))
    ;; :straight nil
    :ensure t
    :custom
    (undo-tree-visualizer-timestamps t)
    (undo-tree-visualizer-diff t)
    ;; (undo-tree-history-directory-alist (quote ((".*" . "~/.emacs.d/.appdata/.undo-tree-history"))))
    (undo-tree-history-directory-alist (list (cons ".*" (expand-file-name "undo-tree-history" poly-cache-dir))))
    (undo-tree-auto-save-history t)
    (undo-tree-visualizer-lazy-drawing 1000)
    :config
    (global-undo-tree-mode)
    )

(use-package hide-mode-line
    :straight t
    :commands (hide-mode-line-mode))

(use-package xclip
    :straight t
    :if IS-LINUX
    :ensure t
    :custom
    (xclip-method 'xclip)
    :config
    (xclip-mode +1)
    (xterm-mouse-mode +1)
    )

(use-package pbcopy
    :straight t
    ;; :if (and IS-MAC (not IS-GUI))
    :if IS-MAC
    :init (turn-on-pbcopy)
    )

(use-package posframe
    :straight (posframe :host github
			:repo "tumashu/posframe"
			:files ("posframe.el"))
    :ensure t
    )

(use-package restart-emacs
    :straight t
    :ensure t)

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


(provide 'core-packages)
;;; core-packages.el ends here
