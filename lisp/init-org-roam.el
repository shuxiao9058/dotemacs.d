;;; lisp/init-org-roam.el -*- lexical-binding: t; -*-

;; https://github.com/iecaser/Configurations/blob/a7e61c25c49556b33d7888599a853da9d4c9cb95/.doom.d/note.el
;; https://www.orgroam.com/manual/Installation-_00281_0029.html#Installation-_00281_0029
(use-package org-roam
  :straight t
  ;; ;; :straight (:files (:defaults "extensions/*"))
  ;; :straight (:host github :repo "org-roam/org-roam"
  ;; 		     :files (:defaults "extensions/*"))
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :init
  (setq org-roam-v2-ack t)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :after org
  :custom
  (org-roam-file-exclude-regexp ".pdf$|.tex$|.bib$|.html$|.log$|.out$|.xml$|agendas.org$")
  (org-roam-verbose nil)  ; https://youtu.be/fn4jIlFwuLU
  (org-roam-buffer-no-delete-other-windows t); make org-roam buffer sticky
  ;; org-roam-graph-viewer "google-chrome-stable"
  (org-roam-completion-system 'default)
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-directory (expand-file-name "Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/beorg/" "~"))
  ;; For org-roam to update LAST_MODIFIED field
  (require 'time-stamp)
  (add-hook 'write-file-functions 'time-stamp) ; update when saving
  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  ;; #+LATEX_HEADER: \\addbibresource{~/Cloud/Documents/bib/zotLib.bib}
  ;;     (setq org-roam-capture-templates
  ;; 	  (quote
  ;; 	   (("d" "default" plain
  ;; 		 (function org-roam-capture--get-point)
  ;; 		 "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}"
  ;; 		 :head "#+LATEX_HEADER: \\usepackage[citestyle=authoryear-icomp,bibstyle=authoryear, hyperref=true,backref=true,maxcitenames=3,url=true,backend=bibtex,natbib=true] {biblatex}
  ;; #+SETUPFILE: ~/.config/emacs/.local/etc/org-html-themes/setup/theme-readtheorg.setup
  ;; #+TITLE: ${title}
  ;; #+CREATED: %u
  ;; Time-stamp: <>
  ;; - tags ::
  ;; " :unnarrowed t))))

  (setq org-roam-capture-templates
	'((
	   "d" "default" plain
	   "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
	   :unnarrowed t
	   )
          (
	   "l" "programming language" plain
	   "* Characteristics:\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t
	   )
          ("b" "book notes" plain
	   "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t
	   )
          ("p" "project" plain
	   "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n*Dates\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:Project")
	   :unnarrowed t
	   ))
	org-roam-dailies-capture-templates '(
                                             (
					      "d" "default" entry
					      "* %<%I:%M %p>: %?"
					      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
					      ))
	;; ;; optimise local variable evaluate and babel
	;; enable-local-variables :all
	;; ;; remote zsh related
	;; shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *"
	)

  ;; ;; Normally, the org-roam buffer doesn't open until you explicitly call
  ;; ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
  ;; ;; org-roam buffer will be opened for you when you use `org-roam-find-file'
  ;; ;; (but not `find-file', to limit the scope of this behavior).
  ;; (add-hook 'find-file-hook
  ;; 	    (defun +org-roam-open-buffer-maybe-h ()
  ;; 	      (and +org-roam-open-buffer-on-find-file
  ;; 		   (memq 'org-roam-buffer--update-maybe post-command-hook)
  ;; 		   (not (window-parameter nil 'window-side)) ; don't proc for popups
  ;; 		   (not (eq 'visible (org-roam-buffer--visibility)))
  ;; 		   (with-current-buffer (window-buffer)
  ;; 		     (org-roam-buffer--get-create)))))
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)
  (require 'org-roam-protocol)
  (require 'org-roam-dailies)
  (org-roam-setup) ;; can cause failure of dump
  ;; :bind (("C-c z l" . org-roam-buffer-toggle)
  ;;        ("C-c z f" . org-roam-node-find)
  ;;        ("C-c z i" . org-roam-node-insert)
  ;;        ("C-c z r" . org-roam-node-random)
  ;;        :map org-mode-map
  ;;        (("C-M-i" . completion-at-point)
  ;;         ("C-c z t" . org-roam-tag-add)
  ;;         ("C-c z a" . org-roam-alias-add)
  ;;         ("C-c z I" . org-roam-node-insert-immediate))
  ;;        :map org-roam-dailies-map
  ;;        ("Y" . org-roam-dailies-capture-yesterday)
  ;;        ("T" . org-roam-dailies-capture-tomorrow))
  ;; :bind-keymap
  ;; ("C-c z d" . org-roam-dailies-map)
  )


(use-package org-roam-ui
  :straight (:type git :host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; required by org-roam-ui
(use-package websocket
  :straight t)

;; (unless (server-running-p)
;;   (org-roam-server-mode))

;; (use-package company-org-roam
;;   :straight t
;;   :after org
;;   :ensure t
;;   :after '(org-roam company)
;;   :config
;;   (add-to-list 'company-backends 'company-org-roam)
;;   ;; :config
;;   ;; (push 'company-org-roam company-backends)
;;   )

(provide 'init-org-roam)
;;; init-org-roam.el ends here
