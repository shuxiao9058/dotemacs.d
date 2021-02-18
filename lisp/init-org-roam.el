;;; lisp/init-org-roam.el -*- lexical-binding: t; -*-

;; https://github.com/iecaser/Configurations/blob/a7e61c25c49556b33d7888599a853da9d4c9cb95/.doom.d/note.el
;; https://www.orgroam.com/manual/Installation-_00281_0029.html#Installation-_00281_0029
(use-package org-roam
  :straight t
  :ensure t
  :hook
  (after-init . org-roam-mode)

  :custom
  (org-roam-directory (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/beorg/"))
  (org-roam-file-exclude-regexp ".pdf$|.tex$|.bib$|.html$|.log$|.out$|.xml$")
  (org-roam-verbose nil)  ; https://youtu.be/fn4jIlFwuLU
  (org-roam-buffer-no-delete-other-windows t); make org-roam buffer sticky
  ;; org-roam-graph-viewer "google-chrome-stable"
  (org-roam-completion-system 'default)
  (org-roam-completion-everywhere t)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :general
  (:keymaps 'org-roam-mode-map
	    "C-c n l"  #'org-roam
	    "C-c n f" #'org-roam-find-file
	    "C-c n g" #'org-roam-graph
	    ;; "C-c n n" . org-roam
            "C-c n f" #'org-roam-find-file
            "C-c n c" #'org-roam-capture
            "C-c n o" #'org-noter ;; open
            "C-c n g" #'org-noter-sync-current-note  ;; goto
            "C-c n G" #'org-noter-sync-current-page-or-chapter
            "C-c n v" #'org-roam-server-open  ;; view
            "C-c n u" #'org-roam-unlinked-references  ;; unlinked
            "C-c n j" #'org-roam-jump-to-index
            "C-c n b" #'org-roam-switch-to-buffer
            "C-c n d" #'deft
            "C-c n r" #'org-ref-helm-insert-cite-link
            ;; "C-c n s" #'+default/org-notes-search)
            "C-c n t" #'org-roam-dailies-today
            "C-c n i"  #'org-roam-insert
	    )
  (:keymaps 'org-mode-map
	    "C-c n i" #'org-roam-insert
	    "C-c n I" #'org-roam-insert-immediate)
  :after org
  :config
  ;; For org-roam to update LAST_MODIFIED field
  (require 'time-stamp)
  (add-hook 'write-file-functions 'time-stamp) ; update when saving

  (setq org-roam-completion-system 'ivy)


  ;; #+LATEX_HEADER: \\addbibresource{~/Cloud/Documents/bib/zotLib.bib}
  (setq org-roam-capture-templates
	(quote
	 (("d" "default" plain
           (function org-roam-capture--get-point)
           "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+LATEX_HEADER: \\usepackage[citestyle=authoryear-icomp,bibstyle=authoryear, hyperref=true,backref=true,maxcitenames=3,url=true,backend=bibtex,natbib=true] {biblatex}
#+SETUPFILE: ~/.config/emacs/.local/etc/org-html-themes/setup/theme-readtheorg.setup
#+TITLE: ${title}
#+CREATED: %u
Time-stamp: <>
- tags ::
" :unnarrowed t))))

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
  )

(use-package org-roam-server
  :straight t
  :ensure t
  :after org
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8848
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 80
        org-roam-server-network-label-wrap-length 25)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url (format "http://localhost:%d" org-roam-server-port))
    ))

(unless (server-running-p)
  (org-roam-server-mode))

(use-package company-org-roam
  :straight t
  :after org
  :ensure t
  :after '(org-roam company)
  :config
  (add-to-list 'company-backends 'company-org-roam)
  ;; :config
  ;; (push 'company-org-roam company-backends)
  )

(use-package org-ref
  :straight t
  :after org
  :config
  (setq
   org-latex-prefer-user-labels t
   ;; open pdf inside emacs
   bibtex-completion-pdf-open-function 'find-file
   ;; open pdf outside emacs
   ;; bibtex-completion-pdf-open-function 'org-open-file
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   ;; reftex-default-bibliography (list zot_bib)
   ;; org-ref-default-bibliography (list zot_bib)
   ;; org-ref-bibliography-notes (concat org_notes "notes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   ;; org-ref-notes-directory org_notes
   org-ref-notes-function 'orb-edit-notes
   ))

(use-package bibtex
  :straight t
  :after org
  :config
  (setq
   ;; org-ref-completion-library 'org-ref-ivy-cite
   ;; bibtex-completion-notes-path org_notes
   ;; bibtex-completion-bibliography zot_bib
   bibtex-completion-pdf-field "file"
   bibtex-completion-additional-search-fields '(tags)
   bibtex-completion-notes-template-multiple-files (concat
						    "#+TITLE: ${title}\n"
						    "#+ROAM_KEY: cite:${=key=}\n"
						    "* TODO Notes\n"
						    ":PROPERTIES:\n"
						    ":Custom_ID: ${=key=}\n"
						    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
						    ":AUTHOR: ${author-abbrev}\n"
						    ":JOURNAL: ${journaltitle}\n"
						    ":DATE: ${date}\n"
						    ":YEAR: ${year}\n"
						    ":DOI: ${doi}\n"
						    ":URL: ${url}\n"
						    ":END:\n\n"
						    )
   ;; bibtex-completion-display-formats
   ;;     '((article       . "${author:36} ${title:*} ${journal:40} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}")
   ;;       (inbook        . "${author:36} ${title:*} Chapter ${chapter:32} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}")
   ;;       (incollection  . "${author:36} ${title:*} ${booktitle:40} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}")
   ;;       (inproceedings . "${author:36} ${title:*} ${booktitle:40} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}")
   ;;       (t             . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}"))

   ))

;; org-noter
(use-package org-noter
  :after org
  :straight t
  :config
  (setq
   ;; The WM can handle splits
   ;; org-noter-notes-window-location 'other-frame
   ;; todo frame jump
   ;; Please stop opening frames
   ;; org-noter-always-create-frame nil
   org-noter-always-create-frame nil
   org-noter-auto-save-last-location t
   ;; org-noter-notes-window-location 'other-frame
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   ;; org-noter-notes-search-path (list org_notes)
   )
  )


(provide 'init-org-roam)
;;; init-org-roam.el ends here
