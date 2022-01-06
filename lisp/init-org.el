;;; lisp/init-org.el -*- lexical-binding: t; -*-

(use-package org
    ;; :straight t
    :straight (org-contrib)
    :ensure org-plus-contrib
    :custom
    (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "NEXT(n!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
			 (sequence "MEETING(m)" "HOLD(h!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
			 (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
    (org-todo-keyword-faces
     '(("TODO"       :foreground "#7c7c75" :weight bold)
       ("MEETING"   :foreground "#7c7c75" :weight bold)
       ("HOLD"       :foreground "#feb24c" :weight bold)
       ("NEXT"       :foreground "#0098dd" :weight bold)
       ("WAIT"       :foreground "#9f7efe" :weight bold)
       ("DONE"       :foreground "#50a14f" :weight bold)
       ("CANCELLED"  :foreground "#ff6480" :weight bold)
       ("REPORT"     :foreground "magenta" :weight bold)
       ("BUG"        :foreground "red"     :weight bold)
       ("KNOWNCAUSE" :foreground "yellow"  :weight bold)
       ("FIXED"      :foreground "green"   :weight bold)))
    (org-use-fast-todo-selection 'expert)
    ;; (org-use-fast-todo-selection 'auto)
    (org-enforce-todo-dependencies t)
    (org-enforce-todo-checkbox-dependencies t)
    (org-priority-faces '((?A :foreground "red")
			  (?B :foreground "orange")
			  (?C :foreground "yellow")))
    (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00")
			     ("STYLE_ALL" . "habit")))
    ;; (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
    (org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
    ;; Remove CLOSED: [timestamp] after switching to non-DONE states
    (org-closed-keep-when-no-todo t)

    ;; log
    (org-log-done 'time)
    (org-log-repeat 'time)
    (org-log-redeadline 'note)
    (org-log-reschedule nil)
    (org-log-into-drawer t)
    (org-log-state-notes-insert-after-drawers nil)
    ;; (org-log-states-order-reversed t)

    ;; tags
    (org-tags-column 0)
    (org-fast-tag-selection-single-key t)
    (org-track-ordered-property-with-tag t)

    ;; calendar
    (org-time-stamp-custom-formats '("<%A, %e. %B %Y>" . "<%A, %e. %B %Y %H:%M>"))
    (org-agenda-start-on-weekday 1)
    (calendar-week-start-day 1)
    (org-display-custom-times t)
    (org-confirm-babel-evaluate nil)
    :config
    (add-to-list 'org-modules 'org-capture)
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-timer)
    (add-to-list 'org-modules 'org-protocol)
    ;; (add-to-list 'org-modules 'org-cliplink)
    ;; (add-to-list 'org-modules 'org-journal)
    (add-to-list 'org-modules 'org-agenda)
    (add-to-list 'org-modules 'org-element)
    (add-to-list 'org-modules 'org-bars)
    ;; (add-to-list 'org-modules 'org-pdfview)
    ;; (add-to-list 'org-modules 'org-download)


    (defun my/org-add-ids-to-headlines-in-file ()
      "Add ID properties to all headlines in the current file which
do not already have one."
      (interactive)
      (org-map-entries 'org-id-get-create))
    (add-hook 'org-mode-hook
              (lambda ()
		(add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

    (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

    (add-hook 'org-mode-hook
	      (lambda () (setq truncate-lines nil)))

    (dolist (face '(org-level-1
		    org-level-2 org-level-3
		    org-level-4 org-level-5
		    org-level-6 org-level-7
		    org-level-8))
      (set-face-attribute face nil :weight 'normal))

    ;; (setq prettify-symbols-unprettify-at-point 'right-edge)
    ;; (add-hook 'org-mode-hook
    ;;           (lambda ()
    ;;             "Beautify Org Checkbox Symbol"
    ;;             (push '("[ ]" . "☐") prettify-symbols-alist)
    ;;             (push '("[X]" . "☑") prettify-symbols-alist)
    ;;             (push '("[-]" . "❍") prettify-symbols-alist)
    ;;             (prettify-symbols-mode)))

    (setq org-directory "~/Dropbox/org")

    (setq org-beorg-directory (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/beorg/"))
    (setq +org-capture-gtd-file (expand-file-name  "personal-gtd.org" org-beorg-directory))
    (setq +org-capture-notes-file (expand-file-name  "personal-note.org" org-beorg-directory))
    (setq +org-capture-work-gtd-file (expand-file-name "work-gtd.org" org-beorg-directory))
    (setq +org-capture-work-notes-file (expand-file-name "work-note.org" org-beorg-directory))
    (setq +org-capture-work-meeting-file (expand-file-name "work-meeting.org" org-beorg-directory))
    (setq +org-capture-work-weekly-file (expand-file-name "work-weekly.org" org-beorg-directory))
    (setq org-agenda-files (list
			    +org-capture-gtd-file
			    +org-capture-work-gtd-file
			    +org-capture-work-meeting-file))

    ;; archived location
    (setq org-archive-location (concat org-directory "archive/%s_archive::"))

    (setq org-refile-targets
	  `((org-agenda-files :maxlevel . 2)
	    (,(list +org-capture-notes-file
		    +org-capture-work-notes-file
		    +org-capture-work-weekly-file) :maxlevel . 2)))

    (setq org-tag-alist
	  '(("ignore" . ?i)
	    ("crypt" . ?c)))

    (setq org-capture-templates
	  '(("t" "Personal todo" entry
	     (file+headline +org-capture-gtd-file "P-GTD")
	     "* TODO %?\n%i%U\n" :kill-buffer nil)
	    ("n" "Personal notes" entry
	     (file+headline +org-capture-notes-file "P-Note")
	     "* %u %?\n%i%U\n%a" :kill-buffer nil)

	    ("w" "Templates for work")
	    ("wt" "Work todo" entry
	     (file+olp +org-capture-work-gtd-file "W-GTD")
	     "* TODO %?\n%i%U\n" :kill-buffer nil)
	    ;; "* TODO %T%?\n%i\n:LOGBOOK:\n\n:END:\n" :prepend t :kill-buffer t)
	    ("wm" "Work meeting" entry
	     (file+olp +org-capture-work-meeting-file  "W-Meeting")
	     "* TODO %?\n%i%U\n")
	    ("wn" "Work notes" entry
	     (file+olp +org-capture-work-notes-file  "W-Note")
	     "* %u %?\n%i\n%a" :kill-buffer nil)
	    ("ww" "Work weekly" entry
	     (file+olp +org-capture-work-weekly-file "W-Weekly")
	     "* %U 周汇报\n\n   本周事项：\n\n     - %?\n\n   下周计划：\n\n     -    \n\n%i\n")

	    ("p" "Templates for projects")
	    ("pt" "Project todo" entry ; {project-root}/todo.org
	     (file+headline +org-capture-project-todo-file "Todo")
	     "* TODO %T%?\n%i\n%a" :prepend nil :kill-buffer t)
	    ("pn" "Project notes" entry ; {project-root}/notes.org
	     (file+headline +org-capture-project-notes-file "Note")
	     "* TODO %?\n%i%U\n%a" :prepend nil :kill-buffer t)
	    ("pc" "Project changelog" entry ; {project-root}/changelog.org
	     (file+headline +org-capture-project-notes-file "Changelog")
	     "* TODO %?\n%i%U\n%a" :prepend nil :kill-buffer t)
	    ("j" "Journal" entry
	     (file+olp+datetree (expand-file-name "journal.org" org-beorg-directory))
	     "* %T%?\nEntered on %U\n %i\n" :empty-lines 1)))
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

    ;; minted required:
    ;; 1. xelatex -shell-escape
    ;; 2. pip install pygments
    ;; 3. pip install git+https://github.com/hg2c/terminal-pygments#egg=terminal-pygments
    (setq org-latex-listings 'minted)

    (setq org-latex-minted-langs  '((emacs-lisp "common-lisp")
				    (cc "c++")
				    (cperl "perl")
				    (shell-script "bash")
				    (caml "ocaml")
				    (emacs-lisp "common-lisp")
				    ;; (lisp "common-lisp")
				    ;; (clojure "Lisp")
				    ;; (c "C")
				    ;; (cc "c++")
				    ;; (fortran "fortran")
				    ;; (perl "Perl")
				    ;; (cperl "Perl")
				    ;; (python "Python")
				    ;; (ruby "Ruby")
				    ;; (html "HTML")
				    ;; (xml "XML")
				    ;; (tex "TeX")
				    ;; (latex "TeX")
				    (shell-script "bash")
				    ;; (gnuplot "Gnuplot")
				    ;; (ocaml "Caml")
				    (sql "SQL")
				    (sqlite "sql")
				    ;; (R-mode "R")
				    (go "go")
				    (lua "lua")
				    (shell "shell")
				    (caml "ocaml")
				    (csp "text")
				    ))
    ;; (setq org-latex-minted-options
    ;;       '(;; ("obeytabs" "true")
    ;;         ;; ("mathescape" "true")
    ;;         ("linenos" "false")
    ;;         ;; ;; ("numbersep" "5pt")
    ;;         ;; ;; ("frame" "none")
    ;;         ;; ("frame" "leftline")
    ;;         ;; ;; ("frame" "lines")
    ;;         ;; ("framerule" "0.2pt")
    ;;         ;; ("framesep" "2mm")
    ;;         ;; ;; ("bgcolor" "lgray")
    ;;         ;; ;; ("bgcolor" "mintedbg")
    ;;         ("tabsize" "2")
    ;;         ("fontsize" "\\scriptsize")
    ;;         ;; ;; ("fontsize" "\\scriptsize")
    ;;         ))


    ;; plantuml
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (js . t)
       (latex . t)
       (lilypond . t)
       (octave . t)
       ;; (perl . t)
       (plantuml . t)
       ;; (python . t)
       ;; (ruby . t)
       (shell . t)
       ;; (sqlite . t)
       ;; (R . t)
       ))
    (setq org-plantuml-jar-path (expand-file-name "bin/plantuml.1.2021.5.jar" poly-local-dir))

    ;; (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

    ;; default article
    (setq org-latex-classes
	  '(("article" "
	%!TEX TS-program = xelatex
	%!TEX encoding = UTF-8 Unicode

	\\documentclass[11pt,titlepage,a4paper]{article}
	\\usepackage{ctex}
	\\usepackage[top=3truecm,bottom=2.5truecm,left=1.1truecm,right=1.1truecm,bindingoffset=1.0truecm,
		     headsep=1.6truecm,
		     footskip=1.5truecm,
		     headheight=15pt    % 标准中没有要求页眉的高度，这里设置成15pt了
		     ]{geometry}
	%\\XeTeXlinebreaklocale \"zh\"
	%\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
	%\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
	%\\usepackage[table]{xcolor}
	%\\definecolor{link}{HTML}{0366D6}
	%\\definecolor{lightgray}{rgb}{0.83, 0.83, 0.83}
	%\\definecolor{mintcream}{rgb}{0.96, 1.0, 0.98}
	%\\rowcolors{3}{lightgray!30}{white}
	%\\usepackage{fontspec}
	%\\newfontfamily\\zhfont[BoldFont=PingFang SC]{PingFang SC}
	%\\newfontfamily\\zhpunctfont{PingFang SC}
	%\\setmainfont{PingFang SC}
	%\\setsansfont{Hiragino Sans GB}
	%\\setmonofont[Scale=0.9]{PingFang SC}
	%\\usepackage{zhspacing}
	%\\zhspacing
	%\\usepackage{indentfirst}
	%\\usepackage[table]{xcolor}
	\\usepackage{xcolor}
	%\\definecolor{link}{HTML}{0366D6}
	% \\definecolor{lightgray}{rgb}{0.83, 0.83, 0.83}
	%\\definecolor{mintcream}{rgb}{0.96, 1.0, 0.98}
	%\\rowcolors{3}{lightgray!30}{white}
	\\usepackage{hyperref}
	%\\hypersetup{
	%  colorlinks=true,
	%   linkcolor=link,
	%   citecolor=[rgb]{0,0.47,0.68},
	%   filecolor=link,
	%   urlcolor=link,
	%   pagebackref=true,
	%   linktoc=all,
	% }
	%\\usepackage[outputdir=./build/tex]{minted}
	\\usepackage[utf8]{inputenc}
	\\usepackage{alltt}
	\\usepackage{caption}
	\\usepackage{listings}
	%                        \\usepackage{xcolor}
	\\usepackage{graphicx}
	\\usepackage{lmodern}
	\\DeclareCaptionFormat{listing}{\\rule{\\dimexpr\\textwidth+17pt\\relax}{0.4}\\vskip1pt#1#2#3}
	% \\captionsetup[lstlisting]{singlelinecheck=false, margin=0pt, font={bf,footnotesize}}
	\\definecolor{wine-stain}{rgb}{0.4,0.3,0.3}
	\\hypersetup{colorlinks,linkcolor=wine-stain,anchorcolor=black,linktoc=all,
	citecolor=black}
	[NO-DEFAULT-PACKAGES]
	"

	     ("\\section{%s}" . "\\section*{%s}")
	     ("\\subsection{%s}" . "\\subsection*{%s}")
	     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	     ("\\paragraph{%s}" . "\\paragraph*{%s}")
	     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


    (unless (assoc "beamer" org-latex-classes)
      (add-to-list 'org-latex-classes
		   '("beamer" "
		     %!TEX TS-program = xelatex
		     %!TEX encoding = UTF-8 Unicode

		  \\documentclass[presentation]{beamer}
		     \\usepackage{ctex}
		     "
		     ("\\section{%s}" . "\\section*{%s}")
		     ("\\subsection{%s}" . "\\subsection*{%s}")
		     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;; (add-to-list 'org-export-latex-packages-alist '("" "minted"))


    ;; LaTex
    ;; (add-to-list 'org-latex-packages-alist '("" "listings" nil))
    ;; (setq org-latex-listings t)
    ;; rather do that for each file separately in header!!!
    (setq org-latex-listings-options
	  '(("basicstyle" "\\small")
	    ;; ("keywordstyle" "\\color{black}\\bfseries\\underbar")
	    ("basicstyle" "\\footnotesize")
	    ("breakatwhitespace" "false")
	    ("breaklines" "true")
	    ("captionpos" "b")
	    ("deletekeywords" "{...}")
	    ("escapeinside" "{\\%*}{*)}")
	    ("extendedchars" "true")
	    ("frame" "single")
	    ("keepspaces" "true")
	    ("keywordstyle" "\\color{blue}")
	    ("otherkeywords" "{*,...}")
	    ("numbers" "left")
	    ("numbersep" "5pt")
	    ("numberstyle" "\\tiny\\color{black}")
	    ("rulecolor" "\\color{black}")
	    ("showspaces" "false")
	    ("showstringspaces" "false")
	    ("showtabs" "false")
	    ("stepnumber" "1")
	    ("tabsize" "2")))
    ;; (setq org-latex-listings-options '(("breaklines" "true")
    ;;                                    ("numberstyle" "\\tiny\\color{black}")
    ;;                                    ))

    ;; ;; letter
    ;; (add-to-list 'org-latex-classes
    ;;              '("letter"
    ;;                "\\documentclass[11pt]{letter}\n
    ;;                   \\usepackage[utf8]{inputenc}\n
    ;;                   \\usepackage[T1]{fontenc}\n
    ;;                   \\usepackage{color}"

    ;;                ("\\section{%s}" . "\\section*{%s}")
    ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


    ;; (add-to-list 'org-latex-packages-alist '("" "ctex"))
    ;; ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;; (add-to-list 'org-latex-packages-alist '("" "color"))
    ;; (add-to-list 'org-latex-packages-alist '("" "geometry"))
    (add-to-list 'org-latex-packages-alist '("" "tabularx"))
    (add-to-list 'org-latex-packages-alist '("" "tabu"))
    (setq    org-latex-default-table-environment "tabu")
    ;; (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
    ;; (add-to-list 'org-latex-packages-alist '("" "natbib"))
    ;; (add-to-list 'org-latex-packages-alist '("" "titlesec"))

    ;; code snippet comes from
    ;; ;; http://joat-programmer.blogspot.com/2013/07/org-mode-version-8-and-pdf-export-with.html
    ;; ;; Include the latex-exporter
    ;; ;; check whether org-mode 8.x is available
    ;; (when (require 'ox-latex nil 'noerror)
    ;;   ;; You need to install pygments to use minted
    ;;   (when (executable-find "pygmentize")
    ;;     ;; Add minted to the defaults packages to include when exporting.
    ;;     (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;;     ;; (add-to-list 'org-latex-minted-langs '(calc "mathematica"))

    ;;     ;; Tell the latex export to use the minted package for source
    ;;     ;; code coloration.
    ;;     (setq org-latex-listings 'minted)

    ;;     ;; ;; Let the exporter use the -shell-escape option to let latex
    ;;     ;; ;; execute external programs.
    ;;     ;; ;; This obviously and can be dangerous to activate!
    ;;     ;; (setq org-latex-minted-options
    ;;     ;;       '(;; ("obeytabs" "true")
    ;;     ;;         ;; ("mathescape" "true")
    ;;     ;;         ("linenos" "false")
    ;;     ;;         ;; ;; ("numbersep" "5pt")
    ;;     ;;         ;; ;; ("frame" "none")
    ;;     ;;         ;; ("frame" "leftline")
    ;;     ;;         ;; ;; ("frame" "lines")
    ;;     ;;         ;; ("framerule" "0.2pt")
    ;;     ;;         ;; ("framesep" "2mm")
    ;;     ;;         ;; ;; ("bgcolor" "lgray")
    ;;     ;;         ;; ;; ("bgcolor" "mintedbg")
    ;;     ;;         ("tabsize" "2")
    ;;     ;;         ("fontsize" "\\scriptsize")
    ;;     ;;         ;; ;; ("fontsize" "\\scriptsize")
    ;;     ;;         ))
    ;;     ;; (setq org-latex-pdf-process
    ;;     ;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;     ;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;     ;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;     ;;         "rm -fr %b.out %b.log %b.tex auto"
    ;;     ;;         ))
    ;;     ))
    ;;
    ;;

    ;; (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
    ;;				  "xelatex -interaction nonstopmode %f"))
    ;; (setq org-latex-pdf-process
    ;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;         "rm -fr %b.out %b.log %b.tex auto"
    ;;         ))

    ;; (setq org-latex-default-packages-alist
    ;;	  (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

    (setq
     ;; org-latex-caption-above nil ;; 表格等标题置于下方
     ;; org-export-latex-listings t
     ;; org-export-latex-tables-column-borders t ;; 表格边框
     org-latex-image-default-width "0.5\\textwidth"
     org-latex-toc-command "\\tableofcontents\n\\clearpage\n" ;; 目录自动分页
     ;; org-latex-pdf-process  '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
     org-latex-pdf-process
     '("xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
       "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
       "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f")

     ;; org-latex-pdf-process '("/Library/TeX/texbin/latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -f %f")

     ;; org-latex-pdf-process   '("/Library/TeX/texbin/latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")
     ;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))
     ;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvips=q/dvips -Ppdf -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %S %D/' -norc -gg -pdfps %f"))
     ;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/platex-ng %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
     ;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/pdflatex %S/' -e '$bibtex=q/bibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/makeindex -o %D %S/' -norc -gg -pdf %f"))
     ;; org-latex-pdf-process
     ;; '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f")
     ;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/luajitlatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
     ;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/xelatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
     org-file-apps
     '(("pdf" . "/usr/bin/open -a PDFGuru\\ Pro.app %s"))
     ;; org-latex-toc-command "\\tableofcontents\\newpage"
     ;; org-latex-pdf-process
     ;;    '("xelatex -shell-escape -interaction nonstopmode %f"
     ;;      "xelatex -shell-escape -interaction nonstopmode %f"
     ;;      "xelatex -shell-escape -interaction nonstopmode %f")
     ;; org-latex-pdf-process
     ;; '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     ;;   "rm -fr %b.out %b.log %b.tex auto")
     ;; org-latex-pdf-process '(
     ;;                         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     ;;                         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     ;;                         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     ;;                         "rm -fr %b.out %b.log %b.tex auto"
     ;;                         )
     org-latex-default-class "article"
     )
    ;; ;; auto save all org files after doing a common action
    ;; (advice-add 'org-agenda-quit      :before #'org-save-all-org-buffers)
    ;; ;; (advice-add 'org-agenda-schedule  :after #'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-todo      :after #'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-refile    :after #'org-save-all-org-buffers)
    ;; (advice-add 'org-agenda-clock-in  :after #'org-save-all-org-buffers)
    ;; ;; (advice-add 'org-agenda-clock-out :after #'org-save-all-org-buffers)

    ;; ;; (advice-add 'org-deadline         :after #'org-save-all-org-buffers)
    ;; ;; (advice-add 'org-schedule         :after #'org-save-all-org-buffers)
    ;; ;; (advice-remove 'org-schedule  #'org-save-all-org-buffers)

    ;; (advice-add 'org-todo             :after #'org-save-all-org-buffers)
    ;; (advice-add 'org-refile           :after #'org-save-all-org-buffers)
    ;; ;; (advice-add 'org-clock-in         :after #'org-save-all-org-buffers)
    ;; ;; (advice-add 'org-clock-out        :after #'org-save-all-org-buffers)
    ;; (advice-add 'org-store-log-note   :after #'org-save-all-org-buffers)

    ;; (advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-refile         :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-clock-in       :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-clock-out      :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-agenda-todo    :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-agenda-refile  :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-agenda-clock-in :after (η #'org-save-all-org-buffers))
    ;; (advice-add 'org-agenda-quit :after (η #'org-save-all-org-buffers))
    ;; (setq org-modules (append org-modules '(org-drill)))
    (dolist (command '(org-agenda-archive
		       org-agenda-archive-default
		       org-sort-entries
		       org-roam-refile
		       org-roam-extract-subtree
		       org-agenda-quit
		       org-agenda-todo
		       org-agenda-refile
		       org-agenda-clock-in
		       org-agenda-clock-out
		       org-deadline
		       org-schedule
		       org-todo
		       org-refile
		       org-clock-in
		       org-clock-out
		       org-clock-report
		       org-clock-cancel
		       org-archive-subtree
		       org-archive-subtree-default
		       org-agenda-set-effort
		       org-cut-special
		       org-pomodoro))
      (advice-add command :after (η #'org-save-all-org-buffers))
      ;; (advice-add command :after  #'org-save-all-org-buffers)
      )
    :bind
    ("C-c l" . org-store-link)
    ("C-c c" . org-capture)
    ("C-c a" . org-agenda))

(use-package org-agenda
    :ensure nil
    :straight nil
    :after (org hydra)
    :hook (org-agenda-finalize . org-agenda-to-appt)
    :config
    ;; ;; update appt list per 10 minutes
    ;; (run-at-time nil 600 'org-agenda-to-appt)
    :init
    (unless (fboundp 'native-comp-available-p)
      ;; Fix `void-function native-comp-available-p`
      (defun native-comp-available-p ()
	nil))
    :custom
    ;; appt
    (appt-display-format 'window)
    (appt-disp-window-function
     (lambda(min-to-app new-time msg)(terminal-notify "Reminder" (format "%s" msg))))
    (appt-display-interval 1) ;; 每过1分钟提醒一次
    (appt-message-warning-time 5) ;; set appt waring to 15 minutes prior to appointment)
    ;; (appt-display-duration 20) ;; 这里已经被notify-send接管了，所以此处持续时间无效)
    (appt-display-mode-line t) ;; show in the modeline
    ;; (org-agenda-files `(,org-directory))
    (org-agenda-insert-diary-extract-time t)
    (org-agenda-compact-blocks t)
    (org-agenda-block-separator nil)
    (org-agenda-sticky t)
    ;; Do not dim blocked tasks
    (org-agenda-dim-blocked-tasks nil)
    ;; Compact the block agenda view
    (org-agenda-compact-blocks t)
    ;; holidays
    (org-agenda-include-diary t)
    (org-agenda-include-deadlines t)
    (org-agenda-todo-ignore-deadlines nil)
    (org-agenda-follow-indirect t)
    (org-agenda-inhibit-startup t)
    (org-agenda-show-all-dates t)
    (org-agenda-time-leading-zero t)
    (org-agenda-start-with-log-mode t)
    (org-agenda-start-with-clockreport-mode t)
    (org-agenda-hide-tags-regexp ":\\w+:")
    (org-agenda-todo-ignore-with-date nil)
    (org-agenda-todo-ignore-deadlines 'far)
    ;; (org-agenda-todo-ignore-scheduled 'all)
    (org-agenda-todo-ignore-timestamp nil)
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-skip-scheduled-if-done t)
    (org-agenda-skip-timestamp-if-done t)
    (org-agenda-skip-unavailable-files t)
    (org-agenda-skip-scheduled-delay-if-deadline t)
    (org-agenda-skip-scheduled-if-deadline-is-shown t)
    (org-agenda-skip-additional-timestamps-same-entry t)
    (org-agenda-text-search-extra-files '(agenda-archives))
    (org-agenda-clockreport-parameter-plist
     '(:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80))
    (org-agenda-columns-add-appointments-to-effort-sum t)
    (org-agenda-restore-windows-after-quit t)
    (org-agenda-window-setup 'current-window)
    ;; starts from Monday
    (org-agenda-start-on-weekday 1)
    (org-agenda-use-time-grid t)
    (org-agenda-timegrid-use-ampm nil)
    (org-agenda-search-headline-for-time nil)
    (org-agenda-prefix-format
     '((agenda . " %i %-12:c%?-12t% s%b")
       (todo . " %i %b")
       ;; (todo . "[%-4e] % t % s %?-17b")
       ;;(tags . "[%-4e] %-17(org-format-outline-path (org-get-outline-path))")
       ;; (search . "[%-4e] %?-17b")
       )
     )
    (org-agenda-custom-commands `(
				  ("1" "Events" agenda "display deadlines and exclude scheduled"
				       ((org-agenda-span 'month)
					(org-agenda-time-grid nil)
					(org-agenda-show-all-dates nil)
					(org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
					(org-deadline-warning-days 0)))
				  ("2" "Show Work GTD & meeting appointment" todo ""
				       ((org-agenda-files '(,+org-capture-work-gtd-file
							    ,+org-capture-work-meeting-file))))))
    :config
    (appt-activate 1)
    ;; (org-agenda-to-appt)
    )

;; overwrite built-in function (proviError running timer appt-delete-window': (error "No buffer named *appt-buf*")de 'init-org)
(defun appt-delete-window ()
  "Nothing.Overwrite built-in function."
  )

;; Write codes in org-mode
(use-package org-src
    :straight nil
    :ensure nil
    :after org
    :hook (org-babel-after-execute . org-redisplay-inline-images)
    :bind (:map org-src-mode-map
		;; consistent with separedit/magit
		("C-c C-c" . org-edit-src-exit))
    :custom
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    (org-src-preserve-indentation t)
    (org-src-window-setup 'current-window)
    (org-confirm-babel-evaluate nil)
    (org-edit-src-content-indentation 0)
    (org-src-lang-modes '(("C"      . c)
			  ("C++"    . c++)
			  ("bash"   . sh)
			  ("cpp"    . c++)
			  ("dot"    . graphviz-dot)
			  ("elisp"  . emacs-lisp)
			  ("ocaml"  . tuareg)
			  ("shell"  . sh)
			  ("sqlite" . sql)))
    (org-babel-load-languages '((awk        . t)
				(C          . t)
				(calc       . t)
				(dot        . t)
				(emacs-lisp . t)
				(eshell     . t)
				(gnuplot    . t)
				(ocaml      . t)
				(python     . t)
				(shell      . t)
				(sql        . t))))

(use-package org-habit
    :straight nil
    :ensure nil
    :after org
    :custom
    (org-habit-show-habits t)
    (org-habit-show-all-today t))

(use-package ox-hugo
    :straight t
    :after ox
    :defer t
    :config
    (setq org-hugo-export-with-section-numbers nil)
    ;;  (add-hook! org-capture-before-finalize #'+cole/org-capture--remove-auto-org-to-hugo-export-maybe)
    ;; (add-hook! org-capture-after-finalize #'+cole/org-capture--add-auto-org-to-hugo-export-maybe)
    )

(use-package ox-taskjuggler
    :straight (:type built-in)
    :after (org osx)
    :custom
    (org-taskjuggler-process-command  "tj3 --silent --no-color --output-dir %o %f && open %o/Plan.html")
    :init
    (setq org-taskjuggler-default-global-properties
	  "shift s39 \"Full time shift\" {
	   workinghours mon-fri 9:00-12:00,13:00-19:00
	}")
    (setq org-duration-units `(("min" . 1)
			       ("h" . 60)
			       ("d" . ,(* 60 8))
			       ("w" . ,(* 60 8 5))
			       ("m" . ,(* 60 8 5 4))
			       ("y" . ,(* 60 8 5 4 10))))
    (org-duration-set-regexps)
    :hook (org-mode . (lambda()
			(require 'ox-taskjuggler)))
    :config
    (add-to-list 'org-export-backends 'taskjuggler)
    ;; (require 'ox-taskjuggler)
    ;; (require 'ox-taskjuggler)
    ;; (require 'taskjuggler-mode)
    ;; (setq org-export-taskjuggler-target-version 3.6
    ;;	org-export-taskjuggler-project-tag "project"
    ;;	org-export-taskjuggler-resource-tag "resource"
    ;;	org-export-taskjuggler-default-project-duration 16256
    ;;	org-export-taskjuggler-default-global-properties "rversion")
    ;; ;; (setq org-taskjuggler-default-reports '("include \"/Users/chenlong/.emacs.d/lisp/reports.tji\""))
    )

;; (use-package tj3-mode
;;   :straight t
;;   :ensure t
;;   :after (org-plus-contrib)
;;   :config
;;   (require 'ox-taskjuggler)
;;   (custom-set-variables
;;    '(org-taskjuggler-process-command "/usr/local/bin/tj3 --silent --no-color --output-dir %o %f")
;;    '(org-taskjuggler-project-tag "PRJ")))

(use-package org-bullets
    :straight t
    :commands (org-bullets-mode org-bullets)
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("⊢" "⋮" "⋱" "⋱" "⋱"))
    ;; (setq org-bullets-bullet-list '("🐳" "🐬" "🐠" "🐟" "🐤"))
    ;; (setq )
    ;; (setq org-bullets-bullet-list '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨"))
    )

(use-package org-kanban
    :straight t
    :commands (org-kanban/initialize-at-end)
    ;; :ensure t
    ;; :defer t
    :after org
    :config
    (setq-default org-kanban/layout '("..." . 30))
    )

(use-package org-crypt
    :straight nil
    :after org
    :config
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; (setq org-crypt-disable-auto-save 'encrypt)
    (setq org-crypt-disable-auto-save t)
    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    (setq org-crypt-key "FC6BDB92CD5BEB22")
    :bind
    (:map org-mode-map
	  ("C-c e" . org-encrypt-entry)
	  ("C-c E" . org-encrypt-entries)
	  ("C-c d" . org-decrypt-entry)
	  ("C-c D" . org-decrypt-entries)
	  ;; ("C-c I" . org-insert-epa-file-local-variables)
	  ))

(use-package ob
    :straight nil
    :after org
    :config
    (require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider)
    )

(use-package org-dashboard
    :straight t
    :after org
    )

;; (defun org-insert-epa-file-local-variables ()
;;   (interactive)
;;   (add-file-local-variable-prop-line 'mode 'org)
;;   (add-file-local-variable-prop-line 'epa-file-encrypt-to
;;                                      (progn
;;                                        (when (not epa-file-encrypt-to)
;;                                          (epa-file-select-keys))
;;                                        epa-file-encrypt-to)))


(defun org-unlinkify ()
  "Replace an org-link with the path, or description."
  (interactive)
  (let ((eop (org-element-context)))
    (when (eq (org-element-type eop) 'link)
      (save-excursion
	(let* ((start (org-element-property :begin eop))
	       (end (org-element-property :end eop))
	       (contents-begin (org-element-property :contents-begin eop))
	       (contents-end (org-element-property :contents-end eop))
	       (path (org-element-property :path eop))
	       (desc (and contents-begin
			  contents-end
			  (buffer-substring contents-begin contents-end))))
	  (setf (buffer-substring start end) (or desc path)))))))

(defalias 'org-delinkify 'org-unlinkify)

(defun org-refile-and-link ()
  "Refile heading, adding a link to the new location.
Prefix arguments are interpreted by `org-refile'."
  (interactive)
  (when (member current-prefix-arg '(3 (4) (16)))
    (user-error "Linking is incompatible with that prefix argument"))
  (let ((heading  (org-get-heading t t))
	(orig-file (buffer-file-name)))
    (call-interactively #'org-refile)
    (let* ((refile-file
	    (bookmark-get-filename
	     (assoc (plist-get org-bookmark-names-plist :last-refile)
		    bookmark-alist)))
	   (same-file (string= orig-file refile-file))
	   (link (if same-file
		     (concat "*" heading)
		   (concat refile-file "::*" heading)))
	   (desc heading))
      (open-line 1)
      (insert (org-make-link-string link desc)))))

(defun my-org-insert-last-stored-link (arg)
  "Insert the last link stored in `org-stored-links' like
`org-insert-last-stored-link', but without a trailing newline."
  (interactive "p")
  (org-insert-all-links arg "" ""))

(bind-key [remap org-insert-last-stored-link]  #'my-org-insert-last-stored-link 'org-mode-map)


(use-package org-present
    :straight t
    :after org
    :config
    (add-hook 'org-present-mode-hook
              (lambda ()
		(org-present-big)
		(org-display-inline-images)
		(global-linum-mode -1)
		(global-hl-line-mode -1)))
    (add-hook 'org-present-mode-quit-hook
              (lambda ()
		(org-present-small)
		(org-remove-inline-images)
		(global-linum-mode)
		(global-hl-line-mode 1))))

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

(use-package mixed-pitch
    :straight t
    :hook
    (text-mode . mixed-pitch-mode))

(use-package org-analyzer
    :straight t
    :after org
    :custom
    (org-analyzer-org-directory org-beorg-directory))

(provide 'init-org)
;;; init-org.el ends here
