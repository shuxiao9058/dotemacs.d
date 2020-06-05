;;; lisp/init-org.el -*- lexical-binding: t; -*-

(use-package org
  :straight t
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda () (setq truncate-lines nil)))

  (dolist (face '(org-level-1
                  org-level-2 org-level-3
                  org-level-4 org-level-5
                  org-level-6 org-level-7
                  org-level-8))
    (set-face-attribute face nil :weight 'normal))

  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%A, %e. %B %Y>" . "<%A, %e. %B %Y %H:%M>"))
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  ;; (setq prettify-symbols-unprettify-at-point 'right-edge)
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             "Beautify Org Checkbox Symbol"
  ;;             (push '("[ ]" . "☐") prettify-symbols-alist)
  ;;             (push '("[X]" . "☑") prettify-symbols-alist)
  ;;             (push '("[-]" . "❍") prettify-symbols-alist)
  ;;             (prettify-symbols-mode)))

  (setq org-directory "~/Dropbox/org-mode")

  (setq +org-capture-todo-file (expand-file-name  "todo-list.org" org-directory))
  (setq +org-capture-notes-file (expand-file-name  "notes.org" org-directory))

  (setq org-agenda-files (list ;; "~/org/anniversary.org"
                          (expand-file-name "notes" org-directory)
                          (expand-file-name "todo-list.org" org-directory)
                          ))

  (setq org-mobile-directory (expand-file-name "~/Dropbox/org-mode/org-mobile/"))
  (setq org-mobile-files (list
                          (expand-file-name "todo-list.org" org-mobile-directory)))
  (setq org-mobile-inbox-for-pull (expand-file-name "refile-beorg.org" org-mobile-directory))

  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)
          ;;
          ;;
          ("p" "Templates for projects")
          ("pt" "Project todo" entry ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "TODOs")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Notes")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Changelog")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("j" "Journal" entry
           (file+olp+datetree "~/Dropbox/org-mode/journal.org")
           "* %?\nEntered on %U\n %i\n" :empty-lines 1)))
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
                                  ;; (shell-script "bash")
                                  ;; (gnuplot "Gnuplot")
                                  ;; (ocaml "Caml")
                                  ;; (sql "SQL")
                                  ;; (sqlite "sql")
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
  (setq plantuml-jar-path "/usr/local/bin/plantuml")

  ;; (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

  ;; default article
  (setq org-latex-classes
        '("article" "
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
%\\setmainfont{Monaco}
%\\setsansfont{Monaco}
%\\setmonofont{Monaco}
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
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

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
  ;; (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  ;; (add-to-list 'org-latex-packages-alist '("" "tabu"))
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

  ;; (setq org-modules (append org-modules '(org-drill)))
  )


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
  :straight nil
  :after (org osx)
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
  :config
  ;; (require 'taskjuggler-mode)
  (setq org-export-taskjuggler-target-version 3.6
	org-export-taskjuggler-project-tag "project"
	org-export-taskjuggler-resource-tag "resource"
	org-export-taskjuggler-default-project-duration 16256
	org-export-taskjuggler-default-global-properties "rversion")
  ;; (setq org-taskjuggler-default-reports '("include \"/Users/chenlong/.emacs.d/lisp/reports.tji\""))
  (add-hook 'org-mode-hook (lambda()
			     (require 'ox-taskjuggler)
			     ))
  )

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

(use-package org-clock-budget
  :straight (org-clock-budget
             :host github
             :repo "Fuco1/org-clock-budget"
             )
  :commands (org-clock-budget-report)
  :init
  (defun my-buffer-face-mode-org-clock-budget ()
    "Sets a fixed width (monospace) font in current buffer"
    (interactive)
    ;; (setq buffer-face-mode-face '(:family "Iosevka" :height 1.0))
    (buffer-face-mode)
    (setq-local line-spacing nil))
  :config
  (add-hook 'org-clock-budget-report-mode-hook (lambda()
						 (progn
						   (toggle-truncate-lines 1)
						   (my-buffer-face-mode-org-clock-budget)
						   )
						 ))
  :general
  (nmap :keymaps 'org-clock-budget-report-mode-map
    "h" #'org-shifttab
    "l" #'org-cycle
    "e" #'org-clock-budget-report
    "s" #'org-clock-budget-report-sort
    "d" #'org-clock-budget-remove-budget
    "q" #'quit-window
    )
  )

(provide 'init-org)
;;; init-org.el ends here
