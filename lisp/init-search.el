;;; lisp/init-search.el -*- lexical-binding: t; -*-

(use-package color-rg
  :straight (color-rg
	     :host github
	     :repo "manateelazycat/color-rg"
	     )
  ;; :ensure t
  :defer t
  ;; :demand t
  :commands (color-rg-search-input color-rg-search-symbol color-rg-search-project color-rg-search-input-in-project color-rg-search-symbol-with-type color-rg-search-project-with-type)
  :custom
  ;; `color-rg' do not kill any buffer
  (color-rg-kill-temp-buffer-p nil)
  (color-rg-search-compressed-file nil)
  ;; :config
  ;; (add-to-list 'evil-emacs-state-modes 'color-rg-mode)
  ;; :init
  ;; (defconst evil-collection-color-rg-maps '(color-rg-mode-map
  ;; 					      color-rg-mode-edit-map))
  :config
  (advice-add #'color-rg-update-header-line :override #'ignore)
  (defhydra color-rg-hydra (:hint nil)
    "
    ^^^^Move               ^^^^filter                     ^^toggle            ^^change
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
    _n_   next keyword   | _r_   replace all            | _I_  toggle ignore  | _d_  change dir
    _p_   prev keyword   | _f_   filter match result    | _c_  toggle case    | _z_  change globs
    _N_   next file      | _F_   filter mismatch result | _i_  open edit mode | _Z_  change exclude
    _P_   prev file      | _x_   filter match files     | ^^                  | _t_  return literal
    _D_   remove line    | _X_   filter mismatch files  | _u_  unfilter       | _s_  return regexp
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
  "
    ("n" color-rg-jump-next-keyword)
    ("p" color-rg-jump-prev-keyword)
    ("N" color-rg-jump-next-file)
    ("P" color-rg-jump-prev-file)

    ("r" color-rg-replace-all-matches)
    ("f" color-rg-filter-match-results)
    ("F" color-rg-filter-mismatch-results)
    ("x" color-rg-filter-match-files)
    ("X" color-rg-mismatch-files)
    ("u" color-rg-unfilter)
    ("D" color-rg-remove-line-from-results)

    ("I" color-rg-rerun-toggle-ignore)
    ("t" color-rg-rerun-literal)
    ("c" color-rg-rerun-toggle-case)
    ("s" color-rg-rerun-regexp)
    ("d" color-rg-rerun-change-dir)
    ("z" color-rg-rerun-change-globs)
    ("Z" color-rg-rerun-change-exclude-files)
    ("C" color-rg-customized-search)
    ("i" color-rg-switch-to-edit-mode)
    ("q" nil "quit"))
  ;; :general
  ;; (nvmap :keymaps '(color-rg-mode-map)
  ;;   ;; "M-o" #'color-rg-hydra/body
  ;;   ;; Lower keys for commands not operating on all the marked files
  ;;   "RET" #'color-rg-open-file
  ;;   ;; "q" #'quit-window
  ;;   "e" #'color-rg-switch-to-edit-mode
  ;;   "p" #'color-rg-jump-prev-keyword
  ;;   "n" #'color-rg-jump-next-keyword
  ;;   "H" #'color-rg-jump-next-file
  ;;   "L" #'color-rg-jump-prev-file
  ;;   "r" #'color-rg-replace-all-matches
  ;;   "f" #'color-rg-filter-match-results
  ;;   "F" #'color-rg-filter-mismatch-results

  ;;   "x" #'color-rg-filter-match-files
  ;;   "X" #'color-rg-filter-mismatch-files
  ;;   "u" #'color-rg-unfilter

  ;;   "D" #'color-rg-remove-line-from-results

  ;;   "I" #'color-rg-rerun-toggle-ignore
  ;;   "t" #'color-rg-rerun-literal
  ;;   "c" #'color-rg-rerun-toggle-case
  ;;   "s" #'color-rg-rerun-regexp
  ;;   "d" #'color-rg-rerun-change-dir
  ;;   "z" #'color-rg-rerun-change-globs
  ;;   "Z" #'color-rg-rerun-change-exclude-files
  ;;   "C" #'color-rg-customized-search

  ;;   "i" #'color-rg-switch-to-edit-mode
  ;;   "q" #'color-rg-quit
  ;;   "?" #'color-rg-hydra/body
  ;;   )
  ;; (nmap :keymaps '(color-rg-mode-edit-map)
  ;;   "j" #'evil-next-line
  ;;   "k" #'evil-previous-line
  ;;   "h" #'evil-backward-char
  ;;   "l" #'evil-forward-char

  ;;   "n" #'color-rg-jump-next-keyword
  ;;   "p" #'color-rg-jump-prev-keyword
  ;;   "N" #'color-rg-jump-next-file
  ;;   "P" #'color-rg-jump-prev-file)
  ;; (:keymaps '(color-rg-mode-edit-map)
  ;;           [remap evil-write] #'color-rg-apply-changed
  ;; 	    [remap evil-quit] #'color-rg-quit)
  )

(provide 'init-search)
;;; init-search.el ends here
