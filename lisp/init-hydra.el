;;; lisp/init-hydra.el -*- lexical-binding: t; -*-

;; (defun zenith/lv-window (fun)
;;   (with-selected-window (funcall fun)
;;     (setq-local header-line-format nil))
;;   lv-wnd)

(use-package hydra
  :straight t
  :ensure t
  ;; :after outline
  :custom
  (hydra-if-helpful t)
  :commands (defhydra)
  :bind ("M-o" . hydra-base/body))

;; (use-package hydra-posframe
;;   :straight (hydra-posframe
;;              :host github
;;              :repo "Ladicle/hydra-posframe"
;;              )
;;   :defer t
;;   :after (hydra posframe)
;; :config
;; (hydra-posframe-enable)
;;   )

(defhydra hydra-base ()
  "
_a_genda
_e_in
_o_utline & outshine
_s_traight
_t_ab
_w_indow
"
  ("a" hydra-agenda-view/body :exit t)
  ("d" dumb-jump-hydra/body :exit t)
  ("w" hydra-window/body :exit t)
  ("o" hydra-outline/body :exit t)
  ("s" hydra-straight/body :exit t)
  ("t" hydra-tab/body :exit t)
  ("e" hydra-ein/body :exit t)
  )

(defhydra hydra-straight (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("q" nil))

(defhydra hydra-window ()
  "
Movement^^        ^Split^         ^Switch^    ^Resize^
----------------------------------------------------------------
_h_ ←         _v_ertical      _b_uffer    _q_ ←→ shrink
_j_ ↓         _x_ horizontal  _f_ind files  _w_ ←→ grow
_k_ ↑         _z_ undo        _a_ce 1   _e_ ↑↓ shrink
_l_ →         _Z_ reset       _s_wap    _r_ ↑↓ grow
_F_ollow       _D_lt Other     _S_ave    max_i_mize
_SPC_ cancel  _o_nly this     _d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" shrink-window-horizontally)
  ("w" enlarge-window-horizontally)
  ("e" shrink-window)
  ("r" enlarge-window)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   )
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))


;; (defhydra hydra-git-gutter (:body-pre (git-gutter+-mode 1)
;; 				      :hint nil)
;;   "
;; Git gutter:
;;   _j_: next hunk        _s_tage hunk     _q_uit
;;   _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
;;   ^ ^                   _p_opup hunk
;;   _h_: first hunk
;;   _l_: last hunk
;; "
;;   ;; set start _R_evision
;;   ("j" git-gutter+:next-hunk)
;;   ("k" git-gutter+:previous-hunk)
;;   ("h" (progn (goto-char (point-min))
;;               (git-gutter+:next-hunk 1)))
;;   ("l" (progn (goto-char (point-min))
;;               (git-gutter+:previous-hunk 1)))
;;   ("s" git-gutter+:stage-hunks)
;;   ("r" git-gutter+:revert-hunks)
;;   ("p" git-gutter+:popup-hunk)
;;   ;;("R" git-gutter:set-start-revision)
;;   ("q" nil :color blue)
;;   ("Q" (progn (git-gutter+-mode -1)
;;               ;; git-gutter-fringe doesn't seem to
;;               ;; clear the markup right away
;;               (sit-for 0.1)
;;               ;;(git-gutter:clear)
;;               )
;;    :color blue)
;;   )

;; from https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/
(defhydra hydra-outline (:color blue :hint nil)
  "
    ^Hide^             ^Show^           ^Move
    ^^^^^^------------------------------------------------------
    _q_: sublevels     _a_: all         _u_: up
    _t_: body          _e_: entry       _n_: next visible
    _o_: other         _i_: children    _p_: previous visible
    _c_: entry         _k_: branches    _f_: forward same level
    _l_: leaves        _s_: subtree     _b_: backward same level
    _d_: subtree
    "
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave")
  )

(defhydra hydra-ein (:hint nil)
  "
 Operations on Cells^^^^^^            On Worksheets^^^^              Other
 ----------------------------^^^^^^   ------------------------^^^^   ----------------------------------^^^^
 [_k_/_j_]^^     select prev/next     [_h_/_l_]   select prev/next   [_t_]^^         toggle output
 [_K_/_J_]^^     move up/down         [_H_/_L_]   move left/right    [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_1_.._9_]  open [1st..last]   [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_+_/_-_]   create/delete      [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           ^^^^                           [_x_]^^         close notebook
 [_u_]^^^^       change type          ^^^^                           [_q_]^^         quit transient-state
 [_RET_]^^^^     execute"

  ("q" nil :exit t)
  ;; ("?" spacemacs//ipython-notebook-ms-toggle-doc)
  ("h" ein:notebook-worksheet-open-prev-or-last)
  ("j" ein:worksheet-goto-next-input)
  ("k" ein:worksheet-goto-prev-input)
  ("l" ein:notebook-worksheet-open-next-or-first)
  ("H" ein:notebook-worksheet-move-prev)
  ("J" ein:worksheet-move-cell-down)
  ("K" ein:worksheet-move-cell-up)
  ("L" ein:notebook-worksheet-move-next)
  ("t" ein:worksheet-toggle-output)
  ("d" ein:worksheet-kill-cell)
  ("R" ein:worksheet-rename-sheet)
  ("y" ein:worksheet-copy-cell)
  ("p" ein:worksheet-yank-cell)
  ("o" ein:worksheet-insert-cell-below)
  ("O" ein:worksheet-insert-cell-above)
  ("u" ein:worksheet-change-cell-type)
  ("RET" ein:worksheet-execute-cell-and-goto-next)
  ;; Output
  ("C-l" ein:worksheet-clear-output)
  ("C-S-l" ein:worksheet-clear-all-output)
  ;;Console
  ("C-o" ein:console-open)
  ;; Merge and split cells
  ("C-k" ein:worksheet-merge-cell)
  ("C-j"
   (lambda ()
     (interactive)
     (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t)))
  ("s" ein:worksheet-split-cell-at-point)
  ;; Notebook
  ("C-s" ein:notebook-save-notebook-command)
  ("C-r" ein:notebook-rename-command)
  ("1" ein:notebook-worksheet-open-1th)
  ("2" ein:notebook-worksheet-open-2th)
  ("3" ein:notebook-worksheet-open-3th)
  ("4" ein:notebook-worksheet-open-4th)
  ("5" ein:notebook-worksheet-open-5th)
  ("6" ein:notebook-worksheet-open-6th)
  ("7" ein:notebook-worksheet-open-7th)
  ("8" ein:notebook-worksheet-open-8th)
  ("9" ein:notebook-worksheet-open-last)
  ("+" ein:notebook-worksheet-insert-next)
  ("-" ein:notebook-worksheet-delete)
  ("x" ein:notebook-close))

;; keymap https://github.com/Timidger/dotfiles/blob/master/.emacs.d/layers/+emacs/org/packages.el
(defhydra hydra-agenda-view (:hint nil)
  "
Headline^^            Visit entry^^               Filter^^                    Date^^               Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------  -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule      [_tf_] follow        [_vd_] day         [_ci_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dd_] set deadline  [_tl_] log           [_vw_] week        [_co_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dt_] timestamp     [_ta_] archive       [_vt_] fortnight   [_ck_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_+_]  do later      [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_hT_] set tags       ^^                          [_fx_] by regexp            [_-_]  do earlier    [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   ^^                   ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
  ;; Entry
  ("ht" org-agenda-todo)
  ("hk" org-agenda-kill)
  ("hr" org-agenda-refile)
  ("hA" org-agenda-archive-default)
  ("hT" org-agenda-set-tags)
  ("hp" org-agenda-priority)

  ;; Visit entry
  ("SPC" org-agenda-show-and-scroll-up)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("RET" org-agenda-switch-to :exit t)
  ("o"   link-hint-open-link :exit t)

  ;; Date
  ("ds" org-agenda-schedule)
  ("dd" org-agenda-deadline)
  ("dt" org-agenda-date-prompt)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)

  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)

  ;; Toggle mode
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("ta" org-agenda-archives-mode)
  ("tr" org-agenda-clockreport-mode)
  ("td" org-agenda-toggle-diary)

  ;; Filter
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fc" org-agenda-filter-by-category)
  ("fh" org-agenda-filter-by-top-headline)
  ("fx" org-agenda-filter-by-regexp)
  ("fd" org-agenda-filter-remove-all)

  ;; Clock
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ("ck" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)

  ;; Other
  ("q" nil :exit t)
  ("gr" org-agenda-redo)
  ("." org-agenda-goto-today)
  ("gd" org-agenda-goto-date))

(defhydra dumb-jump-hydra (:color blue :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

;; (defhydra hydra-clock (:color blue)
;;     "
;;     ^
;;     ^Clock^             ^Do^
;;     ^─────^─────────────^──^─────────
;;     _q_ quit            _c_ cancel
;;     ^^                  _d_ display
;;     ^^                  _e_ effort
;;     ^^                  _i_ in
;;     ^^                  _j_ jump
;;     ^^                  _o_ out
;;     ^^                  _r_ report
;;     ^^                  ^^
;;     "
;;     ("q" nil)
;;     ("c" org-clock-cancel :color pink)
;;     ("d" org-clock-display)
;;     ("e" org-clock-modify-effort-estimate)
;;     ("i" org-clock-in)
;;     ("j" org-clock-goto)
;;     ("o" org-clock-out)
;;     ("r" org-clock-report)
;;   )

;; (defhydra hydra-straight-helper (:hint nil)
;;   "
;; _c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
;; _C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
;; ----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
;; _r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
;; _R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
;;   ("c" straight-check-all)
;;   ("C" straight-check-package)
;;   ("r" straight-rebuild-all)
;;   ("R" straight-rebuild-package)
;;   ("f" straight-fetch-all)
;;   ("F" straight-fetch-package)
;;   ("p" straight-pull-all)
;;   ("P" straight-pull-package)
;;   ("m" straight-merge-all)
;;   ("M" straight-merge-package)
;;   ("n" straight-normalize-all)
;;   ("N" straight-normalize-package)
;;   ("u" straight-push-all)
;;   ("U" straight-push-package)
;;   ("v" straight-freeze-versions)
;;   ("V" straight-thaw-versions)
;;   ("w" straight-watcher-start)
;;   ("W" straight-watcher-quit)
;;   ("g" straight-get-recipe)
;;   ("e" straight-prune-build)
;;   ("q" nil))


;; (defhydra sm/smerge-hydra
;;     (:color pink :hint nil :post (smerge-auto-leave))
;;   "
;; ^Move^       ^Keep^               ^Diff^                 ^Other^
;; ^^-----------^^-------------------^^---------------------^^-------
;; _n_ext       _b_ase               _<_: upper/base        _C_ombine
;; _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;; ^^           _l_ower              _>_: base/lower        _k_ill current
;; ^^           _a_ll                _R_efine
;; ^^           _RET_: current       _E_diff
;; "
;;   ("n" smerge-next)
;;   ("p" smerge-prev)
;;   ("b" smerge-keep-base)
;;   ("u" smerge-keep-upper)
;;   ("l" smerge-keep-lower)
;;   ("a" smerge-keep-all)
;;   ("RET" smerge-keep-current)
;;   ("\C-m" smerge-keep-current)
;;   ("<" smerge-diff-base-upper)
;;   ("=" smerge-diff-upper-lower)
;;   (">" smerge-diff-base-lower)
;;   ("R" smerge-refine)
;;   ("E" smerge-ediff)
;;   ("C" smerge-combine-with-next)
;;   ("r" smerge-resolve)
;;   ("k" smerge-kill-current)
;;   ("ZZ" (lambda ()
;;           (interactive)
;;           (save-buffer)
;;           (bury-buffer))
;; 	"Save and bury buffer" :color blue)
;;   ("q" nil "cancel" :color blue))


(defhydra hydra-smerge (:color pink
                               :hint nil
                               :pre (unless smerge-mode (smerge-mode +1))
                               :post (smerge-auto-leave))
  "
                                                         [smerge]
^Move^       ^Keep^               ^Diff^                 ^Other^
  ╭─────────────────────────────────────────────────────────╯
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _m_ine              _=_: upper/lower       _r_esolve
_C-k_        _o_ther             _>_: base/lower        _R_move
_k_ ↑       _a_ll                _R_efine
_j_ ↓       _RET_: current       _E_diff
_C-j_
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("C-j" smerge-next)
  ("C-k" smerge-prev)
  ("j" next-line)
  ("k" previous-line)
  ("b" smerge-keep-base)
  ("m" smerge-keep-upper) ;; keep mine
  ("o" smerge-keep-lower) ;; keep other
  ;; ("u" smerge-keep-upper)
  ;; ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("H" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("R" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(defhydra hydra-tab (:color red :hint nil)
  "
                                                ^tab^
-------^^-----------------------------^^--------------------------------^^-----------------------^^-------------------
    ^Switch^                        ^Move^                        ^Create & Kill^              ^Other^
_h_:       left tab              _<_: tab to left               _n_: new tab                  _rr_: rename
_l_:       right tab             _>_: tab to right              _N_: new tab with name        _rp_: rename default
[_1_.._9_]: switch [1st..last]     [_m1_..._9_]: move [1st..last]   _x_: kill                     _U_ : undo
                                                                                        _R_ : redo
"

  ;; ("u" winner-undo)
  ;; ;; doesn't work
  ;; ;; ("C-r" winner-redo)

  ;; ;; tab-bar-mode (Emacs 27)
  ;; ;; `awesome-tab`: https://github.com/manateelazycat/awesome-tab
  ("h"  #'tab-bar-switch-to-prev-tab)
  ("l"  #'tab-bar-switch-to-next-tab)
  ("<"  #'toy/tab-move-left)
  (">"  #'toy/tab-move-right)

  ;; ;; FIXME:
  ;; ;; ("w" #'toy/hydra-window/body)
  ;; ("w" (lambda () (interactive) (hydra-disable)
  ;;           (toy/hydra-window/body)))

  ("rr" #'tab-bar-rename-tab)
  ;; rename to project name
  ("rp" #'toy/set-tab-name-default) ;; NOTE: defined in `ide.el`

  ("n" #'tab-bar-new-tab)
  ;; new tab and set name
  ("N" (lambda () (interactive)
	 (tab-bar-new-tab)
	 (call-interactively 'tab-bar-rename-tab)))
  ("x" #'tab-bar-close-tab)

  ;; select tab
  ("1" (lambda () (interactive) (tab-bar-select-tab 1)))
  ("2" (lambda () (interactive) (tab-bar-select-tab 2)))
  ("3" (lambda () (interactive) (tab-bar-select-tab 3)))
  ("4" (lambda () (interactive) (tab-bar-select-tab 4)))
  ("5" (lambda () (interactive) (tab-bar-select-tab 5)))
  ("6" (lambda () (interactive) (tab-bar-select-tab 6)))
  ("7" (lambda () (interactive) (tab-bar-select-tab 7)))
  ("8" (lambda () (interactive) (tab-bar-select-tab 8)))
  ("9" (lambda () (interactive) (tab-bar-select-tab 9)))

  ;; move tab
  ("m1" (lambda () (interactive) (tab-bar-move-tab-to 1)))
  ("m2" (lambda () (interactive) (tab-bar-move-tab-to 2)))
  ("m3" (lambda () (interactive) (tab-bar-move-tab-to 3)))
  ("m4" (lambda () (interactive) (tab-bar-move-tab-to 4)))
  ("m5" (lambda () (interactive) (tab-bar-move-tab-to 5)))
  ("m6" (lambda () (interactive) (tab-bar-move-tab-to 6)))
  ("m7" (lambda () (interactive) (tab-bar-move-tab-to 7)))
  ("m8" (lambda () (interactive) (tab-bar-move-tab-to 8)))
  ("m9" (lambda () (interactive) (tab-bar-move-tab-to 9)))

  ;; winner
  ("U" winner-undo)
  ("R" winner-redo)

  ("q" nil "cancel" :color blue)
  )

(defun toy/tab-move-right ()
  (interactive)
  (let* ((ix (tab-bar--current-tab-index))
         (n-tabs (length (funcall tab-bar-tabs-function)))
         (next-ix (mod (+ ix 1) n-tabs)))
    ;; use 1-based index
    (tab-bar-move-tab-to (+ 1 next-ix))))

(defun toy/tab-move-left ()
  (interactive)
  (let* ((ix (tab-bar--current-tab-index))
         (n-tabs (length (funcall tab-bar-tabs-function)))
         (next-ix (mod (+ ix n-tabs -1) n-tabs)))
    ;; use 1-based index
    (tab-bar-move-tab-to (+ 1 next-ix))))

(provide 'init-hydra)
;;; init-hydra.el ends here
