;;; lisp/init-hydra.el -*- lexical-binding: t; -*-

(use-package hydra
    :straight t
    :ensure t
    :after outline
    :custom
    (hydra-if-helpful t)
    :commands (defhydra)
    :bind (
           ("M-o" . hydra-base/body)
	   ))

(use-package hydra-posframe
    :straight (hydra-posframe
               :host github
               :repo "Ladicle/hydra-posframe"
               )
    :defer t
    :after (hydra posframe)
    :config
    (hydra-posframe-enable))


(defhydra hydra-base ()
  "
_o_utline & outshine
_s_traight
awesome-_t_ab
_w_indow
"
  ("s" hydra-straight/body :exit t)
  ("w" hydra-window/body :exit t)
  ("t" awesome-fast-switch/body :exit t)
  ("o" hydra-outline/body :exit t)
  )

(use-package ace-window
    :straight t
    :ensure t
    :defer 5
    )

;; Activate winner mode for quickly changing window sizes, etc
(when (fboundp 'winner-mode)
  (winner-mode 1))

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
_F_ollow    _D_lt Other     _S_ave    max_i_mize
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
                   'hydra-window/body))
       )
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
       )
  ("Z" winner-redo)
  ("SPC" nil)
  )


;; (defhydra hydra-git-gutter (:body-pre (git-gutter+-mode 1)
;;                             :hint nil)
;;   "
;; Git gutter:
;;   _j_: next hunk        _s_tage hunk     _q_uit
;;   _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
;;   ^ ^                   _p_opup hunk
;;   _h_: first hunk
;;   _l_: last hunk
;; "
;; ;; set start _R_evision
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
;;         )
;;        :color blue)
;; )

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

(defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("0" my-select-window)
  ("1" my-select-window)
  ("2" my-select-window)
  ("3" my-select-window)
  ("4" my-select-window)
  ("5" my-select-window)
  ("6" my-select-window)
  ("7" my-select-window)
  ("8" my-select-window)
  ("9" my-select-window)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("b" ivy-switch-buffer)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("q" nil "quit"))


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


					; (defhydra hydra-straight-helper (:hint nil)
					;   "
					; _c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
					; _C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
					; ----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
					; _r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
					; _R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
					;   ("c" straight-check-all)
					;   ("C" straight-check-package)
					;   ("r" straight-rebuild-all)
					;   ("R" straight-rebuild-package)
					;   ("f" straight-fetch-all)
					;   ("F" straight-fetch-package)
					;   ("p" straight-pull-all)
					;   ("P" straight-pull-package)
					;   ("m" straight-merge-all)
					;   ("M" straight-merge-package)
					;   ("n" straight-normalize-all)
					;   ("N" straight-normalize-package)
					;   ("u" straight-push-all)
					;   ("U" straight-push-package)
					;   ("v" straight-freeze-versions)
					;   ("V" straight-thaw-versions)
					;   ("w" straight-watcher-start)
					;   ("W" straight-watcher-quit)
					;   ("g" straight-get-recipe)
					;   ("e" straight-prune-build)
					;   ("q" nil))



(provide 'init-hydra)
;;; init-hydra.el ends here
