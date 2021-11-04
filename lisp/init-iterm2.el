;;; lisp/init-iterm2.el -*- lexical-binding: t; -*-

;; ;; ITERM2 MOUSE SUPPORT
;; (require 'mouse)

(unless IS-GUI
  (require 'mouse) ;; needed for iterm2 compatibility
  (setq mouse-sel-mode t)
  (setq x-select-enable-clipboard t)
  ;;  (setq x-select-enable-primary nil)
  ;;  (setq mouse-drag-copy-region nil)
  (xterm-mouse-mode t)
  (if (eq system-type 'darwin)
      (progn
	(global-set-key [mouse-4] '(lambda ()
				    (interactive)
				    (scroll-down 1)))
	(global-set-key [mouse-5] '(lambda ()
				    (interactive)
				    (scroll-up 1)))
	))

  (defun track-mouse (e))
  )

;; Fix mouse from emacsclient
;; http://stackoverflow.com/a/6798279
(defun my-terminal-mouse-config (&optional frame)
  "Establish settings for the current terminal."
  (if (not frame) ;; The initial call.
      (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
	;; Re-initialise the mode in case of a new terminal.
	(xterm-mouse-mode 1))))
;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).
(my-terminal-mouse-config)
(add-hook 'after-make-frame-functions 'my-terminal-mouse-config)

(defun track-mouse (e))

(setq mouse-sel-mode t)

(global-set-key (kbd "<mouse-1>") 'mouse-set-point)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(global-set-key (kbd "<mouse-20>") 'scroll-right)
(global-set-key (kbd "<mouse-21>") 'scroll-left)
(global-set-key (kbd "<vertical-scroll-bar> <mouse-1>")
		'scroll-bar-toolkit-scroll)


;; tmux-navigate
;; (use-package navigate
;;   :straight (navigate
;; 	     :host github
;; 	     :repo "keith/evil-tmux-navigator"))

;; (use-package tmux-pane
;;   :ensure t
;;   :straight(tmux-pane
;; 	    :host github
;; 	    :repo "paulojean/emacs-tmux-pane")
;;   :custom
;;   (tmux-pane-terminal-folder-fn #'projectile-project-root)
;;   (tmux-pane-horizontal-percent 25)
;;   :config
;;   (tmux-pane-mode +1)
;;   )

;; TERMINAL MAPPINGS TO SUPPORT ITERM2 FOR MAC
(progn
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
	       function-key-map)))
    (define-key map "\e[1;P9"  (kbd "H-a"))
    (define-key map "\e[1;P10" (kbd "H-b"))
    (define-key map "\e[1;P11" (kbd "H-c"))
    (define-key map "\e[1;P12" (kbd "H-d"))
    (define-key map "\e[1;P13" (kbd "H-e"))
    (define-key map "\e[1;P14" (kbd "H-f"))
    (define-key map "\e[1;P15" (kbd "H-g"))
    (define-key map "\e[1;P16" (kbd "H-h"))
    (define-key map "\e[1;P17" (kbd "H-i"))
    (define-key map "\e[1;P18" (kbd "H-j"))
    (define-key map "\e[1;P19" (kbd "H-k"))
    (define-key map "\e[1;P20" (kbd "H-l"))
    (define-key map "\e[1;P21" (kbd "H-m"))
    (define-key map "\e[1;P22" (kbd "H-n"))
    (define-key map "\e[1;P23" (kbd "H-o"))
    (define-key map "\e[1;P24" (kbd "H-p"))
    (define-key map "\e[1;P25" (kbd "H-q"))
    (define-key map "\e[1;P26" (kbd "H-r"))
    (define-key map "\e[1;P27" (kbd "H-s"))
    (define-key map "\e[1;P28" (kbd "H-t"))
    (define-key map "\e[1;P29" (kbd "H-u"))
    (define-key map "\e[1;P30" (kbd "H-v"))
    (define-key map "\e[1;P31" (kbd "H-w"))
    (define-key map "\e[1;P32" (kbd "H-x"))
    (define-key map "\e[1;P33" (kbd "H-y"))
    (define-key map "\e[1;P34" (kbd "H-z"))
    (define-key map "\e[1;P35" (kbd "H-0"))
    (define-key map "\e[1;P36" (kbd "H-1"))
    (define-key map "\e[1;P37" (kbd "H-2"))
    (define-key map "\e[1;P38" (kbd "H-3"))
    (define-key map "\e[1;P39" (kbd "H-4"))
    (define-key map "\e[1;P40" (kbd "H-5"))
    (define-key map "\e[1;P41" (kbd "H-6"))
    (define-key map "\e[1;P42" (kbd "H-7"))
    (define-key map "\e[1;P43" (kbd "H-8"))
    (define-key map "\e[1;P44" (kbd "H-9"))
    (define-key map "\e[1;P45" (kbd "H-<f1>"))
    (define-key map "\e[1;P46" (kbd "H-<f2>"))
    (define-key map "\e[1;P47" (kbd "H-<f3>"))
    (define-key map "\e[1;P48" (kbd "H-<f4>"))
    (define-key map "\e[1;P49" (kbd "H-<f5>"))
    (define-key map "\e[1;P50" (kbd "H-<f6>"))
    (define-key map "\e[1;P51" (kbd "H-<f7>"))
    (define-key map "\e[1;P52" (kbd "H-<f8>"))
    (define-key map "\e[1;P53" (kbd "H-<f9>"))
    (define-key map "\e[1;P54" (kbd "H-<f10>"))
    (define-key map "\e[1;P55" (kbd "H-<f11>"))
    (define-key map "\e[1;P56" (kbd "H-<f12>"))
    ))

;; https://github.com/choppsv1/dot.spacemacs.d/blob/master/local-lisp/iterm-;; xterm-extra.el
;; (define-key function-key-map "\e[27;5;39~" (kbd "C-'"))
;; (define-key function-key-map "\e[27;6;34~" (kbd "C-\""))
;; (define-key function-key-map "\e[27;3;39~" (kbd "M-'"))
;; (define-key function-key-map "\e[27;4;34~" (kbd "M-\""))
;; (define-key function-key-map "\e[27;7;39~" (kbd "C-M-'"))
;; (define-key function-key-map "\e[27;8;34~" (kbd "C-M-\""))
;; (define-key function-key-map "\e[27;5;45~" (kbd "C--"))
;; (define-key function-key-map "\e[27;6;95~" (kbd "C-_"))
;; (define-key function-key-map "\e[27;3;45~" (kbd "M--"))
;; (define-key function-key-map "\e[27;4;95~" (kbd "M-_"))
;; (define-key function-key-map "\e[27;7;45~" (kbd "C-M--"))
;; (define-key function-key-map "\e[27;8;95~" (kbd "C-M-_"))
;; (define-key function-key-map "\e[27;5;44~" (kbd "C-,"))
;; (define-key function-key-map "\e[27;6;60~" (kbd "C-<"))
;; (define-key function-key-map "\e[27;3;44~" (kbd "M-,"))
;; (define-key function-key-map "\e[27;4;60~" (kbd "M-<"))
;; (define-key function-key-map "\e[27;7;44~" (kbd "C-M-,"))
;; (define-key function-key-map "\e[27;8;60~" (kbd "C-M-<"))
;; (define-key function-key-map "\e[27;5;47~" (kbd "C-/"))
;; (define-key function-key-map "\e[27;6;63~" (kbd "C-?"))
;; (define-key function-key-map "\e[27;3;47~" (kbd "M-/"))
;; (define-key function-key-map "\e[27;4;63~" (kbd "M-?"))
;; (define-key function-key-map "\e[27;7;47~" (kbd "C-M-/"))
;; (define-key function-key-map "\e[27;8;63~" (kbd "C-M-?"))
;; (define-key function-key-map "\e[27;5;46~" (kbd "C-."))
;; (define-key function-key-map "\e[27;6;62~" (kbd "C->"))
;; (define-key function-key-map "\e[27;3;46~" (kbd "M-."))
;; (define-key function-key-map "\e[27;4;62~" (kbd "M->"))
;; (define-key function-key-map "\e[27;7;46~" (kbd "C-M-."))
;; (define-key function-key-map "\e[27;8;62~" (kbd "C-M->"))
;; (define-key function-key-map "\e[27;5;49~" (kbd "C-1"))
;; (define-key function-key-map "\e[27;6;33~" (kbd "C-!"))
;; (define-key function-key-map "\e[27;3;49~" (kbd "M-1"))
;; (define-key function-key-map "\e[27;4;33~" (kbd "M-!"))
;; (define-key function-key-map "\e[27;7;49~" (kbd "C-M-1"))
;; (define-key function-key-map "\e[27;8;33~" (kbd "C-M-!"))
;; (define-key function-key-map "\e[27;5;48~" (kbd "C-0"))
;; (define-key function-key-map "\e[27;6;41~" (kbd "C-)"))
;; (define-key function-key-map "\e[27;3;48~" (kbd "M-0"))
;; (define-key function-key-map "\e[27;4;41~" (kbd "M-)"))
;; (define-key function-key-map "\e[27;7;48~" (kbd "C-M-0"))
;; (define-key function-key-map "\e[27;8;41~" (kbd "C-M-)"))
;; (define-key function-key-map "\e[27;5;51~" (kbd "C-3"))
;; (define-key function-key-map "\e[27;6;35~" (kbd "C-#"))
;; (define-key function-key-map "\e[27;3;51~" (kbd "M-3"))
;; (define-key function-key-map "\e[27;4;35~" (kbd "M-#"))
;; (define-key function-key-map "\e[27;7;51~" (kbd "C-M-3"))
;; (define-key function-key-map "\e[27;8;35~" (kbd "C-M-#"))
;; (define-key function-key-map "\e[27;5;50~" (kbd "C-2"))
;; (define-key function-key-map "\e[27;3;50~" (kbd "M-2"))
;; (define-key function-key-map "\e[27;4;64~" (kbd "M-@"))
;; (define-key function-key-map "\e[27;7;50~" (kbd "C-M-2"))
;; (define-key function-key-map "\e[27;8;64~" (kbd "C-M-@"))
;; (define-key function-key-map "\e[27;5;53~" (kbd "C-5"))
;; (define-key function-key-map "\e[27;6;37~" (kbd "C-%"))
;; (define-key function-key-map "\e[27;3;53~" (kbd "M-5"))
;; (define-key function-key-map "\e[27;4;37~" (kbd "M-%"))
;; (define-key function-key-map "\e[27;7;53~" (kbd "C-M-5"))
;; (define-key function-key-map "\e[27;8;37~" (kbd "C-M-%"))
;; (define-key function-key-map "\e[27;5;52~" (kbd "C-4"))
;; (define-key function-key-map "\e[27;6;36~" (kbd "C-$"))
;; (define-key function-key-map "\e[27;3;52~" (kbd "M-4"))
;; (define-key function-key-map "\e[27;4;36~" (kbd "M-$"))
;; (define-key function-key-map "\e[27;7;52~" (kbd "C-M-4"))
;; (define-key function-key-map "\e[27;8;36~" (kbd "C-M-$"))
;; (define-key function-key-map "\e[27;5;55~" (kbd "C-7"))
;; (define-key function-key-map "\e[27;6;38~" (kbd "C-&"))
;; (define-key function-key-map "\e[27;3;55~" (kbd "M-7"))
;; (define-key function-key-map "\e[27;4;38~" (kbd "M-&"))
;; (define-key function-key-map "\e[27;7;55~" (kbd "C-M-7"))
;; (define-key function-key-map "\e[27;8;38~" (kbd "C-M-&"))
;; (define-key function-key-map "\e[27;5;54~" (kbd "C-6"))
;; (define-key function-key-map "\e[27;6;94~" (kbd "C-^"))
;; (define-key function-key-map "\e[27;3;54~" (kbd "M-6"))
;; (define-key function-key-map "\e[27;4;94~" (kbd "M-^"))
;; (define-key function-key-map "\e[27;7;54~" (kbd "C-M-6"))
;; (define-key function-key-map "\e[27;8;94~" (kbd "C-M-^"))
;; (define-key function-key-map "\e[27;5;57~" (kbd "C-9"))
;; (define-key function-key-map "\e[27;6;40~" (kbd "C-("))
;; (define-key function-key-map "\e[27;3;57~" (kbd "M-9"))
;; (define-key function-key-map "\e[27;4;40~" (kbd "M-("))
;; (define-key function-key-map "\e[27;7;57~" (kbd "C-M-9"))
;; (define-key function-key-map "\e[27;8;40~" (kbd "C-M-("))
;; (define-key function-key-map "\e[27;5;56~" (kbd "C-8"))
;; (define-key function-key-map "\e[27;6;42~" (kbd "C-*"))
;; (define-key function-key-map "\e[27;3;56~" (kbd "M-8"))
;; (define-key function-key-map "\e[27;4;42~" (kbd "M-*"))
;; (define-key function-key-map "\e[27;7;56~" (kbd "C-M-8"))
;; (define-key function-key-map "\e[27;8;42~" (kbd "C-M-*"))
;; (define-key function-key-map "\e[27;5;59~" (kbd "C-;"))
;; (define-key function-key-map "\e[27;6;58~" (kbd "C-:"))
;; (define-key function-key-map "\e[27;3;59~" (kbd "M-;"))
;; (define-key function-key-map "\e[27;4;58~" (kbd "M-:"))
;; (define-key function-key-map "\e[27;7;59~" (kbd "C-M-;"))
;; (define-key function-key-map "\e[27;8;58~" (kbd "C-M-:"))
;; (define-key function-key-map "\e[27;5;61~" (kbd "C-="))
;; (define-key function-key-map "\e[27;6;43~" (kbd "C-+"))
;; (define-key function-key-map "\e[27;3;61~" (kbd "M-="))
;; (define-key function-key-map "\e[27;4;43~" (kbd "M-+"))
;; (define-key function-key-map "\e[27;7;61~" (kbd "C-M-="))
;; (define-key function-key-map "\e[27;8;43~" (kbd "C-M-+"))
;; (define-key function-key-map "\e[27;6;123~" (kbd "C-{"))
;; (define-key function-key-map "\e[27;3;91~" (kbd "M-["))
;; (define-key function-key-map "\e[27;4;123~" (kbd "M-{"))
;; (define-key function-key-map "\e[27;7;91~" (kbd "C-M-["))
;; (define-key function-key-map "\e[27;8;123~" (kbd "C-M-{"))
;; (define-key function-key-map "\e[27;6;125~" (kbd "C-}"))
;; (define-key function-key-map "\e[27;3;93~" (kbd "M-]"))
;; (define-key function-key-map "\e[27;4;125~" (kbd "M-}"))
;; (define-key function-key-map "\e[27;7;93~" (kbd "C-M-]"))
;; (define-key function-key-map "\e[27;8;125~" (kbd "C-M-}"))
;; (define-key function-key-map "\e[27;6;124~" (kbd "C-|"))
;; (define-key function-key-map "\e[27;3;92~" (kbd "M-\\"))
;; (define-key function-key-map "\e[27;4;124~" (kbd "M-|"))
;; (define-key function-key-map "\e[27;7;92~" (kbd "C-M-\\"))
;; (define-key function-key-map "\e[27;8;124~" (kbd "C-M-|"))
;; (define-key function-key-map "\e[27;6;65~" (kbd "C-S-A"))
;; (define-key function-key-map "\e[27;7;97~" (kbd "C-M-a"))
;; (define-key function-key-map "\e[27;8;65~" (kbd "C-M-S-A"))
;; (define-key function-key-map "\e[27;5;96~" (kbd "C-`"))
;; (define-key function-key-map "\e[27;6;126~" (kbd "C-~"))
;; (define-key function-key-map "\e[27;3;96~" (kbd "M-`"))
;; (define-key function-key-map "\e[27;4;126~" (kbd "M-~"))
;; (define-key function-key-map "\e[27;7;96~" (kbd "C-M-`"))
;; (define-key function-key-map "\e[27;8;126~" (kbd "C-M-~"))
;; (define-key function-key-map "\e[27;6;67~" (kbd "C-S-C"))
;; (define-key function-key-map "\e[27;7;99~" (kbd "C-M-c"))
;; (define-key function-key-map "\e[27;8;67~" (kbd "C-M-S-C"))
;; (define-key function-key-map "\e[27;6;66~" (kbd "C-S-B"))
;; (define-key function-key-map "\e[27;7;98~" (kbd "C-M-b"))
;; (define-key function-key-map "\e[27;8;66~" (kbd "C-M-S-B"))
;; (define-key function-key-map "\e[27;6;69~" (kbd "C-S-E"))
;; (define-key function-key-map "\e[27;7;101~" (kbd "C-M-e"))
;; (define-key function-key-map "\e[27;8;69~" (kbd "C-M-S-E"))
;; (define-key function-key-map "\e[27;6;68~" (kbd "C-S-D"))
;; (define-key function-key-map "\e[27;7;100~" (kbd "C-M-d"))
;; (define-key function-key-map "\e[27;8;68~" (kbd "C-M-S-D"))
;; (define-key function-key-map "\e[27;6;71~" (kbd "C-S-G"))
;; (define-key function-key-map "\e[27;7;103~" (kbd "C-M-g"))
;; (define-key function-key-map "\e[27;8;71~" (kbd "C-M-S-G"))
;; (define-key function-key-map "\e[27;6;70~" (kbd "C-S-F"))
;; (define-key function-key-map "\e[27;7;102~" (kbd "C-M-f"))
;; (define-key function-key-map "\e[27;8;70~" (kbd "C-M-S-F"))
;; (define-key function-key-map "\e[27;6;73~" (kbd "C-S-I"))
;; (define-key function-key-map "\e[27;7;105~" (kbd "C-M-i"))
;; (define-key function-key-map "\e[27;8;73~" (kbd "C-M-S-I"))
;; (define-key function-key-map "\e[27;6;72~" (kbd "C-S-H"))
;; (define-key function-key-map "\e[27;7;104~" (kbd "C-M-h"))
;; (define-key function-key-map "\e[27;8;72~" (kbd "C-M-S-H"))
;; (define-key function-key-map "\e[27;6;75~" (kbd "C-S-K"))
;; (define-key function-key-map "\e[27;7;107~" (kbd "C-M-k"))
;; (define-key function-key-map "\e[27;8;75~" (kbd "C-M-S-K"))
;; (define-key function-key-map "\e[27;6;74~" (kbd "C-S-J"))
;; (define-key function-key-map "\e[27;7;106~" (kbd "C-M-j"))
;; (define-key function-key-map "\e[27;8;74~" (kbd "C-M-S-J"))
;; (define-key function-key-map "\e[27;6;77~" (kbd "C-S-M"))
;; (define-key function-key-map "\e[27;7;109~" (kbd "C-M-m"))
;; (define-key function-key-map "\e[27;8;77~" (kbd "C-M-S-M"))
;; (define-key function-key-map "\e[27;6;76~" (kbd "C-S-L"))
;; (define-key function-key-map "\e[27;7;108~" (kbd "C-M-l"))
;; (define-key function-key-map "\e[27;8;76~" (kbd "C-M-S-L"))
;; (define-key function-key-map "\e[27;6;79~" (kbd "C-S-O"))
;; (define-key function-key-map "\e[27;7;111~" (kbd "C-M-o"))
;; (define-key function-key-map "\e[27;8;79~" (kbd "C-M-S-O"))
;; (define-key function-key-map "\e[27;6;78~" (kbd "C-S-N"))
;; (define-key function-key-map "\e[27;7;110~" (kbd "C-M-n"))
;; (define-key function-key-map "\e[27;8;78~" (kbd "C-M-S-N"))
;; (define-key function-key-map "\e[27;6;81~" (kbd "C-S-Q"))
;; (define-key function-key-map "\e[27;7;113~" (kbd "C-M-q"))
;; (define-key function-key-map "\e[27;8;81~" (kbd "C-M-S-Q"))
;; (define-key function-key-map "\e[27;6;80~" (kbd "C-S-P"))
;; (define-key function-key-map "\e[27;7;112~" (kbd "C-M-p"))
;; (define-key function-key-map "\e[27;8;80~" (kbd "C-M-S-P"))
;; (define-key function-key-map "\e[27;6;83~" (kbd "C-S-S"))
;; (define-key function-key-map "\e[27;7;115~" (kbd "C-M-s"))
;; (define-key function-key-map "\e[27;8;83~" (kbd "C-M-S-S"))
;; (define-key function-key-map "\e[27;6;82~" (kbd "C-S-R"))
;; (define-key function-key-map "\e[27;7;114~" (kbd "C-M-r"))
;; (define-key function-key-map "\e[27;8;82~" (kbd "C-M-S-R"))
;; (define-key function-key-map "\e[27;6;85~" (kbd "C-S-U"))
;; (define-key function-key-map "\e[27;7;117~" (kbd "C-M-u"))
;; (define-key function-key-map "\e[27;8;85~" (kbd "C-M-S-U"))
;; (define-key function-key-map "\e[27;6;84~" (kbd "C-S-T"))
;; (define-key function-key-map "\e[27;7;116~" (kbd "C-M-t"))
;; (define-key function-key-map "\e[27;8;84~" (kbd "C-M-S-T"))
;; (define-key function-key-map "\e[27;6;87~" (kbd "C-S-W"))
;; (define-key function-key-map "\e[27;7;119~" (kbd "C-M-w"))
;; (define-key function-key-map "\e[27;8;87~" (kbd "C-M-S-W"))
;; (define-key function-key-map "\e[27;6;86~" (kbd "C-S-V"))
;; (define-key function-key-map "\e[27;7;118~" (kbd "C-M-v"))
;; (define-key function-key-map "\e[27;8;86~" (kbd "C-M-S-V"))
;; (define-key function-key-map "\e[27;6;89~" (kbd "C-S-Y"))
;; (define-key function-key-map "\e[27;7;121~" (kbd "C-M-y"))
;; (define-key function-key-map "\e[27;8;89~" (kbd "C-M-S-Y"))
;; (define-key function-key-map "\e[27;6;88~" (kbd "C-S-X"))
;; (define-key function-key-map "\e[27;7;120~" (kbd "C-M-x"))
;; (define-key function-key-map "\e[27;8;88~" (kbd "C-M-S-X"))
;; (define-key function-key-map "\e[27;6;90~" (kbd "C-S-Z"))
;; (define-key function-key-map "\e[27;7;122~" (kbd "C-M-z"))
;; (define-key function-key-map "\e[27;8;90~" (kbd "C-M-S-Z"))
;; (define-key function-key-map "\eO5P" [C-f1])
;; (define-key function-key-map "\eO2P" [S-f1])
;; (define-key function-key-map "\eO3P" [M-f1])
;; (define-key function-key-map "\eO6P" [C-S-f1])
;; (define-key function-key-map "\eO4P" [M-S-f1])
;; (define-key function-key-map "\eO7P" [C-M-f1])
;; (define-key function-key-map "\eO8P" [C-M-S-f1])
;; (define-key function-key-map "\eO5Q" [C-f2])
;; (define-key function-key-map "\eO2Q" [S-f2])
;; (define-key function-key-map "\eO3Q" [M-f2])
;; (define-key function-key-map "\eO6Q" [C-S-f2])
;; (define-key function-key-map "\eO4Q" [M-S-f2])
;; (define-key function-key-map "\eO7Q" [C-M-f2])
;; (define-key function-key-map "\eO8Q" [C-M-S-f2])
;; (define-key function-key-map "\e[1;5C" [C-right])
;; (define-key function-key-map "\e[1;2C" [S-right])
;; (define-key function-key-map "\e[1;3C" [M-right])
;; (define-key function-key-map "\e[1;6C" [C-S-right])
;; (define-key function-key-map "\e[1;4C" [M-S-right])
;; (define-key function-key-map "\e[1;7C" [C-M-right])
;; (define-key function-key-map "\e[1;8C" [C-M-S-right])
;; (define-key function-key-map "\eO5S" [C-f4])
;; (define-key function-key-map "\eO2S" [S-f4])
;; (define-key function-key-map "\eO3S" [M-f4])
;; (define-key function-key-map "\eO6S" [C-S-f4])
;; (define-key function-key-map "\eO4S" [M-S-f4])
;; (define-key function-key-map "\eO7S" [C-M-f4])
;; (define-key function-key-map "\eO8S" [C-M-S-f4])
;; (define-key function-key-map "\e[15;5~" [C-f5])
;; (define-key function-key-map "\e[15;2~" [S-f5])
;; (define-key function-key-map "\e[15;3~" [M-f5])
;; (define-key function-key-map "\e[15;6~" [C-S-f5])
;; (define-key function-key-map "\e[15;4~" [M-S-f5])
;; (define-key function-key-map "\e[15;7~" [C-M-f5])
;; (define-key function-key-map "\e[15;8~" [C-M-S-f5])
;; (define-key function-key-map "\e[17;5~" [C-f6])
;; (define-key function-key-map "\e[17;2~" [S-f6])
;; (define-key function-key-map "\e[17;3~" [M-f6])
;; (define-key function-key-map "\e[17;6~" [C-S-f6])
;; (define-key function-key-map "\e[17;4~" [M-S-f6])
;; (define-key function-key-map "\e[17;7~" [C-M-f6])
;; (define-key function-key-map "\e[17;8~" [C-M-S-f6])
;; (define-key function-key-map "\e[18;5~" [C-f7])
;; (define-key function-key-map "\e[18;2~" [S-f7])
;; (define-key function-key-map "\e[18;3~" [M-f7])
;; (define-key function-key-map "\e[18;6~" [C-S-f7])
;; (define-key function-key-map "\e[18;4~" [M-S-f7])
;; (define-key function-key-map "\e[18;7~" [C-M-f7])
;; (define-key function-key-map "\e[18;8~" [C-M-S-f7])
;; (define-key function-key-map "\e[19;5~" [C-f8])
;; (define-key function-key-map "\e[19;2~" [S-f8])
;; (define-key function-key-map "\e[19;3~" [M-f8])
;; (define-key function-key-map "\e[19;6~" [C-S-f8])
;; (define-key function-key-map "\e[19;4~" [M-S-f8])
;; (define-key function-key-map "\e[19;7~" [C-M-f8])
;; (define-key function-key-map "\e[19;8~" [C-M-S-f8])
;; (define-key function-key-map "\e[20;5~" [C-f9])
;; (define-key function-key-map "\e[20;2~" [S-f9])
;; (define-key function-key-map "\e[20;3~" [M-f9])
;; (define-key function-key-map "\e[20;6~" [C-S-f9])
;; (define-key function-key-map "\e[20;4~" [M-S-f9])
;; (define-key function-key-map "\e[20;7~" [C-M-f9])
;; (define-key function-key-map "\e[20;8~" [C-M-S-f9])
;; (define-key function-key-map "\e[1;5B" [C-down])
;; (define-key function-key-map "\e[1;2B" [S-down])
;; (define-key function-key-map "\e[1;3B" [M-down])
;; (define-key function-key-map "\e[1;6B" [C-S-down])
;; (define-key function-key-map "\e[1;4B" [M-S-down])
;; (define-key function-key-map "\e[1;7B" [C-M-down])
;; (define-key function-key-map "\e[1;8B" [C-M-S-down])
;; (define-key function-key-map "\eO5R" [C-f3])
;; (define-key function-key-map "\eO2R" [S-f3])
;; (define-key function-key-map "\eO3R" [M-f3])
;; (define-key function-key-map "\eO6R" [C-S-f3])
;; (define-key function-key-map "\eO4R" [M-S-f3])
;; (define-key function-key-map "\eO7R" [C-M-f3])
;; (define-key function-key-map "\eO8R" [C-M-S-f3])
;; (define-key function-key-map "\e[27;5;9~" [C-tab])
;; (define-key function-key-map "\e[27;2;9~" [S-tab])
;; (define-key function-key-map "\e[27;3;9~" [M-tab])
;; (define-key function-key-map "\e[27;6;9~" [C-S-tab])
;; (define-key function-key-map "\e[27;4;9~" [M-S-tab])
;; (define-key function-key-map "\e[27;7;9~" [C-M-tab])
;; (define-key function-key-map "\e[27;8;9~" [C-M-S-tab])
;; (define-key function-key-map "\e[1;5H" [C-home])
;; (define-key function-key-map "\e[1;2H" [S-home])
;; (define-key function-key-map "\e[1;3H" [M-home])
;; (define-key function-key-map "\e[1;6H" [C-S-home])
;; (define-key function-key-map "\e[1;4H" [M-S-home])
;; (define-key function-key-map "\e[1;7H" [C-M-home])
;; (define-key function-key-map "\e[1;8H" [C-M-S-home])
;; (define-key function-key-map "\e[1;5F" [C-end])
;; (define-key function-key-map "\e[1;2F" [S-end])
;; (define-key function-key-map "\e[1;3F" [M-end])
;; (define-key function-key-map "\e[1;6F" [C-S-end])
;; (define-key function-key-map "\e[1;4F" [M-S-end])
;; (define-key function-key-map "\e[1;7F" [C-M-end])
;; (define-key function-key-map "\e[1;8F" [C-M-S-end])
;; (define-key function-key-map "\e[6;5~" [C-next])
;; (define-key function-key-map "\e[6;2~" [S-next])
;; (define-key function-key-map "\e[6;3~" [M-next])
;; (define-key function-key-map "\e[6;6~" [C-S-next])
;; (define-key function-key-map "\e[6;4~" [M-S-next])
;; (define-key function-key-map "\e[6;7~" [C-M-next])
;; (define-key function-key-map "\e[6;8~" [C-M-S-next])
;; (define-key function-key-map "\e[27;5;13~" [C-return])
;; (define-key function-key-map "\e[27;2;13~" [S-return])
;; (define-key function-key-map "\e[27;3;13~" [M-return])
;; (define-key function-key-map "\e[27;6;13~" [C-S-return])
;; (define-key function-key-map "\e[27;4;13~" [M-S-return])
;; (define-key function-key-map "\e[27;7;13~" [C-M-return])
;; (define-key function-key-map "\e[27;8;13~" [C-M-S-return])
;; (define-key function-key-map "\e[2;5~" [C-insert])
;; (define-key function-key-map "\e[2;2~" [S-insert])
;; (define-key function-key-map "\e[2;3~" [M-insert])
;; (define-key function-key-map "\e[2;6~" [C-S-insert])
;; (define-key function-key-map "\e[2;4~" [M-S-insert])
;; (define-key function-key-map "\e[2;7~" [C-M-insert])
;; (define-key function-key-map "\e[2;8~" [C-M-S-insert])
;; (define-key function-key-map "\e[23;5~" [C-f11])
;; (define-key function-key-map "\e[23;2~" [S-f11])
;; (define-key function-key-map "\e[23;3~" [M-f11])
;; (define-key function-key-map "\e[23;6~" [C-S-f11])
;; (define-key function-key-map "\e[23;4~" [M-S-f11])
;; (define-key function-key-map "\e[23;7~" [C-M-f11])
;; (define-key function-key-map "\e[23;8~" [C-M-S-f11])
;; (define-key function-key-map "\e[1;5A" [C-up])
;; (define-key function-key-map "\e[1;2A" [S-up])
;; (define-key function-key-map "\e[1;3A" [M-up])
;; (define-key function-key-map "\e[1;6A" [C-S-up])
;; (define-key function-key-map "\e[1;4A" [M-S-up])
;; (define-key function-key-map "\e[1;7A" [C-M-up])
;; (define-key function-key-map "\e[1;8A" [C-M-S-up])
;; (define-key function-key-map "\e[5;5~" [C-prior])
;; (define-key function-key-map "\e[5;2~" [S-prior])
;; (define-key function-key-map "\e[5;3~" [M-prior])
;; (define-key function-key-map "\e[5;6~" [C-S-prior])
;; (define-key function-key-map "\e[5;4~" [M-S-prior])
;; (define-key function-key-map "\e[5;7~" [C-M-prior])
;; (define-key function-key-map "\e[5;8~" [C-M-S-prior])
;; (define-key function-key-map "\e[24;5~" [C-f12])
;; (define-key function-key-map "\e[24;2~" [S-f12])
;; (define-key function-key-map "\e[24;3~" [M-f12])
;; (define-key function-key-map "\e[24;6~" [C-S-f12])
;; (define-key function-key-map "\e[24;4~" [M-S-f12])
;; (define-key function-key-map "\e[24;7~" [C-M-f12])
;; (define-key function-key-map "\e[24;8~" [C-M-S-f12])
;; (define-key function-key-map "\e[21;5~" [C-f10])
;; (define-key function-key-map "\e[21;2~" [S-f10])
;; (define-key function-key-map "\e[21;3~" [M-f10])
;; (define-key function-key-map "\e[21;6~" [C-S-f10])
;; (define-key function-key-map "\e[21;4~" [M-S-f10])
;; (define-key function-key-map "\e[21;7~" [C-M-f10])
;; (define-key function-key-map "\e[21;8~" [C-M-S-f10])
;; (define-key function-key-map "\e[1;5D" [C-left])
;; (define-key function-key-map "\e[1;2D" [S-left])
;; (define-key function-key-map "\e[1;3D" [M-left])
;; (define-key function-key-map "\e[1;6D" [C-S-left])
;; (define-key function-key-map "\e[1;4D" [M-S-left])
;; (define-key function-key-map "\e[1;7D" [C-M-left])
;; (define-key function-key-map "\e[1;8D" [C-M-S-left])
;; (define-key function-key-map "\e[3;5~" [C-delete])
;; (define-key function-key-map "\e[3;2~" [S-delete])
;; (define-key function-key-map "\e[3;3~" [M-delete])
;; (define-key function-key-map "\e[3;6~" [C-S-delete])
;; (define-key function-key-map "\e[3;4~" [M-S-delete])
;; (define-key function-key-map "\e[3;7~" [C-M-delete])
;; (define-key function-key-map "\e[3;8~" [C-M-S-delete])

(provide 'init-iterm2)
;;; init-iterm2.el ends here
