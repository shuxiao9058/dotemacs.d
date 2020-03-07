;;; lisp/init-keyfreq.el -*- lexical-binding: t; -*-



;; Took from http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
(use-package keyfreq
  :straight t
  :commands (keyfreq-mode keyfreq-autosave-mode keyfreq-show keyfreq-reset)
  :config
  (setq keyfreq-excluded-commands '(org-self-insert-command
                                    abort-recursive-edit
                                    forward-char
                                    backward-char
                                    evil-backward-char
                                    evil-forward-char
                                    next-line
                                    previous-line
                                    right-char
                                    left-char
                                    evil-next-line
                                    evil-previous-line
                                    evil-normal-state
                                    treemacs-next-line
                                    treemacs-previous-line
                                    self-insert-command
                                    cua-scroll-down
                                    company-ignore
                                    org-delete-backward-char
                                    python-indent-dedent-line
                                    python-indent-dedent-line-backspace
                                    delete-backward-char
                                    org-return
                                    ivy-next-line
                                    ivy-backward-delete-char
                                    company-select-next-or-abort
                                    company-select-previous-or-abort
                                    end-of-line
                                    magit-next-line
                                    mwheel-scroll
                                    isearch-printing-char
                                    newline
                                    mouse-drag-region
                                    org-cycle
                                    ivy-previous-line
                                    org-meta-return
                                    mouse-set-point
                                    kill-line
                                    find-file
                                    org-agenda-next-line
                                    ivy-done
                                    minibuffer-keyboard-quit
                                    magit-previous-line
                                    beginning-of-line
                                    indent-for-tab-command
                                    backward-delete-char-untabify
                                    exit-minibuffer
                                    exit-minibuffer
                                    ace-window
                                    awesome-tab-select-visible-tab
                                    company-complete-selection
                                    newline-and-indentj
                                    ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here