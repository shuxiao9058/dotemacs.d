;;; custom.el -*- lexical-binding: t; -*-



(setq auth-sources '("~/.authinfo.gpg")
      ;; auth-sources '("~/.authinfo")
      make-backup-files nil
      enable-local-eval t
      enable-local-variables :all
      save-silently t
      find-file-suppress-same-file-warnings t
      vc-follow-symlinks t
      inhibit-startup-screen t
      whitespace-line-column 100
      default-directory (expand-file-name "workspace/" "~")
      vagrant-vagrantfile (expand-file-name "vagrant/Vagrantfile" default-directory)
      ;; Wrapping
      truncate-lines t
      truncate-partial-width-windows 50
      whitespace-style '(face trailing lines-tail)
      xref-prompt-for-identifier nil)

;; @see https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 3 1024 1024))

;;; lisp
(setq ;; lisp-body-indent   2
 lisp-indent-function  'lisp-indent-function)

;; handle emacs utf-8 input
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")

;; enable C-x C-u to upcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; custom.el ends here
