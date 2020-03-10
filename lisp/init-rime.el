;;; lisp/init-rime.el -*- lexical-binding: t; -*-

(use-package liberime-config
  :straight (
             :local-repo "~/workspace/liberime"
             ;; :host github
             ;; ; :repo "DogLooksGood/liberime"
             ;; :repo "merrickluo/liberime"
             ;; ;; :repo "QiangF/liberime"
             ;; ;; :repo "shuxiao9058/liberime"
             ;; ;; :branch "feature/add_schema_config"
             :files ("liberime-config.el" "src" "CMakeLists.txt" "Makefile")
	     )

  :defer 1
  :init
  (setq liberime-user-data-dir (expand-file-name "rime" poly-local-dir))
  ;; (add-hook 'liberime-after-start-hook
  ;;           (lambda ()
  ;;             ;; ;; Select schema delay 5 second, make sure
  ;;             ;; ;; `liberime-load' run finish.
  ;;             ;; (run-with-timer
  ;;             ;;    5 nil
  ;;             (liberime-select-schema "guhuwubi")
  ;;             ;; )
  ;;             )
  ;;           )
  :config
  ;; ;; build liberime-core.so
  ;; (unless (file-exists-p (liberime-get-module-file))
  ;;   (liberime-build)
  ;;   )
  )

(use-package rime
  :straight (
					; :host github
					; :repo "DogLooksGood/emacs-rime"
             :local-repo "~/workspace/emacs-rime"
             ;; :repo "shuxiao9058/emacs-rime"
             :files ("rime.el"))
  :defer 1
  :config
  (setq default-input-method "rime"
        ;; rime-show-candidate 'overlay
        ;; rime-show-candidate 'message
        ;; rime-show-candidate 'minibuffer
        rime-show-candidate 'popup

        ;; ;; 如果使用模式编辑，或是需要在一些特定的场景下自动使用英文，可以 ~rime-disable-predicates~
        ;; ;; 一个在 ~evil-normal-state~ 中、在英文字母后面以及代码中自动使用英文的例子
        ;; rime-disable-predicates
        ;; '(evil-normal-state-p
        ;;   rime--after-alphabet-char-p
        ;;   rime--prog-in-code-p)
        )
  (global-set-key (kbd "C-\\") 'rime-toggle)
  )

(provide 'init-rime)

;;; init-rime.el ends here
