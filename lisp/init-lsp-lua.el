;;; lisp/init-lsp-lua.el -*- lexical-binding: t; -*-

;; https://github.com/rickysaurav/dotfiles/blob/master/emacs/.emacs.d/config.org
(defcustom lsp-lua-awakened-cat nil nil :type (quote boolean))
(defcustom lsp-lua-completion-call-snippet "Disable" nil :type (quote (choice (:tag "Disable" "Both" "Replace"))))
(defcustom lsp-lua-completion-enable t nil :type (quote boolean))
(defcustom lsp-lua-completion-keyword-snippet "Replace" nil :type (quote (choice (:tag "Disable" "Both" "Replace"))))
(defcustom lsp-lua-develop-debugger-port 11412 nil :type (quote number))
(defcustom lsp-lua-develop-debugger-wait nil nil :type (quote boolean))
(defcustom lsp-lua-develop-enable nil nil :type (quote boolean))
(defcustom lsp-lua-diagnostics-disable nil nil :type (quote lsp-string-vector))
(defcustom lsp-lua-diagnostics-enable t nil :type (quote boolean))
(defcustom lsp-lua-diagnostics-globals nil nil :type (quote lsp-string-vector))
(defcustom lsp-lua-diagnostics-severity nil nil :type (quote nil))
(defcustom lsp-lua-runtime-path ["?.lua" "?/init.lua" "?/?.lua"] nil :type (quote lsp-string-vector))
(defcustom lsp-lua-runtime-version "Lua 5.3" nil :type (quote (choice (:tag "Lua 5.1" "Lua 5.2" "Lua 5.3" "Lua 5.4" "LuaJIT"))))
(defcustom lsp-lua-workspace-ignore-dir [".vscode"] nil :type (quote lsp-string-vector))
(defcustom lsp-lua-workspace-ignore-submodules t nil :type (quote boolean))
(defcustom lsp-lua-workspace-library nil nil :type (quote nil))
(defcustom lsp-lua-workspace-max-preload 300 nil :type (quote number))
(defcustom lsp-lua-workspace-preload-file-size 100 nil :type (quote number))
(defcustom lsp-lua-workspace-use-git-ignore t nil :type (quote boolean))
(defcustom lsp-lua-server-root (expand-file-name "~/workspace/lua-language-server") nil :type (quote string))
(defcustom lsp-lua-server-binary (expand-file-name "bin/macOS/lua-language-server" lsp-lua-server-root) nil :type (quote string))
(defcustom lsp-lua-server-main (expand-file-name "main.lua" lsp-lua-server-root) nil :type (quote string))

(lsp-register-custom-settings
 (quote (("Lua.workspace.useGitIgnore" lsp-lua-workspace-use-git-ignore t)
         ("Lua.workspace.preloadFileSize" lsp-lua-workspace-preload-file-size)
         ("Lua.workspace.maxPreload" lsp-lua-workspace-max-preload)
         ("Lua.workspace.library" lsp-lua-workspace-library)
         ("Lua.workspace.ignoreSubmodules" lsp-lua-workspace-ignore-submodules t)
         ("Lua.workspace.ignoreDir" lsp-lua-workspace-ignore-dir)
         ("Lua.runtime.version" lsp-lua-runtime-version)
         ("Lua.runtime.path" lsp-lua-runtime-path)
         ("Lua.diagnostics.severity" lsp-lua-diagnostics-severity)
         ("Lua.diagnostics.globals" lsp-lua-diagnostics-globals)
         ("Lua.diagnostics.enable" lsp-lua-diagnostics-enable t)
         ("Lua.diagnostics.disable" lsp-lua-diagnostics-disable)
         ("Lua.develop.enable" lsp-lua-develop-enable t)
         ("Lua.develop.debuggerWait" lsp-lua-develop-debugger-wait t)
         ("Lua.develop.debuggerPort" lsp-lua-develop-debugger-port)
         ("Lua.completion.keywordSnippet" lsp-lua-completion-keyword-snippet)
         ("Lua.completion.enable" lsp-lua-completion-enable t)
         ("Lua.completion.callSnippet" lsp-lua-completion-call-snippet)
         ("Lua.awakened.cat" lsp-lua-awakened-cat t))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection `(,lsp-lua-server-binary "-E" "-e" "LANG=\"en\"", lsp-lua-server-main) (lambda() t))
                  :major-modes '(lua-mode)
                  :priority 0
                  :server-id 'lsp-lua))

(provide 'init-lsp-lua)
;;; init-lsp-lua.el ends here
