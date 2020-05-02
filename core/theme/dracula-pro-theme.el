;;; dracula-theme.el --- Dracula Theme

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Author: film42
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/dracula/emacs

;;; Commentary:

;; A dark color theme available for a number of editors.

;;; Code:
(require 'cl-lib)
(deftheme dracula-pro)

;;;; Configuration options:

(defgroup dracula-pro nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-pro-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula-pro)

(defcustom dracula-pro-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'dracula-pro)

(defcustom dracula-pro-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula-pro)

(defcustom dracula-pro-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'dracula-pro)

(defcustom dracula-pro-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula-pro)

(defcustom dracula-pro-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula-pro)

(defvar dracula-pro-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Dracula-Pro theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/dracula-pro/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (dracula-pro-bg      "#22212C" "unspecified-bg" "unspecified-bg") ; official background
                (dracula-pro-fg      "#F8F8F2" "#F8F8F2" "brightwhite") ; official foreground
                (dracula-pro-current "#454158" "#454158" "brightblack") ; official current-line/selection
                (dracula-pro-comment "#7970A9" "#7970A9" "blue")        ; official comment
                (dracula-pro-cyan    "#80FFEA" "#80FFEA" "brightcyan")  ; official cyan
                (dracula-pro-green   "#8AFF80" "#8AFF80" "green")       ; official green
                (dracula-pro-orange  "#FFCA80" "#FFCA80" "brightred")   ; official orange
                (dracula-pro-pink    "#FF80BF" "#FF80BF" "magenta")     ; official pink
                (dracula-pro-purple  "#9580FF" "#9580FF" "brightmagenta") ; official purple
                (dracula-pro-red     "#FF9580" "#FF9580" "red")         ; official red
                (dracula-pro-yellow  "#FFFF80" "#FFFF80" "yellow")      ; official yellow
                ;; Other colors
                (bg2             "#373844" "#121212" "brightblack")
                (bg3             "#464752" "#262626" "brightblack")
                (bg4             "#565761" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (faces '(;; default
               (cursor :background ,fg3)
               (completions-first-difference :foreground ,dracula-pro-pink :weight bold)
               (default :background ,dracula-pro-bg :foreground ,dracula-pro-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,dracula-pro-bg :foreground ,fg4)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,dracula-pro-current :extend t)
               (info-quoted-name :foreground ,dracula-pro-orange)
               (info-string :foreground ,dracula-pro-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,dracula-pro-cyan :underline t)
               (linum :slant italic :foreground ,bg4 :background ,dracula-pro-bg)
               (line-number :slant italic :foreground ,bg4 :background ,dracula-pro-bg)
               (match :background ,dracula-pro-yellow :foreground ,dracula-pro-bg)
               (minibuffer-prompt
                ,@(if dracula-pro-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-fg)
                    (list :weight 'bold :foreground dracula-pro-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground "unspecified-fg" :background ,dracula-pro-orange)
               (vertical-border :foreground ,bg2)
               (success :foreground ,dracula-pro-green)
               (warning :foreground ,dracula-pro-orange)
               (error :foreground ,dracula-pro-red)
               (header-line :background ,dracula-pro-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,dracula-pro-orange)
               (font-lock-comment-face :foreground ,dracula-pro-comment)
               (font-lock-comment-delimiter-face :foreground ,dracula-pro-comment)
               (font-lock-constant-face :foreground ,dracula-pro-cyan)
               (font-lock-doc-face :foreground ,dracula-pro-comment)
               (font-lock-function-name-face :foreground ,dracula-pro-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,dracula-pro-pink)
               (font-lock-negation-char-face :foreground ,dracula-pro-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-pro-orange)
               (font-lock-reference-face :foreground ,dracula-pro-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,dracula-pro-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-pro-purple)
               (font-lock-string-face :foreground ,dracula-pro-yellow)
               (font-lock-type-face :foreground ,dracula-pro-purple)
               (font-lock-variable-name-face :foreground ,dracula-pro-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,dracula-pro-orange :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,dracula-pro-pink)
               ;; company
               (company-echo-common :foreground ,dracula-pro-bg :background ,dracula-pro-fg)
               (company-preview :background ,dracula-pro-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,dracula-pro-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,dracula-pro-green)
               (company-scrollbar-bg :background ,dracula-pro-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,dracula-pro-fg :background ,dracula-pro-current)
               (company-tooltip-search :foreground ,dracula-pro-green
                                       :underline t)
               (company-tooltip-search-selection :background ,dracula-pro-green
                                                 :foreground ,dracula-pro-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,dracula-pro-bg)
               (company-tooltip-common :foreground ,dracula-pro-pink :weight bold)
               ;;(company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,dracula-pro-cyan)
               ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
               ;; diff-hl
               (diff-hl-change :foreground ,dracula-pro-orange :background ,dracula-pro-orange)
               (diff-hl-delete :foreground ,dracula-pro-red :background ,dracula-pro-red)
               (diff-hl-insert :foreground ,dracula-pro-green :background ,dracula-pro-green)
               ;; dired
               (dired-directory :foreground ,dracula-pro-green :weight normal)
               (dired-flagged :foreground ,dracula-pro-pink)
               (dired-header :foreground ,fg3 :background ,dracula-pro-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-pro-fg :weight bold)
               (dired-marked :foreground ,dracula-pro-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,dracula-pro-yellow :weight normal :slant italic)
               (dired-warning :foreground ,dracula-pro-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,dracula-pro-fg)
               (diredp-deletion-file-name :foreground ,dracula-pro-pink :background ,dracula-pro-current)
               (diredp-deletion :foreground ,dracula-pro-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,dracula-pro-orange)
               (diredp-file-name :foreground ,dracula-pro-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,dracula-pro-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,dracula-pro-current)
               (diredp-ignored-file-name :foreground ,dracula-pro-fg)
               (diredp-mode-line-flagged :foreground ,dracula-pro-orange)
               (diredp-mode-line-marked :foreground ,dracula-pro-orange)
               (diredp-no-priv :foreground ,dracula-pro-fg)
               (diredp-number :foreground ,dracula-pro-cyan)
               (diredp-other-priv :foreground ,dracula-pro-orange)
               (diredp-rare-priv :foreground ,dracula-pro-orange)
               (diredp-read-priv :foreground ,dracula-pro-purple)
               (diredp-write-priv :foreground ,dracula-pro-pink)
               (diredp-exec-priv :foreground ,dracula-pro-yellow)
               (diredp-symlink :foreground ,dracula-pro-orange)
               (diredp-link-priv :foreground ,dracula-pro-orange)
               (diredp-autofile-name :foreground ,dracula-pro-yellow)
               (diredp-tagged-autofile-name :foreground ,dracula-pro-yellow)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,dracula-pro-yellow)
               (enh-ruby-op-face :foreground ,dracula-pro-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,dracula-pro-yellow)
               (enh-ruby-string-delimiter-face :foreground ,dracula-pro-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-pro-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-pro-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,dracula-pro-purple)
               (font-latex-italic-face :foreground ,dracula-pro-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,dracula-pro-cyan)
               (font-latex-match-variable-keywords :foreground ,dracula-pro-fg)
               (font-latex-string-face :foreground ,dracula-pro-yellow)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,dracula-pro-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,dracula-pro-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,dracula-pro-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,dracula-pro-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,dracula-pro-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,dracula-pro-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,dracula-pro-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,dracula-pro-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,dracula-pro-pink)
               (gnus-header-from :foreground ,dracula-pro-fg)
               (gnus-header-name :foreground ,dracula-pro-purple)
               (gnus-header-subject :foreground ,dracula-pro-green :weight bold)
               (gnus-summary-markup-face :foreground ,dracula-pro-cyan)
               (gnus-summary-high-unread :foreground ,dracula-pro-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,dracula-pro-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,dracula-pro-pink :weight bold)
               (gnus-summary-low-unread :foreground ,dracula-pro-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,dracula-pro-pink)
               (haskell-constructor-face :foreground ,dracula-pro-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,dracula-pro-purple)
               (helm-buffer-not-saved :foreground ,dracula-pro-purple :background ,dracula-pro-bg)
               (helm-buffer-process :foreground ,dracula-pro-orange :background ,dracula-pro-bg)
               (helm-buffer-saved-out :foreground ,dracula-pro-fg :background ,dracula-pro-bg)
               (helm-buffer-size :foreground ,dracula-pro-fg :background ,dracula-pro-bg)
               (helm-candidate-number :foreground ,dracula-pro-bg :background ,dracula-pro-fg)
               (helm-ff-directory :foreground ,dracula-pro-green :background ,dracula-pro-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,dracula-pro-green :background ,dracula-pro-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,dracula-pro-bg :weight normal)
               (helm-ff-file :foreground ,dracula-pro-fg :background ,dracula-pro-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,dracula-pro-pink :background ,dracula-pro-bg :weight bold)
               (helm-ff-prefix :foreground ,dracula-pro-bg :background ,dracula-pro-pink :weight normal)
               (helm-ff-symlink :foreground ,dracula-pro-pink :background ,dracula-pro-bg :weight bold)
               (helm-grep-cmd-line :foreground ,dracula-pro-fg :background ,dracula-pro-bg)
               (helm-grep-file :foreground ,dracula-pro-fg :background ,dracula-pro-bg)
               (helm-grep-finish :foreground ,fg2 :background ,dracula-pro-bg)
               (helm-grep-lineno :foreground ,dracula-pro-fg :background ,dracula-pro-bg)
               (helm-grep-match :foreground "unspecified-fg" :background "unspecified-bg" :inherit helm-match)
               (helm-grep-running :foreground ,dracula-pro-green :background ,dracula-pro-bg)
               (helm-header :foreground ,fg2 :background ,dracula-pro-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,dracula-pro-green :background ,dracula-pro-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,dracula-pro-purple :background ,dracula-pro-bg)
               (helm-source-go-package-godoc-description :foreground ,dracula-pro-yellow)
               (helm-source-header :foreground ,dracula-pro-pink :background ,dracula-pro-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,dracula-pro-orange :background ,dracula-pro-bg)
               (helm-time-zone-home :foreground ,dracula-pro-purple :background ,dracula-pro-bg)
               (helm-visible-mark :foreground ,dracula-pro-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,dracula-pro-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,dracula-pro-fg)
               (icicle-search-current-input :foreground ,dracula-pro-pink)
               (icicle-search-context-level-8 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-7 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-6 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-5 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-4 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-3 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-2 :foreground ,dracula-pro-orange)
               (icicle-search-context-level-1 :foreground ,dracula-pro-orange)
               (icicle-search-main-regexp-current :foreground ,dracula-pro-fg)
               (icicle-saved-candidate :foreground ,dracula-pro-fg)
               (icicle-proxy-candidate :foreground ,dracula-pro-fg)
               (icicle-mustmatch-completion :foreground ,dracula-pro-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,dracula-pro-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,dracula-pro-orange)
               (icicle-match-highlight-Completions :foreground ,dracula-pro-green)
               (icicle-key-complete-menu-local :foreground ,dracula-pro-fg)
               (icicle-key-complete-menu :foreground ,dracula-pro-fg)
               (icicle-input-completion-fail-lax :foreground ,dracula-pro-pink)
               (icicle-input-completion-fail :foreground ,dracula-pro-pink)
               (icicle-historical-candidate-other :foreground ,dracula-pro-fg)
               (icicle-historical-candidate :foreground ,dracula-pro-fg)
               (icicle-current-candidate-highlight :foreground ,dracula-pro-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,dracula-pro-fg)
               (icicle-complete-input :foreground ,dracula-pro-orange)
               (icicle-common-match-highlight-Completions :foreground ,dracula-pro-purple)
               (icicle-candidate-part :foreground ,dracula-pro-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,dracula-pro-orange)
               ;; ido
               (ido-first-match
                ,@(if dracula-pro-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-pro-green)
                    (list :weight 'bold :foreground dracula-pro-pink)))
               (ido-only-match :foreground ,dracula-pro-orange)
               (ido-subdir :foreground ,dracula-pro-yellow)
               (ido-virtual :foreground ,dracula-pro-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,dracula-pro-fg :background ,dracula-pro-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-pro-bg :background ,dracula-pro-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,dracula-pro-cyan)
               (jde-java-font-lock-modifier-face :foreground ,dracula-pro-pink)
               (jde-java-font-lock-number-face :foreground ,dracula-pro-fg)
               (jde-java-font-lock-package-face :foreground ,dracula-pro-fg)
               (jde-java-font-lock-private-face :foreground ,dracula-pro-pink)
               (jde-java-font-lock-public-face :foreground ,dracula-pro-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,dracula-pro-purple)
               (js2-function-param :foreground ,dracula-pro-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,dracula-pro-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,dracula-pro-yellow)
               (js2-private-function-call :foreground ,dracula-pro-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,dracula-pro-orange)
               (js3-external-variable-face :foreground ,dracula-pro-fg)
               (js3-function-param-face :foreground ,dracula-pro-pink)
               (js3-instance-member-face :foreground ,dracula-pro-cyan)
               (js3-jsdoc-tag-face :foreground ,dracula-pro-pink)
               (js3-warning-face :underline ,dracula-pro-pink)
               ;; magit
               (magit-branch-local :foreground ,dracula-pro-cyan)
               (magit-branch-remote :foreground ,dracula-pro-green)
               (magit-tag :foreground ,dracula-pro-orange)
               (magit-section-heading :foreground ,dracula-pro-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-pro-orange
                                            :background ,dracula-pro-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-pro-orange
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,dracula-pro-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-pro-green)
               (magit-diffstat-removed :foreground ,dracula-pro-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,dracula-pro-orange :weight bold)
               (magit-process-ok :foreground ,dracula-pro-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-pro-orange)
               (markdown-code-face :foreground ,dracula-pro-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pro-pink
                ,@(when dracula-pro-enlarge-headings
                    (list :height dracula-pro-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-pro-purple
                ,@(when dracula-pro-enlarge-headings
                    (list :height dracula-pro-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-pro-green
                ,@(when dracula-pro-enlarge-headings
                    (list :height dracula-pro-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-pro-yellow)
               (markdown-header-face-5 :foreground ,dracula-pro-cyan)
               (markdown-header-face-6 :foreground ,dracula-pro-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,dracula-pro-fg)
               (markdown-inline-code-face :foreground ,dracula-pro-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-pro-orange)
               (markdown-table-face :foreground ,dracula-pro-purple)
               ;; message
               (message-mml :foreground ,dracula-pro-green :weight normal)
               (message-header-xheader :foreground ,dracula-pro-cyan :weight normal)
               ;; mode-line
               (mode-line :background ,dracula-pro-current
                          :box ,dracula-pro-current :inverse-video nil
                          ,@(if dracula-pro-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground "unspecified-fg")))
               (mode-line-inactive
                :inverse-video nil
                ,@(if dracula-pro-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-pro-comment :background dracula-pro-bg
                            :box dracula-pro-bg)
                    (list :foreground dracula-pro-fg :background bg2 :box bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,dracula-pro-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,dracula-pro-purple)
               (mu4e-highlight-face :background ,dracula-pro-bg
                                    :foreground ,dracula-pro-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,dracula-pro-current
                                           :foreground ,dracula-pro-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,dracula-pro-purple)
               (mu4e-cited-1-face :foreground ,dracula-pro-purple)
               (mu4e-cited-2-face :foreground ,dracula-pro-orange)
               (mu4e-cited-3-face :foreground ,dracula-pro-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; org
               (org-agenda-date :foreground ,dracula-pro-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-pro-comment)
               (org-agenda-done :foreground ,dracula-pro-green)
               (org-agenda-structure :foreground ,dracula-pro-purple)
               (org-block :foreground ,dracula-pro-orange)
               (org-code :foreground ,dracula-pro-yellow)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-pro-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,dracula-pro-comment)
               (org-document-title :weight bold :foreground ,dracula-pro-orange
                                   ,@(when dracula-pro-enlarge-headings
                                       (list :height dracula-pro-height-doc-title)))
               (org-done :foreground ,dracula-pro-green)
               (org-ellipsis :foreground ,dracula-pro-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,dracula-pro-pink)
               (org-headline-done :foreground ,dracula-pro-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-pro-bg :background ,dracula-pro-bg)
               (org-level-1 :inherit bold :foreground ,dracula-pro-pink
                            ,@(when dracula-pro-enlarge-headings
                                (list :height dracula-pro-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-pro-purple
                            ,@(when dracula-pro-enlarge-headings
                                (list :height dracula-pro-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-pro-green
                            ,@(when dracula-pro-enlarge-headings
                                (list :height dracula-pro-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-pro-yellow)
               (org-level-5 :weight normal :foreground ,dracula-pro-cyan)
               (org-level-6 :weight normal :foreground ,dracula-pro-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,dracula-pro-fg)
               (org-link :foreground ,dracula-pro-cyan :underline t)
               (org-priority :foreground ,dracula-pro-cyan)
               (org-scheduled :foreground ,dracula-pro-green)
               (org-scheduled-previously :foreground ,dracula-pro-yellow)
               (org-scheduled-today :foreground ,dracula-pro-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,dracula-pro-yellow)
               (org-table :foreground ,dracula-pro-purple)
               (org-tag :foreground ,dracula-pro-pink :weight bold :background ,bg2)
               (org-todo :foreground ,dracula-pro-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,dracula-pro-yellow)
               (org-warning :weight bold :foreground ,dracula-pro-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pro-pink)
               (outline-2 :foreground ,dracula-pro-purple)
               (outline-3 :foreground ,dracula-pro-green)
               (outline-4 :foreground ,dracula-pro-yellow)
               (outline-5 :foreground ,dracula-pro-cyan)
               (outline-6 :foreground ,dracula-pro-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,dracula-pro-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,dracula-pro-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,dracula-pro-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,dracula-pro-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,dracula-pro-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,dracula-pro-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,dracula-pro-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,dracula-pro-fg)
               (rainbow-delimiters-depth-2-face :foreground ,dracula-pro-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,dracula-pro-purple)
               (rainbow-delimiters-depth-4-face :foreground ,dracula-pro-pink)
               (rainbow-delimiters-depth-5-face :foreground ,dracula-pro-orange)
               (rainbow-delimiters-depth-6-face :foreground ,dracula-pro-green)
               (rainbow-delimiters-depth-7-face :foreground ,dracula-pro-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,dracula-pro-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,dracula-pro-green)
               (rpm-spec-doc-face :foreground ,dracula-pro-pink)
               (rpm-spec-ghost-face :foreground ,dracula-pro-purple)
               (rpm-spec-macro-face :foreground ,dracula-pro-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,dracula-pro-purple)
               (rpm-spec-section-face :foreground ,dracula-pro-yellow)
               (rpm-spec-tag-face :foreground ,dracula-pro-cyan)
               (rpm-spec-var-face :foreground ,dracula-pro-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-pro-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-pro-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,dracula-pro-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,dracula-pro-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,dracula-pro-green)
               (speedbar-file-face :foreground ,dracula-pro-cyan)
               (speedbar-directory-face :foreground ,dracula-pro-purple)
               (speedbar-tag-face :foreground ,dracula-pro-yellow)
               (speedbar-selected-face :foreground ,dracula-pro-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,dracula-pro-bg
                                        :foreground ,dracula-pro-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-pro-purple :background ,dracula-pro-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-pro-pink :background ,dracula-pro-bg
                            :box (:line-width 2 :color ,dracula-pro-bg :style nil))
               (tab-bar-tab-inactive :foreground ,dracula-pro-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,dracula-pro-purple :background ,dracula-pro-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,dracula-pro-pink :background ,dracula-pro-bg
                             :box (:line-width 2 :color ,dracula-pro-bg :style nil))
               (tab-line-tab-inactive :foreground ,dracula-pro-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,dracula-pro-red)
               ;; term
               (term :foreground ,dracula-pro-fg :background ,dracula-pro-bg)
               (term-color-black :foreground ,dracula-pro-bg :background ,dracula-pro-bg)
               (term-color-blue :foreground ,dracula-pro-purple :background ,dracula-pro-purple)
               (term-color-cyan :foreground ,dracula-pro-cyan :background ,dracula-pro-cyan)
               (term-color-green :foreground ,dracula-pro-green :background ,dracula-pro-green)
               (term-color-magenta :foreground ,dracula-pro-pink :background ,dracula-pro-pink)
               (term-color-red :foreground ,dracula-pro-red :background ,dracula-pro-red)
               (term-color-white :foreground ,dracula-pro-fg :background ,dracula-pro-fg)
               (term-color-yellow :foreground ,dracula-pro-yellow :background ,dracula-pro-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,dracula-pro-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,dracula-pro-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,dracula-pro-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,dracula-pro-purple)
               (web-mode-html-attr-value-face :foreground ,dracula-pro-green)
               (web-mode-html-tag-face :foreground ,dracula-pro-pink :weight bold)
               (web-mode-keyword-face :foreground ,dracula-pro-pink)
               (web-mode-string-face :foreground ,dracula-pro-yellow)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,dracula-pro-red :foreground ,dracula-pro-red)
               (whitespace-empty :background ,dracula-pro-orange :foreground ,dracula-pro-red)
               (whitespace-hspace :background ,bg3 :foreground ,dracula-pro-comment)
               (whitespace-indentation :background ,dracula-pro-orange :foreground ,dracula-pro-red)
               (whitespace-line :background ,dracula-pro-bg :foreground ,dracula-pro-pink)
               (whitespace-newline :foreground ,dracula-pro-comment)
               (whitespace-space :background ,dracula-pro-bg :foreground ,dracula-pro-comment)
               (whitespace-space-after-tab :background ,dracula-pro-orange :foreground ,dracula-pro-red)
               (whitespace-space-before-tab :background ,dracula-pro-orange :foreground ,dracula-pro-red)
               (whitespace-tab :background ,bg2 :foreground ,dracula-pro-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'dracula-pro
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               (expand-for-kind
                (lambda (kind spec)
                  (when (and (string= (symbol-name kind) "term-colors")
                             dracula-pro-use-24-bit-colors-on-256-colors-terms)
                    (setq kind 'graphic-colors))
                  (cl-progv color-names (symbol-value kind)
                    (eval `(backquote ,spec))))))
           (cl-loop for (face . spec) in faces
                    collect `(,face
                              ((((min-colors 16777216)) ; fully graphical envs
                                ,(funcall expand-for-kind 'graphic-colors spec))
                               (((min-colors 256))      ; terminal withs 256 colors
                                ,(funcall expand-for-kind 'term-colors spec))
                               (t                       ; should be only tty-like envs
                                ,(funcall expand-for-kind 'tty-colors spec))))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-pro)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; dracula-pro-theme.el ends here
