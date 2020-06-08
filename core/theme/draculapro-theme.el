;;; draculapro-theme.el --- Dracula Pro Theme

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
(deftheme draculapro)

;;;; Configuration options:

(defgroup draculapro nil
  "Dracula Pro theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom draculapro-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'draculapro)

(defcustom draculapro-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'draculapro)

(defcustom draculapro-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'draculapro)

(defcustom draculapro-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'draculapro)

(defcustom draculapro-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'draculapro)

(defcustom draculapro-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'draculapro)

(defvar draculapro-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Dracula Pro theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/dracula/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")

;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (draculapro-bg      "#17161D" "color-235") ; official background
                (draculapro-fg      "#F8F8F2" "color-253") ; official foreground
                (draculapro-current "#454158" "color-239") ; official current-line/selection
                (draculapro-comment "#7970A9" "color-61")  ; official comment
                (draculapro-cyan    "#80FFEA" "color-117") ; official cyan
                (draculapro-green   "#8AFF80" "color-84")  ; official green
                (draculapro-orange  "#FFCA80" "color-215") ; official orange
                (draculapro-pink    "#FF80BF" "color-212") ; official pink
                (draculapro-purple  "#9580FF" "color-141") ; official purple
                (draculapro-red     "#FF9580" "color-203") ; official red
                (draculapro-yellow  "#FFFF80" "color-228") ; official yellow
                ;; Other colors
                (bg2             "#0B0B0F" "color-234")
                (bg3             "#17161D" "color-235")
                (bg4             "#7970A9" "color-61")
                (fg2             "#F8F8F2" "color-253")
                (fg3             "#F8F8F2" "color-253")
                (fg4             "#F8F8F2" "color-253")
                (other-blue      "#7970A9" "color-61")))
      (faces '(;; default
               (cursor :background ,fg3)
               (completions-first-difference :foreground ,draculapro-pink :weight bold)
               (default :background ,draculapro-bg :foreground ,draculapro-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,draculapro-bg :foreground ,fg4)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,draculapro-current :extend t)
               (info-quoted-name :foreground ,draculapro-orange)
               (info-string :foreground ,draculapro-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,draculapro-cyan :underline t)
               (linum :slant italic :foreground ,bg4 :background ,draculapro-bg)
               (line-number :slant italic :foreground ,bg4 :background ,draculapro-bg)
               (match :background ,draculapro-yellow :foreground ,draculapro-bg)
               (minibuffer-prompt
                ,@(if draculapro-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground draculapro-fg)
                    (list :weight 'bold :foreground draculapro-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground "unspecified-fg" :background ,draculapro-orange)
               (vertical-border :foreground ,bg2)
               (success :foreground ,draculapro-green)
               (warning :foreground ,draculapro-orange)
               (error :foreground ,draculapro-red)
               (header-line :background ,draculapro-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,draculapro-orange)
               (font-lock-comment-face :foreground ,draculapro-comment)
               (font-lock-comment-delimiter-face :foreground ,draculapro-comment)
               (font-lock-constant-face :foreground ,draculapro-cyan)
               (font-lock-doc-face :foreground ,draculapro-comment)
               (font-lock-function-name-face :foreground ,draculapro-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,draculapro-pink)
               (font-lock-negation-char-face :foreground ,draculapro-cyan)
               (font-lock-preprocessor-face :foreground ,draculapro-orange)
               (font-lock-reference-face :foreground ,draculapro-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,draculapro-cyan)
               (font-lock-regexp-grouping-construct :foreground ,draculapro-purple)
               (font-lock-string-face :foreground ,draculapro-yellow)
               (font-lock-type-face :foreground ,draculapro-purple)
               (font-lock-variable-name-face :foreground ,draculapro-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,draculapro-orange :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,draculapro-pink)
               ;; company
               (company-echo-common :foreground ,draculapro-bg :background ,draculapro-fg)
               (company-preview :background ,draculapro-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,draculapro-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,draculapro-green)
               (company-scrollbar-bg :background ,draculapro-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,draculapro-fg :background ,draculapro-current)
               (company-tooltip-search :foreground ,draculapro-green
                                       :underline t)
               (company-tooltip-search-selection :background ,draculapro-green
                                                 :foreground ,draculapro-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,draculapro-bg)
               (company-tooltip-common :foreground ,draculapro-pink :weight bold)
               ;;(company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,draculapro-cyan)
               ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
               ;; diff-hl
               (diff-hl-change :foreground ,draculapro-orange :background ,draculapro-orange)
               (diff-hl-delete :foreground ,draculapro-red :background ,draculapro-red)
               (diff-hl-insert :foreground ,draculapro-green :background ,draculapro-green)
               ;; dired
               (dired-directory :foreground ,draculapro-green :weight normal)
               (dired-flagged :foreground ,draculapro-pink)
               (dired-header :foreground ,fg3 :background ,draculapro-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,draculapro-fg :weight bold)
               (dired-marked :foreground ,draculapro-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,draculapro-yellow :weight normal :slant italic)
               (dired-warning :foreground ,draculapro-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,draculapro-fg)
               (diredp-deletion-file-name :foreground ,draculapro-pink :background ,draculapro-current)
               (diredp-deletion :foreground ,draculapro-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,draculapro-orange)
               (diredp-file-name :foreground ,draculapro-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,draculapro-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,draculapro-current)
               (diredp-ignored-file-name :foreground ,draculapro-fg)
               (diredp-mode-line-flagged :foreground ,draculapro-orange)
               (diredp-mode-line-marked :foreground ,draculapro-orange)
               (diredp-no-priv :foreground ,draculapro-fg)
               (diredp-number :foreground ,draculapro-cyan)
               (diredp-other-priv :foreground ,draculapro-orange)
               (diredp-rare-priv :foreground ,draculapro-orange)
               (diredp-read-priv :foreground ,draculapro-purple)
               (diredp-write-priv :foreground ,draculapro-pink)
               (diredp-exec-priv :foreground ,draculapro-yellow)
               (diredp-symlink :foreground ,draculapro-orange)
               (diredp-link-priv :foreground ,draculapro-orange)
               (diredp-autofile-name :foreground ,draculapro-yellow)
               (diredp-tagged-autofile-name :foreground ,draculapro-yellow)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,draculapro-yellow)
               (enh-ruby-op-face :foreground ,draculapro-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,draculapro-yellow)
               (enh-ruby-string-delimiter-face :foreground ,draculapro-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,draculapro-orange))
               (flyspell-incorrect :underline (:style wave :color ,draculapro-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,draculapro-purple)
               (font-latex-italic-face :foreground ,draculapro-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,draculapro-cyan)
               (font-latex-match-variable-keywords :foreground ,draculapro-fg)
               (font-latex-string-face :foreground ,draculapro-yellow)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,draculapro-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,draculapro-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,draculapro-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,draculapro-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,draculapro-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,draculapro-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,draculapro-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,draculapro-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,draculapro-pink)
               (gnus-header-from :foreground ,draculapro-fg)
               (gnus-header-name :foreground ,draculapro-purple)
               (gnus-header-subject :foreground ,draculapro-green :weight bold)
               (gnus-summary-markup-face :foreground ,draculapro-cyan)
               (gnus-summary-high-unread :foreground ,draculapro-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,draculapro-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,draculapro-pink :weight bold)
               (gnus-summary-low-unread :foreground ,draculapro-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,draculapro-pink)
               (haskell-constructor-face :foreground ,draculapro-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,draculapro-purple)
               (helm-buffer-not-saved :foreground ,draculapro-purple :background ,draculapro-bg)
               (helm-buffer-process :foreground ,draculapro-orange :background ,draculapro-bg)
               (helm-buffer-saved-out :foreground ,draculapro-fg :background ,draculapro-bg)
               (helm-buffer-size :foreground ,draculapro-fg :background ,draculapro-bg)
               (helm-candidate-number :foreground ,draculapro-bg :background ,draculapro-fg)
               (helm-ff-directory :foreground ,draculapro-green :background ,draculapro-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,draculapro-green :background ,draculapro-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,draculapro-bg :weight normal)
               (helm-ff-file :foreground ,draculapro-fg :background ,draculapro-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,draculapro-pink :background ,draculapro-bg :weight bold)
               (helm-ff-prefix :foreground ,draculapro-bg :background ,draculapro-pink :weight normal)
               (helm-ff-symlink :foreground ,draculapro-pink :background ,draculapro-bg :weight bold)
               (helm-grep-cmd-line :foreground ,draculapro-fg :background ,draculapro-bg)
               (helm-grep-file :foreground ,draculapro-fg :background ,draculapro-bg)
               (helm-grep-finish :foreground ,fg2 :background ,draculapro-bg)
               (helm-grep-lineno :foreground ,draculapro-fg :background ,draculapro-bg)
               (helm-grep-match :foreground "unspecified-fg" :background "unspecified-bg" :inherit helm-match)
               (helm-grep-running :foreground ,draculapro-green :background ,draculapro-bg)
               (helm-header :foreground ,fg2 :background ,draculapro-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,draculapro-green :background ,draculapro-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,draculapro-purple :background ,draculapro-bg)
               (helm-source-go-package-godoc-description :foreground ,draculapro-yellow)
               (helm-source-header :foreground ,draculapro-pink :background ,draculapro-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,draculapro-orange :background ,draculapro-bg)
               (helm-time-zone-home :foreground ,draculapro-purple :background ,draculapro-bg)
               (helm-visible-mark :foreground ,draculapro-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,draculapro-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,draculapro-fg)
               (icicle-search-current-input :foreground ,draculapro-pink)
               (icicle-search-context-level-8 :foreground ,draculapro-orange)
               (icicle-search-context-level-7 :foreground ,draculapro-orange)
               (icicle-search-context-level-6 :foreground ,draculapro-orange)
               (icicle-search-context-level-5 :foreground ,draculapro-orange)
               (icicle-search-context-level-4 :foreground ,draculapro-orange)
               (icicle-search-context-level-3 :foreground ,draculapro-orange)
               (icicle-search-context-level-2 :foreground ,draculapro-orange)
               (icicle-search-context-level-1 :foreground ,draculapro-orange)
               (icicle-search-main-regexp-current :foreground ,draculapro-fg)
               (icicle-saved-candidate :foreground ,draculapro-fg)
               (icicle-proxy-candidate :foreground ,draculapro-fg)
               (icicle-mustmatch-completion :foreground ,draculapro-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,draculapro-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,draculapro-orange)
               (icicle-match-highlight-Completions :foreground ,draculapro-green)
               (icicle-key-complete-menu-local :foreground ,draculapro-fg)
               (icicle-key-complete-menu :foreground ,draculapro-fg)
               (icicle-input-completion-fail-lax :foreground ,draculapro-pink)
               (icicle-input-completion-fail :foreground ,draculapro-pink)
               (icicle-historical-candidate-other :foreground ,draculapro-fg)
               (icicle-historical-candidate :foreground ,draculapro-fg)
               (icicle-current-candidate-highlight :foreground ,draculapro-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,draculapro-fg)
               (icicle-complete-input :foreground ,draculapro-orange)
               (icicle-common-match-highlight-Completions :foreground ,draculapro-purple)
               (icicle-candidate-part :foreground ,draculapro-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,draculapro-orange)
               ;; ido
               (ido-first-match
                ,@(if draculapro-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground draculapro-green)
                    (list :weight 'bold :foreground draculapro-pink)))
               (ido-only-match :foreground ,draculapro-orange)
               (ido-subdir :foreground ,draculapro-yellow)
               (ido-virtual :foreground ,draculapro-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,draculapro-fg :background ,draculapro-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,draculapro-bg :background ,draculapro-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,draculapro-cyan)
               (jde-java-font-lock-modifier-face :foreground ,draculapro-pink)
               (jde-java-font-lock-number-face :foreground ,draculapro-fg)
               (jde-java-font-lock-package-face :foreground ,draculapro-fg)
               (jde-java-font-lock-private-face :foreground ,draculapro-pink)
               (jde-java-font-lock-public-face :foreground ,draculapro-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,draculapro-purple)
               (js2-function-param :foreground ,draculapro-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,draculapro-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,draculapro-yellow)
               (js2-private-function-call :foreground ,draculapro-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,draculapro-orange)
               (js3-external-variable-face :foreground ,draculapro-fg)
               (js3-function-param-face :foreground ,draculapro-pink)
               (js3-instance-member-face :foreground ,draculapro-cyan)
               (js3-jsdoc-tag-face :foreground ,draculapro-pink)
               (js3-warning-face :underline ,draculapro-pink)
               ;; magit
               (magit-branch-local :foreground ,draculapro-cyan)
               (magit-branch-remote :foreground ,draculapro-green)
               (magit-tag :foreground ,draculapro-orange)
               (magit-section-heading :foreground ,draculapro-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,draculapro-orange
                                            :background ,draculapro-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,draculapro-orange
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
               (magit-diff-file-heading :foreground ,draculapro-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,draculapro-green)
               (magit-diffstat-removed :foreground ,draculapro-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,draculapro-orange :weight bold)
               (magit-process-ok :foreground ,draculapro-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,draculapro-orange)
               (markdown-code-face :foreground ,draculapro-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,draculapro-pink
                ,@(when draculapro-enlarge-headings
                    (list :height draculapro-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,draculapro-purple
                ,@(when draculapro-enlarge-headings
                    (list :height draculapro-height-title-2)))
               (markdown-header-face-3
                :foreground ,draculapro-green
                ,@(when draculapro-enlarge-headings
                    (list :height draculapro-height-title-3)))
               (markdown-header-face-4 :foreground ,draculapro-yellow)
               (markdown-header-face-5 :foreground ,draculapro-cyan)
               (markdown-header-face-6 :foreground ,draculapro-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,draculapro-fg)
               (markdown-inline-code-face :foreground ,draculapro-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,draculapro-orange)
               (markdown-table-face :foreground ,draculapro-purple)
               ;; message
               (message-mml :foreground ,draculapro-green :weight normal)
               (message-header-xheader :foreground ,draculapro-cyan :weight normal)
               ;; mode-line
               (mode-line :background ,draculapro-current
                          :box ,draculapro-current :inverse-video nil
                          ,@(if draculapro-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground "unspecified-fg")))
               (mode-line-inactive
                :inverse-video nil
                ,@(if draculapro-alternate-mode-line-and-minibuffer
                      (list :foreground draculapro-comment :background draculapro-bg
                            :box draculapro-bg)
                    (list :foreground draculapro-fg :background bg2 :box bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,draculapro-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,draculapro-purple)
               (mu4e-highlight-face :background ,draculapro-bg
                                    :foreground ,draculapro-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,draculapro-current
                                           :foreground ,draculapro-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,draculapro-purple)
               (mu4e-cited-1-face :foreground ,draculapro-purple)
               (mu4e-cited-2-face :foreground ,draculapro-orange)
               (mu4e-cited-3-face :foreground ,draculapro-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; org
               (org-agenda-date :foreground ,draculapro-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,draculapro-comment)
               (org-agenda-done :foreground ,draculapro-green)
               (org-agenda-structure :foreground ,draculapro-purple)
               (org-block :foreground ,draculapro-orange)
               (org-code :foreground ,draculapro-yellow)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,draculapro-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,draculapro-comment)
               (org-document-title :weight bold :foreground ,draculapro-orange
                                   ,@(when draculapro-enlarge-headings
                                       (list :height draculapro-height-doc-title)))
               (org-done :foreground ,draculapro-green)
               (org-ellipsis :foreground ,draculapro-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,draculapro-pink)
               (org-headline-done :foreground ,draculapro-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,draculapro-bg :background ,draculapro-bg)
               (org-level-1 :inherit bold :foreground ,draculapro-pink
                            ,@(when draculapro-enlarge-headings
                                (list :height draculapro-height-title-1)))
               (org-level-2 :inherit bold :foreground ,draculapro-purple
                            ,@(when draculapro-enlarge-headings
                                (list :height draculapro-height-title-2)))
               (org-level-3 :weight normal :foreground ,draculapro-green
                            ,@(when draculapro-enlarge-headings
                                (list :height draculapro-height-title-3)))
               (org-level-4 :weight normal :foreground ,draculapro-yellow)
               (org-level-5 :weight normal :foreground ,draculapro-cyan)
               (org-level-6 :weight normal :foreground ,draculapro-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,draculapro-fg)
               (org-link :foreground ,draculapro-cyan :underline t)
               (org-priority :foreground ,draculapro-cyan)
               (org-scheduled :foreground ,draculapro-green)
               (org-scheduled-previously :foreground ,draculapro-yellow)
               (org-scheduled-today :foreground ,draculapro-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,draculapro-yellow)
               (org-table :foreground ,draculapro-purple)
               (org-tag :foreground ,draculapro-pink :weight bold :background ,bg2)
               (org-todo :foreground ,draculapro-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,draculapro-yellow)
               (org-warning :weight bold :foreground ,draculapro-pink)
               ;; outline
               (outline-1 :foreground ,draculapro-pink)
               (outline-2 :foreground ,draculapro-purple)
               (outline-3 :foreground ,draculapro-green)
               (outline-4 :foreground ,draculapro-yellow)
               (outline-5 :foreground ,draculapro-cyan)
               (outline-6 :foreground ,draculapro-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,draculapro-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,draculapro-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,draculapro-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,draculapro-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,draculapro-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,draculapro-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,draculapro-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,draculapro-fg)
               (rainbow-delimiters-depth-2-face :foreground ,draculapro-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,draculapro-purple)
               (rainbow-delimiters-depth-4-face :foreground ,draculapro-pink)
               (rainbow-delimiters-depth-5-face :foreground ,draculapro-orange)
               (rainbow-delimiters-depth-6-face :foreground ,draculapro-green)
               (rainbow-delimiters-depth-7-face :foreground ,draculapro-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,draculapro-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,draculapro-green)
               (rpm-spec-doc-face :foreground ,draculapro-pink)
               (rpm-spec-ghost-face :foreground ,draculapro-purple)
               (rpm-spec-macro-face :foreground ,draculapro-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,draculapro-purple)
               (rpm-spec-section-face :foreground ,draculapro-yellow)
               (rpm-spec-tag-face :foreground ,draculapro-cyan)
               (rpm-spec-var-face :foreground ,draculapro-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,draculapro-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,draculapro-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,draculapro-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,draculapro-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,draculapro-green)
               (speedbar-file-face :foreground ,draculapro-cyan)
               (speedbar-directory-face :foreground ,draculapro-purple)
               (speedbar-tag-face :foreground ,draculapro-yellow)
               (speedbar-selected-face :foreground ,draculapro-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,draculapro-bg
                                        :foreground ,draculapro-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,draculapro-purple :background ,draculapro-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,draculapro-pink :background ,draculapro-bg
                            :box (:line-width 2 :color ,draculapro-bg :style nil))
               (tab-bar-tab-inactive :foreground ,draculapro-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,draculapro-purple :background ,draculapro-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,draculapro-pink :background ,draculapro-bg
                             :box (:line-width 2 :color ,draculapro-bg :style nil))
               (tab-line-tab-inactive :foreground ,draculapro-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,draculapro-red)
               ;; term
               (term :foreground ,draculapro-fg :background ,draculapro-bg)
               (term-color-black :foreground ,draculapro-bg :background ,draculapro-bg)
               (term-color-blue :foreground ,draculapro-purple :background ,draculapro-purple)
               (term-color-cyan :foreground ,draculapro-cyan :background ,draculapro-cyan)
               (term-color-green :foreground ,draculapro-green :background ,draculapro-green)
               (term-color-magenta :foreground ,draculapro-pink :background ,draculapro-pink)
               (term-color-red :foreground ,draculapro-red :background ,draculapro-red)
               (term-color-white :foreground ,draculapro-fg :background ,draculapro-fg)
               (term-color-yellow :foreground ,draculapro-yellow :background ,draculapro-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,draculapro-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,draculapro-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,draculapro-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,draculapro-purple)
               (web-mode-html-attr-value-face :foreground ,draculapro-green)
               (web-mode-html-tag-face :foreground ,draculapro-pink :weight bold)
               (web-mode-keyword-face :foreground ,draculapro-pink)
               (web-mode-string-face :foreground ,draculapro-yellow)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,draculapro-red :foreground ,draculapro-red)
               (whitespace-empty :background ,draculapro-orange :foreground ,draculapro-red)
               (whitespace-hspace :background ,bg3 :foreground ,draculapro-comment)
               (whitespace-indentation :background ,draculapro-orange :foreground ,draculapro-red)
               (whitespace-line :background ,draculapro-bg :foreground ,draculapro-pink)
               (whitespace-newline :foreground ,draculapro-comment)
               (whitespace-space :background ,draculapro-bg :foreground ,draculapro-comment)
               (whitespace-space-after-tab :background ,draculapro-orange :foreground ,draculapro-red)
               (whitespace-space-before-tab :background ,draculapro-orange :foreground ,draculapro-red)
               (whitespace-tab :background ,bg2 :foreground ,draculapro-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'draculapro
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               (expand-for-kind
                (lambda (kind spec)
                  (when (and (string= (symbol-name kind) "term-colors")
                             draculapro-use-24-bit-colors-on-256-colors-terms)
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

(provide-theme 'draculapro)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; draculapro-theme.el ends here