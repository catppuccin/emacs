;;; catppuccin-theme.el --- Catppuccin Theme

;; Copyright 2015-present, All rights reserved
;; 
;; SPDX-License-Identifier: MIT

;; Maintainer: Name <namesexistsinemails@gmail.com>
;; Author: pspiagicw
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/catppuccin/emacs

;;; Commentary:

;; A sweet color theme available for a number of editors.

;;; Code:
(deftheme catppuccin)

;;;; Configuration options:

(defgroup catppuccin nil
  "Catppuccin theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom catppuccin-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'catppuccin)

(defcustom catppuccin-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'catppuccin)

(defcustom catppuccin-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'catppuccin)

(defcustom catppuccin-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'catppuccin)

(defcustom catppuccin-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'catppuccin)

(defcustom catppuccin-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'catppuccin)

(defvar catppuccin-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Catppuccin theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/catppuccin/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (catppuccin-bg      "#1e1d2f" "unspecified-bg" "unspecified-bg") ; official background
                (catppuccin-fg      "#d9e0ee" "#ffffff" "brightwhite") ; official foreground
                (catppuccin-current "#44475a" "#303030" "brightblack") ; official current-line/selection
                (catppuccin-comment "#6272a4" "#5f5faf" "blue")        ; official comment
                (catppuccin-cyan    "#89dceb" "#87d7ff" "brightcyan")  ; official cyan
                (catppuccin-green   "#abe9b3" "#5fff87" "green")       ; official green
                (catppuccin-orange  "#f28fad" "#ffaf5f" "brightred")   ; official orange
                (catppuccin-pink    "#f5c2e7" "#ff87d7" "magenta")     ; official pink
                (catppuccin-purple  "#bd93f9" "#af87ff" "brightmagenta") ; official purple
                (catppuccin-red     "#f28fad" "#ff8787" "red")         ; official red
                (catppuccin-yellow  "#fae3b0" "#ffff87" "yellow")      ; official yellow
                ;; Other colors
                (bg2             "#1A1826" "#121212" "brightblack")
                (bg3             "#1A1826" "#262626" "brightblack")
                (bg4             "#1a1826" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#96cdfb" "#0087ff" "brightblue")))
      (faces '(;; default / basic faces
               (cursor :background ,fg3)
               (default :background ,catppuccin-bg :foreground ,catppuccin-fg)
               (default-italic :slant italic)
               (error :foreground ,catppuccin-red)
               (ffap :foreground ,fg4)
               (fringe :background ,catppuccin-bg :foreground ,fg4)
               (header-line :background ,catppuccin-bg)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,catppuccin-current :extend t)
               (info-quoted-name :foreground ,catppuccin-orange)
               (info-string :foreground ,catppuccin-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,catppuccin-cyan :underline t)
               (linum :slant italic :foreground ,catppuccin-comment :background ,catppuccin-bg)
               (line-number :slant italic :foreground ,catppuccin-comment :background ,catppuccin-bg)
               (match :background ,catppuccin-yellow :foreground ,catppuccin-bg)
               (menu :background ,catppuccin-current :inverse-video nil
                     ,@(if catppuccin-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground catppuccin-fg)))
               (minibuffer-prompt
                ,@(if catppuccin-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground catppuccin-fg)
                    (list :weight 'bold :foreground catppuccin-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (shadow :foreground ,catppuccin-comment)
               (success :foreground ,catppuccin-green)
               (tooltip :foreground ,catppuccin-fg :background ,catppuccin-current)
               (trailing-whitespace :background ,catppuccin-orange)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,catppuccin-orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,catppuccin-cyan :slant italic)
               (font-lock-comment-face :inherit shadow)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,catppuccin-purple)
               (font-lock-doc-face :foreground ,catppuccin-comment)
               (font-lock-function-name-face :foreground ,catppuccin-green :weight bold)
               (font-lock-keyword-face :foreground ,catppuccin-pink :weight bold)
               (font-lock-negation-char-face :foreground ,catppuccin-cyan)
               (font-lock-preprocessor-face :foreground ,catppuccin-orange)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,catppuccin-cyan)
               (font-lock-regexp-grouping-construct :foreground ,catppuccin-purple)
               (font-lock-string-face :foreground ,catppuccin-yellow)
               (font-lock-type-face :inherit font-lock-builtin-face)
               (font-lock-variable-name-face :foreground ,catppuccin-fg :weight bold)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,catppuccin-pink)
               ;; company
               (company-echo-common :foreground ,catppuccin-bg :background ,catppuccin-fg)
               (company-preview :background ,catppuccin-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,catppuccin-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,catppuccin-green)
               (company-scrollbar-bg :background ,catppuccin-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :inherit tooltip)
               (company-tooltip-search :foreground ,catppuccin-green
                                       :underline t)
               (company-tooltip-search-selection :background ,catppuccin-green
                                                 :foreground ,catppuccin-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,catppuccin-bg)
               (company-tooltip-common :foreground ,catppuccin-pink :weight bold)
               ;;(company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,catppuccin-cyan)
               ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,catppuccin-green)
               (completions-first-difference :foreground ,catppuccin-pink :weight bold)
               ;; diff-hl
               (diff-hl-change :foreground ,catppuccin-orange :background ,catppuccin-orange)
               (diff-hl-delete :foreground ,catppuccin-red :background ,catppuccin-red)
               (diff-hl-insert :foreground ,catppuccin-green :background ,catppuccin-green)
               ;; dired
               (dired-directory :foreground ,catppuccin-green :weight normal)
               (dired-flagged :foreground ,catppuccin-pink)
               (dired-header :foreground ,fg3 :background ,catppuccin-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,catppuccin-fg :weight bold)
               (dired-marked :foreground ,catppuccin-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,catppuccin-yellow :weight normal :slant italic)
               (dired-warning :foreground ,catppuccin-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,catppuccin-fg)
               (diredp-deletion-file-name :foreground ,catppuccin-pink :background ,catppuccin-current)
               (diredp-deletion :foreground ,catppuccin-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,catppuccin-orange)
               (diredp-file-name :foreground ,catppuccin-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,catppuccin-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,catppuccin-current)
               (diredp-ignored-file-name :foreground ,catppuccin-fg)
               (diredp-mode-line-flagged :foreground ,catppuccin-orange)
               (diredp-mode-line-marked :foreground ,catppuccin-orange)
               (diredp-no-priv :foreground ,catppuccin-fg)
               (diredp-number :foreground ,catppuccin-cyan)
               (diredp-other-priv :foreground ,catppuccin-orange)
               (diredp-rare-priv :foreground ,catppuccin-orange)
               (diredp-read-priv :foreground ,catppuccin-purple)
               (diredp-write-priv :foreground ,catppuccin-pink)
               (diredp-exec-priv :foreground ,catppuccin-yellow)
               (diredp-symlink :foreground ,catppuccin-orange)
               (diredp-link-priv :foreground ,catppuccin-orange)
               (diredp-autofile-name :foreground ,catppuccin-yellow)
               (diredp-tagged-autofile-name :foreground ,catppuccin-yellow)
               ;; elfeed
               (elfeed-search-date-face :foreground ,catppuccin-comment)
               (elfeed-search-title-face :foreground ,catppuccin-fg)
               (elfeed-search-unread-title-face :foreground ,catppuccin-pink :weight bold)
               (elfeed-search-feed-face :foreground ,catppuccin-fg :weight bold)
               (elfeed-search-tag-face :foreground ,catppuccin-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,catppuccin-pink)
               (elfeed-search-filter-face :foreground ,catppuccin-green :weight bold)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,catppuccin-red)
               (elfeed-log-warn-level-face :foreground ,catppuccin-orange)
               (elfeed-log-info-level-face :foreground ,catppuccin-cyan)
               (elfeed-log-debug-level-face :foreground ,catppuccin-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,catppuccin-pink
                                       ,@(when catppuccin-enlarge-headings
                                           (list :height catppuccin-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,catppuccin-purple
                                       ,@(when catppuccin-enlarge-headings
                                           (list :height catppuccin-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,catppuccin-green
                                       ,@(when catppuccin-enlarge-headings
                                           (list :height catppuccin-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,catppuccin-orange)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,catppuccin-yellow)
               (enh-ruby-op-face :foreground ,catppuccin-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,catppuccin-yellow)
               (enh-ruby-string-delimiter-face :foreground ,catppuccin-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,catppuccin-orange))
               (flyspell-incorrect :underline (:style wave :color ,catppuccin-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,catppuccin-purple)
               (font-latex-italic-face :foreground ,catppuccin-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,catppuccin-cyan)
               (font-latex-match-variable-keywords :foreground ,catppuccin-fg)
               (font-latex-string-face :foreground ,catppuccin-yellow)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,catppuccin-pink
                                      ,@(when catppuccin-enlarge-headings
                                          (list :height catppuccin-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,catppuccin-purple
                                      ,@(when catppuccin-enlarge-headings
                                          (list :height catppuccin-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,catppuccin-green
                                      ,@(when catppuccin-enlarge-headings
                                          (list :height catppuccin-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,catppuccin-yellow)
               (gemini-quote-face :foreground ,catppuccin-purple)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,catppuccin-pink)
               (go-test--standard-face :foreground ,catppuccin-cyan)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,catppuccin-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,catppuccin-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,catppuccin-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,catppuccin-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,catppuccin-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,catppuccin-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,catppuccin-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,catppuccin-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,catppuccin-purple)
               (gnus-header-from :foreground ,catppuccin-fg)
               (gnus-header-name :foreground ,catppuccin-green)
               (gnus-header-subject :foreground ,catppuccin-pink :weight bold)
               (gnus-summary-markup-face :foreground ,catppuccin-cyan)
               (gnus-summary-high-unread :foreground ,catppuccin-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,catppuccin-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,catppuccin-pink :weight bold)
               (gnus-summary-low-unread :foreground ,catppuccin-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,catppuccin-pink)
               (haskell-constructor-face :foreground ,catppuccin-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,catppuccin-purple)
               (helm-buffer-not-saved :foreground ,catppuccin-purple :background ,catppuccin-bg)
               (helm-buffer-process :foreground ,catppuccin-orange :background ,catppuccin-bg)
               (helm-buffer-saved-out :foreground ,catppuccin-fg :background ,catppuccin-bg)
               (helm-buffer-size :foreground ,catppuccin-fg :background ,catppuccin-bg)
               (helm-candidate-number :foreground ,catppuccin-bg :background ,catppuccin-fg)
               (helm-ff-directory :foreground ,catppuccin-green :background ,catppuccin-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,catppuccin-green :background ,catppuccin-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,catppuccin-bg :weight normal)
               (helm-ff-file :foreground ,catppuccin-fg :background ,catppuccin-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,catppuccin-pink :background ,catppuccin-bg :weight bold)
               (helm-ff-prefix :foreground ,catppuccin-bg :background ,catppuccin-pink :weight normal)
               (helm-ff-symlink :foreground ,catppuccin-pink :background ,catppuccin-bg :weight bold)
               (helm-grep-cmd-line :foreground ,catppuccin-fg :background ,catppuccin-bg)
               (helm-grep-file :foreground ,catppuccin-fg :background ,catppuccin-bg)
               (helm-grep-finish :foreground ,fg2 :background ,catppuccin-bg)
               (helm-grep-lineno :foreground ,catppuccin-fg :background ,catppuccin-bg)
               (helm-grep-match :inherit match)
               (helm-grep-running :foreground ,catppuccin-green :background ,catppuccin-bg)
               (helm-header :foreground ,fg2 :background ,catppuccin-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,catppuccin-green :background ,catppuccin-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,catppuccin-purple :background ,catppuccin-bg)
               (helm-source-go-package-godoc-description :foreground ,catppuccin-yellow)
               (helm-source-header :foreground ,catppuccin-pink :background ,catppuccin-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,catppuccin-orange :background ,catppuccin-bg)
               (helm-time-zone-home :foreground ,catppuccin-purple :background ,catppuccin-bg)
               (helm-visible-mark :foreground ,catppuccin-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,catppuccin-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,catppuccin-fg)
               (icicle-search-current-input :foreground ,catppuccin-pink)
               (icicle-search-context-level-8 :foreground ,catppuccin-orange)
               (icicle-search-context-level-7 :foreground ,catppuccin-orange)
               (icicle-search-context-level-6 :foreground ,catppuccin-orange)
               (icicle-search-context-level-5 :foreground ,catppuccin-orange)
               (icicle-search-context-level-4 :foreground ,catppuccin-orange)
               (icicle-search-context-level-3 :foreground ,catppuccin-orange)
               (icicle-search-context-level-2 :foreground ,catppuccin-orange)
               (icicle-search-context-level-1 :foreground ,catppuccin-orange)
               (icicle-search-main-regexp-current :foreground ,catppuccin-fg)
               (icicle-saved-candidate :foreground ,catppuccin-fg)
               (icicle-proxy-candidate :foreground ,catppuccin-fg)
               (icicle-mustmatch-completion :foreground ,catppuccin-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,catppuccin-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,catppuccin-orange)
               (icicle-match-highlight-Completions :foreground ,catppuccin-green)
               (icicle-key-complete-menu-local :foreground ,catppuccin-fg)
               (icicle-key-complete-menu :foreground ,catppuccin-fg)
               (icicle-input-completion-fail-lax :foreground ,catppuccin-pink)
               (icicle-input-completion-fail :foreground ,catppuccin-pink)
               (icicle-historical-candidate-other :foreground ,catppuccin-fg)
               (icicle-historical-candidate :foreground ,catppuccin-fg)
               (icicle-current-candidate-highlight :foreground ,catppuccin-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,catppuccin-fg)
               (icicle-complete-input :foreground ,catppuccin-orange)
               (icicle-common-match-highlight-Completions :foreground ,catppuccin-purple)
               (icicle-candidate-part :foreground ,catppuccin-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,catppuccin-orange)
               ;; ido
               (ido-first-match
                ,@(if catppuccin-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground catppuccin-green)
                    (list :weight 'bold :foreground catppuccin-pink)))
               (ido-only-match :foreground ,catppuccin-orange)
               (ido-subdir :foreground ,catppuccin-yellow)
               (ido-virtual :foreground ,catppuccin-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,catppuccin-fg :background ,catppuccin-pink)
               ;; ivy
               (ivy-current-match
                ,@(if catppuccin-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :background catppuccin-current :foreground catppuccin-green)
                    (list :weight 'bold :background catppuccin-current :foreground catppuccin-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,catppuccin-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :background ,catppuccin-green
                                            :foreground ,catppuccin-bg)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,catppuccin-yellow
                                            :foreground ,catppuccin-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,catppuccin-pink
                                            :foreground ,catppuccin-bg)
               (ivy-confirm-face :foreground ,catppuccin-orange)
               (ivy-match-required-face :foreground ,catppuccin-red)
               (ivy-subdir :foreground ,catppuccin-yellow)
               (ivy-remote :foreground ,catppuccin-pink)
               (ivy-virtual :foreground ,catppuccin-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,catppuccin-bg :background ,catppuccin-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,catppuccin-cyan)
               (jde-java-font-lock-modifier-face :foreground ,catppuccin-pink)
               (jde-java-font-lock-number-face :foreground ,catppuccin-fg)
               (jde-java-font-lock-package-face :foreground ,catppuccin-fg)
               (jde-java-font-lock-private-face :foreground ,catppuccin-pink)
               (jde-java-font-lock-public-face :foreground ,catppuccin-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,catppuccin-purple)
               (js2-function-param :foreground ,catppuccin-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,catppuccin-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,catppuccin-yellow)
               (js2-private-function-call :foreground ,catppuccin-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,catppuccin-orange)
               (js3-external-variable-face :foreground ,catppuccin-fg)
               (js3-function-param-face :foreground ,catppuccin-pink)
               (js3-instance-member-face :foreground ,catppuccin-cyan)
               (js3-jsdoc-tag-face :foreground ,catppuccin-pink)
               (js3-warning-face :underline ,catppuccin-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,catppuccin-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,catppuccin-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,catppuccin-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,catppuccin-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,catppuccin-fg :weight ultra-bold
                                               :box (:line-width -1 :color catppuccin-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,catppuccin-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,catppuccin-bg)
               (lsp-ui-doc-header :foreground ,catppuccin-bg :background ,catppuccin-cyan)
               ;; magit
               (magit-branch-local :foreground ,catppuccin-cyan)
               (magit-branch-remote :foreground ,catppuccin-green)
               (magit-tag :foreground ,catppuccin-orange)
               (magit-section-heading :foreground ,catppuccin-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,catppuccin-orange
                                            :background ,catppuccin-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,catppuccin-orange
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
               (magit-diff-file-heading :foreground ,catppuccin-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,catppuccin-green)
               (magit-diffstat-removed :foreground ,catppuccin-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,catppuccin-orange :weight bold)
               (magit-process-ok :foreground ,catppuccin-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,catppuccin-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,catppuccin-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,catppuccin-pink
                ,@(when catppuccin-enlarge-headings
                    (list :height catppuccin-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,catppuccin-purple
                ,@(when catppuccin-enlarge-headings
                    (list :height catppuccin-height-title-2)))
               (markdown-header-face-3
                :foreground ,catppuccin-green
                ,@(when catppuccin-enlarge-headings
                    (list :height catppuccin-height-title-3)))
               (markdown-header-face-4 :foreground ,catppuccin-yellow)
               (markdown-header-face-5 :foreground ,catppuccin-cyan)
               (markdown-header-face-6 :foreground ,catppuccin-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,catppuccin-fg)
               (markdown-inline-code-face :foreground ,catppuccin-green)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,catppuccin-orange)
               (markdown-table-face :foreground ,catppuccin-purple)
               (markdown-list-face :foreground ,catppuccin-cyan)
               (markdown-language-keyword-face :foreground ,catppuccin-comment)
               ;; message
               (message-header-to :foreground ,catppuccin-fg :weight bold)
               (message-header-cc :foreground ,catppuccin-fg :bold bold)
               (message-header-subject :foreground ,catppuccin-orange)
               (message-header-newsgroups :foreground ,catppuccin-purple)
               (message-header-other :foreground ,catppuccin-purple)
               (message-header-name :foreground ,catppuccin-green)
               (message-header-xheader :foreground ,catppuccin-cyan)
               (message-separator :foreground ,catppuccin-cyan :slant italic)
               (message-cited-text :foreground ,catppuccin-purple)
               (message-cited-text-1 :foreground ,catppuccin-purple)
               (message-cited-text-2 :foreground ,catppuccin-orange)
               (message-cited-text-3 :foreground ,catppuccin-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,catppuccin-green :weight normal)
               ;; mode-line
               (mode-line :background ,catppuccin-current
                          :box ,catppuccin-current :inverse-video nil
                          ,@(if catppuccin-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground catppuccin-fg)))
               (mode-line-inactive
                :background ,catppuccin-bg :inverse-video nil
                ,@(if catppuccin-alternate-mode-line-and-minibuffer
                      (list :foreground catppuccin-comment :box catppuccin-bg)
                    (list :foreground fg4 :box bg2)))
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; mu4e
               (mu4e-unread-face :foreground ,catppuccin-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,catppuccin-purple)
               (mu4e-highlight-face :background ,catppuccin-bg
                                    :foreground ,catppuccin-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,catppuccin-current
                                           :foreground ,catppuccin-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,catppuccin-purple)
               (mu4e-cited-1-face :foreground ,catppuccin-purple)
               (mu4e-cited-2-face :foreground ,catppuccin-orange)
               (mu4e-cited-3-face :foreground ,catppuccin-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,catppuccin-orange :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,catppuccin-purple)
               (neo-expand-btn-face :foreground ,catppuccin-fg)
               (neo-file-link-face :foreground ,catppuccin-cyan)
               (neo-header-face :background ,catppuccin-bg
                                :foreground ,catppuccin-fg
                                :weight bold)
               (neo-root-dir-face :foreground ,catppuccin-purple :weight bold)
               (neo-vc-added-face :foreground ,catppuccin-orange)
               (neo-vc-conflict-face :foreground ,catppuccin-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,catppuccin-orange)
               (neo-vc-ignored-face :foreground ,catppuccin-comment)
               (neo-vc-missing-face :foreground ,catppuccin-red)
               (neo-vc-needs-merge-face :foreground ,catppuccin-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,catppuccin-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,catppuccin-green)
               (neo-vc-user-face :foreground ,catppuccin-purple)
               ;; org
               (org-agenda-date :foreground ,catppuccin-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,catppuccin-comment)
               (org-agenda-done :foreground ,catppuccin-green)
               (org-agenda-structure :foreground ,catppuccin-purple)
               (org-block :foreground ,catppuccin-orange)
               (org-code :foreground ,catppuccin-green)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,catppuccin-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,catppuccin-comment)
               (org-document-title :weight bold :foreground ,catppuccin-orange
                                   ,@(when catppuccin-enlarge-headings
                                       (list :height catppuccin-height-doc-title)))
               (org-done :foreground ,catppuccin-green)
               (org-ellipsis :foreground ,catppuccin-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,catppuccin-pink)
               (org-headline-done :foreground ,catppuccin-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,catppuccin-bg :background ,catppuccin-bg)
               (org-level-1 :inherit bold :foreground ,catppuccin-pink
                            ,@(when catppuccin-enlarge-headings
                                (list :height catppuccin-height-title-1)))
               (org-level-2 :inherit bold :foreground ,catppuccin-purple
                            ,@(when catppuccin-enlarge-headings
                                (list :height catppuccin-height-title-2)))
               (org-level-3 :weight normal :foreground ,catppuccin-green
                            ,@(when catppuccin-enlarge-headings
                                (list :height catppuccin-height-title-3)))
               (org-level-4 :weight normal :foreground ,catppuccin-yellow)
               (org-level-5 :weight normal :foreground ,catppuccin-cyan)
               (org-level-6 :weight normal :foreground ,catppuccin-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,catppuccin-fg)
               (org-link :foreground ,catppuccin-cyan :underline t)
               (org-priority :foreground ,catppuccin-cyan)
               (org-quote :foreground ,catppuccin-yellow :slant italic)
               (org-scheduled :foreground ,catppuccin-green)
               (org-scheduled-previously :foreground ,catppuccin-yellow)
               (org-scheduled-today :foreground ,catppuccin-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,catppuccin-yellow)
               (org-table :foreground ,catppuccin-purple)
               (org-tag :foreground ,catppuccin-pink :weight bold :background ,bg2)
               (org-todo :foreground ,catppuccin-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,catppuccin-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,catppuccin-pink)
               ;; outline
               (outline-1 :foreground ,catppuccin-pink)
               (outline-2 :foreground ,catppuccin-purple)
               (outline-3 :foreground ,catppuccin-green)
               (outline-4 :foreground ,catppuccin-yellow)
               (outline-5 :foreground ,catppuccin-cyan)
               (outline-6 :foreground ,catppuccin-orange)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,catppuccin-pink)
               ;; powerline
               (powerline-active1 :background ,catppuccin-bg :foreground ,catppuccin-pink)
               (powerline-active2 :background ,catppuccin-bg :foreground ,catppuccin-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,catppuccin-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,catppuccin-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,catppuccin-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,catppuccin-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,catppuccin-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,catppuccin-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,catppuccin-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,catppuccin-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,catppuccin-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,catppuccin-fg)
               (rainbow-delimiters-depth-2-face :foreground ,catppuccin-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,catppuccin-purple)
               (rainbow-delimiters-depth-4-face :foreground ,catppuccin-pink)
               (rainbow-delimiters-depth-5-face :foreground ,catppuccin-orange)
               (rainbow-delimiters-depth-6-face :foreground ,catppuccin-green)
               (rainbow-delimiters-depth-7-face :foreground ,catppuccin-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,catppuccin-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,catppuccin-green)
               (rpm-spec-doc-face :foreground ,catppuccin-pink)
               (rpm-spec-ghost-face :foreground ,catppuccin-purple)
               (rpm-spec-macro-face :foreground ,catppuccin-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,catppuccin-purple)
               (rpm-spec-section-face :foreground ,catppuccin-yellow)
               (rpm-spec-tag-face :foreground ,catppuccin-cyan)
               (rpm-spec-var-face :foreground ,catppuccin-orange)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,catppuccin-pink :weight bold)
               (rst-level-2 :foreground ,catppuccin-purple :weight bold)
               (rst-level-3 :foreground ,catppuccin-green)
               (rst-level-4 :foreground ,catppuccin-yellow)
               (rst-level-5 :foreground ,catppuccin-cyan)
               (rst-level-6 :foreground ,catppuccin-orange)
               (rst-level-7 :foreground ,other-blue)
               (rst-level-8 :foreground ,catppuccin-fg)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,catppuccin-pink)
               (selectrum-secondary-highlight :foreground ,catppuccin-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,catppuccin-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,catppuccin-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,catppuccin-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,catppuccin-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,catppuccin-green)
               (speedbar-file-face :foreground ,catppuccin-cyan)
               (speedbar-directory-face :foreground ,catppuccin-purple)
               (speedbar-tag-face :foreground ,catppuccin-yellow)
               (speedbar-selected-face :foreground ,catppuccin-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,catppuccin-bg
                                        :foreground ,catppuccin-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,catppuccin-purple :background ,catppuccin-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,catppuccin-pink :background ,catppuccin-bg
                            :box (:line-width 2 :color ,catppuccin-bg :style nil))
               (tab-bar-tab-inactive :foreground ,catppuccin-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,catppuccin-purple :background ,catppuccin-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,catppuccin-pink :background ,catppuccin-bg
                             :box (:line-width 2 :color ,catppuccin-bg :style nil))
               (tab-line-tab-inactive :foreground ,catppuccin-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,catppuccin-red)
               ;; telephone-line
               (telephone-line-accent-active :background ,catppuccin-bg :foreground ,catppuccin-pink)
               (telephone-line-accent-inactive :background ,bg2 :foreground ,catppuccin-purple)
               (telephone-line-unimportant :background ,catppuccin-bg :foreground ,catppuccin-comment)
               ;; term
               (term :foreground ,catppuccin-fg :background ,catppuccin-bg)
               (term-color-black :foreground ,catppuccin-bg :background ,catppuccin-comment)
               (term-color-blue :foreground ,catppuccin-purple :background ,catppuccin-purple)
               (term-color-cyan :foreground ,catppuccin-cyan :background ,catppuccin-cyan)
               (term-color-green :foreground ,catppuccin-green :background ,catppuccin-green)
               (term-color-magenta :foreground ,catppuccin-pink :background ,catppuccin-pink)
               (term-color-red :foreground ,catppuccin-red :background ,catppuccin-red)
               (term-color-white :foreground ,catppuccin-fg :background ,catppuccin-fg)
               (term-color-yellow :foreground ,catppuccin-yellow :background ,catppuccin-yellow)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,catppuccin-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,catppuccin-pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,catppuccin-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,catppuccin-fg)
               (tree-sitter-hl-face:punctuation.special :foreground ,catppuccin-pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,catppuccin-red)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,catppuccin-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,catppuccin-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,catppuccin-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,catppuccin-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :inherit font-lock-constant-face)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,catppuccin-purple)
               (web-mode-html-attr-value-face :foreground ,catppuccin-green)
               (web-mode-html-tag-face :foreground ,catppuccin-pink :weight bold)
               (web-mode-keyword-face :foreground ,catppuccin-pink)
               (web-mode-string-face :foreground ,catppuccin-yellow)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,catppuccin-green)
               ;; whitespace
               (whitespace-big-indent :background ,catppuccin-red :foreground ,catppuccin-red)
               (whitespace-empty :background ,catppuccin-orange :foreground ,catppuccin-red)
               (whitespace-hspace :background ,bg3 :foreground ,catppuccin-comment)
               (whitespace-indentation :background ,catppuccin-orange :foreground ,catppuccin-red)
               (whitespace-line :background ,catppuccin-bg :foreground ,catppuccin-pink)
               (whitespace-newline :foreground ,catppuccin-comment)
               (whitespace-space :background ,catppuccin-bg :foreground ,catppuccin-comment)
               (whitespace-space-after-tab :background ,catppuccin-orange :foreground ,catppuccin-red)
               (whitespace-space-before-tab :background ,catppuccin-orange :foreground ,catppuccin-red)
               (whitespace-tab :background ,bg2 :foreground ,catppuccin-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'catppuccin
         (let ((expand-with-func
                (lambda (func spec)
                  (let (reduced-color-list)
                    (dolist (col colors reduced-color-list)
                      (push (list (car col) (funcall func col))
                            reduced-color-list))
                    (eval `(let ,reduced-color-list
                             (backquote ,spec))))))
               whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (((min-colors 256))      ; terminal withs 256 colors
                       ,(if catppuccin-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'catppuccin
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'catppuccin-bg colors))
               ,(funcall get-func (alist-get 'catppuccin-red colors))
               ,(funcall get-func (alist-get 'catppuccin-green colors))
               ,(funcall get-func (alist-get 'catppuccin-yellow colors))
               ,(funcall get-func (alist-get 'catppuccin-comment colors))
               ,(funcall get-func (alist-get 'catppuccin-purple colors))
               ,(funcall get-func (alist-get 'catppuccin-cyan colors))
               ,(funcall get-func (alist-get 'catppuccin-fg colors))])))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'catppuccin)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; catppuccin-theme.el ends here
