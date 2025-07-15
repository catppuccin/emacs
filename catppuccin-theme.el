;;; catppuccin-theme.el --- Catppuccin for Emacs - 🍄 Soothing pastel theme for Emacs -*- lexical-binding: t; -*-

;; Copyright 2022-present Catppuccin, All rights reserved
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Maintainer: Jeremy Baxter <jeremy@baxters.nz>
;; Author: nyxkrage
;; Original-Author: film42
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/catppuccin/emacs

;;; Commentary:

;; 🍄 Soothing pastel theme for Emacs
;;
;; catppuccin-theme.el provides the theme `catppuccin', a port of the
;; Catppuccin colors to Emacs.  To select a palette and enable the theme,
;; evaluate:
;;
;;     (setq catppuccin-flavor 'frappe) ; or 'latte, 'macchiato, or 'mocha
;;     (load-theme 'catppuccin t)
;;
;; For more information visit <https://catppuccin.com>.

;;; Code:

(eval-when-compile (require 'subr-x))

(deftheme catppuccin)

;;; Configuration options:

(defgroup catppuccin nil
  "Catppuccin theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom catppuccin-highlight-matches nil
  "Use background color to make highlighted matches more visible."
  :type 'boolean
  :group 'catppuccin)

(defcustom catppuccin-italic-comments nil
  "Use :slant italic for comments."
  :type 'boolean
  :group 'catppuccin)

(defcustom catppuccin-italic-blockquotes t
  "Use :slant italic for blockquotes in markdown and org."
  :type 'boolean
  :group 'catppuccin)

(defcustom catppuccin-italic-variables nil
  "Use :slant italic for variables."
  :type 'boolean
  :group 'catppuccin)

(defcustom catppuccin-flavor 'mocha
  "The flavor to use for the Catppuccin theme.
Must be one of `mocha`, `macchiato`, `frappe`, or `latte`."
  :type '(choice
           (const :tag "Mocha" mocha)
           (const :tag "Macchiato" macchiato)
           (const :tag "Frappe" frappe)
           (const :tag "Latte" latte))
  :group 'catppuccin)

(defun define-catppuccin-flavor (flavor colors)
  "Define a new Catppuccin flavor named FLAVOR.

The colors used will correspond to those in COLORS."
  (custom-declare-variable (intern (concat "catppuccin-"
                                     (symbol-name flavor) "-colors"))
    `(funcall ',(lambda () colors))
    (concat "Colors for Catppuccin " (capitalize (symbol-name flavor)))
    :options '(rosewater flamingo pink mauve red maroon peach
                yellow green teal sky sapphire blue lavender
                text subtext1 subtext0 overlay2 overlay1 overlay0
                surface2 surface1 surface0 base mantle crust)
    :type '(alist :key-type symbol :value-type string)
    :group 'catppuccin))

(defvar catppuccin-flavor-alist '()
  "Alist of flavors to alist of names to hex colors.")

(when load-file-name
  ;; load the flavor definitions
  (with-temp-buffer
    (insert-file-contents (expand-file-name "catppuccin-definitions.el"
                            (file-name-directory load-file-name)))
    (setq catppuccin-flavor-alist (read (current-buffer))))

  ;; define flavors
  (let ((flavor #'(lambda (sym) (alist-get sym catppuccin-flavor-alist))))
    (define-catppuccin-flavor 'mocha (funcall flavor 'mocha))
    (define-catppuccin-flavor 'macchiato (funcall flavor 'macchiato))
    (define-catppuccin-flavor 'frappe (funcall flavor 'frappe))
    (define-catppuccin-flavor 'latte (funcall flavor 'latte))))

;; load-file-name is only available when the module is loaded
;; with `load', which is also used by `require'
(unless (listp catppuccin-flavor-alist)
  (error "Please load with `load' or `require'"))

;;; Internal functions:

(defun catppuccin-quantize-color (color)
  "Quantize COLOR to a 256 color palette."
  (let ((i 1)
         (str "#"))
    (while (<= i 5)
      (setq str
        (concat str
          (format
            "%02x"
            (* (round
                 (/
                   (string-to-number (substring color i (+ i 2)) 16)
                   17))
              17))))
      (setq i (+ i 2)))
    str))

;; Color operations
(defun catppuccin--hex-to-rgb (color)
  "Convert a hex COLOR string like \"#rrggbb\" to a list of three integers."
  (mapcar (lambda (i) (string-to-number (substring color i (+ i 2)) 16))
    '(1 3 5)))

(defun catppuccin--rgb-to-hex (r g b)
  "Convert R, G, B integers to a hex color string."
  (format "#%02x%02x%02x" r g b))

(defun catppuccin--rnd (n)
  "Round N to the nearest integer."
  (round (+ 0.5 n)))

(defun catppuccin-lighten (color value)
  "Lighten COLOR by VALUE% (0–100)."
  (let* ((factor (/ value 100.0)))
    (apply #'catppuccin--rgb-to-hex
      (mapcar (lambda (v)
                (catppuccin--rnd
                  (min 255 (+ (* (- 255 v) factor) v))))
        (catppuccin--hex-to-rgb color)))))

(defun catppuccin-darken (color value)
  "Darken COLOR by VALUE% (0–100)."
  (let* ((factor (/ value 100.0)))
    (apply #'catppuccin--rgb-to-hex
      (mapcar (lambda (v)
                (floor (* (- 1 factor) v)))
        (catppuccin--hex-to-rgb color)))))

(defun catppuccin-recolor (color value)
  "Darken or lightens COLOR based on the current flavour."
  (if (eq catppuccin-flavor 'latte)
    (catppuccin-lighten color value)
    (catppuccin-darken color value)))

;;; User functions:

(defun catppuccin-reload ()
  "Reload the Catppuccin theme.

Useful after setting custom colors with `catppuccin-set-color'."
  (interactive)
  (disable-theme 'catppuccin)
  (load-theme 'catppuccin t))

(defun catppuccin-load-flavor (flavor)
  "Set the Catppuccin flavor to FLAVOR.

If called interactively, a list of flavors is presented. Otherwise,
FLAVOR must be one of the symbols `frappe', `latte', `macchiato',
or `mocha'."
  (interactive
    (list
      (intern (completing-read
                "Catppuccin flavor: "
                '(frappe latte macchiato mocha)
                nil   ; predicate
                t)))) ; require-match
  (setq catppuccin-flavor flavor)
  (catppuccin-reload)
  (message "Catppuccin flavor changed to %s" flavor))

(defun catppuccin-colors-of (&optional flavor)
  "Return a symbol for the alist containing FLAVOR's colors.

FLAVOR defaults to the value of `catppuccin-flavor'."
  (intern-soft (concat "catppuccin-"
                 (symbol-name (or flavor catppuccin-flavor)) "-colors")))

(defun catppuccin-set-color (color value &optional flavor)
  "Set the COLOR of FLAVOR or the current flavor to VALUE."
  (interactive "SChange color: \nsSet %s to: ")
  (setcdr (assoc color
            (symbol-value (catppuccin-colors-of flavor))) value))

(defun catppuccin-color (color &optional flavor)
  "Get the COLOR of FLAVOR or the current flavor."
  (interactive "SColor: ")
  (let ((result (alist-get color
                  (symbol-value (catppuccin-colors-of flavor)))))
    (if (called-interactively-p 'interactive)
      (message result)
      result)))

(defalias 'catppuccin-get-color 'catppuccin-color)

(defun catppuccin-insert-color (color &optional flavor)
  "Insert COLOR at point."
  (interactive "SColor: ")
  (let ((color (catppuccin-color color flavor)))
    (insert
      (if (char-equal (char-before) ?#)
        (string-remove-prefix "#" color)
        color))))

;;; Theme definition:

(let
  ((colors
     '((undef     "#ff00ff" "#ff00ff")
        (ctp-rosewater (catppuccin-color 'rosewater)
          (catppuccin-quantize-color (catppuccin-color 'rosewater)))
        (ctp-flamingo  (catppuccin-color 'flamingo)
          (catppuccin-quantize-color (catppuccin-color 'flamingo)))
        (ctp-pink      (catppuccin-color 'pink)
          (catppuccin-quantize-color (catppuccin-color 'pink)))
        (ctp-mauve     (catppuccin-color 'mauve)
          (catppuccin-quantize-color (catppuccin-color 'mauve)))
        (ctp-red       (catppuccin-color 'red)
          (catppuccin-quantize-color (catppuccin-color 'red)))
        (ctp-maroon    (catppuccin-color 'maroon)
          (catppuccin-quantize-color (catppuccin-color 'maroon)))
        (ctp-peach     (catppuccin-color 'peach)
          (catppuccin-quantize-color (catppuccin-color 'peach)))
        (ctp-yellow    (catppuccin-color 'yellow)
          (catppuccin-quantize-color (catppuccin-color 'yellow)))
        (ctp-green     (catppuccin-color 'green)
          (catppuccin-quantize-color (catppuccin-color 'green)))
        (ctp-teal      (catppuccin-color 'teal)
          (catppuccin-quantize-color (catppuccin-color 'teal)))
        (ctp-sky       (catppuccin-color 'sky)
          (catppuccin-quantize-color (catppuccin-color 'sky)))
        (ctp-sapphire  (catppuccin-color 'sapphire)
          (catppuccin-quantize-color (catppuccin-color 'sapphire)))
        (ctp-blue      (catppuccin-color 'blue)
          (catppuccin-quantize-color (catppuccin-color 'blue)))
        (ctp-lavender  (catppuccin-color 'lavender)
          (catppuccin-quantize-color (catppuccin-color 'lavender)))
        (ctp-text      (catppuccin-color 'text)
          (catppuccin-quantize-color (catppuccin-color 'text)))
        (ctp-subtext1  (catppuccin-color 'subtext1)
          (catppuccin-quantize-color (catppuccin-color 'subtext1)))
        (ctp-subtext0  (catppuccin-color 'subtext0)
          (catppuccin-quantize-color (catppuccin-color 'subtext0)))
        (ctp-overlay2  (catppuccin-color 'overlay2)
          (catppuccin-quantize-color (catppuccin-color 'overlay2)))
        (ctp-overlay1  (catppuccin-color 'overlay1)
          (catppuccin-quantize-color (catppuccin-color 'overlay1)))
        (ctp-overlay0  (catppuccin-color 'overlay0)
          (catppuccin-quantize-color (catppuccin-color 'overlay0)))
        (ctp-surface2  (catppuccin-color 'surface2)
          (catppuccin-quantize-color (catppuccin-color 'surface2)))
        (ctp-surface1  (catppuccin-color 'surface1)
          (catppuccin-quantize-color (catppuccin-color 'surface1)))
        (ctp-surface0  (catppuccin-color 'surface0)
          (catppuccin-quantize-color (catppuccin-color 'surface0)))
        (ctp-base      (catppuccin-color 'base)
          (catppuccin-quantize-color (catppuccin-color 'base)))
        (ctp-mantle    (catppuccin-color 'mantle)
          (catppuccin-quantize-color (catppuccin-color 'mantle)))
        (ctp-crust     (catppuccin-color 'crust)
          (catppuccin-quantize-color (catppuccin-color 'crust)))

        (ctp-current   (if (eq catppuccin-flavor 'latte)
                         (catppuccin-darken (catppuccin-color 'base) 5)
                         (catppuccin-lighten (catppuccin-color 'base) 5))
          (catppuccin-quantize-color
            (if (eq catppuccin-flavor 'latte)
              (catppuccin-darken (catppuccin-color 'base) 5)
              (catppuccin-lighten (catppuccin-color 'base) 5))))
        (ctp-selection (if (eq catppuccin-flavor 'latte)
                         (catppuccin-darken (catppuccin-color 'base) 12)
                         (catppuccin-lighten (catppuccin-color 'base) 17))
          (catppuccin-quantize-color
            (if (eq catppuccin-flavor 'latte)
              (catppuccin-darken (catppuccin-color 'base) 12)
              (catppuccin-lighten (catppuccin-color 'base) 17))))))
    (faces
      '(
         ;; default / basic faces
         (cursor :background ,ctp-rosewater)
         (default :background ,ctp-base :foreground ,ctp-text)
         (italic :slant italic)
         (default-italic :slant italic)
         (hl-todo :foreground ,ctp-peach)
         (error :foreground ,ctp-red)
         (ffap :inherit match)
         (fringe :background ,ctp-base :foreground ,ctp-surface1)
         (header-line :inherit mode-line)
         (help-key-binding :background ,ctp-mantle :foreground ,ctp-blue
           :box (:line-width (-1 . -1) :color ,ctp-crust :style nil))
         (highlight :foreground ,ctp-text :background ,ctp-current)
         (hl-line :background ,ctp-current :extend t)
         (info-menu-star :foreground ,ctp-red)
         (info-quoted-name :foreground ,ctp-subtext1)
         (info-string :foreground ,ctp-green)
         (lazy-highlight :foreground ,ctp-subtext1
           :background ,ctp-surface1)
         (link :foreground ,ctp-lavender :underline t)
         (link-unvisited :foreground ,ctp-blue :underline t)
         (linum :inherit default :foreground ,ctp-surface1
           :background ,ctp-base)
         (line-number :inherit default :foreground ,ctp-surface1
           :background ,ctp-base)
         (line-number-current-line :inherit line-number
           :foreground ,ctp-lavender)
         (match :background ,ctp-red :foreground ,ctp-mantle)
         (menu :background ,ctp-current :inverse-video nil
           :foreground ,ctp-text)
         (minibuffer-prompt :weight normal :foreground ,ctp-subtext0)
         (mode-line :background ,ctp-mantle nil :foreground ,ctp-text)
         (mode-line-inactive :background ,ctp-crust :inverse-video nil
           :foreground ,ctp-overlay0)
         (read-multiple-choice-face :inherit completions-first-difference)
         (region :background ,ctp-selection :extend t)
         (shadow :foreground ,ctp-overlay0)
         (success :foreground ,ctp-green)
         (warning :foreground ,ctp-yellow)
         (tooltip :foreground ,ctp-overlay2 :background ,ctp-surface0)
         (trailing-whitespace :inherit warning)
         (window-divider :foreground ,ctp-mantle)
         (vertical-border :foreground ,ctp-mantle)

         ;; tty-menu
         (tty-menu-enabled-face :foreground ,ctp-text :inverse-video nil
           :background ,ctp-current)
         (tty-menu-disabled-face :background ,ctp-crust :inverse-video nil
           :foreground ,ctp-overlay0)
         (tty-menu-selected-face :foreground ,ctp-text
           :background ,ctp-surface1)

         ;; solaire-mode
         (solaire-default-face :background ,ctp-mantle
           :foreground ,ctp-text)
         (solaire-fringe-face :background ,ctp-mantle
           :foreground ,ctp-surface1)
         (solaire-line-number-face :inherit default
           :foreground ,ctp-surface1 :background ,ctp-mantle)
         (solaire-mode-line-face :background ,ctp-crust nil
           :foreground ,ctp-text)
         (solaire-mode-line-inactive-face :inverse-video nil
           :background ,ctp-crust :foreground ,ctp-subtext1)
         (solaire-header-line-face :inherit 'solaire-mode-line-face)

         ;; evil
         (evil-search-highlight-persist-highlight-face :inherit lazy-highlight)
         (evil-ex-lazy-highlight :inherit lazy-highlight)
         (evil-ex-substitute-matches :foreground ,ctp-red :underline t)
         (evil-ex-substitute-replacement :foreground ,ctp-green
           :underline t)

         ;; syntax / font-lock
         (font-lock-bracket-face :foreground ,ctp-overlay2)
         (font-lock-builtin-face :foreground ,ctp-red)
         (font-lock-comment-face ,@(if catppuccin-italic-comments
                                     '(:inherit (shadow italic))
                                     '(:inherit shadow)))
         (font-lock-comment-delimiter-face :inherit shadow)
         (font-lock-constant-face :foreground ,ctp-peach)
         (font-lock-delimiter-face :foreground ,ctp-overlay2)
         (font-lock-doc-face :inherit font-lock-comment-face)
         (font-lock-escape-face :foreground ,ctp-pink)
         (font-lock-function-call-face :foreground ,ctp-blue)
         (font-lock-function-name-face :foreground ,ctp-blue)
         (font-lock-keyword-face :foreground ,ctp-mauve)
         (font-lock-negation-char-face :foreground ,ctp-sky)
         (font-lock-number-face :foreground ,ctp-peach)
         (font-lock-operator-face :foreground ,ctp-sky)
         (font-lock-preprocessor-face :foreground ,ctp-yellow)
         (font-lock-property-name-face :foreground ,ctp-blue)
         (font-lock-reference-face :inherit font-lock-constant-face) ; obsolete
         (font-lock-regexp-grouping-backslash :foreground ,ctp-red)
         (font-lock-regexp-grouping-construct :foreground ,ctp-red)
         (font-lock-string-face :foreground ,ctp-green)
         (font-lock-type-face :foreground ,ctp-yellow)
         (font-lock-variable-name-face :foreground ,ctp-text
           ,@(when catppuccin-italic-variables '(:inherit italic)))
         (font-lock-variable-use-face :foreground ,ctp-text
           ,@(when catppuccin-italic-variables '(:inherit italic)))
         (font-lock-warning-face :inherit warning)

         ;; adoc-mode
         (adoc-anchor-face :foreground ,ctp-blue)
         (adoc-code-face :foreground ,ctp-text)
         (adoc-command-face :foreground ,ctp-yellow)
         (adoc-emphasis-face :inherit bold)
         (adoc-internal-reference-face :foreground ,ctp-green)
         (adoc-list-face :foreground ,ctp-text)
         (adoc-meta-face :inherit font-lock-comment-face)
         (adoc-meta-hide-face :inherit font-lock-comment-face)
         (adoc-reference-face :inherit link)
         (adoc-secondary-text-face :foreground ,ctp-yellow)
         (adoc-title-0-face :foreground ,ctp-red)
         (adoc-title-1-face :foreground ,ctp-peach)
         (adoc-title-2-face :foreground ,ctp-yellow)
         (adoc-title-3-face :foreground ,ctp-green)
         (adoc-title-4-face :foreground ,ctp-sapphire)
         (adoc-typewriter-face :foreground ,ctp-green)
         (adoc-verbatim-face :foreground ,ctp-green)
         (adoc-value-face :foreground ,ctp-yellow)

         ;; auto-complete
         (ac-completion-face :underline t :foreground ,undef)

         ;; anzu
         (anzu-mode-line :foreground ,ctp-blue)

         ;; avy
         (avy-background-face :foreground ,ctp-text :background ,ctp-base)
         (avy-goto-char-timer-face :foreground ,ctp-blue
           :background ,ctp-surface0)
         (avy-lead-face :foreground ,ctp-base :background ,ctp-mauve)
         (avy-lead-face-0 :foreground ,ctp-base :background ,ctp-yellow)
         (avy-lead-face-1 :foreground ,ctp-base :background ,ctp-overlay0)
         (avy-lead-face-2 :foreground ,ctp-base :background ,ctp-sky)

         ;; company
         (company-echo-common :foreground ,ctp-base :background ,ctp-text)
         (company-preview :inherit shadow)
         (company-preview-common :inherit company-preview
           :foreground ,ctp-green)
         (company-preview-search :inherit company-preview
           :foreground ,ctp-red)
         (company-tooltip :inherit tooltip)
         (company-tooltip-search :inherit lazy-highlight)
         (company-tooltip-search-selection :inherit match)
         (company-tooltip-selection :background ,ctp-overlay0
           :foreground ,ctp-text)
         (company-tooltip-mouse :background ,ctp-base)
         (company-tooltip-common :foreground ,ctp-text :weight bold)
         (company-tooltip-common-selection :foreground ,ctp-text
           :weight bold)
         (company-tooltip-annotation :foreground ,ctp-green)
         (company-tooltip-annotation-selection :foreground ,ctp-text)
         (company-tooltip-scrollbar-thumb :background ,ctp-surface2)
         (company-tooltip-scrollbar-track :background ,ctp-surface1)

         ;; compile
         (compilation-mode-line-exit :foreground ,ctp-green)
         (compilation-mode-line-fail :foreground ,ctp-red)

         ;; completions (minibuffer.el)
         (completions-annotations :inherit font-lock-comment-face)
         (completions-common-part :foreground ,ctp-sky)
         (completions-first-difference :foreground ,ctp-text)
         ;; completion-preview
         (completion-preview-exact :underline ,ctp-red :inherit completion-preview-common)

         ;; diff-hl
         (diff-hl-change :background ,ctp-blue
           :foreground ,(catppuccin-recolor ctp-blue 50))
         (diff-hl-delete :background ,ctp-red
           :foreground ,(catppuccin-recolor ctp-red 50))
         (diff-hl-insert :background ,ctp-green
           :foreground ,(catppuccin-recolor ctp-green 50))

         ;; diff-mode
         (diff-header :foreground ,ctp-blue)
         (diff-hunk-header :foreground ,ctp-text :background ,ctp-surface2)
         (diff-added :background ,(catppuccin-recolor ctp-green 60))
         (diff-removed :background ,(catppuccin-recolor ctp-red 60))
         (diff-indicator-added :foreground ,ctp-green)
         (diff-indicator-removed :foreground ,ctp-red)
         (diff-refine-added :background ,(catppuccin-recolor ctp-green 40))
         (diff-refine-removed :background ,(catppuccin-recolor ctp-red 40))
         (diff-refine-changed :background ,ctp-yellow
           :foreground ,ctp-base)

         ;; eshell
         (eshell-ls-archive :foreground ,ctp-mauve)
         (eshell-ls-backup :foreground ,ctp-yellow)
         (eshell-ls-clutter :foreground ,ctp-red :weight bold)
         (eshell-ls-directory :foreground ,ctp-blue :weight bold)
         (eshell-ls-executable :foreground ,ctp-green :weight bold)
         (eshell-ls-missing :foreground ,ctp-red :weight bold)
         (eshell-ls-product :foreground ,ctp-peach)
         (eshell-ls-readonly :foreground ,ctp-flamingo)
         (eshell-ls-special :foreground ,ctp-pink :weight bold)
         (eshell-ls-symlink :foreground ,ctp-sapphire :weight bold)
         (eshell-prompt :foreground ,ctp-blue :weight bold)

         ;; git-gutter
         (git-gutter:modified :foreground ,ctp-peach)
         (git-gutter:deleted :foreground ,ctp-red)
         (git-gutter:added :foreground ,ctp-green)
         (git-gutter:seperator :inherit font-lock-comment-face)
         (git-gutter:unchanged :foreground ,ctp-surface0)

         ;; git-gutter fringe
         (git-gutter-fr:modified :inherit git-gutter:modified)
         (git-gutter-fr:deleted :inherit git-gutter:deleted)
         (git-gutter-fr:added :inherit git-gutter:added)

         ;; dired
         (dired-flagged :foreground ,ctp-maroon :weight bold)
         (dired-marked :weight bold)
         (dired-mark :inherit dired-marked)
         (dired-header :foreground ,ctp-sapphire :weight bold)
         (dired-ignored :inherit font-lock-comment-face)
         (dired-special :foreground ,ctp-yellow)
         (dired-symlink :foreground ,ctp-pink)
         (dired-warning :inherit warning)
         (dired-directory :foreground ,ctp-blue)
         (dired-perm-write :foreground ,ctp-green)
         (dired-broken-symlink :foreground ,ctp-text :background ,ctp-red)
         ;; dired-filetype-face
         (dired-filetype-common :foreground ,ctp-text)
         (dired-filetype-compress :foreground ,ctp-yellow)
         (dired-filetype-document :foreground ,ctp-sky)
         (dired-filetype-execute :foreground ,ctp-red)
         (dired-filetype-image :foreground ,ctp-peach)
         (dired-filetype-js :foreground ,ctp-yellow)
         (dired-filetype-link :foreground ,ctp-maroon)
         (dired-filetype-music :foreground ,ctp-maroon)
         (dired-filetype-omit :foreground ,ctp-mauve)
         (dired-filetype-plain :foreground ,ctp-text)
         (dired-filetype-program :foreground ,ctp-peach)
         (dired-filetype-source :foreground ,ctp-green)
         (dired-filetype-video :foreground ,ctp-maroon)
         (dired-filetype-xml :foreground ,ctp-green)
         ;; dired+ (kept for legacy support)
         ;; TODO (maybe): Show deprecation warning
         ;; This doesn't make sense to keep around
         (diredp-compressed-file-name :inherit dired-file-name)
         (diredp-compressed-file-suffix :foreground ,ctp-green)
         (diredp-date-time :foreground ,ctp-subtext0)
         (diredp-deletion-file-name :inherit dired-flagged)
         (diredp-deletion :inherit dired-flagged)
         (diredp-dir-heading :inherit dired-header)
         (diredp-dir-name :inherit dired-directory)
         (diredp-dir-priv :inherit dired-directory)
         (diredp-executable-tag :foreground ,ctp-red)
         (diredp-file-suffix :inherit dired-file-name)
         (diredp-flag-mark-line :inherit dired-marked)
         (diredp-flag-mark :inherit dired-mark)
         (diredp-ignored-file-name :foreground ,ctp-text)
         (diredp-mode-line-flagged :foreground ,undef)
         (diredp-mode-line-marked :foreground ,undef)
         (diredp-no-priv :foreground ,ctp-surface2)
         (diredp-number :foreground ,ctp-yellow)
         (diredp-other-priv :inherit diredp-exec-priv)
         (diredp-rare-priv :inherit diredp-exec-priv)
         (diredp-read-priv :foreground ,ctp-sky)
         (diredp-write-priv :inherit dired-perm-write)
         (diredp-exec-priv :foreground ,ctp-red)
         (diredp-symlink :inherit dired-symlink)
         (diredp-link-priv :inherit dired-symlink)
         (diredp-autofile-name :foreground ,undef)
         (diredp-tagged-autofile-name :foreground ,undef)
         ;; diredfl (more modernly published dired+)
         (diredfl-file-name :inherit dired-file-name)
         (diredfl-compressed-file-name :inherit dired-file-name)
         (diredfl-compressed-file-suffix :foreground ,ctp-green)
         (diredfl-date-time :foreground ,ctp-subtext0)
         (diredfl-deletion-file-name :inherit dired-flagged)
         (diredfl-deletion :inherit dired-flagged)
         (diredfl-dir-heading :inherit dired-header)
         (diredfl-dir-name :inherit dired-directory)
         (diredfl-dir-priv :inherit dired-directory)
         (diredfl-executable-tag :foreground ,ctp-red)
         (diredfl-file-suffix :inherit dired-file-name)
         (diredfl-flag-mark-line :inherit dired-marked)
         (diredfl-flag-mark :inherit dired-mark)
         (diredfl-ignored-file-name :foreground ,ctp-text)
         (diredfl-mode-line-flagged :foreground ,undef)
         (diredfl-mode-line-marked :foreground ,undef)
         (diredfl-no-priv :foreground ,ctp-surface2)
         (diredfl-number :foreground ,ctp-yellow)
         (diredfl-other-priv :inherit diredfl-exec-priv)
         (diredfl-rare-priv :inherit diredfl-exec-priv)
         (diredfl-read-priv :foreground ,ctp-sky)
         (diredfl-write-priv :inherit dired-perm-write)
         (diredfl-exec-priv :foreground ,ctp-red)
         (diredfl-symlink :inherit dired-symlink)
         (diredfl-link-priv :inherit dired-symlink)
         (diredfl-autofile-name :foreground ,undef)
         (diredfl-tagged-autofile-name :foreground ,undef)

         ;; eldoc-box
         (eldoc-box-border :background ,ctp-current)
         (eldoc-box-body :background ,ctp-current)

         ;; elfeed
         (elfeed-search-date-face :foreground ,ctp-subtext0)
         (elfeed-search-title-face :foreground ,ctp-text)
         (elfeed-search-unread-title-face :foreground ,ctp-red)
         (elfeed-search-feed-face :foreground ,ctp-text :weight bold)
         (elfeed-search-tag-face :foreground ,ctp-green)
         (elfeed-search-last-update-face :weight bold)
         (elfeed-search-unread-count-face :foreground ,ctp-pink)
         (elfeed-search-filter-face :foreground ,ctp-green :weight bold)
         (elfeed-log-date-face :inherit elfeed-search-date-face)
         (elfeed-log-error-level-face :inherit error)
         (elfeed-log-warn-level-face :foreground ,ctp-peach)
         (elfeed-log-info-level-face :weight bold)
         (elfeed-log-debug-level-face :weight bold)

         ;; elpher
         (elpher-gemini-heading1 :weight bold :foreground ,ctp-blue)
         (elpher-gemini-heading2 :foreground ,ctp-blue)
         (elpher-gemini-heading3 :foreground ,ctp-blue)
         (elpher-gemini-preformatted :inherit fixed-pitch
           :foreground ,ctp-green)

         ;; erc
         (erc-action-face :foreground ,ctp-green)
         (erc-command-indicator-face :foreground ,ctp-mauve)
         (erc-current-nick-face :foreground ,ctp-peach)
         (erc-dangerous-host-face :background ,ctp-red
           :foreground ,ctp-text)
         (erc-default-face :foreground ,ctp-text)
         (erc-direct-msg-face :foreground ,ctp-rosewater)
         (erc-error-face :foreground ,ctp-red)
         (erc-fool-face :foreground ,ctp-subtext0)
         (erc-header-line :background ,ctp-crust :foreground ,ctp-text)
         (erc-input-face :foreground ,ctp-rosewater)
         (erc-inverse-face :background ,ctp-text :foreground ,ctp-base)
         (erc-keyword-face :foreground ,ctp-lavender)
         (erc-my-nick-face :foreground ,ctp-lavender)
         (erc-my-nick-prefix-face :foreground ,ctp-lavender)
         (erc-nick-default-face :foreground ,ctp-blue)
         (erc-nick-prefix-face :foreground ,ctp-blue)
         (erc-nick-msg-face :foreground ,ctp-yellow)
         (erc-notice-face :foreground ,ctp-subtext0)
         (erc-pal-face :foreground ,ctp-pink)
         (erc-prompt-face :foreground ,ctp-blue)
         (erc-timestamp-face :foreground ,ctp-teal)
         (bg:erc-color-face0 :background ,ctp-text)
         (bg:erc-color-face1 :background ,ctp-crust)
         (bg:erc-color-face2 :background ,ctp-blue)
         (bg:erc-color-face3 :background ,ctp-green)
         (bg:erc-color-face4 :background ,ctp-red)
         (bg:erc-color-face5 :background ,ctp-maroon)
         (bg:erc-color-face6 :background ,ctp-mauve)
         (bg:erc-color-face7 :background ,ctp-peach)
         (bg:erc-color-face8 :background ,ctp-yellow)
         (bg:erc-color-face9 :background ,ctp-green)
         (bg:erc-color-face10 :background ,ctp-lavender)
         (bg:erc-color-face11 :background ,ctp-teal)
         (bg:erc-color-face12 :background ,ctp-sapphire)
         (bg:erc-color-face13 :background ,ctp-pink)
         (bg:erc-color-face14 :background ,ctp-overlay2)
         (bg:erc-color-face15 :background ,ctp-text)
         (fg:erc-color-face0 :foreground ,ctp-text)
         (fg:erc-color-face1 :foreground ,ctp-crust)
         (fg:erc-color-face2 :foreground ,ctp-blue)
         (fg:erc-color-face3 :foreground ,ctp-green)
         (fg:erc-color-face4 :foreground ,ctp-red)
         (fg:erc-color-face5 :foreground ,ctp-maroon)
         (fg:erc-color-face6 :foreground ,ctp-mauve)
         (fg:erc-color-face7 :foreground ,ctp-peach)
         (fg:erc-color-face8 :foreground ,ctp-yellow)
         (fg:erc-color-face9 :foreground ,ctp-green)
         (fg:erc-color-face10 :foreground ,ctp-lavender)
         (fg:erc-color-face11 :foreground ,ctp-teal)
         (fg:erc-color-face12 :foreground ,ctp-sapphire)
         (fg:erc-color-face13 :foreground ,ctp-pink)
         (fg:erc-color-face14 :foreground ,ctp-overlay2)
         (fg:erc-color-face15 :foreground ,ctp-text)

         ;; eglot-supplements
         (eglot-cthier-recursive-mark-face :foreground ,ctp-red)
         (eglot-marocc-occurence-text :foreground ,ctp-green)

         ;; enh-ruby
         (enh-ruby-heredoc-delimiter-face :foreground ,ctp-yellow)
         (enh-ruby-op-face :inherit haskell-operator-face)
         (enh-ruby-regexp-delimiter-face :foreground ,ctp-yellow)
         (enh-ruby-string-delimiter-face :foreground ,ctp-yellow)

         ;; flymake
         (flymake-error :underline (:style wave :color ,ctp-red))
         (flymake-note :underline (:style wave :color ,ctp-green))
         (flymake-warning :underline (:style wave :color ,ctp-yellow))

         ;; flyspell
         (flyspell-duplicate :underline (:style wave :color ,ctp-teal))
         (flyspell-incorrect :underline (:style wave :color ,ctp-maroon))

         ;; font-latex
         (font-latex-bold-face :foreground ,ctp-red :weight bold)
         (font-latex-italic-face :foreground ,ctp-yellow :slant italic)
         (font-latex-match-reference-keywords :foreground ,ctp-teal)
         (font-latex-match-variable-keywords :foreground ,ctp-text)
         (font-latex-string-face :foreground ,ctp-green)
         (font-latex-warning-face :inherit warning)
         ;; TODO: More latex faces to be themed, especially sections

         ;; gemini
         (gemini-heading-face-1 :weight bold :foreground ,ctp-blue)
         (gemini-heading-face-2 :foreground ,ctp-blue)
         (gemini-heading-face-3 :foreground ,ctp-blue)
         (gemini-heading-face-rest :foreground ,ctp-blue)
         (gemini-quote-face :foreground ,ctp-green)

         ;; gnus
         (gnus-header-content :foreground ,ctp-sky)
         (gnus-header-from :foreground ,ctp-mauve)
         (gnus-header-name :foreground ,ctp-blue :weight bold)
         (gnus-header-subject :foreground ,ctp-mauve :weight bold)

         ;; go-test
         (go-test--ok-face :inherit success)
         (go-test--error-face :inherit error)
         (go-test--warning-face :inherit warning)
         (go-test--pointer-face :foreground ,ctp-pink)
         (go-test--standard-face :foreground ,ctp-teal)

         ;; haskell-mode
         (haskell-operator-face :foreground ,ctp-sky)
         (haskell-constructor-face :foreground ,ctp-mauve)

         ;; helm
         (helm-bookmark-w3m :foreground ,ctp-subtext1)
         (helm-buffer-not-saved :foreground ,ctp-red)
         (helm-buffer-process :foreground ,ctp-red)
         (helm-buffer-saved-out :foreground ,ctp-green)
         (helm-buffer-size :foreground ,ctp-subtext1)
         (helm-candidate-number :foreground ,ctp-yellow)
         (helm-ff-directory :foreground ,ctp-blue)
         (helm-ff-dotted-directory :foreground ,ctp-blue)
         (helm-ff-executable :foreground ,ctp-sky)
         (helm-ff-file :foreground ,ctp-text)
         (helm-ff-invalid-symlink :foreground ,ctp-red)
         (helm-ff-prefix :foreground ,ctp-yellow)
         (helm-ff-symlink :foreground ,ctp-teal)
         (helm-grep-cmd-line :foreground ,ctp-yellow)
         (helm-grep-file :foreground ,ctp-red)
         (helm-grep-finish :foreground ,ctp-red)
         (helm-grep-lineno :foreground ,ctp-subtext0)
         (helm-grep-match :inherit match)
         (helm-grep-running :foreground ,undef)
         (helm-header :foreground ,ctp-subtext1)
         (helm-moccur-buffer :foreground ,undef)
         (helm-selection :underline nil)
         (helm-selection :foreground ,ctp-peach :background ,ctp-surface0)
         (helm-selection-line)
         (helm-separator :foreground ,ctp-subtext1)
         (helm-source-go-package-godoc-description :foreground ,ctp-red)
         (helm-source-header :foreground ,ctp-subtext1)
         (helm-time-zone-current :foreground ,ctp-text)
         (helm-time-zone-home :foreground ,ctp-subtext0)
         (helm-visible-mark :foreground ,ctp-red)

         ;; powerline
         (powerline-active1 :foreground ,ctp-peach :background ,ctp-surface0)
         (powerline-active2 :foreground ,ctp-rosewater :background ,ctp-surface0)
         (powerline-inactive1 :foreground ,ctp-text :background ,ctp-surface2)
         (powerline-inactive2 :foreground ,ctp-subtext1 :background ,ctp-overlay2)

         ;; consult
         (consult-async-split :foreground ,ctp-mauve)

         ;; corfu
         (corfu-default :background ,ctp-surface0)
         (corfu-current :background ,ctp-surface1)
         (corfu-bar :background ,ctp-subtext0)
         (corfu-border :inherit corfu-default)
         (corfu-annotations :inherit font-lock-comment-face)
         (corfu-deprecated :strike-through t)

         ;; highlight-indentation minor mode
         (highlight-indentation-face :background ,ctp-mantle)

         ;; icicle
         ;; TODO: Verify this looks proper
         (icicle-whitespace-highlight :background ,ctp-text)
         (icicle-special-candidate :foreground ,ctp-subtext1)
         (icicle-extra-candidate :foreground ,ctp-subtext1)
         (icicle-search-main-regexp-others :foreground ,ctp-text)
         (icicle-search-current-input :foreground ,ctp-pink)
         (icicle-search-context-level-8 :foreground ,ctp-blue)
         (icicle-search-context-level-7 :foreground ,ctp-blue)
         (icicle-search-context-level-6 :foreground ,ctp-blue)
         (icicle-search-context-level-5 :foreground ,ctp-blue)
         (icicle-search-context-level-4 :foreground ,ctp-blue)
         (icicle-search-context-level-3 :foreground ,ctp-blue)
         (icicle-search-context-level-2 :foreground ,ctp-blue)
         (icicle-search-context-level-1 :foreground ,ctp-blue)
         (icicle-search-main-regexp-current :foreground ,ctp-text)
         (icicle-saved-candidate :foreground ,ctp-text)
         (icicle-proxy-candidate :foreground ,ctp-text)
         (icicle-mustmatch-completion :foreground ,ctp-mauve)
         (icicle-multi-command-completion :foreground ,ctp-subtext0)
         (icicle-msg-emphasis :foreground ,ctp-green)
         (icicle-mode-line-help :foreground ,ctp-overlay2)
         (icicle-match-highlight-minibuffer :foreground ,ctp-mauve)
         (icicle-match-highlight-Completions :foreground ,ctp-green)
         (icicle-key-complete-menu-local :foreground ,ctp-text)
         (icicle-key-complete-menu :foreground ,ctp-text)
         (icicle-input-completion-fail-lax :foreground ,ctp-maroon)
         (icicle-input-completion-fail :foreground ,ctp-maroon)
         (icicle-historical-candidate-other :foreground ,ctp-text)
         (icicle-historical-candidate :foreground ,ctp-text)
         (icicle-current-candidate-highlight :foreground ,ctp-pink)
         (icicle-Completions-instruction-2 :foreground ,ctp-overlay2)
         (icicle-Completions-instruction-1 :foreground ,ctp-overlay2)
         (icicle-completion :foreground ,ctp-text)
         (icicle-complete-input :foreground ,ctp-peach)
         (icicle-common-match-highlight-Completions
           :foreground ,ctp-mauve)
         (icicle-candidate-part :foreground ,ctp-text)
         (icicle-annotation :foreground ,ctp-overlay2)

         ;; icomplete
         (icompletep-determined :foreground ,ctp-blue)
         (icomplete-selected-match :inherit match)

         ;; ido
         (ido-first-match :foreground ,ctp-green)
         (ido-only-match :foreground ,ctp-green)
         (ido-subdir :inherit dired-directory)
         (ido-virtual :foreground ,ctp-sapphire)
         (ido-incomplete-regexp :inherit warning)
         (ido-indicator :foreground ,ctp-text :weight bold)

         ;; iedit
         (iedit-occurrence :inherit match :weight bold)
         (iedit-read-only-occurrence :inherit 'region)

         ;; indent-guide
         (indent-guide-face :foreground ,ctp-surface1)

         ;; ivy
         (ivy-current-match :background ,ctp-red :foreground ,ctp-mantle
           :bold t)
         (ivy-action :background unspecified :foreground ,ctp-lavender)
         (ivy-grep-line-number :background unspecified
           :foreground ,ctp-teal)
         (ivy-minibuffer-match-face-1 :background unspecified
           :foreground ,ctp-blue :bold t)
         (ivy-minibuffer-match-face-2
           :background ,ctp-teal   :foreground ,ctp-mantle)
         (ivy-minibuffer-match-face-3
           :background unspecified :foreground ,ctp-lavender)
         (ivy-minibuffer-match-face-4 :background unspecified
           :foreground ,ctp-mauve)
         (ivy-minibuffer-match-highlight
           :foreground ,ctp-blue)
         (ivy-grep-info :foreground ,ctp-blue)
         (ivy-grep-line-number :foreground ,ctp-mauve)
         (ivy-confirm-face :foreground ,ctp-green)
         (ivy-remote :foreground ,ctp-mauve)
         (ivy-match-required-face :foreground ,ctp-red)

         ;; isearch
         (isearch :inherit match :weight bold)
         (isearch-fail :inherit error)

         ;; jde-java
         (jde-java-font-lock-constant-face
           :inherit font-lock-constant-face)
         (jde-java-font-lock-modifier-face
           :inherit font-lock-keyword-face)
         (jde-java-font-lock-number-face :foreground ,ctp-text)
         (jde-java-font-lock-package-face :foreground ,ctp-text)
         (jde-java-font-lock-private-face
           :inherit font-lock-keyword-face)
         (jde-java-font-lock-public-face :inherit font-lock-keyword-face)

         ;; js2-mode
         (js2-external-variable :foreground ,ctp-red)
         (js2-function-param
           :inherit tree-sitter-hl-face:variable.parameter)
         (js2-jsdoc-html-tag-delimiter
           :inherit web-mode-html-tag-bracket-face)
         (js2-jsdoc-html-tag-name :inherit web-mode-html-tag-face)
         (js2-jsdoc-value :foreground ,ctp-text)
         (js2-private-function-call
           :inherit tree-sitter-hl-face:function.call)
         (js2-private-member :inherit font-lock-variable-name-face)

         ;; js3-mode
         (js3-error-face :inherit error)
         (js3-external-variable-face :foreground ,ctp-text)
         (js3-function-param-face :inherit js2-function-param)
         (js3-instance-member-face :inherit font-lock-variable-name-face)
         (js3-jsdoc-tag-face :inherit web-mode-html-tag-face)
         (js3-warning-face :inherit warning)

         ;; lsp
         (lsp-ui-peek-peek :background ,ctp-mantle)
         (lsp-ui-peek-list :background ,ctp-mantle)
         (lsp-ui-peek-filename :foreground ,ctp-text)
         (lsp-ui-peek-line-number :inherit default
           :foreground ,ctp-surface1)
         (lsp-ui-peek-highlight
           :inherit highlight :distant-foreground ,ctp-base)
         (lsp-ui-peek-header :background ,ctp-mantle
           :foreground ,ctp-blue :weight bold)
         (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
         (lsp-ui-peek-selection :inherit match)
         (lsp-ui-sideline-symbol :foreground ,ctp-subtext0)
         (lsp-ui-sideline-current-symbol :foreground ,ctp-text
           :weight bold)
         (lsp-ui-sideline-code-action :foreground ,ctp-yellow)
         (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
         (lsp-ui-doc-background :background ,ctp-mantle)
         (lsp-ui-doc-header :foreground ,ctp-sapphire)

         ;; magit
         (magit-branch-local :foreground ,ctp-teal)
         (magit-branch-remote :foreground ,ctp-green)
         (magit-tag :foreground ,ctp-peach)
         (magit-section-heading :foreground ,ctp-blue :weight bold)
         (magit-section-highlight :background ,ctp-surface0 :extend t)
         (magit-diff-context-highlight :background ,ctp-surface0
           :foreground ,ctp-text :extend t)
         (magit-diff-revision-summary :foreground ,ctp-blue :weight bold)
         (magit-diff-revision-summary-highlight :foreground ,ctp-blue
           :weight bold)
         (magit-diff-added :foreground ,ctp-green :extend t)
         (magit-diff-added-highlight :background ,ctp-surface1
           :foreground ,ctp-green :extend t)
         (magit-diff-removed :foreground ,ctp-red :extend t)
         (magit-diff-removed-highlight :background ,ctp-surface1
           :foreground ,ctp-red :extend t)
         (magit-diff-file-heading :foreground ,ctp-text)
         (magit-diff-file-heading-highlight
           :inherit magit-section-highlight)
         (magit-diffstat-added :foreground ,ctp-green)
         (magit-diffstat-removed :foreground ,ctp-red)
         (magit-hash :foreground ,ctp-subtext0)
         (magit-diff-hunk-heading :inherit diff-hunk-header)
         (magit-diff-hunk-heading-highlight :inherit diff-hunk-header
           :weight bold)
         (magit-log-author :foreground ,ctp-subtext0)
         (magit-process-ng :foreground ,ctp-peach :weight bold)
         (magit-process-ok :foreground ,ctp-green :weight bold)

         ;; markdown
         (markdown-blockquote-face :extend t :background ,ctp-mantle
           :foreground ,ctp-green
           ,@(when catppuccin-italic-blockquotes '(:slant italic)))
         (markdown-code-face :foreground ,ctp-text)
         (markdown-footnote-face :foreground ,ctp-yellow)
         (markdown-header-face :weight normal)
         (markdown-header-face-1 :foreground ,ctp-red)
         (markdown-header-face-2 :foreground ,ctp-peach)
         (markdown-header-face-3 :foreground ,ctp-yellow)
         (markdown-header-face-4 :foreground ,ctp-green)
         (markdown-header-face-5 :foreground ,ctp-sapphire)
         (markdown-header-face-6 :foreground ,ctp-lavender)
         (markdown-inline-code-face :foreground ,ctp-green)
         (markdown-plain-url-face :inherit link)
         (markdown-pre-face :foreground ,ctp-green)
         (markdown-table-face :foreground ,ctp-text)
         (markdown-list-face :foreground ,ctp-mauve)
         (markdown-language-keyword-face :inherit font-lock-comment-face)

         ;; message
         (message-header-to :foreground ,ctp-text :weight bold)
         (message-header-cc :foreground ,ctp-text :weight bold)
         (message-header-subject :foreground ,ctp-blue)
         (message-header-newsgroups :foreground ,ctp-mauve)
         (message-header-other :foreground ,ctp-mauve)
         (message-header-name :foreground ,ctp-green)
         (message-header-xheader :foreground ,ctp-lavender)
         (message-separator :inherit font-lock-comment-face)
         (message-cited-text :foreground ,ctp-green)
         (message-cited-text-1 :foreground ,ctp-yellow)
         (message-cited-text-2 :inherit font-lock-comment-face)
         (message-cited-text-3 :inherit font-lock-comment-face)
         (message-cited-text-4 :inherit font-lock-comment-face)
         (message-mml :foreground ,ctp-green :weight normal)

         ;; mini-modeline
         (mini-modeline-mode-line :inherit mode-line :height 0.1
           :box nil)
         ;; mu4e
         (mu4e-unread-face :foreground ,ctp-red)
         (mu4e-view-url-number-face :foreground ,ctp-yellow)
         (mu4e-highlight-face :background ,ctp-base
           :weight bold
           :extend t)
         (mu4e-header-highlight-face :background ,ctp-current
           :underline nil :weight bold
           :extend t)
         (mu4e-header-key-face :inherit message-mml)
         (mu4e-header-marks-face :foreground ,ctp-mauve)
         (mu4e-replied-face :foreground ,ctp-green)
         (mu4e-forwarded-face :foreground ,ctp-peach)
         (mu4e-cited-1-face :foreground ,ctp-green)
         (mu4e-cited-2-face :foreground ,ctp-yellow)
         (mu4e-cited-3-face :inherit font-lock-comment-face)
         (mu4e-cited-4-face :inherit font-lock-comment-face)
         (mu4e-cited-5-face :inherit font-lock-comment-face)
         ;; mu4e-column-faces
         (mu4e-column-faces-date :foreground ,ctp-blue)
         (mu4e-column-faces-flags :foreground ,ctp-yellow)
         (mu4e-column-faces-to-from :foreground ,ctp-sky)

         ;; neotree
         (neo-banner-face :foreground ,ctp-blue :weight bold)
         ;;(neo-button-face :underline nil)
         (neo-dir-link-face :foreground ,ctp-blue :weight bold)
         (neo-file-link-face :inherit link)
         (neo-expand-btn-face :foreground ,ctp-text)
         (neo-header-face :weight bold)
         (neo-root-dir-face :foreground ,ctp-green :weight bold)
         (neo-vc-added-face :foreground ,ctp-green)
         (neo-vc-conflict-face :inherit error)
         (neo-vc-default-face :inherit default)
         (neo-vc-edited-face :foreground ,ctp-peach)
         (neo-vc-ignored-face :inherit font-lock-comment-face)
         (neo-vc-missing-face :foreground ,ctp-maroon)
         (neo-vc-needs-merge-face :foreground ,ctp-maroon
           :weight bold)
         ;;(neo-vc-needs-update-face :underline t)
         (neo-vc-removed-face :foreground ,ctp-red)
         (neo-vc-unlocked-changes-face :foreground ,ctp-red)
         ;;(neo-vc-unregistered-face nil)
         (neo-vc-up-to-date-face :foreground ,ctp-text)
         (neo-vc-user-face :foreground ,ctp-mauve)

         ;; orderless
         (orderless-match-face-0 :foreground ,ctp-blue :weight bold)
         (orderless-match-face-1 :foreground ,ctp-mauve :weight bold)
         (orderless-match-face-2 :foreground ,ctp-teal :weight bold)
         (orderless-match-face-3 :foreground ,ctp-peach :weight bold)

         ;; org
         (org-agenda-date :foreground ,ctp-subtext0 :weight normal)
         (org-agenda-date-today :foreground ,ctp-subtext0 :weight bold)
         (org-agenda-date-weekend :inherit org-agenda-date)
         (org-agenda-date-weekend-today :inherit org-agenda-date
           :weight bold)
         (org-agenda-dimmed-todo-face :inherit font-lock-comment-face)
         (org-agenda-done :foreground ,ctp-green)
         (org-agenda-structure :foreground ,ctp-subtext0)
         (org-block :extend t :background ,ctp-mantle
           :foreground ,ctp-green)
         (org-block-begin-line :inherit org-meta-line :extend t
           :background ,ctp-mantle)
         (org-block-end-line :inherit org-block-begin-line :extend t
           :background ,ctp-mantle)
         (org-code :foreground ,ctp-green)
         (org-column :background ,ctp-surface0)
         (org-column-title :inherit org-column :weight bold :underline t)
         (org-date :inherit org-agenda-date)
         (org-document-info :foreground ,ctp-sapphire)
         (org-document-info-keyword :inherit font-lock-comment-face)
         (org-document-title :weight bold :foreground ,ctp-blue)
         (org-done :inherit font-lock-comment-face)
         (org-ellipsis :inherit font-lock-comment-face)
         (org-footnote :foreground ,ctp-mauve)
         (org-formula :foreground ,ctp-pink)
         (org-headline-done :inherit org-done)
         (org-hide :foreground ,ctp-crust :background ,ctp-base)
         (org-indent :foreground ,ctp-base)
         (org-level-1 :inherit bold :foreground ,ctp-red)
         (org-level-2 :inherit bold :foreground ,ctp-peach)
         (org-level-3 :weight normal :foreground ,ctp-yellow)
         (org-level-4 :weight normal :foreground ,ctp-green)
         (org-level-5 :weight normal :foreground ,ctp-sapphire)
         (org-level-6 :weight normal :foreground ,ctp-lavender)
         (org-level-7 :weight normal :foreground ,ctp-mauve)
         (org-level-8 :weight normal :foreground ,ctp-maroon)
         (org-link :inherit link)
         (org-meta-line :inherit font-lock-comment-face)
         (org-mode-line-clock-overrun :inherit mode-line :foreground ,ctp-red)
         (org-priority :foreground ,ctp-yellow)
         (org-quote :extend t :background ,ctp-mantle
           :foreground ,ctp-green
           ,@(when catppuccin-italic-blockquotes '(:slant italic)))
         (org-scheduled :foreground ,ctp-green)
         (org-scheduled-previously :foreground ,ctp-teal)
         (org-scheduled-today :foreground ,ctp-green :weight bold)
         (org-sexp-date :foreground ,ctp-subtext0)
         (org-special-keyword :inherit font-lock-keyword-face)
         (org-table :foreground ,ctp-overlay0)
         (org-tag :foreground ,ctp-mauve :weight bold)
         (org-todo :foreground ,ctp-peach)
         (org-upcoming-deadline :foreground ,ctp-maroon)
         (org-verbatim :inherit org-quote)
         (org-warning :inherit warning)

         ;; calfw
         (cfw:face-title :foreground ,ctp-blue :weight bold :height 1.5)
         (cfw:face-header :foreground ,ctp-text)
         (cfw:face-sunday :foreground ,ctp-overlay1)
         (cfw:face-saturday :foreground ,ctp-overlay1)
         (cfw:face-holiday :foreground ,ctp-green)
         (cfw:face-grid :foreground ,ctp-surface0)
         (cfw:face-default-content :foreground ,ctp-peach)
         (cfw:face-periods :foreground ,undef)
         (cfw:face-day-title :foreground ,ctp-subtext0)
         (cfw:face-default-day :foreground ,ctp-text)
         (cfw:face-annotation :foreground ,undef)
         (cfw:face-disable :foreground ,ctp-surface1)
         (cfw:face-today-title :foreground ,ctp-peach)
         (cfw:face-today :inherit cfw:face-today-title)
         (cfw:face-select :background ,ctp-surface1
           :foreground ,ctp-text)
         (cfw:face-toolbar :background ,ctp-base)
         (cfw:face-toolbar-button-off :foreground ,ctp-lavender)
         (cfw:face-toolbar-button-on :foreground ,ctp-mauve)

         ;; outline
         (outline-1 :foreground ,ctp-blue)
         (outline-2 :foreground ,ctp-blue)
         (outline-3 :foreground ,ctp-blue)
         (outline-4 :foreground ,ctp-blue)
         (outline-5 :foreground ,ctp-blue)
         (outline-6 :foreground ,ctp-blue)
         ;; outline-minor-faces
         (outline-minor-1 :foreground ,ctp-lavender :weight ultra-bold)
         (outline-minor-2 :foreground ,ctp-pink :weight bold)
         (outline-minor-3 :foreground ,ctp-blue :weight bold)
         (outline-minor-4 :foreground ,ctp-red)
         (outline-minor-5 :foreground ,ctp-green)
         (outline-minor-6 :foreground ,ctp-peach)
         (outline-minor-7 :foreground ,ctp-mauve)
         (outline-minor-8 :foreground ,ctp-text)

         ;; perspective
         (persp-selected-face :weight bold :foreground ,ctp-pink)

         ;; popup
         (popup-tip-face :background ,ctp-surface0 :foreground ,ctp-text)

         ;; rainbow-delimiters
         (rainbow-delimiters-depth-1-face :foreground ,ctp-red)
         (rainbow-delimiters-depth-2-face :foreground ,ctp-peach)
         (rainbow-delimiters-depth-3-face :foreground ,ctp-yellow)
         (rainbow-delimiters-depth-4-face :foreground ,ctp-green)
         (rainbow-delimiters-depth-5-face :foreground ,ctp-sapphire)
         (rainbow-delimiters-depth-6-face :foreground ,ctp-lavender)
         (rainbow-delimiters-depth-7-face :foreground ,ctp-mauve)
         (rainbow-delimiters-depth-8-face :foreground ,ctp-maroon)
         (rainbow-delimiters-unmatched-face :inherit warning)

         ;; rst (reStructuredText)
         (rst-level-1 :foreground ,ctp-red)
         (rst-level-2 :foreground ,ctp-peach)
         (rst-level-3 :foreground ,ctp-yellow)
         (rst-level-4 :foreground ,ctp-green)
         (rst-level-5 :foreground ,ctp-sapphire)
         (rst-level-6 :foreground ,ctp-lavender)
         (rst-level-7 :foreground ,ctp-mauve)
         (rst-level-8 :foreground ,ctp-maroon)

         ;; show-paren
         (show-paren-match :foreground ,ctp-pink :weight bold
           ,@(when catppuccin-highlight-matches
               (list :background ctp-surface0)))
         (show-paren-match-expression :inherit match)
         (show-paren-mismatch :inherit warning)

         ;; slime
         (slime-repl-inputed-output-face :foreground ,ctp-mauve)

         ;; smerge
         (smerge-lower :inherit diff-added :background unspecified)
         (smerge-upper :inherit diff-removed :background unspecified)
         (smerge-refined-added :inherit diff-refine-added
           :background unspecified)
         (smerge-refined-removed :inherit diff-refine-removed
           :background unspecified)
         (smerge-base :inherit diff-refine-changed :background unspecified)

         ;; swiper
         ;;(swiper-line-face :inherit swiper-match-face-1)
         ;;(swiper-line-face-1 :inherit swiper-match-face-1)
         (swiper-background-match-face-1 :background ,ctp-teal)
         (swiper-match-face-1 :foreground ,ctp-text :background ,ctp-red)
         (swiper-background-match-face-2 :foreground ,ctp-mantle
           :background ,ctp-teal)
         (swiper-match-face-2 :foreground ,ctp-mantle
           :background ,ctp-red)
         ;;(swiper-background-match-face-3 :inherit swiper-match-face-1)
         ;;(swiper-match-face-3 :inherit swiper-match-face-1)
         ;;(swiper-background-match-face-4 :inherit swiper-match-face-1)
         ;;(swiper-match-face-4 :inherit swiper-match-face-1)

         ;; spam
         (spam :inherit gnus-summary-normal-read :foreground ,ctp-peach
           :strike-through t :slant oblique)

         ;; tab-bar & tab-line (since Emacs 27.1)
         (tab-bar :foreground ,ctp-subtext0 :background ,ctp-base)
         (tab-bar-tab :foreground ,ctp-text :background ,ctp-current)
         (tab-bar-tab-inactive :foreground ,ctp-subtext0
           :background ,ctp-base)
         (tab-line :inherit tab-bar)
         (tab-line-tab :inherit tab-bar-tab)
         (tab-line-tab-inactive :inherit tab-bar-tab-inactive)
         (tab-line-tab-current :inherit tab-line-tab)
         (tab-line-highlight :background ,ctp-surface1)

         ;; centaur-tabs
         (centaur-tabs-default :foreground ,ctp-subtext0
           :background ,ctp-base)
         (centaur-tabs-unselected :foreground ,ctp-subtext0
           :background ,ctp-mantle)
         (centaur-tabs-selected :foreground ,ctp-text
           :background ,ctp-current)
         (centaur-tabs-unselected-modified :foreground ,ctp-maroon
           :background ,ctp-mantle)
         (centaur-tabs-selected-modified :foreground ,ctp-red
           :background ,ctp-current)
         (centaur-tabs-close-unselected :foreground ,ctp-subtext0
           :background ,ctp-mantle)
         (centaur-tabs-close-selected :foreground ,ctp-text
           :background ,ctp-current)
         (centaur-tabs-name-mouse-face :foreground ,ctp-text
           :background ,ctp-surface1)
         (centaur-tabs-close-mouse-face :foreground ,ctp-red
           :background ,ctp-surface1)
         (centaur-tabs-modified-marker-selected
           :inherit centaur-tabs-selected-modified)
         (centaur-tabs-modified-marker-unselected
           :inherit centaur-tabs-unselected-modified)

         ;; term
         (term :foreground ,ctp-text :background ,ctp-base)
         (term-color-black
           ,@(if (eq catppuccin-flavor 'latte)
               (list :foreground ctp-subtext1  :background ctp-subtext1)
               (list :foreground ctp-surface1 :background ctp-surface1)))
         (term-color-black-white
           ,@(if (eq catppuccin-flavor 'latte)
               (list :foreground ctp-subtext0 :background ctp-subtext0)
               (list :foreground ctp-surface2 :background ctp-surface2)))
         (term-color-red :foreground ,ctp-red :background ,ctp-red)
         (term-color-bright-red :foreground ,ctp-red
           :background ,ctp-red)
         (term-color-green :foreground ,ctp-green :background ,ctp-green)
         (term-color-bright-green :foreground ,ctp-green
           :background ,ctp-green)
         (term-color-yellow :foreground ,ctp-yellow
           :background ,ctp-yellow)
         (term-color-bright-yellow :foreground ,ctp-yellow
           :background ,ctp-yellow)
         (term-color-blue :foreground ,ctp-blue :background ,ctp-blue)
         (term-color-bright-blue :foreground ,ctp-blue
           :background ,ctp-blue)
         (term-color-magenta :foreground ,ctp-pink :background ,ctp-pink)
         (term-color-bright-magenta :foreground ,ctp-pink
           :background ,ctp-pink)
         (term-color-cyan :foreground ,ctp-teal :background ,ctp-teal)
         (term-color-bright-cyan :foreground ,ctp-teal
           :background ,ctp-teal)
         (term-color-white
           ,@(if (eq catppuccin-flavor 'latte)
               (list :foreground ctp-surface2  :background ctp-surface2)
               (list :foreground ctp-subtext1 :background ctp-subtext1)))
         (term-color-bright-white
           ,@(if (eq catppuccin-flavor 'latte)
               (list :foreground ctp-surface1 :background ctp-surface1)
               (list :foreground ctp-subtext0 :background ctp-subtext0)))

         ;; ansi-color (emacs >= 28.1)
         (ansi-color-black :foreground ,ctp-surface1)
         (ansi-color-red :foreground ,ctp-red)
         (ansi-color-yellow :foreground ,ctp-yellow)
         (ansi-color-green :foreground ,ctp-green)
         (ansi-color-blue :foreground ,ctp-blue)
         (ansi-color-magenta :foreground ,ctp-pink)
         (ansi-color-cyan :foreground ,ctp-teal)
         (ansi-color-white :foreground ,ctp-subtext1)
         (ansi-color-bright-black :foreground ,ctp-surface2)
         (ansi-color-bright-red :foreground ,ctp-red)
         (ansi-color-bright-yellow :foreground ,ctp-yellow)
         (ansi-color-bright-green :foreground ,ctp-green)
         (ansi-color-bright-blue :foreground ,ctp-blue)
         (ansi-color-bright-magenta :foreground ,ctp-pink)
         (ansi-color-bright-cyan :foreground ,ctp-teal)
         (ansi-color-bright-white :foreground ,ctp-subtext0)

         ;; treemacs
         (treemacs-async-loading-face :foreground ,ctp-text)
         (treemacs-directory-face :foreground ,ctp-blue)
         (treemacs-directory-collapsed-face :foreground ,ctp-blue)
         (treemacs-file-face :foreground ,ctp-text)
         (treemacs-fringe-indicator-face :foreground ,ctp-text)
         (treemacs-git-added-face :foreground ,ctp-green)
         (treemacs-git-commit-diff-face :foreground ,ctp-green)
         (treemacs-git-conflict-face :foreground ,ctp-red)
         (treemacs-git-ignored-face :foreground ,ctp-overlay1)
         (treemacs-git-modified-face :foreground ,ctp-red)
         (treemacs-git-renamed-face :foreground ,ctp-blue)
         (treemacs-git-unmodified-face :foreground ,ctp-text)
         (treemacs-git-untracked-face :foreground ,ctp-green)
         (treemacs-header-button-face :foreground ,ctp-text)
         (treemacs-help-column-face :foreground ,ctp-blue)
         (treemacs-help-title-face :foreground ,ctp-text)
         (treemacs-hl-line-face :background ,ctp-current :extend t)
         (treemacs-marked-file-face :foreground ,ctp-red)
         (treemacs-nerd-icons-face :foreground ,ctp-blue)
         (treemacs-on-failure-pulse-face :foreground ,ctp-text)
         (treemacs-on-success-pulse-face :foreground ,ctp-text)
         (treemacs-peek-mode-indicator-face :foreground ,ctp-text)
         (treemacs-remote-face :foreground ,ctp-text)
         (treemacs-root-face :foreground ,ctp-blue :background ,ctp-base)
         (treemacs-root-remote-disconnected-face :foreground ,ctp-yellow)
         (treemacs-root-remote-unreadable-face :foreground ,ctp-yellow)
         (treemacs-root-unreadable-face :foreground ,ctp-red)
         (treemacs-tags-face :foreground ,ctp-text)
         (treemacs-term-node-face :foreground ,ctp-blue)
         (treemacs-window-background-face :background ,ctp-base)
         ;; treemacs-nerd-icons
         (treemacs-nerd-icons-root-face :foreground ,ctp-blue)
         (treemacs-nerd-icons-file-face :foreground ,ctp-blue)

         ;; tree-sitter
         (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
         (tree-sitter-hl-face:property :foreground ,ctp-lavender)
         (tree-sitter-hl-face:property.definition
           :foreground ,ctp-lavender)
         (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
         (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
         (tree-sitter-hl-face:constant.builtin
           :inherit font-lock-builtin-face)
         (tree-sitter-hl-face:constructor
           :inherit font-lock-constant-face)
         (tree-sitter-hl-face:escape :foreground ,undef)
         (tree-sitter-hl-face:function
           :inherit font-lock-function-name-face)
         (tree-sitter-hl-face:function.builtin
           :inherit font-lock-builtin-face)
         (tree-sitter-hl-face:function.call
           :inherit font-lock-function-name-face :weight normal)
         (tree-sitter-hl-face:function.macro
           :inherit font-lock-preprocessor-face)
         (tree-sitter-hl-face:function.special
           :inherit font-lock-preprocessor-face)
         (tree-sitter-hl-face:keyword
           :inherit font-lock-keyword-face) ; we are missing keyword levels (eg.: Typescript: import / const should be different: blue / mauve)
         (tree-sitter-hl-face:punctuation :foreground ,undef)
         (tree-sitter-hl-face:punctuation.bracket
           :foreground ,ctp-overlay2)
         (tree-sitter-hl-face:punctuation.delimiter
           :foreground ,ctp-overlay2)
         (tree-sitter-hl-face:punctuation.special :foreground ,undef)
         (tree-sitter-hl-face:string :inherit font-lock-string-face)
         (tree-sitter-hl-face:string.special :foreground ,ctp-teal)
         (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
         (tree-sitter-hl-face:type :inherit font-lock-type-face)
         (tree-sitter-hl-face:type.parameter :foreground ,ctp-sapphire)
         (tree-sitter-hl-face:variable
           :inherit font-lock-variable-name-face)
         (tree-sitter-hl-face:variable.parameter :foreground ,ctp-red)
         (tree-sitter-hl-face:operator :foreground ,ctp-teal)

         ;; undo-tree
         (undo-tree-visualizer-current-face :foreground ,ctp-peach)
         (undo-tree-visualizer-default-face :foreground ,ctp-subtext0)
         (undo-tree-visualizer-register-face :foreground ,ctp-mauve)
         (undo-tree-visualizer-unmodified-face :foreground ,ctp-text)

         ;; web-mode
         (web-mode-builtin-face :inherit font-lock-builtin-face)
         (web-mode-comment-face :inherit font-lock-comment-face)
         (web-mode-constant-face :inherit font-lock-constant-face)
         (web-mode-css-property-name-face
           :inherit font-lock-constant-face)
         (web-mode-doctype-face :inherit font-lock-comment-face)
         (web-mode-function-name-face
           :inherit font-lock-function-name-face)
         (web-mode-html-attr-name-face :foreground ,ctp-blue)
         (web-mode-html-attr-value-face :foreground ,ctp-green)
         (web-mode-html-tag-face :foreground ,ctp-mauve)
         (web-mode-keyword-face :inherit font-lock-keyword-face)
         (web-mode-string-face :inherit font-lock-string-face)
         (web-mode-type-face :inherit font-lock-type-face)
         (web-mode-warning-face :inherit warning)

         ;; which-func
         (which-func :inherit font-lock-function-name-face)

         ;; which-key
         (which-key-key-face :inherit font-lock-builtin-face)
         (which-key-command-description-face :inherit default)
         (which-key-separator-face
           :inherit font-lock-comment-delimiter-face)
         (which-key-local-map-description-face :foreground ,ctp-green)

         ;; whitespace
         (whitespace-big-indent :foreground ,ctp-peach)
         (whitespace-empty :inherit warning)
         (whitespace-hspace :background ,undef :foreground ,undef)
         (whitespace-indentation :foreground ,ctp-surface0)
         (whitespace-line :underline (:style wave :color ,ctp-mauve))
         (whitespace-newline :inherit font-lock-comment-face)
         (whitespace-space :inherit font-lock-comment-face)
         (whitespace-space-after-tab :inherit warning)
         (whitespace-space-before-tab :inherit warning)
         (whitespace-tab :inherit whitespace-newline)
         (whitespace-trailing :inherit trailing-whitespace)
         (trailing-whitespace :foreground ,ctp-peach
           :background ,ctp-peach)

         ;; yard-mode
         (yard-tag-face :inherit font-lock-builtin-face)
         (yard-directive-face :inherit font-lock-builtin-face)

         ;; line-reminder
         (line-reminder-modified-sign-face :foreground ,ctp-green)

         ;; highlight-indent-guides
         ;;(highlight-indent-guides-odd-face :background ,ctp-base)
         ;;(highlight-indent-guides-even-face :background ,ctp-base)
         (highlight-indent-guides-character-face
           :foreground ,ctp-surface0)
         ;;(highlight-indent-guides-top-odd-face :background ,ctp-base)
         ;;(highlight-indent-guides-top-even-face :background ,ctp-base)
         (highlight-indent-guides-top-character-face
           :foreground ,ctp-pink)
         ;;(highlight-indent-guides-stack-odd-face :background ,ctp-base)
         ;;(highlight-indent-guides-stack-even-face
         ;;  :background ,ctp-base)
         (highlight-indent-guides-stack-character-face
           :foreground ,ctp-flamingo)

         ;; lui
         (lui-button-face :foreground ,ctp-sky :underline t)
         (lui-highlight-face :foreground ,ctp-sky)
         (lui-time-stamp-face :foreground ,ctp-lavender :weight bold)

         ;; circe
         (circe-fool-face :foreground ,ctp-subtext1)
         (circe-highlight-nick-face :foreground ,ctp-sky :weight bold)
         (circe-prompt-face :foreground ,ctp-base
           :background ,ctp-teal
           :weight bold)
         (circe-server-face :foreground ,ctp-blue :weight bold))))

  (apply #'custom-theme-set-faces
    'catppuccin
    (let* ((expand-with-func
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
                   (t                      ; 256 color terminals
                     ,(funcall expand-with-func
                        '(lambda (v) (cadr (cdr v))) spec))))
          whole-theme))
      whole-theme))

  (apply #'custom-theme-set-variables
    'catppuccin
    (let ((get-func
            (pcase (display-color-cells)
              ((pred (<= 16777216)) 'car) ; fully graphical envs
              (_ 'cadr))))                ; 256 color terminals
      `((ansi-color-names-vector
          [,(funcall get-func
              (alist-get (if (eq catppuccin-flavor 'latte)
                           'ctp-subtext1  'ctp-surface1) colors))
            ,(funcall get-func (alist-get 'ctp-red colors))
            ,(funcall get-func (alist-get 'ctp-green colors))
            ,(funcall get-func (alist-get 'ctp-yellow colors))
            ,(funcall get-func (alist-get 'ctp-blue colors))
            ,(funcall get-func (alist-get 'ctp-pink colors))
            ,(funcall get-func (alist-get 'ctp-teal colors))
            ,(funcall get-func
               (alist-get (if (eq catppuccin-flavor 'latte)
                            'ctp-surface2 'ctp-subtext1) colors))]))
      `((rustic-ansi-faces
          (vector
            ,(funcall get-func
               (alist-get (if (eq catppuccin-flavor 'latte)
                            'ctp-subtext1  'ctp-surface1) colors))
            ,(funcall get-func (alist-get 'ctp-red colors))
            ,(funcall get-func (alist-get 'ctp-green colors))
            ,(funcall get-func (alist-get 'ctp-yellow colors))
            ,(funcall get-func (alist-get 'ctp-blue colors))
            ,(funcall get-func (alist-get 'ctp-pink colors))
            ,(funcall get-func (alist-get 'ctp-teal colors))
            ,(funcall get-func
               (alist-get (if (eq catppuccin-flavor 'latte)
                            'ctp-surface2  'ctp-subtext1) colors))))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'catppuccin)

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; End:

;;; catppuccin-theme.el ends here
