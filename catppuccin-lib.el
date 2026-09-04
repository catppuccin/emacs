;;; catppuccin-lib.el - -*- lexical-binding: t; -*-

(eval-and-compile
  (unless (require 'modus-themes nil :noerror)
    ;; Fall back to the copy bundled with Emacs (etc/themes is not in
    ;; `load-path', so a plain `require' cannot find it).
    (require-theme 'modus-themes)))

;; Color operations

;;;###autoload
(defun catppuccin--hex-to-rgb (color)
  "Convert a hex COLOR string like \"#rrggbb\" to a list of three integers."
  (mapcar (lambda (i) (string-to-number (substring color i (+ i 2)) 16))
          '(1 3 5)))

;;;###autoload
(defun catppuccin--rgb-to-hex (r g b)
  "Convert R, G, B integers to a hex color string."
  (format "#%02x%02x%02x" r g b))

;;;###autoload
(defun catppuccin--rnd (n)
  "Round N to the nearest integer."
  (round (+ 0.5 n)))

;;;###autoload
(defun catppuccin-lighten (color value)
  "Lighten COLOR by VALUE% (0–100)."
  (let* ((factor (/ value 100.0)))
    (apply #'catppuccin--rgb-to-hex
      (mapcar (lambda (v)
                (catppuccin--rnd
                  (min 255 (+ (* (- 255 v) factor) v))))
        (catppuccin--hex-to-rgb color)))))

;;;###autoload
(defun catppuccin-darken (color value)
  "Darken COLOR by VALUE% (0–100)."
  (let* ((factor (/ value 100.0)))
    (apply #'catppuccin--rgb-to-hex
      (mapcar (lambda (v)
                (floor (* (- 1 factor) v)))
        (catppuccin--hex-to-rgb color)))))

;;;###autoload
(defmacro defcatppuccin (flavor-name &rest body)
  (let* ((flavor            (symbol-name flavor-name))
         (light-p           (string-equal flavor "latte"))
         (definitions       (intern (format "catppuccin-definitions-%s" flavor)))
         (id                (intern (format "catppuccin-%s" flavor)))
         (theme             (intern (format "catppuccin-%s-theme" flavor)))
         (palette           (intern (format "catppuccin-%s-theme-palette" flavor)))
         (palette-user      (intern (format "catppuccin-%s-theme-palette-user" flavor)))
         (palette-overrides (intern (format "catppuccin-%s-theme-palette-overrides" flavor)))
         (faces             (intern (format "catppuccin-%s-theme-faces" flavor)))
         (custom            (intern (format "catppuccin-%s-theme-custom-variables" flavor))))
    `(progn
       (defgroup ,theme ()
         ,(format "Soothing pastel theme - %s variant.
Built on the Modus themes infrastructure." flavor)
         :group 'modus-themes
         :link '(url-link :tag "GitHub" "https://github.com/catppuccin/emacs")
         :prefix ,(format "catppuccin-%s-" flavor)
         :tag ,(format "catppuccin-%s" flavor))

       (defcustom ,palette-user nil
         ,(format "Like `catppuccin-%s-theme-palette' for user-defined entries.
This is meant to extend the palette with custom named colors and/or
semantic palette mappings.  Those may then be used in combination with
palette overrides (also see `modus-themes-common-palette-overrides' and
`catppuccin-%s-theme-palette-overrides')." flavor flavor)
         :group ',theme
         :type '(repeat (list symbol (choice symbol string)))
         :link '(info-link "(modus-themes) Option to extend the palette for use with overrides"))

       (defcustom ,palette-overrides nil
         ,(format "Overrides for `catppuccin-%s-theme-palette'.
Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides." flavor)
         :group ',theme
         :type '(repeat (list symbol (choice symbol string)))
         :link '(info-link "(modus-themes) Palette overrides"))

       (defconst ,palette
         (cl-flet* ((color (name)
                      (car (alist-get name ,definitions)))
                    (intensify (name &optional value)
                      (funcall #',(if light-p 'catppuccin-darken 'catppuccin-lighten)
                               (color name) (or value 5)))
                    (dim (name &optional value)
                      (funcall #',(if light-p 'catppuccin-lighten 'catppuccin-darken)
                               (color name) (or value 5))))
           (append
            ,definitions
            `(
              (fg-alt subtext0)
              (fg-dim overlay0)

              ;; Special purpose

              (bg-completion ,(intensify 'base))
              (bg-hl-line ,(intensify 'base))
              (bg-region ,(dim 'base ,(if light-p 12 17)))
              (bg-popup mantle)
              (bg-hover bg-cyan-intense)
              (bg-hover-secondary bg-yellow-subtle)

              ;; Mode-line

              (bg-mode-line-active mantle)
              (fg-mode-line-active text)
              (border-mode-line-active unspecified)
              (bg-mode-line-inactive crust)
              (fg-mode-line-inactive overlay0)
              (border-mode-line-inactive unspecified)

              (modeline-err red)
              (modeline-warning yellow)
              (modeline-info sapphire)

              ;; Tab bar

              (bg-tab-bar base)
              (bg-tab-current mantle)
              (bg-tab-other bg-main)

              ;; Diffs

              (bg-added ,(intensify 'green 60))
              (bg-added-refine ,(intensify 'green 40))
              (bg-changed ,(intensify 'blue 60))
              (bg-changed-refine ,(intensify 'blue 40))
              (bg-removed ,(intensify 'red 60))
              (bg-removed-refine ,(intensify 'red 40))

              (fg-added green)
              (fg-changed yellow)
              (fg-removed red)

              ;; Paren match
              (bg-paren-match bg-main)
              (fg-paren-match rosewater)
              (bg-paren-expression bg-yellow-nuanced)

              ;; General mappings

              (cursor rosewater)
              (name mauve)
              (identifier mauve)
              (fringe bg-main)

              (err red)
              (warning yellow)
              (info green)

              (bg-active bg-main)
              (bg-prominent-err bg-removed)
              (fg-prominent-err peach)

              ;; Code mappings

              (builtin red)
              (comment overlay0)
              (constant peach)
              (docstring overlay1)
              (fnname blue)
              (keyword mauve)
              (number peach)
              (property blue)
              (string green)
              (type yellow)
              (variable text)
              (parenthesis shadow)
              (shadow comment)

              ;; Accent mappings

              (accent-0 blue)
              (accent-1 pink)
              (accent-2 sky)
              (accent-3 red)

              ;; Completion mappings

              (bg-completion-match-0 surface0)
              (bg-completion-match-1 surface0)
              (bg-completion-match-2 surface0)
              (bg-completion-match-3 surface0)
              (fg-completion-match-0 sky)
              (fg-completion-match-1 pink)
              (fg-completion-match-2 green)
              (fg-completion-match-3 peach)

              ;; Date mappings

              ;; (date-weekday sky)
              ;; (date-weekend peach)

              ;; Line number mappings

              ;; TODO(olivia): Toggleable option
              (bg-line-number-active mantle)
              (bg-line-number-inactive mantle)
              (fg-line-number-active lavender)
              (fg-line-number-inactive surface1)

              ;; Link mappings

              (fg-link lavender)

              ;; Mark mappings

              (bg-mark-delete bg-removed)
              (fg-mark-delete red)
              (bg-mark-select bg-changed)
              (fg-mark-select sky)

              ;; Prompt mappings

              (bg-prompt unspecified)
              (fg-prompt subtext0)

              ;; Prose mappings

              (bg-prose-block-contents mantle)
              (bg-prose-block-delimiter bg-prose-block-contents)
              (fg-prose-block-delimiter surface0)
              (fg-prose-verbatim green)

              ;; Search mappings

              (bg-search-current red)
              (fg-search-current bg-main)
              (bg-search-lazy bg-region)
              (fg-search-lazy sapphire)
              (bg-search-static bg-region)
              (fg-search-static teal)

              ;; Heading mappings

              (fg-heading-0 red)
              (fg-heading-1 peach)
              (fg-heading-2 yellow)
              (fg-heading-3 green)
              (fg-heading-4 sapphire))

            ;; Generate a palette using the catppuccin colors. This takes care to generate all of the
            ;; variations of the colors that the modus themes are powered by.
            ;; TODO(olivia): Make the mapping user configurable, e.g. choosing between lavender and mauve
            ;; for the magenta base.
            (modus-themes-generate-palette
             `((bg-main ,(color 'base))
               (fg-main ,(color 'text))
               (magenta ,(color 'mauve))
               (red ,(color 'red))
               (yellow ,(color 'yellow))
               (green ,(color 'green))
               (cyan ,(color 'sky))
               (blue ,(color 'blue)))
             'cool)))
         ,(format "The entire palette of the `catppuccin-%s' theme.

This palette is based on `modus-themes-vivendi-palette' with the
Catppuccin %s colors taking precedence (palette lookup returns the first
match).

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE." flavor (capitalize flavor)))

       ;; Custom face overrides
       ;;
       ;; Faces whose styling cannot be expressed through palette mappings
       ;; alone (sizes, slants, or faces the Modus themes do not map).

       (defconst ,faces
         '(
           ;; completion
           `(modus-themes-completion-selected ((,c :background ,bg-completion :foreground ,sky)))
           ;; tab-bar
           ;; :box nil is load-bearing: the built-in `tab-bar-tab' defface sets a
           ;; `released-button' box on dark displays, and `tab-bar-tab-inactive'
           ;; inherits it.  Unlike the hacked-modus variant (which had modus-vivendi
           ;; enabled to supply a flat same-color box), this standalone theme drops
           ;; modus's own tab specs, so without :box nil the defface box leaks through
           ;; and every tab renders highlighted.
           `(tab-bar ((,c :background ,bg-main :foreground ,subtext0 :box nil)))
           `(tab-bar-tab ((,c :background ,crust :foreground ,lavender :underline nil :box nil)))
           `(tab-bar-tab-inactive ((,c :background ,bg-main :foreground ,subtext0 :box nil)))
           `(tab-bar-tab-group-current ((,c :background ,crust :foreground ,sky :box nil)))
           `(tab-bar-tab-group-inactive ((,c :background ,bg-main :foreground ,surface0 :box nil)))

           ;; git-gutter
           `(git-gutter:modified ((,c :foreground ,peach)))
           `(git-gutter:deleted ((,c :foreground ,red)))
           `(git-gutter:added ((,c :foreground ,green)))
           `(git-gutter:separator ((,c :foreground ,comment)))
           `(git-gutter:unchanged ((,c :foreground ,surface0)))

           ;; git-gutter fringe
           `(git-gutter-fr:modified ((,c :inherit git-gutter:modified)))
           `(git-gutter-fr:deleted ((,c :inherit git-gutter:deleted)))
           `(git-gutter-fr:added ((,c :inherit git-gutter:added)))

           ;; magit
           `(magit-branch-local ((,c :foreground ,teal)))
           `(magit-branch-remote ((,c :foreground ,green)))
           `(magit-tag ((,c :foreground ,peach)))
           `(magit-section-heading ((,c :foreground ,blue :weight bold)))
           `(magit-section-highlight ((,c :background ,surface0 :extend t)))
           `(magit-diff-context-highlight ((,c :background ,surface0 :foreground ,text :extend t)))
           `(magit-diff-revision-summary ((,c :foreground ,blue :weight bold)))
           `(magit-diff-revision-summary-highlight ((,c :foreground ,blue :weight bold)))
           `(magit-diff-added ((,c :foreground ,green :extend t)))
           `(magit-diff-added-highlight ((,c :background ,surface1 :foreground ,green :extend t)))
           `(magit-diff-removed ((,c :foreground ,red :extend t)))
           `(magit-diff-removed-highlight ((,c :background ,surface1 :foreground ,red :extend t)))
           `(magit-diff-file-heading ((,c :foreground ,text)))
           `(magit-diff-file-heading-highlight ((,c :inherit magit-section-highlight)))
           `(magit-diffstat-added ((,c :foreground ,green)))
           `(magit-diffstat-removed ((,c :foreground ,red)))
           `(magit-hash ((,c :foreground ,subtext0)))
           `(magit-diff-hunk-heading ((,c :inherit diff-hunk-header)))
           `(magit-diff-hunk-heading-highlight ((,c :inherit diff-hunk-header :weight bold)))
           `(magit-log-author ((,c :foreground ,subtext0)))
           `(magit-process-ng ((,c :foreground ,peach :weight bold)))
           `(magit-process-ok ((,c :foreground ,green :weight bold))))
         ,(format "Custom face overrides for the `catppuccin-%s' theme." flavor))

       (defvar ,custom nil
         ,(format "Custom variable overrides for the `catppuccin-%s' theme." flavor))

       ;; Instantiate the theme
       (modus-themes-theme
        ',id
        'catppuccin
        ""
        ',(if light-p 'light 'dark)
        ',palette
        ',palette-user
        ',palette-overrides
        ',faces
        ',custom)

       (provide-theme ',id))))


(provide 'catppuccin-lib)
;;; catppuccin-lib.el ends here
