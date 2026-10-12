;;; modus-vivendi-embers-theme.el --- Modus Vivendi derivative dominated by orange and red -*- lexical-binding:t -*-

;;; Commentary:

;; A dark theme built on top of `modus-themes' that pushes nearly every
;; accent color -- syntax highlighting, headings, mode-line, region,
;; links, search -- into the orange/red part of the spectrum.  A couple
;; of cool colors (`info', a subset of the diff faces via the shared
;; `modus-themes-common-palette-overrides') are deliberately left alone
;; so functional distinctions (info vs. warning/error, added vs. removed)
;; remain legible.
;;
;; The theme's own warm-toned identity lives in `modus-vivendi-embers-palette',
;; a full palette derived from `modus-themes-vivendi-palette' with its
;; warm entries overridden in place.  `modus-vivendi-embers-palette-overrides'
;; is a genuine, empty-by-default user-override slot -- setq'ing it (e.g.
;; from init.el) layers on top of the theme instead of replacing it.
;; Customize that variable to tweak further, the same way you would with
;; any other modus theme.

;;; Code:

(unless (and (fboundp 'require-theme)
             load-file-name
             (equal (file-name-directory load-file-name)
                    (expand-file-name "themes/" data-directory))
             (require-theme 'modus-themes t))
  (require 'modus-themes))

(defcustom modus-vivendi-embers-palette-user nil
  "Like `modus-vivendi-embers-palette-overrides', for user-defined entries.
This is meant to extend the palette with custom named colors and/or
semantic palette mappings."
  :group 'modus-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Option to extend the palette for use with overrides"))

(defconst modus-vivendi-embers-palette
  (append
   '(;; Basic values: a warm, near-black background with a cream foreground.
    (bg-main     "#120c0a")
    (bg-dim      "#221410")
    (fg-main     "#f5e8dc")
    (fg-dim      "#a8938a")
    (fg-alt      "#e0a877")
    (bg-active   "#4a2c1f")
    (bg-inactive "#2b1913")
    (border      "#6b3d28")

    (red         "#ff0000")
    (red-warmer  "#ff2b00")
    (red-cooler  "#ff002b")
    (red-faint   "#f15a5a")
    (red-intense "#ff1a1a")

    ;; A couple of extra named colors to round out the orange/red range
    ;; beyond what the base palette already provides (red, red-warmer,
    ;; red-cooler, yellow-warmer, rust, gold, maroon).
					;    (ember "#ff6a1f")
    (ember "#ee7600")
    (blaze "#ffab40")

    ;; General mappings
    (cursor  blaze)
    (keybind red-warmer)
    (name    gold)
    (identifier yellow-warmer)

    (bg-active-argument bg-red-nuanced)
    (fg-active-argument red-warmer)
    (bg-active-value    bg-yellow-nuanced)
    (fg-active-value    yellow-warmer)

    ;; Code mappings
    (builtin      red-cooler)
    (comment      fg-dim)
    (constant     maroon)
    (docstring    yellow-faint)
    (fnname       ember)
    (fnname-call  blaze)
    (keyword      red-warmer)
    (preprocessor rust)
    (property     gold)
    (rx-backslash red)
    (rx-construct yellow-warmer)
    (string       blaze)
    (type         rust)
    (variable     yellow-cooler)
    (variable-use gold)

    ;; Accent + completion-match mappings (line numbers, corfu/vertico, etc.)
    (accent-0 red-warmer)
    (accent-1 ember)
    (accent-2 yellow-warmer)
    (accent-3 rust)
    (fg-completion-match-0 red-warmer)
    (fg-completion-match-1 ember)
    (fg-completion-match-2 yellow-warmer)
    (fg-completion-match-3 rust)

    ;; Mode line
    (bg-mode-line-active       "#5a2a18")
    (fg-mode-line-active       fg-main)
    (border-mode-line-active   "#8a4a2a")
    (bg-mode-line-inactive     "#2b1913")
    (fg-mode-line-inactive     fg-dim)
    (border-mode-line-inactive "#4a2c1f")

    ;; Region, current line, completion popup, paren match
    (bg-region      "#6b2f1a")
    (fg-region      fg-main)
    (bg-hl-line     "#2a1712")
    (bg-completion  "#4a2418")
    (bg-paren-match "#a8461f")
    (fg-paren-match fg-main)

    ;; Search
    (bg-search-current bg-red-intense)
    (bg-search-lazy    bg-yellow-intense)
    (bg-search-replace bg-red-subtle)

    ;; Headings: bright orange at the top, deepening toward red/maroon.
    (fg-heading-0 blaze)
    (fg-heading-1 ember)
    (fg-heading-2 rust)
    (fg-heading-3 gold)
    (fg-heading-4 yellow-warmer)
    (fg-heading-5 red-cooler)
    (fg-heading-6 maroon)
    (fg-heading-7 red-warmer)
    (fg-heading-8 fg-dim)

    ;; Links
    (fg-link                 yellow-warmer)
    (underline-link          yellow-warmer)
    (fg-link-symbolic        gold)
    (underline-link-symbolic gold)
    (fg-link-visited         maroon)
    (underline-link-visited  maroon)

    ;; Prompts and prose
    (fg-prompt        red-warmer)
    (fg-prose-code    yellow-warmer)
    (fg-prose-macro   red-cooler)
    (fg-prose-verbatim ember)

    ;; Rainbow delimiters / outline levels -- all warm, at the cost of
    ;; some nesting-level distinguishability, which is the point of
    ;; this theme.
    (rainbow-0 fg-main)
    (rainbow-1 red-warmer)
    (rainbow-2 yellow-warmer)
    (rainbow-3 ember)
    (rainbow-4 rust)
    (rainbow-5 red-cooler)
    (rainbow-6 gold)
    (rainbow-7 maroon)
    (rainbow-8 blaze))
   modus-themes-vivendi-palette)
  "The `modus-vivendi-embers' palette: `modus-themes-vivendi-palette'
with its warm-toned entries overridden in place, biasing the theme
toward orange and red accents.  This is the theme's own identity, not
a user-facing customization point -- do not `setq' or `add-to-list'
this.  To customize the theme, use `modus-vivendi-embers-palette-overrides'
or `modus-vivendi-embers-palette-user' instead.")

(defcustom modus-vivendi-embers-palette-overrides nil
  "Overrides for `modus-vivendi-embers-palette'.
This is the customization point for this theme: mirror the elements of
`modus-vivendi-embers-palette' (or the base modus palette) to override
their value, the same way you would with e.g. `modus-vivendi-palette-overrides'."
  :group 'modus-themes
  :type '(repeat (list symbol (choice symbol string))))

(modus-themes-theme
 'modus-vivendi-embers
 'modus-themes-embers
 "A `modus-vivendi' derivative dominated by warm orange and red accents."
 'dark
 'modus-vivendi-embers-palette
 'modus-vivendi-embers-palette-user
 'modus-vivendi-embers-palette-overrides)

(provide-theme 'modus-vivendi-embers)

;;; modus-vivendi-embers-theme.el ends here
