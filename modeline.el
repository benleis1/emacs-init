;;; modeline.el --- A native, header-line-safe replacement for doom-modeline -*- lexical-binding: t; -*-

;;; Commentary:

;; A  small mode-line built directly out of stock  Emacs mode-line machinery
;; (`format-mode-line', `mode-line-modes', `vc-mode', nerd-icons).  Each segment is a plain function
;; re-evaluated on every redisplay via `:eval', so there's no advice and no
;; `enable-theme-functions' hook: theme-conditional rendering (the squared icon
;; badge under the nano-like theme) just checks the live theme state each time
;; it runs instead of being patched in and out when a theme (de)activates.
;;
;; The result, `my-modeline-format', is an ordinary mode-line-format spec, so
;; it works verbatim as either `mode-line-format' or `header-line-format':
;;
;;   (setq-default mode-line-format my-modeline-format)     ; the default
;;   (setq-default header-line-format my-modeline-format)   ; or here instead
;;
;; Mouse clicks need this too: Emacs's own mode-line constructs are usually
;; already dual-bound (`mode-line-buffer-identification-keymap' and
;; `mode-line-modes' both carry bindings under the `mode-line' AND
;; `header-line' event prefixes), but a few, like `vc-mode''s click-to-menu
;; binding, only bind `mode-line'.  `my-modeline--render-dual' mirrors
;; whatever it finds onto both prefixes, and custom segments in this file
;; build their keymaps dual-bound from the start -- so no segment cares which
;; line it ends up displayed in.

;;; Code:

(require 'nerd-icons nil t)

;; eglot's own menu keymaps, reused as-is for the rocket-icon segment's
;; clicks (see `my-modeline-segment-eglot'); declared here only to quiet
;; the byte-compiler, since eglot is loaded lazily and these aren't defined
;; until it is -- by which point `bound-and-true-p eglot--managed-mode'
;; being non-nil already guarantees they exist.
(defvar eglot-menu)
(defvar eglot-server-menu)

;;; Customization

(defcustom my-modeline-icon t
  "Whether to show a file/major-mode icon in the buffer-info segment."
  :type 'boolean
  :group 'my-modeline)

(defcustom my-modeline-position-format "L%l:%c"
  "`format-mode-line' spec used for the buffer position segment."
  :type 'string
  :group 'my-modeline)

(defvar my-modeline-icon-font-family
  (if (boundp 'my-default-fixed-pitch-font)
      my-default-fixed-pitch-font
    (and (boundp 'nerd-icons-font-family) nerd-icons-font-family))
  "Font family explicitly requested for every icon glyph this file draws,
via `my-modeline--icon'. nerd-icons' own icon functions default to
`nerd-icons-font-family' (\"Symbols Nerd Font Mono\"), but that exact font
may not be what's actually installed -- this config uses
`my-default-fixed-pitch-font' instead (see init.el), the same override
`my-imenu-list-icon-face' needs for the same reason. Forcing it explicitly
on every call, rather than relying on nerd-icons' default, means an icon
can't silently render as a tofu box just because the two names don't match.")

(defun my-modeline--icon (fn &rest args)
  "Call FN, a nerd-icons icon function (e.g. `nerd-icons-octicon'), with
ARGS, forcing `my-modeline-icon-font-family'. nerd-icons' generated icon
functions read the `nerd-icons-font-family' variable directly and have no
`:family' key in ARGS to override per call, so the only way to force a
specific family for just these calls -- without changing the family for
nerd-icons-dired and everything else that shares that global default --
is a dynamic let-binding around the call."
  (let ((nerd-icons-font-family (or my-modeline-icon-font-family nerd-icons-font-family)))
    (apply fn args)))

(defun my-modeline--icon-safe (fn &rest args)
  "Like `my-modeline--icon', but nil instead of an error when FN isn't a
bound function (e.g. its icon set's package isn't installed) or the call
itself errors (e.g. a renamed/missing glyph name) -- every icon-drawing
segment in this file guards its nerd-icons calls this way, so a missing
icon set can never break the mode-line."
  (and (fboundp fn) (ignore-errors (apply #'my-modeline--icon fn args))))

;;; Powerline-style file-type badge
;;
;; A flat-colored chip -- icon, then a short file-extension label, both on
;; `my-modeline-file-icon-face' -- capped by a solid divider glyph
;; (`nf-pl-left_hard_divider') that fades the chip's background back into
;; the plain mode-line, powerline-style. Text stays normal-sized throughout;
;; no enlarging, no line-height juggling.

(defface my-modeline-file-icon-face
  '((t :foreground "white" :background "gray60"))
  "Background face for the powerline-style file-type badge. Its
`:background' is kept in sync with the active modus theme's `cursor'
palette color by `my-modeline--sync-file-icon-face' whenever `modus-themes'
is available; the \"gray60\" here is only the fallback otherwise.")

(defface my-modeline-file-icon-divider-face
  '((t :foreground "gray60"))
  "Foreground-only face for the divider glyph capping off the
powerline-style file-type badge. Kept in sync with
`my-modeline-file-icon-face''s `:background' by
`my-modeline--sync-file-icon-face'. (A raw color plist can't be passed as
nerd-icons' `:face' argument: nerd-icons puts it straight into an
`:inherit' slot, which only accepts a face symbol -- hence syncing real
named faces instead of computing an ad hoc one per call.)")

(defun my-modeline--sync-file-icon-face ()
  "Refresh `my-modeline-file-icon-face' and `my-modeline-file-icon-divider-face'
from the active modus theme's `cursor' palette color, when `modus-themes'
is available. A no-op otherwise, leaving each face's own static
`:background'/`:foreground'. Called fresh on every render -- like every
other segment in this file -- instead of hooked to theme-change events, so
it can't go stale."
  (when (fboundp 'modus-themes-get-color-value)
    (let ((bg (modus-themes-get-color-value 'cursor))
	  (fg (modus-themes-get-color-value 'bg-main)))
      (when (stringp bg)
        (set-face-attribute 'my-modeline-file-icon-face nil :background bg :foreground fg)
        (set-face-attribute 'my-modeline-file-icon-divider-face nil :foreground bg)))))

(defvar my-modeline-wellknown-buffers '((lisp-interaction-mode . "ELISP"))
  "A map for translating well known buffers to a special name")

(defun my-modeline--file-type-label ()
  "A short, upcased label for the current buffer's file type: its file
extension when visiting a file, or the first few letters of `mode-name'
otherwise."
  (let* ((mode (or (cdr (assoc major-mode my-modeline-wellknown-buffers)) (format-mode-line mode-name)))
	 (shortmode (upcase (substring mode 0 (min 9 (length mode))))))

    (if buffer-file-name
	(let ((ext (file-name-extension buffer-file-name)))
          (or (and ext (upcase ext)) shortmode))
      shortmode)))

(defun my-modeline--file-type-icon ()
  "Plain file/mode icon glyph, recolored to `my-modeline-file-icon-face' to
match the badge it renders inside of."
  (if buffer-file-name
      (my-modeline--icon-safe #'nerd-icons-icon-for-file
                               (file-name-nondirectory buffer-file-name)
                               :face 'my-modeline-file-icon-face)
    (my-modeline--icon-safe #'nerd-icons-icon-for-mode major-mode
                             :face 'my-modeline-file-icon-face)))

(defun my-modeline--file-type-divider ()
  "A solid right-pointing divider glyph, on `my-modeline-file-icon-divider-face',
capping off the powerline-style file-type badge as it fades into the plain
mode-line."
  (my-modeline--icon-safe #'nerd-icons-powerline "nf-pl-left_hard_divider"
                           :face 'my-modeline-file-icon-divider-face))

(defun my-modeline--buffer-icon ()
  "Powerline-style file-type badge: an icon and a short, upcased,
bold file-extension label on `my-modeline-file-icon-face', ending in
`my-modeline--file-type-divider'. Carries the same help text and keymap as
the major mode name in `my-modeline-segment-modes', since the badge
represents the mode too."
  (when my-modeline-icon
    (ignore-errors
      (my-modeline--sync-file-icon-face)
      (let* ((prefix (concat " " (or (my-modeline--file-type-icon) "") " "))
             (label (my-modeline--file-type-label))
             (chip (propertize (concat prefix label " ")
                                'face 'my-modeline-file-icon-face
                                'help-echo (format "%s\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
                                                    (format-mode-line mode-name))
                                'local-map (my-modeline--dualize-keymap mode-line-major-mode-keymap))))
        (add-face-text-property (length prefix) (+ (length prefix) (length label))
                                 '(:weight bold) nil chip)
        (concat chip (or (my-modeline--file-type-divider) ""))))))

;;; Modified/read-only state icon

(defface my-modeline-buffer-modified-face
  '((t :inherit (warning bold)))
  "Face for the modified-buffer icon in `my-modeline--buffer-state-icon'.
Forked from doom-modeline's own `doom-modeline-buffer-modified'
(effectively `(warning bold)', its `doom-modeline' base face being an
empty placeholder), rather than depending on the doom-modeline package
for it.")

(defun my-modeline--buffer-state-icon ()
  "Small icon noting whether the buffer is read-only or modified. The
modified-buffer icon mirrors doom-modeline's own
`doom-modeline-update-buffer-file-state-icon' (`nf-md-content_save_edit')."
  (when my-modeline-icon
    (cond (buffer-read-only
           (my-modeline--icon-safe #'nerd-icons-octicon "nf-oct-lock" :face 'nerd-icons-red))
          ((and buffer-file-name (buffer-modified-p))
           (my-modeline--icon-safe #'nerd-icons-mdicon "nf-md-content_save_edit"
                                    :face 'my-modeline-buffer-modified-face)))))

;;; Mode-line / header-line dual-prefix mouse bindings

(defun my-modeline-mouse-map (bindings)
  "Return a keymap for BINDINGS, an alist of (MOUSE-EVENT . COMMAND), bound
under both the `mode-line' and `header-line' event prefixes, so it works
regardless of which one this modeline ends up displayed in."
  (let ((map (make-sparse-keymap)))
    (dolist (prefix '(mode-line header-line))
      (dolist (binding bindings)
        (define-key map (vector prefix (car binding)) (cdr binding))))
    map))

(defun my-modeline--dualize-keymap (map)
  "Return a copy of MAP with any `mode-line'/`header-line' prefixed submap
mirrored onto whichever of the two prefixes it's missing."
  (when (keymapp map)
    (let* ((new (copy-keymap map))
           (mode-sub (lookup-key map [mode-line]))
           (header-sub (lookup-key map [header-line])))
      (when (and (keymapp mode-sub) (not (keymapp (lookup-key new [header-line]))))
        (define-key new [header-line] mode-sub))
      (when (and (keymapp header-sub) (not (keymapp (lookup-key new [mode-line]))))
        (define-key new [mode-line] header-sub))
      new)))

(defun my-modeline--dualize-local-maps (string)
  "Copy STRING with every `local-map' text property dualized via
`my-modeline--dualize-keymap', so mouse bindings baked in by code that only
thought about the mode-line (e.g. `vc-mode') still work in the header-line."
  (let ((s (copy-sequence string))
        (pos 0)
        (len (length string)))
    (while (< pos len)
      (let* ((next (or (next-single-property-change pos 'local-map s) len))
             (map (get-text-property pos 'local-map s)))
        (when (keymapp map)
          (put-text-property pos next 'local-map (my-modeline--dualize-keymap map) s))
        (setq pos next)))
    s))

(defun my-modeline--render-dual (construct)
  "Render CONSTRUCT, a `mode-line-format' construct, via `format-mode-line',
then dualize its mouse bindings (see `my-modeline--dualize-local-maps')."
  (my-modeline--dualize-local-maps (format-mode-line construct)))

(defun my-modeline--first-property (string prop)
  "The first non-nil value of text property PROP found anywhere in STRING,
or nil if it has none. Useful for lifting a click binding off of a stock
construct (e.g. `vc-mode') whose properties start a character or two in,
rather than at position 0."
  (let ((pos (text-property-not-all 0 (length string) prop nil string)))
    (and pos (get-text-property pos prop string))))

;;; Dedicated windows
;;
;; Buffers that only ever show up in their own disposable popup window (e.g.
;; flymake's diagnostics list) are better served by mouse-1 in the mode-line
;; closing that window than by the usual `mouse-select-window' -- there's
;; nothing else worth switching to it for. Add a major mode here (or a
;; derived-mode-p-style symbol) to opt it into that behavior.

(defcustom my-modeline-dedicated-window-modes '(flymake-diagnostics-buffer-mode)
  "Major modes whose buffers get a click-to-close mode-line/header-line,
via `my-modeline--buffer-id', instead of the usual buffer-name bindings."
  :type '(repeat symbol)
  :group 'my-modeline)

(defun my-modeline--dedicated-window-p ()
  (apply #'derived-mode-p my-modeline-dedicated-window-modes))

(defun my-modeline-close-dedicated-window (event)
  "Close the dedicated window clicked on in its mode-line/header-line."
  (interactive "e")
  (quit-window nil (posn-window (event-start event))))

(defvar my-modeline-dedicated-window-map
  (my-modeline-mouse-map '((mouse-1 . my-modeline-close-dedicated-window))))

;;; Segments

(defun my-modeline--icon-badge ()
  "Icon/state badge for the current buffer: the powerline-style file-type
badge from `my-modeline--buffer-icon' plus a small modified/read-only
indicator. Shared by the full mode-line's buffer-info segment and the
dedicated-window reduced one."
  (concat (my-modeline--buffer-icon) " " (my-modeline--buffer-state-icon) " "))

(defun my-modeline--buffer-id ()
  "Buffer name, styled like the stock `mode-line-buffer-identification'
default, but rebuilt fresh on every call so the help text is accurate"
  (if (my-modeline--dedicated-window-p)
      (propertize "%12b"
                  'face 'mode-line-buffer-id
                  'help-echo "mouse-1: close the window"
                  'mouse-face 'mode-line-highlight
                  'local-map my-modeline-dedicated-window-map)
    (propertize "%12b"
                'face 'mode-line-buffer-id
                'help-echo
                '(format "%s\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                         (if buffer-file-name
                             (abbreviate-file-name buffer-file-name)
                           (buffer-name)))
                'mouse-face 'mode-line-highlight
                'local-map mode-line-buffer-identification-keymap)))

(defun my-modeline-segment-buffer-info ()
  "Buffer icon/state badge, followed by the buffer name. No extra pad on
the left -- the badge's own colored chip (see `my-modeline--buffer-icon')
already starts with a 1-space inset, and an unstyled space in front of it
would just show up as a mismatched sliver against the chip's background."
  (concat (my-modeline--icon-badge)
          (format-mode-line (my-modeline--buffer-id))))

(defun my-modeline-segment-remote-host ()
  "The remote host name, when the buffer is visiting a TRAMP file."
  (when-let* ((host (and default-directory (file-remote-p default-directory 'host))))
    (propertize (format " @%s " host) 'face 'mode-line-emphasis
                'help-echo "Remote host")))

(defun my-modeline--scroll-percent ()
  "Window scroll percentage: \"Top\"/\"All\" at the ends of the buffer, or
a number with a trailing `%' in between. Computed directly from
`window-start'/`window-end'/`point-min'/`point-max' with enough escaped % signs to
 work with multiple levels of evaluation."

  (let ((top (point-min))
        (bot (point-max))
        (start (window-start))
        (end (window-end nil t)))
    (cond ((and (<= start top) (>= end bot)) "All")
          ((<= start top) "Top")
          (t (format "%d%%%%%%%%" (round (* 100.0 (/ (float (- start top))
                                                     (max 1 (- bot top))))))))))

(defvar my-modeline-show-percent nil "Set to t to add the scroll percentage segment or nil to not")

(when (fboundp 'modus-themes-get-color-value)
  (defface my-modeline-position-face
    `((t :inherit mode-line
	 :weight semi-bold
	 :background ,(modus-themes-get-color-value 'bg-mode-line-emphasis t) ))

  "Face for `my-modeline-segment-position' in the selected window defined by the modus bg-mode-line-emphasis color. Only applied when the modeline is active."))

;;(set-face-attribute 'my-modeline-position-face nil :background "gray50")

(defface my-modeline-position-face-inactive
  '((t :inherit mode-line-inactive :weight semi-bold))
  "Face for `my-modeline-segment-position' in a non-selected window.
Unlike `my-modeline-position-face', `:background' is left unspecified so
the segment blends into the rest of the toned down modeline.")

(defun my-modeline--position-face ()
  "Face to use for `my-modeline-segment-position': `my-modeline-position-face'
when the window whose mode-line is being redisplayed is selected, or
`my-modeline-position-face-inactive' otherwise."
  (if (mode-line-window-selected-p)
      'my-modeline-position-face
    'my-modeline-position-face-inactive))

(defun my-modeline-segment-position ()
  "Line/column position, per `my-modeline-position-format', followed by
the window scroll percentage and, when `size-indication-mode' is on, a
buffer size indication -- all with the same right-click menu (toggle
line/column/size display) stock `mode-line-position' has, see
`mode-line-column-line-number-mode-map'. The whole segment is boxed via
`my-modeline--position-face' to look slightly depressed. Nested
`:propertize' forms don't inherit an enclosing one's `face' -- each covers
only the text it directly wraps -- so `face ,face' is listed explicitly in
every one of them below, including the plain leading/trailing spaces,
rather than relying on a single outer wrapper."
  (let ((face (my-modeline--position-face)))
    (my-modeline--render-dual
     `((:propertize " " face ,face)
       (:propertize ,my-modeline-position-format
                    face ,face
                    local-map ,mode-line-column-line-number-mode-map
                    mouse-face mode-line-highlight
                    help-echo "Line number and Column number\nmouse-1: Display Line and Column Mode Menu")

       (my-modeline-show-percent
        ((:propertize " " face ,face)
         (:propertize (:eval (my-modeline--scroll-percent))
                      face ,face
                      local-map ,mode-line-column-line-number-mode-map
                      mouse-face mode-line-highlight
                      help-echo "Window Scroll Percentage\nmouse-1: Display Line and Column Mode Menu")))
       (size-indication-mode
        (:propertize " of %I"
                     face ,face
                     local-map ,mode-line-column-line-number-mode-map
                     mouse-face mode-line-highlight
                     help-echo "Size indication mode\nmouse-1: Display Line and Column Mode Menu"))
       (:propertize " " face ,face)))))

(defun my-modeline-segment-selection-info ()
  "Size of the active region, when there is one."
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (lines (count-lines beg end))
           (chars (- end beg)))
      (propertize (if (> lines 1)
                      (format " %dL:%dC " lines chars)
                    (format " %dC " chars))
                  'face 'mode-line-emphasis))))

(defun my-modeline-segment-misc-info ()
  "Whatever third-party packages (flycheck, ...) publish via
`mode-line-misc-info', except eglot's own bracketed text indicator --
filtered out (non-destructively, via `remq'; no hook touches the shared
global list) since `my-modeline-segment-eglot' replaces it with a
colorized rocket icon instead."
  (my-modeline--render-dual
   (remq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info)))

(defun my-modeline-segment-eglot ()
  "A rocket icon standing in for eglot's own bracketed text mode-line
indicator (see `my-modeline-segment-misc-info'), colored by connection
health: `my-modeline-lsp-success' (green) once connected,
`my-modeline-lsp-warning' while requests are pending or no project
nickname is available yet, and `my-modeline-lsp-error' after a JSON-RPC
error -- mirroring doom-modeline's own eglot/lsp segment. Mouse-1 opens
eglot's menu, mouse-3 its server-control submenu, exactly as they do on
eglot's own indicator."
  (when (bound-and-true-p eglot--managed-mode)
    (let* ((server (eglot-current-server))
           (nick (and server (eglot-project-nickname server)))
           (pending (and server (jsonrpc-continuation-count server)))
           (last-error (and server (jsonrpc-last-error server)))
           (face (cond (last-error 'my-modeline-lsp-error)
                       ((and pending (> pending 0)) 'my-modeline-lsp-warning)
                       (nick 'my-modeline-lsp-success)
                       (t 'my-modeline-lsp-warning)))
           (icon (my-modeline--icon-safe #'nerd-icons-octicon "nf-oct-rocket" :face face)))
      (when icon
        (propertize icon
                    'help-echo (format "Eglot connected [%s]\nmouse-1: Display minor mode menu\nmouse-3: LSP server control menu"
                                        (or nick ""))
                    'mouse-face 'mode-line-highlight
                    'local-map (my-modeline-mouse-map
                                (list (cons 'mouse-1 eglot-menu)
                                      (cons 'mouse-3 eglot-server-menu))))))))

(defun my-modeline-segment-modes ()
  "Major mode name and `mode-line-process', deliberately without the
`minor-mode-alist' text stock `mode-line-modes' also shows: mouse-3 on the
major mode name already pops up the minor-mode toggle menu (see
`mode-line-major-mode-keymap'), so there's no need to list them inline too."
  (my-modeline--render-dual
   `((:propertize ("" mode-name)
                  help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
                  mouse-face mode-line-highlight
                  local-map ,mode-line-major-mode-keymap)
     ("" mode-line-process))))

(defun my-modeline--vcs-state ()
  (and buffer-file-name
       (let ((backend (vc-backend buffer-file-name)))
         (and backend (vc-state buffer-file-name backend)))))

;;; VCS state faces
;;
;; Loosely forked from doom-modeline's own `doom-modeline-info'/`-warning'/
;; `-urgent'/`-vcs-default' faces -- not referenced or inherited from them,
;; just started from the same idea -- so this file has no dependency on
;; the doom-modeline package at all, even implicitly via face names a
;; theme happens to style. doom-modeline's own default (no theme override)
;; is effectively green/yellow/red via `success'/`warning'/`error', so
;; that's the static fallback here too. This config's modus-derived themes
;; (folio, nano-like, ...) don't color plain `success' at all, though --
;; `my-modeline--refresh-vcs-faces' instead pulls a real green/the
;; `warning'/`modeline-err' palette slots into these forked faces, so a
;; clean repository still reads as green under them.

(defface my-modeline-vcs-info
  '((t :inherit success :foreground "ForestGreen"))
  "Face for an up-to-date or otherwise neutral/in-progress VCS state (e.g.
edited, added, a merge in progress) -- green, like doom-modeline's
`doom-modeline-info' renders by default.")

(defface my-modeline-vcs-warning
  '((t :inherit warning))
  "Face for a VCS state that will need attention soon, e.g. an incoming
pull. Forked from doom-modeline's `doom-modeline-warning'.")

(defface my-modeline-vcs-urgent
  '((t :inherit error))
  "Face for a VCS state that needs attention now, e.g. a conflict. Forked
from doom-modeline's `doom-modeline-urgent'.")

(defface my-modeline-vcs-default
  '((t :inherit (my-modeline-vcs-info bold)))
  "Default face for a VCS state not otherwise called out above. Forked
from doom-modeline's `doom-modeline-vcs-default'.")

;; The eglot rocket-icon segment (`my-modeline-segment-eglot') below reuses
;; this same green/warning/urgent palette, just under its own face names --
;; `my-modeline-lsp-success' etc., mirroring doom-modeline's own naming for
;; its lsp/eglot segment -- so both segments read consistently and neither
;; needs its own palette-refresh logic.
(defface my-modeline-lsp-success
  '((t :inherit my-modeline-vcs-info))
  "Face for a healthy eglot/LSP connection. Forked from doom-modeline's
`doom-modeline-lsp-success'.")

(defface my-modeline-lsp-warning
  '((t :inherit my-modeline-vcs-warning))
  "Face for an eglot/LSP connection with requests pending, or none
established yet. Forked from doom-modeline's `doom-modeline-lsp-warning'.")

(defface my-modeline-lsp-error
  '((t :inherit my-modeline-vcs-urgent))
  "Face for an eglot/LSP connection that hit a JSON-RPC error. Forked from
doom-modeline's `doom-modeline-lsp-error'.")

(defun my-modeline--refresh-vcs-faces (&rest _)
  "Pull `my-modeline-vcs-info'/`-warning'/`-urgent''s colors from the
active modus-themes palette's `green'/`warning'/`modeline-err' slots --
green being the closest equivalent to doom-modeline's own default
green-via-`success' look, since this config's modus-derived themes don't
color plain `success' at all -- so a clean repository still reads as
green under them. A no-op when modus-themes isn't loaded, or the current
theme isn't one of its derivatives."
  (when (fboundp 'modus-themes-get-color-value)
    (dolist (spec '((my-modeline-vcs-info . green)
                    (my-modeline-vcs-warning . warning)
                    (my-modeline-vcs-urgent . modeline-err)))
      (let ((color (modus-themes-get-color-value (cdr spec) t)))
        (unless (eq color 'unspecified)
          (set-face-attribute (car spec) nil :foreground color))))))

(add-hook 'enable-theme-functions #'my-modeline--refresh-vcs-faces)
(my-modeline--refresh-vcs-faces)

(defun my-modeline--vcs-face (state)
  "Face for STATE, using the forked faces above: `my-modeline-vcs-default'
for a neutral/in-progress state, bold `my-modeline-vcs-warning' for a
pending pull, and bold `my-modeline-vcs-urgent' for something that needs
attention (removed, conflict, unregistered)."
  (cond ((eq state 'needs-update) '(my-modeline-vcs-warning bold))
        ((memq state '(removed conflict unregistered)) '(my-modeline-vcs-urgent bold))
        (t 'my-modeline-vcs-default)))

(defun my-modeline--vcs-icon (state face)
  "Icon for STATE, colored with FACE: a compare/merge/pull-request/branch
glyph depending on STATE, mirroring doom-modeline's vcs segment (an alert
triangle instead, for a state that needs attention)."
  (if (memq state '(removed conflict unregistered))
      (my-modeline--icon-safe #'nerd-icons-octicon "nf-oct-alert" :face face)
    (my-modeline--icon-safe
     #'nerd-icons-devicon
     (cond ((eq state 'needs-update) "nf-dev-git_pull_request")
           ((eq state 'needs-merge) "nf-dev-git_merge")
           ((memq state '(edited added)) "nf-dev-git_compare")
           (t "nf-dev-git_branch"))
     :face face)))

(defun my-modeline--vcs-branch-name ()
  "Just the branch/state part of `vc-mode', stripped of its \"Backend:\"
prefix (e.g. \"Git:main\" -> \"main\") -- the icon already conveys which
backend it is, so showing it twice is redundant."
  (and vc-mode (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+"))))

(defvar my-modeline-vcs-collapsed nil
  "When non-nil, `my-modeline-segment-vcs' shows only its icon, with the
branch name omitted from the text (it's still available in the segment's
help-echo tooltip). Toggled by mouse-3 on the segment, via
`my-modeline-vcs-toggle-collapsed'.")

(defun my-modeline-vcs-toggle-collapsed ()
  "Toggle whether `my-modeline-segment-vcs' shows the branch name inline."
  (interactive)
  (setq my-modeline-vcs-collapsed (not my-modeline-vcs-collapsed))
  (force-mode-line-update t))

(defvar my-modeline-vcs-map
  (my-modeline-mouse-map '((mouse-3 . my-modeline-vcs-toggle-collapsed)))
  "Keymap composed on top of vc-mode's own click binding in
`my-modeline-segment-vcs' (see `make-composed-keymap' there), adding
mouse-3 as `my-modeline-vcs-toggle-collapsed' without disturbing whatever
vc-mode itself already bound (typically mouse-1).")

(defun my-modeline-segment-vcs ()
  "Version-control branch, when the buffer is under version control.
Icon and coloring mirror doom-modeline's vcs segment: a git
compare/merge/pull-request/branch glyph depending on `vc-state', with the
icon and branch name both colored by that same state -- not just the text
-- and the whole segment clickable via `vc-mode''s own binding, mirrored
onto the header-line too (see `my-modeline--dualize-keymap'). Mouse-3
toggles `my-modeline-vcs-collapsed', collapsing the segment down to just
the icon; the branch name stays in the help-echo tooltip either way, so
it's still reachable by hovering while collapsed."
  (when vc-mode
    (let* ((state (my-modeline--vcs-state))
           (face (my-modeline--vcs-face state))
           (icon (my-modeline--vcs-icon state face))
           (name (my-modeline--vcs-branch-name))
           (vc-map (my-modeline--dualize-keymap
                    (my-modeline--first-property vc-mode 'local-map)))
           (map (if vc-map (make-composed-keymap my-modeline-vcs-map vc-map)
                  my-modeline-vcs-map))
           (help (format "Branch: %s\n%s\nmouse-1: Version Control menu\nmouse-3: toggle collapsed view"
                         name
                         (nth 0 (vc-mode-line-state state)))))
      (propertize (concat icon
                          (and (not my-modeline-vcs-collapsed) name (concat " " name)))
                  'face face
                  'mouse-face 'mode-line-highlight
                  'help-echo help
                  'local-map map))))

;;; Right alignment
;;
;; Stock Emacs 30 already has `mode-line-format-right-align' for exactly
;; this, and its pixel math is subtle enough (frame vs. window, fringes,
;; scroll bars, dividers) that it isn't worth re-deriving from scratch --
;; get it wrong and content quietly falls short of, or overshoots, the
;; real edge. The only problem is that its docstring is explicit that it
;; only works when embedded directly in the variable `mode-line-format':
;; it locates itself there via `memq' on that one hardcoded variable name
;; to measure what follows it, which is silently wrong here, since this
;; file's whole point is a format that works the same way installed as
;; either `mode-line-format' or `header-line-format'.
;;
;; `my-modeline--right-align' reuses the real implementation function,
;; `mode--line-format-right-align', instead of a rewrite: it dynamically
;; binds `mode-line-format' -- a `defvar'-declared, and therefore always
;; dynamically (not lexically) scoped, special variable -- to a throwaway
;; list with TRAILING right after the marker, for just the duration of
;; that one call. The `memq' lookup inside then finds exactly what this
;; function handed it, however this format is actually installed.

(defun my-modeline--right-align (trailing)
  "Return a padding string that right-aligns TRAILING (already-rendered
mode-line content) against the edge of the window, via stock Emacs's own
`mode--line-format-right-align'."
  (let ((mode-line-format (list "" 'mode-line-format-right-align trailing)))
    (mode--line-format-right-align)))

(defvar my-modeline-right-margin "  "
  "Fixed padding kept between the rightmost content and the true edge of
the window, in both `my-modeline--render' and `my-modeline--dedicated-render'.
Without it, the last character renders flush against the very edge, which
can look clipped even when it technically isn't.")

;;; Dedicated windows' reduced mode-line
;;
;; Modeled on `imenu-list-mode-line-format' in init.el, which does the same
;; thing by hand for its own popup buffers: a minimal mode-line with just a
;; pin icon -- marking the window as dedicated/disposable -- and the buffer
;; name. No file-type/read-only icon and no position/selection/VC/misc-info
;; clutter either; there's nothing else worth showing in a window this size.

(defun my-modeline--pin-icon ()
  (my-modeline--icon-safe #'nerd-icons-mdicon "nf-md-pin" :face 'mode-line-emphasis))

(defun my-modeline--dedicated-render ()
  "The pin icon and buffer name, both sharing the same click-to-close
binding and help text (see `my-modeline-dedicated-window-map') -- so
mousing over or clicking either half of this reduced mode-line behaves
the same standard way, rather than only the buffer name being clickable."
  (propertize (concat (or (my-modeline--pin-icon) "") " "
                       (format-mode-line (my-modeline--buffer-id)))
              'help-echo "mouse-1: close the window"
              'mouse-face 'mode-line-highlight
              'local-map my-modeline-dedicated-window-map))

;;; Assembly

(defvar my-modeline-left-segments
  '(my-modeline-segment-buffer-info
    my-modeline-segment-remote-host
    my-modeline-segment-selection-info)
  "Segment functions rendered left-to-right on the left side of
`my-modeline-format'.")

(defvar my-modeline-right-segments
  '(my-modeline-segment-misc-info
    my-modeline-segment-eglot
    my-eglot-flymake-segment
    my-modeline-segment-position
    my-modeline-segment-vcs
    )
  "Segment functions rendered left-to-right immediately after the left-side
segments in `my-modeline-format', each separated by a space (see
`my-modeline--eval-segments''s SEPARATOR argument) so e.g. the flymake
counter never ends up glued directly to the VCS branch name. See
`my-modeline-anchored-right-segments' for segments that should instead
hug the window's right edge.")

(defvar my-modeline-anchored-right-segments
  '(tab2-view-segment my-modeline-segment-zoom)
  "Segment functions right-justified flush against the window's right
edge -- separated from `my-modeline-right-segments' by however much space
remains, and from each other by a couple of spaces, rather than run
together with everything else. E.g. the current tab2 view, or the
rightmost zoom in/out icon.")

(defun my-modeline--eval-segments (segments &optional separator)
  "Concatenate the non-nil results of calling each of SEGMENTS, joined by
SEPARATOR (default: none)."
  (let ((results (delq nil (mapcar #'funcall segments))))
    (if separator (mapconcat #'identity results separator) (apply #'concat results))))

(defvar my-modeline-right-gap " "
  "Fixed spacing kept between `my-modeline-right-segments' and
`my-modeline-anchored-right-segments' in `my-modeline--render', on top of
whatever space `my-modeline--right-align' leaves -- so e.g. the major
mode name and the tab2 view never end up touching just because the rest
of the line happened to fill the window exactly.")

(defcustom my-modeline-box-line-width 4
  "`:line-width' used by `my-modeline--sync-frame-box' to pad out the
whole mode-line. The box is always drawn in its own background color, so
this only ever adds height/seamless padding, never a visible border."
  :type 'integer
  :group 'my-modeline)

(defun my-modeline--sync-frame-box ()
  "Give `mode-line' (and `mode-line-active', where it exists) and
`mode-line-inactive' a box `my-modeline-box-line-width' wide, each in its
own `:background' -- so the box is invisible as a border and just adds
padding/height, seamlessly, to both the active and inactive mode-line.
Recomputed on every render, like every other theme-dependent bit in this
file (see the file commentary), so a theme switch can't leave it stale."
  (dolist (face (if (facep 'mode-line-active)
                     '(mode-line mode-line-active mode-line-inactive)
                   '(mode-line mode-line-inactive)))
    (set-face-attribute face nil :box
                         (list :line-width my-modeline-box-line-width
                               :color (face-attribute face :background nil t)))))

(defun my-modeline--render ()
  "The full mode-line, or, in a dedicated window (see
`my-modeline-dedicated-window-modes'), the reduced one from
`my-modeline--dedicated-render'.

The right side has two zones: `my-modeline-right-segments' is positioned
(via `my-modeline--right-align') to end exactly `my-modeline-right-gap'
before `my-modeline-anchored-right-segments' begins, and that second zone
gets its own, independent `my-modeline--right-align' call measuring only
itself, so it always lands flush against the window edge regardless of
how wide the first zone renders -- the git branch and tab2 view, say,
don't just run together with everything else, or with the rest of the line.
`my-modeline-right-margin' keeps the very last character from rendering
flush against the true edge."
  (my-modeline--sync-frame-box)
  (if (my-modeline--dedicated-window-p)
      (my-modeline--dedicated-render)
    (let* ((left (my-modeline--eval-segments my-modeline-left-segments))
           (general (my-modeline--eval-segments my-modeline-right-segments " "))
           (anchored (my-modeline--eval-segments my-modeline-anchored-right-segments "  ")))
      (concat left
              (my-modeline--right-align
               (concat general my-modeline-right-gap anchored my-modeline-right-margin))
              general
              my-modeline-right-gap
              (my-modeline--right-align (concat anchored my-modeline-right-margin))
              anchored
              my-modeline-right-margin))))

(defvar my-modeline-format
  '("%e" (:eval (my-modeline--render)))
  "Native replacement for doom-modeline's mode-line spec. Assign this to
`mode-line-format' (see `my-modeline-mode') or `header-line-format' -- both
work the same way.")

(defvar my-modeline--default-format (default-value 'mode-line-format)
  "The stock `mode-line-format' saved before `my-modeline-mode' overrides it.")

(define-minor-mode my-modeline-mode
  "Install `my-modeline-format' as the mode-line, in place of the default."
  :group 'my-modeline
  :global t
  (setq-default mode-line-format
                (if my-modeline-mode my-modeline-format my-modeline--default-format)))


;; The tab2 view segment. See tab-config.el
(defvar tab2-mode-line-view-map
  (my-modeline-mouse-map '((mouse-1 . tab2-next-view)
                           (mouse-3 . tab2-prev-view))))

(defun tab2-view-segment ()
  "Show the current tab2 view, marked with a desktop icon."
  (when (> (tab2-num-views) 1)
     (let ((icon (my-modeline--icon-safe #'nerd-icons-mdicon "nf-md-desktop_classic"
					:face 'mode-line-emphasis)))
      (propertize (concat (or icon "") " " (tab2-view-name (tab2-get-current-view)))
                  'help-echo "Current tab view - click to switch to the next one"
                  'mouse-face 'mode-line-highlight
                  'local-map tab2-mode-line-view-map))))

(defvar my-flymake-modeline-map
  (my-modeline-mouse-map '((mouse-1 . flymake-show-buffer-diagnostics))))

(defun my-eglot-flymake-segment ()
  "Flymake's error/warning/note counter, but only when eglot manages the buffer.
Elsewhere (e.g. elisp buffers using plain flymake) stays silent.
Click (mouse-1) anywhere in the segment to pop up flymake's diagnostics
list for the buffer, even when it's currently empty."
  (when (bound-and-true-p eglot--managed-mode)
    (let ((counters (format-mode-line flymake-mode-line-counters)))
      (propertize (if (string-empty-p counters) "  OK  " counters)
                  'local-map my-flymake-modeline-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: show flymake diagnostics"))))

;;; Zoom slider

(defun my-modeline-zoom-in ()
  "Increase the buffer's text scale (see `text-scale-adjust')."
  (interactive)
  (text-scale-adjust 1))

(defun my-modeline-zoom-out ()
  "Decrease the buffer's text scale (see `text-scale-adjust')."
  (interactive)
  (text-scale-adjust -1))

(defvar-local my-modeline-zoom-slider-active nil
  "Non-nil when `my-modeline-segment-zoom' should show the draggable
slider (`my-modeline--zoom-bar') instead of its default magnifying-glass
icon, in this buffer. Toggled by `my-modeline-zoom-show-slider' (mouse-1
on the icon) and `my-modeline-zoom-show-icon' (mouse-3 on the slider).")

(defun my-modeline-zoom-show-slider ()
  "Switch `my-modeline-segment-zoom' to its draggable-slider display.
Bound to `mouse-1' on the magnifying-glass icon via
`my-modeline-zoom-icon-map'."
  (interactive)
  (setq my-modeline-zoom-slider-active t)
  (force-mode-line-update))

(defun my-modeline-zoom-show-icon ()
  "Switch `my-modeline-segment-zoom' back to its magnifying-glass-icon
display. Bound to `mouse-3' on the slider via `my-modeline-zoom-map'."
  (interactive)
  (setq my-modeline-zoom-slider-active nil)
  (force-mode-line-update))

;;
;; A draggable slider in the same spirit as `mlscroll''s mode-line scrollbar:
;; the track and thumb are drawn with plain propertized space characters
;; carrying a `(space :width (N))' `display' spec (see `my-modeline--zoom-bar'),
;; not an image It replaces the magnifying-glass icon on `mouse-1' (see
;; `my-modeline-zoom-show-slider') and hands display back to that icon on
;; `mouse-3' (see `my-modeline-zoom-show-icon').

(defconst my-modeline-zoom-min -6
  "Lower bound of the zoom slider in `my-modeline-segment-zoom', in
`text-scale-mode-amount' units. Purely a UI limit on the slider's own
range -- `text-scale-mode-amount' itself has no hard bound in stock
Emacs, so a value set by other means (e.g. `text-scale-increase' well
past this) still displays, just clamped to the near end of the track
\(see `my-modeline--zoom-frac'), and dragging back onto the track from
there re-takes control normally.")

(defconst my-modeline-zoom-max 5
  "Upper bound of the zoom slider in `my-modeline-segment-zoom'. See
`my-modeline-zoom-min'.")

(defconst my-modeline-zoom-track-width 50
  "Pixel width of the zoom slider's track in `my-modeline-segment-zoom',
excluding the thumb.")

(defconst my-modeline-zoom-thumb-width 6
  "Pixel width of the zoom slider's thumb in `my-modeline-segment-zoom'.")

(defface my-modeline-zoom-track-face
  '((t :inherit my-modeline-position-face))
  "Background face for the left/right track portions of the zoom slider
in `my-modeline-segment-zoom'. Inheriting `my-modeline-position-face'
gives the track the same \"emphasis\" background `my-modeline-segment-position'
already uses elsewhere in this mode-line, so it stands out against the
thumb (see `my-modeline-zoom-thumb-face'). Its `:box' color is
overridden explicitly by `my-modeline--sync-zoom-faces' to the plain,
regular mode-line background, so the padding above/below reads as
ordinary box padding rather than a border in this face's own, different,
background.")

(defface my-modeline-zoom-thumb-face
  '((t :inherit mode-line))
  "Background face for the zoom slider's thumb/handle in
`my-modeline-segment-zoom'. Its `:background' is overridden explicitly
by `my-modeline--sync-zoom-faces' to the mode-line's own current
*foreground* (text) color, so the thumb reads as a solid block in the
same ink the rest of the mode-line's text is drawn in, against the
emphasized track either side of it (see `my-modeline-zoom-track-face').
Its `:box' color is likewise forced to the plain, regular mode-line
background, consistent with the track's box; the inherited `mode-line'
here is only the fallback before that first sync runs.")

(defun my-modeline--sync-zoom-faces ()
  "Set `my-modeline-zoom-thumb-face''s `:background' to the mode-line's
own current foreground (text) color, and give both
`my-modeline-zoom-track-face'/`-thumb-face' a `:box' whose color is the
plain, regular mode-line background -- not their own, different, fill
colors -- so the padding above/below the slider reads as the same
seamless box `my-modeline--sync-frame-box' gives every other segment,
rather than a colored border matching whichever segment's own
background happens to be showing. Called fresh on every render, like
`my-modeline--sync-frame-box' itself, so a theme switch can't leave it
stale."
  (let ((bg (face-attribute 'mode-line :background nil t))
        (fg (face-attribute 'mode-line :foreground nil t)))
    ;; Clear any stale explicit `:background' left over on the track face by
    ;; an earlier version of this function (`set-face-attribute' sticks to
    ;; the live face across `eval-buffer', redefining the `defface' above
    ;; does not reset it) -- so it reliably falls through to its inherited
    ;; `my-modeline-position-face' background instead.
    (set-face-attribute 'my-modeline-zoom-track-face nil :background 'unspecified)
    (when (stringp fg)
      (set-face-attribute 'my-modeline-zoom-thumb-face nil :background fg))
    (when (stringp bg)
      (dolist (face '(my-modeline-zoom-track-face my-modeline-zoom-thumb-face))
        (set-face-attribute face nil :box
                             (list :line-width my-modeline-box-line-width :color bg))))))

(defun my-modeline--zoom-frac (amount)
  "Fraction (0.0-1.0) along the zoom slider's track for AMOUNT, a
`text-scale-mode-amount' value, clamped to `my-modeline-zoom-min'/`-max'."
  (/ (float (- (max my-modeline-zoom-min (min my-modeline-zoom-max amount))
               my-modeline-zoom-min))
     (float (- my-modeline-zoom-max my-modeline-zoom-min))))

(defun my-modeline--zoom-amount-at-x (x)
  "Map X, a pixel offset into the slider's track span (0 at its left
edge, `my-modeline-zoom-track-width' at its right), to the
`text-scale-mode-amount' integer it represents, clamped to
`my-modeline-zoom-min'/`-max'."
  (let ((frac (max 0.0 (min 1.0 (/ (float x) my-modeline-zoom-track-width)))))
    (round (+ my-modeline-zoom-min (* frac (- my-modeline-zoom-max my-modeline-zoom-min))))))

(defun my-modeline--zoom-absolute-x (posn)
  "Absolute pixel x, relative to the whole zoom bar's left edge, for
mouse position POSN landing on one of `my-modeline--zoom-bar''s three
segments, or nil if POSN isn't over one of them. Each segment carries
its own left-edge offset as the `my-modeline-zoom-origin' text property
\(set when the bar was drawn), since `posn-object-x-y' reports pixel
coordinates relative to whichever one of the three separately-propertized
segments was actually clicked, not the bar as a whole -- mirroring how
`mlscroll-mouse' adds back its own clicked segment's preceding width via
`mlscroll-find-index' before calling `mlscroll-scroll-to'."
  (let* ((str-pos (posn-string posn))
         (origin (and str-pos
                      (get-text-property (cdr str-pos) 'my-modeline-zoom-origin (car str-pos))))
         (local-x (car (posn-object-x-y posn))))
    (and origin local-x (+ origin local-x))))

(defun my-modeline-zoom-mouse (start-event)
  "Click-to-set, then live-drag the zoom slider in `my-modeline-segment-zoom'
to the buffer's `text-scale-mode-amount'. Bound to `down-mouse-1' on the
slider's track/thumb via `my-modeline-zoom-map'. Mirrors `mlscroll-mouse'
almost exactly: jump to the clicked position immediately, then track
further mouse movement with `track-mouse' -- updating live as the pointer
moves, not only on release -- until a non-movement event (the button
release) ends the drag."
  (interactive "e")
  (let* ((start-posn (event-start start-event))
         (win (posn-window start-posn))
         (x (my-modeline--zoom-absolute-x start-posn))
         (xstart-abs (car (posn-x-y start-posn)))
         xnew event)
    (when (and (window-live-p win) x xstart-abs)
      (with-selected-window win (text-scale-set (my-modeline--zoom-amount-at-x x)))
      (force-mode-line-update)
      (let ((mouse-fine-grained-tracking t))
        (track-mouse
          (setq track-mouse 'dragging)
          (while (and (setq event (read-event)) (mouse-movement-p event))
            (let ((end (event-end event)))
              (when (memq (posn-area end) '(mode-line header-line))
                (setq xnew (+ x (- (car (posn-x-y end)) xstart-abs)))
                (with-selected-window win (text-scale-set (my-modeline--zoom-amount-at-x xnew)))
                (force-mode-line-update)))))))))

(defvar my-modeline-zoom-map
  (my-modeline-mouse-map
   (list (cons 'down-mouse-1 #'my-modeline-zoom-mouse)
         (cons 'mouse-3 #'my-modeline-zoom-show-icon)
         (cons 'wheel-up #'my-modeline-zoom-in)
         (cons 'wheel-down #'my-modeline-zoom-out)))
  "Keymap for the draggable track/thumb portion of
`my-modeline-segment-zoom'. `down-mouse-1' is `my-modeline-zoom-mouse'
\(click-to-set, then drag); `mouse-3' hands display back to the
magnifying-glass icon (`my-modeline-zoom-show-icon'); wheel up/down step
zoom in/out one `text-scale-mode-step' at a time, mirroring
`mlscroll-mouse''s own wheel bindings.")

(defvar my-modeline-zoom-icon-map
  (my-modeline-mouse-map
   (list (cons 'mouse-1 #'my-modeline-zoom-show-slider)
         (cons 'wheel-up #'my-modeline-zoom-in)
         (cons 'wheel-down #'my-modeline-zoom-out)))
  "Keymap for the magnifying-glass icon in `my-modeline-segment-zoom'.
`mouse-1' switches display to the draggable slider
\(`my-modeline-zoom-show-slider'); wheel up/down step zoom in/out, same
as `my-modeline-zoom-map'.")

(defun my-modeline--zoom-bar ()
  "The draggable track+thumb portion of `my-modeline-segment-zoom': three
adjacent stretchable space characters (before-thumb, thumb, after-thumb),
sized by the buffer's current zoom via `my-modeline--zoom-frac' and
colored via `my-modeline-zoom-track-face'/`-thumb-face' -- the same
`display' `(space :width (N))' trick `mlscroll-mode-line' uses for its
own scrollbar, so the bar needs no image/SVG support to draw or resize.
Each segment's own left-edge pixel offset is recorded as its
`my-modeline-zoom-origin' text property, for `my-modeline--zoom-absolute-x'
to recover on click."
  (my-modeline--sync-zoom-faces)
  (let* ((frac (my-modeline--zoom-frac (if (bound-and-true-p text-scale-mode)
                                            text-scale-mode-amount
                                          0)))
         (before (round (* my-modeline-zoom-track-width frac)))
         (after (- my-modeline-zoom-track-width before))
         (bar (concat
               (propertize " " 'face 'my-modeline-zoom-track-face
                           'display `(space :width (,before))
                           'my-modeline-zoom-origin 0)
               (propertize " " 'face 'my-modeline-zoom-thumb-face
                           'display `(space :width (,my-modeline-zoom-thumb-width))
                           'my-modeline-zoom-origin before)
               (propertize " " 'face 'my-modeline-zoom-track-face
                           'display `(space :width (,after))
                           'my-modeline-zoom-origin (+ before my-modeline-zoom-thumb-width)))))
    (propertize bar 'local-map my-modeline-zoom-map)))

(defconst my-modeline-zoom-amount-width 2
  "Fixed character width reserved for the digits/sign of the zoom amount
text in `my-modeline-segment-zoom' (blank-padded on the left when the
number is narrower, e.g. \" 3\" vs \"-12\"), so the slider itself doesn't
shift left/right as the amount's digit count changes going from
positive to negative or between one and two digits.")

(defun my-modeline--zoom-icon ()
  "The magnifying-glass icon portion of `my-modeline-segment-zoom',
shown instead of the draggable slider (`my-modeline--zoom-bar') until
`mouse-1' switches to it (see `my-modeline-zoom-show-slider')."
  (or (my-modeline--icon-safe #'nerd-icons-faicon "nf-fa-magnifying_glass"
                               :face 'mode-line-emphasis)
      "?"))

(defun my-modeline-segment-zoom ()
  "The magnifying-glass icon (`my-modeline--zoom-icon') by default, or --
after a `mouse-1' click on it -- the draggable slider
\(`my-modeline--zoom-bar') for jumping straight to a zoom level (see
`my-modeline-zoom-slider-active'), followed either way by the buffer's
current zoom amount (see `text-scale-mode-amount') in a fixed-width
field (see `my-modeline-zoom-amount-width'), blank when at its default
of zero."
  (let ((amount (string-pad (if (bound-and-true-p text-scale-mode)
                                 (number-to-string text-scale-mode-amount)
                               "")
                             my-modeline-zoom-amount-width ?\s t))
        (control (if my-modeline-zoom-slider-active
                      (my-modeline--zoom-bar)
                    (propertize (my-modeline--zoom-icon)
                                'help-echo "mouse-1: show zoom slider"
                                'mouse-face 'mode-line-highlight
                                'local-map my-modeline-zoom-icon-map))))
    (concat control amount)))

(provide 'modeline)

;; Experimental use of header for the modeline
(defun my-modeline-switch-to-header ()
  (set-face-attribute 'header-line nil :background "gray90")
  (setq-default header-line-format mode-line-format)
  (setq-default mode-line-format nil)
  (set-face-attribute 'header-line nil :box
                    (list :line-width 8
                          :color "gray90")))

;;; modeline.el ends here
