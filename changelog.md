# Purpose
This will track major shifts I'm making in the config files for later reference

# 8/2026

* Remove coupling to doom themes and doom modeline in favor of modus built in
  themes and a simplified local modeline implementation (modeline.el) The latter
  was done because I've been recently adding more actively tweaking this
  behavior and its easier to deal with baseline code.

* A reworking of the modeline in general. Biggest change is around the
mode glyph which I'm leveraging on the far left

* Move over to Eglot from lsp-mode

* Breakout imenu code into imenu.el since I also have invested a lot of focus
    on customizing here between custom indexing for the java-ts-mode and the
    styling overrides.

* More markdown quality of life additions for use with jekyll around
      wikimode, tag completion and making links just work.

* Tweak ispell a bit including moving over to aspell, turning on some coding
  side rules for camel case etc and unbdining M-TAB.  I want that for general
  completion and I do spelling fixups via the context menu because that is an
  engrained pattern.

* install.sh script to make things easier on new setups.

# 9/2026

* Switch to Elpaca to save startup time. Added an elpaca.lock for consistency (another ~0.3s off the TUI start).
  Also general perf work: gc tuning in early-init, more deferred package loads,
  and a move to Emacs 30.1.

* Java debugging via DAPE, split out into dape.el (plus dape-java.el with
  diagnostics and tests). Treesit grammars for Java now auto-install, and jdtls
  expands file-URI style jars so xref lookups work.

* A minimal ERT suite with a pre-commit hook, since expanded to cover startup
  time and sanity checks for the modeline and tab config.

* imenu-list rework: hierarchical elisp sorting, diff-hl based VC highlighting
  and my own fold-state handling (the built-in approach broke on Emacs 31).

* org-timegrid added as a strip in the agenda. Appointment notifications play
  sounds once, and meetings-log.org is tracked for time clocking.

* Configuration hygiene: local.el for config that isn't shareable, a
  my-environment custom group instead of hardcoded paths, font heights
  separated from custom.el, and lock files/autosave redirected under .saves.

# 10/2026

* Platform split: macos.el broken out of init.el and a linux.el with basic
  Ubuntu + niri compatibility. Emacs 31+ only functions (like
  mouse-shift-adjust-mode) are now guarded.

* Dark theme work: modus-vivendi adjustments, plus the embers and matrix
  variants, and dark versions of the close/funnel icons. Title bar removed.

* Built in profiling support
* new install.sh script and my-diagnose-config helper function to check for dependencies
* Added pulsar to show where jumps are occurring.
* Pixel scrolling: smooth page up/down so tall images in markdown don't make
  the view jump. Only enabled in graphical frames, and ghostel buffers keep
  PgUp/PgDn for the terminal.
* ghostel integration. Hooked into tabs and modeline with some focus on surfacing busy/needs notification sessions.
 * tab-config now persists views. It also has support for tear off tabs including drag and drop.
 * elisp runs flymake by default.
* Project bound terminals: ghostel buffers now belong to a project and show up
  in that project's tab group. Pinned projects are generic (tab2-pinned-project)
  and ghostel name transforms are pluggable rather than baked into tab-config.
* The desktop auto-saves hourly once it has been loaded, so views and terminals
  survive a crash.
* The flymake modeline segment is now universal and shows an indicator while
  flymake is running. Flymake itself is working properly now with elisp.
* Elisp byte-compile hygiene: compile-declarations.el predeclares variables and
  functions to quiet warnings, with ERTs covering compile warnings and
  top-level unbound variable detection. Flymake for elisp is hooked in
  permanently (see docs/flymake.md).
* trusted-content setup (including symlinked paths) so flymake and elisp
  checking can run without prompting on files I own.
* menu-config.el: menu bar reorganization adapted from anju. Help is replaced
  wholesale with a short menu (manual opens in a new frame), consult/ghostel
  replacements fall back to stock entries when those packages are absent, Edit gains
  Rectangle/Transpose/Delete submenus plus Flush/Keep Lines, Align Regexp and
  Duplicate, and the Text menu gets Transform Text/Center/Fill submenus.
  Menus are set up after elpaca loads all packages. See docs/menu-config.md.
* Right-click context menus: dictionary/thesaurus entries in text modes and a
  ghostel hyperlink that copies to the kill ring for pasting later.

# 11/2026

* tab-config renamed to tab-plus and cut over to tab-plus-mode, with function
  names cleaned up to separate internal from external. Closing a tab prefers
  staying in the current project group, and desktop restore handles frames
  that are out of range. ghostel tab names survive a desktop restore.

* ilist-plus replaces my imenu-list customizations (cutover plus a matching
  modeline segment).

* Transparency: alpha blending is now a global defcustom with a
  my-alpha-blend toggle, glyphs are blended explicitly, the cursor is a box
  everywhere and the window divider is wider so it's easier to drag. An
  experimental Emacs patch for a gradient divider on macOS lives in patches/.

* More context menus: diff-hl menus on the fringe and in the line-number
  margin (with dape/diff-hl ownership of the fringe sorted out), and word
  count in the Edit menu. See docs/context-menus.md.

* Trust: prompt before opening untrusted files.

* speedbar adopted, with nerd-font icons and helpers for decoding file icons.
  See docs/speedbar.md.

* Smaller items: windmove moved to C-S, profiling code moved from init.el to
  early-init.el,
