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
