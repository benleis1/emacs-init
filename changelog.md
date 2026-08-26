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
  
  
  
