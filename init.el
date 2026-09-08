;; -*- lexical-binding: t; -*-
;; ```
;;  oooooooooooo
;;  `888'     `8
;;   888         ooo. .oo.  .oo.    .oooo.    .ooooo.   .oooo.o
;;   888oooo8    `888P"Y88bP"Y88b  `P  )88b  d88' `"Y8 d88(  "8
;;   888    "     888   888   888   .oP"888  888       `"Y88b.
;;   888       o  888   888   888  d8(  888  888   .o8 o.  )88b
;;  o888ooooood8 o888o o888o o888o `Y888""8o `Y8bod8P' 8""888P'
;; ```
;;
;; Author: Benjamin Leis

;;; Commentary:
;;
;; ## Philosophy
;;
;; My emacs configuration is an opinionated (some might say highly opinionated)
;; setup that I have made public to share pieces with others.  These are the
;; high level priorities that inform the decisions I've made throughout this
;; file.  First, unlike many other users who have shared their config files, I
;; like using the mouse and even the occasional menu rather than remembering key
;; bindings for everything. So I've spent some time trying to get emacs to work
;; more consistently for these modes. For example with flyspell on you can right
;; click and get a context menu with the possible spellings like in most other
;; applications.
;;
;; If possible I'll use built in functionality or packages that require minimal
;; adaptation and just a use-package declaration. If not I try to keep
;; everything in one section by general functionality area. Along these lines
;; currently I prefer one larger file to a series of smaller ones for both
;; reading and modifying. This may shift in the future but currently I only move
;; code out to a new file if it reaches a "sufficiently" large size.
;;
;; Two areas in particular are key to many of my workflows. First, is imenu and
;; imenu-list which I use in multiple modes to see document structure and leave
;; open on the right side at almost all times. I've invested a fair amount of
;; configuration tailoring this via custom indexing, styling key mode maps,
;; sorting and modeline changes. Second, is the completion framework which I
;; have increasingly found to be fundamental. I use the modern completion stack
;; of vertico, orderless marginalia and consult. And where needed I have added
;; additional completion at point routines like the ones I added for markdown
;; tags.
;;
;; I have a work style where I want to have a manageable small set of files open
;; in a tabbed format.  I'll save these to a desktop and reload them when I
;; start things up again. I've plumbed save/load desktop into the system menus
;; and also extensively modified tab-line to fit my work flow.  Longterm if the
;; need arises I plan to either integrate in bookmark+ or activities to save
;; related sets of these files. For now I have customized tab-line with "views"
;; to facilitate this. See tab-config.el for more details.
;;
;; Style-wise, I prefer a fairly minimal design theme. I'm currently using the
;; folio theme which is based on the builtin modus-themes and have changed most
;; faces to just use the same default foreground color or a bolder one for
;; emphasis. I really only want color in critical locations.  Likewise, I
;; currently have a very minimal custom mode line that only features segments I
;; actually use and leverages the hover help text to convey extra information
;; like a full buffer file path.
;;
;; Sample screen:
;; ![sample screen](./sample-screen.png)
;;
;; Normally I run a gui standalone emacs as well as an emacs server for terminal mode editing
;; My typical alias setup
;;  ```
;;  # launcher for terminal emacs
;;  alias emacs='emacsclient -t -s default --alternate-editor=`
;;
;;  # launcher for gui emacs
;;  function gemacs() {
;;     /opt/homebrew/bin/emacs $* &
;;  }
;; ```
;;
;; ## Portability
;;   I have the config on github both for my own backup and as a
;;   way to share snippets and ideas.  I've worked to make this mostly reusable
;;   where reasonable but there is still some coupling to my own environment and
;;   workflow discussed below.
;;
;; ## Prerequisites
;;   Things you'll want in place before this config will load and work cleanly:
;;   - MacOS. There's direct use of pbcopy and other OS specific integration.
;;   - Emacs 30 or later (31 preferred; some of the :vc package handling is
;;     conditioned on the major version).
;;   - git on PATH, since several packages are pulled straight from source via
;;     use-package's :vc keyword rather than from MELPA.
;;   - A Nerd Font installed (I use DejaVu Sans Mono Nerd Font) for the
;;     mode-line and dired icons to render correctly.
;;   - aspell installed (falls back to ispell if not found) for flyspell.
;;   - A Java installation reachable via `my-java-home' (defaults to a jenv
;;     path) plus jdtls on PATH if you want eglot's Java support.
;;   - pgformatter on PATH if you want the SQL formatting commands to work.
;;
;; ## Major areas configured
;; - Markdown
;; - Org
;; - Java
;; - Ediff
;; - Python (partly)
;; - SQL

;;; Code:

;;; Package setup

;; Define an ignore macro that doesn't even evaluate the argument. This is useful for
;; display purposes when using elispdoc rather than commenting whole regions out.
(defmacro my-ignore (_form))

;; Setup melpa as a repository.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See
;; `package-archive-priorities` and `package-pinned-packages`. Most users will
;; not need or want to do this.
(my-ignore (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))

;; use-package has been part of core emacs since version 29 so I assume its OK
;; to just require it.
(require 'use-package)

;; Legacy Emacs 29 setup for :vc so we can load directly from github for
;; selected packages not in melpa.  Note: long term move to :fetcher :repo
;; syntax
(when (< emacs-major-version 30)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

;; Enable automatic package installation globally
(setq use-package-always-ensure t)

;; early on setup follow-symlinks to true for loaded files
(setq vc-follow-symlinks t)

;; GUI Emacs on macOS is launched by launchd, not a login shell, so it only
;; gets a minimal PATH/exec-path -- Homebrew-installed tools like aspell,
;; jdtls and pgformatter aren't visible to `executable-find' without this.
;; But exec-path-from-shell is relatively expensive so as compromise
;; just add homebrew onto the path as needed

(unless (member "/opt/homebrew/bin" exec-path)
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;;; Customizations

;;
;; Color name redirection for use with custom faces
;; requires manual editing of custom-set-faces or modus definitions
;; to  use i.e with the  ` back tick operator.
;;

(defvar margin-tan-bg "#EEE8D5")
(defvar margin-gray-bg "gray20")

;;; Font setup
;; This needs to be done prior to theme setup.

;; Mixed-pitch mode. I use this in markdown and org modes currently.
(defvar my-default-fixed-pitch-font "DejaVuSansM Nerd Font"
  "Default fixed-pitch font family.")
(defvar my-default-variable-pitch-font "Helvetica"
    "Default variable-pitch font family.")

(use-package mixed-pitch
  :ensure t
  :init
  (set-face-attribute 'variable-pitch nil
                      :font my-default-variable-pitch-font
                      :height 1.0)

  ;; Ensure the fixed-pitch face strictly inherits from the default font
  (set-face-attribute 'fixed-pitch nil
                      :inherit 'default)

  ;; Leave cursor-type alone; don't let mixed-pitch swap it to a bar cursor.
  (setq mixed-pitch-variable-pitch-cursor nil)

  :hook ((org-mode . mixed-pitch-mode)
	 (markdown-mode . mixed-pitch-mode)))

;;; modus theme configuration.

;; Disable the theme safety check.
(setq custom-safe-themes t)

;; Disable all previously loaded themes before loading another one.
(advice-add 'load-theme :before
            (lambda (&rest _varargs)
              (mapc #'disable-theme custom-enabled-themes)))

;; These mostly global level changes make switching around easier between themes
;; They preserve the tabbing styling I use and mute the colors a bit.
;; The consequence of moving over to modus is the need to not generally customize faces in
;; custom.el.

;; Override all modus themes to use the background color from tab-line
;; This keeps visual parity with what I currently use
;; Make headers all the same color as foreground

(setq modus-themes-common-palette-overrides
      `((bg-margins ,margin-tan-bg)  ;; common setup for a color alias to override.
	(bg-tab-bar bg-margins)
        (bg-tab-current bg-main)
        (bg-tab-other bg-margins)
	(bg-line-number-inactive bg-margins)

	;; custom hl face for imenu-list
	(fg-hl-imenu  "DarkOrange2")

	;; Tone down the headings: use the default foreground instead
        ;; of the theme's per-level accent colors.
        (fg-heading-0 fg-main)
        (fg-heading-1 fg-main)
        (fg-heading-2 fg-main)
        (fg-heading-3 fg-main)
        (fg-heading-4 fg-main)
        (fg-heading-5 fg-main)
        (fg-heading-6 fg-main)
        (fg-heading-7 fg-main)
        (fg-heading-8 fg-main)

	;; tone down code blocks
	(bg-prose-block-contents unspecified)
	(bg-prose-code unspecified)
        (bg-prose-block-delimiter unspecified)
        (fg-prose-block-delimiter fg-dim)

	;;diffs - duplicate modus deuteranopia colors (I don't like red/green)
	(bg-added             bg-yellow-subtle)
        (bg-added-faint       bg-yellow-faint)
        (bg-added-refine      bg-yellow-refine)
        (bg-added-intense     bg-yellow-intense)
        (fg-added             yellow)
        (fg-added-intense     yellow-intense)
        (bg-removed           bg-blue-subtle)
        (bg-removed-faint     bg-blue-faint)
        (bg-removed-refine    bg-blue-refine)
        (bg-removed-intense   bg-blue-intense)
        (fg-removed           blue)
        (fg-removed-intense   blue-intense)
	))

;; enable fixed fonts for code and variable for text
(setq modus-themes-mixed-fonts t)

;; bold works well for coding faces
(setq modus-themes-bold-constructs t)

;; Tone down the cursor specifically for modus-operandi-tinted.
(setq modus-operandi-tinted-palette-overrides
      '((cursor "gray60")
	(comment fg-main)
	(keyword yellow-intense)
	(bg-completion bg-yellow-nuanced)
	))

;; Specific folio theme overrides
(setq folio-theme-palette-overrides
      ;; headers need some color and overlines stripped
      `(
	(bg-margins ,margin-tan-bg)
	(bg-heading-2 unspecified)
	(overline-heading-1 unspecified)
        (overline-heading-2 unspecified)
	(overline-heading-3 unspecified)
        (overline-heading-4 unspecified)
	(bg-prose-block-contents bg-tab-bar)

	;; I like a slightly bolder mode-line background
	(bg-mode-line-active "gray75")
	(bg-mode-line-emphasis "gray85")

	;; paren-matching
	(fg-paren-match red)
	(bg-paren-match bg-red-intense)

	;; Current set of coding keyword color selections.
	(keyword green-warmer)
	(type unspecified)
	(fnname yellow-cooler)
	(fnname-call unspecified)
	))

(setq modus-vivendi-palette-overrides
      `((bg-margins ,margin-gray-bg)
;;	(fg-dim blue)
	(bg-mode-line-emphasis "gray50")
	(fg-hl-imenu magenta-cooler)
	))

;; Give nano-like its own fixed/variable-pitch fonts (relies on
;; `modus-themes-mixed-fonts', set above, actually using `fixed-pitch' and
;; `variable-pitch'). Revert to the unspecified/default family otherwise.

(defvar my-nano-fixed-pitch-font "Roboto Mono for Powerline"
  "Fixed-pitch font family used while the nano-like theme is active.")
(defvar my-nano-variable-pitch-font "Fira Code"
  "Variable-pitch font family used while the nano-like theme is active.")

;; Loop through all the buffers and force mixed-pitch-mode ones to reload.
(defun my-reload-fonts ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p mixed-pitch-mode)
	(mixed-pitch-mode)))))

;; Add the font changes onto the enable theme hook
(add-hook 'enable-theme-functions
          (lambda (theme)
            (if (eq theme 'nano-like-modus)
                (progn
		  (set-face-attribute 'default nil :family my-nano-fixed-pitch-font)
                  (set-face-attribute 'fixed-pitch nil :family my-nano-fixed-pitch-font)
                  (set-face-attribute 'variable-pitch nil :family my-nano-variable-pitch-font)
		  (my-reload-fonts))
              (progn
		(set-face-attribute 'default nil :family my-default-fixed-pitch-font)
                (set-face-attribute 'fixed-pitch nil :family my-default-fixed-pitch-font)
                (set-face-attribute 'variable-pitch nil :family my-default-variable-pitch-font)
		(my-reload-fonts)
		))

	    ;; I'd like to have select on the modeline change the foreground rather than
	    ;; than the background and that needs a custom hook.
	    (when (eq theme 'folio)
	      (set-face-attribute 'mode-line-highlight nil :foreground "DarkOrange4"
				  :background 'unspecified))

	    ;; I have my own handling for tab line modified outside of modus
	    (if (facep 'tab-line-tab-modified)
		(set-face-attribute 'tab-line-tab-modified nil :foreground 'unspecified))

            ;; Additional color overrides modus doesn't control by default
	    (if (facep 'tab-line-tab-inactive)
		(set-face-attribute 'tab-line-tab-inactive nil :foreground
				    (modus-themes-get-color-value 'fg-dim t)))

	    (if (facep 'my-hl-imenu-face)
		(set-face-attribute 'my-hl-imenu-face nil :foreground
				    (modus-themes-get-color-value 'fg-hl-imenu t)))

	    (if (facep 'my-modeline-position-face)
		(set-face-attribute 'my-modeline-position-face nil :background
				    (modus-themes-get-color-value 'bg-mode-line-emphasis t)))))


;; Modus doesn't handle fonts so just set this directly here where all other styling is
;; being done. I like using a 1.3 scaled version of the system UI font for the tabs.
(if (< emacs-major-version 31)
  (custom-set-faces
   '(tab-line ((t :family ".AppleSystemUIFont" :height 1.3))))

  (custom-set-faces
   '(tab-line-active ((t :family ".AppleSystemUIFont" :height 1.3))))

  (custom-set-faces
   '(tab-line-inactive ((t :family ".AppleSystemUIFont" :height 1.3)))))

;; Currently trying out the folio theme as my main theme.
(use-package folio-theme
  :vc (:url "https://github.com/kn66/folio-theme.el"
            :rev :newest)
  :config
  (load-theme 'folio t))

(use-package nano-like-modus-theme
  :vc (:url "https://github.com/benleis1/nano-like-modus-theme")
  :ensure t)

;; Deal with dark/light mode macos ui elements like the scrollbar
(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode 1))

;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html
;; All customizations are stored on the side in custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Basic Appearance and startup
;; A set of configurations related to display style that are not covered by themes or faces.

;; Make things silently start.
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-buffer-choice nil
      initial-scratch-message "")

;; Initial major mode is text for new buffers
(setq-default major-mode 'text-mode)
;; break paragraphs on after 80 characters.
(setq-default fill-column 80)

;; turn off menu mode in text mode to save space
(unless window-system
  (menu-bar-mode 0))

;; turn off tool bar always
(tool-bar-mode 0)

;; turn on scroll bars when in window mode
;; testing on-demand scroll bar
;; Use the normal right click brings up a context menu
;; Add a faint window divider
(when window-system
  (use-package on-demand-scroll-bar
    :vc (:url "https://github.com/florommel/on-demand-scroll-bar.git")

    :config
    (on-demand-scroll-bar-mode 1))

  (context-menu-mode)

  ;; Add dividers on the right and bottom
  (setq window-divider-default-places t)
  ;; Set the width of the dividers to 1 pixel
  (setq window-divider-default-right-width 1
	window-divider-default-bottom-width 1)
  (window-divider-mode 1)
  )

(my-ignore (setq scroll-conservatively 10))
;; don't allow overscrolling.


(when window-system
  (setq use-system-tooltips nil))

;; Use winner mode by default for managing window configurations
;; particularly useful when popping up a 2nd or 3rd window and
;; wanting to go back to the previous config.
(winner-mode 1)

;; Generally remove trailing white space except on markdown where trailing space is meaningful
(defun my-before-save-hook ()
  (unless (equal major-mode 'markdown-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'my-before-save-hook)

;; Auto complete on tab if not at start of line in modes
;; where tab auto indents
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Override the default value so isearch is always full screen. If set too low emacs
;; tries to render less
(setq baud-rate 19200)

;; Use short y or no prompts.
(setopt use-short-answers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Enable mouse in text mode
;; Note: this removes iterm2 cut and paste integration so we add advice later on to call pbcopy
;; after copying to the kill ring
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (_e))) ;;check if stubbing this out is still needed in v31

;; Setup recent files mode - this is much more in use now that I have consult
;; `recentf-auto-cleanup' defaults to `mode', which runs a synchronous
;; `recentf-cleanup' pass (stat-checking every entry in the saved list)
;; the moment `recentf-mode' turns on -- measured ~49ms total, of which
;; ~23ms is the cleanup itself (the rest is just loading the saved list).
;; Deferring cleanup to 30s idle keeps it fully automatic but takes it
;; off the startup critical path.
(setq recentf-auto-cleanup 30)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 50)

;; Every 10 minutes up date the list since I usually either run the server or keep
;; the gui app open for long periods of time
(run-at-time nil 600 'recentf-save-list)

;; Switch focus to help windows when they come up
(setq help-window-select t)

;;; backup and autosave.
;; put everything in .saves under .emacs.d

;; Define a directory for auto-save files
(defconst my-auto-save-folder (locate-user-emacs-file ".saves"))

;; Ensure the directory exists
(unless (file-exists-p my-auto-save-folder)
  (make-directory my-auto-save-folder t))

(setq
 auto-save-file-name-transforms `((".*" , my-auto-save-folder t))
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 `(("." . ,my-auto-save-folder))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 lockfile-name-transforms `((".*" ,my-auto-save-folder t))
 )

;; alternative strategy - just turn off auto-save.
(my-ignore (setq auto-save-default nil))

;;; Dired

;; nerd icons setup.
;; These are used by my-modeline (see modeline.el) and nerd-icons-dired; color
;; adjustments need to be done prior to loading either.
(use-package nerd-icons
  :config
  ;; set the nerd icon color for lisp mode prior to starting up. Yellow doesn't read well.
  (add-to-list 'nerd-icons-mode-icon-alist
               '(lisp-interaction-mode nerd-icons-sucicon "nf-custom-emacs" :face nerd-icons-green)))

;; Icons for dired. I'm not sure if I care enough to keep this longterm yet.
(use-package nerd-icons-dired
  :defer t
  :if window-system
  :ensure t
  :hook ((dired-mode . nerd-icons-dired-mode))
  )


;; Do all dired ops in a single window
(setq dired-kill-when-opening-new-dired-buffer t)
;; allow find-alternate-file i.e. open and kill dired
(put 'dired-find-alternate-file 'disabled nil)

;;; modeline
;; Native mode-line/header-line implementation, replacing doom-modeline (see
;; modeline.el for the full rationale).
;; Note: its important to have a nerd font installed for the icons to work properly
;; I use DejaVu Sans Mono with the Nerd Font extension. For now I leave the icons on
;; even in terminal mode although they are a bit too small there.
(load (locate-user-emacs-file "modeline.el"))
(my-modeline-mode 1)

;; Load all of my custom tab-line config.
(load  (locate-user-emacs-file "tab-config.el"))

;;; Global key bindings
;; After some analysis I have altered a couple of top level chords below.
;; my preference is for short key strokes and to usually bind other global things to
;; Control + a number key which are closer than the function keys.

(global-set-key (kbd "C-u") 'undo) ;; I use undo all the time
(global-set-key (kbd "C-+") 'universal-argument) ;; I never use universal-argument.
(global-set-key (kbd "C-f") 'goto-line) ;; Another swap. I use arrow keys for basic movement.
(global-set-key (kbd "C-1") 'treemacs)
(global-set-key (kbd "C-2") 'org-capture)
(global-set-key (kbd "C-3") 'wikimode-toggle)
(global-set-key (kbd "C-\\") 'imenu-list-smart-toggle)
(global-set-key (kbd "C-<tab>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-S-<tab>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "<pinch>") 'ignore) ;; this also causes chaos for me so disable.

;; I hit cmd-x too often expecting M-x which is dangerous so just bind it to that
;; TODO should I just bind cmd - to the meta key and give up up cmd-c and cmd-v?
(global-set-key (kbd "s-x") 'execute-extended-command)

;; Copy to clipboard functions for terminal mode
;; copy the current region directly
(defun pbcopy-region ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

;; copy the latest kill ring
(defun pbcopy-kill-ring (&optional _xpush)
  (interactive)
  (let ((process-connection-type nil)
	(text (current-kill 0)))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Final version hook into interprogram-cut-function instead
;; for terminal mode cut to system clipboard
(defun paste-for-osx (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(unless window-system
  (setq interprogram-cut-function 'paste-for-osx))

;;; flyspell config
;; currently not bound to a key

;; Set the ispell program name to aspell if available which generally offers better
;; performance than ispell.
(setq ispell-program-name (or (executable-find "aspell")
			      (executable-find "ispell")))

;; Set the global default dictionary for the Ispell process.
(setq ispell-dictionary "en_US")

;; Reduce unnecessary messages when checking individual words.
(setq ispell-quietly t)

;; Configure Aspell's suggestion mode to "ultra", which favors very close
;; spelling and phonetic matches when generating suggestions.
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Free up M-TAB/C-M-i (normally `flyspell-auto-correct-word') so it falls
;; through to the default `completion-at-point' binding instead. I do corrections
;; typically via right-click context menus.
(setq flyspell-use-meta-tab nil)

(defun my-flyspell-prog-mode (&rest _args)
  "Enable `flyspell-prog-mode' with buffer-local Aspell arguments."
  ;; The --run-together flag instructs Aspell to accept words formed by
  ;; combining two or more valid dictionary words without spaces, treating the
  ;; resulting string as valid.
  ;;
  ;; This is excellent for source code. Code is heavily populated with
  ;; compound variable names and technical terms (e.g., filepath, buffername,
  ;; checkbox).
  ;; URL: https://www.jamescherti.com/emacs-spell-checker-flyspell-ispell-aspell/
  (make-local-variable 'ispell-extra-args)
  (dolist (item '("--run-together"
                  ;; "--ignore=2"
                  ;; "--run-together-min=3"
                  ;; "--run-together-limit=4"
                  ;; "--camel-case"
                  ))
    (add-to-list 'ispell-extra-args item))
  (flyspell-prog-mode))

(defun flyspell-on-for-buffer-type ()
      "Enable Flyspell appropriately for the major mode of the current buffer.
Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only
strings and comments get checked.  All other buffers get `flyspell-mode'
to check all text.  If flyspell is already enabled, does nothing."
      (interactive)
      (if (not (symbol-value flyspell-mode)) ; if not already on
        (progn
          (if (derived-mode-p 'prog-mode)
              (my-flyspell-prog-mode)
            ;; else
            (flyspell-mode 1))
          )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it
uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
    ;; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;; preset modes to have flyspell on. Deferred to the next idle moment rather
;; than enabled directly in the hook
(defun my-flyspell-defer-enable ()
  "Defer `flyspell-on-for-buffer-type' for the current buffer until Emacs
is next idle, so opening a buffer doesn't block on starting Aspell."
  (let ((buf (current-buffer)))
    (run-with-idle-timer 0.1 nil
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (flyspell-on-for-buffer-type)))))))

(add-hook 'text-mode-hook #'my-flyspell-defer-enable)
(add-hook 'prog-mode-hook #'my-flyspell-defer-enable)

;; Enable which key
(which-key-mode)

;;; diff-hl
;; Defer turning diff-hl on until the first version-controlled file is
;; opened, rather than always paying its load cost at startup even on
;; sessions that never touch a VC-tracked buffer.
(defun my-diff-hl-enable-if-vc ()
  "Turn on `global-diff-hl-mode' the first time a VC-tracked file is visited."
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (remove-hook 'find-file-hook #'my-diff-hl-enable-if-vc)
    (global-diff-hl-mode 1)
    (diff-hl-flydiff-mode 1)))

(add-hook 'find-file-hook #'my-diff-hl-enable-if-vc)

(use-package diff-hl
  :ensure t
  :defer t
  :config
  (let ((changed-color (modus-themes-get-color-value 'bg-changed-fringe t)))
    (set-face-attribute 'diff-hl-insert nil :background changed-color)
    (set-face-attribute 'diff-hl-change nil :background changed-color)
    (set-face-attribute 'diff-hl-delete nil :background changed-color)))

;;; markdown mode

(use-package markdown-mode
   :ensure t
   :defer t)

(use-package stripe-buffer
   :ensure t
   :defer t)

(setq markdown-header-scaling t)

;; render remote images
(setq markdown-display-remote-images t)

;; Make markdown coding faces inherit as need from from fixed pitch
(my-ignore (custom-set-faces
 '(markdown-markup-face ((t (:inherit fixed-pitch))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))))

;; When following a link whose target can't be found as-is, retry
;; with a ".md" extension appended (e.g. a link to "foo" or "foo.html"
;; falls back to "foo.md" if that file exists).
;; This helps with compatibility with how github does relative links in its wiki mode
(defun my-markdown-translate-filename-add-md-extension (filename)
  "Return FILENAME, retrying with a \".md\" extension if it doesn't exist."
  (if (file-exists-p filename)
      filename
    (let ((with-md (concat filename ".md")))
      (if (and (not (string-suffix-p ".md" filename t))
	       (file-exists-p with-md))
	  with-md
        filename))))

;; insert a date function for use in the markdown snippets.
(defun insert-date ()
  "Insert the current date and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(setq markdown-translate-filename-function #'my-markdown-translate-filename-add-md-extension)

(defconst my-markdown-liquid-post-url-regexp
  "{% post_url[[:space:]]*\\([^ ]*\\)[[:space:]]*%}"
  "Matches a Jekyll {% post_url NAME %} liquid tag; group 1 is NAME.")

;; Fix up for liquid style pre-processed links used by jekyll.
;; markdown-link-at-pos (called by markdown-link-url) splits an inline link's
;; parenthesized destination on the first whitespace into a url/title pair,
;; to support `[text](url "title")` syntax. That mangles a liquid tag like
;; "{% post_url NAME %}" down to just "{%" before we'd ever see it via
;; markdown-link-url. Detect the tag directly from
;; the raw buffer text instead and otherwise defer to the original function.
(defun my-markdown-liquid-post-url-at-point ()
  "Return NAME if point is on a link/tag whose destination is {% post_url NAME %}."
  (let* ((values (and (markdown-link-p) (markdown-link-at-pos (point))))
         (begin (nth 0 values))
         (end (nth 1 values)))
    (if (and begin end)
        ;; Formal `[text](...)` link: search its whole span, since point may
        ;; land on the visible text rather than the (possibly hidden) tag.
        (save-excursion
          (goto-char begin)
          (when (re-search-forward my-markdown-liquid-post-url-regexp end t)
            (match-string 1)))
      ;; No recognized link syntax: only match a bare tag point is inside.
      (save-excursion
        (let ((pt (point)) (eol (line-end-position)))
          (goto-char (line-beginning-position))
          (catch 'found
            (while (re-search-forward my-markdown-liquid-post-url-regexp eol t)
              (when (and (<= (match-beginning 0) pt) (<= pt (match-end 0)))
                (throw 'found (match-string 1))))))))))

(defun my-markdown-follow-liquid-post-url (orig-fn &rest args)
  "Browse a resolved {% post_url %} liquid link at point, else call ORIG-FN."
  (let ((name (my-markdown-liquid-post-url-at-point)))
    (if name
        (markdown--browse-url name)
      (apply orig-fn args))))

;; markdown-mode is now deferred (see its use-package declaration above),
;; so this can't run until markdown-follow-link-at-point actually exists.
(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-follow-link-at-point :around #'my-markdown-follow-liquid-post-url))

(add-hook 'markdown-mode-hook 'markdown-toggle-inline-images)
(add-hook 'markdown-mode-hook 'stripe-table-mode)

;;Prettify check boxes to use Unicode characters.
;; I've also adjusted the faces to scale these up quite a bit so they're more visible
(add-hook 'markdown-mode-hook (lambda ()
  "Beautify md Checkbox Symbol"
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[x]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))


;; Automatically add the index menu entry for org and markdown modes. This will
;; also be available via the context menus
(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'imenu-add-menubar-index)

;; Complete a frontmatter "tags:" value against every tag already used
;; elsewhere in the project, via wikimode's project-wide tag scan
;; (`wikimode-project-tags').
(defun my-markdown-frontmatter-bounds ()
  "Return (START . END) of the current buffer's YAML frontmatter body, or nil."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (looking-at-p "---[ \t]*$")
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "^---[ \t]*$" nil t)
            (cons start (line-beginning-position))))))))

(defun my-markdown-tags-line-value-start ()
  "If point's line is a frontmatter tags entry, return where its value starts.
Matches either the inline form (\"tags: [a, b]\" or \"tags: a, b\") or a
block-list item (\"  - a\") under a bare \"tags:\" header line above it."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "tags:[ \t]*") (match-end 0))
     ((looking-at "[ \t]*-[ \t]+")
      (let ((value-start (match-end 0)) (found nil))
        (while (and (not found) (zerop (forward-line -1)))
          (cond
           ((looking-at-p "[ \t]*-[ \t]+"))
           ((looking-at-p "tags:[ \t]*$") (setq found t))
           (t (setq found 'stop))))
        (and (eq found t) value-start))))))

(defun my-markdown-tags-capf ()
  "`completion-at-point-functions' entry for markdown frontmatter tag values."
  (let ((fm (my-markdown-frontmatter-bounds)))
    (when (and fm (<= (car fm) (point)) (< (point) (cdr fm)))
      (let ((value-start (my-markdown-tags-line-value-start)))
        (when value-start
          (let* ((eol (line-end-position))
                 (before (save-excursion
                           (if (re-search-backward "[,[]" value-start t)
                               (1+ (point))
                             value-start)))
                 (after (save-excursion
                          (if (re-search-forward "[],]" eol t)
                              (match-beginning 0)
                            eol)))
                 (start (save-excursion (goto-char before)
                                        (skip-chars-forward " \t\"'") (point)))
                 (end (save-excursion (goto-char after)
                                      (skip-chars-backward " \t\"'" start) (point))))
            (list (min start end) (max start end)
                  (wikimode-project-tags) :exclusive 'no)))))))

(add-hook 'markdown-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'my-markdown-tags-capf nil t)))


;;; org-mode
;; My typical usage of Org includes a main work tracking file, org-agenda,
;; integration with my exchange calendar and simple daily journal for which I
;; have a capture template to add standup entries


;; Define an org root directory

(defcustom my-org-root "~/org" "Root location for org files"
  :type 'string
  :group 'local)

;; mouse support
;; This is fairly expensive so we defer it until org is actually loaded
;; rather than paying the cost on every startup.
(with-eval-after-load 'org
  (require 'org-mouse))

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; word wrap for normal text and stripe mode for tables
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'stripe-buffer-mode)
  (my-ignore (add-hook 'org-mode-hook (lambda() (setq line-spacing 0.5)))))

;; hide asterisks in headers
;; ignored because right now I'm using base org-bullets-mode instead
(my-ignore
 (use-package org-bullets
   :ensure t
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
   (setq org-bullets-bullet-list '("\u200b"))
   ))

;; change list markers from hyphens ;to squares
;; ignored currently
(my-ignore (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▪")))))))

;; set the org-agenda prefix to skip printing the source files
(setq org-agenda-prefix-format '(
  ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
  (agenda  . " %-12t ")
  (timeline  . "  % s")
  (todo  . " %i %-12:c")
  (tags  . " %i %-12:c")
  (search . " %i %-12:c")))

;; 3 States for TODO
(setq org-todo-keywords
      '((sequence "TODO" "BLOCKED" "|" "DONE" )))

;; Autolist completion
(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))

;; org-modern styling - handles bullets, checkboxes, styling of todo, timestamps etc.
;; disable table formatting in favor of org-pretty-table because of header rendering issues
(use-package org-modern
  :defer t
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table nil)
  ;; Level-3's default fold indicator (⯈/⯆, U+2BC8/U+2BC6) lives in the sparse
  ;; Miscellaneous Symbols and Arrows block and doesn't render in our fonts,
  ;; unlike the other levels' triangles (Geometric Shapes block). Swap it for
  ;; the universally-supported Arrows block instead.
  (setq org-modern-fold-stars
        '(("▶" . "▼") ("▷" . "▽") ("→" . "↓") ("▹" . "▿") ("▸" . "▾"))))

;; Indent by heading depth
(setq org-startup-indented t)

;; Setup capture templates
;; currently only have one for standup summaries
(defconst my-capturefile (file-name-concat my-org-root "standup.org") "Standup summary filename")
(setq org-capture-templates
  '(    ;; ... other templates

    ("s" "Standup Entry"
         entry (file+datetree my-capturefile )
         "* %?"
         :empty-lines 1)

        ;; ... other templates
    ))

;; Show up to 4 levels of org headings in the imenu and imenu-list
;; This is set due to how expensive building the org imenu tree is.
(setq org-imenu-depth 4)

;; Older unused code to prettify check boxes to use Unicode characters.
;; Currently superseded by org-modern
(my-ignore
(add-hook 'org-mode-hook (lambda ()
 "Beautify Org Checkbox Symbol"
 (push '("[ ]" .  "☐") prettify-symbols-alist)
 (push '("[X]" . "☑" ) prettify-symbols-alist)
 (push '("[-]" . "❍" ) prettify-symbols-alist)
 (prettify-symbols-mode))))

;; I'm using org-pretty-table rather than org-modern's support for now
;; because it works better
(use-package org-pretty-table
  :ensure t
  :vc (:url "https://github.com/Fuco1/org-pretty-table")
  :config
  :hook (org-mode . org-pretty-table-mode)
)

;;; Programming modes

;; magit - git porcelain installation.
;; transient is bundled with Emacs, but the bundled copy here is older than
;; what current magit requires. Explicitly managing it via package.el gets a
;; fresh install that satisfies magit's minimum.
(use-package transient
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t)

(defun my-common-prog-mode-setup ()
  (display-line-numbers-mode)
  (column-number-mode))

;; Set display line number mode on
(add-hook 'prog-mode-hook #'my-common-prog-mode-setup)

;;;; Project.el settings.

;; I prefer to have project-switch-project to just change the project for the next project
;; command. bear in mind, project mostly uses the current directory of the buffer to
;; determine the project
(setq project-switch-commands 'project-any-command)
;; Set project boundary at the first build.gradle found as well
(setopt project-vc-extra-root-markers '("build.gradle"))

;;;; treesit-fold - useful for folding things like imports in java
;;
;; Java's grammar has no single node wrapping all the imports -- each
;; import_declaration is just a sibling of the next, so folding on
;; import_declaration alone (e.g. via treesit-fold-range-seq) only ever
;; folds one import. This walks forward across the run of consecutive
;; import_declaration (and interleaved comment) siblings so that folding
;; on any import line collapses that line and every import after it.
(defun treesit-fold-range-java-imports (node offset)
  "Fold the contiguous run of Java import declarations starting at NODE."
  (let ((last-node node)
        (sibling (treesit-node-next-sibling node t)))
    (while (member (treesit-node-type sibling)
                    '("import_declaration" "line_comment" "block_comment"))
      (when (equal (treesit-node-type sibling) "import_declaration")
        (setq last-node sibling))
      (setq sibling (treesit-node-next-sibling sibling t)))
    (unless (treesit-node-eq last-node node)
      (treesit-fold--cons-add
       (cons (treesit-node-start node) (treesit-node-end last-node))
       offset))))

;; Automatically fold the import block when a Java buffer is opened, called
;; from `setup-common-java' below.
(defun treesit-fold-close-java-imports ()
  "Fold the run of Java import declarations at the top of the buffer, if any."
  (when (and (treesit-available-p) (treesit-parser-list) (treesit-fold-usable-mode-p))
    (when-let* ((first-import
                 (seq-find (lambda (n) (equal (treesit-node-type n) "import_declaration"))
                           (treesit-node-children (treesit-buffer-root-node)))))
      ;; `treesit-fold-summary-java' recovers the folded node from `point',
      ;; so move there rather than passing the node to `treesit-fold-close'
      ;; directly -- see the comment above `treesit-fold-summary-java'.
      (goto-char (treesit-node-start first-import))
      (treesit-fold-close))))

(defun treesit-fold-summary-java (doc-str)
  "Summarize a folded Java DOC-STR by the tree-sitter node type it came from."
  (let* ((node (treesit-fold--foldable-node-at-pos))
         (type (and node (treesit-node-type node))))
    (if (member type '("block_comment" "line_comment"))
        (treesit-fold-summary-javadoc doc-str)
      (or type (treesit-fold-summary-javadoc doc-str)))))

;; Add a "Fold Line" entry to the right-click context menu when the click
;; lands on the line-number gutter. Clicks there are reported as ordinary
;; text-area clicks whose X pixel offset is measured from the left edge of
;; the text area (which includes the line-number glyphs), so a click is in
;; the gutter when its X is less than the line-number display's pixel width.
(defun context-menu-fold-line (menu click)
  "Add a fold-line entry to MENU when CLICK lands on the line-number gutter."
  (when (and (bound-and-true-p display-line-numbers-mode)
             (treesit-available-p)
             (treesit-parser-list)
             (treesit-fold-usable-mode-p))
    (let* ((posn (event-end click))
           (x (car (posn-x-y posn)))
           (pos (and (posn-point posn)
                     (save-excursion
                       (goto-char (posn-point posn))
                       (line-beginning-position)))))
      (when (and x pos
                 (< x (line-number-display-width t))
                 (treesit-fold--foldable-node-at-pos pos))
        (define-key menu [treesit-fold-line]
          `(menu-item "Fold Line"
                      ,(lambda ()
                         (interactive)
                         ;; `treesit-fold-summary-java' recovers the folded
                         ;; node from `point', so move there rather than
                         ;; passing the node to `esit-fold-close' directly.
                         (save-excursion
                           (goto-char pos)
                           (treesit-fold-close)))
                      :help "Fold the code at this line")))))
  menu)

(use-package treesit-fold
  :ensure t
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  ;; Only java-mode/java-ts-mode buffers actually call into this (see
  ;; `setup-common-java'), so there's no need to load it eagerly at
  ;; startup -- its own autoloads cover `treesit-fold-mode' et al, and
  ;; that first call is what triggers this :config block to run.
  :defer t
  :config
  ;; Add a rule for java-mode and java-ts-mode to fold the whole run of imports at once
  (dolist (mode '(java-mode java-ts-mode))
    (push '(import_declaration . treesit-fold-range-java-imports)
          (alist-get mode treesit-fold-range-alist)))
  (dolist (mode '(java-mode java-ts-mode))
    (setf (alist-get mode treesit-fold-summary-parsers-alist)
          #'treesit-fold-summary-java))
  (add-hook 'context-menu-functions #'context-menu-fold-line))

;;;; Java

;; set java home
(defgroup my-environment nil
  "Personal environment config."
  :group 'environment)

(defun my-java-home-set (symbol value)
  "Set `my-java-home' to VALUE and propagate it to dependent Java tooling.
Re-applies JAVA_HOME and the jdtls/dap-mode settings derived from it --
so customizing `my-java-home' (e.g. via M-x customize-variable) takes
effect without a restart."
  (set-default symbol value)
  (setenv "JAVA_HOME" value)
  (setq dap-java-java-command (concat value "/bin/java"))
  (setq my-jdtls-settings `(:java (:home ,value)))
  (setq-default eglot-workspace-configuration my-jdtls-settings))

(defcustom my-java-home
  (expand-file-name "~/.jenv/versions/21.0")
  "Java path used by eglot/jdtls"
  :type 'directory
  :set #'my-java-home-set
  :group 'my-environment)

;; Establish the initial dap-mode/jdtls settings derived from my-java-home.
;; This is the single source of truth for that derivation -- see
;; `my-java-home-set', which also reruns it on later customization.
(my-java-home-set 'my-java-home my-java-home)

(defun setup-common-java ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t)
  (setq-local imenu-depth 3)
  (setq-local imenu-create-index-function 'ilist-plus-java-ts-index)
  (treesit-fold-mode)
  (treesit-fold-close-java-imports))

(add-hook 'java-mode-hook 'setup-common-java)
(add-hook 'java-ts-mode-hook 'setup-common-java)

; Setup automatic mode remapping so we always use treesitter for java
(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)))

;;;; eglot

;; eglot configuration.
(setq eglot-max-file-watches 5000)
(setq eglot-connect-timeout 180)
(setq eglot-report-progress 'messages) ;; progress updates in the message bar
;; tree sitter is fine for faces and we don't need highlighting on the current line.
(setq eglot-ignored-server-capabilities '(:semanticTokensProvider :documentHighlightProvider))
(setq eglot-stay-out-of '(imenu)) ;; Use my version
;; asynchronous connections to not freeze during initial handshake.
(setq eglot-sync-connect nil)

;; prettier format for the json rpc - needed a bit less with the eglot-report-progress to
;; messages buffer but still easier to parse
(setq eglot-events-buffer-config '(:size 2000000 :format short))

;; jsonrpc--log-event's `short' branch is just the preamble
;; (direction/method/id) -- :log-text is only ever populated for
;; internal debug/warn events, never for real protocol messages, so
;; there's normally nothing after it. Inject a summary of :message
;; (method+params, or result/error) as :log-text when one isn't
;; already present; the `full'/`lisp' branches prefer :json/
;; :foreign-message over :log-text, so this only affects `short'.
(defun jsonrpc-elide-text-document (params)
  "Copy of PARAMS with params.textDocument.text elided.
That field carries a whole file's contents (e.g. on
textDocument/didOpen) and otherwise floods the short log format.
uri/version and everything else in PARAMS are left untouched."
  (if (and (listp params) (plist-member params :textDocument))
      (let ((td (plist-get params :textDocument)))
        (if (and (listp td) (plist-member td :text))
            (plist-put (copy-sequence params) :textDocument
                       (plist-put (copy-sequence td) :text "<elided>"))
          params))
    params))

(defun jsonrpc-elide-token (params)
  "Copy of PARAMS with a top-level :token field elided.
$/progress notifications carry a fresh token on every call and
otherwise flood the short log format with noise."
  (if (and (listp params) (plist-member params :token))
      (plist-put (copy-sequence params) :token "<elided>")
    params))

(defun jsonrpc-log-text (message)
  "One-line summary of jsonrpc MESSAGE, for `short' events format."
  (cond ((equal (plist-get message :method) "$/progress")
         (let ((value (plist-get (plist-get message :params) :value)))
           (format "%s: %s" (or (plist-get value :kind) "")
                   (or (plist-get value :message) ""))))
        ((plist-get message :method)
         ;; preamble already shows the method name (e.g. "--> $/progress"),
         ;; so only contribute the params here.
         (format "%s" (or (jsonrpc-elide-token
                            (jsonrpc-elide-text-document
                             (plist-get message :params)))
                           "")))
        ((plist-get message :result)
         (format "=> %s" (plist-get message :result)))
        ((plist-get message :error)
         (format "ERROR %s" (plist-get message :error)))))

(defun jsonrpc-skip-message-p (message)
  "Non-nil if MESSAGE shouldn't be logged at all.
Suppresses $/progress begin/end notifications entirely -- `begin'
just opens a task and `end' just closes it, neither carries
anything as useful as the `report' messages in between."
  (and (equal (plist-get message :method) "$/progress")
       (member (plist-get (plist-get (plist-get message :params) :value)
                           :kind)
               '("begin" "end"))))

(with-eval-after-load 'jsonrpc
  (advice-add 'jsonrpc--log-event :around
              (lambda (orig connection origin &rest plist)
                (let ((message (plist-get plist :message)))
                  (unless (jsonrpc-skip-message-p message)
                    (when (and message (not (plist-get plist :log-text)))
                      (setq plist (plist-put plist :log-text
                                              (jsonrpc-log-text message))))
                    (apply orig connection origin plist))))))

(with-eval-after-load 'eglot
  ;; Keep jdtls metadata in a per-project dir under jdtls-cache.
  ;; Must be a lambda, not a precomputed path: eglot only loads
  ;; (and evaluates this let-binding) once, on first use, so a
  ;; a lambda is needed.
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode)
                 . ,(lambda (&optional _interactive project)
                      (let ((cache-dir (expand-file-name
                                        (md5 (or (and project (project-root project))
                                                 default-directory))
                                        (locate-user-emacs-file "jdtls-cache"))))
                        (list "jdtls"
                              "--jvm-arg=-Djava.import.generatesMetadataFilesAtProjectRoot=false"
                              "-data" cache-dir
                              :initializationOptions
                              (list :settings my-jdtls-settings)))))))

;;;; flymake

;; Dock the diagnostics list as a bottom "problems panel" instead of
;; letting it split whatever window happens to be current.
(add-to-list 'display-buffer-alist
             '("\\`\\*Flymake diagnostics for"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.25)
               (dedicated . t)))

;; [mode-line mouse-1] is globally bound to `mouse-select-window' (see
;; mouse.el), so clicking the flymake diagnostics window's mode-line
;; just focuses it instead of closing it. Override that specifically
;; for this mode.
(defun flymake-buffer-quit (event)
  "Close the flymake diagnostics window clicked on in its mode-line."
  (interactive "e")
  (quit-window nil (posn-window (event-start event))))

(defvar-keymap my-flymake-buffer-id-keymap
  "<mode-line> <mouse-1>" #'flymake-buffer-quit)

(add-hook 'flymake-diagnostics-buffer-mode-hook
          (lambda ()
            (setq-local mode-line-buffer-identification-keymap
                        my-flymake-buffer-id-keymap)))

;; Save space by not showing zero warn/error counter in the mode line
(setq flymake-suppress-zero-counters t)

;; Configure lightbulbs with corresponding diagnostic colors/faces'
;; And place them in the margin rather than the fringe
(setq flymake-indicator-type 'margins)
(setq flymake-margin-indicators-string
 `((error   ,(nerd-icons-mdicon "nf-md-lightbulb") compilation-error)
   (warning ,(nerd-icons-mdicon "nf-md-lightbulb") compilation-warning)
   (note    ,(nerd-icons-mdicon "nf-md-lightbulb") compilation-info)))

;;;; python

;; TODO turn on eglot integration later.


;;;; elisp

;; Group `use-package` declarations under their own imenu heading.
;; Also extract all the ;;; sections.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (setq-local imenu-depth 2)
            (setq-local imenu-create-index-function 'ilist-plus-elisp-index)))


;;; SQL
;; clutch - database access
(use-package clutch
  :ensure t
  :defer t)

;; sql formatting setup for sqlformat-* functions.
(use-package sqlformat
  :defer t
  :ensure t
  :config (setq sqlformat-command 'pgformatter
                sqlformat-args '("-s2" "-g")))


;;; imenu-list
;; Note: C-\ is bound to smart toggle.
(use-package imenu-list
  :ensure t
  :config
  ;; Some built in default around resizing and window focus I prefer.
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil
	;; rescan buffers as they change
	imenu-auto-rescan t))

;; Load all of my custom imenu extensions.
(use-package ilist-plus
  :ensure nil
  ;; For local test/dev when turned on.
  ;;   :load-path "~/dev/ilist-plus/"
  :vc (:url "https://github.com/benleis1/ilist-plus")
  :init
  ;; Bind the fixed pitch icon font for the imenu modeline
  (setq ilist-plus-fixed-font my-default-fixed-pitch-font))

;; Now that modeline.el (loaded above) has defined the richer dedicated-window
;; keymap, rebuild the *Ilist* mode-line to use it instead of imenu.el's
;; self-contained fallback after imenu loads.

(with-eval-after-load 'imenu-list
  (setq imenu-list-mode-line-format
	(ilist-plus--build-mode-line-format my-modeline-dedicated-window-map)))

;; Setup file menu to include load/save desktop
;; Note: lookup-key is the way to find existing entry names
(define-key-after
  (lookup-key global-map [menu-bar file])
  [load-desktop]
  '("Load Saved Desktop" . desktop-read)
  'project-open-file)

(define-key-after
  (lookup-key global-map [menu-bar file])
  [save-desktop]
  '("Save Desktop" . desktop-save)
  'write-file)

;; Add zoom in/out to buffer menu
;; TODO get the keybinding message straight?
(define-key-after
  (lookup-key global-map [menu-bar buffer])
  [zoom-in]
  '("Zoom in" . text-scale-increase)
  'next-buffer)

(define-key-after
  (lookup-key global-map [menu-bar buffer])
  [zoom-out]
  '("Zoom out" . text-scale-decrease)
  'zoom-in)

;; Some gymnastics to place things in the order I want since
;; I haven't found a way to directly place after a separator
(define-key global-map [menu-bar buffer next-buffer] nil)
(define-key-after
  (lookup-key global-map [menu-bar buffer])
  [next-buffer]
  '("Next Buffer" . next-buffer)
  'zoom-out)

;;; Excorporate setup.
;; I've modified this quite a bit to directly generate org files.

(use-package excorporate
  :after org-agenda
  :ensure t
  :defer t
  :init
  (setq excorporate-update-diary nil)
  (setq excorporate-update-org t)
  ;; Configure excorporate to use the a file which I've linked to agenda for daily meetings
  ;; setq excorporate-org-buffer-name "~/org/daily-meetings.org"
  (setq	excorporate-org-persist-buffer t)
  )

;; Track whether we've turned excorporate on or not
(setq my-calendar-init nil)

;; setup a callback to cleanup the diary buffers for use below
(defun my-diary-cleanup ()
  (when (get-buffer "diary")
    (kill-buffer "diary"))
  (when (get-buffer "diary-excorporate-transient")
    (kill-buffer "diary-excorporate-transient"))
  (when (get-buffer "diary-excorporate-today")
    (kill-buffer "diary-excorporate-today"))
  (org-agenda-maybe-redo)
  (message "Cleaned up diary buffers"))

;; TODO - advice after exco-diary--fix-percent-signs to redo org agenda?
;; We should probably just ignore the diaries in tab-line because we can't tell
;; the difference between diary direct mode vs org-agenda mode
;; this would frankly be better if we directly added into a org-file and brought to front

;; Advice function to run before org-agenda and download the calendar
;; args are unused
;; excorporate uses the fsm (finite-state-machine) to do most operations aysnc
;; Issues: the initial excorporate setup triggers a diary download without a callback to do cleanup
;; The agenda itself loads the diary buffer - we should probably just leave it off the tab-line?
(defun my-agenda-update-diary (&optional _args)
  "call excorporate to update the diary for today"

  ;; onetime setup
  (if (not my-calendar-init)
    (progn
      (message "excorporate setup starting")
      ;; turn off diarying for this call
      (excorporate)
      (message "excorporate setup done")
      (setq my-calendar-init t))

    ;; skip if the file was updated within the last minute
    (message "my diary update started %s" (current-time-string))
    (let* ((time-list (decode-time (current-time)))
	   (day (nth 3 time-list))
	   (month (nth 4 time-list))
	   (year (nth 5 time-list)))
      (exco-org-show-day month day year))))

(advice-add 'org-agenda :before #'my-agenda-update-diary)

;; Import emacs calendar/diary entries in org. Ignored currently due to
;; the customizations done above
(my-ignore (setq org-agenda-include-diary t))


;;; ediff

;; Setup a command line switch gemacs -diff file1 file2
;; TODO: this doesn't work with emacsclient only gemacs
(defun command-line-diff (_switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

(with-eval-after-load 'ediff
  (defvar my-ediff-buffers '() "Track the buffers being used")
  (defvar my-ediff-bwin-config nil  "Track the initial window configuration")

  ;; Capture window state and turn off my-modeline
  (defun my-ediff-bsh ()
    "Function to be called before any buffers or window setup for
    ediff."
    (setq my-ediff-buffers '())
    (setq my-ediff-bwin-config (current-window-configuration))
    ;;  (setq my-ediff-linenum-state (bound-and-true-p display-line-number-mode))
    (my-modeline-mode -1))

  ;; Create a mode-line-buffer that prints the filename and contains a
  ;; static hint about the full filename
  (defun simple-mode-line-buffer ()
    (list (propertize
           "%12b"
           'face 'mode-line-buffer-id
           'help-echo
	   (if (buffer-file-name) (buffer-file-name) (buffer-name))
           'mouse-face 'mode-line-highlight
           'local-map mode-line-buffer-identification-keymap)))

  ;; Return buffer state we want to save/restore as a list
  (defun my-get-buffer-state ()
    (list (current-buffer)
	  (bound-and-true-p display-line-numbers-mode)
	  (bound-and-true-p tab-line-mode)))

  ;; Restore back the saved buffer state
  (defun my-restore-buffer-state ( state )
    (let* ((buffer (nth 0 state))
	   (linenums (nth 1 state))
	   (tab-line (nth 2 state)))
      (with-current-buffer buffer
	(progn
	  (message "restoring %s" buffer)
	  (unless linenums (display-line-numbers-mode -1))
	  (if tab-line (tab-line-mode 1))))))

  ;; hook before prep buffers to fixup the mode line hints
  ;; Turn off tab-line, turn on line numbers and record the list of buffers
  (defun my-ediff-prep-buffers ()
    "Function that is called after each buffer to be diff'ed is setup"
    (message "setting mode line %s f: %s" (current-buffer) (buffer-file-name))
    (setq my-ediff-buffers (cons (my-get-buffer-state) my-ediff-buffers))
    (tab-line-mode -1)
    (display-line-numbers-mode 1)
    (setq mode-line-format (simple-mode-line-buffer)))

  ;; Restore back the old states
  (defun my-ediff-qh ()
    "Function to be called when ediff quits."
    (my-modeline-mode 1)
    (dolist (element my-ediff-buffers)
      (my-restore-buffer-state element))
    (when my-ediff-bwin-config
      (set-window-configuration my-ediff-bwin-config)))

  (add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
  (add-hook 'ediff-quit-hook 'my-ediff-qh)
  (add-hook 'ediff-prepare-buffer-hook 'my-ediff-prep-buffers 'append)

  ;; side by side comparison layout
  (setq ediff-split-window-function 'split-window-horizontally)

  ;; Keep the control window in the default frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; Diff counter + up/down nav buttons prepended before the control
  ;; buffer's help line ("Type ? for help"). ediff centers that line by
  ;; padding it with leading whitespace to (roughly) the window width, so
  ;; inserting our text via before-string just lengthens the line past the
  ;; window width and wraps it, growing the control window by a line. Instead
  ;; we put a 'display overlay over that leading whitespace (the same span
  ;; `ediff-setup-control-buffer' itself skips past via `ediff-whitespace')
  ;; so our text replaces the padding instead of adding to it. Buttons use a
  ;; plain 'keymap' text property (like button.el), not 'local-map' on the
  ;; mode/header line, which needs a [header-line mouse-1]-prefixed binding
  ;; to receive clicks at all.
  ;; Advising ediff-setup-control-buffer and ediff-refresh-mode-lines (rather
  ;; than a fixed list of hooks) keeps the overlay in sync across startup,
  ;; ?-toggled help text, and every diff-position change, without needing to
  ;; enumerate every command that can move ediff-current-difference.
  (defvar-local my-ediff-nav-overlay nil)

  (defun my-ediff-nav-button (label command help)
    (propertize label
		'help-echo help
		'mouse-face 'highlight
		'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [mouse-1] command)
                          map)))

  (defun my-ediff-nav-string ()
    (let ((cur ediff-current-difference)
          (total ediff-number-of-differences))
      (concat
       (cond ((< cur 0) (format "_/%d" total))
             ((>= cur total) (format "$/%d" total))
             (t (format "%d/%d" (1+ cur) total)))
       " "
       (my-ediff-nav-button "▲" #'ediff-previous-difference "Previous diff")
       " "
       (my-ediff-nav-button "▼" #'ediff-next-difference "Next diff")
       "  ")))

  (defun my-ediff-install-nav-overlay (&rest _)
    (unless (overlayp my-ediff-nav-overlay)
      (setq my-ediff-nav-overlay (make-overlay (point-min) (point-min))))
    (let ((pad-end (save-excursion
                     (goto-char (point-min))
                     (skip-chars-forward ediff-whitespace)
                     (point))))
      (move-overlay my-ediff-nav-overlay (point-min) pad-end)
      (overlay-put my-ediff-nav-overlay 'display nil)
      (overlay-put my-ediff-nav-overlay 'before-string nil)
      (overlay-put my-ediff-nav-overlay
                   (if (> pad-end (point-min)) 'display 'before-string)
                   (my-ediff-nav-string))))

  (advice-add 'ediff-setup-control-buffer :after #'my-ediff-install-nav-overlay)
  (advice-add 'ediff-refresh-mode-lines :after #'my-ediff-install-nav-overlay)
  )
;; Some GC analytics to see if tuning GC is interesting
;; this is a bit intrusive so I'll turnoff most of the time
(defun gc-notification ()
  (message "Garbage Collection occurred"))

(my-ignore (add-hook 'post-gc-hook #'gc-notification))

;;; Completion frameworks.

;; marginalia + vertico + orderless for completions. I'm still deciding about Corfu.
;; this enables a vertical list of completions with context dependent notes in the minibuffer

(use-package vertico
  :ensure t
  :custom
  (vertico-sort-function 'vertico-sort-history-length-alpha)
  :init (vertico-mode 1))

(use-package marginalia
  :ensure t
  :init (marginalia-mode 1))

;; Trying out orderless completion
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  ;; `global-corfu-mode' is commented out below, so nothing currently
  ;; activates corfu and it can be deferred.
  :defer t
  :init
;;  (global-corfu-mode)
  )

;;; Font name completion for customize buffers
;; This was added to base emacs in version 31 and I will remove it soon.

;; return if the current position is a Font Family widget. Checks one
;; character back too since widget-at looks at the char *after* point, which
;; falls outside the field once you've typed up to its end.
(defun font-family-widget-p ()
  (let ((w (or (widget-at (point))
               (widget-at (max (point-min) (1- (point)))))))
    (equal (widget-get w :tag) "Font Family")))

;; Completion function for font names to be hooked to custom mode
;; need to return a list (start end collection) if this matches or nil if not
(defun complete-font-name ()
  (when (font-family-widget-p)
    `(,(pos-bol) ,(pos-eol) ,(font-family-list))))

;; Hook completion in and setup M-TAB binding for it. custom-field-keymap
;; normally binds M-TAB to `widget-complete', which bypasses
;; completion-at-point-functions entirely and falls back to the `string'
;; widget's own :complete, hard-coded to ispell-complete-word. Rebind both
;; the GUI (<M-tab>) and terminal (M-TAB, i.e. ESC TAB) forms so either
;; frame type reaches completion-at-point/complete-font-name.
(defun add-complete-font-name()
  (add-hook 'completion-at-point-functions #'complete-font-name nil t)
  (define-key custom-field-keymap (kbd "M-<tab>") 'completion-at-point)
  (define-key custom-field-keymap (kbd "M-TAB") 'completion-at-point))

(add-hook 'Custom-mode-hook 'add-complete-font-name)

;; Inline "Set Font..." button next to the Font Family field in
;; customize-face buffers, on the same line like the Value Menu buttons
;; next to Weight/Slant/etc.
;;
;; Uses x-select-font directly (the same dialog menu-set-font uses under the
;; hood) rather than menu-set-font itself, since menu-set-font's job is to
;; apply the chosen font as the new default frame font on all frames --
;; x-select-font just returns the pick without touching any frame's font.
(defun my-select-font ()
  "Read a font from the user without changing any frame's font.
Uses the native font dialog via `x-select-font' when available,
falling back to `mouse-select-font'."
  (if (fboundp 'x-select-font)
      (x-select-font)
    (mouse-select-font)))

(defun my-widget-find-ancestor (widget type)
  "Walk up WIDGET's :parent chain and return the nearest ancestor of TYPE."
  (while (and widget (not (eq (widget-type widget) type)))
    (setq widget (widget-get widget :parent)))
  widget)

(defun my-customize-face-set-font (button &rest _)
  "Pick a font with `my-select-font' and fill its family, height, weight,
and slant into the enclosing custom-face-edit widget's attributes."
  (unless window-system
    (user-error "Selecting a font requires a graphical frame"))
  (let ((edit-widget (my-widget-find-ancestor button 'custom-face-edit)))
    (unless edit-widget
      (user-error "Could not find the face attribute editor"))
    (let ((font (my-select-font)))
      (unless font
        (user-error "No font selected"))
      (let ((attrs (font-face-attributes font))
            (value (copy-sequence (widget-value edit-widget))))
        (dolist (key '(:family :height :weight :slant))
          (when (plist-member attrs key)
            (setq value (plist-put value key (plist-get attrs key)))))
        (widget-value-set edit-widget value)
        (widget-setup)))))

(defun my-face-family-value-create (widget)
  "Render an inline \"Set Font...\" button right after the Font Family
tag, followed by the normal editable field."
  (let ((buttons (widget-get widget :buttons)))
    (push (widget-create-child-and-convert
           widget 'push-button
           :tag " Set Font... "
           :help-echo "Pick a font from the system font panel and fill in family/height/weight/slant."
           :action #'my-customize-face-set-font)
          buttons)
    (widget-put widget :buttons buttons))
  (widget-insert " ")
  (widget-field-value-create widget))

;; custom-face-edit's :args is computed once, at cus-edit.el load time, by
;; mapping over custom-face-attributes -- and it embeds the very same list
;; object as each attribute's widget spec (not a copy). So replacing the
;; :family entry's spec (e.g. via setcar) would only repoint
;; custom-face-attributes's own slot, leaving custom-face-edit's
;; already-frozen :args pointing at the old list. Extending that shared
;; list object in place with nconc reaches both.
(let ((spec (cadr (assq :family custom-face-attributes))))
  (when (and spec (not (plist-member spec :value-create)))
    (nconc spec (list :value-create #'my-face-family-value-create))))

;;; Snippets
;; This is currently used in org and markdown mode but I only have it on by
;; default for markdown.
(use-package yasnippet
  :ensure t
  :defer t
  :init
  (add-hook 'markdown-mode-hook #'yas-minor-mode))

;; No key binding for now.
(use-package consult-yasnippet
  :ensure t
  :defer t)

;;; Wikimode
(use-package wikimode
  :ensure t
  :vc (:url "https://github.com/benleis1/wikimode")
  ;; Deferred but wikimode-project-tags is used independently so it's listed
  ;; explicitly here to get an autoload stub too.
  :commands (wikimode-toggle wikimode-project-tags)
  :defer t)

;;;  Consult navigation package
(use-package consult
  :ensure t
  :config
  ;; Replace switch-buffers with consult-buffer
  (keymap-global-set "C-x b" 'consult-buffer)

  ;; live preview when M-. is pressed rather than automatically since it easily
  ;; splits the window
  (setq consult-preview-key "M-.")

  ;; vertico-mode only takes over completing-read (minibuffer), not in-buffer
  ;; completion-at-point, which otherwise falls back to the *Completions*
  ;; buffer popup (e.g. for the font-family widget completion done earlier). Route it
  ;; through consult so it also uses the minibuffer.
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args))))

;;; Local.el loading
(when (file-exists-p (locate-user-emacs-file "local.el"))
  (load-file (locate-user-emacs-file "local.el")))

;;; temptemp - try out new builtin completion.

(when (> emacs-major-version 30)
  (use-package completion-preview
    :ensure nil
    :demand t
    :bind
    ( :map completion-preview-active-mode-map
      ("M-i" . completion-preview-insert-word)
      ("M-n" . completion-preview-next-candidate)
      ("M-p" . completion-preview-prev-candidate)
      ("M-<RET>" . completion-preview-insert)
      ;; With TAB we effectively defer to the *Completions* buffer to
      ;; show more completion candidates at once.
      ("<tab>" . completion-preview-complete))
    :config
    (setq completion-preview-minimum-symbol-length 3)
    (with-eval-after-load 'org
      (add-to-list 'completion-preview-commands #'org-self-insert-command))
    (global-completion-preview-mode 1)))

;;; GC tuning
;; Adaptive GC pacing: keep `gc-cons-threshold' high while editing and only
;; collect when Emacs goes idle, instead of paying GC pauses during normal
;; typing. Picks up where early-init.el's startup-only threshold bump leaves
;; off.
(use-package gcmh
  :ensure t
  :init
  (setq gcmh-high-cons-threshold (* 32 1024 1024))
  (gcmh-mode 1))
