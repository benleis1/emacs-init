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

;; Emacs configuration file
;; Author: Benjamin Leis

;;; Commentary:
;;
;; ## Philosophy
;;
;; These are all the high level priorities that inform the decisions I've made throughout this file.
;; Unlike many other users who have shared their config files, I like using the mouse and even
;; the occasional menu rather than remembering key bindings for everything. So I've spent some time
;; trying to get emacs to work more consistently for these modes. For example with flyspell on you
;; can right click and get a context menu with the possible spellings like in most other applications.
;;
;; If possible I'll use built in functionality or packages that require minimal adaptation and
;; just a use-package declaration. If not I try to keep everything in one section by general
;; functionality area. Along these lines currently I prefer one larger file to a series of smaller
;; ones for both reading and modifying. This may shift in the future but currently I only move
;; code out to  a new file if it reaches a "sufficiently" large size.
;;
;; I have a work style where I want to have a manageable small set of files open in a tabbed format.
;; I'll save these to a desktop and reload them when I start things up again. I've plumbed
;; save/load desktop into the system menus and also extensively modified tab-line to fit my work flow.
;; Longterm if the need arises I plan to either integrate in bookmark+ or activities to
;; save related sets of these files. For now I have customized tab-line with "views" to
;; facilitate this. See tab-config.el for more details.
;;
;; Style-wise, I prefer a fairly minimal design theme. I'm currently using the folio theme which is
;; based on the builtin  modus-themes and have changed most faces to just  use the same default
;; foreground color or a bolder one for emphasis. I really only want color in critical locations.
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
;; ## Major modes configured
;; - Markdown
;; - Org
;; - Java
;; - Ediff
;; - Python (partly)

;;; Code:

;;; Package setup

;; Define an ignore macro that doesn't even evaluate the argument. This is useful for
;; display purposes when using elispdoc rather than commenting whole regions out.
(defmacro my-ignore (form))

;; Setup melpa as a repository.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(my-ignore (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))

;; use-package has been part of core emacs since version 29 so I now assume its ok to just require it.
(require 'use-package)

;; Legacy Emacs 29 setup for :vc so we can load directly from github for selected packages not in melpa.
(when (< emacs-major-version 30)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

;; Enable automatic package installation globally
(setq use-package-always-ensure t)

;; early on setup follow-symlinks to true for loaded files
(setq vc-follow-symlinks t)

;;; Customizations

;;
;; Color name redirection for use with custom faces
;; requires manual editing of custom-set-faces to use ` back tick operator.
;;

(defvar my-code-bright "goldenrod3")
(defvar my-code-dark "goldenrod4")

;; doom-themes provides the doom-solarized-light theme that custom.el enables
;; via `custom-enabled-themes'. It has to be installed and on `load-path'
;; *before* custom-file loads below.
(my-ignore (use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;;  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;;  (doom-themes-org-config)
  ))

;;; modus theme configuration.

;; These mostly global level changes make switching around easier between themes
;; They preserve the tabbing styling I use and mute the colors a bit.
;; The consequence of moving over to modus is the need to not generally customize faces in
;; custom.el.

;; Override all modus themes to use the background color from tab-line
;; This keeps visual parity with what I currently use
;; Make headers all the same color as foreground
(setq modus-themes-common-palette-overrides
      '((bg-tab-bar EEE8D5)
        (bg-tab-current bg-main)
        (bg-tab-other EEE8D5)

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
        (bg-prose-block-delimiter unspeficied)
        (fg-prose-block-delimiter fg-dim)

	;; paren-matching
	(fg-paren-match red)
	(bg-paren-match bg-red-intense)

	;; For folio - which oddly sets only these to blue with an overline
	(bg-heading-2 unspecified)
	(overline-heading-1 unspecified)
        (overline-heading-2 unspecified)

	))

;; enable fixed fonts for code and variable for text
(setq modus-themes-mixed-fonts t)

;; Tone down the cursor specifically for modus-operandi-tinted.
(setq modus-operandi-tinted-palette-overrides
      '((cursor "gray60")
	(comment fg-main)
	(keyword yellow-intense)
	(bg-completion bg-yellow-nuanced)
	))

;; Currently trying out the folio theme.
(use-package folio-theme
  :vc (:url "https://github.com/kn66/folio-theme.el"
            :rev :newest)
  :config
  (load-theme 'folio t))

;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html
;; All customizations are stored on the side in custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Basic Appearance and startup
;; A set of configurations related to display style that are not covered by customized faces and variables

;; Make things silently start.
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-buffer-choice nil
      initial-scratch-message "")

;; Initial major mode is text for new buffers
(setq-default major-mode 'text-mode)

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
    :load-path "/Users/benjamin.leis/.emacs.d/on-demand-scroll-bar"
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

;; WIP: Scroll only to the last line
;; this is still a bit buggy and not quite the right behavior.
(defun limit-scrolling (&optional win start)
  ;; handle case where buffer is totally empty
  (unless (= (buffer-size) 0)
    (let ((visible-lines (count-lines (or start (window-start)) (buffer-size)))
          (lines-to-end (count-lines (point) (buffer-size))))
      (when (< visible-lines (window-text-height))
	(progn
	  (recenter (- lines-to-end)))))))

;; Only install the limit scrolling hook on gui modes where scrolling is enabled
(when window-system
  (setq use-system-tooltips nil)
  (add-hook 'post-command-hook #'limit-scrolling))

;; Use winner mode by default for managing window configurations
;; particularly useful when popping up a 2nd or 3rd window and
;; wanting to go back to the previous config.
(winner-mode 1)

;; Generally remove trailing white space except on markdown
(defun my-before-save-hook ()
  (unless (equal major-mode 'markdown-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'my-before-save-hook)

;; Auto complete on tab if not at start of line in modes
;; where tab auto indents
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Switch buffer name context tip to actually be the buffer name
(setq-default mode-line-buffer-identification
              (list (propertize
                     "%12b"
                     'face 'mode-line-buffer-id
                     'help-echo
                     '(format "%s\nmouse-1: Previou2 buffer\nmouse-3: Next buffer"
                       (buffer-name))
                     'mouse-face 'mode-line-highlight
                     'local-map mode-line-buffer-identification-keymap)))

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
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; Setup recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Every 10 minutes up date the list since I usually either run the server or keep
;; the gui app open for long periods of time
(run-at-time nil 600 'recentf-save-list)

;;; backup and autosave - put everything in ~/.saves

;; Define a directory for auto-save files
(defvar my-auto-save-folder (concat user-emacs-directory ".saves"))

;; Ensure the directory exists
(unless (file-exists-p my-auto-save-folder)
  (make-directory my-auto-save-folder t))

(setq
 auto-save-default nil ;; disable auto save files
 auto-save-file-name-transforms `((".*" , my-auto-save-folder t))
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;; Dired

;; nerd icons setup.
;; These are used by doom-modeline and color adjustments need to be done prior to loading it
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

;; doom-modeline setup
;; Note: its important to have a nerd font installed for the icons to work properly
;; I usd DejaVu Sans Mono with the Nerd Font extension
;; For now I leave the icons on even in terminal mode although they are a bit too small there.
;; It has to be installed here before tab-config.el (loaded right below) uses its
;; `doom-modeline-def-segment'/`doom-modeline-def-modeline' macros at load time.
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; Load all of my custom tab-line config.
(load  (locate-user-emacs-file "tab-config.el"))

;;; Global key bindings
;; I try to keep this minimally different from stock emacs.
;; my preference is for short key strokes and to usually bind global things to
;; function keys.

(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-f") 'goto-line)
(global-set-key (kbd "C-1") 'treemacs)
(global-set-key (kbd "C-2") 'org-capture)
(global-set-key (kbd "C-\\") 'imenu-list-smart-toggle)
(global-set-key (kbd "C-<tab>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-S-<tab>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-3") 'wikimode-toggle)

;; I hit cmd-x too often expecting M-x which is dangerous so just bind it to that
;; TODO should I just bind cmd - to the meta key and give up up cmd-c and cmd-v?
(global-set-key (kbd "s-x") 'execute-extended-command)

;; Add standard minimal CUA  key bindings ctrl-c, ctrl-v insert paste etc.
;; TODO - I need ctrl-z to still be suspend
;;(cua-mode)

;; Copy to clipboard functions for terminal mode
;; copy the current region directly
(defun pbcopy-region ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

;; copy the latest kill ring
(defun pbcopy-kill-ring (&optional push)
  (interactive)
  (let ((process-connection-type nil)
	(text (current-kill 0)))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Final version hook into interprogram-cut-function instead
;; for terminal mode cut to system clipboard
(defun paste-for-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(unless window-system
  (setq interprogram-cut-function 'paste-for-osx))

;;; sql formatting setup for sqlformat-* functions.
(use-package sqlformat
  :defer t
  :ensure t
  :config (setq sqlformat-command 'pgformatter
                sqlformat-args '("-s2" "-g")))

;;; flyspell config
;; currently not bound to a key
(defun flyspell-on-for-buffer-type ()
      "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
      (interactive)
      (if (not (symbol-value flyspell-mode)) ; if not already on
        (progn
          (if (derived-mode-p 'prog-mode)
            (progn
              (flyspell-prog-mode))
            ;; else
            (progn
              (flyspell-mode 1)))
          )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
    ; else - flyspell is off, turn it on
    (flyspell-on-for-buffers-type)))

;; preset modes to have flyspell on
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Enable which key
(setq-default which-key-mode t)
(which-key-mode)

;;; org-mode
;; My typical usage of Org includes a main work tracking file, org-agenda, integration with my exchange calendar
;; and simple daily journal for which I have a capture template to add standup entries

;; mouse support
;; This is fairly expensive so we defer it until idle
(use-package emacs
  :defer 2
  :config
  (require 'org-mouse))

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; word wrap for normal text and stripe mode for tables
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hoom #'stripe-buffer-mode))

;; Use monospaced font faces in org mode - currently the hook is off
(defun org-mode-fonts ()
  "Sets up display fonts for org-mode"
  (interactive)
  (setq buffer-face-mode-face '(:family "TeX Gyre Pagella-13" :height 100))
  (buffer-face-mode))

;; Set default font faces for Info and ERC modes
;; (add-hook 'org-mode-hook 'org-mode-fonts)

;; set org-mode to use variable width fonts smartly
(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode))

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

;; increase line spacing
;; ignored currently because it looks bad with tables.
(my-ignore (add-hook 'org-mode-hook (lambda() (setq line-spacing 0.5))))

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
  :config (setq org-modern-table nil) )

;; Indent by heading depth
(setq org-startup-indented t)

;; Setup capture templates
;; currently only have one for standup summaries
(setq org-capture-templates
  '(    ;; ... other templates

    ("s" "Standup Entry"
         entry (file+datetree "~/org/standup.org")
         "* %?"
         :empty-lines 1)

        ;; ... other templates
    ))

;; Show up to 4 levels of org headings in the imenu and imenu-list
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
  :ensure t)

(use-package magit
  :ensure t)

;; Set display line number mode on
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Project.el settings.

;; I prefer to have project-switch-project to just change the project for the next project
;; command. bear in mind, project mostly uses the current directory of the buffer to
;; determine the project
(setq project-switch-commands 'project-any-command)
;; Set project boundary at the first build.gradle found as well
(setopt project-vc-extra-root-markers '("build.gradle"))

;; treesit-fold - useful for folding things like imports in java
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
                         ;; passing the node to `treesit-fold-close' directly.
                         (save-excursion
                           (goto-char pos)
                           (treesit-fold-close)))
                      :help "Fold the code at this line")))))
  menu)

(use-package treesit-fold
  :ensure t
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :config
  ;; Add a rule for java-mode and java-ts-mode to fold the whole run of imports at once
  (dolist (mode '(java-mode java-ts-mode))
    (push '(import_declaration . treesit-fold-range-java-imports)
          (alist-get mode treesit-fold-range-alist)))
  (dolist (mode '(java-mode java-ts-mode))
    (setf (alist-get mode treesit-fold-summary-parsers-alist)
          #'treesit-fold-summary-java))
  (add-hook 'context-menu-functions #'context-menu-fold-line))

;;; Java

;; set java home
(setq java-home "/Users/benjamin.leis/.jenv/versions/21.0")
(setenv "JAVA_HOME" java-home)

(defun setup-common-java ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t)
  (setq-local imenu-depth 3)
  (setq-local imenu-create-index-function 'my/generate-ts-imenu)
  (treesit-fold-mode)
  (treesit-fold-close-java-imports))

(add-hook 'java-mode-hook 'setup-common-java)
(add-hook 'java-ts-mode-hook 'setup-common-java)

; Setup automatic mode remapping so we always use treesitter for java
(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)))

(setq dap-java-java-command (concat java-home "/bin/java"))

;;; eglot

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

;; Pin the java version for JDT. Set it here and after the load.
(setq my-jdtls-settings
      `(:java (
	       :home ,java-home

		     )))

(setq-default eglot-workspace-configuration my-jdtls-settings)

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

;;; flymake

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

(defvar-keymap flymake-buffer-id-keymap
  "<mode-line> <mouse-1>" #'flymake-buffer-quit)

(add-hook 'flymake-diagnostics-buffer-mode-hook
          (lambda ()
            (setq-local mode-line-buffer-identification-keymap
                        flymake-buffer-id-keymap)))

;; Save space by not showing zero warn/error counter in the mode line
(setq flymake-suppress-zero-counters t)

;;; python - TODO turn on eglot integration later.

;;; markdown mode

(use-package markdown-mode
   :ensure t)

(use-package stripe-buffer
   :ensure t)

(setq markdown-header-scaling t)
;; Use helvetica for the current mode when hooked
(defun buffer-face-mode-helvetica ()
  "Set default font to helvetica in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "helvetica" :height 180))
  (buffer-face-mode))

;; render remote images
(setq markdown-display-remote-images t)

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

(setq markdown-translate-filename-function #'my-markdown-translate-filename-add-md-extension)

(add-hook 'markdown-mode-hook 'buffer-face-mode-helvetica)
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
(add-hook 'orgmode-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)

;; Note: C-\ is bound to smart toggle.
(use-package imenu-list
  :ensure t
  :config
  ;; Some built in default around resizing and window focus I prefer.
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil)

  ;; doom-modeline renders its icons at `doom-modeline-icon-scale-factor'
  ;; This invisible spacer forces the same line height here so the
  ;; imenu-list mode-line matches the height of the main buffers' doom-modeline.
  (defvar my-imenu-list-mode-line-height-spacer
    (propertize " " 'display '(height 1.3))
    "Zero-effect text used only to match doom-modeline's line height.")

  ;; Simplified buffer name with icon for the menu bar.
  (setq imenu-list-mode-line-format
	`("%e" mode-line-frame-identification
	  ,my-imenu-list-mode-line-height-spacer
	  (:propertize "󰉹" face mode-line-buffer-id) " "
	  (:eval (buffer-name imenu-list--displayed-buffer)) "  "
	  (:eval (format "[%s]" (my/imenu-current-sort imenu-list--displayed-buffer))) "  "
	  mode-line-end-spaces))

  (defvar imenu-depth 2 "Initial depth to expand imenu-ilist window")

  ;; Track whether we autofolded per buffer.
  (defvar-local imenu-list--folded-once nil
    "`my-imenu-list-fold-below-depth' has folded this buffer's imenu-list.")

  (defconst my-imenu-list-collapsed-marker "▶"
    "Marker shown before a folded (hidden) imenu-list entry.")

  (defconst my-imenu-list-expanded-marker "▼"
    "Marker shown before an unfolded (visible) imenu-list entry.")

  ;; I need a more visible highlight for the current block
  (defface my-hl-imenu-face
  '((t (:foreground "ivory" :background "DarkOrange2" :weight bold)))
  "A new custom face for highlighting."
  :group 'my-custom-group)

  (defun my-imenu-list--hide-ellipsis (ov)
    "Suppress hideshow's default \"...\" indicator on OV.
The leading arrow marker already conveys fold state, so the ellipsis
would just be redundant clutter."
    (when (eq (overlay-get ov 'invisible) 'hs)
      (overlay-put ov 'display "")))

  ;; Hook for setup of the mode,
  (add-hook 'imenu-list-major-mode-hook
            (lambda ()
              ;; Wire in my custom highlight face.
              (setq-local face-remapping-alist '((hl-line my-hl-imenu-face)))
              ;; High enough priority for this face so it takes precedence
              ;; unlike normal I don't want to preserve the underlying foreground color
              (setq-local hl-line-overlay-priority 10)
              ;; Wire in the ellipsis twiddling.
              (setq-local hs-set-up-overlay #'my-imenu-list--hide-ellipsis)))

  (defun my-imenu-list-fold-below-depth (&optional depth)
    "Collapse imenu-list entries nested deeper than DEPTH (default `imenu-depth'). Top-level entries are depth 1."
    (interactive)
    (let ((depth (or depth imenu-depth)))
      (with-current-buffer imenu-list-buffer-name
        (save-excursion
          (goto-char (+ 1 (point-min)))
          (hs-hide-level depth)))))

    (defun my-imenu-list-fold-below-depth-once (&optional depth)
    "Run default folding once per buffer, then refresh fold markers."
    (unless imenu-list--folded-once
      (setq imenu-list--folded-once t)
      (my-imenu-list-fold-below-depth depth))
    (my-imenu-list-update-fold-markers))

  (add-hook 'imenu-list-update-hook #'my-imenu-list-fold-below-depth-once)

  (defun my-imenu-list--set-marker-at-point ()
    "Make the fold marker on the current line display as an arrow reflecting whether the block starting here is currently hidden."
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^ *\\(\\+\\) ")
        (let ((inhibit-read-only t))
          (put-text-property (match-beginning 1) (match-end 1)
                              'display
                              (if (hs-already-hidden-p)
                                  my-imenu-list-collapsed-marker
                                my-imenu-list-expanded-marker))))))

  (defun my-imenu-list-update-fold-markers ()
    "Update every foldable entry's marker in the *Ilist* buffer to match its current hidden/shown state."
    (when (get-buffer imenu-list-buffer-name)
      (with-current-buffer imenu-list-buffer-name
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (my-imenu-list--set-marker-at-point)
            (forward-line 1))))))

  ;; Apply the arrow overlays when manually adjusting folded sections
  (defun my-imenu-list--refresh-marker-after-toggle (&rest _)
      (when (eq major-mode 'imenu-list-major-mode)
       (my-imenu-list--set-marker-at-point)))

  (advice-add 'hs-toggle-hiding :after #'my-imenu-list--refresh-marker-after-toggle)

  ;; Refold
  (defun my-after-imenu-list-toggle (&rest args)
    "Run custom code after `imenu-list-smart-toggle` occurs."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (local-variable-p 'imenu-list--folded-once)
	  (setq imenu-list--folded-once nil)))))

  (advice-add 'imenu-list-smart-toggle :before #'my-after-imenu-list-toggle)

  ;; When the tracked entry is inside a currently-folded block, `hl-line-mode'
  ;; highlights the (invisible) entry line, which visually collapses to just
  ;; the fold ellipsis at the end of the header line.  Move point up to the
  ;; visible header line instead so the highlight bar actually shows.
  (defun my-imenu-list-reveal-current-entry (&rest _)
    (when (get-buffer-window imenu-list-buffer-name)
      (with-selected-window (get-buffer-window imenu-list-buffer-name)
        (when (invisible-p (point))
          (goto-char (previous-single-char-property-change (point) 'invisible))
          (beginning-of-line)
          (hl-line-highlight)))))

  (advice-add 'imenu-list--show-current-entry :after #'my-imenu-list-reveal-current-entry))

;; Load all of my custom imenu extensions.
(load-file (locate-user-emacs-file "imenu.el"))

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

;; WIP - add zoom in/out to buffer menu
;; TODO get the keybinding message straight?
;; also add to context menu?
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


;;; elisp

;; Group `use-package' declarations under their own imenu heading --
;; the default `lisp-imenu-generic-expression' only covers def* forms,
;; so use-package calls otherwise don't show up at all.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'imenu-generic-expression
                          (list "Use-package"
                                (concat "^\\s-*(use-package\\s-+\\("
                                        lisp-mode-symbol-regexp "\\)")
                                1))))

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
	;;setq excorporate-org-buffer-name "~/org/daily-meetings.org"
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
(defun my-agenda-update-diary (&optional args)
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

;; Capture window state and turn off doom mode line
(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-buffers '())
  (setq my-ediff-bwin-config (current-window-configuration))
;;  (setq my-ediff-linenum-state (bound-and-true-p display-line-number-mode))
  (doom-modeline-mode -1))

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
  (doom-modeline-mode 1)
  (dolist (element my-ediff-buffers)
    (my-restore-buffer-state element))
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
(add-hook 'ediff-prepare-buffer-hook 'my-ediff-prep-buffers 'append)

;; side by side comparison layout
(setq ediff-split-window-function 'split-window-horizontally)

;; todo this doesn't work with emacsclient only gemacs
;; Setup a command line switch gemacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; Keep the control window in the default frame
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; experiment with widgets in the control frame
(defun ediff-add-buttons ()
  (message "setting up buttons")

  (widget-create 'push-button
                 :tag "next"
                 :help-echo "Ediff next"
                 :tag-glyph "fwd-arrow"
                 :action (lambda (widget &optional event)
                           (ediff-next-difference)))
  (widget-create 'push-button
                 :tag "prev"
                 :help-echo "Ediff previous"
                 :tag-glyph "back-arrow"
                 :action (lambda (widget &optional event)
                           (ediff-previous-difference)))

  (widget-create 'push-button
                 :tag "quit"
                 :help-echo "Ediff quit"
                 :tag-glyph "exit"
                 :action (lambda (widget &optional event)
                           (ediff-quit nil)))
					;(ediff-previous-difference)))

  (widget-setup))

;; Adds widgets but they they don't work yet - probably need to set the keymap
;; also I still want to type text in the box and have it work
;;(add-hook 'ediff-mode-hook 'ediff-add-buttons)

(my-ignore
((require 'wid-edit)
(set-keymap-parent ediff-mode-map widget-keymap)))

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
  )

;;
;; Font name completion for customize buffers
;;

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
    `(,(point-at-bol) ,(point-at-eol) ,(font-family-list))))

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

;; Yanippet used defines some markdown templates for the blog
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (add-hook 'markdown-mode-hook #'yas-minor-mode)
  )

(use-package wikimode
  :ensure t
  :vc (:url "https://github.com/benleis1/wikimode")
  )

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
