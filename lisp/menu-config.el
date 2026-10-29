;;; menu-config.el --- Menu bar reorganization, adapted from anju -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Reorganizes six parts of the menu bar, motivated by the principles from kickingvegas/anju:
;;
;; * File: drops Visit New File, Insert File and Recover Crashed Session, makes Open File… (always
;;   the minibuffer, never the GUI dialog) the top entry, and replaces the recentf Open Recent
;;   submenu with a consult-recent-file entry.
;; * Windows: renames Buffers to Windows and orders it windows, buffers, frames, text size, taking
;;   the window and frame commands out of File, dropping the tab commands and the buffer list, and
;;   using consult-buffer for Switch to Buffer.
;; * Help: replaced wholesale with a short menu (manual in a new frame, documentation search,
;;   describe commands, tutorial, bug report) rather than patched entry by entry.
;; * Edit: groups scattered editing commands into Select, Rectangle, Registers, Transpose,
;;   Whitespace, Lines, Delete and Transform Text submenus, adds Duplicate, Align Regexp, Indent Region and
;;   Comment/Uncomment, and points file search at consult-ripgrep.
;; * Text: replaces the flat centering items with Center and Fill submenus.
;; * Tools: removes Games, mail, news, web browsing, encryption, EDE and Semantic entries, replaces
;;   Grep and project search with consult-ripgrep, and adds a ghostel terminal entry.
;;
;; # Screenshots
;;
;; <table>
;; <tr>
;; <td valign="top">
;; <img src="../images/filemenu.png" width="100%" />
;; </td>
;; <td valign="top">
;; <img src="../images/editmenu.png" width="100%" />
;; </td>
;; <td valign="top">
;; <img src="../images/windowmenu.png" width="100%" />
;; </td>
;; <td valign="top">
;; <img src="../images/toolmenu.png" width="100%" />
;; </td>
;; <td valign="top">
;; <img src="../images/helpmenu.png" width="100%" />
;; </td>
;; </tr>
;; </table>
;;
;; # Requirements
;; Requires Emacs 29.1 or later (`duplicate-dwim', `undelete-frame-mode', `project-list-buffers').
;; Developed and tested on Emacs 31; the stock menu keys it edits are checked by
;; menu-config-tests.el.
;;
;; consult and ghostel are optional.  Each replacement is applied only if its command is `fboundp'
;; when `my-menu-config' runs (so load them first), and otherwise the stock entry is left alone:
;;
;; * `consult-recent-file' missing: the stock Open Recent submenu stays.
;; * `consult-buffer' missing: Switch to Buffer uses `switch-to-buffer'.
;; * `consult-ripgrep' missing: Search in Files, Grep and project search stay stock.
;; * `ghostel' missing: no terminal entry is added (the stock Shell Commands submenu remains).
;;
;; Each piece is a separate function so it can be disabled by not calling it.  `my-menu-config'
;; applies all of them and is called once from init.el.

;;; Code:

(require 'easymenu)
(require 'text-mode)

(defun my-menu--command-in-new-frame (command &optional arg)
  "Run COMMAND in a newly created frame.
If ARG is non-nil, call COMMAND with ARG as its single argument;
otherwise call it interactively."
  (select-frame-set-input-focus (make-frame))
  (if arg
      (funcall command arg)
    (call-interactively command)))

(defun my-menu-emacs-manual-in-new-frame ()
  "Show the Emacs manual in a new frame."
  (interactive)
  (my-menu--command-in-new-frame #'info-display-manual "emacs")
  (tab-line-mode -1))

;;; Help

(easy-menu-define my-help-menu nil
  "Replacement for the stock Help menu."
  '("Help"
    ["Open the Emacs Manual" my-menu-emacs-manual-in-new-frame
     :help "Show the Emacs manual in a new frame"]
    ("Search Documentation"
     ["Emacs Terminology" search-emacs-glossary
      :help "Display the Glossary section of the Emacs manual"]
     ["Look Up Subject in User Manual…" emacs-index-search
      :help "Find description of a subject in Emacs User manual"]
     ["Look Up Subject in Elisp Manual…" elisp-index-search
      :help "Find description of a subject in Emacs Lisp manual"]
     ["Look Up Key in User Manual…" Info-goto-emacs-key-command-node
      :help "Display manual section that describes a key"]
     ["Look Up Command in User Manual…" Info-goto-emacs-command-node
      :help "Display manual section that describes a command"]
     ["Look Up Symbol in Manual…" info-lookup-symbol
      :help "Display manual section that describes a symbol"]
     "--"
     ["Find Commands by Name…" apropos-command
      :help "Find commands whose names match a regexp"]
     ["Find Options by Name…" apropos-user-option
      :help "Find user options whose names match a regexp"]
     ["Find Options by Value…" apropos-value
      :help "Find variables whose values match a regexp"]
     ["Find Any Object by Name…" apropos
      :help "Find symbols of any kind whose names match a regexp"]
     ["Search Documentation Strings…" apropos-documentation
      :help "Find functions and variables whose doc strings match a regexp"])
    ("Describe"
     ["Describe Buffer Modes" describe-mode
      :help "Describe this buffer's major and minor mode"]
     ["Describe Key or Mouse Operation…" describe-key
      :help "Display documentation of command bound to a key, a click, or a menu item"]
     ["Function Group Overview…" shortdoc-display-group
      :help "Display a function overview for a specific topic"]
     ["Describe Command…" describe-command :help "Display documentation of command"]
     ["Describe Function…" describe-function :help "Display documentation of function/command"]
     ["Describe Variable…" describe-variable :help "Display documentation of variable/option"]
     ["Describe Face…" describe-face :help "Display the properties of a face"]
     ["Describe Package…" describe-package :help "Display documentation of a Lisp package"]
     ["Describe Display Table" describe-current-display-table
      :help "Describe the current display table"]
     ["Show Recent Inputs" view-lossage
      :help "Display last few input events and the commands they ran"]
     ["List Key Bindings" describe-bindings
      :help "Display all current key bindings (keyboard shortcuts)"]
     "--"
     ["Describe Language Environment…" describe-language-environment
      :help "Describe a language environment"]
     ["Describe Input Method…" describe-input-method
      :help "Keyboard layout for specific input method"]
     ["Describe Coding System…" describe-coding-system]
     ["Describe Coding System (Briefly)" describe-current-coding-system-briefly]
     ["Show All of Mule Status" mule-diag
      :help "Display multilingual environment settings"])
    ["Search Built-in Packages" finder-by-keyword
     :help "Find built-in packages and features by keyword"]
    "--"
    ("More Info"
     ["Emacs FAQ" view-emacs-FAQ :help "View the Emacs FAQ"]
     ["Emacs News" view-emacs-news :help "View news about this Emacs release"]
     ["Emacs Known Problems" view-emacs-problems :help "View known problems of this Emacs"]
     ["Library Commentary…" finder-commentary
      :help "Show the commentary of an Emacs Lisp library"])
    ["Emacs Tutorial" help-with-tutorial :help "Learn how to use Emacs"]
    ["Send Bug Report…" report-emacs-bug :help "Send an Emacs bug report"]
    "--"
    ["About Emacs" about-emacs
     :help "Display version number, copyright info, and basic help"]))

(defun my-menu-reconfigure-help ()
  "Replace the contents of the stock Help menu with `'#my-help-menu'.
The keymap `menu-bar-help-menu' is modified in place, so packages that
add entries to it still reach the visible menu."
  (setcdr menu-bar-help-menu (cdr my-help-menu)))

;;; Edit

(easy-menu-define my-rectangle-menu nil
  "Rectangle submenu of the Edit menu."
  '("Rectangle"
    ["Cut" kill-rectangle :enable (and (use-region-p) (not buffer-read-only))
     :help "Kill the rectangle"]
    ["Copy" copy-rectangle-as-kill :enable (use-region-p)
     :help "Copy the rectangle to the kill ring"]
    ["Paste" yank-rectangle :enable (not buffer-read-only)
     :help "Yank the last killed rectangle at point"]
    ["Delete" delete-rectangle :enable (and (use-region-p) (not buffer-read-only))
     :help "Delete the rectangle without saving it"]
    "--"
    ["Replace…" string-rectangle :enable (and (use-region-p) (not buffer-read-only))
     :help "Replace each line of the rectangle with a string"]
    ["Insert…" string-insert-rectangle :enable (and (use-region-p) (not buffer-read-only))
     :help "Insert a string on each line of the rectangle"]
    ["Number" rectangle-number-lines :enable (and (use-region-p) (not buffer-read-only))
     :help "Number the lines of the rectangle"]
    ["Fill with Spaces" clear-rectangle :enable (and (use-region-p) (not buffer-read-only))
     :help "Replace the rectangle with spaces"]
    ["Insert Blank Space" open-rectangle :enable (and (use-region-p) (not buffer-read-only))
     :help "Blank the rectangle, shifting text right"]
    ["Delete Leading Spaces" delete-whitespace-rectangle
     :enable (and (use-region-p) (not buffer-read-only))
     :help "Delete whitespace in each line of the rectangle"]))

(easy-menu-define my-registers-menu nil
  "Registers submenu of the Edit menu."
  '("Registers"
    ["Copy Region to Register…" copy-to-register :enable (use-region-p)
     :help "Save the region in a register"]
    ["Append Region to Register…" append-to-register :enable (use-region-p)
     :help "Append the region to the text in a register"]
    ["Prepend Region to Register…" prepend-to-register :enable (use-region-p)
     :help "Prepend the region to the text in a register"]
    ["Copy Rectangle to Register…" copy-rectangle-to-register :enable (use-region-p)
     :help "Save the rectangle in a register"]
    ["Insert Register…" insert-register :enable (not buffer-read-only)
     :help "Insert the contents of a register at point"]
    "--"
    ["Save Point to Register…" point-to-register :help "Save point in a register"]
    ["Save Window Configuration to Register…" window-configuration-to-register
     :help "Save the current window configuration in a register"]
    ["Save Frame Set to Register…" frameset-to-register
     :help "Save the state of all frames in a register"]
    ["Jump to Register…" jump-to-register
     :help "Go to the position, window configuration or frame set in a register"]
    "--"
    ["Store Number in Register…" number-to-register :help "Store a number in a register"]
    ["Increment Register…" increment-register
     :help "Add a number to the number in a register, or append the region to its text"]
    "--"
    ["View Register…" view-register :help "Display the contents of a register"]
    ["List Registers" list-registers :help "Display a list of all nonempty registers"]))

(easy-menu-define my-transpose-menu nil
  "Transpose submenu of the Edit menu."
  '("Transpose"
    :visible (not buffer-read-only)
    ["Characters" transpose-chars :help "Swap the characters around point"]
    ["Words" transpose-words :help "Swap the words around point"]
    ["Lines" transpose-lines :help "Swap the current line with the previous one"]
    ["Sentences" transpose-sentences :help "Swap the sentences around point"]
    ["Paragraphs" transpose-paragraphs :help "Swap the paragraphs around point"]
    ["Regions" transpose-regions :help "Swap two regions of text"]
    ["Balanced Expressions" transpose-sexps :help "Swap the balanced expressions around point"]))

(easy-menu-define my-whitespace-menu nil
  "Whitespace submenu of the Edit menu."
  '("Whitespace"
    :visible (not buffer-read-only)
    ["Join Line" join-line :help "Join this line to the previous one"]
    ["Just One Space" just-one-space :help "Reduce surrounding whitespace to one space"]
    ["Horizontal Space" delete-horizontal-space :help "Delete spaces and tabs around point"]
    "--"
    ["Blank Lines" delete-blank-lines :help "Delete blank lines around point"]
    ["Trailing Whitespace" delete-trailing-whitespace
     :help "Delete trailing whitespace in the buffer or region"]
    ["Whitespace Cleanup" whitespace-cleanup :help "Clean up whitespace problems in the buffer"]))

(easy-menu-define my-lines-menu nil
  "Lines submenu of the Edit menu."
  '("Lines"
    :visible (not buffer-read-only)
    ["Flush Lines…" flush-lines :help "Delete lines containing matches for a regexp"]
    ["Keep Lines…" keep-lines
     :help "Delete all lines except those containing matches for a regexp"]
    ["Delete Duplicate Lines" delete-duplicate-lines :enable (use-region-p)
     :help "Delete duplicate lines in the region"]
    "--"
    ["Sort Lines" sort-lines :enable (use-region-p) :help "Sort the lines in the region"]
    ["Sort Paragraphs" sort-paragraphs :enable (use-region-p)
     :help "Sort the paragraphs in the region"]
    ["Reverse Lines" reverse-region :enable (use-region-p)
     :help "Reverse the order of the lines in the region"]))

(easy-menu-define my-select-menu nil
  "Select submenu of the Edit menu."
  '("Select"
    ["Word" mark-word :help "Mark the word at point"]
    ["Paragraph" mark-paragraph :help "Mark the paragraph at point"]))

(easy-menu-define my-delete-menu nil
  "Delete submenu of the Edit menu."
  '("Delete"
    :visible (not buffer-read-only)
    ["Pair" delete-pair :help "Delete a balanced pair of delimiters following point"]
    ["Zap up to…" zap-up-to-char
     :help "Kill up to, but not including, the next occurrence of a character"]
    ["Zap to…" zap-to-char :help "Kill up to and including the next occurrence of a character"]))

(easy-menu-define my-transform-text-menu nil
  "Transform Text submenu of the Edit menu."
  '("Transform Text"
    :visible (not buffer-read-only)
    ["Make Upper Case" upcase-region :enable (use-region-p)]
    ["Make Lower Case" downcase-region :enable (use-region-p)]
    ["Capitalize" capitalize-region :enable (use-region-p)]))

(defun my-menu-reconfigure-edit ()
  "Add grouped editing submenus and line commands to the Edit menu.
The stock Fill item is hidden in `text-mode' buffers, where the Text
menu has a Fill submenu. Search in Files uses `consult-ripgrep' if
available, else the stock entry stays."
  (easy-menu-add-item global-map '(menu-bar edit)
                      [duplicate-dwim duplicate-dwim
                                      :label "Duplicate"
                                      :help "Duplicate the current line or region"
                                      :visible (not buffer-read-only)]
                      'mark-whole-buffer)
  (easy-menu-add-item global-map '(menu-bar edit) my-select-menu 'separator-search)
  (when (fboundp 'consult-ripgrep)
    (define-key global-map [menu-bar edit search project-search]
                '(menu-item "Search in Files…" consult-ripgrep
                            :help "Search files in the project or current directory with ripgrep")))
  ;; Only NS builds have a Spell entry in the Edit menu; elsewhere anchor on the entry that follows it.
  (let ((spell-anchor (if (assq 'spell (cdr (lookup-key global-map [menu-bar edit])))
                          'spell
                        'execute-extended-command)))
    (dolist (item
             (list my-rectangle-menu
                   my-registers-menu
                   my-transpose-menu
                   my-whitespace-menu
                   my-lines-menu
                   my-delete-menu
                   "--"
                   my-transform-text-menu
                   [align-regexp align-regexp
                                 :label "Align Regexp…"
                                 :help "Align the region using a rule read from the minibuffer"
                                 :enable (use-region-p)
                                 :visible (not buffer-read-only)]
                   [indent-region indent-region
                                  :label "Indent Region"
                                  :help "Indent each nonblank line in the region"
                                  :enable (use-region-p)
                                  :visible (not buffer-read-only)]
                   [comment-dwim comment-dwim
                                 :label "Comment/Uncomment"
                                 :help "Comment or uncomment the region or the current line"
                                 :visible (not buffer-read-only)]
                   "--"))
      (easy-menu-add-item global-map '(menu-bar edit) item spell-anchor)))
  (define-key global-map [menu-bar edit fill]
              '(menu-item "Fill" fill-region
                          :enable (and mark-active (not buffer-read-only))
                          :visible (not (derived-mode-p 'text-mode))
                          :help "Fill text in region to fit between left and right margin")))

;;; Text

(easy-menu-define my-center-text-menu nil
  "Center submenu of the Text menu."
  '("Center"
    ["Line" center-line]
    ["Region" center-region :enable (use-region-p)]
    ["Paragraph" center-paragraph]))

(easy-menu-define my-fill-text-menu nil
  "Fill submenu of the Text menu."
  '("Fill"
    ["Paragraph" fill-paragraph]
    ["Paragraph ⤶" fill-paragraph-semlf :visible (fboundp 'fill-paragraph-semlf)]
    ["Unfill Paragraph" unfill-paragraph :visible (fboundp 'unfill-paragraph)]
    "--"
    ["Region" fill-region :enable (use-region-p)]
    ["Region as Paragraph" fill-region-as-paragraph :enable (use-region-p)]
    ["Region as Paragraph ⤶" fill-region-as-paragraph-semlf
     :enable (use-region-p) :visible (fboundp 'fill-region-as-paragraph-semlf)]
    "--"
    ["Individual Paragraphs" fill-individual-paragraphs :enable (use-region-p)]
    ["Non-uniform Paragraphs" fill-nonuniform-paragraphs :enable (use-region-p)]))

(defun my-menu-reconfigure-text ()
  "Replace the flat centering items of the Text menu with submenus."
  (dolist (label '("Center Line" "Center Region" "Center Paragraph" "Paragraph Indent"))
    (easy-menu-remove-item text-mode-menu nil label))
  ;; Separators are stored under a nil key, which `easy-menu-remove-item' cannot match.  The stock
  ;; one only separated the removed entries, so drop it and add one above Auto Fill.
  (setcdr text-mode-menu (assq-delete-all nil (cdr text-mode-menu)))
  (dolist (item (list my-center-text-menu my-fill-text-menu "--"))
    (easy-menu-add-item text-mode-menu nil item "Auto Fill")))

;;; Tools

(defun my-menu-reconfigure-tools ()
  "Remove mail, news, encryption, EDE, Semantic and Games from Tools.
If `consult-ripgrep' is available, also replace Recursive Grep, Grep and
project search with it. If `ghostel' is available, add a terminal entry."
  (dolist (key '(games separator-games gnus rmail compose-mail directory-search
                       browse-web separator-net encryption-decryption
                       separator-encryption-decryption ede semantic))
    (define-key global-map (vector 'menu-bar 'tools key) nil t))
  (when (fboundp 'consult-ripgrep)
    (define-key global-map [menu-bar tools rgrep] nil t)
    (define-key global-map [menu-bar tools grep]
                '(menu-item "Search Files (Ripgrep)…" consult-ripgrep
                            :help "Search files under a directory with ripgrep"))
    (define-key global-map [menu-bar tools project project-or-external-find-regexp] nil t)
    (define-key global-map [menu-bar tools project project-find-regexp]
                '(menu-item "Search Project (Ripgrep)…" consult-ripgrep
                            :help "Search files in the current project with ripgrep")))
  (when (fboundp 'ghostel)
    (easy-menu-add-item global-map '(menu-bar tools)
                        [ghostel ghostel
                                 :label "Terminal (Ghostel)"
                                 :help "Start a ghostel terminal"]
                        'shell-commands)))

;;; File

(defun my-menu-find-file ()
  "Run `find-file' with the minibuffer, not the native GUI file dialog."
  (interactive)
  (let ((use-file-dialog nil)
        (use-dialog-box nil))
    (call-interactively #'find-file)))

(defun my-menu-reconfigure-file ()
  "Reconfig the File menu.
Drop Visit New File, Insert File and Recover Crashed Session; put Open
File on top. If `consult-recent-file' is available, replace the recentf
Open Recent submenu with a minibuffer Open Recent entry; otherwise the
stock submenu stays."
  (dolist (key '(new-file insert-file recover-session))
    (define-key global-map (vector 'menu-bar 'file key) nil t))
  (when (fboundp 'consult-recent-file)
    (define-key global-map [menu-bar file Open\ Recent] nil t)
    (easy-menu-add-item global-map '(menu-bar file)
                        [open-recent consult-recent-file
                                     :label "Open Recent…"
                                     :help "Pick a recently opened file"]
                        'project-open-file))
  (define-key global-map [menu-bar file open-file]
              '(menu-item "Open File…" my-menu-find-file
                          :help "Read an existing file or create a new one into an Emacs buffer"))
  (let* ((file-map (lookup-key global-map [menu-bar file]))
         (entry (assq 'open-file file-map))
         (head (if (stringp (cadr file-map)) (cdr file-map) file-map)))
    (when entry
      (setcdr file-map (delq entry (cdr file-map)))
      (setcdr head (cons entry (cdr head)))))
  (dolist (key '(separator-window separator-tab separator-frame))
    (define-key global-map (vector 'menu-bar 'file key) nil t)))

;;; Windows

(defvar my-menu--windows-groups nil
  "Window and frame entries lifted out of the File menu.
Stored as an alist of (GROUP . ENTRIES).")

(defun my-menu--windows-buffer-entries ()
  "Return the buffer entries of the Windows menu.
Switch to Buffer uses `consult-buffer' if available, else `switch-to-buffer'."
  `((select-named-buffer menu-item "Switch to Buffer…"
                         ,(if (fboundp 'consult-buffer) 'consult-buffer 'switch-to-buffer)
                         :help "Pick another buffer to make current")
    (next-buffer menu-item "Next Buffer" next-buffer
                 :help "Switch to the \"next\" buffer in a cyclic order")
    (previous-buffer menu-item "Previous Buffer" previous-buffer
                     :help "Switch to the \"previous\" buffer in a cyclic order")
    (select-buffer-in-project menu-item "Select Buffer In Project…" project-switch-to-buffer
                              :help "Switch to a buffer belonging to the current project")
    (list-all-buffers menu-item "List All Buffers" list-buffers
                      :help "Pop up a window listing all Emacs buffers")
    (list-buffers-in-project menu-item "List Buffers In Project…" project-list-buffers
                             :help "Pop up a window listing the current project's buffers")))

(defconst my-menu--windows-text-size-entries
  '((zoom-in menu-item "Increase Text Size" text-scale-increase
             :help "Increase the font size of the text in this buffer")
    (zoom-out menu-item "Decrease Text Size" text-scale-decrease
              :help "Decrease the font size of the text in this buffer"))
  "Text size entries of the Windows menu.")

(defconst my-menu--windows-extra-entries
  '((delete-window menu-item "Close Window" delete-window
                   :help "Delete the selected window")
    (other-window menu-item "Other Window" other-window
                  :help "Select the next window"))
  "Window entries added to the Windows menu after those taken from File.")

(defun my-menu--take-file-entries (keys)
  "Remove the File menu entries named by KEYS and return them in that order."
  (let ((file-map (lookup-key global-map [menu-bar file])))
    (delq nil (mapcar (lambda (key)
                        (prog1 (assq key (cdr file-map))
                          (define-key global-map (vector 'menu-bar 'file key) nil t)))
                      keys))))

(defun my-menu--rebuild-windows-menu (&rest _)
  "Lay out the Windows menu: windows, buffers, frames, then text size.
The list of buffers is dropped, and the Frames submenu that Emacs adds when
several frames exist goes at the end of the frames group."
  (let* ((frames (assq 'frames (cdr global-buffers-menu-map)))
         (groups (list (append (cdr (assq 'windows my-menu--windows-groups))
                               my-menu--windows-extra-entries)
                       (my-menu--windows-buffer-entries)
                       (append (cdr (assq 'frames my-menu--windows-groups))
                               (and frames (list frames)))
                       my-menu--windows-text-size-entries))
         (entries nil)
         (n 0))
    (dolist (group groups)
      (when group
        (when entries
          (setq n (1+ n))
          (setq entries (append entries
                                (list (list (intern (format "my-windows-separator-%d" n)) "--")))))
        (setq entries (append entries group))))
    (setcdr global-buffers-menu-map (cons "Windows" entries))))

(defun my-menu-reconfigure-windows ()
  "Rename the Buffers menu to Windows.
Place window, tab and frame commands there."
  (setq my-menu--windows-groups
        (list (cons 'windows (my-menu--take-file-entries
                              '(new-window-below new-window-on-right one-window)))
              (cons 'frames (my-menu--take-file-entries
                             '(make-frame make-frame-on-display make-frame-on-monitor
                                          delete-this-frame undelete-last-deleted-frame
                                          undelete-frame-mode)))))
  (my-menu--take-file-entries '(make-tab close-tab))
  (define-key global-map [menu-bar buffer] (cons "Windows" global-buffers-menu-map))
  (advice-add 'menu-bar-update-buffers :after #'my-menu--rebuild-windows-menu)
  (menu-bar-update-buffers t))

;;;###autoload
(defun my-menu-config ()
  "Apply every menu bar reorganization in this file."
  (my-menu-reconfigure-file)
  (my-menu-reconfigure-windows)
  (my-menu-reconfigure-help)
  (my-menu-reconfigure-edit)
  (my-menu-reconfigure-text)
  (my-menu-reconfigure-tools))

(provide 'menu-config)
;;; menu-config.el ends here
