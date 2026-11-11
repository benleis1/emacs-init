tab

```
| |_ __ _| |__         ___ ___  _ __  / _(_) __ _
| __/ _` | '_ \ _____ / __/ _ \| '_ \| |_| |/ _` |
| || (_| | |_) |_____| (_| (_) | | | |  _| | (_| |
 \__\__,_|_.__/       \___\___/|_| |_|_| |_|\__, |
                                            |___/
```
Commentary:

tab-line configuration
Originally these changes were aimed at making tabs operate similarly to the tabs in the browser
or terminal.

I use tab-line rather than tab-bar because I want tabs in the "main" windows and not on the
sidebar ones and I'm buffer oriented rather than workspace oriented

I then expanded support for the grouping modes to include filtering by file backed buffers
or project and moved to a button oriented UI.

This allowed an extension to create views similar to tab-bar that store isolated buffer lists
and window configurations.

Because I might want to turn this into a package I'm going to use the tab-plus prefix for all
functions that I add on. You'll also see original tab-line vars and funcs referenced.

I've borrowed and modified some code from Nicolas Rougier to gather input on the header line
which is less of a distracting jump away from the tabs when needed.

Recently I've completed view support integration with desktop saves and a first non-buffer
integration for ghostel buffers.

TODO: setup global key bindings

# Code:
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Code:](#code)
- [Mode exclusions](#mode-exclusions)
- [UI Styling](#ui-styling)
  - [tab-plus--dark-background-p](#tab-plus--dark-background-p)
  - [tab-plus--update-close-button](#tab-plus--update-close-button)
  - [tab-plus--space-tab-name](#tab-plus--space-tab-name)
- [View Support](#view-support)
  - [tab-plus-get-current-view](#tab-plus-get-current-view)
  - [tab-plus--default-view-p](#tab-plus--default-view-p)
  - [tab-plus--save-view-state](#tab-plus--save-view-state)
  - [tab-plus--get-buffer-list](#tab-plus--get-buffer-list)
  - [tab-plus--set-buffer-list](#tab-plus--set-buffer-list)
  - [tab-plus-reorder-buffers-by-usage](#tab-plus-reorder-buffers-by-usage)
  - [tab-plus-reorder-buffers-by-name](#tab-plus-reorder-buffers-by-name)
  - [tab-plus--get-view-by-name](#tab-plus--get-view-by-name)
  - [tab-plus-new-view](#tab-plus-new-view)
  - [tab-plus-prompt-new-view](#tab-plus-prompt-new-view)
  - [tab-plus-list-views](#tab-plus-list-views)
  - [tab-plus-num-views](#tab-plus-num-views)
  - [tab-plus-switch-view-by-name](#tab-plus-switch-view-by-name)
  - [tab-plus-close-view-by-name](#tab-plus-close-view-by-name)
  - [tab-plus-next-view](#tab-plus-next-view)
  - [tab-plus-prev-view](#tab-plus-prev-view)
  - [tab-plus--buffer-mode](#tab-plus--buffer-mode)
  - [tab-plus--buffer-filter](#tab-plus--buffer-filter)
  - [tab-plus--get-filtered-buffer-list](#tab-plus--get-filtered-buffer-list)
  - [tab-plus--skip-foreign-buffer](#tab-plus--skip-foreign-buffer)
  - [tab-plus--get-buffer-from-tab](#tab-plus--get-buffer-from-tab)
  - [tab-plus-shift-tab-right](#tab-plus-shift-tab-right)
  - [tab-plus-shift-tab-left](#tab-plus-shift-tab-left)
  - [tab-plus-copy-current-filename-to-clipboard](#tab-plus-copy-current-filename-to-clipboard)
  - [tab-plus--leave-window](#tab-plus--leave-window)
  - [tab-plus--tear-off-buffer](#tab-plus--tear-off-buffer)
  - [tab-plus--close-tear-off-view](#tab-plus--close-tear-off-view)
  - [tab-plus-tear-off-tab](#tab-plus-tear-off-tab)
  - [tab-plus-buffer-in-multiple-viewsp](#tab-plus-buffer-in-multiple-viewsp)
  - [tab-plus--close-tab](#tab-plus--close-tab)
- [Drag and drop support](#drag-and-drop-support)
  - [tab-plus--pointer-outside-frame](#tab-plus--pointer-outside-frame)
  - [tab-plus--window-at-pointer](#tab-plus--window-at-pointer)
  - [tab-plus--move-buffer-to-window](#tab-plus--move-buffer-to-window)
  - [tab-plus-mouse-move-tab](#tab-plus-mouse-move-tab)
- [Group and Tab Selection](#group-and-tab-selection)
  - [tab-plus--update-tab-line-visibility](#tab-plus--update-tab-line-visibility)
  - [tab-plus--git-state](#tab-plus--git-state)
  - [tab-plus--format-tab](#tab-plus--format-tab)
  - [tab-plus--make-group-tab](#tab-plus--make-group-tab)
  - [tab-plus--make-view-category-tab](#tab-plus--make-view-category-tab)
  - [tab-plus--make-view-tab](#tab-plus--make-view-tab)
  - [tab-plus--buffer-modified-file-p](#tab-plus--buffer-modified-file-p)
  - [tab-plus--normalize-root](#tab-plus--normalize-root)
  - [tab-plus--buffer-in-project-p](#tab-plus--buffer-in-project-p)
  - [tab-plus--filter-buffers-by-group](#tab-plus--filter-buffers-by-group)
  - [tab-plus--find-first](#tab-plus--find-first)
  - [tab-plus--get-tabs](#tab-plus--get-tabs)
  - [tab-plus--auto-track-selected-window](#tab-plus--auto-track-selected-window)
- [Serialization](#serialization)
  - [tab-plus--tear-off-view-names](#tab-plus--tear-off-view-names)
  - [tab-plus--convert-to-persist-format](#tab-plus--convert-to-persist-format)
  - [tab-plus--serialize-views](#tab-plus--serialize-views)
  - [tab-plus--convert-from-persist-format](#tab-plus--convert-from-persist-format)
  - [tab-plus--deserialize-views](#tab-plus--deserialize-views)
  - [tab-plus--save-to-frame](#tab-plus--save-to-frame)
  - [tab-plus--rebuild-buffer-list-from-frame-params](#tab-plus--rebuild-buffer-list-from-frame-params)
  - [tab-plus-adopt-restored-buffer](#tab-plus-adopt-restored-buffer)
  - [tab-plus--add-on-filter-button](#tab-plus--add-on-filter-button)
  - [tab-plus--force-update](#tab-plus--force-update)
  - [tab-plus-select-filter-button](#tab-plus-select-filter-button)
  - [tab-plus-quick-command--update](#tab-plus-quick-command--update)
  - [tab-plus-quick-command](#tab-plus-quick-command)
- [Debugging](#debugging)
  - [tab-plus--buffer-project-root](#tab-plus--buffer-project-root)
  - [tab-plus--buffer-groups](#tab-plus--buffer-groups)
  - [tab-plus--window-mode](#tab-plus--window-mode)
  - [tab-plus-dump-state](#tab-plus-dump-state)

<!-- markdown-toc end -->

```
(defgroup tab-plus nil
  "Overrides and extensions to `tab-line'."
  :group 'tab-line)

(declare-function vc-git-state "vc-git")
(declare-function project-root "project")
```


# Mode exclusions

Set tab-line always on except for the exclude modes list
```
(setq tab-line-exclude-modes '(completion-list-mode treemacs-mode doc-view-mode imenu-list-major-mode ediff-meta-mode ediff-mode flymake-diagnostics-buffer-mode wikimode-mode clutch-result-mode clutch-describe-mode help-mode special-mode debugger-mode compilation-mode org-agenda-mode ))
```

dape defines a handful of auxiliary major modes for its info/repl/memory
buffers (dape-repl-mode, dape-info-stack-mode, etc). Rather than hand
maintaining the list above as dape adds new ones, enumerate every
`dape-...-mode' major mode once dape is loaded and add any that are
missing.
```
(with-eval-after-load 'dape
  (dolist (mode (apropos-internal "\\`dape-.*-mode\\'"
                                   (lambda (s) (get s 'derived-mode-parent))))
    (unless (memq mode tab-line-exclude-modes)
      (push mode tab-line-exclude-modes)))
  (tab-line-force-update t))
```

# UI Styling

```
(global-tab-line-mode t)
```

Always suppress the tab line separator in both windows and term mode
```
(setq tab-line-separator " ")
```

Unused: Some simple but readable close icons - use all-the-icons-insert to add a new one
```
(defconst tab-line-unused-close-icons '( "✖️" " "))
```

Override the height on the all the button so they are properly sized on 4k display
fixed in version 30

```
(defconst tab-plus-directory
  (file-name-directory (or load-file-name buffer-file-name)))
```

## tab-plus--dark-background-p
>Non-nil when the background of FACE is dark.

```
(defun tab-plus--dark-background-p (face)
  "Non-nil when the background of FACE is dark."
  (let ((rgb (color-values (let ((bg (face-background face nil t)))
			     (if (stringp bg) bg "white")))))
    (< (/ (apply #'+ rgb) 3.0) (/ 65535 2.0))))
```

## tab-plus--update-close-button
>Set `tab-line-close-button' using the icon matching the current theme.

```
(defun tab-plus--update-close-button ()
  "Set `tab-line-close-button' using the icon matching the current theme."
  (setq tab-line-close-button
	(propertize " x "
		    'display `(image :type png
				     :file ,(expand-file-name
					     (if (tab-plus--dark-background-p 'tab-line-tab-current) "close-dark.png" "close.png")
					     tab-plus-directory)
				     :height (0.9 . em)
				     :face shadow
				     :margin (2 . 0)
				     :ascent center)
		    'keymap tab-line-tab-close-map
		    'mouse-face 'tab-line-close-highlight
		    'help-echo "Click to close tab")))

(tab-plus--update-close-button)

(setq tab-line-left-button
  (propertize " <"
              'display '(image :type xpm
                               :file "tabs/left-arrow.xpm" ;; builtin image
			       :height (0.8 . em)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-left-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll left"))

(setq tab-line-right-button
  (propertize "> "
              'display '(image :type xpm
                               :file "tabs/right-arrow.xpm" ;; builtin image
			       :height (0.8 . em)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-right-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll right"))
```

simple unicode button for the modified marker
(defvar tab-plus-modified-marker "◎")
```
(defvar tab-plus-modified-marker "⏺")
```

git modified markers - I'm still looking for a better one to display in terminal mode
```
(defvar tab-plus-git-modified-marker (if window-system "" "◨"))
```

Space the tabs out a bit
```
(defvar ghostel--command-running)
(defvar my-ghostel-input-needed)
(defvar tab-plus-pinned-project)
```

## tab-plus--space-tab-name
```
(defun tab-plus--space-tab-name (buffer &optional _buffers)
  (let ((name (buffer-name buffer)))
    (when (and (eq (buffer-local-value 'major-mode buffer) 'ghostel-mode)
               (string-match "\\`\\*ghostel: \\(.*\\)\\*\\(<[0-9]+>\\)?\\'" name))
      (setq name (concat (match-string 1 name) (match-string 2 name))))
    ;; The shell title starts with an em space; `nobreak-char-display' would underline it.
    (setq name (string-replace " " " " name))
    (format " %s%s " (or (and (eq (buffer-local-value 'major-mode buffer) 'ghostel-mode)
                              (when-let* ((icon (my-modeline--icon-safe
                                                 #'nerd-icons-octicon "nf-oct-terminal")))
                                (concat icon "  ")))
                         "")
            (cond
             ((not (eq (buffer-local-value 'major-mode buffer) 'ghostel-mode)) name)
             ((buffer-local-value 'my-ghostel-input-needed buffer) (concat (my-modeline--keyboard-icon) " " name))
             ((buffer-local-value 'ghostel--command-running buffer) (concat "⏳ " name))
             (t name)))))

(setq tab-line-tab-name-function #'tab-plus--space-tab-name)
```

only put a close button on selected tabs - set to nil if totally unwanted
```
(setq tab-line-close-button-show `selected)
```

# View Support

Structure for tracking the views
tab-plus concept of virtual views is a window configuration + buffer list
wstate is the printable `window-state-get' form, kept so layouts survive a desktop save.
```
(cl-defstruct tab-plus-view name buffers wc wstate)
```

Maintain a global list of views
TODO: we don't really need to capture the wc until a swap
defvar/unless so reloading this file doesn't discard the live views.
```
(defvar tab-plus-views (list (make-tab-plus-view :name "default" :wc (current-window-configuration))))
```
(setq tab-plus-current-view 0)
```
(unless (frame-parameter nil 'tab-line-sel-view)
  (set-frame-parameter nil 'tab-line-sel-view 0))
```

## tab-plus-get-current-view
Return the current view
```
(defun tab-plus-get-current-view ()
  ;; Use the frame of the window being rendered, not the selected frame:
  ;; tab lines of unselected frames are redisplayed too.
  (let ((pos (frame-parameter (window-frame) 'tab-line-sel-view)))
    (nth (or pos 0) tab-plus-views)))
```

## tab-plus--default-view-p
Return if the current view is the default view
```
(defun tab-plus--default-view-p ()
  (equal (frame-parameter (window-frame) 'tab-line-sel-view)  0))
```

## tab-plus--save-view-state
Save any state - currently just the window configuration but I expect to add more.
```
(defun tab-plus--save-view-state ()
  (let ((view (tab-plus-get-current-view)))
    (setf (tab-plus-view-wc view) (current-window-configuration)
          (tab-plus-view-wstate view) (window-state-get (frame-root-window) t))))
```

## tab-plus--get-buffer-list
Simple low-level accessor. Most callers should use tab-plus--get-filtered-buffer-list
```
(defun tab-plus--get-buffer-list ()
  (tab-plus-view-buffers (tab-plus-get-current-view)))
```

## tab-plus--set-buffer-list
Simple setter
```
(defun tab-plus--set-buffer-list (form)
  (setf (tab-plus-view-buffers (tab-plus-get-current-view)) form))
```

## tab-plus-reorder-buffers-by-usage
Reorder the current views buffer list to match the LRU order
in the native buffer-list
```
(defun tab-plus-reorder-buffers-by-usage ()
  (interactive)
  (let* ((current-bufs (seq-filter 'tab-plus--buffer-filter (buffer-list)))
	 (intersect-tabs (seq-filter (lambda (it) (member it (tab-plus--get-buffer-list)))
				     current-bufs)))

      (tab-plus--set-buffer-list intersect-tabs)))
```

## tab-plus-reorder-buffers-by-name
Reorder the current views buffer list alphabetically
```
(defun tab-plus-reorder-buffers-by-name ()
  (interactive)
  (let* ((current-tabs (tab-plus--get-buffer-list))
	 (sorted-tabs (sort current-tabs (lambda (tab1 tab-plus)
					   (string< (buffer-name (tab-plus--get-buffer-from-tab tab1))
					      (buffer-name (tab-plus--get-buffer-from-tab tab-plus)))))))
      (tab-plus--set-buffer-list sorted-tabs)))
```


## tab-plus--get-view-by-name
Return the view with the given name
```
(defun tab-plus--get-view-by-name (name)
    (seq-find (lambda (it) (equal (tab-plus-view-name it) name))   tab-plus-views))
```

## tab-plus-new-view
Create a new view and switch to it.
```
(defun tab-plus-new-view (name)
  (interactive "sNew View name: ")
  (message "Creating view: %s" name)
  (when (tab-plus--get-view-by-name name)
    (error "View %s already exists" name))
  (setq tab-plus-views (append tab-plus-views (list (make-tab-plus-view :name name))))
  ;; Save the current wc
  (tab-plus--save-view-state)
  ;; Switch over
  (set-frame-parameter nil 'tab-line-sel-view (- (length tab-plus-views) 1))
;;  (setq tab-plus-current-view (- (length tab-plus-views) 1))
  ;; Switch to an initial scratch buffer
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (force-mode-line-update))
```

## tab-plus-prompt-new-view
```
(defun tab-plus-prompt-new-view ()
  (interactive)
  (let ((name (tab-plus-quick-command "New view name:")))
    (when name (tab-plus-new-view name))))

(defvar-keymap tab-plus-new-view-keymap
  "<tab-line> <mouse-1>" 'tab-plus-prompt-new-view
  "RET" #'tab-plus-new-view)

(setq tab-plus-new-view-button
  (propertize " + "
              'display '(image :type xpm
                               :file "tabs/new.xpm"
                               :height (0.8 . em)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-plus-new-view-keymap
              'mouse-face 'tab-line-highlight
              'help-echo "Click to add a view"))
```

## tab-plus-list-views
View enumeration by name.
```
(defun tab-plus-list-views ()
  (interactive)
  (mapcar 'tab-plus-view-name tab-plus-views))
```

## tab-plus-num-views
Number of views currently tracked
```
(defun tab-plus-num-views ()
  (length tab-plus-views))
```

## tab-plus-switch-view-by-name
Switch between views
```
(defun tab-plus-switch-view-by-name (name)
  (interactive (list (completing-read "Switch to view: " (tab-plus-list-views))))
  (let* ((new-view (tab-plus--get-view-by-name name))
	(new-view-pos (cl-position new-view tab-plus-views))
	(old-view (tab-plus-get-current-view)))

    (when (not new-view)
      (error "No view was found named %s" name))

    (when (and new-view (not (equal new-view old-view)))
      (progn
	(message "switch to %s" name)
	(tab-plus--save-view-state)
	(set-frame-parameter nil 'tab-line-sel-view new-view-pos)
	(cond ((tab-plus-view-wc new-view)
	       (set-window-configuration (tab-plus-view-wc new-view)))
	      ((tab-plus-view-wstate new-view)
	       (window-state-put (tab-plus-view-wstate new-view) (frame-root-window) 'safe)))))))
```

## tab-plus-close-view-by-name
Close a view and revert back to default
```
(defun tab-plus-close-view-by-name (name)
  (interactive (list (completing-read "Close view: " (tab-plus-list-views))))
  (let* ((closing-view (tab-plus--get-view-by-name name))
	 (current-view (tab-plus-get-current-view)))
    (when (not closing-view)
      (error "No view was found named %s" name))

    (when (equal (tab-plus-view-name closing-view) "default")
      (error "You cannot close the default view"))

    ;; temptemp
    (message "Closing view %s" name)

    ;; switch to default if current is closing
    (when (equal closing-view current-view)
      (progn
	(tab-plus-switch-view-by-name "default")
	(set-frame-parameter nil 'tab-line-sel-view 0)))

    ;; Finally remove the closing-view from the view list
    (setq tab-plus-views (remove closing-view tab-plus-views))
  ))
```

## tab-plus-next-view
Switch to the next view in the list
```
(defun tab-plus-next-view()
  (interactive)
  (let* ((oldpos (or (frame-parameter nil 'tab-line-sel-view) 0))
	 (newpos (mod (+ 1 oldpos) (length tab-plus-views)))
	 (new-view-name (tab-plus-view-name (nth newpos tab-plus-views))))

;;    (message "next %s to %s:%s" oldpos newpos  new-view-name)
    (tab-plus-switch-view-by-name  new-view-name)))
```

## tab-plus-prev-view
Switch to the prev view in the list
```
(defun tab-plus-prev-view()
  (interactive)
  (let* ((oldpos (or (frame-parameter nil 'tab-line-sel-view) 0))
	 (newpos (mod (- oldpos 1) (length tab-plus-views)))
	 (new-view-name (tab-plus-view-name (nth newpos tab-plus-views))))

;;    (message "prev %s to %s:%s" oldpos newpos new-view-name)
    (tab-plus-switch-view-by-name  new-view-name)))
```

## tab-plus--buffer-mode
>Returns the major mode associated with a buffer.

categorize a buffer's mode
```
(defun tab-plus--buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defvar tab-plus-white-list-modes
  '(text-mode lisp-interaction-mode messages-buffer-mode ghostel-mode)
  "List of modes that will be tracked even if the buffer is not a file")

(defvar tab-plus-white-list-buffer-names
  '()
  "List of buffer names that will be tracked even if the buffer is not a file")
```

## tab-plus--buffer-filter
Filter which buffers we'll track in the tabs
```
(defun tab-plus--buffer-filter (buffer)
  (or (buffer-file-name buffer)
      (member (tab-plus--buffer-mode buffer) tab-plus-white-list-modes)
      (member (buffer-name buffer) tab-plus-white-list-buffer-names)))
```

## tab-plus--get-filtered-buffer-list
Function that returns the current buffer-list.
This filters out all special buffers and leaves only files and whitelisted buffers by mode
Adds on the current buffer its filters and isn't in the list
```
(defun tab-plus--get-filtered-buffer-list ()
  (let* ((current-bufs (seq-filter 'tab-plus--buffer-filter (buffer-list)))
	 (intersect-tabs (seq-filter (lambda (it) (member it current-bufs)) (tab-plus--get-buffer-list)))
	 (new-tab (when (and (not (member (current-buffer) (tab-plus--get-buffer-list)))
			     (member (current-buffer) current-bufs))
		    (current-buffer))))
    (if new-tab
	(tab-plus--set-buffer-list (append intersect-tabs (list new-tab)))
      (tab-plus--set-buffer-list intersect-tabs))))
```

## tab-plus--skip-foreign-buffer
>Skip tracked BUFFER when switching away in WINDOW unless it is in WINDOW's view.

Window history (prev/next buffers, also restored from a saved window state) is shared by
every view, so killing/burying/cycling away from a buffer could land on one that belongs to
another view, and `tab-plus--get-filtered-buffer-list' would then adopt it into this one.
```
(defun tab-plus--skip-foreign-buffer (window buffer _bury-or-kill)
  "Skip tracked BUFFER when switching away in WINDOW unless it is in WINDOW's view."
  (and (tab-plus--buffer-filter buffer)
       (with-selected-window window
         (not (memq buffer (tab-plus--get-buffer-list))))))

(setq switch-to-prev-buffer-skip #'tab-plus--skip-foreign-buffer)
```

Filter special buffers from tab-line maintaining current order
```
(setq tab-line-tabs-function 'tab-plus--get-filtered-buffer-list)
```

## tab-plus--get-buffer-from-tab
tabs may just be a buffer or an alist depending on the mode.
```
(defun tab-plus--get-buffer-from-tab (tab)
  (if (bufferp tab) tab (alist-get 'buffer tab)))
```

## tab-plus-shift-tab-right
>Shift current buffer or selected tab right

Custom shift tab right tab-plus-shift-tab-right (&optional event)
```
(defun tab-plus-shift-tab-right (&optional event)
"Shift current buffer or selected tab right"
  (interactive (list last-nonmenu-event))
;;  (when (tab-line-track-tap event)
    (let* ((posnp (and (listp event)
                       (event-start event)))
           (window (and posnp (posn-window posnp)))
           (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
           (buffer (tab-plus--get-buffer-from-tab tab))
	   (n (seq-position (tab-plus--get-buffer-list) buffer)))
      (with-selected-window (or window (selected-window))

	(when (and n (< n (1- (length (tab-plus--get-buffer-list)))))
          (tab-plus--set-buffer-list
              (append
               (seq-take (tab-plus--get-buffer-list) n)
               (list (elt (tab-plus--get-buffer-list) (+ n 1)))
               (list (elt (tab-plus--get-buffer-list) n))
               (seq-drop (tab-plus--get-buffer-list) (+ n 2)))))))
    (force-mode-line-update))
```

## tab-plus-shift-tab-left
>Shift current buffer or selected tab left

Custom tab shift left
```
(defun tab-plus-shift-tab-left (&optional event)
  "Shift current buffer or selected tab left"
  (interactive (list last-nonmenu-event))
;;  (when (tab-line-track-tap event)
    (let* ((posnp (and (listp event)
                       (event-start event)))
           (window (and posnp (posn-window posnp)))
           (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
	   (buffer (tab-plus--get-buffer-from-tab tab))
	   (n (seq-position (tab-plus--get-buffer-list) buffer)))
      (with-selected-window (or window (selected-window))

    (when
        (and n (> n 0))
      (progn
        (tab-plus--set-buffer-list
              (append
               (seq-take (tab-plus--get-buffer-list) (- n 1))
               (list (elt (tab-plus--get-buffer-list) n))
               (list (elt (tab-plus--get-buffer-list) (- n 1)))
               (seq-drop (tab-plus--get-buffer-list) (+ n 1)))))))
    (force-mode-line-update)))
```

## tab-plus-copy-current-filename-to-clipboard
copy current buffer name to kill ring
```
(defun tab-plus-copy-current-filename-to-clipboard (&optional e)
  (interactive "e" )
  (let* ((posnp (event-start e))
         (tab (get-pos-property 1 'tab (car (posn-string posnp))))
	 (buffer (tab-plus--get-buffer-from-tab tab))
	 (filename (and buffer (buffer-file-name buffer))))

    (if filename
	(kill-new filename)
      (message "No file associated with this tab"))))
```

## tab-plus--leave-window
>Stop showing BUFFER's tab in WINDOW without killing BUFFER.
Mirrors `tab-plus--close-tab': drop it from the view, switch away if WINDOW
shows it, and delete WINDOW if that was its only tab.

Like `tear-off-window' but acts on the clicked tab and also works when it
is the only window in the frame. The torn-off buffer gets its own view in
the new frame: views are global and shared by every frame on the same view,
so otherwise the new frame's tab line would re-add the buffer to the
original frame's tabs. The original window is then handled like
`tab-plus--close-tab' does, except the buffer is never killed.
```
(defun tab-plus--leave-window (buffer window)
  "Stop showing BUFFER's tab in WINDOW without killing BUFFER.
Mirrors `tab-plus--close-tab': drop it from the view, switch away if WINDOW
shows it, and delete WINDOW if that was its only tab."
  (with-selected-window window
    (let ((tab-list (tab-line-tabs-window-buffers)))
      (tab-plus--set-buffer-list (remove buffer (tab-plus--get-buffer-list)))
      (if (eq buffer (current-buffer))
          (bury-buffer)
        (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
        (set-window-next-buffers window (delq buffer (window-next-buffers))))
      (unless (cdr tab-list)
        (ignore-errors (delete-window window)))))
  ;; Redisplay hasn't run yet, so the window's tab group still belongs to the
  ;; torn-off buffer and would hide the buffer that took its place (e.g. a
  ;; non-file ghostel buffer while the group is still a file one).
  (when (window-live-p window)
    (with-selected-window window (tab-plus--auto-track-selected-window))))
```

## tab-plus--tear-off-buffer
>Move BUFFER, shown in WINDOW, into a new frame with its own view.

```
(defun tab-plus--tear-off-buffer (buffer window)
  "Move BUFFER, shown in WINDOW, into a new frame with its own view."
  (let* ((frame (make-frame))
         (n 1)
         (name (format "tear off[%d]" n)))
    (while (tab-plus--get-view-by-name name)
      (setq n (1+ n)
            name (format "tear off[%d]" n)))
    (setq tab-plus-views (append tab-plus-views
                             (list (make-tab-plus-view :name name
                                                   :buffers (list buffer)))))
    (with-selected-frame frame
      (set-frame-parameter nil 'tab-line-sel-view (1- (length tab-plus-views)))
      (set-frame-parameter nil 'tab-plus-tear-off-view name)
      (switch-to-buffer buffer))
    (tab-plus--leave-window buffer window)
    (force-mode-line-update t)
    (tab-line-force-update t)
    (select-frame-set-input-focus frame)
    (redisplay t)
    frame))
```

## tab-plus--close-tear-off-view
>Close the view created by tearing off a tab into FRAME, if any.

Only the view name is kept on the frame: frame parameters are written to the
desktop file, which can't hold a view's buffers.
```
(defun tab-plus--close-tear-off-view (frame)
  "Close the view created by tearing off a tab into FRAME, if any."
  (when-let* ((name (frame-parameter frame 'tab-plus-tear-off-view))
              (view (tab-plus--get-view-by-name name))
              (pos (cl-position view tab-plus-views)))
    (setq tab-plus-views (delq view tab-plus-views))
    ;; Views are referenced by index, so fix up the other frames.
    (dolist (f (frame-list))
      (unless (eq f frame)
        (let ((sel (frame-parameter f 'tab-line-sel-view)))
          (cond ((not (integerp sel)))
                ((= sel pos) (set-frame-parameter f 'tab-line-sel-view 0))
                ((> sel pos) (set-frame-parameter f 'tab-line-sel-view (1- sel)))))))))

(add-hook 'delete-frame-functions #'tab-plus--close-tear-off-view)
```

## tab-plus-tear-off-tab
>Move the buffer of the tab clicked in event E into a new frame.

```
(defun tab-plus-tear-off-tab (&optional e)
  "Move the buffer of the tab clicked in event E into a new frame."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (tab (get-pos-property 1 'tab (car (posn-string posnp))))
         (buffer (tab-plus--get-buffer-from-tab tab)))
    (when buffer
      (tab-plus--tear-off-buffer buffer window))))
```

Override original definition of tab-line-tab-context-menu
Bind custom actions into the context menu if we are not in group mode
```
(define-advice tab-line-tab-context-menu (:override (&optional _event))
  "Pop up the context menu for a tab-line tab."
  (interactive "e")

  (unless (or (window-parameter nil 'tab-line-groups)
	      (window-parameter nil 'tab-line-views))
    (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))
      (define-key-after menu [close]
	'(menu-item "Close" tab-line-close-tab :help "Close the tab"))
      (define-key-after menu [shiftleft]
	'(menu-item "Shift Left" tab-plus-shift-tab-left :help "Shift the tab left"))
      (define-key-after menu [shiftright]
	'(menu-item "Shift Right" tab-plus-shift-tab-right :help "Shift the tab right"))
      (define-key-after menu [tearoff]
	'(menu-item "Tear Off Window" tab-plus-tear-off-tab :help "Move this tab into a new frame"))
      (define-key-after menu [copypath]
	'(menu-item "Copy Path" tab-plus-copy-current-filename-to-clipboard :help "Copy the current tab filename to the clipboard"))
      (define-key-after menu [reorder-lru]
	'(menu-item "Reorder tabs by usage" tab-plus-reorder-buffers-by-usage :help "Reorder the tabs based on their usage in the buffer-list"))

      (popup-menu menu))))
```

## tab-plus-buffer-in-multiple-viewsp
```
(defun tab-plus-buffer-in-multiple-viewsp (buffer)
  (let ((count 0))
    (dolist (view tab-plus-views)
      (when (member buffer (tab-plus-view-buffers view)) (setq count (+ 1 count))))

    (> count 1)))
```


## tab-plus--close-tab
Logic to carefully close a tab. buffer is only killed if its the last one.
```
(defun tab-plus--close-tab (&optional e)
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (tab (get-pos-property 1 'tab (car (posn-string posnp))))
	 (buffer (tab-plus--get-buffer-from-tab tab)))

    (if (tab-plus-buffer-in-multiple-viewsp buffer)
	;; The buffer is in other views so only:
	;; remove this buffer from the buffer list for this view and switch
	(let* ((view (tab-plus-get-current-view))
	      (new-buffers (remove buffer (tab-plus-view-buffers view))))

	  (setf (tab-plus-view-buffers view) new-buffers)

	  (if new-buffers (switch-to-buffer (nth 0 new-buffers))
	    (switch-to-buffer "*scratch*")))

      ;; This buffer is only in this view
      (with-selected-window window
	(let ((tab-list (tab-line-tabs-window-buffers))
              (buffer-list (flatten-list
                            (seq-reduce (lambda (list window)
                                          (select-window window t)
                                          (cons (tab-line-tabs-window-buffers) list))
					(window-list) nil))))

          (select-window window)
          (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
              (progn
		(if (eq buffer (current-buffer))
                    (bury-buffer)
                  (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                  (set-window-next-buffers window (delq buffer (window-next-buffers))))
		(unless (cdr tab-list)
                  (ignore-errors (delete-window window))))
	    (progn
              (and (kill-buffer buffer)
		   (unless (cdr tab-list)
                     (ignore-errors (delete-window window))))))))
)))

(define-advice tab-line-close-tab (:override (&optional e))
  "Close the selected tab.
If the tab is presented in another window, close the tab by using the `bury-buffer` function.
If the tab is unique to all existing windows, kill the buffer with the `kill-buffer` function.
Lastly, if no tabs are left in the window, it is deleted with the `delete-window` function."
  (interactive "e")

  (let* ((posnp (event-start e))
	 (tab (get-pos-property 1 'tab (car (posn-string posnp))))
	 (close (unless (bufferp tab) (alist-get 'close tab))))

    ;; User the specified close function if specified or default to tab-plus--close-tab
    (if (functionp close) (funcall close e) (tab-plus--close-tab e))
    (force-mode-line-update)))
```

# Drag and drop support

## tab-plus--pointer-outside-frame
>Return the pointer's absolute (X . Y) if it is outside FRAME, else nil.

```
(defun tab-plus--pointer-outside-frame (frame)
  "Return the pointer's absolute (X . Y) if it is outside FRAME, else nil."
  (when-let* ((pos (mouse-absolute-pixel-position))
              (edges (frame-edges frame 'outer-edges)))
    (pcase-let ((`(,left ,top ,right ,bottom) edges))
      (unless (and (>= (car pos) left) (< (car pos) right)
                   (>= (cdr pos) top) (< (cdr pos) bottom))
        pos))))
```

## tab-plus--window-at-pointer
>Return the window under pointer POS in a frame other than EXCLUDE-FRAME.
POS is an absolute (X . Y) pixel position.  Only visible frames count.  Falls
back to that frame's selected window when the pointer is over frame chrome
rather than a window.

```
(defun tab-plus--window-at-pointer (pos exclude-frame)
  "Return the window under pointer POS in a frame other than EXCLUDE-FRAME.
POS is an absolute (X . Y) pixel position.  Only visible frames count.  Falls
back to that frame's selected window when the pointer is over frame chrome
rather than a window."
  (cl-some
   (lambda (f)
     (when (and (not (eq f exclude-frame)) (frame-visible-p f) (not (frame-parent f))
                (null (tab-plus--pointer-outside-frame f)))
       (or (seq-find (lambda (w)
                       (pcase-let ((`(,l ,top ,r ,b) (window-absolute-pixel-edges w)))
                         (and (>= (car pos) l) (< (car pos) r)
                              (>= (cdr pos) top) (< (cdr pos) b)
                              (not (window-dedicated-p w))
                              (not (window-parameter w 'window-side)))))
                     (window-list f 'no-minibuf))
           (frame-selected-window f))))
   (frame-list)))
```

## tab-plus--move-buffer-to-window
>Move BUFFER's tab from FROM-WINDOW's frame into TO-WINDOW's frame.
A tear-off frame left without tabs is deleted.

```
(defun tab-plus--move-buffer-to-window (buffer from-window to-window)
  "Move BUFFER's tab from FROM-WINDOW's frame into TO-WINDOW's frame.
A tear-off frame left without tabs is deleted."
  (let ((from-frame (window-frame from-window))
        (to-frame (window-frame to-window)))
    (with-selected-window to-window
      (unless (memq buffer (tab-plus--get-buffer-list))
        (tab-plus--set-buffer-list (append (tab-plus--get-buffer-list) (list buffer))))
      (switch-to-buffer buffer))
    (tab-plus--leave-window buffer from-window)
    (when-let* (((frame-live-p from-frame))
                (name (frame-parameter from-frame 'tab-plus-tear-off-view))
                (view (tab-plus--get-view-by-name name))
                ((not (seq-some #'buffer-live-p (tab-plus-view-buffers view)))))
      (delete-frame from-frame))
    (tab-line-force-update t)
    (select-frame-set-input-focus to-frame)))
```

## tab-plus-mouse-move-tab
>Move a tab to a different position on the tab line.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event.

todo may need to force update more expansively for multiple windows when split
```
(defun tab-plus-mouse-move-tab (event)
  "Move a tab to a different position on the tab line.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event."
  (interactive "e")
  (let* ((from-str (car (posn-string (event-start event))))
         (to-str (car (posn-string (event-end event))))
         (from-rowcol (posn-col-row (event-start event)))
         (to-rowcol (posn-col-row (event-end event)))
         (from (and from-str (tab-line--get-tab-property 'tab from-str)))
         (to (and to-str (tab-line--get-tab-property 'tab to-str)))
         (from-buffer (tab-plus--get-buffer-from-tab from))
         (to-buffer (tab-plus--get-buffer-from-tab to))
         (window (posn-window (event-start event)))
         (outside-pos (and from-buffer (window-live-p window)
                           (tab-plus--pointer-outside-frame (window-frame window)))))

;;    (message "move %s p:%s to %s p:%s" from-str (car from-rowcol) to-str (car to-rowcol))

    (cond
     ;; Released outside the frame: tear the tab off into a frame at the pointer.
     ;; Released over another frame: move the tab into that frame's window.
     (outside-pos
      (if-let* ((target (tab-plus--window-at-pointer outside-pos (window-frame window))))
          (tab-plus--move-buffer-to-window from-buffer window target)
        (let ((frame (tab-plus--tear-off-buffer from-buffer window)))
          (set-frame-position frame (car outside-pos) (cdr outside-pos)))))

     ;; Only adjust when the drag started and ended on two distinct real tabs.
     ;; A drop that lands off the tab strip (blank fill area, scroll/filter
     ;; buttons, or just off the strip) has to-str/to as nil; without this
     ;; guard the dragged buffer got removed from the list below but never
     ;; re-inserted (nothing matches a nil to-buffer), so it silently vanished.
     ;; if going left to right add on the right and vice versa if going right to left
     ((and from-buffer to-buffer (not (eq from-buffer to-buffer)))
      (tab-plus--set-buffer-list
       (reverse (let (value)
                  (dolist (elt (tab-plus--get-buffer-list) value)
                    ;; add the element in its new position moving leftwards
                    (if (and (equal elt to-buffer) (> (car from-rowcol) (car to-rowcol)))
                        (setq value (cons from-buffer value)))
                    ;; add all other elements in old position
                    (if (not (equal elt from-buffer))
                        (setq value (cons elt value)))
                    ;; add the element in its new position moving rightwards
                    (if (and (equal elt to-buffer) (>= (car to-rowcol) (car from-rowcol)))
                        (setq value (cons from-buffer value)))))))
      (force-mode-line-update))

     ;; Dropped on blank tab-line space in the same window (right of the last
     ;; tab): move the tab to the end.  Buttons have a string but no tab, so
     ;; they are excluded by the null to-str check.
     ((and from-buffer (null to-str)
           (eq (posn-area (event-end event)) 'tab-line)
           (eq (posn-window (event-end event)) window)
           (memq from-buffer (tab-plus--get-buffer-list)))
      (tab-plus--set-buffer-list
       (append (delq from-buffer (copy-sequence (tab-plus--get-buffer-list)))
               (list from-buffer)))
      (force-mode-line-update)))))
```

Add on our extra key map for the drag event
```
(keymap-set tab-line-tab-map   "<tab-line> <drag-mouse-1>"      #'tab-plus-mouse-move-tab)
```

# Group and Tab Selection

Advice to add on add-new view button in the view view
```
(define-advice tab-line-format (:filter-return (format))
  (if (window-parameter nil 'tab-line-views)
      (append format (list tab-plus-new-view-button))
    format))
```

## tab-plus--update-tab-line-visibility
>Hide the tab-line on every window of FRAME except the active one.

Only show the tab-line on the active window. The `tab-line-format' window
parameter overrides the buffer-local value; `none' removes the line entirely.
```
(defun tab-plus--update-tab-line-visibility (&optional frame)
  "Hide the tab-line on every window of FRAME except the active one."
  (let* ((frame (or frame (selected-frame)))
         (active (if (minibuffer-window-active-p (minibuffer-window frame))
                     (minibuffer-selected-window)
                   (frame-selected-window frame))))
    (dolist (win (window-list frame 'never))
      (set-window-parameter win 'tab-line-format (if (eq win active) nil 'none)))))

(add-hook 'window-selection-change-functions #'tab-plus--update-tab-line-visibility)
(add-hook 'window-configuration-change-hook #'tab-plus--update-tab-line-visibility)
```

Convenience wrapper for getting the git state which needs to be done in buffer
but is  more accurate than vc-state. Cached with a short TTL since this is
called from every tab-line render and `vc-git-state' shells out to git.
```
(defvar-local tab-plus-git-state-cache nil
  "Cons of (STATE . TIMESTAMP) from the last `tab-plus--git-state' check.")

(defconst tab-plus-git-state-cache-ttl 2
  "Seconds a cached result from `tab-plus--git-state' is considered fresh.")
```

## tab-plus--git-state
```
(defun tab-plus--git-state (buffer)
  (with-current-buffer buffer
    (let ((now (float-time)))
      (unless (and tab-plus-git-state-cache
		   (< (- now (cdr tab-plus-git-state-cache)) tab-plus-git-state-cache-ttl))
	(setq tab-plus-git-state-cache
	      (cons (vc-git-state (buffer-file-name buffer)) now)))
      (car tab-plus-git-state-cache))))
```

## tab-plus--format-tab
>Override for  `tab-line-tab-name-format-function' that adds on a modified buffer face and indicator

Custom tab-line-name-format function to add on a face for the modified signifier
so it can be colored or not depending on being selected
and filter icon to the first tab
```
(defun tab-plus--format-tab (tab tabs)
  "Override for  `tab-line-tab-name-format-function' that adds on a modified buffer face and indicator"
  (let* ((buffer (tab-plus--get-buffer-from-tab tab))
         (selected-p (if buffer
                         (eq buffer (window-buffer))
                       (cdr (assq 'selected tab))))

         (name (if buffer
                   (funcall tab-line-tab-name-function buffer tabs)
                 (cdr (assq 'name tab))))
         (face (if selected-p
                   'tab-line-tab-current
                 'tab-line-tab-inactive)))

    ;; the face funcs are called even when the tab isn't a buffer
    ;; so we can't use buffer directly here
    (dolist (fn tab-line-tab-face-functions)
      (progn
	(setf face (funcall fn tab tabs face (bufferp tab) selected-p))))

    (apply 'propertize
           (concat
	    ;; Append FACE rather than replace so an icon's own :family face survives.
	    (let ((str (string-replace "%" "%%" name))) ;; (bug#57848)
	      (add-face-text-property 0 (length str) face t str)
	      (add-text-properties 0 (length str)
	                           `(keymap ,tab-line-tab-map
	                             help-echo ,(if selected-p "Current tab"
	                                          "Click to select tab")
	                             ;; Don't turn mouse-1 into mouse-2 (bug#49247)
	                             follow-link ignore)
	                           str)
	      str)

	    ;; Modified marker - TODO - move to custom faces
	    (cond ((and buffer (buffer-modified-p buffer) (buffer-file-name buffer))
		   (if selected-p
		       (propertize (format "%s " tab-plus-modified-marker) 'face `(:inherit ,face :foreground "red2" :height .9 :slant normal ))
		     (propertize (format "%s " tab-plus-modified-marker) 'face `(:inherit ,face :foreground "gray50" :height .9 :slant normal ))))

		  ((and buffer (buffer-file-name buffer)
			(string= (tab-plus--git-state buffer) "edited"))
		   (progn
;;		     (message "git modified: %s %s" buffer (vc-state (buffer-file-name buffer)))
		    (if selected-p
			(propertize (format "%s " tab-plus-git-modified-marker) 'face `(:inherit ,face :foreground "dark cyan" :height .9 :slant normal ))
		      (propertize (format "%s " tab-plus-git-modified-marker) 'face `(:inherit ,face :foreground "gray50" :height .9 :slant normal ))))))

            (let* ((close (or (and (or buffer (assq 'close tab))
                                  tab-line-close-button-show
                                  (not (eq tab-line-close-button-show
                                           (if selected-p 'non-selected
                                             'selected)))
                                  tab-line-close-button)
                             ""))
                   (close2 (copy-sequence close)))

              ;; Don't overwrite the icon face
              (add-face-text-property 0 (length close2) face t close2)
              close2))

           `(
             tab ,tab
             ,@(if selected-p '(selected t))
             mouse-face tab-line-highlight))
    ))
```


## tab-plus--make-group-tab
Group of tab constructors
```
(defun tab-plus--make-group-tab (selected-group groupname)
  (let ((formatted-name   (format " %s " groupname)))
    `(tab
      (name . ,formatted-name)
      (selected . ,(equal selected-group groupname))
      (select . ,(lambda ()
                   (set-window-parameter nil 'tab-line-groups nil)
                   (set-window-parameter nil 'tab-line-group groupname)
                   (set-window-parameter nil 'tab-line-hscroll nil))))))
```

## tab-plus--make-view-category-tab
```
(defun tab-plus--make-view-category-tab (selected-view viewname)
  (let ((formatted-name   (format " %s " viewname)))
    `(tab
      (name . ,formatted-name)
      (selected . ,(equal selected-view viewname))
      (select . ,(lambda ()
                   (set-window-parameter nil 'tab-line-views t)
                   (set-window-parameter nil 'tab-line-hscroll nil))))))
```

## tab-plus--make-view-tab
```
(defun tab-plus--make-view-tab (selected-view viewname)
  (let* ((formatted-name (format " %s " viewname))
	 (new-view (tab-plus--get-view-by-name viewname))
	 (new-view-pos (cl-position new-view tab-plus-views)))

    (if (equal viewname "default")
	`(tab
	  (name . ,formatted-name)
	  (selected . ,(equal selected-view viewname))
	  (select . ,(lambda ()
		       (tab-plus-switch-view-by-name viewname)
		       (set-frame-parameter nil 'tab-line-sel-view new-view-pos)
                       (set-window-parameter nil 'tab-line-hscroll nil))))

	`(tab
	  (name . ,formatted-name)
	  (selected . ,(equal selected-view viewname))
	  (close . ,(lambda (&optional _buffer)
		      (tab-plus-close-view-by-name viewname)))
	  (select . ,(lambda ()
		       (tab-plus-switch-view-by-name viewname)
		       (set-frame-parameter nil 'tab-line-sel-view new-view-pos)
                       (set-window-parameter nil 'tab-line-hscroll nil)))))))
```


## tab-plus--buffer-modified-file-p
predicate to determine if a buffer is modified and backed by a file
```
(defun tab-plus--buffer-modified-file-p (buffer)
  (and (buffer-file-name buffer) (buffer-modified-p buffer)))
```

Per-buffer cache of project membership: (PATH ROOT . MEMBERP).  Membership is
recomputed only when the buffer's path or the current project root changes,
instead of rescanning every buffer (as `project-buffers' does) on each redraw.
```
(defvar-local tab-plus-project-membership-cache nil)
```

## tab-plus--normalize-root
>Return ROOT as an absolute directory name with a trailing slash, or nil.

```
(defun tab-plus--normalize-root (root)
  "Return ROOT as an absolute directory name with a trailing slash, or nil."
  (when root (expand-file-name (file-name-as-directory root))))
```

## tab-plus--buffer-in-project-p
>Return non-nil if BUFFER belongs to the project rooted at ROOT.
ROOT must already be normalized with `tab-plus--normalize-root'.

```
(defun tab-plus--buffer-in-project-p (buffer root)
  "Return non-nil if BUFFER belongs to the project rooted at ROOT.
ROOT must already be normalized with `tab-plus--normalize-root'."
  (when root
    ;; A pinned project (e.g. a terminal's) wins over its (cd-able) directory.
    (if-let* ((pinned (buffer-local-value 'tab-plus-pinned-project buffer)))
        (equal (tab-plus--normalize-root pinned) root)
      (let ((path (or (buffer-file-name buffer)
                      (buffer-local-value 'default-directory buffer)))
            (cache (buffer-local-value 'tab-plus-project-membership-cache buffer)))
        (if (and cache (equal (car cache) path) (equal (cadr cache) root))
            (cddr cache)
          (let ((member (and path (file-in-directory-p path root) t)))
            (with-current-buffer buffer
              (setq tab-plus-project-membership-cache (cons path (cons root member))))
            member))))))
```

## tab-plus--filter-buffers-by-group
Filter the list of buffers based on the selected group
if Files then return all of the file based buffers.
if Project then return all buffers in the curent project.
```
(defun  tab-plus--filter-buffers-by-group (buffers curgroup)
  (cond ((or (not curgroup) (equal curgroup "All")) buffers)
        ((equal curgroup "Files")
         (seq-filter (lambda (b) (buffer-file-name b)) buffers))

        ((equal curgroup "Project")
         (let ((root (when-let* ((pr (project-current nil)))
                       (tab-plus--normalize-root (project-root pr)))))
           (seq-filter (lambda (b) (tab-plus--buffer-in-project-p b root)) buffers)))

        ((equal curgroup "Modified")
         (seq-filter (lambda (b) (tab-plus--buffer-modified-file-p b)) buffers))

        (t (seq-filter (lambda (b)
                         (equal (tab-line-tabs-buffer-group-name b) curgroup))
                       buffers))))
```

## tab-plus--find-first
```
(defun tab-plus--find-first (fn ls)
  (cond ((null ls) nil)
        ((funcall fn (car ls)) (car ls))
        (t (tab-plus--find-first fn (cdr ls)))))
```

## tab-plus--get-tabs
>Return a list of tabs that should be displayed in the tab line.
 This version is based on tab-line-tabs-buffers-groups but
 changes first group to an icon and adds on all files

Setup the list of tabs to be displayed.
Note: these will returned in the tab list format
tab-plus--format-tab is then responsible for formatting each one i.e faces / close icons etc.
```
(defun tab-plus--get-tabs ()
  "Return a list of tabs that should be displayed in the tab line.
  This version is based on tab-line-tabs-buffers-groups but
  changes first group to an icon and adds on all files"

  (cond
   ;; views view
   ((window-parameter nil 'tab-line-views)
    (let* ((selected-view (tab-plus-view-name (tab-plus-get-current-view)))
	   (tabs (mapcar (lambda (view)
			   (tab-plus--make-view-tab selected-view (tab-plus-view-name view)))
			 tab-plus-views)))
      tabs))

   ;; groups view
   ((window-parameter nil 'tab-line-groups)
    (let* ((buffers (funcall tab-line-tabs-buffer-list-function))
	   (groups (delq nil (mapcar #'car (seq-group-by
					    (lambda (buffer)
                                              (tab-line-tabs-buffer-group-name
                                               buffer))
                                            buffers))))
	   (selected-group (window-parameter nil 'tab-line-group))
	   (tabs (mapcar (apply-partially 'tab-plus--make-group-tab selected-group) groups)))

      ;;insert a Files and Project group
      (append
       (when (project-current nil)
	 (list (tab-plus--make-group-tab selected-group "Project")))
       (list (tab-plus--make-group-tab selected-group "Files"))
       (list (tab-plus--make-group-tab selected-group "All"))

       ;; Insert a modified group if any files are modified
       (when (tab-plus--find-first 'tab-plus--buffer-modified-file-p buffers)
	 (list (tab-plus--make-group-tab selected-group "Modified")))
       tabs
       (list (tab-plus--make-view-category-tab
	      (tab-plus-view-name (tab-plus-get-current-view)) "Views")))))

   ;; default tabs view
   (t (let* (
	     (selected-group (window-parameter nil 'tab-line-group))
	     (all-buffers (funcall tab-line-tabs-buffer-list-function))
	     (buffers (tab-plus--filter-buffers-by-group all-buffers selected-group))
	     (sorted-buffers (if (functionp tab-line-tabs-buffer-group-sort-function)
				 (seq-sort tab-line-tabs-buffer-group-sort-function
					   buffers)
			       buffers))
	     (tabs (mapcar (lambda (buffer)
			     `(tab
			       (name . ,(funcall tab-line-tab-name-function buffer))
			       (selected . ,(eq buffer (current-buffer)))
			       (buffer . ,buffer)
			       (close . tab-plus--close-tab)))
			   sorted-buffers)))

;;	(message "chk g:%s b:%s" selected-group buffers)
	tabs))))
```

## tab-plus--auto-track-selected-window
If the current window is in a valid group that is not currently
set - switch the selected group to match it.
TODO: make configurable?
```
(defun tab-plus--auto-track-selected-window (&optional _frame)
  (let ((bufgroup (tab-line-tabs-buffer-group-name (current-buffer)))
	(filep (buffer-file-name (current-buffer)))
	(valid (tab-plus--buffer-filter (current-buffer)))
	(selected-group (window-parameter nil 'tab-line-group)))

;;    (message "autotrack: %s %s %s %s" bufgroup filep valid selected-group)

    (when (or (and (not selected-group) valid)
	      (and selected-group
		   valid
		   (not (equal selected-group "All"))
		   (not (equal bufgroup selected-group))
		   ;; Optimization: lets files skip the project buffer list rebuild below.
		   (not (and filep (equal selected-group "Project") (project-current nil)))
		   (not (and (equal selected-group "Project")
			     (tab-plus--filter-buffers-by-group (list (current-buffer)) "Project")))
		   (not (and filep (equal selected-group "Files")))))

      (set-window-parameter nil 'tab-line-group bufgroup))))
```


Setup the window buffer changes functions to monitor when a window is selected
Hook that up to the auto-track function
```
(add-hook 'window-buffer-change-functions 'tab-plus--auto-track-selected-window)
```


Set the override tab name format function to the one I've defined
```
(setopt tab-line-tab-name-format-function 'tab-plus--format-tab)
```

Use custom tab grouping function.
```
(setq tab-line-tabs-function 'tab-plus--get-tabs)
```

Preserve manual tab ordering (drag-and-drop, shift left/right via the
context menu). Stock tab-line defaults this to an alphabetical sort,
which silently re-sorted the buffer list back into place on every
redisplay -- right after a reorder took effect -- making drag-and-drop
and the context-menu shift commands look like they did nothing.
```
(setq tab-line-tabs-buffer-group-sort-function nil)
```

Set the list-function to use the same one I'm overriding in basic mode
```
(setq tab-line-tabs-buffer-list-function 'tab-plus--get-filtered-buffer-list)
```

# Serialization
```
(cl-defstruct tab-plus-persist-view name buffernames wstate)
```

## tab-plus--tear-off-view-names
```
(defun tab-plus--tear-off-view-names ()
  (delq nil (mapcar (lambda (f) (frame-parameter f 'tab-plus-tear-off-view)) (frame-list))))
```

## tab-plus--convert-to-persist-format
serialize a view
```
(defun tab-plus--convert-to-persist-format (view)
  (make-tab-plus-persist-view
   :name (tab-plus-view-name view)
   :buffernames (mapcar #'buffer-name (seq-filter #'buffer-live-p (tab-plus-view-buffers view)))
   :wstate (tab-plus-view-wstate view)))
```

## tab-plus--serialize-views
serialize all views except tear-off ones, which belong to frames that aren't restored
```
(defun tab-plus--serialize-views ()
  (let ((skip (tab-plus--tear-off-view-names)))
    (mapcar #'tab-plus--convert-to-persist-format
            (seq-remove (lambda (v) (member (tab-plus-view-name v) skip)) tab-plus-views))))
```

## tab-plus--convert-from-persist-format
Deserialize a view, dropping buffers that desktop didn't recreate
```
(defun tab-plus--convert-from-persist-format (view)
  (make-tab-plus-view :name (tab-plus-persist-view-name view)
                  :buffers (delq nil (mapcar #'get-buffer (tab-plus-persist-view-buffernames view)))
                  :wstate (tab-plus-persist-view-wstate view)))
```

## tab-plus--deserialize-views
```
(defun tab-plus--deserialize-views (views)
  (mapcar #'tab-plus--convert-from-persist-format views))
```

## tab-plus--save-to-frame
persist views and the selected view's name to the frame for reload
```
(defun tab-plus--save-to-frame ()
  (tab-plus--save-view-state)
  (set-frame-parameter nil 'tab-plus-views (tab-plus--serialize-views))
  (set-frame-parameter nil 'tab-plus-sel-view-name
                       (let ((name (tab-plus-view-name (tab-plus-get-current-view))))
                         (unless (member name (tab-plus--tear-off-view-names)) name))))
```

## tab-plus--rebuild-buffer-list-from-frame-params
process the saved views in the frame parameters and set up to match
```
(defun tab-plus--rebuild-buffer-list-from-frame-params ()
  (let* ((views (tab-plus--deserialize-views (frame-parameter nil 'tab-plus-views)))
         (sel (frame-parameter nil 'tab-plus-sel-view-name)))
    (when views
      (setq tab-plus-views views)
      (set-frame-parameter nil 'tab-line-sel-view
                           (or (cl-position sel views :key #'tab-plus-view-name :test #'equal) 0)))
    (force-mode-line-update)))
```

## tab-plus-adopt-restored-buffer
A buffer desktop restored under a transient name (ghostel renames itself once
its shell reports a title) misses the by-name lookup above.  Once it has its
final name, re-add it to the views the saved frame params list it in.
```
(defun tab-plus-adopt-restored-buffer (buffer)
  (dolist (pv (frame-parameter nil 'tab-plus-views))
    (when (member (buffer-name buffer) (tab-plus-persist-view-buffernames pv))
      (when-let* ((view (tab-plus--get-view-by-name (tab-plus-persist-view-name pv))))
        (unless (memq buffer (tab-plus-view-buffers view))
          (setf (tab-plus-view-buffers view)
                (append (tab-plus-view-buffers view) (list buffer))))))))

(defcustom tab-plus-desktop-save-views-only t
  "When non-nil, only save tab-tracked buffers that are in a saved view.
Buffers that were opened but never joined a view (and buffers in tear-off
views) are then left out of the desktop file."
  :type 'boolean
  :group 'tab-plus)
```

This runs after `desktop-save-hook', so the view lists are already current.
```
(define-advice desktop-save-buffer-p (:around (orig filename bufname mode &rest rest) tab-plus-views-only)
  (let ((buffer (get-buffer bufname))
        (skip (tab-plus--tear-off-view-names)))
    (and (apply orig filename bufname mode rest)
         (or (not tab-plus-desktop-save-views-only)
             (not buffer)
             (not (tab-plus--buffer-filter buffer))
             (seq-some (lambda (v)
                         (and (not (member (tab-plus-view-name v) skip))
                              (memq buffer (tab-plus-view-buffers v))))
                       tab-plus-views)))))
```

hook the rebuild function to run after read desktop
```
(add-hook 'desktop-after-read-hook 'tab-plus--rebuild-buffer-list-from-frame-params)
(add-hook 'desktop-save-hook 'tab-plus--save-to-frame)
```

Preserve the directory where the script was loaded from for
use in doing relative open of resource files
```
(setq tab-plus-load-dir (file-name-directory load-file-name))
```

## tab-plus--add-on-filter-button
Add on filter button at the front of the tab list via advice to
tab-line-format-template
```
(defun tab-plus--add-on-filter-button (tabs)
  (let* ((group-view (window-parameter nil 'tab-line-groups))
	 (viewp (window-parameter nil 'tab-line-views))
	 (icon-file (cond (viewp "desktop2.png")
			  ((tab-plus--dark-background-p 'tab-line) "funnel4-dark.png")
			  (t "funnel4.png")))
	 (icon-color (modus-themes-get-color-value 'bg-tab-bar t))
	 (icon-name (format "%s%s" tab-plus-load-dir icon-file)))

    ;; Only prepend the filter button when in buffer-groups mode
    (if (eq  tab-line-tabs-function 'tab-plus--get-tabs)
	(cons
	 (propertize "▼"
		     'face `(:box (:line-width (0 . 4) :color ,icon-color ))
		     'keymap tab-line-button-map
		     'help-echo "Click to change tab filtering"
		     'follow-link 'ignore
		     'display `(image :type png
				      :file ,icon-name
				      :height (0.8 . em)
				      :margin (3 . 0)
				      :relief ,(if group-view -3 3)
				      :ascent 97
				      ))
	 tabs)
      tabs)))
```

## tab-plus--force-update
Update the current tabline. Used during theme load.
```
(defun tab-plus--force-update (_theme)
  (tab-plus--update-close-button)
  (tab-line-force-update nil))
```

Add a hook to enable themes that will force an update so we pickup the icon background color
```
(add-hook 'enable-theme-functions 'tab-plus--force-update)
```

## tab-plus-select-filter-button
keymap action for when the filter button is selected
Swaps between group and file mode
```
(defun tab-plus-select-filter-button (&optional _event)
  (interactive "e")
  (let ((group-view (window-parameter nil 'tab-line-groups)))
    (if group-view
	(progn
	  ;; turn off group and view view
	  (set-window-parameter nil 'tab-line-groups nil)
	  (set-window-parameter nil 'tab-line-views nil)
          (set-window-parameter nil 'tab-line-hscroll nil))
      (progn
	;; turn on group view
	(set-window-parameter nil 'tab-line-groups t)
        (set-window-parameter nil 'tab-line-hscroll nil))))

  (force-mode-line-update))

(defvar-keymap tab-line-button-map
  :doc "Local keymap for `tab-line-mode' filter button."
  "<tab-line> <mouse-1>" #'tab-plus-select-filter-button
  "RET" #'tab-plus-select-filter-button)
```

Via advice add on the filter button
```
(advice-add 'tab-line-format-template :filter-return 'tab-plus--add-on-filter-button)
```

Note: the tab-plus-aware modeline segments themselves (the eglot/flymake
counter and the current tab-plus view) are defined in modeline.el, alongside
the rest of the modeline segment code, and included there unconditionally.

bunch of code from nicolas rougier adapted to use the header-line
```
(defface quick-command-face
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-background 'highlight nil t)
       :height ,(face-attribute 'default :height)
       :box (:line-width (1 . 1)
              :color ,(face-foreground 'default))
       ))
  "Face for quick command")

(defface quick-command-prompt-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :weight ,(face-attribute 'bold :weight nil 'default)
       :box (:line-width (1 . 1)
             :color ,(face-foreground 'default))
       ))
  "Face for prompt")

(defface quick-command-cursor-face
  `((t :foreground ,(face-background 'default)
       ;; :background "#d0d0d0"
       :background ,(face-foreground 'default)
       :box (:line-width (1 . 1)
             :color ,(face-foreground 'default))
       ))
  "Face for cursor")
```

## tab-plus-quick-command--update
>Update header-line with current command

```
(defun tab-plus-quick-command--update (current-buffer command-buffer)
  "Update header-line with current command"

  (with-current-buffer command-buffer
    (let* ((text (concat (buffer-substring (point-min) (point-max)) " "))
           (point (point)))
      (add-face-text-property (- point 1) point 'quick-command-cursor-face t text)
      (with-current-buffer current-buffer
        (setq-local header-line-format text))
      (force-mode-line-update))))
```

## tab-plus-quick-command
>Read user-input from the header-line using the given PROMPT.

```
(defun tab-plus-quick-command (&optional prompt)
  "Read user-input from the header-line using the given PROMPT."

  (interactive)
  (let* ((saved-mode-line header-line-format)
         (command nil)
         (current-buffer (current-buffer))
         (current-window (selected-window))
         (prompt (format " %s " (or prompt "Quick command")))
         (prompt (concat (propertize prompt 'face 'quick-command-prompt-face)
                         " "))
         (command-buffer (get-buffer-create " *quick-command*")))

    ;; To make sure to remove the relative face
    (unwind-protect

        ;; Catch enter or exit key press
        (catch 'break

          (with-current-buffer command-buffer

            ;; Clear previous buffer content
            (let ((inhibit-read-only t))
              (erase-buffer))

            ;; Insert prompt
            (insert (concat (propertize prompt 'intangible t
                                               'cursor-intangible t
                                               'read-only t
                                               'front-sticky nil
                                               'rear-nonsticky t)))
            (tab-plus-quick-command--update current-buffer command-buffer)
            (cursor-intangible-mode t)

            ;; Main loop where we read key sequences until RET or ESC is pressed
            (while t
              (let* ((message "")
                     (key (key-description (read-key-sequence message))))

                ;; Command enter
                (when (string= key "RET")
                  (setq command (buffer-substring (+ (length prompt) 1) (point-max)))
                  (if (> (length command) 0)
                      (throw 'break command)
                    (throw 'break nil)))

                ;; Command abort
                (when (and (string= key "C-g")
                           (with-current-buffer command-buffer
                             (not (region-active-p))))
                  (setq command nil)
                  (throw 'break nil))

                ;; Execute key sequence in command buffer
                (set-window-buffer current-window command-buffer t)
                (condition-case nil
                    (execute-kbd-macro (kbd key))
                  ((beginning-of-buffer end-of-buffer text-read-only)))
                ;; Make sure to not go into prompt area
                (goto-char (max (+ (length prompt) 1) (point)))

                (set-window-buffer current-window current-buffer t)

                ;; Update mode line
                (tab-plus-quick-command--update current-buffer command-buffer)))))

      ;; Command entered or aborted: restore mode line
      (with-current-buffer current-buffer
        (setq-local header-line-format saved-mode-line)
        (force-mode-line-update))
      (kill-buffer command-buffer)
      (switch-to-buffer current-buffer))))
```

# Debugging

## tab-plus--buffer-project-root
>Return the project root BUFFER belongs to, or nil.
A terminal's pinned `tab-plus-pinned-project' wins over its directory.

```
(defun tab-plus--buffer-project-root (buffer)
  "Return the project root BUFFER belongs to, or nil.
A terminal's pinned `tab-plus-pinned-project' wins over its directory."
  (or (tab-plus--normalize-root (buffer-local-value 'tab-plus-pinned-project buffer))
      (with-current-buffer buffer
        (when-let* ((pr (project-current nil)))
          (tab-plus--normalize-root (project-root pr))))))
```

## tab-plus--buffer-groups
>Return the list of tab group names BUFFER is assigned to.
ROOT is the project root used for the virtual "Project" group.

```
(defun tab-plus--buffer-groups (buffer root)
  "Return the list of tab group names BUFFER is assigned to.
ROOT is the project root used for the virtual \"Project\" group."
  (seq-filter (lambda (g) (and g (not (equal g ""))))
              (list (when (buffer-file-name buffer) "Files")
                    (when (and root (tab-plus--buffer-in-project-p buffer root)) "Project")
                    (when (tab-plus--buffer-modified-file-p buffer) "Modified")
                    (tab-line-tabs-buffer-group-name buffer))))
```

## tab-plus--window-mode
>Describe which kind of tab line WINDOW is showing.

```
(defun tab-plus--window-mode (window)
  "Describe which kind of tab line WINDOW is showing."
  (cond ((window-parameter window 'tab-line-views) "views")
        ((window-parameter window 'tab-line-groups) "groups")
        (t "tabs")))
```

## tab-plus-dump-state
>Show all tab-plus state in the *tab-plus-state* buffer.
Lists frames, windows, views and every tracked buffer with the groups it is
assigned to and its project root.

```
(defun tab-plus-dump-state ()
  "Show all tab-plus state in the *tab-plus-state* buffer.
Lists frames, windows, views and every tracked buffer with the groups it is
assigned to and its project root."
  (interactive)
  (let ((buf (get-buffer-create "*tab-plus-state*"))
        (frames (frame-list))
        (views tab-plus-views)
        (origin (selected-window)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (special-mode)
        (erase-buffer)
        (insert (format "== Frames (%d) ==\n" (length frames)))
        (dolist (f frames)
          (let* ((sel (frame-parameter f 'tab-line-sel-view))
                 (view (nth (or sel 0) views)))
            (insert (format "%s%s: view %s (index %s)%s\n"
                            (if (eq f (selected-frame)) "* " "  ")
                            (frame-parameter f 'name)
                            (if view (format "%S" (tab-plus-view-name view)) "<none>")
                            sel
                            (if-let* ((tear (frame-parameter f 'tab-plus-tear-off-view)))
                                (format " [tear-off %S]" tear) "")))
            (dolist (w (window-list f 'no-minibuf))
              (let* ((wbuf (window-buffer w))
                     (group (window-parameter w 'tab-line-group))
                     (root (tab-plus--buffer-project-root wbuf)))
                (insert (format "    %s%s  buffer=%s  mode=%s  group=%s  project=%s\n"
                                (if (eq w origin) "* " "  ")
                                w (buffer-name wbuf) (tab-plus--window-mode w)
                                (if group (format "%S" group) "nil")
                                (or root "-")))
                (when view
                  (insert (format "      shown tabs: %s\n"
                                  (mapconcat #'buffer-name
                                             (tab-plus--filter-buffers-by-group
                                              (seq-filter #'buffer-live-p (tab-plus-view-buffers view))
                                              group)
                                             ", "))))))))
        (insert (format "\n== Views (%d) ==\n" (length views)))
        (dolist (view views)
          (let ((bufs (tab-plus-view-buffers view)))
            (insert (format "\n[%s]  %d buffer(s)%s\n"
                            (tab-plus-view-name view) (length bufs)
                            (if (tab-plus-view-wstate view) "  (window state saved)" "")))
            (dolist (b bufs)
              (if (not (buffer-live-p b))
                  (insert "  <dead buffer>\n")
                (let ((root (tab-plus--buffer-project-root b)))
                  (insert (format "  %-32s groups=%s  tracked=%s\n"
                                  (buffer-name b)
                                  (mapconcat #'identity (tab-plus--buffer-groups b root) ",")
                                  (if (tab-plus--buffer-filter b) "yes" "no")))
                  (insert (format "    mode=%s  file=%s\n    project=%s%s\n"
                                  (tab-plus--buffer-mode b)
                                  (or (buffer-file-name b) "-")
                                  (or root "-")
                                  (if (buffer-local-value 'tab-plus-pinned-project b) " (pinned)" ""))))))))
        (let ((untracked (seq-filter
                          (lambda (b)
                            (and (tab-plus--buffer-filter b)
                                 (not (seq-some (lambda (v) (memq b (tab-plus-view-buffers v))) views))))
                          (buffer-list))))
          (insert (format "\n== Trackable buffers in no view (%d) ==\n" (length untracked)))
          (dolist (b untracked)
            (insert (format "  %s\n" (buffer-name b)))))
        (goto-char (point-min))))
    (pop-to-buffer buf)))
```

> This file was auto-generated by elispdoc.el
