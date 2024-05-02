;;; tab-config.el --- my overrides to tab-line -*- lexical-binding: t; -*-

;; ```
;; | |_ __ _| |__         ___ ___  _ __  / _(_) __ _
;; | __/ _` | '_ \ _____ / __/ _ \| '_ \| |_| |/ _` |
;; | || (_| | |_) |_____| (_| (_) | | | |  _| | (_| |
;;  \__\__,_|_.__/       \___\___/|_| |_|_| |_|\__, |
;;                                             |___/
;; ```
;; Commentary:
;;
;; tab-line configuration
;; Originally these changes were aimed at making tabs operate similarly to the tabs in the browser
;; or terminal.
;;
;; I use tab-line rather than tab-bar because I want tabs in the "main" windows and not on the
;; sidebar ones and I'm buffer oriented rather than workspace oriented
;;
;; I then expanded support for the grouping modes to include filtering by file backed buffers
;; or project and moved to a button oriented UI.
;;
;; This allowed an extension to create views similar to tab-bar that store isolated buffer lists
;; and window configurations.
;;
;; Because I might want to turn this into a package I'm going to use the tab2 prefix for all
;; functions that I add on. You'll also see original tab-line vars and funcs referenced.
;;
;; I've borrowed and modified some code from Nicolas Rougier to gather input on the header line
;; which is less of a distracting jump away from the tabs when needed.
;;
;;
;; TODO:
;;       persist view  to file - tab / buffer separation is done but not window-state

;; TODO: key bindings
;;
;;; Code:

;; Set tab-line always on
(global-tab-line-mode t)

;; Always suppress the tab line separator in both windows and term mode
(setq tab-line-separator " ")

;; Unused: Some simple but readable close icons - use all-the-icons-insert to add a new one
(setq tab-line-unused-close-icons '( "✖️" " "))

;; Override the height on the all the button so they are properly sized on 4k display
;; fixed in version 30

;; Current modern style button I'm using
(setq tab-line-close-button
  (propertize " x "
              'display '(image :type png
                               :file "/Users/benjamin.leis/.emacs.d/close.png"
			       :height (0.9 . em)
			       :face shadow
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-tab-close-map
              'mouse-face 'tab-line-close-highlight
              'help-echo "Click to close tab"))

(setq tab-line-left-button
  (propertize " <"
              'display '(image :type xpm
                               :file "tabs/left-arrow.xpm"
			       :height (0.8 . em)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-left-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll left"))

(setq tab-line-right-button
  (propertize "> "
              'display '(image :type xpm
                               :file "tabs/right-arrow.xpm"
			       :height (0.8 . em)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-right-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll right"))

;; simple unicode button for the modified marker
;;(defvar tab2-modified-marker "◎")
(defvar tab2-modified-marker "⏺")

;; git modified markers - I'm still looking for a better one to display in terminal mode
(defvar tab2-git-modified-marker (if window-system "" "◨"))

;; Space the tabs out a bit
(defun tab2-space-tab-name (buffer &optional _buffers)
  (format " %s " (buffer-name buffer)))

(setq tab-line-tab-name-function #'tab2-space-tab-name)

;; only put a close button on selected tabs - set to nil if totally unwanted
(setq tab-line-close-button-show `selected)

;; But don't include in treemacs windows, doc-view or imenu-list
(setq tab-line-exclude-modes '(completion-list-mode treemacs-mode doc-view-mode imenu-list-major-mode ediff-meta-mode ediff-mode flymake-diagnostics-buffer-mode end ))

;;; Structure for tracking the views
;; tab2 concept of virtual views is a window configuration + buffer list

(cl-defstruct tab2-view name buffers wc)

;; Maintain a global list of views
;; TODO: we don't really need to capture the wc until a swap
(setq tab2-views (list (make-tab2-view :name "default" :wc (current-window-configuration))))
;;(setq tab2-current-view 0)
(set-frame-parameter nil 'tab-line-sel-view 0)

;; Return the current view
(defun tab2-get-current-view ()
  (let ((pos (frame-parameter nil 'tab-line-sel-view)))
    (nth (or pos 0) tab2-views)))

;; Return if the current view is the default view
(defun tab2-default-view-p ()
  (equal (frame-parameter nul 'tab-line-sel-view)  0))

;; Save any state - currently just the window configuration but I expect to add more.
(defun tab2-save-view-state ()
  (setf (tab2-view-wc (tab2-get-current-view)) (current-window-configuration)))

;; Simple low-level accessor. Most callers should use tab2-get-filtered-buffer-list
(defun tab2-get-buffer-list ()
  (tab2-view-buffers (tab2-get-current-view)))

;; Simple setter
(defun tab2-set-buffer-list (form)
  (setf (tab2-view-buffers (tab2-get-current-view)) form))

;; Reorder the current views buffer list to match the LRU order
;; in the native buffer-list
(defun tab2-reorder-buffers-by-usage ()
  (interactive)
  (let* ((current-bufs (seq-filter 'tab2-buffer-filter (buffer-list)))
	 (intersect-tabs (seq-filter (lambda (it) (member it (tab2-get-buffer-list)))
				     current-bufs)))

      (tab2-set-buffer-list intersect-tabs)))

;; Reorder the current views buffer list alphabetically
(defun tab2-reorder-buffers-by-name ()
  (interactive)
  (let* ((current-tabs (tab2-get-buffer-list))
	 (sorted-tabs (sort current-tabs (lambda (tab1 tab2)
					   (string< (buffer-name (tab2-get-buffer-from-tab tab1))
					      (buffer-name (tab2-get-buffer-from-tab tab2)))))))
      (tab2-set-buffer-list sorted-tabs)))


;; Return the view with the given name
(defun tab2-get-view-by-name (name)
    (seq-find (lambda (it) (equal (tab2-view-name it) name))   tab2-views))

;; Create a new view and switch to it.
(defun tab2-new-view (name)
  (interactive "sNew View name: ")
  (message "Creating view: %s" name)
  (when (tab2-get-view-by-name name)
    (error "View %s already exists" name))
  (setq tab2-views (append tab2-views (list (make-tab2-view :name name))))
  ;; Save the current wc
  (tab2-save-view-state)
  ;; Switch over
  (set-window-parameter nil 'tab2-linesel-view (- (length tab2-views) 1))
;;  (setq tab2-current-view (- (length tab2-views) 1))
  ;; Switch to an initial scratch buffer
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (force-mode-line-update))

(defun tab2-prompt-new-view ()
  (interactive)
  (let ((name (tab2-quick-command "New view name:")))
    (when name (tab2-new-view name))))

(defvar-keymap tab2-new-view-keymap
  "<tab-line> <mouse-1>" 'tab2-prompt-new-view
  "RET" #'tab2-new-view)

(setq tab2-new-view-button
  (propertize " + "
              'display '(image :type xpm
                               :file "tabs/new.xpm"
                               :height (0.8 . em)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab2-new-view-keymap
              'mouse-face 'tab-line-highlight
              'help-echo "Click to add a view"))

;; View enumeration by name.
(defun tab2-list-views ()
  (interactive)
  (mapcar 'tab2-view-name tab2-views))

;; Switch between views
(defun tab2-switch-view-by-name (name)
  (interactive (list (completing-read "Switch to view: " (tab2-list-views))))
  (let* ((new-view (tab2-get-view-by-name name))
	(new-view-pos (cl-position new-view tab2-views))
	(old-view (tab2-get-current-view)))

    (when (not new-view)
      (error (format "No view was found named %s" name)))

    (when (and new-view (not (equal new-view old-view)))
      (progn
	(message "switch to %s" name)
	(tab2-save-view-state)
	(set-frame-parameter nil 'tab-line-sel-view new-view-pos)
	(when (tab2-view-wc new-view)
	  (set-window-configuration (tab2-view-wc new-view)))))))

;; Close a view and revert back to default
(defun tab2-close-view-by-name (name)
  (interactive (list (completing-read "Close view: " (tab2-list-views))))
  (let* ((closing-view (tab2-get-view-by-name name))
	 (current-view (tab2-get-current-view)))
    (when (not closing-view)
      (error (format "No view was found named %s" name)))

    (when (equal (tab2-view-name closing-view) "default")
      (error (format "You cannot close the default view" name)))

    ;; temptemp
    (message "Closing view %s" name)

    ;; switch to default if current is closing
    (when (equal closing-view current-view)
      (progn
	(tab2-switch-view-by-name "default")
	(set-frame-parameter nil 'tab-line-sel-view 0)))

    ;; Finally remove the closing-view from the view list
    (setq tab2-views (remove closing-view tab2-views))
  ))

;; Switch to the next view in the list
(defun tab2-next-view()
  (interactive)
  (let* ((oldpos (or (frame-parameter nil 'tab-line-sel-view) 0))
	 (newpos (mod (+ 1 oldpos) (length tab2-views)))
	 (new-view-name (tab2-view-name (nth newpos tab2-views))))

;;    (message "next %s to %s:%s" oldpos newpos  new-view-name)
    (tab2-switch-view-by-name  new-view-name)))

;; Switch to the prev view in the list
(defun tab2-prev-view()
  (interactive)
  (let* ((oldpos (or (frame-parameter nil 'tab-line-sel-view) 0))
	 (newpos (mod (- oldpos 1) (length tab2-views)))
	 (new-view-name (tab2-view-name (nth newpos tab2-views))))

;;    (message "prev %s to %s:%s" oldpos newpos new-view-name)
    (tab2-switch-view-by-name  new-view-name)))

;; categorize a buffer's mode
(defun tab2-buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defvar tab2-white-list-modes
  '(text-mode lisp-interaction-mode messages-buffer-mode)
  "List of modes that will be tracked even if the buffer is not a file")

(defvar tab2-white-list-buffer-names
  '()
  "List of buffer names that will be tracked even if the buffer is not a file")

;; Filter which buffers we'll track in the tabs
(defun tab2-buffer-filter (buffer)
  (or (buffer-file-name buffer)
      (member (tab2-buffer-mode buffer) tab2-white-list-modes)
      (member (buffer-name buffer) tab2-white-list-buffer-names)))

;; Function that returns the current buffer-list.
;; This filters out all special buffers and leaves only files and whitelisted buffers by mode
;; Adds on the current buffer its filters and isn't in the list
(defun tab2-get-filtered-buffer-list ()
  (let* ((current-bufs (seq-filter 'tab2-buffer-filter (buffer-list)))
	 (intersect-tabs (seq-filter (lambda (it) (member it current-bufs)) (tab2-get-buffer-list)))
	 (new-tab (when (and (not (member (current-buffer) (tab2-get-buffer-list)))
			     (member (current-buffer) current-bufs))
		    (current-buffer))))
    (if new-tab
	(tab2-set-buffer-list (append intersect-tabs (list new-tab)))
      (tab2-set-buffer-list intersect-tabs))))

;; Filter special buffers from tab-line maintaining current order
(setq tab-line-tabs-function 'tab2-get-filtered-buffer-list)

;; tabs may just be a buffer or an alist depending on the mode.
(defun tab2-get-buffer-from-tab (tab)
  (if (bufferp tab) tab (alist-get 'buffer tab)))

;; Custom shift tab right tab2-shift-tab-right (&optional event)
(defun tab2-shift-tab-right (&optional event)
"Shift current buffer or selected tab right"
  (interactive (list last-nonmenu-event))
;;  (when (tab-line-track-tap event)
    (let* ((posnp (and (listp event)
                       (event-start event)))
           (window (and posnp (posn-window posnp)))
           (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
           (buffer (tab2-get-buffer-from-tab tab))
	   (n (seq-position (tab2-get-buffer-list) buffer)))
      (with-selected-window (or window (selected-window))

	(progn
          (tab2-set-buffer-list
              (append
               (seq-take (tab2-get-buffer-list) n)
               (list (elt (tab2-get-buffer-list) (+ n 1)))
               (list (elt (tab2-get-buffer-list) n))
               (seq-drop (tab2-get-buffer-list) (+ n 2)))))))
    (force-mode-line-update))

;; Custom tab shift left
(defun tab2-shift-tab-left (&optional event)
  "Shift current buffer or selected tab left"
  (interactive (list last-nonmenu-event))
;;  (when (tab-line-track-tap event)
    (let* ((posnp (and (listp event)
                       (event-start event)))
           (window (and posnp (posn-window posnp)))
           (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
	   (buffer (tab2-get-buffer-from-tab tab))
	   (n (seq-position (tab2-get-buffer-list) buffer)))
      (with-selected-window (or window (selected-window))

    (when
        (> n 0)
      (progn
        (tab2-set-buffer-list
              (append
               (seq-take (tab2-get-buffer-list) (- n 1))
               (list (elt (tab2-get-buffer-list) n))
               (list (elt (tab2-get-buffer-list) (- n 1)))
               (seq-drop (tab2-get-buffer-list) (+ n 1)))))))
    (force-mode-line-update)))

;; copy current buffer name to kill ring
(defun tab2-copy-current-filename-to-clipboard (&optional e)
  (interactive "e" )
  (let* ((posnp (event-start e))
         (tab (get-pos-property 1 'tab (car (posn-string posnp))))
	 (buffer (tab2-get-buffer-from-tab tab)))

    (kill-new (buffer-file-name buffer))))

;; Override original definition of tab-line-tab-context-menu
;; Bind custom actions into the context menu if we are not in group mode
(define-advice tab-line-tab-context-menu (:override (&optional event))
  "Pop up the context menu for a tab-line tab."
  (interactive "e")

  (unless (window-parameter nil 'tab-line-groups)
    (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))
      (define-key-after menu [close]
	'(menu-item "Close" tab-line-close-tab :help "Close the tab"))
      (define-key-after menu [shiftleft]
	'(menu-item "Shift Left" tab2-shift-tab-left :help "Shift the tab left"))
      (define-key-after menu [shiftright]
	'(menu-item "Shift Right" tab2-shift-tab-right :help "Shift the tab right"))
      (define-key-after menu [copypath]
	'(menu-item "Copy Path" tab2-copy-current-filename-to-clipboard :help "Copy the current tab filename to the clipboard"))
      (define-key-after menu [reorder-lru]
	'(menu-item "Reorder tabs by usage" tab2-reorder-buffers-by-usage :help "Reorder the tabs based on their usage in the buffer-list"))

      (popup-menu menu))))

(defun tab2-buffer-in-multiple-viewsp (buffer)
  (let ((count 0))
    (dolist (view tab2-views)
      (when (member buffer (tab2-view-buffers view)) (setq count (+ 1 count))))

    (> count 1)))


;; Logic to carefully close a tab. buffer is only killed if its the last one.
(defun tab2-close-tab (&optional e)
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (tab (get-pos-property 1 'tab (car (posn-string posnp))))
	 (buffer (tab2-get-buffer-from-tab tab)))

    (if (tab2-buffer-in-multiple-viewsp buffer)
	;; The buffer is in other views so only:
	;; remove this buffer from the buffer list for this view and switch
	(let* ((view (tab2-get-current-view))
	      (new-buffers (remove buffer (tab2-view-buffers view))))

	  (setf (tab2-view-buffers view) new-buffers)

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
	 (buffer (tab2-get-buffer-from-tab tab))
	 (close (unless (bufferp tab) (alist-get 'close tab))))

    ;; User the specified close function if specified or default to tab2-close-tab
    (if (functionp close) (funcall close e) (tab2-close-tab e))
    (force-mode-line-update)))

;; todo may need to force update more expansively for multiple windows when split
(defun tab2-mouse-move-tab (event)
  "Move a tab to a different position on the tab line.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event."
  (interactive "e")
  (let* ((from-str (posn-string (event-start event)))
         (to-str (posn-string (event-end event)))
	 (from-rowcol (posn-col-row (event-start event)))
	 (to-rowcol (posn-col-row (event-end event)))
	 (from (tab-line--get-tab-property 'tab (car from-str)))
         (to (tab-line--get-tab-property 'tab (car to-str))))

;;    (message "move %s p:%s to %s p:%s" from-str (car from-rowcol) to-str (car to-rowcol))

    ;; Only adjust if the two tabs are different
    ;; if going left to right add on the right and vice versa if going right to left
    (unless (or (eq from to) (eq from t) (eq to t))
      (tab2-set-buffer-list
	    (reverse (let (value)
		       (dolist (elt (tab2-get-buffer-list) value)
			 ;; add the element in its new position moving leftwards
			 (if (and (equal elt (tab2-get-buffer-from-tab to)) (> (car from-rowcol) (car to-rowcol)))
			     (setq value (cons (tab2-get-buffer-from-tab from) value)))
			 ;; add all other elements in old position
			 (if (not (equal elt (tab2-get-buffer-from-tab from)))
			     (setq value (cons elt value)))
			 ;; add the element in its new position moving rightwards
			 (if (and (equal elt (tab2-get-buffer-from-tab to)) (>= (car to-rowcol) (car from-rowcol)))
			     (setq value (cons (tab2-get-buffer-from-tab from) value)))
			 ))))
      (force-mode-line-update))))

;; Add on our extra key map for the drag event
(keymap-set tab-line-tab-map   "<tab-line> <drag-mouse-1>"      #'tab2-mouse-move-tab)

;; Advice to add on add-new view button in the view view
(define-advice tab-line-format (:filter-return (format))
  (if (window-parameter nil 'tab-line-views)
      (append format (list tab2-new-view-button))
    format))

(my-ignore (advice-remove 'tab-line-format nil))

;; Convenience wrapper for getting the git state which needs to be done in buffer
;; but is  more accurate than vc-state
(defun tab2-git-state (buffer)
  (with-current-buffer buffer
    (vc-git-state (buffer-file-name buffer))))

;; Custom tab-line-name-format function to add on a face for the modified signifier
;; so it can be colored or not depending on being selected
;; and filter icon to the first tab
(defun tab2-format-tab (tab tabs)
  "Override for  `tab-line-tab-name-format-function' that adds on a modified buffer face and indicator"
  (let* ((buffer (tab2-get-buffer-from-tab tab))
         (selected-p (if buffer
                         (eq buffer (window-buffer))
                       (cdr (assq 'selected tab))))

         (name (if buffer
                   (funcall tab-line-tab-name-function buffer tabs)
                 (cdr (assq 'name tab))))
         (face (if selected-p
 ;;                  (if (mode-line-window-selected-p)
                       'tab-line-tab-current
   ;;                  'tab-line-tab)
                 'tab-line-tab-inactive))
	 (group-view (window-parameter nil 'tab-line-groups)))

    ;; the face funcs expect to get called with whether the tab is a buffer
    ;; so we can't use buffer directly here
    (dolist (fn tab-line-tab-face-functions)
      (progn
	(setf face (funcall fn tab tabs face (bufferp tab) selected-p))))

    (apply 'propertize
           (concat
	    (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                               'face face
                               'keymap tab-line-tab-map
                               'help-echo (if selected-p "Current tab"
                                            "Click to select tab")
                               ;; Don't turn mouse-1 into mouse-2 (bug#49247)
                               'follow-link 'ignore)

	    ;; Modified marker - TODO - move to custom faces
	    (cond ((and buffer (buffer-modified-p buffer) (buffer-file-name buffer))
		   (if selected-p
		       (propertize (format "%s " tab2-modified-marker) 'face `(:inherit ,face :foreground "red2" :height .9 :slant normal ))
		     (propertize (format "%s " tab2-modified-marker) 'face `(:inherit ,face :height .9 :slant normal ))))

		  ((and buffer (buffer-file-name buffer)
			(string= (tab2-git-state buffer) "edited"))
		   (progn
;;		     (message "git modified: %s %s" buffer (vc-state (buffer-file-name buffer)))
		    (if selected-p
			(propertize (format "%s " tab2-git-modified-marker) 'face `(:inherit ,face :foreground "dark cyan" :height .9 :slant normal ))
		      (propertize (format "%s " tab2-git-modified-marker) 'face `(:inherit ,face :height .9 :slant normal ))))))

            (let ((close (or (and (or buffer (assq 'close tab))
                                  tab-line-close-button-show
                                  (not (eq tab-line-close-button-show
                                           (if selected-p 'non-selected
                                             'selected)))
                                  tab-line-close-button)
                             "")))

	      (setq close2 (copy-sequence close))
              ;; Don't overwrite the icon face
              (add-face-text-property 0 (length close2) face t close2)
              close2))

           `(
             tab ,tab
             ,@(if selected-p '(selected t))
             mouse-face tab-line-highlight))
    ))


;; Group of tab constructors
(defun tab2-make-group-tab (selected-group groupname)
  (let ((formatted-name   (format " %s " groupname)))
    `(tab
      (name . ,formatted-name)
      (selected . ,(equal selected-group groupname))
      (select . ,(lambda ()
                   (set-window-parameter nil 'tab-line-groups nil)
                   (set-window-parameter nil 'tab-line-group groupname)
                   (set-window-parameter nil 'tab-line-hscroll nil))))))

(defun tab2-make-view-category-tab (selected-view viewname)
  (let ((formatted-name   (format " %s " viewname)))
    `(tab
      (name . ,formatted-name)
      (selected . ,(equal selected-view viewname))
      (select . ,(lambda ()
                   (set-window-parameter nil 'tab-line-views t)
                   (set-window-parameter nil 'tab-line-hscroll nil))))))

(defun tab2-make-view-tab (selected-view viewname)
  (let* ((formatted-name (format " %s " viewname))
	 (new-view (tab2-get-view-by-name viewname))
	 (new-view-pos (cl-position new-view tab2-views)))

    (if (equal viewname "default")
	`(tab
	  (name . ,formatted-name)
	  (selected . ,(equal selected-view viewname))
	  (select . ,(lambda ()
		       (tab2-switch-view-by-name viewname)
		       (set-frame-parameter nil 'tab-line-sel-view new-view-pos)
                       (set-window-parameter nil 'tab-line-hscroll nil))))

	`(tab
	  (name . ,formatted-name)
	  (selected . ,(equal selected-view viewname))
	  (close . ,(lambda (&optional b)
		      (tab2-close-view-by-name viewname)))
	  (select . ,(lambda ()
		       (tab2-switch-view-by-name viewname)
		       (set-frame-parameter nil 'tab-line-sel-view new-view-pos)
                       (set-window-parameter nil 'tab-line-hscroll nil)))))))


;; predicate to determine if a buffer is modified and backed by a file
(defun tab2-buffer-modified-file-p (buffer)
  (and (buffer-file-name buffer) (buffer-modified-p buffer)))

;; Return a list of the buffers opened in the current project
;; or nil if we're not in a project
(defun tab2-get-project-buffer-list ()
  (let ((pr (project-current nil)))
    (when pr (project-buffers pr))))

;; Filter the list of buffers based on the selected group
;; if Files then return all of the file based buffers.
;; if Project then return all files in the curent project.
(defun  tab2-filter-buffers-by-group (buffers curgroup)
  (let ((project-buffers (tab2-get-project-buffer-list)))
    (cond ((not curgroup) buffers)
	  ((equal curgroup "Files")
	   (seq-filter (lambda (b) (buffer-file-name b)) buffers))

	  ((equal curgroup "Project")
	   (seq-filter (lambda (b) (member b project-buffers)) buffers))

	  ((equal curgroup "Modified")
	   (seq-filter (lambda (b) (tab2-buffer-modified-file-p b)) buffers))

	  (t (seq-filter (lambda (b)
			   (equal (tab-line-tabs-buffer-group-name b) curgroup))
			 buffers)))))

(defun find-first (fn ls)
  (cond ((null ls) nil)
        ((funcall fn (car ls)) (car ls))
        (t (find-first fn (cdr ls)))))

;; Setup the list of tabs to be displayed.
;; Note: these will returned in the tab list format
;; tab2-format-tab is then responsible for formatting each one i.e faces / close icons etc.
(defun tab2-get-tabs ()
  "Return a list of tabs that should be displayed in the tab line.
  This version is based on tab-line-tabs-buffers-groups but
  changes first group to an icon and adds on all files"

  (cond
   ;; views view
   ((window-parameter nil 'tab-line-views)
    (let* ((selected-view (tab2-view-name (tab2-get-current-view)))
	   (tabs (mapcar (lambda (view)
			   (tab2-make-view-tab selected-view (tab2-view-name view)))
			 tab2-views)))
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
	   (tabs (mapcar (apply-partially 'tab2-make-group-tab selected-group) groups)))

      ;;insert a Files and Project group
      (append
       (when (project-current nil)
	 (list (tab2-make-group-tab selected-group "Project")))
       (list (tab2-make-group-tab selected-group "Files"))
       ;; Insert a modified group if any files are modified

       (when (find-first 'tab2-buffer-modified-file-p buffers)
	 (list (tab2-make-group-tab selected-group "Modified")))
       tabs
       (list (tab2-make-view-category-tab
	      (tab2-view-name (tab2-get-current-view)) "Views")))))

   ;; default tabs view
   (t (let* (
	     (selected-group (window-parameter nil 'tab-line-group))
	     (all-buffers (funcall tab-line-tabs-buffer-list-function))
	     (buffers (tab2-filter-buffers-by-group all-buffers selected-group))
	     (sorted-buffers (if (functionp tab-line-tabs-buffer-group-sort-function)
				 (seq-sort tab-line-tabs-buffer-group-sort-function
					   buffers)
			       buffers))
	     (tabs (mapcar (lambda (buffer)
			     `(tab
			       (name . ,(funcall tab-line-tab-name-function buffer))
			       (selected . ,(eq buffer (current-buffer)))
			       (buffer . ,buffer)
			       (close . tab2-close-tab)))
			   sorted-buffers)))

;;	(message "chk g:%s b:%s" selected-group buffers)
	tabs))))

;; If the current window is in a valid group that is not currently
;; set - switch the selected group to match it.
;; TODO: make configurable?
(defun tab2-auto-track-selected-window (&optional _frame)
  (let ((bufgroup (tab-line-tabs-buffer-group-name (current-buffer)))
	(filep (buffer-file-name (current-buffer)))
	(valid (tab2-buffer-filter (current-buffer)))
	(selected-group (window-parameter nil 'tab-line-group)))

;;    (message "autotrack: %s %s %s %s" bufgroup filep valid selected-group)

    (when (or (and (not selected-group) valid)
	      (and selected-group
		   valid
		   (not (equal bufgroup selected-group))
		   (not (and filep (equal selected-group "Project") (project-current nil)))
		   (not (and filep (equal selected-group "Files")))))

      (set-window-parameter nil 'tab-line-group bufgroup))))


;; Setup the window buffer changes functions to monitor when a window is selected
;; Hook that up to the auto-track function
(setq window-buffer-change-functions (cons 'tab2-auto-track-selected-window window-buffer-change-functions))

;; Set the override tab name format function to the one I've defined
(setopt tab-line-tab-name-format-function 'tab2-format-tab)

;; Use custom tab grouping function.
(setq tab-line-tabs-function 'tab2-get-tabs)

;; Set the list-function to use the same one I'm overriding in basic mode
(setq tab-line-tabs-buffer-list-function 'tab2-get-filtered-buffer-list)

;;; Serialization
(cl-defstruct tab2-persist-view name buffernames)

;; serialize a view
(defun tab2-convert-to-persist-format (view)
  (let ((name (tab2-view-name view))
	(buffers (mapcar (lambda (it) (buffer-name it)) (tab2-view-buffers view))))

    (make-tab2-persist-view :name name :buffernames buffers)))

;; serialize all views
(defun tab2-serialize-views ()
  (mapcar (lambda (it) (tab2-convert-to-persist-format it)) tab2-views))

;; Deserialize a view
(defun tab2-convert-from-persist-format (view)
  (let ((name (tab2-persist-view-name view))
	(buffers (mapcar (lambda (it) (get-buffer it)) (tab2-persist-view-buffernames view))))
    (message "deserialized %s" name)
    (make-tab2-view :name name :buffers buffers)))

(defun tab2-deserialize-views (views)
  (mapcar (lambda (it) (tab2-convert-from-persist-format it)) views))

;; persist bufnames to frame in order for reload
(defun tab2-save-to-frame ()
  (set-frame-parameter nil 'tab2-views (tab2-serialize-views)))

;; process the tab-buffer list in the frame parameters and set up to match
(defun tab2-rebuild-buffer-list-from-frame-params ()
  (let* ((serialized-views (frame-parameter nil 'tab2-views))
	 (views (tab2-deserialize-views serialized-views)))
    (when views
      (setq tab2-views views))
    (force-mode-line-update)))

;; hook the rebuild function to run after read desktop
(add-hook 'desktop-after-read-hook 'tab2-rebuild-buffer-list-from-frame-params)
(add-hook 'desktop-save-hook 'tab2-save-to-frame)

;;(remove-hook 'desktop-save-hook 'tab2-save-to-frame)

;; Preserve the directory where the script was loaded from for
;; use in doing relative open of resource files
(setq tab2-load-dir (file-name-directory load-file-name))

(setq tab2-load-dir "~/.emacs.d/")

;; Add on filter button at the front of the tab list via advice to
;; tab-line-format-template
(defun tab2-add-on-filter-button (tabs)
  (let* ((group-view (window-parameter nil 'tab-line-groups))
	 (viewp (window-parameter nil 'tab-line-views))
	 (icon-file (if viewp "desktop2.png" "funnel4.png"))
	 (icon-name (format "%s%s" tab2-load-dir icon-file)))

    ;; Only prepend the filter button when in buffer-groups mode
    (if (eq  tab-line-tabs-function 'tab2-get-tabs)
	(cons
	 (propertize "▼"
		     'face '(:box (:line-width (0 . 4) :color "#DED8C5" ))
		     ;; todo: change me.
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


;; keymap action for when the filter button is selected
;; Swaps between group and file mode
(defun tab2-select-filter-button (&optional _event)
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
  "<tab-line> <mouse-1>" #'tab2-select-filter-button
  "RET" #'tab2-select-filter-button)

;; Via advice add on the filter button
(advice-add 'tab-line-format-template :filter-return 'tab2-add-on-filter-button)

;;; Doom modeline integration

(defvar-keymap tab2-mode-line-view-map
  "<mode-line> <mouse-3>" #'tab2-next-view
  "<mode-line> <mouse-1>" #'tab2-prev-view)

(doom-modeline-def-segment tab2-view-segment
  "show the current tab2 view"
  (propertize (concat
	       "    "
	       (tab2-view-name (tab2-get-current-view)))
	      'help-echo "Current tab view - click to switch to the next one"
	      'local-map tab2-mode-line-view-map
	      ))

(doom-modeline-def-modeline 'tab2-aware-modeline
  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  '(misc-info minor-modes input-method major-mode process vcs checker tab2-view-segment))

;; TODO: move out into main init.el

;; Set once on start
(doom-modeline-set-modeline 'tab2-aware-modeline 'default)

;; Hook in after that.
(add-hook 'doom-modeline-mode-hook
          (lambda ()
            (doom-modeline-set-modeline 'tab2-aware-modeline 'default)))

;; bunch of code from nicolas rougier adapted to use the header-line
(defface quick-command-face
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-background 'highlight nil t)
       :height ,(face-attribute 'default :height)
       :box '(:line-width (1 . 1)
              :color ,(face-foreground 'default)
              :style none)))
  "Face for quick command")

(defface quick-command-prompt-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :weight ,(face-attribute 'bold :weight nil 'default)
       :box (:line-width (1 . 1)
             :color ,(face-foreground 'default)
             :style none)))
  "Face for prompt")

(defface quick-command-cursor-face
  `((t :foreground ,(face-background 'default)
       ;; :background "#d0d0d0"
       :background ,(face-foreground 'default)
       :box (:line-width (1 . 1)
             :color ,(face-foreground 'default)
             :style none)))
  "Face for cursor")

(defun tab2-quick-command--update (current-buffer command-buffer)
  "Update header-line with current command"

  (with-current-buffer command-buffer
    (let* ((text (concat (buffer-substring (point-min) (point-max)) " "))
           (point (point))
           (region-beg (if (use-region-p) (- (region-beginning) 1)))
           (region-end (if (use-region-p) (region-end))))
      (add-face-text-property (- point 1) point 'quick-command-cursor-face t text)
      (with-current-buffer current-buffer
        (setq-local header-line-format text))
      (force-mode-line-update))))

(defun tab2-quick-command (&optional prompt)
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
            (tab2-quick-command--update current-buffer command-buffer)
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
                (condition-case error
                    (execute-kbd-macro (kbd key))
                  ((beginning-of-buffer end-of-buffer text-read-only)))
                ;; Make sure to not go into prompt area
                (goto-char (max (+ (length prompt) 1) (point)))

                (set-window-buffer current-window current-buffer t)

                ;; Update mode line
                (tab2-quick-command--update current-buffer command-buffer)))))

      ;; Command entered or aborted: restore mode line
      (with-current-buffer current-buffer
        (setq-local header-line-format saved-mode-line)
        (force-mode-line-update))
      (kill-buffer command-buffer)
      (switch-to-buffer current-buffer))))
