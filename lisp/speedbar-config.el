;;; speedbar-config.el --- Speedbar customizations -*- lexical-binding: t -*-

;;; Commentary:

;; Everything that tweaks the built in speedbar: window mode fixes, nerd-icons glyphs, the git
;; tracked "project view" (`P'), mode line, tooltips, scrolling and key bindings.  All of it is
;; deferred until speedbar itself loads.  Needs modeline.el loaded first for the dedicated-window
;; mode line hooks.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'project)
  (require 'speedbar)
  (require 'nerd-icons nil t))

(defvar my-modeline-format)
(defvar my-modeline-dedicated-window-modes)
(defvar my-modeline-dedicated-label-functions)
(defvar my-modeline-dedicated-help-functions)
(declare-function my-modeline--icon-safe "modeline")
(declare-function my-modeline-dedicated-line "modeline")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function dframe-close-frame "dframe")
(declare-function dframe-message "dframe")
(declare-function speedbar-add-supported-extension "speedbar")
(declare-function speedbar-change-initial-expansion-list "speedbar")
(declare-function speedbar-directory-line "speedbar")
(declare-function speedbar-expand-line "speedbar")
(declare-function speedbar-find-selected-file "speedbar")
(declare-function speedbar-frame-or-window "speedbar")
(declare-function speedbar-item-info "speedbar")
(declare-function speedbar-line-file "speedbar")
(declare-function speedbar-mode "speedbar")
(declare-function speedbar-refresh "speedbar")
(declare-function speedbar-set-mode-line-format "speedbar")
(declare-function speedbar-timer-fn "speedbar")
(declare-function speedbar-window--close "speedbar")
(declare-function speedbar-window--live-p "speedbar")

;; The builtin simple icons actually work better than this package:
;; (use-package nerd-icons-speedbar
;;   :ensure (:host "github" :repo "Akane-6730/nerd-icons-speedbar")
;;   :after speedbar
;;   :config
;;   (nerd-icons-speedbar-mode 1))

;;; Definitions
;;
;; Defined at top level, rather than inside the `with-eval-after-load' forms
;; below, so the byte compiler can see them.

;; Emacs 31's `speedbar-window-mode' never runs `speedbar-mode' in its buffer
;; (unlike `speedbar-frame-mode'). Fix that so it runs normally.
(defun my-speedbar-window-ensure-mode (&rest _)
  (unless (buffer-live-p speedbar-buffer)
    (with-current-buffer
        (setq speedbar-buffer (get-buffer-create speedbar--buffer-name))
      (speedbar-mode))))

(defun my-speedbar--file-type (file)
  "Describe FILE's type: \"directory\", else its mode or extension.
Tries the major mode `auto-mode-alist' picks, then the bare extension."
  (if (file-directory-p file)
      "directory"
    (let* ((mode (assoc-default file auto-mode-alist #'string-match))
           (mode (if (consp mode) (car mode) mode))
           (ext (file-name-extension file)))
      (or (and mode (symbolp mode)
               (not (eq mode 'ignore))
               (upcase-initials
                (replace-regexp-in-string
                 "-" " "
                 (replace-regexp-in-string
                  "\\(-ts\\)?-mode\\'" "" (symbol-name mode)))))
          (and ext (concat "." ext " file"))
          "unknown"))))

(defun my-speedbar-help-echo ()
  "Return the item info, plus file type, for the speedbar row at point."
  (let ((info (speedbar-item-info))
        (file (speedbar-line-file)))
    (if (and info file)
        (format "%s\nType: %s" info (my-speedbar--file-type file))
      info)))

(defun my-speedbar-describe-keymap ()
  "Pop up the key bindings active in this speedbar buffer.
Lists each command with the first line of its docstring.  Uses the current
local map rather than `speedbar-mode-map', which lacks the display-specific
bindings (files, buffers, ...)."
  (interactive)
  (let ((seen (make-hash-table :test #'equal))
        (rows nil))
    (cl-labels
        ((collect (map prefix)
           (map-keymap
            (lambda (event binding)
              (when (or (characterp event) (symbolp event))
                (let* ((keys (vconcat prefix (vector event)))
                       (desc (key-description keys)))
                  (unless (gethash desc seen)
                    (puthash desc t seen)
                    (cond
                     ((keymapp binding) (collect binding keys))
                     ((and (symbolp binding) (commandp binding)
                           (not (eq binding 'undefined)))
                      (push (list desc binding) rows)))))))
            map)))
      (collect (current-local-map) []))
    (with-help-window "*Speedbar Keys*"
      (dolist (row (sort rows (lambda (a b) (string< (car a) (car b)))))
        (let* ((command (cadr row))
               (doc (ignore-errors (documentation command t))))
          (princ (format "%-22s %-36s %s\n" (car row) command
                         (if doc (car (split-string doc "\n")) ""))))))))

(defun my-speedbar-quit (&optional _window)
  "Close speedbar, whether it is displayed in a window or a frame.
The optional window is ignored; `my-modeline' passes one on a mode-line click."
  (interactive)
  (if (eq (speedbar-frame-or-window) 'window)
      (speedbar-window--close)
    (dframe-close-frame)))

;; Speedbar binds b, f and r to anonymous lambdas, which show up as
;; #<byte-code-function> in key listings; give them names.
(defun my-speedbar-show-buffers ()
  "Switch speedbar to the quick buffers display."
  (interactive)
  (speedbar-change-initial-expansion-list "quick buffers"))

(defun my-speedbar-show-files ()
  "Switch speedbar to the files display."
  (interactive)
  (speedbar-change-initial-expansion-list "files"))

(defun my-speedbar-show-previous-display ()
  "Switch speedbar back to the previously used display."
  (interactive)
  (speedbar-change-initial-expansion-list
   speedbar-previously-used-expansion-list-name))

;; Append the file's `vc-state' to the echo-area info shown for a file line.
(defun my-speedbar-item-info-file (&optional filename)
  "Like `speedbar-item-info-file-helper', but also show the `vc-state'."
  (let* ((item (or filename (speedbar-line-file)))
         (attr (and item (file-attributes item))))
    (when attr
      (let ((state (and (not (file-remote-p item))
                        (not (file-directory-p item))
                        (ignore-errors (vc-state item)))))
        (dframe-message "%s %-6d %s%s"
                        (file-attribute-modes attr)
                        (file-attribute-size attr)
                        item
                        (if state (format "  [vc: %s]" state) ""))))))

;; Replace the folder and file XPM images with nerd-icons glyphs
(defface my-speedbar-dir-icon '((t :inherit dired-directory))
  "Face for speedbar directory glyphs."
  :group 'speedbar)

(defface my-speedbar-file-icon '((t :inherit default :height 1.2))
  "Face for speedbar file glyphs."
  :group 'speedbar)

(defun my-speedbar-glyph (orig start length)
  "Show a nerd-icons folder or file for its marker, else defer to ORIG."
  (let* ((text (buffer-substring-no-properties start (+ start length)))
         (spec (pcase text
                 ("<+>" '(nerd-icons-mdicon "nf-md-folder" my-speedbar-dir-icon))
                 ("<->" '(nerd-icons-mdicon "nf-md-folder_open" my-speedbar-dir-icon))
                 ("< >" '(nerd-icons-mdicon "nf-md-folder_outline" my-speedbar-dir-icon))
                 ((or "[ ]" "[?]") '(nerd-icons-mdicon "nf-md-file" my-speedbar-file-icon))
                 ((or "[+]" "[-]")
                  '(nerd-icons-mdicon "nf-md-note_plus" my-speedbar-file-icon)))))
    (if (and spec (require 'nerd-icons nil t))
        (add-text-properties
         start (+ start length)
         (list 'display (funcall (nth 0 spec) (nth 1 spec) :face (nth 2 spec))
               'face nil
               'rear-nonsticky '(display face)))
      (funcall orig start length))))

;; Project view: only the git-tracked files of the current project, rooted
;; at the project root.  `P' enters it; `f' (or any other view) leaves it.
(defvar my-speedbar-project-view-p nil
  "Non-nil while speedbar shows only the tracked files of a project.")

(defvar my-speedbar--project-root nil
  "Root directory shown by the speedbar project view.")

(defvar my-speedbar--tracked nil
  "Cache of (ROOT FILES DIRS) hash tables for the tracked files under ROOT.")

(defvar my-speedbar--git-status nil
  "Cache of (ROOT . HASH) mapping absolute file names under ROOT to a git state.")

(defun my-speedbar--root-of (dir)
  "Return the project root containing DIR, or nil."
  (when-let* ((pr (and dir (not (file-remote-p dir))
                       (project-current nil dir))))
    (expand-file-name (project-root pr))))

(defun my-speedbar--tracked (root)
  "Return (ROOT FILES DIRS) for the VC-tracked files under ROOT, or nil."
  (unless (equal (car my-speedbar--tracked) root)
    (let ((files (make-hash-table :test #'equal))
          (dirs (make-hash-table :test #'equal))
          (pr (project-current nil root))
          (project-vc-include-untracked nil))
      (when (eq (car-safe pr) 'vc)
        (dolist (file (project-files pr))
          (puthash file t files)
          (let ((dir (file-name-directory file)))
            (while (and (length> dir (length root))
                        (not (gethash dir dirs)))
              (puthash dir t dirs)
              (setq dir (file-name-directory (directory-file-name dir)))))))
      (setq my-speedbar--tracked
            (and (> (hash-table-count files) 0) (list root files dirs)))))
  my-speedbar--tracked)

(defun my-speedbar-project-file-lists (orig directory)
  "In the project view, drop untracked files and directories from ORIG's result."
  (let ((result (funcall orig directory))
        (tracked (and my-speedbar-project-view-p
                      (my-speedbar--tracked my-speedbar--project-root))))
    (if (not tracked)
        result
      (let ((dir (file-name-as-directory (expand-file-name directory))))
        (cons (seq-filter (lambda (d) (gethash (concat dir d "/") (nth 2 tracked)))
                          (car result))
              (list (seq-filter (lambda (f) (gethash (concat dir f) (nth 1 tracked)))
                                (cadr result))))))))

(defun my-speedbar--project-switch-root ()
  "Return the project root to draw for the current buffer, or nil to do nothing.
Nil when the buffer is outside any project or in the project already shown."
  (let ((root (my-speedbar--root-of default-directory)))
    (and root
         (not (and (equal root my-speedbar--project-root)
                   (seq-some (lambda (d) (string-prefix-p root d))
                             speedbar-shown-directories)))
         root)))

(defun my-speedbar-project-update-contents (orig &rest args)
  "In the project view, always draw the root of the buffer's project."
  (cond ((not my-speedbar-project-view-p)
         (apply orig args))
        ((eq (current-buffer) speedbar-buffer)
         ;; Directory navigation (`speedbar-dir-follow', `speedbar-up-directory')
         ;; sets our `default-directory'; honor it unless it leaves the project.
         ;; A `setq', not a `let': speedbar resolves row paths against this buffer's
         ;; `default-directory', so it must stay at the directory drawn.
         (unless (string-prefix-p my-speedbar--project-root
                                  (expand-file-name default-directory))
           (setq default-directory my-speedbar--project-root))
         (apply orig args))
        ((when-let* ((root (my-speedbar--project-switch-root)))
           (setq my-speedbar--project-root root)
           (let ((default-directory root))
             (apply orig args))))))

(defun my-speedbar-project-update-localized (orig &rest args)
  "In the project view, ignore buffer switches that stay in the shown project."
  (when (or (not my-speedbar-project-view-p)
            (eq (current-buffer) speedbar-buffer)
            (my-speedbar--project-switch-root))
    (apply orig args)))

(defun my-speedbar--project-mode-line ()
  "Mode-line text naming the project shown by the project view."
  (my-modeline-dedicated-line
   (concat (propertize "project: " 'face '(:inherit mode-line-emphasis :slant normal))
           (propertize (file-name-nondirectory
                        (directory-file-name my-speedbar--project-root))
                       'face '(:inherit mode-line-buffer-id :slant normal)))))

(defun my-speedbar-project-mode-line (orig &rest args)
  "In the project view, show the project name instead of speedbar's own line."
  (if (not my-speedbar-project-view-p)
      (apply orig args)
    (when (buffer-live-p speedbar-buffer)
      (with-current-buffer speedbar-buffer
        (setq mode-line-format '(:eval (my-speedbar--project-mode-line)))
        (force-mode-line-update)))))

(defun my-speedbar-project-view-exit (&rest _)
  "Leave the project view before speedbar switches to another view."
  (when my-speedbar-project-view-p
    (setq my-speedbar-project-view-p nil
          speedbar-full-text-cache nil
          speedbar-shown-directories nil)))

(defun my-speedbar-project-refresh (&rest _)
  "Forget the cached list of tracked files so a refresh sees new ones."
  (setq my-speedbar--tracked nil
        my-speedbar--git-status nil))

;; Color file names by git state.  Overlays, so they layer over the current-file
;; highlight speedbar puts on the name with text properties.
(defface my-speedbar-file-modified '((t :inherit warning))
  "Face for modified files in the speedbar project view."
  :group 'speedbar)

(defface my-speedbar-file-added '((t :inherit success))
  "Face for newly added files in the speedbar project view."
  :group 'speedbar)

(defface my-speedbar-file-conflict '((t :inherit error))
  "Face for files with merge conflicts in the speedbar project view."
  :group 'speedbar)

;; Background-only variants for the current file, so `speedbar-selected-face'
;; keeps supplying the foreground.
(defface my-speedbar-file-modified-selected '((t nil))
  "Background of `my-speedbar-file-modified' on the current file."
  :group 'speedbar)

(defface my-speedbar-file-added-selected '((t nil))
  "Background of `my-speedbar-file-added' on the current file."
  :group 'speedbar)

(defface my-speedbar-file-conflict-selected '((t nil))
  "Background of `my-speedbar-file-conflict' on the current file."
  :group 'speedbar)

(defconst my-speedbar--status-faces
  '((modified bg-changed my-speedbar-file-modified my-speedbar-file-modified-selected)
    (added bg-added my-speedbar-file-added my-speedbar-file-added-selected)
    (conflict bg-removed my-speedbar-file-conflict my-speedbar-file-conflict-selected))
  "Rows of (STATE MODUS-COLOR FACE SELECTED-FACE) for the project view.")

(defun my-speedbar-update-status-faces (&rest _)
  "Recompute the project view's file state faces from the current modus palette."
  (when (fboundp 'modus-themes-get-color-value)
    (pcase-dolist (`(,_ ,color . ,faces) my-speedbar--status-faces)
      (when-let* ((bg (ignore-errors (modus-themes-get-color-value color))))
        (dolist (face faces)
          (set-face-attribute face nil :background bg :extend nil))))))

(defun my-speedbar--git-status (root)
  "Return a hash of file name -> `modified', `added' or `conflict' for ROOT.
File names are absolute."
  (unless (equal (car my-speedbar--git-status) root)
    (let ((states (make-hash-table :test #'equal))
          (default-directory root))
      (with-temp-buffer
        (when (zerop (process-file "git" nil '(t nil) nil
                                   "status" "--porcelain=v1" "-z" "--untracked-files=no"))
          (let ((entries (split-string (buffer-string) "\0" t)))
            (while entries
              (let* ((entry (pop entries))
                     (xy (substring entry 0 2))
                     (file (concat root (substring entry 3))))
                (when (string-match-p "[RC]" xy)
                  (pop entries))
                (puthash file
                         (cond ((or (string-match-p "U" xy) (member xy '("AA" "DD")))
                                'conflict)
                               ((equal xy "A ") 'added)
                               (t 'modified))
                         states))))))
      (setq my-speedbar--git-status (cons root states))))
  (cdr my-speedbar--git-status))

(defun my-speedbar--colorize (&rest _)
  "Color the file names in the project view by their git state."
  (when (and my-speedbar-project-view-p (buffer-live-p speedbar-buffer))
    (with-current-buffer speedbar-buffer
      (remove-overlays (point-min) (point-max) 'my-speedbar-status t)
      (let ((states (my-speedbar--git-status my-speedbar--project-root)))
        (unless (zerop (hash-table-count states))
          (save-excursion
            (goto-char (point-min))
            (while-let ((match (text-property-search-forward
                                'speedbar-function 'speedbar-find-file t)))
              (let* ((beg (prop-match-beginning match))
                     (file (save-excursion (goto-char beg) (speedbar-line-file)))
                     (state (and file (gethash (expand-file-name file) states)))
                     (ov (and state (make-overlay beg (prop-match-end match) nil t))))
                (when ov
                  (overlay-put ov 'my-speedbar-status t)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'priority 10)
                  (overlay-put ov 'face
                               (nth (if (memq 'speedbar-selected-face
                                              (ensure-list (get-text-property beg 'face)))
                                        3 2)
                                    (assq state my-speedbar--status-faces))))))))))))

(defun my-speedbar--colorize-after-save ()
  "Refresh the project view's git colors after a file is saved."
  (when (and my-speedbar-project-view-p (buffer-live-p speedbar-buffer)
             buffer-file-name
             (string-prefix-p my-speedbar--project-root
                              (expand-file-name buffer-file-name)))
    (setq my-speedbar--git-status nil)
    (my-speedbar--colorize)))

(defun my-speedbar-project-view ()
  "Show only the git-tracked files of the current project in speedbar."
  (interactive)
  (unless (derived-mode-p 'speedbar-mode)
    (user-error "Not in a speedbar buffer"))
  (let* ((mru (get-mru-window nil nil t))
         (root (or (and mru (my-speedbar--root-of
                             (buffer-local-value 'default-directory
                                                 (window-buffer mru))))
                   (my-speedbar--root-of default-directory)
                   (user-error "Not in a project"))))
    (setq my-speedbar--tracked nil)
    (unless (my-speedbar--tracked root)
      (user-error "No git-tracked files in %s" root))
    (unless (equal speedbar-initial-expansion-list-name "files")
      (speedbar-change-initial-expansion-list "files"))
    (setq my-speedbar--project-root root
          default-directory root
          my-speedbar-project-view-p t
          speedbar-full-text-cache nil
          speedbar-shown-directories nil)
    (speedbar-refresh)
    (speedbar-set-mode-line-format)))

(defun my-speedbar--reveal-file (file)
  "Expand the directories above FILE in the speedbar buffer; non-nil if any opened."
  (with-current-buffer speedbar-buffer
    (let ((root (file-name-as-directory (expand-file-name default-directory)))
          (dir nil)
          (opened nil))
      (when (string-prefix-p root file)
        (setq dir root)
        (dolist (part (butlast (split-string (substring file (length root)) "/" t)))
          (setq dir (concat dir part "/"))
          (unless (member dir speedbar-shown-directories)
            (when (ignore-errors (speedbar-directory-line dir))
              (speedbar-expand-line)
              (setq opened t)))))
      opened)))

(defun my-speedbar-sync-to-last-window ()
  "Update speedbar for the window that was active before it, like its idle timer.
Opens the directories above that window's file so its row is visible.
Selection stays in the speedbar."
  (interactive)
  (if-let* ((win (get-mru-window nil nil t)))
      (let ((speedbar-update-flag t)
            (file (buffer-file-name (window-buffer win))))
        (with-selected-window win (speedbar-timer-fn))
        (when (and file my-speedbar-project-view-p
                   (my-speedbar--reveal-file (expand-file-name file)))
          (setq speedbar-last-selected-file nil)
          (with-selected-window win (speedbar-timer-fn))))
    (user-error "No other window")))

;; Speedbar wraps every mode's mode line in << and >>; strip them off.
(defun my-speedbar--strip-arrows ()
  "Restyle the speedbar mode line.
The files display gets the dedicated-window mode line from modeline.el;
the others just lose the leading << and trailing >>."
  (when (and (buffer-live-p speedbar-buffer)
             (not (bound-and-true-p my-speedbar-project-view-p)))
    (with-current-buffer speedbar-buffer
      (if (equal speedbar-initial-expansion-list-name "files")
          (setq-local mode-line-format my-modeline-format)
        (when (listp mode-line-format)
          (setq-local mode-line-format
                      (mapcar (lambda (elt)
                                (if (stringp elt)
                                    (replace-regexp-in-string
                                     "\\`<<\\s-*\\|\\s-*>>\\'" "" elt)
                                  elt))
                              mode-line-format))))
      (force-mode-line-update))))

;; Style the root directory path at the top like a header: boxed, on the
;; margin background.
(defface my-speedbar-root-face '((t :extend t))
  "Face layered over speedbar's root directory path line."
  :group 'speedbar)

(defun my-speedbar-update-root-face (&rest _)
  "Recompute `my-speedbar-root-face' from the current modus palette."
  (when (fboundp 'modus-themes-get-color-value)
    (let ((bg (modus-themes-get-color-value 'bg-margins t))
          (border (modus-themes-get-color-value 'border t)))
      (when (and (stringp bg) (stringp border))
        (set-face-attribute 'my-speedbar-root-face nil
                            :background bg
                            :box `(:line-width -1 :color ,border))))))

(defun my-speedbar--root-face (orig &rest args)
  "Call ORIG with ARGS, then layer `my-speedbar-root-face' over what it drew."
  (let ((start (point)))
    (prog1 (apply orig args)
      (add-face-text-property start (point) 'my-speedbar-root-face t))))

;; Follow the current file after a shorter idle time than `dframe-update-speed'
;; (which is global and shared with other dframe users).
(defvar my-speedbar-update-speed 0.2
  "Idle seconds before speedbar follows the current file.")

(defun my-speedbar--update-speed (args)
  "Replace the timer interval in ARGS with `my-speedbar-update-speed'."
  (if (car args) (list my-speedbar-update-speed) args))

;; In window mode the speedbar window isn't selected, so moving point to the
;; newly current file doesn't scroll it; recenter when the file isn't in view.
(defun my-speedbar--reveal (orig &rest args)
  "Call ORIG; if the current file changed, scroll speedbar to show its row."
  (let ((before speedbar-last-selected-file))
    (prog1 (apply orig args)
      (when (and speedbar-last-selected-file
                 (not (equal before speedbar-last-selected-file))
                 (speedbar-window--live-p))
        (let ((win speedbar--window))
          (save-current-buffer
            (when (speedbar-find-selected-file speedbar-last-selected-file)
              (beginning-of-line)
              (set-window-point win (point))
              (unless (pos-visible-in-window-p (point) win)
                (with-selected-window win (recenter))))))))))

;; The files display's mode line is modeline.el's dedicated-window one: a pin,
;; then the label below, closing the window on click.
(defvar-local my-speedbar--file-count-cache nil
  "Cons of (`buffer-chars-modified-tick' . COUNT) for the speedbar buffer.")

(defun my-speedbar--file-count ()
  "Number of file rows currently listed in the speedbar buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (if (eq (car my-speedbar--file-count-cache) tick)
        (cdr my-speedbar--file-count-cache)
      (let ((count 0) (pos (point-min)))
        (while pos
          (when (eq (get-text-property pos 'speedbar-function) 'speedbar-find-file)
            (cl-incf count))
          (setq pos (next-single-property-change pos 'speedbar-function)))
        (setq my-speedbar--file-count-cache (cons tick count))
        count))))

(defun my-speedbar-modeline-label ()
  "Auto-update glyph and file count for the dedicated mode line."
  (let ((icon (or (my-modeline--icon-safe
                   #'nerd-icons-mdicon
                   (if speedbar-update-flag "nf-md-sync" "nf-md-sync_off")
                   :face (if speedbar-update-flag 'mode-line-emphasis 'shadow))
                  (if speedbar-update-flag "#" "!")))
        (count (my-speedbar--file-count)))
    (format "%s %d %s" icon count (if (= count 1) "file" "files"))))

(defun my-speedbar-modeline-help ()
  "Directory, project root and auto-refresh state for the mode line's help."
  (let ((proj (ignore-errors (project-current nil default-directory))))
    (format "Directory: %s\nProject root: %s\nAuto-refresh: %s"
            (abbreviate-file-name default-directory)
            (if proj (abbreviate-file-name (project-root proj)) "none")
            (if speedbar-update-flag "on" "off"))))

;; Imenu returns just ("*Rescan*") when a file has no tags, which speedbar
;; treats as a real list.  Return nil (no tags; t would mean "error, try the
;; next backend") so the file's button becomes [?].
(defun my-speedbar--no-tags-imenu (result)
  "Return nil when RESULT has only *Rescan* or nils."
  (if (and (consp result)
           (seq-every-p (lambda (e) (or (null e) (equal (car-safe e) "*Rescan*")))
                        result))
      nil
    result))

(defun my-speedbar--no-tags-dynamic (orig file)
  "Message \"No tags in FILE\" and return nil when ORIG finds no entries.
ORIG returns (INSERT-FN) with no entries when the imenu result was empty."
  (let ((result (funcall orig file)))
    (when (and result (null (cdr result)))
      (setq result nil))
    (unless result
      (message "No tags in %s" (file-name-nondirectory file)))
    result))

;; A redraw erases the buffer and restores the old scroll position, which no
;; longer matches the new contents.  Start from the top instead, so locating
;; the current file afterwards scrolls from a known place.  An in-place
;; expansion doesn't erase anything and keeps its position.
(defun my-speedbar--reset-top (orig &rest args)
  "Call ORIG with ARGS; if it redrew the speedbar buffer, scroll to the top."
  (if (not (buffer-live-p speedbar-buffer))
      (apply orig args)
    (let ((token (list nil)))
      (with-current-buffer speedbar-buffer
        (when (> (point-max) (point-min))
          ;; ORIG doesn't say whether it erased the buffer or expanded a
          ;; directory in place.  Stamp the first character with a unique
          ;; value: if it's gone afterwards, the buffer was erased and
          ;; refilled (a stale stamp from cached text won't match TOKEN).
          (with-silent-modifications
            (put-text-property (point-min) (1+ (point-min))
                               'my-redraw-token token))))
      ;; ORIG leaves the speedbar buffer current.  The stealthy
      ;; `speedbar-update-current-file' that follows in the same timer tick
      ;; reads `buffer-file-name' from the current buffer, so without
      ;; restoring it the highlight can't move until the next tick.
      (prog1 (save-current-buffer (apply orig args))
        (with-current-buffer speedbar-buffer
          (if (eq (get-text-property (point-min) 'my-redraw-token) token)
              (with-silent-modifications
                (remove-text-properties (point-min) (1+ (point-min))
                                        '(my-redraw-token nil)))
            (goto-char (point-min))
            (when-let* ((win (get-buffer-window speedbar-buffer 0)))
              (set-window-point win (point-min))
              (set-window-start win (point-min)))))))))

;;; Legacy fixup for v31
(defun my-v31-fixups ()
  "Fixups that I think can be removed in v32."
  (advice-add 'speedbar-window-mode :before #'my-speedbar-window-ensure-mode)
  ;; The stock `dframe-help-echo' tests `dframe-help-echo-function' in whatever
  ;; buffer is current, so the tooltip vanishes whenever the selected window
  ;; isn't the speedbar.  Give the variable a global default as well as the
  ;; speedbar-local value so the stock function works from any buffer.
  (setq-default dframe-help-echo-function #'my-speedbar-help-echo)
  (add-hook 'speedbar-mode-hook
            (lambda () (setq-local dframe-help-echo-function
                                   #'my-speedbar-help-echo)))
  (keymap-set speedbar-mode-map "h" #'my-speedbar-describe-keymap)
  (keymap-set speedbar-mode-map "q" #'my-speedbar-quit)
  (keymap-set speedbar-mode-map "Q" #'undefined)
  (keymap-set speedbar-mode-map "b" #'my-speedbar-show-buffers)
  (keymap-set speedbar-mode-map "f" #'my-speedbar-show-files)
  (keymap-set speedbar-mode-map "r" #'my-speedbar-show-previous-display)

  )

;;; Main Setup
(with-eval-after-load 'speedbar
  ;; show all files
  (setq speedbar-show-unknown-files t)
  ;; Don't check and show vc or compile state.
  (setq speedbar-obj-do-check nil)
  (setq speedbar-vc-do-check nil)
  ;; don't use a separate frame,
  (setq speedbar-prefer-window t)

  ;; Do fixups for v31
  (my-v31-fixups)
  (advice-add 'speedbar-item-info-file-helper :override
              #'my-speedbar-item-info-file)
  (advice-add 'speedbar-insert-image-button-maybe :around #'my-speedbar-glyph)
  (advice-add 'speedbar-file-lists :around #'my-speedbar-project-file-lists)
  (advice-add 'speedbar-update-directory-contents :around
              #'my-speedbar-project-update-contents)
  (advice-add 'speedbar-update-localized-contents :around
              #'my-speedbar-project-update-localized)
  (advice-add 'speedbar-set-mode-line-format :around #'my-speedbar-project-mode-line)
  (advice-add 'speedbar-change-initial-expansion-list :before
              #'my-speedbar-project-view-exit)
  (advice-add 'speedbar-refresh :before #'my-speedbar-project-refresh)
  (my-speedbar-update-status-faces)
  (add-hook 'enable-theme-functions #'my-speedbar-update-status-faces)
  (advice-add 'speedbar-update-directory-contents :after #'my-speedbar--colorize)
  (advice-add 'speedbar-update-current-file :after #'my-speedbar--colorize)
  (advice-add 'speedbar-expand-line :after #'my-speedbar--colorize)
  (add-hook 'after-save-hook #'my-speedbar--colorize-after-save)

  (define-key speedbar-mode-map "P" #'my-speedbar-project-view)
  ;; `o' only acts in the project view; elsewhere it stays unbound.
  (define-key speedbar-mode-map "o"
              '(menu-item "" my-speedbar-sync-to-last-window
                          :filter (lambda (cmd) (and my-speedbar-project-view-p cmd))))
  (advice-add 'speedbar-set-mode-line-format :after #'my-speedbar--strip-arrows)
  (my-speedbar-update-root-face)
  (add-hook 'enable-theme-functions #'my-speedbar-update-root-face)
  (advice-add 'speedbar-directory-buttons :around #'my-speedbar--root-face)

  ;; Just set directories to bold.
  (when (fboundp 'modus-themes-get-color-value)
    (set-face-attribute 'speedbar-directory-face nil
                        :foreground (modus-themes-get-color-value 'fg-main)))
  (advice-add 'speedbar-set-timer :filter-args #'my-speedbar--update-speed)
  (advice-add 'speedbar-update-current-file :around #'my-speedbar--reveal)
  (with-eval-after-load 'modeline
    (add-to-list 'my-modeline-dedicated-window-modes
                 '(speedbar-mode . my-speedbar-quit))
    (add-to-list 'my-modeline-dedicated-label-functions
                 '(speedbar-mode . my-speedbar-modeline-label))
    (add-to-list 'my-modeline-dedicated-help-functions
                 '(speedbar-mode . my-speedbar-modeline-help)))
  (advice-add 'speedbar-fetch-dynamic-imenu :filter-return #'my-speedbar--no-tags-imenu)
  (advice-add 'speedbar-fetch-dynamic-tags :around #'my-speedbar--no-tags-dynamic)
  (advice-add 'speedbar-update-directory-contents :around #'my-speedbar--reset-top)

  ;; Markdown isn't a supported extension by default, so its files can't be expanded into tags.
  (speedbar-add-supported-extension '(".md" ".markdown")))

(provide 'speedbar-config)
;;; speedbar-config.el ends here
