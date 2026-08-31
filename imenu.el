;; -*- lexical-binding: t; -*-

;;; Commentary

;;  o8o
;;  `"'
;; oooo  ooo. .oo.  .oo.    .ooooo.  ooo. .oo.   oooo  oooo
;; `888  `888P"Y88bP"Y88b  d88' `88b `888P"Y88b  `888  `888
;;  888   888   888   888  888ooo888  888   888   888   888
;;  888   888   888   888  888    .o  888   888   888   888
;; o888o o888o o888o o888o `Y8bod8P' o888o o888o  `V88V"V8P'


;; Imenu and imenu-list extensions

;; Included here are all of the extensions off of Imenu-List
;; * Arrow icons
;; * sorting
;; * custom mode-line formatting
;; * fixes for highlighting even empty headers
;; * special handling for org mode
;; * custom indexing for elisp

;;; Code

;;
;; Custom hierarchical parsing of the treesitter tree
;;

;; Generate a marker for the given node
;; This can only be done while in the buffer
(defun my/make-marker (buffer point)
  (with-current-buffer buffer
    (copy-marker point)))

;; Treesitter node name function for most node types
(defun my/get-def-name (node)
  (treesit-node-text
   (treesit-node-child-by-field-name node "name") t))

;; Treesitter node name function for class fields
(defun my/get-field-name (node)
  (treesit-node-text
   (treesit-node-child-by-field-name (treesit-node-child-by-field-name node "declarator") "name") t))

;; Simple wrapper to make an imenu leaf from a treesitter node
(defun my/imenu-leaf (node buffer name-func)
  (cons (funcall name-func node)
        (my/make-marker buffer (treesit-node-start node))))

;; Compare two imenu nodes
(defun my/imenu-compare (left right)
  (string-lessp (car left) (car right)))

;; Global variable to track sorting function
;; which we'll set per buffer and then multiplex on
(defvar my-imenu-list-sort-function nil)

;; String for which sorting mode we're in for use in the mode-line
(defun  my/imenu-current-sort (&optional buffer)
  (if buffer
      (with-current-buffer buffer
        (progn
          (if my-imenu-list-sort-function "alpha" "pos")))
    (if my-imenu-list-sort-function "alpha" "pos")))

;; Multiplexer advice that inserts a sorting function if one is
;; defined above.
(defun my/imenu-list-sort-advice ()
  (when my-imenu-list-sort-function
    (progn
      (setq imenu--index-alist (funcall my-imenu-list-sort-function)))))

(define-advice imenu-list-rescan-imenu (:after ())
  (my/imenu-list-sort-advice))

;; Custom sorting function that alphabetizes per imenu object type.
;; There is no built in facility to extend sorting so we have to wire this in via advice
;; This is written generically to handle elisp which just inserts all the functions as leaf nodes
;; and java lsp/treesitter which insert everything under categories.
(defun my/imenu-list-sort-alphabetically ()
  (interactive)
  (let ((entries imenu--index-alist)
        (leaf-entries nil)
        (sorted-entries nil))

    (dolist (entry entries)

      ;; if its a category container sort the entries within it
      ;; o/w add to a temp list to be sorted below
      (if (not (listp (cdr entry)))
          (setq leaf-entries (cons entry leaf-entries))
        (let* ((objects (cdr entry))
               (type (car entry))
               (sorted-objects (sort objects
                                     (lambda (left right)
                                       (string-lessp (car left) (car right))))))

          (setq sorted-entries (append sorted-entries (list (cons type sorted-objects))))
          )))

    ;; Sort the top level leaf entries
    (setq sorted-entries (append sorted-entries
				 (sort leaf-entries
				       (lambda (left right)
					 (string-lessp (car left) (car right))))))
    ))

;; Interactive command to make it easy to swap how the symbols are sorted
;; Note: default is to go by position so we don't have to override for that
(defun imenu-list-switch-sort (type)
  (interactive
   (with-current-buffer imenu-list--displayed-buffer
     (unless (eq imenu-create-index-function 'my/generate-ts-imenu)
       (user-error "Sort switching is only available for treesitter class/interface imenus"))
     (let ((choices '(("alphabetical"  . my/imenu-list-sort-alphabetically)
		      ("by position" . nil )))) ;; default no override needed
       (list (alist-get
	      (completing-read "Choose: " choices)
	      choices nil nil 'equal)))))
  (with-current-buffer imenu-list--displayed-buffer
    (setq-local my-imenu-list-sort-function type)
    ;; mode line update to add the sort message.
    (force-mode-line-update))
  (imenu-list-refresh))

;; Work around an upstream imenu-list bug: `imenu-list-major-mode's docstring
;; references `\{imenu-list-mode-map}' for its `describe-mode' (bound to "h")
;; substitution, but no such variable exists -- only `imenu-list-major-mode-map'
;; does -- so pressing "h" errors instead of showing the bindings.
(defvaralias 'imenu-list-mode-map 'imenu-list-major-mode-map)

;; Let "s" in the *Ilist* buffer itself switch sort order, since that's
(define-key imenu-list-major-mode-map (kbd "s") #'imenu-list-switch-sort)
(define-key imenu-list-major-mode-map (kbd "c") #'hs-hide-evel)

;; Sort a list of imenu nodes
(defun my/imenu-sort (seq)
  (sort seq 'my/imenu-compare))

;; Walk the parent node class of an interface, class or enum and
;; construct a list of all fields, constructors and methods.
;; Recursion occurs when there is an inner class.
(defun my/walk-object-declaration (classnode buffer)
    (let ((constructors ())
          (fields ())
          (methods ())
          (inner-classes ())
          (result ())
          (orderfn (if my-imenu-list-sort-function 'my/imenu-sort 'reverse)))
      (dolist (node (treesit-node-children classnode))
        (progn
          (cond ((equal (treesit-node-type node) "constructor_declaration")
                 (push (my/imenu-leaf node buffer 'my/get-def-name) constructors))

                ((equal (treesit-node-type node) "method_declaration")
                 (push (my/imenu-leaf node buffer 'my/get-def-name) methods))

                ((equal (treesit-node-type node) "class_declaration")
                 (let* ((body (treesit-node-child-by-field-name node "body"))
                        (classname (my/get-def-name node))
			(subleafs (cons (cons "declaration" (my/make-marker buffer (treesit-node-start node)))
					(my/walk-object-declaration body buffer))))

                   (push (cons classname subleafs) inner-classes)))

                ((equal (treesit-node-type node) "field_declaration")
                 (push (my/imenu-leaf node buffer 'my/get-field-name) fields)))))

      (when inner-classes (push (cons "Inner Classes" (funcall orderfn inner-classes)) result))
      (when methods (push (cons "Methods" (funcall orderfn methods)) result))
      (when fields (push (cons "Fields" (funcall orderfn fields)) result))
      (when constructors (push (cons "Constructors" (funcall orderfn constructors)) result))
      ;; final value
      result))

(setq my/first-level-ts-filters '(("Classes" "class_declaration")
                                  ("Interfaces" "interface_declaration")
                                  ("Records" "record_declaration")))

;; Main routine that walks top level of the grammar tree and constructs imenu nodes
;; to turn on - (setq imenu-create-index-function 'my/generate-ts-imenu)
(defun my/generate-ts-imenu (&optional buffer)
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer (if buffer (get-buffer buffer) (current-buffer))
    (let ((classes '())
          (interfaces '())
          (enums '())
          (class_declaration '())
          (subresults '())
          (result '()))

      (dolist (node (treesit-node-children (treesit-buffer-root-node)))
        (let ((type (treesit-node-type node)))
          (when (or (equal type "class_declaration")
                    (equal type "interface_declaration")
                    (equal type "enum_declaration"))
            (let* ((body (treesit-node-child-by-field-name node "body"))
                   (subleafs  (when body (my/walk-object-declaration body buffer)))
                   (objectname (my/get-def-name node))
                   (object-start (treesit-node-start node)))

              (push (cons "declaration" (my/make-marker buffer object-start)) subleafs)
              (unless (assoc type subresults) (push (cons type nil) subresults))
              (push (cons objectname subleafs) (cdr (assoc type subresults)))

              (cond ((equal type "class_declaration")
                     (push (cons objectname subleafs) classes))
                    ((equal type "enum_declaration")
                     (push (cons objectname subleafs) enums))
                    ((equal type "interface_declaration")
                     (push (cons objectname subleafs) interfaces)))))))

      (when enums (push (cons "Enums" (reverse enums)) result))
      (when (assoc "class_declaration" subresults)
        (push (cons "Classes" (reverse (cdr (assoc "class_declaration" subresults)))) result))
      (when interfaces (push (cons "Interfaces" (reverse interfaces)) result))
      result)))

;;
;; Elisp: fold `defun'/`use-package' entries under the ";;; Section" comment
;; header they're physically located under (see the "Sections"/"Use-package"
;; imenu-generic-expression patterns added by the emacs-lisp-mode-hook in
;; init.el), leaving every other category (Variables, ...) untouched at the
;; top level.
;;

;; (NAME BEG END) for each section in SECTIONS (an alist of (name . marker),
;; already sorted by position by `imenu--generic-function'), covering from
;; the section's own marker up to the next section's marker, or point-max
;; for the last one.
(defun my/imenu-elisp-section-ranges (sections)
  (let (ranges)
    (while sections
      (push (list (caar sections) (cdar sections)
                  (if (cadr sections) (cdadr sections) (point-max)))
            ranges)
      (setq sections (cdr sections)))
    (nreverse ranges)))

;; Name of the section in RANGES that POS falls inside, or nil
;; if POS precedes the first section (or there are no sections at all).
(defun my/imenu-elisp-find-section (ranges pos)
  (catch 'found
    (dolist (range ranges)
      (when (and (>= pos (nth 1 range)) (< pos (nth 2 range)))
        (throw 'found (nth 0 range))))))

;; Bucket ENTRIES (sorted ascending by position) into RANGES by
;; `my/imenu-elisp-find-section', returning (BUCKETS . ORPHANS): BUCKETS is a
;; hash table of section name -> entries (ascending), ORPHANS the entries
;; (ascending) that precede every section.
(defun my/imenu-elisp-bucket-by-section (entries ranges)
  (let ((buckets (make-hash-table :test 'equal))
        (orphans nil))
    (dolist (entry entries)
      (let ((section (my/imenu-elisp-find-section ranges (cdr entry))))
        (if section
            (puthash section (cons entry (gethash section buckets)) buckets)
          (push entry orphans))))
    (maphash (lambda (k v) (puthash k (nreverse v) buckets)) buckets)
    (cons buckets (nreverse orphans))))

;; If one of SECTIONS is named "Code" (the ";;; Code:" boilerplate header
;; conventional in Elisp files), nest every section that follows it
;; underneath it instead of leaving them as top-level siblings, since
;; everything after ";;; Code:" belongs to "the code" rather than being a
;; peer of Commentary/Code/etc.
(defun my/imenu-elisp-nest-under-code (sections)
  (let ((rest sections) before)
    (catch 'done
      (while rest
        (if (equal (caar rest) "Code:")
            (throw 'done (append (nreverse before)
                                  (list (cons "Code" (append (cdar rest) (cdr rest))))))
          (push (car rest) before)
          (setq rest (cdr rest))))
      sections)))

;; Custom imenu-create-index-function for emacs-lisp-mode. `defun's are
;; returned by `imenu--generic-function' as plain top-level leaves,
;; and "Use-package" is our own added category -- both get regrouped here
;; under their enclosing "Sections" header, with use-package calls kept in
;; their own "Use-package" sub-header within each section (mirroring the
;; top-level category they'd otherwise be filed under). Anything else
;; (e.g. "Variables") is passed through untouched. Every section also gets
;; a leading "." entry jumping to the section header itself (mirroring the
;; "declaration" entry `my/walk-object-declaration' adds for a class), so a
;; section with no functions or use-package calls is still navigable.
(defun my/imenu-elisp-index ()
  (let ((raw (imenu--generic-function imenu-generic-expression))
        sections usepkg other functions)
    (dolist (entry raw)
      (cond ((equal (car entry) "Sections") (setq sections (cdr entry)))
            ((equal (car entry) "Use-package") (setq usepkg (cdr entry)))
            ((listp (cdr entry)) (push entry other))
            (t (push entry functions))))
    (setq functions (sort functions (lambda (left right) (< (cdr left) (cdr right)))))
    (setq usepkg (sort usepkg (lambda (left right) (< (cdr left) (cdr right)))))
    (let* ((ranges (my/imenu-elisp-section-ranges sections))
           (fn-bucketed (my/imenu-elisp-bucket-by-section functions ranges))
           (pkg-bucketed (my/imenu-elisp-bucket-by-section usepkg ranges))
           (fn-buckets (car fn-bucketed))
           (pkg-buckets (car pkg-bucketed))
           (pkg-orphans (cdr pkg-bucketed)))
      (append (nreverse other)
              (my/imenu-elisp-nest-under-code
               (mapcar (lambda (range)
                         (let* ((name (car range))
                                (fns (gethash name fn-buckets))
                                (pkgs (gethash name pkg-buckets)))
                           (cons name (append (list (cons "" (nth 1 range)))
                                              (when pkgs (list (cons "Use-package" pkgs)))
                                              fns))))
                       ranges))
              (when pkg-orphans (list (cons "Use-package" pkg-orphans)))
              (cdr fn-bucketed)))))

  ;; `imenu-list--current-entry' deliberately skips subalist (container)
  ;; entries when deciding which line to highlight, since a plain subalist
  ;; cons has no position of its own. But `org-imenu-get-tree' still stamps
  ;; each entry's *name* string with an `org-imenu-marker' text property
  ;; pointing at that heading's own position, even for headings that end up
  ;; container-only (i.e. any heading with a child heading, like "IMenu" in
  ;; tour.org). Recover that so point-in-container also highlights the
  ;; container's own line instead of falling back to the previous sibling.
  (defun my-imenu-list--entry-position (entry)
    "Return a comparable buffer position for ENTRY, or nil if none exists."
    (if (imenu--subalist-p entry)
        (get-text-property 0 'org-imenu-marker (car entry))
      (funcall (imenu-list-position-translator)
               (if (listp (cdr entry)) (cadr entry) (cdr entry)))))

  (defun my-imenu-list--current-entry ()
    "Like `imenu-list--current-entry', but also matches container entries
that carry an `org-imenu-marker' text property on their name."
    (let ((point-pos (point-marker))
          (offset (point-min-marker))
          match-entry)
      (dolist (entry imenu-list--line-entries match-entry)
        (let ((entry-pos (my-imenu-list--entry-position entry)))
          (when (and entry-pos (imenu-list-<= offset entry-pos point-pos))
            (setq offset entry-pos)
            (setq match-entry entry))))))

  (advice-add 'imenu-list--current-entry :override #'my-imenu-list--current-entry)


  ;;; Org mode optimization. Its not completely clear if its needed.

  ;; `imenu-list-collect-entries' unconditionally makes imenu rescan the
  ;; whole buffer for headings every time `imenu-list-update' runs (driven by
  ;; `imenu-list-idle-update-delay'), even when nothing has changed since the
  ;; last scan. Skip that rescan for org buffers that haven't been modified
  ;; since we last collected entries, and just keep reusing the previously
  ;; generated tree.
  (defvar-local my-imenu-list--last-tick nil
    "`buffer-chars-modified-tick' as of the last `imenu-list-collect-entries' rescan.")

  (defun my-imenu-list--skip-rescan-if-unmodified (orig-fn)
    "Skip ORIG-FN's imenu rescan in org-mode buffers that are unmodified
since the last rescan; reuse the existing `imenu--index-alist' instead."
    (if (and (derived-mode-p 'org-mode)
             imenu--index-alist
             my-imenu-list--last-tick
             (= my-imenu-list--last-tick (buffer-chars-modified-tick)))
        (setq imenu-list--imenu-entries imenu--index-alist
              imenu-list--displayed-buffer (current-buffer))
      (funcall orig-fn)
      (setq my-imenu-list--last-tick (buffer-chars-modified-tick))))

 (advice-add 'imenu-list-collect-entries :around #'my-imenu-list--skip-rescan-if-unmodified)

;; Turn on auto rescan
(setq imenu-auto-rescan t)
