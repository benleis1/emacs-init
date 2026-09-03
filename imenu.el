;; -*- lexical-binding: t; -*-

;;; Commentary

;; ```
;;  o8o
;;  `"'
;; oooo  ooo. .oo.  .oo.    .ooooo.  ooo. .oo.   oooo  oooo
;; `888  `888P"Y88bP"Y88b  d88' `88b `888P"Y88b  `888  `888
;;  888   888   888   888  888ooo888  888   888   888   888
;;  888   888   888   888  888    .o  888   888   888   888
;; o888o o888o o888o o888o `Y8bod8P' o888o o888o  `V88V"V8P'
;; ```


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

;; Buffer-local sort/grouping strategy, set via `imenu-list-switch-sort' and
;; multiplexed on by `my/imenu-list-sort-advice' and, for the elisp indexer,
;; `my/imenu-elisp-index' itself. One of:
;;   nil (or 'position) -- the default: whatever order/structure the
;;     create-index-function naturally produces (physical position, nested
;;     under headers for elisp).
;;   'alphabetical -- `my/imenu-list-sort-alphabetically' post-processes
;;     whatever structure the create-index-function produced.
;;   'by-type -- elisp-only: `my/imenu-elisp-index' skips header nesting
;;     entirely and groups flatly by raw imenu category instead.
(defvar-local my-imenu-list-sort-strategy nil)

;; String for which sorting mode we're in for use in the mode-line
(defun my/imenu-current-sort (&optional buffer)
  (let ((strategy (if buffer
                       (buffer-local-value 'my-imenu-list-sort-strategy buffer)
                     my-imenu-list-sort-strategy)))
    (cond ((eq strategy 'alphabetical) "alpha")
          ((eq strategy 'by-type) "by-type")
          (t "pos"))))

;; Multiplexer advice that post-sorts alphabetically when that strategy is
;; selected. `by-type' is handled structurally by `my/imenu-elisp-index'
;; instead, and plain position order needs no override.
(defun my/imenu-list-sort-advice ()
  (when (eq my-imenu-list-sort-strategy 'alphabetical)
    (setq imenu--index-alist (my/imenu-list-sort-alphabetically))))

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
;; Note: default is to go by position so we don't have to override for that.
;; "by type" is elisp-only (see `my/imenu-elisp-index-by-type'); harmless
;; but pointless for treesitter, which is already grouped by type
;; regardless of strategy, so it's only offered there.
(defun imenu-list-switch-sort (strategy)
  (interactive
   (with-current-buffer imenu-list--displayed-buffer
     (unless (memq imenu-create-index-function '(my/generate-ts-imenu my/imenu-elisp-index))
       (user-error "Sort switching is only available for treesitter or elisp imenus"))
     (let ((choices (append '(("alphabetical" . alphabetical)
                               ("by position" . position))
                             (when (eq imenu-create-index-function 'my/imenu-elisp-index)
                               '(("by type" . by-type))))))
       (list (alist-get
	      (completing-read "Choose: " choices)
	      choices nil nil 'equal)))))
  (with-current-buffer imenu-list--displayed-buffer
    (setq-local my-imenu-list-sort-strategy (unless (eq strategy 'position) strategy))
    ;; mode line update to add the sort message.
    (force-mode-line-update))
  (imenu-list-refresh))

;; Work around an upstream imenu-list bug: `imenu-list-major-mode's docstring
;; references `\{imenu-list-mode-map}' for its `describe-mode' (bound to "h")
;; substitution, but no such variable exists -- only `imenu-list-major-mode-map'
;; does -- so pressing "h" errors instead of showing the bindings.
(defvaralias 'imenu-list-mode-map 'imenu-list-major-mode-map)

;; Let "s" in the *Ilist* buffer itself switch sort order
(define-key imenu-list-major-mode-map (kbd "s") #'imenu-list-switch-sort)
(define-key imenu-list-major-mode-map (kbd "c") #'my-imenu-list-fold-children)

;; Rebind hideshow's TAB/"f" to our own irect-overlay toggle
(define-key imenu-list-major-mode-map (kbd "TAB") #'my-imenu-list-toggle-at-point)
(define-key imenu-list-major-mode-map (kbd "f") #'my-imenu-list-toggle-at-point)

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
          (orderfn (if (eq my-imenu-list-sort-strategy 'alphabetical) 'my/imenu-sort 'reverse)))
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

;;; Elisp custom header handling

;; fold `defun'/`use-package' entries under the ";;; Section" comment
;; header they're physically located under (see the "Sections"/"Use-package";;

;; Every leaf (NAME . MARKER) cons across all of RAW's category groups
;; (cdr is a list, e.g. "Sections"/"Types"/"Variables"/"Use-package") and
;; ungrouped entries (cdr is a marker, e.g. plain functions), flattened
;; into one list, any order. RAW is `imenu--generic-function''s direct
;; output.
(defun my/imenu-elisp-flatten-raw (raw)
  (let (leaves)
    (dolist (entry raw)
      (if (listp (cdr entry))
          (dolist (leaf (cdr entry)) (push leaf leaves))
        (push entry leaves)))
    leaves))

;; Back POS up over contiguous comment-only/blank lines immediately
;; preceding it in BUFFER, never crossing below FLOOR (the previous
;; entry's own raw position in adjacency order) -- a `diff-hl' hunk
;; touching a doc-comment that introduces an entry should count as a
;; change to THAT entry, not to whichever entry happens to sort right
;; before it, since the comment lines themselves aren't separately
;; indexed. FLOOR stops the backward walk from swallowing a line that
;; already belongs to the previous entry.
(defun my/imenu-elisp-back-over-comments (pos buffer floor)
  (with-current-buffer buffer
    (save-excursion
      ;; Compare whole-line boundaries, not raw positions: FLOOR usually
      ;; falls mid-line (it's another entry's own match position, e.g. at
      ;; a defun's NAME, not its line start), but for `^'-anchored
      ;; patterns like "Sections" it lands right at that line's own
      ;; beginning -- in which case a raw `>=' comparison would let the
      ;; walk step onto and swallow FLOOR's entire line as if it were
      ;; just an anonymous leading comment, when it's actually the
      ;; previous entry's own exclusive territory.
      (let ((floor-bol (progn (goto-char floor) (line-beginning-position))))
        (goto-char pos)
        (beginning-of-line)
        (let ((start (point)))
          (while (and (> (point) floor-bol)
                      (progn (forward-line -1)
                             (and (> (point) floor-bol)
                                  (looking-at "^[ \t]*\\(;.*\\)?$"))))
            (setq start (point)))
          start)))))

;; Buffer-local table (in the SOURCE buffer, i.e. the elisp buffer being
;; indexed, not the *Ilist* buffer) mapping every leaf's raw, normalized
;; buffer position to its true (BEG . END) span, as computed once by
;; `my/imenu-elisp-parse-and-tag-ranges'. `my-imenu-list--entry-range'
;; reads this to answer "is a `diff-hl' hunk inside this entry's span
(defvar-local my-imenu-list--range-table nil)

;; Call `imenu--generic-function' in the current buffer and populate
;; `my-imenu-list--range-table' with every resulting leaf's true (BEG
;; . END) span -- BEG backed up over its own leading comment (see
;; `my/imenu-elisp-back-over-comments'), END the very next entry's
;; (equally backed-up) start anywhere in the buffer, across every
;; category including "Sections"/"Subsections", or `point-max' for the
;; physically last entry. Computed once here, from true physical
;; adjacency across ALL entries regardless of category, before any
;; mode-specific bucketing/nesting/sorting.
;;
;; Keyed by each entry's own RAW position
;; (not its backed-up BEG), normalized to a plain integer since two
;; distinct marker objects at the same buffer position are not `eql' --
;; this is also what makes the table double as a lookup for the
;; synthetic "" declaration leaf `my/imenu-elisp-build-header' fabricates
;; for a Section/Subsection header (a fresh cons with no property of its
;; own, but pointing at the same raw position the header's own leaf in
;; RAW was keyed under). Returns RAW, for the caller to bucket/nest as
;; before.
(defun my/imenu-elisp-parse-and-tag-ranges ()
  (let* ((raw (imenu--generic-function imenu-generic-expression))
         (leaves (sort (my/imenu-elisp-flatten-raw raw)
                       (lambda (a b) (< (cdr a) (cdr b)))))
         (buf (current-buffer))
         (table (make-hash-table :test 'eql))
         (floor (point-min))
         (rest leaves)
         starts)
    (while rest
      (let ((s (my/imenu-elisp-back-over-comments (cdar rest) buf floor)))
        (push s starts)
        (setq floor (cdar rest)))
      (setq rest (cdr rest)))
    (setq starts (nreverse starts))
    (let ((ls leaves) (ss starts))
      (while ls
        (let* ((entry (car ls))
               (key (let ((p (cdr entry))) (if (markerp p) (marker-position p) p)))
               (beg (car ss))
               (end (if (cdr ss) (cadr ss) (point-max))))
          (puthash key (cons beg end) table))
        (setq ls (cdr ls) ss (cdr ss))))
    (setq-local my-imenu-list--range-table table)
    raw))

;; (NAME BEG END) for each entry in ENTRIES (an alist of (name . marker),
;; already sorted by position), covering from the entry's own marker up to
;; the next entry's marker, or point-max for the last one.
(defun my/imenu-elisp-ranges (entries)
  (let (ranges)
    (while entries
      (push (list (caar entries) (cdar entries)
                  (if (cadr entries) (cdadr entries) (point-max)))
            ranges)
      (setq entries (cdr entries)))
    (nreverse ranges)))

;; Name of the range in RANGES that POS falls inside, or nil
;; if POS precedes the first range (or there are no ranges at all).
(defun my/imenu-elisp-find-section (ranges pos)
  (catch 'found
    (dolist (range ranges)
      (when (and (>= pos (nth 1 range)) (< pos (nth 2 range)))
        (throw 'found (nth 0 range))))))

;; Bucket ENTRIES (an alist of (name . marker), sorted ascending by
;; position) into RANGES by `my/imenu-elisp-find-section', returning
;; (BUCKETS . ORPHANS): BUCKETS is a hash table of range name -> entries
;; (ascending), ORPHANS the entries (ascending) that precede every range.
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

;; Build a nested imenu alist entry for a header named NAME at buffer
;; position START, with CHILDREN (already-built nested entries, if any --
;; used to nest a Section's Subsections underneath it) inserted before
;; header's own Use-package/function/other-category entries.
;; CATEGORY-BUCKETS is an alist of (CATEGORY-NAME . HASH-TABLE), one entry
;; per non-function, non-Use-package category (Variables/Types/...), each
;; HASH-TABLE mapping a header NAME to that category's entries physically
;; inside it -- same shape as FN-BUCKETS/PKG-BUCKETS.
(defun my/imenu-elisp-build-header (name start fn-buckets pkg-buckets category-buckets &optional children)
  (let ((fns (gethash name fn-buckets))
        (pkgs (gethash name pkg-buckets)))
    (cons name (append (list (cons "" start))
                        (when pkgs (list (cons "Use-package" pkgs)))
                        (delq nil (mapcar (lambda (cb)
                                            (let ((entries (gethash name (cdr cb))))
                                              (when entries (cons (car cb) entries))))
                                          category-buckets))
                        fns
                        children))))

;; Custom imenu-create-index-function for emacs-lisp-mode, dispatching on
;; `my-imenu-list-sort-strategy' between the header-nested view (the
;; default) and the flat by-type view.
(defun my/imenu-elisp-index ()
  (if (eq my-imenu-list-sort-strategy 'by-type)
      (my/imenu-elisp-index-by-type)
    (my/imenu-elisp-index-by-position)))

;; Regroup the basic indexed nodes under a "Sections" header they fll under.
;; "Subsections" (";;;; " headers) also nest under whichever "Sections" (";;; ")
;; header they physically fall inside. Every section/subsection also gets
;; a leading "." entry jumping to its own header (mirroring the
;; "declaration" entry `my/walk-object-declaration' adds for a class), so
;; one with no other content is still navigable.
(defun my/imenu-elisp-index-by-position ()
  (let ((raw (my/imenu-elisp-parse-and-tag-ranges))
        sections subsections usepkg categories functions)
    (dolist (entry raw)
      (cond ((equal (car entry) "Sections") (setq sections (cdr entry)))
            ((equal (car entry) "Subsections") (setq subsections (cdr entry)))
            ((equal (car entry) "Use-package") (setq usepkg (cdr entry)))
            ((listp (cdr entry)) (push entry categories))
            (t (push entry functions))))
    (setq categories (nreverse categories))
    (setq functions (sort functions (lambda (left right) (< (cdr left) (cdr right)))))
    (setq usepkg (sort usepkg (lambda (left right) (< (cdr left) (cdr right)))))
    (setq subsections (sort subsections (lambda (left right) (< (cdr left) (cdr right)))))
    (let* ((section-ranges (my/imenu-elisp-ranges sections))
           ;; `sort' on a list is destructive so use `copy-sequence' gives the merge its own cells.
           (fine-ranges (my/imenu-elisp-ranges
                         (sort (copy-sequence (append sections subsections))
                               (lambda (left right) (< (cdr left) (cdr right))))))
           (fn-bucketed (my/imenu-elisp-bucket-by-section functions fine-ranges))
           (pkg-bucketed (my/imenu-elisp-bucket-by-section usepkg fine-ranges))
           ;; One (buckets . orphans) pair per other category, same shape
           ;; as PKG-BUCKETED.
           (category-bucketed
            (mapcar (lambda (cat)
                      (cons (car cat) (my/imenu-elisp-bucket-by-section (cdr cat) fine-ranges)))
                    categories))
           ;; ...but a Subsection's *parent* is decided against Sections
           ;; alone, since a Subsection always nests directly under the
           ;; Section it physically falls inside, regardless of any
           ;; intervening Subsection siblings.
           (sub-bucketed (my/imenu-elisp-bucket-by-section subsections section-ranges))
           (fn-buckets (car fn-bucketed))
           (pkg-buckets (car pkg-bucketed))
           (sub-buckets (car sub-bucketed))
           (pkg-orphans (cdr pkg-bucketed))
           (sub-orphans (cdr sub-bucketed))
           (category-buckets (mapcar (lambda (c) (cons (car c) (car (cdr c)))) category-bucketed))
           ;; Members of an other-category that precede every section --
           ;; these have nothing to nest under, so they stay flat
           ;; top-level siblings, same as PKG-ORPHANS.
           (category-orphans (delq nil (mapcar (lambda (c)
                                                  (let ((orphans (cdr (cdr c))))
                                                    (when orphans (cons (car c) orphans))))
                                                category-bucketed))))
      (append (my/imenu-elisp-nest-under-code
               (mapcar (lambda (range)
                         (my/imenu-elisp-build-header
                          (car range) (nth 1 range) fn-buckets pkg-buckets category-buckets
                          (mapcar (lambda (sub)
                                    (my/imenu-elisp-build-header
                                     (car sub) (cdr sub) fn-buckets pkg-buckets category-buckets))
                                  (gethash (car range) sub-buckets))))
                       section-ranges))
              (when pkg-orphans (list (cons "Use-package" pkg-orphans)))
              category-orphans
              (mapcar (lambda (sub)
                        (my/imenu-elisp-build-header (car sub) (cdr sub) fn-buckets pkg-buckets category-buckets))
                      sub-orphans)
              (cdr fn-bucketed)))))

;; Flat by-type view: no header nesting at all -- every category
;; (Functions/Use-package/Variables/Types/...) becomes its own top-level
;; group, sorted by position within. "Sections"/"Subsections" are dropped
;; rather than shown as an empty-feeling category, since they're purely a
;; structural device for the by-position view, not content in their own
;; right.
(defun my/imenu-elisp-index-by-type ()
  (let ((raw (my/imenu-elisp-parse-and-tag-ranges))
        categories functions)
    (dolist (entry raw)
      (cond ((equal (car entry) "Sections") nil)
            ((equal (car entry) "Subsections") nil)
            ((listp (cdr entry)) (push entry categories))
            (t (push entry functions))))
    (setq categories (nreverse categories))
    (append (when functions
              (list (cons "Functions"
                          (sort functions (lambda (left right) (< (cdr left) (cdr right)))))))
            (mapcar (lambda (cat)
                      (cons (car cat)
                            (sort (copy-sequence (cdr cat))
                                  (lambda (left right) (< (cdr left) (cdr right))))))
                    categories))))

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


;;; VC Highlighting

;; Highlight imenu entries whose corresponding source section has an
;; uncommitted VC change, per `diff-hl' hunk overlays (each hunk gets one
;; overlay spanning its changed lines, tagged with the `diff-hl-hunk'
;; property).

(defface my-imenu-list-modified-face
    `((t (:background ,(modus-themes-get-color-value 'bg-changed))))
    "Face for imenu-list entries covering a source section with a pending `diff-hl' change."
    :group 'my-custom-group)

  (defun my-imenu-list--flatten-entries (index-alist depth)
    "Flatten INDEX-ALIST into (ENTRY . DEPTH) pairs, in the order `imenu-list' displays them."
    (apply #'nconc
           (mapcar (lambda (entry)
                     (cons (cons entry depth)
                           (when (imenu--subalist-p entry)
                             (my-imenu-list--flatten-entries (cdr entry) (1+ depth)))))
                   index-alist)))

  (defun my-imenu-list--section-modified-p (start end buffer)
    "Return non-nil if BUFFER has a `diff-hl' hunk overlapping [START, END)."
    (when (and start end)
      (with-current-buffer buffer
        (let ((ovs (overlays-in start end))
              found)
          (while (and ovs (not found))
            (setq found (overlay-get (car ovs) 'diff-hl-hunk))
            (setq ovs (cdr ovs)))
          found))))

  (defun my-imenu-list--entry-range (entry buffer)
    "ENTRY's true (BEG . END) span, or nil if it has no position of its
own or BUFFER's indexer doesn't tag ranges (see
`my-imenu-list--range-table', elisp-only via
`my/imenu-elisp-parse-and-tag-ranges'). Looks up ENTRY's own
`my-imenu-list--entry-position' (normalized to a plain integer, since two
distinct marker objects at the same buffer position are not `eql') in
BUFFER's range table -- this works for the synthetic \"\" declaration leaf
`my/imenu-elisp-build-header' fabricates for a Section/Subsection header
too, since that leaf's position is the same raw position the header's own
leaf in RAW was keyed under when the table was built."
    (let ((pos (my-imenu-list--entry-position entry)))
      (when pos
        (let ((key (if (markerp pos) (marker-position pos) pos))
              (table (buffer-local-value 'my-imenu-list--range-table buffer)))
          (and table (gethash key table))))))

  (defun my-imenu-list--mark-modified (entries buffer index-table leaf-modified)
    "Mark LEAF-MODIFIED (via INDEX-TABLE, a hash table from entry to its
index in the flattened, display-order list) for every leaf in ENTRIES --
any nesting depth -- whose own `my-imenu-list--entry-range' overlaps a
`diff-hl' hunk in BUFFER, per `my-imenu-list--section-modified-p'. A
container is marked whenever any of its descendants is (plain recursive
OR, no separate span check of its own) -- unlike the old sibling-order
inference this replaced, ranges are computed once, up front, from true
physical adjacency across every entry regardless of category (see
`my/imenu-elisp-parse-and-tag-ranges'), so they already gaplessly and
correctly partition the buffer; there's no leftover \"gap between
children\" a container could need to separately claim.

Returns non-nil if anything in ENTRIES or their descendants got marked."
    (let (any-modified)
      (dolist (entry entries)
        (let ((modified
               (if (imenu--subalist-p entry)
                   (my-imenu-list--mark-modified (cdr entry) buffer index-table leaf-modified)
                 (let ((range (my-imenu-list--entry-range entry buffer)))
                   (and range (my-imenu-list--section-modified-p (car range) (cdr range) buffer))))))
          (when modified
            (aset leaf-modified (gethash entry index-table) t)
            (setq any-modified t))))
      any-modified))

  (defun my-imenu-list-highlight-modified-entries ()
    "Overlay `my-imenu-list-modified-face' on *Ilist* lines covering a source
section with a pending `diff-hl' change, via `my-imenu-list--mark-modified'."
    (let ((src-buf imenu-list--displayed-buffer))
      (when (buffer-live-p src-buf)
        (let* ((flat (my-imenu-list--flatten-entries imenu-list--imenu-entries 0))
               (n (length flat))
               (leaf-modified (make-vector n nil))
               (index-table (make-hash-table :test 'eq))
               (i 0))
          (dolist (pair flat)
            (puthash (car pair) i index-table)
            (setq i (1+ i)))
          (my-imenu-list--mark-modified imenu-list--imenu-entries src-buf index-table leaf-modified)
          (with-current-buffer imenu-list-buffer-name
            (remove-overlays (point-min) (point-max) 'my-imenu-list-modified t)
            (let ((inhibit-read-only t))
              (dotimes (i n)
                (when (aref leaf-modified i)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line i)
                    (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                      (overlay-put ov 'my-imenu-list-modified t)
                      (overlay-put ov 'face 'my-imenu-list-modified-face)))))))))))

  ;; Explicit positive depth so this always runs after the fold hook
  ;; (negative depth, in init.el) has settled the buffer's hs overlays for
  ;; this update cycle, regardless of load order between the two files.
  (add-hook 'imenu-list-update-hook #'my-imenu-list-highlight-modified-entries 10)


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
