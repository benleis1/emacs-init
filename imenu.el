;; -*- lexical-binding: t; -*-

;;; Commentary

;;
;; Imenu and imenu-list extensions
;;

;; Included here are all of the extensions off of Imenu-List
;; * Arrow icons
;; * sorting
;; * custom mode-line formatting


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

;; Let "s" in the *Ilist* buffer itself switch sort order, since that's
(define-key imenu-list-major-mode-map (kbd "s") #'imenu-list-switch-sort)

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
