;; -*- lexical-binding: t; -*-
;; ERT tests for the pure/deterministic pieces of modeline.el: the
;; zoom-slider math and the mode-line/header-line "dualizing" helpers.
;; Anything that renders the actual mode-line, reads `vc-mode' state,
;; or drives real mouse-drag events is out of scope here.
(require 'ert)

;; In the normal CI flow (see .github/workflows/test.yml) `init.el' is
;; loaded first, which loads `modeline.el' itself -- so the functions
;; under test are already defined by the time this file loads. This
;; fallback just makes the file independently runnable too (e.g.
;; `emacs -Q --batch -l ert -l modeline-tests.el -f
;; ert-run-tests-batch-and-exit'), without requiring `nerd-icons' --
;; none of the functions exercised below call into it.
(unless (fboundp 'my-modeline--zoom-frac)
  (require 'modeline (expand-file-name "modeline.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name default-directory)))))

;;; `my-modeline--zoom-frac'

(ert-deftest modeline-test/zoom-frac-at-min-is-zero ()
  (should (= (my-modeline--zoom-frac my-modeline-zoom-min) 0.0)))

(ert-deftest modeline-test/zoom-frac-at-max-is-one ()
  (should (= (my-modeline--zoom-frac my-modeline-zoom-max) 1.0)))

(ert-deftest modeline-test/zoom-frac-clamps-below-min ()
  (should (= (my-modeline--zoom-frac (1- my-modeline-zoom-min)) 0.0)))

(ert-deftest modeline-test/zoom-frac-clamps-above-max ()
  (should (= (my-modeline--zoom-frac (1+ my-modeline-zoom-max)) 1.0)))

;;; `my-modeline--zoom-amount-at-x'

(ert-deftest modeline-test/zoom-amount-at-left-edge-is-min ()
  (should (eql (my-modeline--zoom-amount-at-x 0) my-modeline-zoom-min)))

(ert-deftest modeline-test/zoom-amount-at-right-edge-is-max ()
  (should (eql (my-modeline--zoom-amount-at-x my-modeline-zoom-track-width)
                my-modeline-zoom-max)))

(ert-deftest modeline-test/zoom-amount-clamps-past-right-edge ()
  (should (eql (my-modeline--zoom-amount-at-x (* 10 my-modeline-zoom-track-width))
                my-modeline-zoom-max)))

(ert-deftest modeline-test/zoom-amount-clamps-before-left-edge ()
  (should (eql (my-modeline--zoom-amount-at-x -50) my-modeline-zoom-min)))

;;; `my-modeline--first-property'

(ert-deftest modeline-test/first-property-finds-value-mid-string ()
  (let ((s (concat "ab" (propertize "cd" 'my-prop 'found) "ef")))
    (should (eq (my-modeline--first-property s 'my-prop) 'found))))

(ert-deftest modeline-test/first-property-nil-when-absent ()
  (should (null (my-modeline--first-property "plain string" 'my-prop))))

(ert-deftest modeline-test/first-property-nil-on-empty-string ()
  (should (null (my-modeline--first-property "" 'my-prop))))

;;; `my-modeline--dualize-keymap'

(ert-deftest modeline-test/dualize-keymap-mirrors-mode-line-onto-header-line ()
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my-command)
    (let ((dual (my-modeline--dualize-keymap map)))
      (should (eq (lookup-key dual [header-line mouse-1]) 'my-command))
      (should (eq (lookup-key dual [mode-line mouse-1]) 'my-command)))))

(ert-deftest modeline-test/dualize-keymap-mirrors-header-line-onto-mode-line ()
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'my-command)
    (let ((dual (my-modeline--dualize-keymap map)))
      (should (eq (lookup-key dual [mode-line mouse-1]) 'my-command)))))

(ert-deftest modeline-test/dualize-keymap-does-not-mutate-input ()
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my-command)
    (my-modeline--dualize-keymap map)
    (should-not (keymapp (lookup-key map [header-line])))))

(ert-deftest modeline-test/dualize-keymap-nil-for-non-keymap ()
  (should (null (my-modeline--dualize-keymap "not-a-keymap"))))

;;; `my-modeline--dualize-local-maps'

(ert-deftest modeline-test/dualize-local-maps-mirrors-submap-in-place ()
  (let* ((submap (let ((m (make-sparse-keymap)))
                    (define-key m [mode-line mouse-1] 'my-command)
                    m))
         (s (propertize "text" 'local-map submap))
         (dual (my-modeline--dualize-local-maps s)))
    (should (eq (lookup-key (get-text-property 0 'local-map dual) [header-line mouse-1])
                 'my-command))))

(ert-deftest modeline-test/dualize-local-maps-does-not-mutate-input ()
  (let* ((submap (let ((m (make-sparse-keymap)))
                    (define-key m [mode-line mouse-1] 'my-command)
                    m))
         (s (propertize "text" 'local-map submap)))
    (my-modeline--dualize-local-maps s)
    (should-not (keymapp (lookup-key (get-text-property 0 'local-map s) [header-line])))))

(ert-deftest modeline-test/dualize-local-maps-unchanged-without-local-map ()
  (should (equal (my-modeline--dualize-local-maps "plain") "plain")))

(provide 'modeline-tests)
