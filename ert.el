;; -*- lexical-binding: t; -*-
(require 'ert)

;; Minimal viability.
(ert-deftest my/test-config-loading ()
  "Test that the user configuration loads without errors."
  (should
   (load (expand-file-name "init.el" user-emacs-directory) t)))
