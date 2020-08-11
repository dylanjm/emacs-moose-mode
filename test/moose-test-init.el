;;; moose-test-init.el --- Test Initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar moose-test-path
  (file-name-directory (or load-file-name buffer-file-name))
  "Path to tests directory.")

(require 'cl-lib)
(require 'ert)

(dolist (test-file (directory-files moose-test-path t "-tests.el$"))
  (load test-file nil t))
