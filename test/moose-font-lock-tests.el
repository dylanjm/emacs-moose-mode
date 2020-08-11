;;; moose-font-lock-tests.el --- Font-Lock Tests for `moose-mode'.

;; Copyright (C) 2020-2021 Dylan McDowell
;; Author: Dylan McDowell <dylan.mcdowell@inl.gov>
;; Maintainer: Dylan McDowell <dylan.mcdowell@inl.gov>
;; URL: https://github.com/dylanjm/emacs-moose-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "26.3"))

;;; Usage:
;; Put the following code in your init.el or other relevant file
;; (add-to-list 'load-path "path-to-moose-mode")
;; (require 'moose-mode)

;;; Commentary:
;; This file provides font-lock testing for `moose-mode'.

;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'moose-test-helpers)

(ert-deftest moose--test-font-locking-at-toplevel ()
  "Top level blocks get font-locked as keywords."
  (moose--should-font-lock
    "[Adaptivity]" 2 'font-lock-keyword-face))

(ert-deftest moose--test-font-locking-at-type ()
  "Keyword `type' will get font-locked as built-in"
  (moose--should-font-lock
    "type = TestType" 1 'font-lock-builtin-face)
  (moose--should-font-lock
    "type = TestType" 8 'font-lock-function-name-face))

(ert-deftest moose--test-font-locking-numerics ()
  (moose--should-font-lock
    "10000" 1 'font-lock-constant-face)
  (moose--should-font-lock
    "10000.5" 1 'font-lock-constant-face)
  (moose--should-font-lock
    "1e+4" 1 'font-lock-constant-face)
  (moose--should-font-lock
    "1e-4" 1 'font-lock-constant-face)
  (moose--should-font-lock
    "1.5e-4" 1 'font-lock-constant-face)
  (moose--should-font-lock
    "1.5e+4" 1 'font-lock-constant-face)
  ;; Moose should not highlight strings of alpha-numerics
  (moose--should-font-lock
    "6f2a5f12291f8875d1f80037aebd4cc4cc9faba1" 1 'nil))

(ert-deftest moose--test-font-locking-comments ()
  (moose--should-font-lock
    "# This is a comment" 1 'font-lock-comment-face)
  (moose--should-font-lock
    "type = TestType # Inline comment" 17 'font-lock-comment-face))

(ert-deftest moose--test-font-locking-strings ()
  (moose--should-font-lock
    "'x+cos(3)'" 1 'font-lock-string-face)
  (moose--should-font-lock
    "'x+cos(3)'" 2 'font-lock-variable-name-face)
  (moose--should-font-lock
    "'x+cos(3)'" 3 'nil)
  (moose--should-font-lock
    "'x+cos(3)'" 4 'font-lock-function-name-face)
  (moose--should-font-lock
    "'x+cos(3)'" 7 'nil)
  (moose--should-font-lock
    "'x+cos(3)'" 8 'font-lock-constant-face)
  (moose--should-font-lock
    "'x+cos(3)'" 9 'nil))

(provide 'moose-font-lock-tests)
;;; moose-font-lock-tests.el ends here
