;;; moose-indent-tests.el --- Indentation Tests for moose-mode.el

;; Copyright (C) 2020-2021 Dylan McDowell
;; Author: Dylan McDowell <dylan.mcdowell@inl.gov>
;; Maintainer: Dylan McDowell <dylan.mcdowell@inl.gov>
;; URL: https://github.com/dylanjm/emacs-moose-mode

;;; Commentary:
;; Contains ert tests for moose-mode.el

;;; License:
;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'moose-mode-test)

(ert-deftest moose--test-indent-old-syntax ()
  "`moose-mode' should indent even when deprecated syntax is used."
  (moose--should-indent
    "
[./sub_block_func]
type = TestType
variable = w
[../]
"
    "
[./sub_block_func]
  type = TestType
  variable = w
[../]
"))

(ert-deftest moose--test-indent-single-block ()
  "`moose-mode' should indent correctly inside non-nested blocks."
  (moose--should-indent
    "
[Adaptivity]
type = TestType
variable = w
[]
"
    "
[Adaptivity]
  type = TestType
  variable = w
[]
"))

(ert-deftest moose--test-indent-nested-block ()
  "`moose-mode' should indent correctly nested blocks."
  (moose--should-indent
    "
[Adaptivity]
type = TestType
[sub_block_func]
type = TestType
variable = w
[]
[]
"
    "
[Adaptivity]
  type = TestType
  [sub_block_func]
    type = TestType
    variable = w
  []
[]
"))

(ert-deftest moose--test-indent-multiple-nested-block ()
  "`moose-mode' should indent correctly inside multiple nested blocks."
  (moose--should-indent
    "
[Adaptivity]
[sub_block_func_01]
type = TestType
[sub_block_func_02]
type = TestType
[sub_block_func_03]
type = TestType
[]
[]
[]
[]
"
    "
[Adaptivity]
  [sub_block_func_01]
    type = TestType
    [sub_block_func_02]
      type = TestType
      [sub_block_func_03]
        type = TestType
      []
    []
  []
[]
"))

;;; run-all tests
(defun moose--run-tests ()
  "Run all `moose-mode-tests'."
  (interactive)
  (if (featurep 'ert)
    (ert-run-tests-interactively "moose--test")
    (message "Can't run moose-mode-tests because ert is not available.")))


(provide 'moose-mode-tests.el)
;; Local Variables:
;; coding: utf-8
;; End:
;;; moose-mode-tests.el ends here
