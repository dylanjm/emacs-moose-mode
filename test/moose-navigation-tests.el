;;; moose-navigation-tests.el --- Navigation Tests for `moose-mode'.

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
;; This file provides navigation tests for `moose-mode'.

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

(ert-deftest moose--test-navigation-beginning-of-block ()
  "Assert that point moves to beginning of blocks."
  (moose--should-move-point
    "[Adaptivity]
  type = TestType
  variable = w
[]
" 'beginning-of-defun "w$" 1))

(provide 'moose-navigation-tests)
;;; moose-navigation-tests.el ends here
