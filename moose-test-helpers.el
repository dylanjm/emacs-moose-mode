;;; moose-test-helpers.el --- Utilities for `moose-mode' tests -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Dylan McDowell
;; Author: Dylan McDowell <dylan.mcdowell@inl.gov>
;; Maintainer: Dylan McDowell <dylan.mcdowell@inl.gov>
;; URL: https://github.com/dylanjm/emacs-moose-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "26.3"))

;;; Usage:
;; Put the following code in your init.el or other relevant file
;; (add-to-list 'load-path "<path-to-moose-mode>")
;; (require 'moose-mode)

;;; Commentary:
;; This file provides utilities for the `moose-mode' testing suite.

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
(require 'moose-mode)

(defmacro moose--should-indent (from to)
  "Assert that indent text FROM produces text TO in `moose-mode'."
  `(with-temp-buffer
     (let ((moose-indent-offset 2))
       (moose-mode)
       (insert ,from)
       (let ((inhibit-message t))
         (indent-region (point-min) (point-max)))
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                 ,to)))))

(defmacro moose--should-font-lock (text pos face)
  "Assert that TEXT at position POS gets font-locked with FACE in `moose-mode'."
  `(with-temp-buffer
     (moose-mode)
     (insert ,text)
     (if (fboundp 'font-lock-ensure)
       (font-lock-ensure (point-min) (point-max))
       (with-no-warnings
         (font-lock-fontify-buffer)))
     (should (eq ,face (get-text-property ,pos 'face)))))

(defmacro moose--should-move-point (text fun from to &optional end arg)
  "Assert that TEXT in `moose-mode', after calling FUN, the point should move FROM
to TO."
  (declare (indent defun))
  `(with-temp-buffer
     (moose-mode)
     (insert ,text)
     (let ((inhibit-message t))
       (indent-region (point-min) (point-max)))
     (goto-char (point-min))
     (if (stringp ,from)
       (re-search-forward ,from)
       (goto-char ,from))
     (funcall ,fun ,arg)
     (should (eq (point) (if (stringp ,to)
                           (progn (goto-char (point-min))
                             (re-search-forward ,to)
                             (if ,end (goto-char (match-end 0))
                               (goto-char (match-beginning 0))
                               (point-at-bol)))
                           ,to)))))

  (provide 'moose-test-helpers)
;;; moose-test-helpers.el ends here
