;;; moose-mode.el --- Major mode for editing MOOSE input files -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 moose-mode contributors
;; URL: https://github.com/dylanjm/emacs-moose-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "26.3"))

;;; Usage:
;; Put the following code in your init.el or other relevant file
;; (add-to-list 'load-path "path-to-moose-mode")
;; (require 'moose-mode)

;;; Commentary:
;; This is the official Emacs mode for editing MOOSE input files.

;;; License:
;; Copyright 2020-2021 Dylan McDowell
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
(require 'rx)
(require 'cl-lib)

(defgroup moose ()
  "Provides syntax highlighting for moose input files."
  :group 'languages
  :prefix "moose-")

(defcustom moose-indent-offset 2
  "Number of spaces per indentation level."
  :type 'integer)

;;;###autoload
(dolist (modes '( ("\\.i\\'" . moose-mode)
                  ("^assessment\\'" . moose-mode)
                  ("^test\\'" . moose-mode)))
  (add-to-list 'auto-mode-alist modes))

(defconst moose-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "w" table)          ;; . is part of a word
    (modify-syntax-entry ?_ "w" table)          ;; _ is part of a word
    (modify-syntax-entry ?= ". 23bn" table)     ;; = is punctuation
    (modify-syntax-entry ?' "\"" table)         ;; ' is a string delimiter
    (modify-syntax-entry ?\" "\"" table)        ;; " is a string delimiter too
    (modify-syntax-entry ?# "< 14" table)       ;; # is a comment starter
    (modify-syntax-entry ?\n ">" table)         ;; \n is a comment ender
    table))

(defvar moose-mode-hook nil)

(eval-when-compile
  (defun moose-string-keyword-matcher (regex)
    "Use REGEX to find keywords within strings and return closure."
    (lambda (end)
      (let (pos (case-fold-search t))
        (while (and (setq pos (re-search-forward regex end t))
                 (null (nth 3 (syntax-ppss pos)))))
        pos))))

(defconst moose-control-keywords-regexp-1
  (rx (group "[")
    (group
      (or "Adaptivity" "Bounds" "Mesh" "MeshGenerators" "MeshModifiers"
        "Kernels" "AuxKernels" "ScalarKernels" "AuxScalarKernels" "Variables"
        "AuxVariables" "Materials" "Postprocessors" "BCs" "ICs" "Executioner"
        "Outputs" "Problem" "Debug" "Preconditioning" "UserObjects" "Functions"
        "GlobalParams" "VectorPostprocessors" "Dampers" "DiracKernels"
        "DGKernels" "Constraints" "NodalNormals" "CoupledProblems"
        "DeprecatedBlock" "MultiApps" "Transfers" "InterfaceKernels"
        "NodalKernels" "Controls" "Modules"))
    (group "]")))

(defconst moose-control-keywords-regexp-2
  (rx "["
    (group "." "/")
    (group
      (or "TimeStepper" "TimePeriods" "Quadrature" "Predictor" "Adaptivity"
        "Indicators" "Markers" "Periodic" "InitialCondition" "MortarInterfaces"))
    "]"))

(defconst moose-function-regexp
  (rx (zero-or-more (in space))
    "["
    (? (group (+ ".") (+ "/")))
    (group (+ (in word)))
    "]"))

(defconst moose-type-variable-regexp
  (rx (group bow "type")
    (zero-or-more (in space))
    (group "=")
    (zero-or-more (in space))
    (group (one-or-more (not (any space))))))

(defconst moose-order-constant-regexp
  (rx bow "order"
    (zero-or-more (in space))
    (group "=")
    (zero-or-more (in space))
    (group
      (or "CONSTANT" "FIRST" "SECOND" "THIRD"  "FOURTH" "FIFTH" "SIXTH"
        "SEVENTH" "EIGHTH" "NINTH"))))

(defconst moose-family-constant-regexp
  (rx bow "family"
    (zero-or-more (in space))
    (group "=")
    (zero-or-more (in space))
    (group
      (or "LAGRANGE" "MONOMIAL" "HERMITE" "SCALAR" "HIERARCHIC" "CLOUGH"
        "XYZ" "SZABAB" "BERNSTEIN" "L2_LAGRANGE" "L2_HIERARCHIC"))))

(defconst moose-element-constant-regexp
  (rx bow "elem_type"
    (zero-or-more (in space))
    (group "=")
    (zero-or-more (in space))
    (group
      (or "EDGE" "EDGE2" "EDGE3" "EDGE4" "QUAD" "QUAD4" "QUAD8" "QUAD9" "TRI3"
        "TRI6" "HEX" "HEX8" "HEX20" "HEX27" "TET4" "TET10" "PRISM6" "PRISM15"
        "PRISM18"))))

(defconst moose-boolean-constant-regexp
  (rx bow (group (or "false" "true"))))

(defconst moose-numeric-regexp-1
  "[[:digit:]]*\\.[[:digit:]]+")

(defconst moose-numeric-regexp-2
  "[[:digit:]]+e[+-]\\{0,1\\}[[:digit:]]+")

(defconst moose-numeric-regexp-3
  "[[:digit:]]*\\.[[:digit:]]+e[+-]\\{0,1\\}[[:digit:]]+")

(defconst moose-numeric-regexp-4 "\\b[[:digit:]]+\\b")

(defconst moose-math-functions-regexp
  (rx (group
        (or "abs" "acos" "acosh" "arg" "asin" "atan" "atan2" "atanh" "cbrt"
          "ceil" "conj" "cos" "cosh" "cot" "csc" "exp" "exp2" "floor"
          "hypot" "if" "imag" "int" "log" "log10" "log2" "max" "min"
          "polar" "pow" "real" "sec" "sin" "sinh" "sqrt" "tan" "tanh"
          "trunc" "plog"))
    "("))

(defconst moose-comment-regexp "^ *\\(\\(#+\\).*\\)")
(defconst moose-inline-comment-regexp  "\\(\\(#+\\).*\\)")
(defconst moose-inline-operator-regexp "[\\+\\*\\/\\^%\\()-]")

(defconst moose-mode-font-lock-keywords
  `((,moose-comment-regexp (1 font-lock-comment-face))
     (,moose-inline-comment-regexp (1 font-lock-comment-face))
     (,moose-control-keywords-regexp-1 (2 font-lock-keyword-face))
     (,moose-control-keywords-regexp-2 (2 font-lock-keyword-face))
     (,moose-function-regexp (2 font-lock-function-name-face))
     (,moose-type-variable-regexp (1 font-lock-builtin-face))
     (,moose-type-variable-regexp (3 font-lock-function-name-face))
     (,moose-order-constant-regexp (2 font-lock-constant-face))
     (,moose-family-constant-regexp (2 font-lock-constant-face))
     (,moose-element-constant-regexp (2 font-lock-constant-face))
     (,moose-numeric-regexp-1 (0 font-lock-constant-face))
     (,moose-numeric-regexp-2 (0 font-lock-constant-face))
     (,moose-numeric-regexp-3 (0 font-lock-constant-face))
     (,moose-numeric-regexp-4 (0 font-lock-constant-face))
     (,moose-boolean-constant-regexp (1 font-lock-constant-face))
     (,(moose-string-keyword-matcher "\\sw+") 0 font-lock-variable-name-face t)
     (,(moose-string-keyword-matcher moose-inline-operator-regexp) 0 nil t)
     (,(moose-string-keyword-matcher moose-math-functions-regexp) 1 font-lock-function-name-face t)
     (,(moose-string-keyword-matcher moose-numeric-regexp-1) 0 font-lock-constant-face t)
     (,(moose-string-keyword-matcher moose-numeric-regexp-2) 0 font-lock-constant-face t)
     (,(moose-string-keyword-matcher moose-numeric-regexp-3) 0 font-lock-constant-face t)
     (,(moose-string-keyword-matcher moose-numeric-regexp-4) 0 font-lock-constant-face t)))

(defun moose-comment-or-string-p (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is inside string or comment."
  (nth 8 (or syntax-ppss (syntax-ppss))))

(defun moose-in-comment-p (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is inside comment."
  (nth 4 (or syntax-ppss (syntax-ppss))))

(defun moose-in-string-p (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is inside string."
  (nth 3 (or syntax-ppss (syntax-ppss))))

(defun moose-looking-at-beginning-of-block (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is at `beginning-of-block'."
  (and (not (moose-comment-or-string-p (or syntax-ppss (syntax-ppss))))
    (save-excursion
      (beginning-of-line 1)
      (looking-at moose-function-regexp))))

(defun moose-looking-at-end-of-block (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is at `beginning-of-block'."
  (and (not (moose-comment-or-string-p (or syntax-ppss (syntax-ppss))))
    (save-excursion
      (beginning-of-line 1)
      (looking-at
        (rx (zero-or-more (in space)) "[" (? (+ ".") (+ "/")) "]")))))

(defun moose-last-open-block-pos (min)
  "Return position of the last open block if found.
Do not move back beyone position MIN."
  (save-excursion
    (let ((nesting-count 0))
      (while (not (or (> nesting-count 0) (<= (point) min)))
        (backward-sexp)
        (setq nesting-count
          (cond
            ((moose-looking-at-beginning-of-block)
              (+ nesting-count 1))
            ((moose-looking-at-end-of-block)
              (- nesting-count 1))
            (t nesting-count))))
      (if (> nesting-count 0)
        (point)
        nil))))

(defun moose-last-open-block (min)
  "Move point back and return indentation level for last open block.
Do not move back beyond MIN."
  (setq min (max min (point-min)))
  (let ((pos (moose-last-open-block-pos min)))
    (and pos
      (progn
        (goto-char pos)
        (+ moose-indent-offset (current-indentation))))))

(defun moose-indent-line ()
  "Indent current line of MOOSE input file."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation))))
    (indent-line-to
      (save-excursion
        (beginning-of-line)
        (forward-to-indentation 0)
        (let ((endtok (moose-looking-at-end-of-block))
               (last-open-block (moose-last-open-block (- (point) 2000))))
          (max 0 (+ (or last-open-block 0)
                   (if endtok
                     (- moose-indent-offset)
                     0))))))
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

;;;###autoload
(define-derived-mode moose-mode prog-mode "MOOSE"
  "Major mode for editing MOOSE input files."
  :syntax-table moose-mode-syntax-table
  :group 'moose
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[[:space]]-*")
  (setq-local font-lock-defaults '(moose-mode-font-lock-keywords))
  (setq-local indent-line-function #'moose-indent-line)
  (setq indent-tabs-mode nil))

(provide 'moose-mode)
;;; moose-mode.el ends here
