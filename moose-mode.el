;;; moose-mode.el --- Syntax Highlighting for MOOSE  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'rx)

(eval-when-compile
  (defun moose-string-length> (e1 e2)
    "Predicate function for function `sort'."
    (> (length e1) (length e2))))

(defgroup moose ()
  "Provides syntax highlighting for moose input files."
  :group 'languages
  :prefix "moose-")

(defvar moose-mode-hook nil)

(defconst moose-control-keywords-regexp-1
  (rx (group "[")
    (group
      (or "Adaptivity" "Bounds" "Mesh" "MeshGenerators" "MeshModifiers" "Kernels"
        "AuxKernels" "ScalarKernels" "AuxScalarKernels" "Variables" "AuxVariables"
        "Materials" "Postprocessors" "BCs" "ICs" "Executioner" "Outputs" "Problem"
        "Debug" "Preconditioning" "UserObjects" "Functions" "GlobalParams"
        "VectorPostprocessors" "Dampers" "DiracKernels" "DGKernels"
        "Constraints" "NodalNormals" "CoupledProblems" "DeprecatedBlock"
        "MultiApps" "Transfers" "InterfaceKernels" "NodalKernels" "Controls"
        "Modules"))
    (group "]")))

(defconst moose-control-keywords-regexp-2
  (rx "["
    (group "." "/")
    (group
      (or "TimeStepper" "TimePeriods" "Quadrature" "Predictor" "Adaptivity"
        "Indicators" "Markers" "Periodic" "InitialCondition" "MortarInterfaces"))
    "]"))

(defconst moose-function-regexp
  (rx "[" (group (* ".") (* "/")) (group (* any)) "]"))

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

(defconst moose-numeric-regexp-4
  "[[:digit:]]+")

(defconst moose-math-functions-regexp
  (rx (group
        (or "abs" "acos" "acosh" "arg" "asin" "atan" "atan2" "atanh" "cbrt"
          "ceil" "conj" "cos" "cosh" "cot" "csc" "exp" "exp2" "floor"
          "hypot" "if" "imag" "int" "log" "log10" "log2" "max" "min"
          "polar" "pow" "real" "sec" "sin" "sinh" "sqrt" "tan" "tanh"
          "trunc" "plog"))
    "("))

(defconst moose-comment-regexp
  "^ *\\(\\(#+\\).*\\)")

(defconst moose-inline-comment-regexp
  "\\(\\(#+\\).*\\)")

(defconst moose-mode-font-lock-keywords
  `((,moose-comment-regexp (1 font-lock-comment-face))
     (,moose-inline-comment-regexp (1 font-lock-comment-face))
     (,moose-control-keywords-regexp-1 (2 font-lock-keyword-face))
     (,moose-control-keywords-regexp-2 (2 font-lock-keyword-face))
     (,moose-function-regexp (2 font-lock-function-name-face))
     (,moose-type-variable-regexp (1 font-lock-keyword-face))
     (,moose-type-variable-regexp (3 font-lock-function-name-face))
     (,moose-order-constant-regexp (2 font-lock-constant-face))
     (,moose-family-constant-regexp (2 font-lock-constant-face))
     (,moose-element-constant-regexp (2 font-lock-constant-face))
     (,moose-numeric-regexp-1 (0 font-lock-constant-face))
     (,moose-numeric-regexp-2 (0 font-lock-constant-face))
     (,moose-numeric-regexp-3 (0 font-lock-constant-face))
     (,moose-numeric-regexp-4 (0 font-lock-constant-face))
     (,moose-math-functions-regexp (1 font-lock-function-name-face))
     (,moose-boolean-constant-regexp (1 font-lock-constant-face))))

(defconst moose-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)     ;; _ is part of a word
    ;;(modify-syntax-entry ?' "\"" table)    ;; ' is a string delimiter
    (modify-syntax-entry ?\" "\"" table)   ;; " is a string delimiter too
    (modify-syntax-entry ?# ". 12" table)  ;; # is a comment starter
    (modify-syntax-entry ?\n ">" table)    ;; \n is a comment ender
    table))

(define-derived-mode moose-mode prog-mode "MOOSE"
  :syntax-table moose-mode-syntax-table
  :group 'moose
  (setq-local font-lock-defaults '(moose-mode-font-lock-keywords)))

(provide 'moose-mode)
;;; moose-mode.el ends here
