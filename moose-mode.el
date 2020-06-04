;;; moose-mode.el --- Syntax Highlighting Mode for Moose Input Files -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'rx)


(eval-and-compile
  (defun moose-string-length< (a b) (< (length a) (length b)))
  (defun moose-string-length> (a b) (not (moose-string-length< a b))))

(eval-when-compile
  (defun moose-string-length> (e1 e2)
    "Predicate function for function `sort'. E1 & E2 are strings from a sequence."
    (> (length e1) (length e2))))

(defgroup moose-mode nil
  "Provides syntax highlighting for moose input files."
  :group 'languages)

(defcustom moose-control-keywords
  (eval-when-compile
    (sort '("Adaptivity" "AuxKernels" "AuxScalarKernels" "AuxVariables"
            "AxialRelocation" "BCs" "Bounds" "Burnup" "CladdingHydrides"
            "Constraints" "Contact" "Controls" "CoolantChannel" "DGKernels"
            "Dampers" "Debug" "DefaultElementQuality" "DeprecatedBlock"
            "DiracKernels" "Distributions" "DomainIntegral" "Executioner"
            "FluidPropertiesInterrogator" "Functions" "GlobalParams"
            "GrayDiffuseRadiation" "ICs" "InterfaceKernels" "Kernels"
            "LayeredPlenumTemperature" "Materials" "Mesh" "MeshGenerators"
            "MeshModifiers" "Modules" "MultiApps" "NodalKernels" "NodalNormals"
            "NuclearMaterials" "Outputs" "PerformanceMetricOutputs"
            "PlenumTemperature" "Postprocessors" "Preconditioning"
            "Problem" "Samplers" "ScalarKernels" "SolidMechanics"
            "StandardLWRFuelRodOutputs" "ThermalContact"
            "Transfers" "UserObjects" "Variables"
            "VectorPostprocessors" "XFEM")
          #'moose-string-length>))
  "List of MOOSE control keywords."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-sub-control-keywords
  (eval-when-compile
    (sort '("TimeStepper" "TimePeriods" "Quadrature" "Predictor"
            "Adaptivity" "Indicators" "Markers" "Periodic"
            "InitialCondition" "MortarInterfaces")
          #'moose-string-length>))
  "List of MOOST sub-control keywrods."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-mode-orders
  (eval-when-compile
    (sort '("CONSTANT" "FIRST" "SECOND" "THIRD"
            "FOURTH" "FIFTH" "SIXTH" "SEVENTH"
            "EIGHTH" "NINTH")
          #'moose-string-length>))
  "List of MOOSE polynomial orders."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-mode-families
  (eval-when-compile
    (sort '("LAGRANGE" "MONOMIAL" "HERMITE" "SCALAR"
            "HIERARCHIC" "CLOUGH" "XYZ" "SZABAB"
            "BERNSTEIN" "L2_LAGRANGE"
            "L2_HIERARCHIC")
          'moose-string-length>))
  "List of MOOSE families."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-mode-element-types
  (eval-when-compile
    (sort '("EDGE" "EDGE2" "EDGE3" "EDGE4" "QUAD" "QUAD4"
            "QUAD8" "QUAD9" "TRI3" "TRI6" "HEX" "HEX8"
            "HEX20" "HEX27" "TET4" "TET10" "PRISM6"
            "PRISM15" "PRISM18")
          'moose-string-length>))
  "List of MOOSE element types."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-mode-variable-constants
  (eval-when-compile
    (sort '("none" "initial" "linear" "nonlinear"
            "timestep_end" "timestep_begin"
            "final" "failed"
            "custom")
          #'moose-string-length>))
  "List of Moose var types."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)


(defcustom moose-mode-sub-variable-constants
  (eval-when-compile
    (sort '("initial" "linear" "nonlinear"
            "timestep_end" "timestep_begin" "final"
            "custom")
          #'moose-string-length>))
  "List of Moose var types."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-mode-math-functions
  (eval-when-compile
    (sort '("abs" "acos" "acosh" "arg" "asin" "atan" "atan2" "atanh"
            "cbrt" "ceil" "conj" "cos" "cosh" "cot" "csc" "exp" "exp2"
            "floor" "hypot" "if" "imag" "int" "log" "log10" "log2"
            "max" "min" "polar" "pow" "real" "sec" "sin" "sinh"
            "sqrt" "tan" "tanh" "trunc" "plog")
          #'moose-string-length>))
  "List of MOOSE built-in math functions."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)


(defcustom moose-mode-operators
  (eval-when-compile
    (sort '("+" "*" "/" "%" "-") 'moose-string-length>))
  "List of MOOSE operators."
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'moose-mode)

(defcustom moose-mode-literal-booleans
  (eval-when-compile
    (sort '("TRUE" "true" "YES" "yes" "ON" "on"
             "FALSE" "false" "NO" "no" "OFF" "off")
      'moose-string-length>))
  "List of moose-mode boolean literals."
  :type '(choice (const :tag "Disabled" nil)
           (repeat string))
  :group 'moose-mode)

(defvar moose-mode-keywords nil)

(defun moose-mode-generate-font-lock-keywords ()
  "Create all regexp for moose-mode blocks."
  <<<<<<< HEAD
  (let ((types-regexp     (regexp-opt moose-mode-types 'symbols))
        (orders-regexp   (regexp-opt moose-mode-orders 'words))
        (family-regexp   (regexp-opt moose-mode-families 'words))
        (elem-regexp     (regexp-opt moose-mode-element-types 'words))
        (operator-regexp (regexp-opt moose-mode-operators))
        (boolean-regexp  (regexp-opt moose-mode-literal-booleans 'words))
        (number-regexp   "[+-]?[0-9]*(\\.[0-9]*)?([eE][+-][0-9]+)?"))
    (setq moose-mode-keywords
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,types-regexp (0 font-lock-keyword-face))
            (,family-regexp (1 font-lock-constant-face))
            (,orders-regexp (0 font-lock-constant-face))
            (,elem-regexp (0 font-lock-constant-face))
            (,operator-regexp (0 font-lock-constant-face))))))

(defcustom moose-mode-literal-boolean
  t
  "Enable font-lock for boolean literals. For more information."
  :type 'boolean
  :group 'moose-mode)

(defvar moose-mode-font-lock-literal-boolean nil)

(defun moose-mode-generate-font-lock-literal-boolean ()
  (let ((literal-boolean-regexp (regexp-opt
                                 (eval-when-compile (sort '("false" "true") 'moose-string-length>))
                                 'words)))
    (setq moose-mode-font-lock-literal-boolean
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,literal-boolean-regexp (0 font-lock-constant-face))))))



(defun moose-mode-add-keywords (&optional mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (font-lock-add-keywords mode
                          (moose-mode-generate-font-lock-keywords) nil))

(defun moose-mode-remove-keywords (&optional mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (font-lock-remove-keywords mode moose-mode-keywords))

(defconst moose-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; # is a comment starter
    (modify-syntax-entry ?# ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))



(define-derived-mode my-moose-mode fundamental-mode "MOOSE"
  :syntax-table moose-mode-syntax-table
  :group 'moose-mode
  (if moose-mode
      (moose-mode-add-keywords)
    (moose-mode-remove-keywords))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-minor-mode moose-mode
  "Provides font-locking as for MOOSE Input File"
  :init-value nil
  :lighter " moose"
  :group 'moose-mode
  (if moose-mode
      (moose-mode-add-keywords)
    (moose-mode-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))



;;;###autoload
(define-global-minor-mode moose-mode-global-mode moose-mode
  (lambda ()
    (when (apply 'derived-mode-p '(c++-mode))
      (moose-mode 1)))
  :group 'moose-mode)


(provide 'moose-mode)
;;; moose-mode.el ends here
