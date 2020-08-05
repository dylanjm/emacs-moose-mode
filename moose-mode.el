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

(defun moose-mode-generate-font-lock-keywords ()
  "Create all regexp for moose-mode blocks.")

(defconst moose-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)    ;; ' is a string delimiter
    (modify-syntax-entry ?\" "\"" table)   ;; " is a string delimiter too
    (modify-syntax-entry ?# ". 12" table)  ;; # is a comment starter
    (modify-syntax-entry ?\n ">" table)    ;; \n is a comment ender
    table))

(define-derived-mode moose-mode prog-mode "MOOSE"
  :syntax-table moose-mode-syntax-table
  :group 'moose-mode)


(provide 'moose-mode)
;;; moose-mode.el ends here
