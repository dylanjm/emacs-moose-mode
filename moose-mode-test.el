;;; moose-mode-test.el --- Utilities for moose-mode tests -*- lexical-binding: t; -*-
;;; Commentary:
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

(provide 'moose-mode-test)
;;; moose-mode-test.el ends here
