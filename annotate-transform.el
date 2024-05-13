;;; annotate-transform.el --- Display transformers for variables and functions -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/annotate-transform
;; Keywords: lisp, help
;; Version: 0.1.2
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The package provides functions to annotate functions, variables during minibuffer completion.

;; Customization

;; `annotate-transform-ignored-variables'
;;      List of symbols that shouldn't be annotated.

;; `annotate-transform-max-doc-length'
;;      Max length for displaying documentation.

;; `annotate-transform-max-value-length'
;;      Max length for displaying value of a variable.

;;; Code:


(require 'cl-lib)
(eval-when-compile
  (require 'subr-x))

(defcustom annotate-transform-max-value-length 80
  "Max length for displaying value of a variable."
  :type 'integer
  :group 'annotate-transform)

(defcustom annotate-transform-max-doc-length 100
  "Max length for displaying documentation."
  :type 'integer
  :group 'annotate-transform)

(defcustom annotate-transform-align-column 80
  "Whether to align descriptions.
If nil, don't align, if integer align to those column."
  :type '(choice (const :tag "No align" nil)
                 (integer :tag "Column" 50))
  :group 'counsel-extra)

(defcustom annotate-transform-ignored-variables
  '(kill-ring
    obarray
    values
    buffer-face-mode-face
    buffer-face-toggle
    buffer-face-set
    buffer-menu
    buffer-face-mode-hook
    buffer-substring-filters
    buffer-stale-function
    buffer-face-mode
    buffer-face-mode-face
    buffer-face-remapping
    buffer-access-fontify-functions
    minibuffer-history
    tibetan-base-to-subjoined-alist
    tibetan-composite-vowel-alist
    buffer-undo-tree
    buffer-undo-list)
  "List of symbols that shouldn't be annotated."
  :type '(repeat symbol)
  :group 'annotate-transform)

(defcustom annotate-transform-hashtable-badge (propertize "HASH-TABLE"
                                                          'face
                                                          'font-lock-warning-face)
  "Badge displayed for hash table values.

A badge used to visually represent hash tables in formatted output.

It is displayed as \"HASH-TABLE\" with a warning face to distinguish hash tables
from other types of values in the output.

The appearance of the badge can be customized by changing its face or text.

Designed for use in functions that format Emacs Lisp values for readability,
particularly when displaying complex data structures."
  :group 'annotate-transform
  :type 'string)


(defcustom annotate-transform-obarray-badge (propertize "OBARRAY"
                                                        'face
                                                        'font-lock-warning-face)
  "Badge text for obarray values, styled with a warning face.

A string used to visually represent obarray values in annotations.

It is displayed with a warning face to highlight its presence among other
values. Customize to change the appearance or text of the badge for obarray
values in annotated transformations."
  :group 'annotate-transform
  :type 'string)

(defcustom annotate-transform-syntax-table-badge (propertize "SYNTAX-TABLE"
                                                             'face
                                                             'font-lock-warning-face)
  "Badge displayed for syntax table values.

Specifies the badge to represent syntax tables in formatted annotations.

The default value is the string \"SYNTAX-TABLE\" with a warning face applied to
it, indicating that the value being annotated is a syntax table.

To customize, set it to a string that represents syntax tables in a way that is
meaningful for the context in which annotations are used. The string can include
text properties such as faces to improve visual distinction."
  :group 'annotate-transform
  :type 'string)

(defcustom annotate-transform-compiled-function-badge (propertize "COMPILED-FUNCTION"
                                                                  'face
                                                                  'font-lock-warning-face)
  "Badge for displaying compiled functions visually.

Specifies the badge to represent compiled functions in annotations.

The default value is a string \"COMPILED-FUNCTION\" with a warning face applied
to it, indicating that the value being annotated is a compiled function.

To customize, set it to a string that visually distinguishes compiled functions
in your annotations. Applying a specific face for emphasis or differentiation is
recommended."
  :group 'annotate-transform
  :type 'string)

(defcustom annotate-transform-function-badge (propertize "FUNCTION"
                                                         'face
                                                         'font-lock-warning-face)
  "Default badge for displaying functions.

A string used to visually represent Emacs Lisp function values in annotations.

The default value is the word \"FUNCTION\" with a warning face applied to it,
making function values easily distinguishable in annotated content.

To customize, set it to any string value. Applying text properties, such as a
specific face, can enhance visibility and differentiation from other types of
values."
  :group 'annotate-transform
  :type 'string)

(defcustom annotate-transform-t-badge (propertize "t"
                                                  'face
                                                  'success)
  "Badge displayed for `true' values in annotations.

A string used to represent the boolean value t in formatted output.

It is visually enhanced with the `success' face to distinguish it from other
types of values in the output.

This customization allows for a clear and immediate recognition of `t' values
when inspecting Emacs Lisp values, enhancing readability and debugging
efficiency.

To change the representation, modify this variable to a different string or use
`propertize' to apply a different face or visual effect."
  :group 'annotate-transform
  :type 'string)

(defcustom annotate-transform-nil-badge (propertize "nil"
                                                    'face
                                                    'font-lock-comment-face)
  "Display style for nil values in annotations.

Specifies the badge to represent nil values in annotations.

The default is \"nil\" styled with `font-lock-comment-face' to distinguish nil
values visually in annotations.

To customize, set to a string that represents nil values as desired,
optionally using `propertize' to apply text properties such as face changes for
styling."
  :group 'annotate-transform
  :type 'string)



(defcustom annotate-transform-autoload-badge (propertize "AUTOLOAD"
                                                         'face
                                                         'font-lock-builtin-face)
  "String displayed for autoloaded functions.

A string used to indicate that a function is autoloaded, displayed in the
annotation of function names.

It is visually distinguished by applying the `font-lock-builtin-face' to enhance
readability and quick identification of autoloaded functions within annotated
lists or documentation.

Adjusting this value allows customization of the autoload indication, catering
to personal or project-specific aesthetics and readability preferences."
  :group 'annotate-transform
  :type 'string)

(defcustom annotate-transform-keymap-badge (propertize "KEYMAP"
                                                       'face
                                                       'font-lock-builtin-face)
  "String to visually represent keymaps in annotations.

Specifies the badge text to represent keymaps in transformed annotations.

The default value is the string \"KEYMAP\" with a font face of
`font-lock-builtin-face', making it visually distinct in annotations.

To customize, set it to a string that represents keymaps in a way that fits the
desired annotation style. The string can be propertized to include font face
attributes for styling."
  :group 'annotate-transform
  :type 'string)

(defun annotate-transform-trim (str max)
  "Trim STR if the length exceeds MAX, else return STR."
  (if (and (> (length str) max))
      (concat (substring-no-properties str 0 (- max 3)) "...")
    str))

(defun annotate-transform-trim-or-pad (str min-len &optional max-len)
  "Trim or pad a string STR to fit within a specified minimum and maximum length.

Argument STR is a string that the function will either trim or pad based on its
length compared to the minimum and maximum length parameters.
Argument MIN-LEN is the minimum length that the string should be, and if the
string is shorter than this, it will be padded with spaces until it reaches this
length.
Argument MAX-LEN is the maximum length that the string should be, and if the
string is longer than this, it will be trimmed down to this length, with the
last three characters replaced by an ellipsis."
  (let ((len (length str)))
    (unless max-len (setq max-len min-len))
    (cond ((> len max-len)
           (concat (substring str 0 (- max-len 3)) "..."))
          ((< len min-len)
           (concat str (make-string (- min-len len) ?\ )))
          (t str))))




(defun annotate-transform-annotate-function (fn-name)
  "Return string with active key and short documentation of FN-NAME.
FN-NAME should be a string."
  (let ((sym (intern fn-name)))
    (when (symbolp sym)
      (let ((args-str (ignore-errors
                        (annotate-transform-get-args-string
                         sym))))
        (concat
         (annotate-transform-trim-or-pad
          (if
              (autoloadp (symbol-function
                          sym))
              annotate-transform-autoload-badge
            "")
          9)
         (annotate-transform-trim-or-pad
          (if
              (and args-str
                   (string= args-str
                            "[arg list not available until function definition is loaded.]"))
              "[unknown]"
            (or args-str ""))
          20)
         "\s"
         "\s"
         (annotate-transform-trim-or-pad
          (annotate-transform-get-function-doc
           sym)
          90))))))

(defun annotate-transform-symbol-variable-p (symb)
  "Return t if SYMB is a variable."
  (and
   (symbolp symb)
   (boundp symb)
   (or (get symb 'variable-documentation)
       (and
        (keywordp symb)
        (not (memq symb '(nil t)))))))

(defun annotate-transform-annotatable-var-p (var-sym)
  "Return non nil if VAR-SYM can be annotated."
  (and
   (annotate-transform-symbol-variable-p var-sym)
   (null (memq var-sym annotate-transform-ignored-variables))))


(defun annotate-transform-get-function-key (sym buffer)
  "Return string with active key for command SYM in BUFFER.
SYM should be a symbol."
  (when (commandp sym)
    (with-current-buffer buffer
      (let ((k (where-is-internal sym nil t)))
        (when k
          (let ((i (cl-search [?\C-x ?6] k)))
            (when i
              (let ((dup (vconcat (substring k 0 i) [f2]
                                  (substring k (+ i 2))))
                    (map (current-global-map)))
                (when (equal (lookup-key map k)
                             (lookup-key map dup))
                  (setq k dup)))))
          (propertize (format "(%s)" (key-description k))
                      'face 'font-lock-variable-name-face))))))


(defun annotate-transform-get-function-doc (sym)
  "Return a stirng with short documentation of symbol SYM or nil.
SYM should be a symbol."
  (when-let ((documentation
              (if (fboundp sym)
                  (documentation sym t)
                (documentation-property
                 sym
                 'variable-documentation
                 t))))
    (and (stringp documentation)
         (string-match ".*$" documentation)
         (propertize (format "\s%s" (match-string
                                     0
                                     documentation))
                     'face
                     'font-lock-negation-char-face))))

(defvar annotate-transform-func-definitions-keywords-regex
  (regexp-opt '("&optional"
                "&rest"
                "&key"
                "&allow-other-keys"
                "&aux"
                "&declare"
                "&lambda")
              'symbols))

(defun annotate-transform-get-args-string (sym)
  "Return a string containing the parameter list of the function SYM.
INDEX is the index of the parameter in the returned string to highlight.
If SYM is a subr and no arglist is obtainable from the docstring
or elsewhere, return a 1-line docstring."
  (let ((argstring
         (cond ((not (and sym (symbolp sym)
                          (fboundp sym)))
                nil)
               ((and (eq sym (aref elisp--eldoc-last-data 0))
                     (eq 'function (aref elisp--eldoc-last-data 2)))
                (aref elisp--eldoc-last-data 1))
               (t
                (let* ((advertised (get-advertised-calling-convention
                                    (indirect-function sym)))
                       doc
                       (args
                        (cond ((listp advertised) advertised)
                              ((setq doc (help-split-fundoc
                                          (condition-case nil (documentation sym t)
                                            (invalid-function nil))
                                          sym))
                               (substitute-command-keys (car doc)))
                              (t (help-function-arglist sym)))))
                  (elisp--last-data-store sym (elisp-function-argstring args)
                                          'function))))))
    (when argstring
      (setq argstring (downcase argstring))
      (cond ((string-match-p annotate-transform-func-definitions-keywords-regex
                             argstring)
             (with-temp-buffer (insert argstring)
                               (while (re-search-backward
                                       annotate-transform-func-definitions-keywords-regex
                                       nil
                                       t 1)
                                 (add-face-text-property (point)
                                                         (match-end 0)
                                                         'font-lock-type-face))
                               (buffer-string)))
            (t argstring)))))


(defun annotate-transform-function-name (name)
  "Return NAME annotated with its active key binding and documentation.
NAME should be a string."
  (or (ignore-errors
        (string-trim-right
         (let ((buff
                (if-let ((minw (minibuffer-selected-window)))
                    (with-selected-window minw
                      (current-buffer))
                  (current-buffer)))
               (sym))
           (setq sym (intern name))
           (when (symbolp sym)
             (let* ((args-str
                     (let ((str (ignore-errors
                                  (annotate-transform-get-args-string
                                   sym))))
                       (if
                           (and str
                                (string= str
                                         "[arg list not available until function definition is loaded.]"))
                           "[unknown]"
                         (when str
                           (add-face-text-property 0 1
                                                   'font-lock-keyword-face nil
                                                   str)
                           (add-face-text-property (1- (length str))
                                                   (length str)
                                                   'font-lock-keyword-face
                                                   nil str))
                         str)))
                    (result
                     (concat
                      (annotate-transform-trim-or-pad
                       (concat name
                               (if-let ((k (annotate-transform-get-function-key
                                            sym buff)))
                                   (concat " " k)
                                 "")
                               " " (or args-str ""))
                       annotate-transform-align-column)
                      "\s"
                      (annotate-transform-trim-or-pad
                       (if
                           (autoloadp (symbol-function sym))
                           annotate-transform-autoload-badge
                         "")
                       9)
                      (annotate-transform-trim-or-pad
                       (annotate-transform-get-function-doc
                        sym)
                       90))))
               (cond ((eq sym major-mode)
                      (propertize result 'face 'font-lock-variable-name-face))
                     ((and
                       (memq sym minor-mode-list)
                       (boundp sym)
                       (buffer-local-value sym buff))
                      (propertize result 'face 'font-lock-builtin-face))
                     (t result)))))))
      name))

(defun annotate-transform--format-value (val)
  "Format various Emacs Lisp values into readable strings or badges.

Argument VAL is the value to format."
  (pcase val
    ((pred (eq t))
     annotate-transform-t-badge)
    ((pred (eq nil))
     annotate-transform-nil-badge)
    ((pred (syntax-table-p))
     annotate-transform-syntax-table-badge)
    ((pred (compiled-function-p))
     annotate-transform-compiled-function-badge)
    ((pred (symbolp))
     (format "%s" val))
    ((pred (stringp))
     (propertize
      (replace-regexp-in-string "[\n\r]" "\s"
                                (prin1-to-string val))
      'face 'font-lock-string-face))
    ((pred (functionp))
     annotate-transform-function-badge)
    ((pred (obarrayp))
     annotate-transform-obarray-badge)
    ((pred (keymapp))
     annotate-transform-keymap-badge)
    ((pred (hash-table-p))
     (let* ((hash-keys
             (hash-table-keys val))
            (description (format "%s" hash-keys)))
       (concat annotate-transform-hashtable-badge "\s"
               (format "(%s) %s"
                       (hash-table-size val)
                       (annotate-transform-trim
                        description
                        annotate-transform-max-value-length)))))
    (_ (propertize (replace-regexp-in-string "[\n\r]" "\s" (format "%s" val))
                   'face 'font-lock-variable-name-face))))

(defun annotate-transform-get-var-value (sym)
  "Return string with formatted value of symbol SYM."
  (ignore-errors
    (let ((val
           (if-let ((minw (minibuffer-selected-window)))
               (with-selected-window minw
                 (buffer-local-value sym (current-buffer)))
             (symbol-value sym))))
      (annotate-transform--format-value val))))

(defun annotate-transform-get-var-doc (sym)
  "Return string with annotated documentation of SYM."
  (ignore-errors
    (when-let ((doc (documentation-property sym 'variable-documentation t)))
      (let ((description (string-trim
                          (replace-regexp-in-string
                           "[\n].+" "" (substring-no-properties
                                        doc)))))
        (if (string-empty-p description)
            nil
          (propertize
           description
           'face 'font-lock-negation-char-face))))))

(defun annotate-transform-annotate-var-name (var-name &optional separator)
  "Return annotation for a variable VAR-NAME with its value and documentation.

Argument VAR-NAME is the variable name that is to be annotated and transformed.
Argument SEPARATOR is an optional parameter that specifies the character or
string to separate the variable value and its documentation."
  (let ((sym (if (symbolp var-name)
                 var-name
               (intern var-name))))
    (when (annotate-transform-annotatable-var-p sym)
      (string-join (delq nil
                         (list
                          (annotate-transform-get-var-value sym)
                          (annotate-transform-get-var-doc sym)))
                   (or separator "\s")))))

(defun annotate-transform-variable-name (var-name)
  "Return VAR-NAME annotated with its value and documentation."
  (let* ((sym (intern var-name))
         (doc (annotate-transform-get-var-doc sym))
         (value
          (ignore-errors
            (if-let ((minw (minibuffer-selected-window)))
                (with-selected-window minw
                  (buffer-local-value sym (current-buffer)))
              (symbol-value sym)))))
    (if (not (or value doc))
        var-name
      (string-join
       (delq nil
             (list
              (annotate-transform-trim-or-pad var-name 50)
              (annotate-transform-trim-or-pad
               (annotate-transform--format-value value)
               (if doc annotate-transform-max-value-length 0)
               annotate-transform-max-value-length)
              doc))
       " "))))

(provide 'annotate-transform)
;;; annotate-transform.el ends here