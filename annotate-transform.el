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

(defcustom annotate-transform-variable-alist `((,(apply-partially #'eq t) "t"
                                                nil)
                                               (not "nil" font-lock-comment-face)
                                               (numberp number-to-string nil)
                                               (symbolp symbol-name nil)
                                               (syntax-table-p "SYNTAX-TABLE"
                                                font-lock-warning-face)
                                               (annotate-transform-oclosure-p
                                                "#<oclosure"
                                                font-lock-warning-face)
                                               (byte-code-function-p
                                                "#<byte-code-function>"
                                                font-lock-warning-face)
                                               (recordp "RECORD"
                                                font-lock-warning-face)
                                               (bool-vector-p "BOOL-VECTOR"
                                                font-lock-warning-face)
                                               (annotate-transform-abbrev-table-p
                                                "ABBREV-TABLE")
                                               (char-table-p "CHARTABLE"
                                                font-lock-builtin-face)
                                               (compiled-function-p
                                                "COMPILED-FUNCTION"
                                                font-lock-warning-face)
                                               (annotate-transform-long-vector-p
                                                annotate-transform-long-vector
                                                font-lock-warning-face)
                                               (functionp "FUNCTION"
                                                font-lock-warning-face)
                                               (obarrayp "OBARRAY"
                                                font-lock-warning-face)
                                               (keymapp "KEYMAP"
                                                font-lock-builtin-face)
                                               (hash-table-p "HASH-TABLE"
                                                font-lock-warning-face)
                                               (autoloadp "AUTOLOAD"
                                                font-lock-builtin-face)
                                               (annotate-transform--multi-line-stringp
                                                "MULTI LINE STRING"
                                                font-lock-string-face))
  "Alist mapping predicates to their string representations and optional faces.

An alist mapping predicates to their corresponding string
representations and optional font-lock faces.

Each element is a list where the first item is a predicate function
that takes a value and returns non-nil if the value matches the
predicate.

The second item is either a string or a function. If it is a string,
it will be used as the representation for values matching the
predicate. If it is a function, it will be called with the value and
should return a string representation.

The third item is an optional face to apply to the string
representation. If nil, no face will be applied."
  :group 'annotate-transform
  :type '(alist
          :key-type function
          :value-type (list
                       (radio
                        string
                        function)
                       (radio :tag "Face"
                        (face)
                        (const nil)))))

(defvar annotate-transform-hash (make-hash-table :test #'equal))

(defun annotate-transform-cache-variable-annotation (&rest _)
  "Update `annotate-transform-hash' with annotated variable names."
  (setq annotate-transform-hash (clrhash annotate-transform-hash))
  (mapatoms (lambda (it)
              (when (annotate-transform-symbol-variable-p it)
                (let ((name (symbol-name it)))
                  (puthash name
                           (annotate-transform-variable-name name)
                           annotate-transform-hash))))))



(defcustom annotate-transform-not-bound-value-badge (propertize "NOT BOUND"
                                                                'face
                                                                'font-lock-warning-face)
  "Badge for displaying not bounded variables values.

Specifies the badge to represent compiled functions in annotations.

The default value is a string \"COMPILED-FUNCTION\" with a warning face applied
to it, indicating that the value being annotated is a compiled function.

To customize, set it to a string that visually distinguishes compiled functions
in your annotations. Applying a specific face for emphasis or differentiation is
recommended."
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
           (concat (substring-no-properties str 0 (- max-len 3)) "..."))
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
   (or (and (boundp symb)
            (not (keywordp symb)))
       (get symb 'variable-documentation))))

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
  (when-let* ((documentation
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
                (if-let* ((minw (minibuffer-selected-window)))
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
                               (if-let* ((k (annotate-transform-get-function-key
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

(defun annotate-transform-read-variable ()
  "Annotate-Transform."
  (let ((vals)
        (orig-buffer (current-buffer)))
    (mapatoms
     (lambda (sym)
       (when (or (get sym 'variable-documentation)
                 (and (not (keywordp sym))
                      ;; Since the variable may only exist in the
                      ;; original buffer, we have to look for it
                      ;; there.
                      (buffer-local-boundp sym orig-buffer)))
         (push
          (cons sym
                (annotate-transform--format-value
                 (condition-case nil
                     (symbol-value sym)
                   (error (message "sym %s unbound" sym)
                          "NOT_BOUND"))
                 annotate-transform-max-value-length))
          vals))))
    (length vals)))

(defun annotate-transform-abbrev-table-p (val)
  "Return non-nil if VAL is an abbreviation table.

Argument VAL is the value to be checked if it is an abbrev table."
  (or (and (eval-when-compile (< emacs-major-version 30))
           (vectorp val)
           (ignore-errors (abbrev-table-p val)))
      (abbrev-table-p val)))

(defun annotate-transform-oclosure-p (val)
  "Return non-nil if VAL is an oclosure.

Argument VAL is the value to be checked if it is an oclosure."
  (and (fboundp 'oclosure-type)
       (oclosure-type val)))


(defun annotate-transform--multi-line-stringp (value)
  "Check if VALUE is a string containing newline, carriage return, or form feed.

Argument VALUE is the string to be checked for multiple lines."
  (and (stringp value)
       (string-match-p "[\n\r\f]" value)))

(defun annotate-transform-long-vector-p (vect)
  "Check if VECT is a vector with a length of at least 256.

Argument VECT is the vector to be checked for length and type."
  (and (vectorp vect)
       (>= (length vect) 256)))

(defun annotate-transform-long-vector (vect)
  "Return a formatted string indicating the length of the vector VECT.

Argument VECT is the vector to be transformed and annotated."
  (format "LONG VECTOR (%d)" (length vect)))

(defun annotate-transform-truncate-newlines (val)
  "Return VAL truncated at the first newline or carriage return character.

Argument VAL is the string to be truncated at the first newline character."
  (let* ((start (string-match-p "[\n\r\f]" val)))
    (substring-no-properties val 0 start)))

(defun annotate-transform--format-value (val &optional max-len)
  "Format VAL based on its type, optionally trimming it to MAX-LEN.

Argument VAL is the value to be formatted.

Optional argument MAX-LEN is the maximum length of the formatted string."
  (pcase-let ((`(,_pred ,formatter ,face)
               (seq-find (pcase-lambda (`(,pred . _))
                           (funcall pred val))
                         annotate-transform-variable-alist)))
    (cond
     ((not formatter)
      (if (stringp val)
          (propertize
           (prin1-to-string
            (annotate-transform-truncate-newlines
             (annotate-transform-trim
              val
              (or max-len
                  annotate-transform-max-value-length))))
           'face
           'font-lock-string-face)
        (let ((str (annotate-transform-truncate-newlines
                    (prin1-to-string
                     val))))
          (annotate-transform-trim str
                                   (or max-len
                                       annotate-transform-max-value-length)))))
     ((stringp formatter)
      (propertize (substring-no-properties formatter)
                  'face face))
     ((functionp formatter)
      (funcall formatter val)))))

(defun annotate-transform-get-var-value (sym)
  "Return string with formatted value of symbol SYM."
  (ignore-errors
    (let ((val
           (if-let* ((minw (minibuffer-selected-window)))
               (with-selected-window minw
                 (buffer-local-value sym (current-buffer)))
             (symbol-value sym))))
      (annotate-transform--format-value val))))

(defun annotate-transform-get-var-doc (sym)
  "Return string with annotated documentation of SYM."
  (ignore-errors
    (when-let* ((doc (documentation-property sym 'variable-documentation t)))
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
  (or (gethash var-name annotate-transform-hash)
      (let* ((sym (intern var-name))
             (doc (annotate-transform-get-var-doc sym))
             (unbound-var)
             (value)
             (res))
        (condition-case nil
            (setq value
                  (if-let* ((minw (minibuffer-selected-window)))
                      (with-selected-window minw
                        (buffer-local-value sym (current-buffer)))
                    (symbol-value sym)))
          (error (setq unbound-var t)))
        (setq res (if (not (or value doc))
                      var-name
                    (concat
                     (annotate-transform-trim-or-pad var-name 50)
                     " "
                     (let
                         ((str
                           (if unbound-var
                               (annotate-transform-trim-or-pad
                                annotate-transform-not-bound-value-badge
                                annotate-transform-max-value-length)
                             (annotate-transform--format-value
                              value
                              annotate-transform-max-value-length))))
                       (if (and doc
                                (< (length str) annotate-transform-max-value-length))
                           (concat str (make-string
                                        (- annotate-transform-max-value-length (length str))
                                        ?\ ))
                         str))
                     doc)))
        res)))

(provide 'annotate-transform)
;;; annotate-transform.el ends here