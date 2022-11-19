;;; annotate-transform.el --- Display transformers for variables and functions -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/annotate-transform
;; Keywords: lisp, help
;; Version: 0.1.2
;; Package-Requires: ((emacs "27.1"))

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

(defcustom annotate-transform-max-value-length 60
  "Max length for displaying value of a variable."
  :type 'integer
  :group 'annotate-transform)

(defcustom annotate-transform-max-doc-length 100
  "Max length for displaying documentation."
  :type 'integer
  :group 'annotate-transform)

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

(defvar annotate-transform-hashtable-badge
  (propertize "HASH-TABLE"
              'face
              'font-lock-warning-face))

(defun annotate-transform-trim (str max)
  "Trim STR if the length exceeds MAX, else return STR."
  (if (and (> (length str) max))
      (concat (substring-no-properties str 0 (- max 3)) "...")
    str))

;;;###autoload
(defun annotate-transform-annotate-function (fn-name)
  "Return string with active key and short documentation of FN-NAME.
FN-NAME should be a string."
  (let ((buff
         (if-let ((minw (minibuffer-selected-window)))
             (with-selected-window minw
               (current-buffer))
           (current-buffer)))
        (sym))
    (setq sym (intern fn-name))
    (when (symbolp sym)
      (string-join
       (delete
        nil
        (list
         (ignore-errors (elisp-get-fnsym-args-string sym))
         (annotate-transform-get-function-key
          sym buff)
         (annotate-transform-get-function-doc
          sym)))
       "\s"))))

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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun annotate-transform-function-name (name)
  "Return NAME annotated with its active key binding and documentation.
NAME should be a string."
  (or (ignore-errors
        (let ((buff
               (if-let ((minw (minibuffer-selected-window)))
                   (with-selected-window minw
                     (current-buffer))
                 (current-buffer)))
              (sym))
          (setq sym (intern name))
          (when (symbolp sym)
            (let ((result (concat name "\s"
                                  (annotate-transform-annotate-function name))))
              (cond ((eq sym major-mode)
                     (propertize result 'face 'font-lock-variable-name-face))
                    ((and
                      (memq sym minor-mode-list)
                      (boundp sym)
                      (buffer-local-value sym buff))
                     (propertize result 'face 'font-lock-builtin-face))
                    (t result))))))
      name))

;;;###autoload
(defun annotate-transform-get-var-value (sym)
  "Return string with formatted value of symbol SYM."
  (ignore-errors
    (let ((val (if-let ((minw (minibuffer-selected-window)))
                   (with-selected-window minw
                     (buffer-local-value sym (current-buffer)))
                 (symbol-value sym))))
      (and val
           (or (not (hash-table-p val))
               (not (hash-table-empty-p val)))
           (string-join
            (split-string
             (or (when-let ((special-type (cond ((keymapp val)
                                                 "KEYMAP")
                                                ((syntax-table-p val)
                                                 "SYNTAX-TABLE"))))
                   (propertize special-type 'face 'font-lock-builtin-face))
                 (when-let* ((hash-keys (when (hash-table-p val)
                                          (hash-table-keys val)))
                             (description (format "%s" hash-keys)))
                   (concat annotate-transform-hashtable-badge "\s"
                           (format "(%s) %s"
                                   (hash-table-size val)
                                   (annotate-transform-trim
                                    description
                                    annotate-transform-max-value-length))))
                 (propertize
                  (annotate-transform-trim (format "%s" val)
                                           annotate-transform-max-value-length)
                  'face 'font-lock-variable-name-face))
             nil t)
            "\s")))))

;;;###autoload
(defun annotate-transform-get-var-doc (sym)
  "Return string with annotated documentation of SYM."
  (ignore-errors
    (when-let ((doc (documentation-property sym 'variable-documentation t)))
      (let ((description (string-join
                          (split-string
                           doc
                           nil t)
                          "\s")))
        (propertize
         (annotate-transform-trim description
                                  annotate-transform-max-doc-length)
         'face 'font-lock-negation-char-face)))))

;;;###autoload
(defun annotate-transform-annotate-var-name (var-name &optional separator)
  "Concatenate value and documentation of VAR-NAME with SEPARATOR.
VAR-NAME should be a string.
Default value for SEPARATOR is space."
  (let ((sym (if (symbolp var-name)
                 var-name
               (intern var-name))))
    (when (annotate-transform-annotatable-var-p sym)
      (string-join (delete nil
                           (list
                            (annotate-transform-get-var-value sym)
                            (annotate-transform-get-var-doc sym)))
                   (or separator "\s")))))

;;;###autoload
(defun annotate-transform-variable-name (var-name)
  "Return VAR-NAME annotated with its value and documentation."
  (if-let ((annotation (ignore-errors (annotate-transform-annotate-var-name
                                       var-name))))
      (concat var-name "\s" annotation)
    var-name))

(provide 'annotate-transform)
;;; annotate-transform.el ends here