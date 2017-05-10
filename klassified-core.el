;;; klassified-core.el --- Structures and manipulating functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)

(eval-when-compile
  (require 'rx))

(cl-defstruct (klassified-core-position
               (:constructor klassified-core--position-make)
               (:conc-name klassified-core--position-))
  file
  line
  project)

(cl-defstruct (klassified-core-classref
               (:constructor klassified-core--classref-make)
               (:conc-name klassified-core--classref-))
  name)


;;; Class

(cl-defstruct (klassified-core-class
               (:constructor klassified-core--class-make)
               (:conc-name klassified-core--class-))
  name
  superclass-ref
  definition
  stubp
  abstractp)

(defun klassified-core-class-name (class)
  "Return name of CLASS."
  (klassified-core--class-name class))

(defun klassified-core-class-abstract-p (class)
  "Return t if and only if CLASS is abstract."
  (klassified-core--class-abstractp class))

(defun klassified-core-class-stub-p (class)
  "Return t iff CLASS is a class-stub."
  (klassified-core--class-stubp class))

(defun klassified-core-class-filepath (class)
  "Return the file path defining CLASS relative to project."
  (if-let ((definition (klassified-core--class-definition class)))
      (klassified-core--position-file definition)))

(defun klassified-core-class-projectpath (class)
  "Return the project path defining CLASS."
  (if-let ((definition (klassified-core--class-definition class)))
      (klassified-core--position-project definition)))

(defun klassified-core-class-full-filepath (class)
  "Return absolute file path defining CLASS."
  (expand-file-name (klassified-core-class-filepath class)
                    (klassified-core-class-projectpath class)))

(defun klassified-core-class-line (class)
  "Return the line number defining CLASS."
  (if-let ((definition (klassified-core--class-definition class)))
      (klassified-core--position-line definition)))

(defun klassified-core-class-superclass-name (class)
  "Return the name of CLASS' superclass."
  (if-let ((superclass-ref (klassified-core--class-superclass-ref class)))
      (klassified-core--classref-name superclass-ref)))

(defun klassified-core-class-stub-make (name)
  "Return a class-stub named NAME.

This is useful when a class has a superclass that is part of another
project.  In this class, the superclass can be a class-stub."
  (klassified-core--class-make :name name :stubp t))

(defun klassified-core-buffer-editing-class (class)
  "Return a buffer visiting CLASS."
  (find-file-noselect (klassified-core-class-full-filepath class)))

(defun klassified-core-move-to-class (class)
  "Return a buffer defining CLASS and move point there."
  (with-current-buffer (klassified-core-buffer-editing-class class)
    (widen)
    (goto-char (point-min))
    (forward-line (1- (klassified-core-class-line class)))
    (current-buffer)))

(defvar klassified-core-class-regexp
  (rx
   (or "return "
       ;; optional variable declaration (2)
       (and (or "let" "var")
            space
            (group-n 2 (+ (not space)))
            " = "))
   ;; superclass reference (4)
   (group-n 4 (+? (not space)))
   "."
   (group-n 5 (or "subclass" "abstractSubclass"))
   "(")
  "A regexp matching a class definition in a search result.
When matching, this regexp puts the class name (if any) inside group 2 and
the superclass reference inside group 4.  The subclass creation function is
put in group 5.")

(defun klassified-core-class-make-from-match-data (match-data &optional projectpath filepath line)
  "Create a new class based on MATCH-DATA.

This calls `klassified-core--class-make-from-extracted-data' with strings
extracted from MATCH-DATA and PROJECTPATH, FILEPATH and LINE unchanged.

MATCH-DATA at 2 should be the class name, nil or the empty string.

MATCH-DATA at 4 should be the string used to reference the super class.

MATCH-DATA at 5 should be the JS function name used to create the
subclass (i.e., \"subclass\" or \"abstractSuclass\")."
  (save-match-data
    (set-match-data match-data)
    (klassified-core--class-make-from-extracted-data
     (match-string-no-properties 2) ; optional class name
     (match-string-no-properties 4) ; superclass reference
     (match-string-no-properties 5) ; subclass message send
     projectpath
     filepath
     line)))

(defun klassified-core--class-make-from-extracted-data (class-name
                                                        superclass-ref subclassfn
                                                        &optional projectpath filepath line)
  "Create and return a new class.

CLASS-NAME should be the class name, nil or the empty string.  If nil or
the empty string, the `file-name-sans-extension' is used for the class
name.

SUPERCLASS-REF should be the string used to reference the super class.

SUBCLASSFN should be the JS function name used to create the
subclass (i.e., \"subclass\" or \"abstractSuclass\").

PROJECTPATH is a path to the JavaScript project containing all classes.
PROJECTPATH defaults to the result of function `klassified-core-project-path'.

FILEPATH is a path to a JavaScript file relative to PROJECTPATH.  FILEPATH
defaults to result of function `buffer-file-name' interpreted relatively
to PROJECTPATH.

LINE is the line number at which MATCH-DATA started matching.  LINE
defaults to result of function `klassified--current-line'."
  (when-let ((projectpath (or projectpath (klassified-core-project-path)))
             (filepath (or filepath (buffer-file-name)))
             (filepath (file-relative-name filepath projectpath))
             (class-name (or (and class-name
                                  (not (string-empty-p class-name))
                                  class-name)
                             (file-name-sans-extension (file-name-nondirectory filepath)))))
    (klassified-core--class-make
     :name class-name
     :superclass-ref (klassified-core--classref-make :name superclass-ref)
     :definition (klassified-core-position-make projectpath filepath line)
     :abstractp (equal "abstractSubclass" subclassfn))))

(defun klassified-core-get-superclass (class classes)
  "Return superclass of CLASS within CLASSES.

If class if a class-stub, return nil.  If the superclass is not in CLASSES,
return a class-stub."
  (unless (klassified-core-class-stub-p class)
    (let* ((superclass-name (klassified-core-class-superclass-name class))
           (superclass (map-elt classes superclass-name nil)))
      (when (and superclass-name (not superclass))
        (setq superclass (klassified-core-class-stub-make superclass-name))
        (map-put classes superclass-name superclass))
      superclass)))


;;; Method

(cl-defstruct (klassified-core-method
               (:constructor klassified-core--method-make)
               (:conc-name klassified-core--method-))
  name
  definition
  class)

(defun klassified-core-method-name (method)
  "Return name of METHOD."
  (klassified-core--method-name method))

(defun klassified-core-method-class (method)
  "Return class defining METHOD."
  (klassified-core--method-class method))

(defun klassified-core-method-classname (method)
  "Return name of class defining METHOD."
  (let ((class (klassified-core-method-class method)))
    (klassified-core-class-name class)))

(defun klassified-core-method-implemented-p (method)
  "Return non-nil if METHOD is defined in its class.

The purpose of this is to distinguish between proper method definitions and
ghost methods that are part of a hierarchy defining METHOD but whose class
does not."
  ;; the method-name is nil in ghost methods
  (klassified-core-method-name method))

(defvar klassified-core--method-regexp
  (rx
   (group-n 1
            (or "my" "that")
            "."
            (+ (any word "_")))
   (* space)
   "=")
  "Regexp matching the start of a method definition.

When matching, this regexp puts the method name inside group 1.")

(defun klassified-core-move-to-method-in-current-buffer (method-name)
  "Move point to METHOD-NAME after current position.

Return point if METHOD-NAME is found, nil if not."
  (let (matched)
    (while
        (and
         (setq matched (re-search-forward klassified-core--method-regexp nil t))
         (not (string= (match-string 1) method-name))))
    (if (and matched (string= (match-string 1) method-name))
        (point)
      nil)))

(defun klassified-core-move-to-method (method)
  "Return a buffer defining METHOD and move point there."
  (with-current-buffer (klassified-core-move-to-class (klassified-core-method-class method))
    (if (klassified-core-method-implemented-p method)
        (klassified-core-move-to-method-in-current-buffer (klassified-core-method-name method))
      (message "This class does not define %s" (klassified-core-method-name method)))
    (current-buffer)))

(defun klassified-core-method-make-from-match-data (match-data class)
  "Create a new methood based on MATCH-DATA inside CLASS.

This calls `klassified-core--position-make' with strings
extracted from MATCH-DATA.

MATCH-DATA at 1 should be the method name.

MATCH-DATA at 2 should be the class name, nil or the empty string.

MATCH-DATA at 4 should be the string used to reference the super class.

MATCH-DATA at 5 should be the JS function name used to create the
subclass (i.e., \"subclass\" or \"abstractSuclass\")."
  (save-match-data
    (set-match-data match-data)
    (klassified-core--method-make
     :name (match-string-no-properties 1)
     :definition (klassified-core-position-make)
     :class class)))


;;; Project

(defun klassified-core-buffer-project-path (&optional buffer)
  "Return path of project containing BUFFER.

BUFFER defaults to the current buffer.

The project's path is the buffer's directory or a parent.  The project's
path is detected by searching typical JS project files (e.g., gulp.js)."
  (with-current-buffer (or buffer (current-buffer))
    (klassified-core-project-path default-directory)))

(defun klassified-core-project-path (&optional directory)
  "Return path of project containing DIRECTORY.

DIRECTORY defaults fo `default-directory'.

The project's path is DIRECTORY or a parent.  The project's path
is detected by searching typical JS project files (e.g.,
gulp.js)."
  (let ((directory (or directory default-directory)))
    (directory-file-name
     (if-let ((project-path (locate-dominating-file directory "gulpfile.js")))
         (expand-file-name project-path)
       (expand-file-name directory)))))

(defun klassified-core--current-line (&optional pos)
  "Return (widen) line number at position POS.
If POS is nil, use current buffer location."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos pos))))

(defun klassified-core-position-make (&optional projectpath filepath line)
  "Return a new position.

PROJECTPATH is a path to the JavaScript project containing all classes.
PROJECTPATH defaults to the result of function `klassified-project-path'.

FILEPATH is a path to a JavaScript file relative to PROJECTPATH.  FILEPATH
defaults to result of function `buffer-file-name' interpreted relatively
to PROJECTPATH.

LINE is the line number at which MATCH-DATA started matching.  LINE
defaults to result of function `klassified--current-line'."
  (when-let ((projectpath (or projectpath (klassified-core-project-path)))
             (filepath (or filepath (and
                                     (buffer-file-name)
                                     (file-relative-name (buffer-file-name) projectpath))))
             (line (or line (klassified-core--current-line))))
    (klassified-core--position-make
     :file filepath
     :line line
     :project projectpath)))

(provide 'klassified-core)
;;; klassified-core.el ends here
