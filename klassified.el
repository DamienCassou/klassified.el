;;; klassified.el --- Ease development of klassified-based projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; GIT: https://github.com/DamienCassou/klassified

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package facilitates the development of projects using klassified (a
;; class-based object model for JavaScript).

;;; Code:

(require 'subr-x)
(require 'xref)

(eval-when-compile
  (require 'rx))

(require 'hierarchy)

(cl-defstruct (klassified-position
               (:constructor klassified--position-make)
               (:conc-name klassified--position-))
  file
  line
  project)

(cl-defstruct (klassified-classref
               (:constructor klassified--classref-make)
               (:conc-name klassified--classref-))
  name)


;;; Class

(cl-defstruct (klassified-class
               (:constructor klassified--class-make)
               (:conc-name klassified--class-))
  name
  superclass-ref
  definition
  stubp
  abstractp)

(defun klassified-class-name (class)
  "Return name of CLASS."
  (klassified--class-name class))

(defun klassified-class-abstract-p (class)
  "Return t if and only if CLASS is abstract."
  (klassified--class-abstractp class))

(defun klassified-class-stub-p (class)
  "Return t iff CLASS is a class-stub."
  (klassified--class-stubp class))

(defun klassified-class-filepath (class)
  "Return the file path defining CLASS relative to project."
  (if-let ((definition (klassified--class-definition class)))
      (klassified--position-file definition)))

(defun klassified-class-projectpath (class)
  "Return the project path defining CLASS."
  (if-let ((definition (klassified--class-definition class)))
      (klassified--position-project definition)))

(defun klassified-class-full-filepath (class)
  "Return absolute file path defining CLASS."
  (expand-file-name (klassified-class-filepath class)
                    (klassified-class-projectpath class)))

(defun klassified-class-line (class)
  "Return the line number defining CLASS."
  (if-let ((definition (klassified--class-definition class)))
      (klassified--position-line definition)))

(defun klassified-class-superclass-name (class)
  "Return the name of CLASS' superclass."
  (if-let ((superclass-ref (klassified--class-superclass-ref class)))
      (klassified--classref-name superclass-ref)))

(defun klassified--class-stub-make (name)
  "Return a class-stub named NAME.

This is useful when a class has a superclass that is part of another
project.  In this class, the superclass can be a class-stub."
  (klassified--class-make :name name :stubp t))

(defun klassified-goto-class (class)
  "Open file defining CLASS and move point there."
  (find-file (klassified-class-full-filepath class))
  (widen)
  (goto-char (point-min))
  (forward-line (1- (klassified-class-line class))))


;;; Methods

(cl-defstruct (klassified-method
               (:constructor klassified--method-make)
               (:conc-name klassified--method-))
  name
  definition
  class)

(defun klassified-method-name (method)
  "Return name of METHOD."
  (klassified--method-name method))

(defun klassified-method-class (method)
  "Return class defining METHOD."
  (klassified--method-class method))

(defun klassified-method-classname (method)
  "Return name of class defining METHOD."
  (let ((class (klassified-method-class method)))
    (klassified-class-name class)))

(defun klassified-method-implemented-p (method)
  "Return non-nil if METHOD is defined in its class.

The purpose of this is to distinguish between proper method definitions and
ghost methods that are part of a hierarchy defining METHOD but whose class
does not."
  ;; the method-name is nil in ghost methods
  (klassified-method-name method))

(defvar klassified-method-definition
  (rx
   (group-n 1
            (or "my" "that")
            "."
            (+ (any word "_")))
   (* space)
   "=")
  "Regexp matching the start of a method definition.

When matching, this regexp puts the method name inside group 1.")

(defun klassified-method-at-point (&optional js-buffer)
  "Return method at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (with-current-buffer (or js-buffer (current-buffer))
    (save-excursion
      (let* ((class (klassified--class-at-point)))
        (back-to-indentation)
        (when (looking-at klassified-method-definition)
          (klassified--method-make
           :name (match-string-no-properties 1)
           :definition (klassified-position-make)
           :class class))))))

(defun klassified--method-hierarchy-at-point (&optional js-buffer)
  "Build hierarchy of method around point in JS-BUFFER.

Return (hierarchy method class).

JS-BUFFER defaults to current buffer."
  (cl-destructuring-bind (class-hierarchy class) (klassified--class-hierarchy-at-point js-buffer)
    (let* ((method (klassified-method-at-point js-buffer))
           (method-hierarchy (klassified-class-hierarchy-to-method-hierarchy
                              class-hierarchy
                              (klassified-method-name method))))
      (list method-hierarchy method class))))

(defun klassified-move-to-method (method-name)
  "Move point to METHOD-NAME after current position.

Return point if METHOD-NAME is found, nil if not."
  (let (matched)
    (while
        (and
         (setq matched (re-search-forward klassified-method-definition nil t))
         (not (string= (match-string 1) method-name))))
    (if (and matched (string= (match-string 1) method-name))
        (point)
      nil)))

(defun klassified-class-to-method (method-name class)
  "Return the method named METHOD-NAME defined in CLASS if any."
  (let ((ghost-method (klassified--method-make
                       :name nil
                       :definition nil
                       :class class)))
    (if (klassified-class-stub-p class)
        ghost-method
      (save-excursion
        (save-restriction
          (klassified-goto-class class)
          (if (klassified-move-to-method method-name)
              (klassified--method-make
               :name method-name
               :definition (klassified-position-make)
               :class class)
            ghost-method))))))

(defun klassified-class-hierarchy-to-method-hierarchy (class-hierarchy method-name)
  "Convert CLASS-HIERARCHY to a method hierarchy for METHOD-NAME."
  (hierarchy-map-hierarchy (lambda (class _)
                             (klassified-class-to-method method-name class))
                           class-hierarchy))

(defun klassified-goto-method (method)
  "Open file defining METHOD and move point there."
  (klassified-goto-class (klassified-method-class method))
  (if (klassified-method-implemented-p method)
      (klassified-move-to-method (klassified--method-name method))
    (message "Method not implemented in this class")))


;;; Search

(defvar klassified--search-class-definition
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

(defvar klassified--search-ag-class-definition
  (concat
   (rx
    line-start
    ;; file path (1)
    (group-n 1
             (optional (+? anything) "/")
             ;; filename without extension (2)
             (group-n 2 (+? (not (any "/"))))
             ".js")
    ":"
    ;; line number (3)
    (group-n 3 (any "1-9") (* digit))
    ":"
    (* blank))
   klassified--search-class-definition)
  "A regexp matching an ag-match of a class definition in a search result.

This is the same as `klassified--search-class-definition' except this also
matches file and line number of the ag search result.

When matching, this regexp puts the file path inside group 4, the file name
without extension inside group 2 and the line number inside group 3.  If
`klassified--search-class-definition' matches a class name, this regex puts
that in group 2 instead of the file name.")

(defun klassified--search-class-at-point (&optional search-buffer)
  "Read the class definition at point in SEARCH-BUFFER.

Return a `klassified-class' or nil if no class if found at point.  The
point must be at the beginning of a class definition.

SEARCH-BUFFER must be a search result buffer as produced by
`klassified--search-run-ag'.  If SEARCH-BUFFER is nil, the current buffer is
used instead."
  (with-current-buffer (or search-buffer (current-buffer))
    (when (looking-at klassified--search-ag-class-definition)
      (klassified-class-make-from-match-data
       (match-data)
       (directory-file-name default-directory)
       (match-string 1)
       (string-to-number (match-string 3))))))

(defun klassified--search-regexp-to-pcre (regexp)
  "Convert REGEXP to pcre form."
  (replace-regexp-in-string "(\\?[0-9]*:" "("
                            (xref--regexp-to-extended regexp)))

(defun klassified--search-run-ag (directory)
  "Return buffer containing result of running ag inside DIRECTORY."
  (let ((buffer (get-buffer-create (concat "klassified--" directory))))
    (with-current-buffer buffer
      (erase-buffer)
      (cd directory)
      (let ((status (call-process
                     "/usr/bin/ag"
                     nil            ; stdin
                     '(t t)         ; stdout ⇒ current buffer
                     t              ; redisplay
                     "--js" "--nocolor" "--nogroup" "--nomultiline"
                     "--numbers" "--nopager" "--case-sensitive"
                     "--silent" "--width=200"
                     "--context=0"
                     ;; if speed is an issue, we could replace this regexp by a
                     ;; much faster one (e.g., "\.(abstractS|s)ubclass\(")
                     (klassified--search-regexp-to-pcre klassified--search-class-definition)
                     ".")))
        (unless (equal status 0)
          (error "Klassified: can't run ag (return status is '%s')" status))))
    buffer))

(defun klassified--search-collect-next-class ()
  "Return next class in current buffer if any.

Move point after match.

Return the class, nil if none."
  (when (re-search-forward klassified--search-ag-class-definition nil t)
    (beginning-of-line)
    (let ((class (klassified--search-class-at-point)))
      (forward-line)
      class)))

(defun klassified--search-collect-classes (&optional buffer)
  "Return a map of all classes in BUFFER.

If BUFFER is nil, use `current-buffer' instead."
  (let ((classes (make-hash-table :test 'equal)))
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (while (when-let ((class (klassified--search-collect-next-class)))
               (puthash (klassified-class-name class) class classes))))
    classes))



(defun klassified--class-at-point (&optional js-buffer)
  "Return class at point in JS-BUFFER.

Return a `klassified-class' or nil if no class if found at point.  The
point must be on a line of a class definition or after one such line.

JS-BUFFER must be a JS file buffer.  If JS-BUFFER is nil, the current
buffer is used instead."
  (with-current-buffer (or js-buffer (current-buffer))
    (save-excursion
      (back-to-indentation)
      (if (looking-at klassified--search-class-definition)
          (klassified-class-make-from-match-data (match-data))
        (when (re-search-backward klassified--search-class-definition nil t)
          (klassified-class-make-from-match-data (match-data)))))))

(defun klassified--current-line (&optional pos)
  "Return (widen) line number at position POS.
If POS is nil, use current buffer location."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos pos))))

(defun klassified-project-path (&optional buffer)
  "Return path of project containing BUFFER.

BUFFER defaults to the current buffer.

The project's path is the buffer's directory or a parent.  The project's
path is detected by searching typical JS project files (e.g., gulp.js)."
  (with-current-buffer (or buffer (current-buffer))
    (directory-file-name
     (if-let ((directory (locate-dominating-file default-directory "gulpfile.js")))
         (expand-file-name directory)
       default-directory))))

(defun klassified-class-make-from-match-data (match-data &optional projectpath filepath line)
  "Create a new class based on MATCH-DATA.

This calls `klassified-class-make-from-extracted-data' with strings
extracted from MATCH-DATA and PROJECTPATH, FILEPATH and LINE unchanged.

MATCH-DATA at 2 should be the class name, nil or the empty string.

MATCH-DATA at 4 should be the string used to reference the super class.

MATCH-DATA at 5 should be the JS function name used to create the
subclass (i.e., \"subclass\" or \"abstractSuclass\")."
  (save-match-data
    (set-match-data match-data)
    (klassified-class-make-from-extracted-data
     (match-string-no-properties 2) ; optional class name
     (match-string-no-properties 4) ; superclass reference
     (match-string-no-properties 5) ; subclass message send
     projectpath
     filepath
     line)))

(defun klassified-class-make-from-extracted-data (class-name
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
PROJECTPATH defaults to the result of function `klassified-project-path'.

FILEPATH is a path to a JavaScript file relative to PROJECTPATH.  FILEPATH
defaults to result of function `buffer-file-name' interpreted relatively
to PROJECTPATH.

LINE is the line number at which MATCH-DATA started matching.  LINE
defaults to result of function `klassified--current-line'."
  (when-let ((projectpath (or projectpath (klassified-project-path)))
             (filepath (or filepath (buffer-file-name)))
             (filepath (file-relative-name filepath projectpath))
             (class-name (or (and class-name
                                  (not (string-empty-p class-name))
                                  class-name)
                             (file-name-sans-extension (file-name-nondirectory filepath)))))
    (klassified--class-make
     :name class-name
     :superclass-ref (klassified--classref-make :name superclass-ref)
     :definition (klassified-position-make projectpath filepath line)
     :abstractp (equal "abstractSubclass" subclassfn))))

(defun klassified-position-make (&optional projectpath filepath line)
  "Return a new position.

PROJECTPATH is a path to the JavaScript project containing all classes.
PROJECTPATH defaults to the result of function `klassified-project-path'.

FILEPATH is a path to a JavaScript file relative to PROJECTPATH.  FILEPATH
defaults to result of function `buffer-file-name' interpreted relatively
to PROJECTPATH.

LINE is the line number at which MATCH-DATA started matching.  LINE
defaults to result of function `klassified--current-line'."
  (when-let ((projectpath (or projectpath (klassified-project-path)))
             (filepath (or filepath (and
                                     (buffer-file-name)
                                     (file-relative-name (buffer-file-name) projectpath))))
             (line (or line (klassified--current-line))))
    (klassified--position-make
     :file filepath
     :line line
     :project projectpath)))

(defun klassified-get-superclass (class classes)
  "Return superclass of CLASS within CLASSES.

If class if a class-stub, return nil.  If the superclass is not in CLASSES,
return a class-stub."
  (unless (klassified-class-stub-p class)
    (let* ((superclass-name (klassified-class-superclass-name class))
           (superclass (map-elt classes superclass-name nil)))
      (when (and superclass-name (not superclass))
        (setq superclass (klassified--class-stub-make superclass-name))
        (map-put classes superclass-name superclass))
      superclass)))

(defun klassified--classes-to-class-hierarchy (classes)
  "Create a class hierarchy for the map of CLASSES.

CLASSES maps a class name to a class."
  (let ((hierarchy (hierarchy-new))
        (parentfn (lambda (class) (klassified-get-superclass class classes))))
    (hierarchy-add-trees hierarchy (map-values classes) parentfn)
    (hierarchy-sort hierarchy (lambda (class1 class2)
                                (string< (klassified-class-name class1)
                                         (klassified-class-name class2))))
    hierarchy))

(defun klassified--class-hierarchy-at-point (&optional js-buffer)
  "Build hierarchy of class around point in JS-BUFFER.

Return (hierarchy class)"
  (with-current-buffer (or js-buffer (current-buffer))
    (let* ((project-hierarchy (klassified--make-project-hierarchy))
           (class (klassified--class-at-point js-buffer)))
      (list (hierarchy-extract-tree project-hierarchy class) class))))

(defun klassified--actionp-find-file (class &optional _indent)
  "Open file at point defining CLASS.

Ignore INDENT."
  (klassified-goto-class class))

(defun klassified--labelfn-class (class indent)
  "Render CLASS prefixed with INDENT."
  (funcall
   (hierarchy-labelfn-indent
    (hierarchy-labelfn-button-if
     (lambda (class _) (insert (klassified-class-name class)))
     (lambda (class _) (not (klassified-class-stub-p class)))
     #'klassified--actionp-find-file))
   class
   indent))

(defun klassified--labelfn-method (method indent)
  "Render METHOD prefixed with INDENT."
  (funcall
   (hierarchy-labelfn-indent
    (lambda (item indent)
      (funcall (hierarchy-labelfn-button-if
                (lambda (method _) (insert (klassified-method-classname method)))
                (lambda (method _) (not (klassified-class-stub-p (klassified-method-class method))))
                (lambda (method _) (klassified-goto-method method)))
               item indent)
      (when (klassified-method-implemented-p method)
        (insert " "
                (propertize (klassified-method-name method)
                            'font-lock-face 'font-lock-comment-face)))))
   method
   indent))

(defun klassified--make-project-hierarchy (&optional directory)
  "Return hierarchy of all classes under DIRECTORY.

DIRECTORY default to `klassified-project-path'."
  (when-let ((directory (or directory (klassified-project-path))))
    (klassified--classes-to-class-hierarchy
     (klassified--search-collect-classes
      (klassified--search-run-ag directory)))))

(defun klassified--move-point-to-class-in-hierarchy-buffer (class &optional buffer)
  "Move point to CLASS in BUFFER.

BUFFER defaults to `current-buffer'."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (if (re-search-forward (rx-to-string
                            `(and
                              line-start
                              (+ space)
                              ,(klassified-class-name class)
                              (any space)))
                           nil t)
        (back-to-indentation)
      (goto-char (point-min)))))


;;; Viewing

(defun klassified--show-class-hierarchy-tabulated (hierarchy &optional buffer)
  "Show HIERARCHY in a tabulated list in BUFFER.

BUFFER defaults to a buffer named \"klassified-hierarchy\".

Returns buffer."
  (switch-to-buffer
   (hierarchy-tabulated-display
    hierarchy
    #'klassified--labelfn-class
    (or buffer
        (get-buffer-create "klassified-hierarchy")))))

(defun klassified--show-method-hierarchy-tabulated (hierarchy &optional buffer)
  "Show HIERARCHY in a tabulated list in BUFFER.

BUFFER defaults to a buffer named \"klassified-hierarchy\".

Returns buffer."
  (switch-to-buffer
   (hierarchy-tabulated-display
    hierarchy
    #'klassified--labelfn-method
    (or buffer
        (get-buffer-create "klassified-hierarchy")))))

;;;###autoload
(defun klassified-show-hierarchy-project (&optional directory)
  "Show hierarchy of all classes under DIRECTORY.

DIRECTORY default to `klassified-project-path'."
  (interactive)
  (klassified--show-class-hierarchy-tabulated (klassified--make-project-hierarchy directory)))

;;;###autoload
(defun klassified-show-class-hierarchy-at-point (&optional js-buffer)
  "Show hierarchy of class at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (interactive)
  (cl-destructuring-bind (class-hierarchy class) (klassified--class-hierarchy-at-point js-buffer)
    (klassified--show-class-hierarchy-tabulated class-hierarchy)
    (klassified--move-point-to-class-in-hierarchy-buffer class)))

;;;###autoload
(defun klassified-show-method-hierarchy-at-point (&optional js-buffer)
  "Show hierarchy of method at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (interactive)
  (cl-destructuring-bind (method-hierarchy _method class)
      (klassified--method-hierarchy-at-point js-buffer)
    (klassified--show-method-hierarchy-tabulated method-hierarchy)
    (klassified--move-point-to-class-in-hierarchy-buffer class)))

(defvar klassified-js-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h p") #'klassified-show-hierarchy-project)
    (define-key map (kbd "C-c h c") #'klassified-show-class-hierarchy-at-point)
    (define-key map (kbd "C-c h m") #'klassified-show-method-hierarchy-at-point)
    map)
  "Keymap for `klassified-js-mode'.")

;;;###autoload
(define-minor-mode klassified-js-mode
  "Minor mode to interact with klassified from JavaScript files.

\\{klassified-js-mode-map}"
  :lighter " Klassified"
  :keymap klassified-js-mode-map)

(provide 'klassified)

;;; klassified.el ends here

;;  LocalWords:  superclass pcre Klassified
