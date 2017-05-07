;;; klassified-hierarchy.el --- Create hierarchies for klassified-core structures  -*- lexical-binding: t; -*-

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

(require 'klassified-core)
(require 'klassified-jsbuffer)
(require 'hierarchy)

(defun klassified-class-hierarchy-to-method-hierarchy (class-hierarchy method-name)
  "Convert CLASS-HIERARCHY to a method hierarchy for METHOD-NAME."
  (hierarchy-map-hierarchy (lambda (class _)
                             (klassified-jsbuffer-class-to-method method-name class))
                           class-hierarchy))



(defun klassified-hierarchy-from-class-at-point (&optional js-buffer)
  "Build hierarchy of class around point in JS-BUFFER.

Return (hierarchy class)"
  (with-current-buffer (or js-buffer (current-buffer))
    (let* ((project-hierarchy (klassified--make-project-hierarchy))
           (class (klassified-jsbuffer-class-at-point js-buffer)))
      (list (hierarchy-extract-tree project-hierarchy class) class))))

(defun klassified-hierarchy-from-method-at-point (&optional js-buffer)
  "Build hierarchy of method around point in JS-BUFFER.

Return (hierarchy method class).

JS-BUFFER defaults to current buffer."
  (cl-destructuring-bind (class-hierarchy class)
      (klassified-hierarchy-from-class-at-point js-buffer)
    (let* ((method (klassified-jsbuffer-method-at-point js-buffer))
           (method-hierarchy (klassified-class-hierarchy-to-method-hierarchy
                              class-hierarchy
                              (klassified-core-method-name method))))
      (list method-hierarchy method class))))

(defun klassified-hierarchy--make-from-classes (classes)
  "Create a class hierarchy for the map of CLASSES.

CLASSES maps a class name to a class."
  (let ((hierarchy (hierarchy-new))
        (parentfn (lambda (class) (klassified-core-get-superclass class classes))))
    (hierarchy-add-trees hierarchy (map-values classes) parentfn)
    (hierarchy-sort hierarchy (lambda (class1 class2)
                                (string< (klassified-core-class-name class1)
                                         (klassified-core-class-name class2))))
    hierarchy))

(defun klassified--make-project-hierarchy (&optional directory)
  "Return hierarchy of all classes under DIRECTORY.

DIRECTORY default to `klassified-project-path'."
  (when-let ((directory (or directory (klassified-core-buffer-project-path))))
    (klassified-hierarchy--make-from-classes
     (klassified-search-collect-classes
      (klassified-search-run-ag directory)))))

(provide 'klassified-hierarchy)
;;; klassified-hierarchy.el ends here
