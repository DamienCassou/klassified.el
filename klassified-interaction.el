;;; klassified-interaction.el --- User-level functions and keymaps  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/DamienCassou/klassified
;; Package-requires: ((emacs "25.1"))
;; Version: 0.1.0

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

;; This file provides the entry points of klassified.

;;; Code:

(require 'hierarchy)
(require 'klassified-core)
(require 'klassified-hierarchy)

(defun klassified-interaction-goto-class (class &optional _indent)
  "Make file defining CLASS visible and move point there."
  (pop-to-buffer
   (klassified-core-move-to-class class)))

(defun klassified-interaction-goto-method (method &optional _indent)
  "Make file defining METHOD visible and move point there."
  (pop-to-buffer
   (klassified-core-move-to-method method)))

(defun klassified-interaction--labelfn-class (class indent)
  "Render CLASS prefixed with INDENT."
  (funcall
   (hierarchy-labelfn-indent
    (hierarchy-labelfn-button-if
     (lambda (class _) (insert (klassified-core-class-name class)))
     (lambda (class _) (not (klassified-core-class-stub-p class)))
     #'klassified-interaction-goto-class))
   class
   indent))

(defun klassified-interaction--labelfn-method (method indent)
  "Render METHOD prefixed with INDENT."
  (funcall
   (hierarchy-labelfn-indent
    (lambda (item indent)
      (funcall (hierarchy-labelfn-button-if
                (lambda (method _) (insert (klassified-core-method-classname method)))
                (lambda (method _) (not (klassified-core-class-stub-p (klassified-core-method-class method))))
                #'klassified-interaction-goto-method)
               item indent)
      (when (klassified-core-method-implemented-p method)
        (insert " "
                (propertize (klassified-core-method-name method)
                            'font-lock-face 'font-lock-comment-face)))))
   method
   indent))

(defun klassified-interaction--show-hierarchy-tabulated (hierarchy labelfn &optional buffer)
  "Show HIERARCHY with LABELFN in a tabulated list in BUFFER.

Display each element with LABELFN.

BUFFER defaults to a buffer named \"klassified-hierarchy\".

Returns buffer."
  (pop-to-buffer
   (hierarchy-tabulated-display
    hierarchy
    labelfn
    (or buffer (get-buffer-create "klassified-hierarchy")))))

(defun klassified-interaction--show-method-hierarchy-tabulated (hierarchy &optional buffer)
  "Show HIERARCHY in a tabulated list in BUFFER.

Returns buffer."
  (klassified-interaction--show-hierarchy-tabulated hierarchy #'klassified-interaction--labelfn-method buffer))

(defun klassified-interaction--show-class-hierarchy-tabulated (hierarchy &optional buffer)
  "Show HIERARCHY in a tabulated list in BUFFER.

Returns buffer."
  (klassified-interaction--show-hierarchy-tabulated hierarchy #'klassified-interaction--labelfn-class buffer))

;;;###autoload
(defun klassified-interaction-show-hierarchy-project (&optional directory)
  "Show hierarchy of all classes under DIRECTORY.

DIRECTORY default to `klassified-project-path'."
  (interactive)
  (klassified-interaction--show-class-hierarchy-tabulated (klassified-hierarchy-make-from-project directory)))

;;;###autoload
(defun klassified-interaction-show-class-hierarchy-at-point (&optional js-buffer)
  "Show hierarchy of class at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (interactive)
  (cl-destructuring-bind (class-hierarchy class) (klassified-hierarchy-from-class-at-point js-buffer)
    (klassified-interaction--show-class-hierarchy-tabulated class-hierarchy)
    (klassified-hierarchy-move-point-to-class (klassified-core-class-name class))))

;;;###autoload
(defun klassified-interaction-show-method-hierarchy-at-point (&optional js-buffer)
  "Show hierarchy of method at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (interactive)
  (cl-destructuring-bind (method-hierarchy _method class)
      (klassified-hierarchy-from-method-at-point js-buffer)
    (klassified-interaction--show-method-hierarchy-tabulated method-hierarchy)
    (klassified-hierarchy-move-point-to-class (klassified-core-class-name class))))

(defvar klassified-interaction-js-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-h C-p") #'klassified-interaction-show-hierarchy-project)
    (define-key map (kbd "C-c C-h C-c") #'klassified-interaction-show-class-hierarchy-at-point)
    (define-key map (kbd "C-c C-h C-m") #'klassified-interaction-show-method-hierarchy-at-point)
    map)
  "Keymap for function `klassified-interaction-js-mode'.")

;;;###autoload
(define-minor-mode klassified-interaction-js-mode
  "Minor mode to interact with klassified from JavaScript files.

\\{klassified-interaction-js-mode-map}"
  :lighter " klassified"
  :keymap klassified-interaction-js-mode-map)

(provide 'klassified-interaction)
;;; klassified-interaction.el ends here
