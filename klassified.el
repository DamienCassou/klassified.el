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

(defun klassified-goto-method (method)
  "Open file defining METHOD and move point there."
  (klassified-goto-class (klassified-method-class method))
  (if (klassified-method-implemented-p method)
      (klassified-move-to-method (klassified--method-name method))
    (message "Method not implemented in this class")))


;;; Search

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





(defun klassified--current-line (&optional pos)
  "Return (widen) line number at position POS.
If POS is nil, use current buffer location."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos pos))))

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

(defun klassified--move-point-to-class-in-hierarchy-buffer (class-name &optional buffer)
  "Move point to CLASS-NAME in BUFFER.

BUFFER defaults to `current-buffer'."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (if (re-search-forward (rx-to-string
                            `(and
                              line-start
                              (* space)
                              ,class-name
                              (optional (and " " (*? anything)))
                              line-end))
                           nil t)
        (back-to-indentation)
      (goto-char (point-min)))))


;;; Viewing

(defun klassified--show-class-hierarchy-tabulated (hierarchy &optional buffer)
  "Show HIERARCHY in a tabulated list in BUFFER.

BUFFER defaults to a buffer named \"klassified-hierarchy\".

Returns buffer."
  (pop-to-buffer
   (hierarchy-tabulated-display
    hierarchy
    #'klassified--labelfn-class
    (or buffer
        (get-buffer-create "klassified-hierarchy")))))

(defun klassified--show-method-hierarchy-tabulated (hierarchy &optional buffer)
  "Show HIERARCHY in a tabulated list in BUFFER.

BUFFER defaults to a buffer named \"klassified-hierarchy\".

Returns buffer."
  (pop-to-buffer
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
    (klassified--move-point-to-class-in-hierarchy-buffer (klassified-class-name class))))

;;;###autoload
(defun klassified-show-method-hierarchy-at-point (&optional js-buffer)
  "Show hierarchy of method at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (interactive)
  (cl-destructuring-bind (method-hierarchy _method class)
      (klassified--method-hierarchy-at-point js-buffer)
    (klassified--show-method-hierarchy-tabulated method-hierarchy)
    (klassified--move-point-to-class-in-hierarchy-buffer (klassified-class-name class))))

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
