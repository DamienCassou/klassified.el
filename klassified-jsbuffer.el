;;; klassified-jsbuffer.el --- Read and manipulate JavaScript buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>
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

;; This file brings klassified features to javascript buffers.

;;; Code:

(require 'klassified-core)

(defun klassified-jsbuffer-class-at-point (&optional js-buffer)
  "Return class at point in JS-BUFFER.

Return a `klassified-class' or nil if no class if found at point.  The
point must be on a line of a class definition or after one such line.

JS-BUFFER must be a JS file buffer.  If JS-BUFFER is nil, the current
buffer is used instead."
  (with-current-buffer (or js-buffer (current-buffer))
    (save-excursion
      (back-to-indentation)
      (when (or (looking-at klassified-core-class-regexp)
                (re-search-backward klassified-core-class-regexp nil t)
                (re-search-forward klassified-core-class-regexp nil t))
        (klassified-core-class-make-from-match-data (match-data))))))

(defun klassified-jsbuffer-method-at-point (&optional js-buffer)
  "Return method at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (with-current-buffer (or js-buffer (current-buffer))
    (save-excursion
      (let* ((class (klassified-jsbuffer-class-at-point)))
        (back-to-indentation)
        (if (looking-at klassified-core--method-regexp)
            (klassified-core-method-make-from-match-data (match-data) class)
          (when (re-search-backward klassified-core--method-regexp)
            (klassified-core-method-make-from-match-data (match-data) class)))))))

(defun klassified-jsbuffer-class-to-method (method-name class)
  "Return the method named METHOD-NAME defined in CLASS if any."
  (let ((ghost-method (klassified-core--method-make
                       :name nil
                       :definition nil
                       :class class)))
    (if (klassified-core-class-stub-p class)
        ghost-method
      (with-current-buffer (klassified-core-move-to-class class)
        (if (klassified-core-move-to-method-in-current-buffer method-name)
            (klassified-core--method-make
             :name method-name
             :definition (klassified-core-position-make)
             :class class)
          ghost-method)))))

(provide 'klassified-jsbuffer)
;;; klassified-jsbuffer.el ends here
