;;; klassified-jsbuffer.el --- Read and manipulate JavaScript buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

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

(defun klassified-jsbuffer-class-at-point (&optional js-buffer)
  "Return class at point in JS-BUFFER.

Return a `klassified-class' or nil if no class if found at point.  The
point must be on a line of a class definition or after one such line.

JS-BUFFER must be a JS file buffer.  If JS-BUFFER is nil, the current
buffer is used instead."
  (with-current-buffer (or js-buffer (current-buffer))
    (save-excursion
      (back-to-indentation)
      (if (looking-at klassified-core--class-regexp)
          (klassified-core--class-make-from-match-data (match-data))
        (when (re-search-backward klassified-core--class-regexp nil t)
          (klassified-core--class-make-from-match-data (match-data)))))))

(defun klassified-jsbuffer-method-at-point (&optional js-buffer)
  "Return method at point in JS-BUFFER.

JS-BUFFER defaults to current buffer."
  (with-current-buffer (or js-buffer (current-buffer))
    (save-excursion
      (let* ((class (klassified-jsbuffer-class-at-point)))
        (back-to-indentation)
        (when (looking-at klassified-core--method-regexp)
          (klassified-core--method-make
           :name (match-string-no-properties 1)
           :definition (klassified-core--position-make)
           :class class))))))

(defun klassified-jsbuffer-move-to-method (method-name)
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

(defun klassified-jsbuffer-class-to-method (method-name class)
  "Return the method named METHOD-NAME defined in CLASS if any."
  (let ((ghost-method (klassified-core--method-make
                       :name nil
                       :definition nil
                       :class class)))
    (if (klassified-core-class-stub-p class)
        ghost-method
      (save-excursion
        (save-restriction
          (klassified-core-goto-class class)
          (if (klassified-jsbuffer-move-to-method method-name)
              (klassified-core--method-make
               :name method-name
               :definition (klassified-core--position-make)
               :class class)
            ghost-method))))))

(provide 'klassified-jsbuffer)
;;; klassified-jsbuffer.el ends here