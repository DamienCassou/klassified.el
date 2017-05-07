;;; klassified-search.el --- Search for klassified structures within multiple files  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'rx))

(defvar klassified-search--ag-class-regexp
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
   klassified-core--class-regexp)
  "A regexp matching an ag-match of a class definition in a search result.

This is the same as `klassified-core--class-regexp' except this also
matches file and line number of the ag search result.

When matching, this regexp puts the file path inside group 4, the file name
without extension inside group 2 and the line number inside group 3.  If
`klassified-search-class-definition' matches a class name, this regex puts
that in group 2 instead of the file name.")

(defun klassified-search--class-at-point (&optional search-buffer)
  "Read the class definition at point in SEARCH-BUFFER.

Return a `klassified-class' or nil if no class if found at point.  The
point must be at the beginning of a class definition.

SEARCH-BUFFER must be a search result buffer as produced by
`klassified--search-run-ag'.  If SEARCH-BUFFER is nil, the current buffer is
used instead."
  (with-current-buffer (or search-buffer (current-buffer))
    (when (looking-at klassified-search--ag-class-regexp)
      (klassified-class-make-from-match-data
       (match-data)
       (directory-file-name default-directory)
       (match-string 1)
       (string-to-number (match-string 3))))))

(defun klassified-search--collect-next-class ()
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

(provide 'klassified-search)
;;; klassified-search.el ends here