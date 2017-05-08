;;; klassified-jsbuffer-test.el --- Tests for klassified-jsbuffer  -*- lexical-binding: t; -*-

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

(load "test/test-helper")

(require 'klassified-jsbuffer)

(require 'buttercup)
(require 'assess)

(describe "klassified-jsbuffer"
  (describe "class-at-point"
    (it "calls class-make-from-match-data when line indented with spaces"
      (spy-on 'klassified-core-class-make-from-match-data)
      (with-temp-buffer
        (insert "  return object.subclass(")
        (klassified-jsbuffer-class-at-point))
      (expect 'klassified-core-class-make-from-match-data :to-have-been-called))

    (it "calls class-make-from-match-data when line indented with tabs"
      (spy-on 'klassified-core-class-make-from-match-data)
      (with-temp-buffer
        (insert "	return object.subclass(")
        (klassified-jsbuffer-class-at-point))
      (expect 'klassified-core-class-make-from-match-data :to-have-been-called))

    (it "calls class-make-from-match-data when line not indented"
      (spy-on 'klassified-core-class-make-from-match-data)
      (with-temp-buffer
        (insert "return object.subclass(")
        (klassified-jsbuffer-class-at-point))
      (expect 'klassified-core-class-make-from-match-data :to-have-been-called))

    (it "calls class-make-from-match-data when point on next line"
      (spy-on 'klassified-core-class-make-from-match-data)
      (with-temp-buffer
        (insert "  return object.subclass(\n")
        (klassified-jsbuffer-class-at-point))
      (expect 'klassified-core-class-make-from-match-data :to-have-been-called))))

(provide 'klassified-jsbuffer-test)
;;; klassified-jsbuffer-test.el ends here
