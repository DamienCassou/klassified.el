;;; klassified-hierarchy-test.el --- Tests for klassified-hierarchy  -*- lexical-binding: t; -*-

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

(require 'klassified-hierarchy)

(require 'buttercup)
(require 'assess)

(describe "klassified-hierarchy"
  (describe "move-point-to-class"
    (it "can move to non-indented class"
      (assess-with-preserved-buffer-list
       (with-temp-buffer
         (insert "animal\nbird")
         (klassified-hierarchy-move-point-to-class "bird")
         (expect (looking-at-p "bird$")))))

    (it "can move to indented class"
      (assess-with-preserved-buffer-list
       (with-temp-buffer
         (insert "animal\n  bird")
         (klassified-hierarchy-move-point-to-class "bird")
         (expect (looking-at-p "bird$")))))

    (it "moves point when buffer contains classes"
      (assess-with-preserved-buffer-list
       (with-temp-buffer
         (insert "animal\n  bird\n    dove\n    pigeon\n")
         (klassified-hierarchy-move-point-to-class "dove")
         (expect (looking-at-p "dove")))))

    (it "moves point when buffer contains methods"
      (assess-with-preserved-buffer-list
       (with-temp-buffer
         (insert "animal that.alert\n  bird\n    dove that.alert\n    pigeon that.alert\n")
         (klassified-hierarchy-move-point-to-class "dove")
         (expect (looking-at-p "dove")))))

    (it "does not stop at the first prefix"
      (assess-with-preserved-buffer-list
       (with-temp-buffer
         (insert "foo12\nfoo\n")
         (klassified-hierarchy-move-point-to-class "foo")
         (expect (looking-at-p "foo$")))))))

(provide 'klassified-hierarchy-test)
;;; klassified-hierarchy-test.el ends here
