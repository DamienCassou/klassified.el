;;; klassified-interaction-test.el --- Tests for klassified-interaction  -*- lexical-binding: t; -*-

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

(require 'klassified-interaction)
(require 'buttercup)
(require 'assess)

(describe "klassified-interaction"
  (it "returns a result included in the project hierarchy"
    (assess-with-preserved-buffer-list
     (assess-with-filesystem '(("animal.js"
                                "let animal = object.abstractSubclass((that, my) => {}\n"))
                             (assess-with-temp-buffers
                              ((ag-search
                                (insert "animal.js:1:let animal = object.abstractSubclass((that, my) => {}\n")))
                              (spy-on 'klassified-search--run-ag :and-return-value ag-search)
                              (let ((class (assess-with-find-file "animal.js"
                                                                  (klassified-jsbuffer-class-at-point)))
                                    (hierarchy (klassified-hierarchy-make-from-project "foo/")))
                                (expect (hierarchy-has-item hierarchy class) :to-be-truthy)))))))

(provide 'klassified-interaction-test)
;;; klassified-interaction-test.el ends here
