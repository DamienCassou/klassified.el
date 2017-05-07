;;; klassified-core-test.el --- Tests for klassified-core  -*- lexical-binding: t; -*-

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

(require 'buttercup)

(describe "klassified-core"
  (describe "can create a class and"
    (it "extract its name"
      (let* ((class (klassified-core--class-make :name "animal"))
             (result (klassified-core-class-name class)))
        (expect result :to-equal "animal")))

    (it "tell if it is abstract"
      (let ((abstract (klassified-core--class-make :abstractp t))
            (non-abstract (klassified-core--class-make)))
        (expect (klassified-core-class-abstract-p abstract) :to-be-truthy)
        (expect (klassified-core-class-abstract-p non-abstract) :not :to-be-truthy)))

    (it "tell it is not stub"
      (let ((non-stub (klassified-core--class-make)))
        (expect (klassified-core-class-stub-p non-stub) :not :to-be-truthy)))

    (it "extract its filepath"
      (let* ((position (klassified-core--position-make :file "animal.js"))
             (class (klassified-core--class-make :definition position))
             (result (klassified-core-class-filepath class)))
        (expect result :to-equal "animal.js")))

    (it "extract its projectpath"
      (let* ((position (klassified-core--position-make :project "/tmp"))
             (class (klassified-core--class-make :definition position))
             (result (klassified-core-class-projectpath class)))
        (expect result :to-equal "/tmp")))

    (it "extract its full filepath"
      (let* ((position (klassified-core--position-make :file "animal.js" :project "/tmp"))
             (class (klassified-core--class-make :definition position))
             (result (klassified-core-class-full-filepath class)))
        (expect result :to-equal "/tmp/animal.js")))

    (it "extract its definition line"
      (let* ((position (klassified-core--position-make :line 42))
             (class (klassified-core--class-make :definition position))
             (result (klassified-core-class-line class)))
        (expect result :to-be 42)))

    (it "extract its superclass name"
      (let* ((superclass-ref (klassified-core--classref-make :name "animal"))
             (class (klassified-core--class-make :superclass-ref superclass-ref))
             (result (klassified-core-class-superclass-name class)))
        (expect result :to-equal "animal"))))

  (describe "can create a stub class"
    (it "and tell it is a stub"
      (let ((class (klassified-core-class-stub-make "animal")))
        (expect (klassified-core-class-stub-p class) :to-be-truthy)))

    (it "and extract its name"
      (let ((class (klassified-core-class-stub-make "animal")))
        (expect (klassified-core-class-name class) :to-equal "animal"))))

  (describe "can match a class definition"
    (it "defined with `return'"
      (expect "return animal.subclass("
              :to-match klassified-core--class-regexp))

    (it "defined with `let'"
      (expect "let cow = animal.subclass("
              :to-match klassified-core--class-regexp))

    (it "defined with `var'"
      (expect "var cow = animal.subclass("
              :to-match klassified-core--class-regexp))

    (it "indented with a few spaces"
      (expect "  var cow = animal.subclass("
              :to-match klassified-core--class-regexp))

    (it "indented with a few tabulations"
      (expect "		var cow = animal.subclass("
              :to-match klassified-core--class-regexp))

    (it "whose superclass reference contains no dot"
      (expect "var cow = animal.subclass("
              :to-match klassified-core--class-regexp))

    (it "whose superclass reference contains dots"
      (expect "var animal = klassified-core.object.subclass("
              :to-match klassified-core--class-regexp)))

  (describe "can extract the class name from a class"
    (it "defined with `let'"
      (let ((string "let cow = animal.subclass("))
        (string-match klassified-core--class-regexp string)
        (expect (match-string 2 string) :to-equal "cow")))

    (it "defined with `var'"
      (let ((string "var cow = animal.subclass("))
        (string-match klassified-core--class-regexp string)
        (expect (match-string 2 string) :to-equal "cow"))))

  (describe "can extract the superclass function used"
    (it "when it is subclass()"
      (let ((string "var cow = animal.subclass("))
        (string-match klassified-core--class-regexp string)
        (expect (match-string 5 string) :to-equal "subclass")))

    (it "when it is abstractSubclass()"
      (let ((string "var bird = animal.abstractSubclass("))
        (string-match klassified-core--class-regexp string)
        (expect (match-string 5 string) :to-equal "abstractSubclass"))))

  (describe "does not match a class"
    (it "with a `subclass' function call"
      (expect "var a = subclass(" :not :to-match klassified-core--class-regexp)))

  (describe "can match a method definition"
    (it "starting with my"
      (expect "my.foo = function(" :to-match klassified-core--method-regexp))

    (it "starting with that"
      (expect "that.foo = function(" :to-match klassified-core--method-regexp)))

  (describe "does not match a method"
    (it "starting with let"
      (expect "let foo = function(" :not :to-match klassified-core--method-regexp))

    (it "starting with var"
      (expect "var foo = function(" :not :to-match klassified-core--method-regexp))

    (it "with a function regexp"
      (expect "function foo(" :not :to-match klassified-core--method-regexp))

    (it "call"
      (expect "my.foo(" :not :to-match klassified-core--method-regexp)))

  (describe "extracts method name from method definition"
    (it "starting with my"
      (let ((string "my.foo = function("))
        (string-match klassified-core--method-regexp string)
        (expect (match-string 1 string) :to-equal "my.foo")))

    (it "starting with that"
      (let ((string "that.foo = function("))
        (string-match klassified-core--method-regexp string)
        (expect (match-string 1 string) :to-equal "that.foo")))

    (it "with camel-case"
      (let ((string "that.fooBar = function("))
        (string-match klassified-core--method-regexp string)
        (expect (match-string 1 string) :to-equal "that.fooBar")))

    (it "with underscore"
      (let ((string "that.foo_bar = function("))
        (string-match klassified-core--method-regexp string)
        (expect (match-string 1 string) :to-equal "that.foo_bar")))))


(provide 'klassified-core-test)
;;; klassified-core-test.el ends here
