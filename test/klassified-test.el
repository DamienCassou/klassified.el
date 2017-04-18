;;; klassified-tests.el --- Tests for klassified.el

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

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

;; Tests for klassified.el

;;; Code:

(require 'ert)
(require 'assess)
(require 'hierarchy)
(require 'assess)
(require 'buttercup)

(declare-function undercover "undercover")

(when (require 'undercover nil t)
  (undercover "klassified.el"))

(require 'klassified)

(describe "klassified"
  (describe "can create a class and"
    (it "extract its name"
      (let* ((class (klassified--class-make :name "animal"))
             (result (klassified-class-name class)))
        (expect result :to-equal "animal")))

    (it "tell if it is abstract"
      (let ((abstract (klassified--class-make :abstractp t))
            (non-abstract (klassified--class-make)))
        (expect (klassified-class-abstract-p abstract) :to-be-truthy)
        (expect (klassified-class-abstract-p non-abstract) :not :to-be-truthy)))

    (it "tell it is not stub"
      (let ((non-stub (klassified--class-make)))
        (expect (klassified-class-stub-p non-stub) :not :to-be-truthy)))

    (it "extract its filepath"
      (let* ((position (klassified--position-make :file "animal.js"))
             (class (klassified--class-make :definition position))
             (result (klassified-class-filepath class)))
        (expect result :to-equal "animal.js")))

    (it "extract its projectpath"
      (let* ((position (klassified--position-make :project "/tmp"))
             (class (klassified--class-make :definition position))
             (result (klassified-class-projectpath class)))
        (expect result :to-equal "/tmp")))

    (it "extract its full filepath"
      (let* ((position (klassified--position-make :file "animal.js" :project "/tmp"))
             (class (klassified--class-make :definition position))
             (result (klassified-class-full-filepath class)))
        (expect result :to-equal "/tmp/animal.js")))

    (it "extract its definition line"
      (let* ((position (klassified--position-make :line 42))
             (class (klassified--class-make :definition position))
             (result (klassified-class-line class)))
        (expect result :to-be 42)))

    (it "extract its superclass name"
      (let* ((superclass-ref (klassified--classref-make :name "animal"))
             (class (klassified--class-make :superclass-ref superclass-ref))
             (result (klassified-class-superclass-name class)))
        (expect result :to-equal "animal"))))

  (describe "can create a stub class"
    (it "and tell it is a stub"
      (let ((class (klassified--class-stub-make "animal")))
        (expect (klassified-class-stub-p class) :to-be-truthy)))

    (it "and extract its name"
      (let ((class (klassified--class-stub-make "animal")))
        (expect (klassified-class-name class) :to-equal "animal"))))

  (describe "can match a class definition"
    (it "defined with `return'"
      (expect "return animal.subclass("
              :to-match klassified--search-class-definition))

    (it "defined with `let'"
      (expect "let cow = animal.subclass("
              :to-match klassified--search-class-definition))

    (it "defined with `var'"
      (expect "var cow = animal.subclass("
              :to-match klassified--search-class-definition))

    (it "indented with a few spaces"
      (expect "  var cow = animal.subclass("
              :to-match klassified--search-class-definition))

    (it "indented with a few tabulations"
      (expect "		var cow = animal.subclass("
              :to-match klassified--search-class-definition))

    (it "whose superclass reference contains no dot"
      (expect "var cow = animal.subclass("
              :to-match klassified--search-class-definition))

    (it "whose superclass reference contains dots"
      (expect "var animal = klassified.object.subclass("
              :to-match klassified--search-class-definition)))

  (describe "can extract the class name from a class"
    (it "defined with `let'"
      (let ((string "let cow = animal.subclass("))
        (string-match klassified--search-class-definition string)
        (expect (match-string 2 string) :to-equal "cow")))

    (it "defined with `var'"
      (let ((string "var cow = animal.subclass("))
        (string-match klassified--search-class-definition string)
        (expect (match-string 2 string) :to-equal "cow"))))

  (describe "can extract the superclass function used"
    (it "when it is subclass()"
      (let ((string "var cow = animal.subclass("))
        (string-match klassified--search-class-definition string)
        (should (equal (match-string 5 string) "subclass"))))

    (it "when it is abstractSubclass()"
      (let ((string "var bird = animal.abstractSubclass("))
        (string-match klassified--search-class-definition string)
        (should (equal (match-string 5 string) "abstractSubclass")))))

  (describe "does not match a class"
    (it "with a `subclass' function call"
      (expect "var a = subclass(" :not :to-match klassified--search-class-definition)))

  (describe "can match a method definition"
    (it "starting with my"
      (expect "my.foo = function(" :to-match klassified-method-definition))

    (it "starting with that"
      (expect "that.foo = function(" :to-match klassified-method-definition)))

  (describe "does not match a method"
    (it "starting with let"
      (expect "let foo = function(" :not :to-match klassified-method-definition))

    (it "starting with var"
      (expect "var foo = function(" :not :to-match klassified-method-definition))

    (it "with a function definition"
      (expect "function foo(" :not :to-match klassified-method-definition))

    (it "call"
      (expect "my.foo(" :not :to-match klassified-method-definition)))

  (describe "extracts method name from method definition"
    (it "starting with my"
      (let ((string "my.foo = function("))
        (string-match klassified-method-definition string)
        (expect (match-string 1 string) :to-equal "my.foo")))

    (it "starting with that"
      (let ((string "that.foo = function("))
        (string-match klassified-method-definition string)
        (expect (match-string 1 string) :to-equal "that.foo")))

    (it "with camel-case"
      (let ((string "that.fooBar = function("))
        (string-match klassified-method-definition string)
        (expect (match-string 1 string) :to-equal "that.fooBar")))

    (it "with underscore"
      (let ((string "that.foo_bar = function("))
        (string-match klassified-method-definition string)
        (expect (match-string 1 string) :to-equal "that.foo_bar"))))

  (describe "can analyze search results"
    (describe "and match a class definition"
      (it "with `return'"
        (expect "foo/bar.js:42: return object.subclass("
                :to-match klassified--search-ag-class-definition))

      (it "with no space after column"
        (expect "foo/bar.js:42:return object.subclass("
                :to-match klassified--search-ag-class-definition))

      (it "but not two"
        (let* ((string1 "bar.js:42:return object.subclass(")
               (string2 "\nfoo.js:99:return bar.subclass(\n")
               (string (concat string1 string2)))
          (expect (string-match klassified--search-ag-class-definition string) :to-be-truthy)
          (expect (match-end 0) :to-be (length string1)))))

    (it "and extract a filepath with directory"
      (let ((string "foo/bar.js:42: let baz = object.subclass("))
        (string-match klassified--search-ag-class-definition string)
        (expect (match-string 1 string) :to-equal "foo/bar.js")))

    (it "and extract a filepath without directory"
      (let ((string "bar.js:42: let baz = object.subclass("))
        (string-match klassified--search-ag-class-definition string)
        (expect (match-string 1 string) :to-equal "bar.js")))

    (it "and extract a class name from the filename"
      (let ((string "foo/bar.js:42: return object.subclass("))
        (string-match klassified--search-ag-class-definition string)
        (expect (match-string 2 string) :to-equal "bar")))

    (it "and will prefer variable name for classname"
      (let ((string "foo/bar.js:42: let baz = object.subclass("))
        (string-match klassified--search-ag-class-definition string)
        (expect (match-string 2 string) :to-equal "baz")))

    (it "and extract a line number"
      (let ((string "foo/bar.js:42: let baz = object.subclass("))
        (string-match klassified--search-ag-class-definition string)
        (expect (match-string 3 string) :to-equal "42"))))

  (describe "search-class-at-point"
    (it "returns a class"
      (with-temp-buffer
        (insert "bar/animal.js:21:            return object.subclass(")
        (beginning-of-line)
        (let* ((default-directory "/project"))
          (expect (klassified-class-p (klassified--search-class-at-point))))))

    (it "calls `current-buffer' if no buffer is passed"
      (spy-on 'current-buffer :and-return-value (get-buffer-create "foo"))
      (klassified--search-class-at-point)
      (expect 'current-buffer :to-have-been-called))

    (it "does not call `current-buffer' if a buffer is passed"
      (spy-on 'current-buffer)
      (klassified--search-class-at-point (get-buffer-create "foo"))
      (expect 'current-buffer :not :to-have-been-called)))

  (describe "class-at-point"
    (it "calls class-make-from-match-data when line indented with spaces"
      (spy-on 'klassified-class-make-from-match-data)
      (with-temp-buffer
        (insert "  return object.subclass(")
        (klassified--class-at-point))
      (expect 'klassified-class-make-from-match-data :to-have-been-called))

    (it "calls class-make-from-match-data when line indented with tabs"
      (spy-on 'klassified-class-make-from-match-data)
      (with-temp-buffer
        (insert "	return object.subclass(")
        (klassified--class-at-point))
      (expect 'klassified-class-make-from-match-data :to-have-been-called))

    (it "calls class-make-from-match-data when line not indented"
      (spy-on 'klassified-class-make-from-match-data)
      (with-temp-buffer
        (insert "return object.subclass(")
        (klassified--class-at-point))
      (expect 'klassified-class-make-from-match-data :to-have-been-called))

    (it "calls class-make-from-match-data when point on next line"
      (spy-on 'klassified-class-make-from-match-data)
      (with-temp-buffer
        (insert "  return object.subclass(\n")
        (klassified--class-at-point))
      (expect 'klassified-class-make-from-match-data :to-have-been-called))

    (it "returns a result included in the project hierarchy"
      (assess-with-preserved-buffer-list
       (assess-with-filesystem '(("animal.js"
                                  "let animal = object.abstractSubclass((that, my) => {}\n"))
                               (assess-with-temp-buffers
                                ((ag-search
                                  (insert "animal.js:1:let animal = object.abstractSubclass((that, my) => {}\n")))
                                (spy-on 'klassified--search-run-ag :and-return-value ag-search)
                                (let ((class (assess-with-find-file "animal.js"
                                                                    (klassified--class-at-point)))
                                      (hierarchy (klassified--make-project-hierarchy "foo/")))
                                  (expect (hierarchy-has-item hierarchy class) :to-be-truthy)))))))

  (describe "current-line"
    (it "detects line in widen buffer"
      (let ((target-line 3))
        (with-temp-buffer
          (insert "a\nb\nc\nd\n")
          (goto-char (point-min))
          (forward-line (1- target-line))
          (expect (klassified--current-line) :to-be target-line))))

    (it "detects line in narrow buffer"
      (let ((target-line 3))
        (with-temp-buffer
          (insert "a\nb\nc\nd\n")
          (goto-char (point-min))
          (forward-line (1- target-line))
          (narrow-to-region (line-beginning-position) (line-end-position))
          (expect (klassified--current-line) :to-be target-line))))

    (it "detects line of a given position"
      (let ((target-line 3)
            pos)
        (with-temp-buffer
          (insert "a\nb\nc\nd\n")
          (goto-char (point-min))
          (forward-line (1- target-line))
          (setq pos (point))
          (goto-char (point-min))
          (expect (klassified--current-line pos) :to-be target-line))))

    (it "saves excursion"
      (let (pos)
        (with-temp-buffer
          (insert "a\nb\nc\nd\n")
          (goto-char (point-min))
          (forward-line 2)
          (setq pos (point))
          (klassified--current-line (point-max))
          (expect pos :to-be (point)))))

    (it "saves restriction"
      (let (pos)
        (with-temp-buffer
          (insert "a\nb\nc\nd\n")
          (goto-char (point-min))
          (forward-line 2)
          (narrow-to-region (line-beginning-position) (line-end-position))
          (expect (line-number-at-pos) :to-be 1)
          (klassified--current-line (point-max))
          (expect (line-number-at-pos) :to-be 1)))))

  (describe "position-make"
    (it "takes into account filepath parameter"
      (let ((result (klassified-position-make "/project" "js/animal.js" 42)))
        (expect (klassified--position-file result) :to-equal "js/animal.js")))

    (it "builds filepath from `buffer-file-name' when not provided"
      (let* ((buffer-file-name "/project/js/animal.js")
             (result (klassified-position-make "/project" nil 42)))
        (expect (klassified--position-file result) :to-equal "js/animal.js"))))

  (describe "project-path"
    (it "searches for gulpfile.js"
      (assess-with-filesystem '(("project/animals" ("gulpfile.js" "js/animal.js")))
                              (assess-with-find-file "project/animals/js/animal.js"
                                                     (expect (klassified-project-path) :to-match "project/animals$"))))

    (it "returns `default-directory' when no project file"
      (assess-with-filesystem '("project/animals/js/animal.js")
                              (assess-with-find-file "project/animals/js/animal.js"
                                                     (expect (klassified-project-path) :to-match "project/animals/js$")))))

  (describe "class-make-from-match-data"
    (it "extracts data"
      (spy-on 'klassified-class-make-from-extracted-data)
      (with-temp-buffer
        (insert "abc")
        (goto-char (point-min))
        (looking-at  "\\(?2:.\\)\\(?4:.\\)\\(?5:.\\)")
        (klassified-class-make-from-match-data (match-data) 1 2 3))
      (expect 'klassified-class-make-from-extracted-data
              :to-have-been-called-with "a" "b" "c" 1 2 3)))

  (describe "class-make-from-extracted-data"
    (it "uses filename for classname when classname is nil"
      (let ((class (klassified-class-make-from-extracted-data
                    nil "" "" "/tmp" "/tmp/foo/animal.js" 1)))
        (expect (klassified-class-name class) :to-equal "animal")))

    (it "uses filename for classname when classname is empty"
      (let ((class (klassified-class-make-from-extracted-data
                    "" "" "" "/tmp" "/tmp/foo/animal.js" 1)))
        (expect (klassified-class-name class) :to-equal "animal")))

    (it "uses classname when provided"
      (let ((class (klassified-class-make-from-extracted-data
                    "bird" "" "" "/tmp" "/tmp/foo/animal.js" 1)))
        (expect (klassified-class-name class) :to-equal "bird"))))

  (describe "search-regexp-to-pcre"
    (it "calls `xref--regexp-to-extended'"
      (spy-on 'xref--regexp-to-extended :and-return-value "foo")
      (klassified--search-regexp-to-pcre "foo")
      (expect 'xref--regexp-to-extended :to-have-been-called-with "foo"))

    (it "removes group id"
      (expect (klassified--search-regexp-to-pcre "\\(?1:.\\)") :to-equal "(.)")))

  (describe "search-run-ag"
    (it "throws an error when call-process returns no-0"
      (spy-on 'call-process :and-return-value 1)
      (spy-on 'error)
      (assess-with-filesystem '("foo/")
                              (assess-with-preserved-buffer-list
                               (klassified--search-run-ag "foo")))
      (expect 'call-process :to-have-been-called)
      (expect 'error :to-have-been-called))

    (it "converts regular expressions"
      (spy-on 'call-process :and-return-value 0)
      (spy-on 'klassified--search-regexp-to-pcre)
      (assess-with-filesystem '("foo/")
                              (assess-with-preserved-buffer-list
                               (klassified--search-run-ag "foo")))
      (expect 'klassified--search-regexp-to-pcre :to-have-been-called)))

  (describe "search-collect-next-class"
    (it "calls search-next-class-at-point"
      (spy-on 're-search-forward :and-return-value t)
      (spy-on 'beginning-of-line)
      (spy-on 'forward-line)
      (spy-on 'klassified--search-class-at-point :and-return-value "foo")
      (expect (klassified--search-collect-next-class) :to-equal "foo")
      (expect 'klassified--search-class-at-point :to-have-been-called))

    (it "returns nil if no match"
      (spy-on 're-search-forward :and-return-value nil)
      (expect (klassified--search-collect-next-class) :to-be nil))

    (it "returns nil if no class"
      (spy-on 're-search-forward :and-return-value nil)
      (spy-on 'beginning-of-line)
      (spy-on 'forward-line)
      (spy-on 'klassified--search-class-at-point :and-return-value nil)
      (expect (klassified--search-collect-next-class) :to-be nil)))

  (describe "search-collect-classes"
    (it "calls collect-next-class"
      (spy-on 'klassified--search-collect-next-class)
      (with-temp-buffer
        (klassified--search-collect-classes))
      (expect 'klassified--search-collect-next-class :to-have-been-called))))

(provide 'klassified-test)
;;; klassified-test.el ends here
