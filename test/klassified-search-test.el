;;; klassified-search-test.el --- Tests for klassified-search  -*- lexical-binding: t; -*-

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

(require 'klassified-search)

(require 'buttercup)
(require 'assess)

(describe "klassified-search"
  (describe "can analyze search results"
    (describe "and match a class definition"
      (it "with `return'"
        (expect "foo/bar.js:42: return object.subclass("
                :to-match klassified-search-ag-class-regexp))

      (it "with no space after column"
        (expect "foo/bar.js:42:return object.subclass("
                :to-match klassified-search-ag-class-regexp))

      (it "but not two"
        (let* ((string1 "bar.js:42:return object.subclass(")
               (string2 "\nfoo.js:99:return bar.subclass(\n")
               (string (concat string1 string2)))
          (expect (string-match klassified-search-ag-class-regexp string) :to-be-truthy)
          (expect (match-end 0) :to-be (length string1)))))

    (it "and extract a filepath with directory"
      (let ((string "foo/bar.js:42: let baz = object.subclass("))
        (string-match klassified-search-ag-class-regexp string)
        (expect (match-string 1 string) :to-equal "foo/bar.js")))

    (it "and extract a filepath without directory"
      (let ((string "bar.js:42: let baz = object.subclass("))
        (string-match klassified-search-ag-class-regexp string)
        (expect (match-string 1 string) :to-equal "bar.js")))

    (it "and extract a class name from the filename"
      (let ((string "foo/bar.js:42: return object.subclass("))
        (string-match klassified-search-ag-class-regexp string)
        (expect (match-string 2 string) :to-equal "bar")))

    (it "and will prefer variable name for classname"
      (let ((string "foo/bar.js:42: let baz = object.subclass("))
        (string-match klassified-search-ag-class-regexp string)
        (expect (match-string 2 string) :to-equal "baz")))

    (it "and extract a line number"
      (let ((string "foo/bar.js:42: let baz = object.subclass("))
        (string-match klassified-search-ag-class-regexp string)
        (expect (match-string 3 string) :to-equal "42"))))

  (describe "search-class-at-point"
    (it "returns a class"
      (with-temp-buffer
        (insert "bar/animal.js:21:            return object.subclass(")
        (beginning-of-line)
        (let* ((default-directory "/project"))
          (expect (klassified-core-class-p (klassified-search--class-at-point))))))

    (it "calls `current-buffer' if no buffer is passed"
      (spy-on 'current-buffer :and-return-value (get-buffer-create "foo"))
      (klassified-search--class-at-point)
      (expect 'current-buffer :to-have-been-called))

    (it "does not call `current-buffer' if a buffer is passed"
      (spy-on 'current-buffer)
      (klassified-search--class-at-point (get-buffer-create "foo"))
      (expect 'current-buffer :not :to-have-been-called)))

  (describe "regexp-to-pcre"
    (it "calls `xref--regexp-to-extended'"
      (spy-on 'xref--regexp-to-extended :and-return-value "foo")
      (klassified-search--regexp-to-pcre "foo")
      (expect 'xref--regexp-to-extended :to-have-been-called-with "foo"))

    (it "removes group id"
      (expect (klassified-search--regexp-to-pcre "\\(?1:.\\)") :to-equal "(.)")))

  (describe "run-ag"
    (it "throws an error when call-process returns no-0"
      (spy-on 'call-process :and-return-value 1)
      (spy-on 'error)
      (assess-with-filesystem '("foo/")
                              (assess-with-preserved-buffer-list
                               (klassified-search--run-ag "foo")))
      (expect 'call-process :to-have-been-called)
      (expect 'error :to-have-been-called))

    (it "converts regular expressions"
      (spy-on 'call-process :and-return-value 0)
      (spy-on 'klassified-search--regexp-to-pcre)
      (assess-with-filesystem '("foo/")
                              (assess-with-preserved-buffer-list
                               (klassified-search--run-ag "foo")))
      (expect 'klassified-search--regexp-to-pcre :to-have-been-called)))

  (describe "collect-next-class"
    (it "calls class-at-point"
      (spy-on 're-search-forward :and-return-value t)
      (spy-on 'beginning-of-line)
      (spy-on 'forward-line)
      (spy-on 'klassified-search--class-at-point :and-return-value "foo")
      (expect (klassified-search--collect-next-class) :to-equal "foo")
      (expect 'klassified-search--class-at-point :to-have-been-called))

    (it "returns nil if no match"
      (spy-on 're-search-forward :and-return-value nil)
      (expect (klassified-search--collect-next-class) :to-be nil))

    (it "returns nil if no class"
      (spy-on 're-search-forward :and-return-value nil)
      (spy-on 'beginning-of-line)
      (spy-on 'forward-line)
      (spy-on 'klassified-search--class-at-point :and-return-value nil)
      (expect (klassified-search--collect-next-class) :to-be nil)))

  (describe "search-collect-classes"
    (it "calls collect-next-class"
      (spy-on 'klassified-search--collect-next-class)
      (with-temp-buffer
        (klassified-search--collect-classes-in-buffer))
      (expect 'klassified-search--collect-next-class :to-have-been-called))))

(provide 'klassified-search-test)
;;; klassified-search-test.el ends here
