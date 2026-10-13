;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 15 of the 1992 manual, "Keywords and
;;;; Symbols" (manual/Dylan-1992/book.annotated/ch15.html), in the form of
;;;; tests/codegen/runner.lisp.  A program's symbols and keywords are
;;;; entered when it is read, so where the chapter's transcript depends on
;;;; which spelling was entered first, the program spells it that way
;;;; first.

(
 ;; 15 <symbol>, <keyword>: each a subclass of <object>; keywords are a
 ;; distinct class, not symbols ("Syntax", foo:).  From the
 ;; specification.
 (ch15-classes
  "(print (list (subclass? <symbol> <object>) (subclass? <keyword> <object>)
                (instance? 'foo <symbol>) (instance? foo: <keyword>)
                (instance? foo: <symbol>) (instance? 'foo <keyword>) (id? 'foo foo:)))"
  "(#t #t #t #t #f #f #f)
")

 ;; 15 as <symbol> and as <keyword>, the examples:
 ;;   (as <symbol> "foo") => foo        (as <keyword> "foo") => foo:
 (ch15-as-symbol-keyword
  "(print (as <symbol> \"foo\"))
   (print (as <keyword> \"foo\"))"
  "foo
foo:
")

 ;; 15, the example (id? 'FOO (as <symbol> \"Foo\")) => #t: two symbols
 ;; (or keywords) with the same name are id?, whatever their case, and as
 ;; always returns the same one for strings of the same characters.
 (ch15-symbol-identity
  "(print (id? 'FOO (as <symbol> \"Foo\")))
   (print (list (id? 'abc 'ABC) (id? 'abc 'aBc) (id? (as <symbol> \"xyz\") (as <symbol> \"XYZ\"))
                (id? 'abc 'abd)))
   (print (list (id? abc: ABC:) (id? (as <keyword> \"Key\") key:) (id? (as <keyword> \"q\") q:)
                (id? abc: abd:)))"
  "#t
(#t #t #t #f)
(#t #t #t #f)
")

 ;; 15: "the case of keywords and symbols is remembered from the first
 ;; time the keyword or symbol is entered.  This case is used when the
 ;; keyword or symbol is printed."  The example 'Foo => foo, foo having
 ;; been entered first.
 (ch15-symbol-case
  "(print 'foo)
   (print 'Foo)
   (print 'FOO)
   (print (as <symbol> \"FoO\"))
   (print 'Hello)
   (print 'hello)
   (print (as <symbol> \"HELLO\"))
   (print Quux:)
   (print quux:)
   (print (as <keyword> \"QUUX\"))"
  "foo
foo
foo
foo
Hello
Hello
Hello
Quux:
Quux:
Quux:
")

 ;; 15 as <string>, the examples (as <string> 'Foo) => \"foo\" (foo
 ;; entered first) and (as <string> 'bar:) => \"bar\": the name, in the
 ;; symbol's or keyword's case, without the colon.
 (ch15-as-string
  "(print 'foo)
   (print (as <string> 'Foo))
   (print (as <string> 'bar:))
   (print (as <string> 'MixedCase))
   (print (as <string> Other:))
   (print (instance? (as <string> 'foo) <string>))"
  "foo
\"foo\"
\"bar\"
\"MixedCase\"
\"Other\"
#t
")
)
