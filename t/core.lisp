(defpackage #:html2text-test/core
  (:use #:cl
        #:html2text/core
        #:rove
        #:hamcrest/rove))
(in-package html2text-test/core)



(deftest test-html-with-just-text
  (testing "Plain text should not be transformed"
    (ok (equal (html2text "Foo bar")
               "Foo bar")))
  (testing "Newlines and should be trimmed from the beginning and the end"
    (ok (equal (html2text "

Foo bar

")
               "Foo bar")))
  
  (testing "Whitespaces should be normalized."
    (ok (equal (html2text "   foo    bar   ")
               "foo bar"))))

(deftest test-html-with-single-text-node
  (ok (equal (html2text "<html>Foo bar</html>")
             "Foo bar")))


(deftest test-html-with-body-tag
  (ok (equal (html2text "<html><body>Foo bar</body></html>")
             "Foo bar")))


(deftest test-html-with-body-tag-and-bold
  (ok (equal (html2text "<html><body><b>Foo</b> bar</body></html>")
             "**Foo** bar"))
  (ok (equal (html2text "<html><body><strong>Foo</strong> bar</body></html>")
             "**Foo** bar")))


(deftest test-html-with-body-tag-and-italic
  (ok (equal (html2text "<html><body><em>Foo</em> bar</body></html>")
             "_Foo_ bar"))
  (ok (equal (html2text "<html><body><i>Foo</i> bar</body></html>")
             "_Foo_ bar"))
  (ok (equal (html2text "<html><body><u>Foo</u> bar</body></html>")
             "_Foo_ bar")))


(deftest test-html-with-paragraphs
  (testing "Paragraphs should be separated with a single line."
    (testing "If there is a space between <p> tags, it is removed"
      (ok (equal (html2text "<p>Foo</p> <p>bar</p>")
                 "Foo

bar

")))
    ;; This test does not work yet, because Plump parses
    ;; this documents as <p>Foo<p>bar</p></p>
    ;; (testing "Also, spaces are removed from the beginning and the end of paragraph texts"
    ;;       (ok (equal (html2text "<p>  Foo  <p>  bar  ")
    ;;                  "Foo

    ;; bar

    ;; ")))
    (testing "Newlines are removed as well"
      (ok (equal (html2text "<p>
Foo
</p>

<p>
bar
</p>")
                 "Foo

bar

")))))


(deftest test-html-with-link
  (ok (equal (html2text "This is <a href=\"http://ultralisp.org/\">ultralisp</a> project.")
             "This is [ultralisp](http://ultralisp.org/) project.")))


(deftest test-style-and-script-should-be-remove
  (ok (equal (html2text "Doc with <style>.body {color: black;}</style>.")
             "Doc with ."))
  (ok (equal (html2text "Doc with <script>alert(\"foo\");</script>.")
             "Doc with .")))


(deftest test-how-simple-ul-is-rendered
  (ok (equal (html2text "
<ul>
   <li>This is a first line.</li>
   <li>Second line.</li>
   <li>And third line.</li>
</ul>")
             
             "* This is a first line.
* Second line.
* And third line.
")))


;; This case is not handled by python's html2text!
(deftest test-how-multiline-ul-is-rendered
  (ok (equal (html2text "
<ul>
   <li>This is a first line.</li>
   <li>Second line is
       multiline.</li>
   <li><p>And third contains few paragraphs.</p>
       <p>Second paragraph.</p></li>
   <li>And last line.</li>
</ul>")
             
             "* This is a first line.
* Second line is multiline.
* And third contains few paragraphs.

  Second paragraph.


* And last line.
")))


(deftest test-bold-in-the-paragraph
  (ok (equal (html2text "<p>This is a multi
  line
  paragraph.<b>
      This is a multi
  line
  span.</b>
</p>")
             ;; TODO: here should be a space before the first **
             ;;       but I can't figure out how to implement this.
             "This is a multi line paragraph. **This is a multi line span.**

")))
