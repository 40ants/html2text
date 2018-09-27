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
                 "
Foo

bar")))
    (testing "Also, spaces are removed from the beginning and the end of paragraph texts"
      (ok (equal (html2text "<p>  Foo  <p>  bar  ")
                 "
Foo

bar")))
    (testing "Newlines are removed as well"
      (ok (equal (html2text "<p>Foo

<p>bar")
                 "
Foo

bar")))))


(deftest test-html-with-link
  (ok (equal (html2text "This is <a href=\"http://ultralisp.org/\">ultralisp</a> project.")
             "This is [ultralisp](http://ultralisp.org/) project.")))
