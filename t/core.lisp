(defpackage #:html2text-test/core
  (:use #:cl
        #:html2text/core
        #:rove
        #:hamcrest/rove)
  (:shadow #:write))
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

bar")))
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

bar")))))


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
* And third line.")))

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

* And last line.")))


(deftest test-how-simple-ol-is-rendered
  (ok (equal (html2text "
<ol>
   <li>This is a first line.</li>
   <li>Second line.</li>
   <li>And third line.</li>
</ol>")
             
             "1. This is a first line.
2. Second line.
3. And third line.")))


(deftest test-how-nested-ol-are-rendered
  (ok (equal (html2text "
<ol>
   <li>This is a first line.</li>
   <li>Variants:
     <ol>
       <li>First.</li>
       <li>Second.</li>
       <li>Third.</li>
     </ol>
   </li>
   <li>And third line.</li>
</ol>
")
             
             "1. This is a first line.
2. Variants:

   1. First.
   2. Second.
   3. Third.

3. And third line.")))


(deftest test-bold-in-the-paragraph
  (ok (equal (html2text "<p>This is a multi
  line
  paragraph.<b>
      This is a multi
  line
  span.</b>
</p>")
             "This is a multi line paragraph. **This is a multi line span.** ")))


(deftest test-blockquote
  (testing "If there is plaintext inside the tag, it should be streamlined."
    (ok (equal (html2text "<blockquote>
All the world's a stage, and all the men and women merely players: they have their exits and their entrances; and one man in his time plays many parts, his acts being seven ages.

William Shakespeare
</blockquote>")
               "> All the world's a stage, and all the men and women merely players: they have their exits and their entrances; and one man in his time plays many parts, his acts being seven ages. William Shakespeare"
               )))

  
  (testing "If there are some tags inside, they are rendered as usual, but prefixed with \"> \"."
    (ok (equal (html2text "<blockquote>
<p>All the world's a stage, and all the men and women merely players: they have their exits and their entrances; and one man in his time plays many parts, his acts being seven ages.</p>

<p>William Shakespeare</p>
</blockquote>")
               "> All the world's a stage, and all the men and women merely players: they have their exits and their entrances; and one man in his time plays many parts, his acts being seven ages.
>
> William Shakespeare"))))


(deftest test-hr-tag
  (ok (equal (html2text "<p>First paragraph.</p>
<hr/>
<p>Second paragraph.</p>")
             "First paragraph.

***

Second paragraph.")))


(deftest test-img
  (testing "Simple case"
    (ok (equal (html2text "<img src=\"http://example.com/img.png\">")
               "![](http://example.com/img.png)")))

  ;; Note, there is a space after the image in the output.
  ;; Don't want to think how to get rid of it.
  ;; Python's html2text renders it with two spaces, like that
  ;; [ ![](http://example.com/img.png) ](http://blah.org)
  ;; but I think the right solution is no spaces either side.
  (testing "When image is inside the \"a\" tag"
    (ok (equal (html2text "<a href=\"http://blah.org\">
<img src=\"http://example.com/img.png\">
</a>")
               "[![](http://example.com/img.png) ](http://blah.org)"))))


(deftest test-simple-code-block
  (ok (equal (html2text "This is <code>a code</code> in the sentence.")
             "This is `a code` in the sentence.")))


(deftest test-code-block-surrounded-by-pre
  (testing "How <pre><code> block is rendered when inline."
    (ok (equal (html2text "This is <pre><code>a code</code></pre> in the sentence.")
               "This is

```
a code
```

in the sentence.")))
  
  (testing "How does multiline code block works"
    (ok (equal (html2text "How about

<pre>
<code>
a snippet
of code

in the middle
of the text?
</code>
</pre>

Is it ok?")
               "How about

```
a snippet
of code

in the middle
of the text?
```

Is it ok?"))))


(defmacro get-output (blocks)
  `(let ((*print-pretty* t)
         (html2text/core::*written-newlines* 0)
         (html2text/core::*block-index* 0))
     
     (with-output-to-string (html2text/core::*output-stream*)
       ,blocks)))

(defmacro compare-output (blocks expected)
  `(ok (string=
        (get-output ,blocks)
        ,expected)))


(deftest test-blocks-margins
  (compare-output
   (text-block ()
     (inline-block ()
       (write "Foo"))

     ;; Here we expect "Bar" will be surrounded
     ;; only by one empty line from top and bottom
     (text-block (:margin 1)
       (text-block (:margin 1)
         (write "Bar")))
     
     (inline-block ()
       (write "Blah minor")))
   "Foo

Bar

Blah minor"))


(deftest test-blocks-margins1
  (compare-output
   (text-block ()
     (text-block ()
       (write "Foo"))
     (text-block ()
       (write "Bar")))

   "Foo
Bar"))


(deftest test-blocks-margins2
  (compare-output
   (text-block ()
     (text-block ()
       (write "Foo"))
     (text-block (:margin 1)
       (write "Bar")))

   "Foo

Bar"))


(deftest test-blocks-margins3
  (compare-output
   (text-block ()
     (text-block (:margin 1)
       (write "Foo"))
     (text-block (:margin 1)
       (write "Bar")))

   ;; Here we expect, that empty line
   ;; before "Foo" will be omitted, because it is
   ;; the first element
   "Foo

Bar"))


(deftest test-blocks-margins4
  (compare-output
   (text-block (:per-line-prefix "> ")
     (text-block (:margin 1)
       (write "Foo"))
     (text-block (:margin 1)
       (write "Bar")))

   ;; The same, empty lines before "Foo"
   ;; are omitted, because it is the first element
   "> Foo
>
> Bar"))


(deftest test-blocks-margins5
  (compare-output
   (text-block (:per-line-prefix "> ")
     (text-block (:margin 1)
       (write "Foo"))
     (text-block ()
       (write "Bar")))

   "> Foo
>
> Bar"))


(deftest test-blocks-margins6
  (compare-output
   (text-block () 
     (text-block (:margin 1)
       (write "First line"))
       
     (text-block (:per-line-prefix "> ")
       (text-block (:margin 1)
         (write "Foo"))
       (text-block ()
         (write "Bar"))))

   "First line

> Foo
>
> Bar"))


(deftest test-html-headings
  (ok (equal (html2text "<h1>First level</h1>
<h2>Second level</h2><h3>Third level</h3>
<h4>Fourth level</h4><h5>Fifth level</h5>
<h6>Sixth level</h6>")
             "# First level

## Second level

### Third level

#### Fourth level

##### Fifth level

###### Sixth level")))
