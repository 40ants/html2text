(defpackage #:html2text-test/utils
  (:use #:cl
        #:html2text/utils
        #:rove
        #:hamcrest/rove))
(in-package html2text-test/utils)


;; TODO: remove
;; (deftest test-string-cleaning
;;   (ok (equal (with-output-to-string (s)
;;                (write-cleaned-string "  Blah
;;   minor
;;   " s))
;;              "Blah minor")))
