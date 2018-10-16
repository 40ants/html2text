(defpackage #:html2text-test/link-revealer
  (:use #:cl
        #:rove)
  (:import-from #:html2text-link-revealer
                #:get-final-url)
  (:import-from #:cl-mock
                #:answer
                #:with-mocks))
(in-package html2text-test/link-revealer)


(deftest test-error-processing
  (function-cache:clear-cache-all-function-caches)
  (testing "500 error should be ignored"
    (ok (equal (get-final-url "http://httpbin.org/status/500")
               "http://httpbin.org/status/500")))
  
  (function-cache:clear-cache-all-function-caches)
  (testing "404 error should be ignored"
    (ok (equal (get-final-url "http://httpbin.org/status/404")
               "http://httpbin.org/status/404")))
  
  (function-cache:clear-cache-all-function-caches)
  (testing "Timeouts should be ignored as well"
    (with-mocks ()
      (answer dex:request (error 'usocket:timeout-error))
      (ok (equal (get-final-url "http://httpbin.org/status/200")
                 "http://httpbin.org/status/200")))))
