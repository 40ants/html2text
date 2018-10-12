(defpackage #:html2text-link-revealer/utils
  (:nicknames #:link-revealer)
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:log4cl)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:get-final-url
   #:with-turned-on))
(in-package html2text-link-revealer/utils)


(defvar *max-redirects* 10
  "A maximum redirects we'll try to follow.")


(defun head-or-get (url &key (max-redirects *max-redirects*))
  (handler-case (dex:request url :method :head
                                 :max-redirects max-redirects)
    (dexador.error:http-request-method-not-allowed ()
      (dex:request url :method :get
                       :max-redirects max-redirects))))


(defcached get-final-url (url &key (max-redirects *max-redirects*))
  "Goes through all redirects and returns a real URL."
  (let ((uri (nth-value 3 (head-or-get url
                                       :max-redirects max-redirects))))
    (quri:render-uri uri)))


(defmacro with-turned-on ((&key (max-redirects nil max-redirects-p))
                          &body body)
  (let ((opts (append `((html2text:*href-processor* 'get-final-url))
                      (when max-redirects-p
                        `((*max-redirects* ,max-redirects))))))
    `(let (,@opts)
       ,@body)))
