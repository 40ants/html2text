(defpackage #:html2text-link-revealer/utils
  (:nicknames #:link-revealer #:html2text-link-revealer)
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:log4cl)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:cl-strings
                #:starts-with)
  (:export
   #:get-final-url
   #:with-turned-on))
(in-package html2text-link-revealer/utils)


(defvar *max-redirects* 10
  "A maximum redirects we'll try to follow.")


(defun head-or-get (url &key
                          (max-redirects *max-redirects*)
                          (timeout 1))
  (handler-case (dex:request url :method :head
                                 :max-redirects max-redirects
                                 :timeout timeout)
    (dexador.error:http-request-method-not-allowed ()
      (dex:request url :method :get
                       :max-redirects max-redirects
                       :timeout timeout))))


(defcached get-final-url (url &key
                              (max-redirects *max-redirects*)
                              (timeout 1))
  "Goes through all redirects and returns a real URL."
  (cond
    ((or (starts-with url "http://")
         (starts-with url "https://"))
     (handler-case (let ((uri (nth-value 3 (head-or-get url
                                                        :max-redirects max-redirects
                                                        :timeout timeout))))
                     (quri:render-uri uri))
       ;; In case of errors, we'll return URL as is
       (error ()
         url)))
    (t url)))


(defmacro with-turned-on ((&key (max-redirects nil max-redirects-p))
                          &body body)
  (let ((opts (append `((html2text:*href-processor* 'get-final-url))
                      (when max-redirects-p
                        `((*max-redirects* ,max-redirects))))))
    `(let (,@opts)
       ,@body)))
