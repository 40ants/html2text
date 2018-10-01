(defsystem html2text
           :version (:read-file-form "version.lisp-expr")
           :author "Alexander Artemenko"
           :license "BSD"
           :class :package-inferred-system
           :pathname "src"
           :depends-on ("html2text/core")
           :description "Converts HTML text into the Markdown."
           :long-description
           #.(with-open-file (stream (merge-pathnames
                                      #p"README.rst"
                                      (or *load-pathname* *compile-file-pathname*))
                                     :if-does-not-exist nil
                                     :direction :input)
               (when stream
                 (let ((seq (make-array (file-length stream)
                                        :element-type 'character
                                        :fill-pointer t)))
                   (setf (fill-pointer seq)
                         (read-sequence seq stream))
                   seq)))
           :in-order-to ((test-op (test-op html2text-test))))

