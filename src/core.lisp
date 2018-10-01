(defpackage #:html2text/core
   (:nicknames #:html2text)
   (:use #:cl)
   (:import-from #:alexandria
                 #:ensure-list
                 #:make-keyword)
   (:import-from #:html2text/utils
                 #:write-pretty-string)
   (:import-from #:plump)
   (:import-from #:log4cl)
   (:export #:foo
            #:bar
            #:html2text
            #:serialize))
(in-package html2text/core)


(defvar *output-stream* t)


(defparameter *tags-to-remove* '(:style :script))
(defparameter *block-elements* '(:p :style :script :ul :ol :li :div :hr))


(defgeneric get-node-tag (node)
  (:method ((node t))
    "Returning nil by default, because not all plump's objects are tags."
    nil)
  (:documentation "Returns a keyword like :p or :div or nil by default."))


(defgeneric serialize (tag node)
  (:method ((tag t) (node t))
    "By default, we don't serialize a node, because not all nodes should have text representation."
    (values))
  (:documentation "Receives a tag (which can be a nil or a keyword) and a plump document node.
                   Writes to a standard output stream a text representation of the node if any."))

(defmethod get-node-tag ((node plump:element))
  (make-keyword (string-upcase (plump:tag-name node))))


(defmethod serialize ((tag t) (node plump:nesting-node))
  (let ((children (plump:children node))
        (output-was-produced nil))
    
    (loop with prev-node-was-block = t
          for idx below (length children)
          for node = (aref children idx)
          for node-tag = (get-node-tag node)
          for tag-should-be-skipped = (member node-tag *tags-to-remove*)
          
          ;; Here we track a type of the previous node,
          ;; to know if we need to trim leading whitespace
          ;; For example, when this HTML "<span>foo</span> bar"
          ;; is transformed into the text, a whitespace before "bar"
          ;; should be keeped. But if we replace "span" with "p",
          ;; then space should be removed.
          do (unless tag-should-be-skipped
               (unless prev-node-was-block
                 (log:debug "Writing whitespace")
                 (write-char #\Space *output-stream*))
               ;; Serialize should return non nil if tag didn't produce any output
               (when (serialize node-tag
                                node)
                 (setf output-was-produced t)
                 (setf prev-node-was-block
                       (member node-tag *block-elements*)))))
    (or (call-next-method)
        output-was-produced)))


(defun normalize-whitespaces (string &key
                                       (chars-to-trim '(#\Newline #\Space #\Tab))
                                       (trim-left t)
                                       (trim-right t))
  "Returns a string with multiple spaces replaced by one, optionally trimmed."
  (check-type string string)
  (check-type chars-to-trim list)
  (let* ((char-found nil)
         (string (if trim-left
                     (string-left-trim chars-to-trim string)
                     string))
         (string (if trim-right
                     (string-right-trim chars-to-trim string)
                     string)))

    (with-output-to-string (stream)
      (loop for c across string do
        (if (member c chars-to-trim :test #'char=)
            (when (not char-found)
              (write-char #\Space stream)
              (setq char-found t))
            (progn
              (when char-found
                  (setf char-found nil))
              (write-char c stream)))))))


(defmethod serialize ((tag t) (node plump:text-node))
  (let* ((text (plump:text node))
         (normalized-text (normalize-whitespaces text)))

    (log:debug "Serializing text node" normalized-text)
    
    (unless (string= normalized-text "")
      (write-string normalized-text
                    *output-stream*)
      ;; We need to indicate that some value was written to the output
      (values t))))


(defmacro def-tag-serializer ((&rest tags) &body body)
  (let ((definitions (loop for tag in tags
                           collect `(defmethod serialize ((tag (eql ,tag)) node)
                                      (declare (ignorable node))
                                      ,@body))))
    `(progn ,@definitions)))


(def-tag-serializer (:b :strong)
  (write-string "**" *output-stream*)
  (call-next-method)
  (write-string "**" *output-stream*))


(def-tag-serializer (:em :i :u)
  (write-string "_" *output-stream*)
  (call-next-method)
  (write-string "_" *output-stream*))


(def-tag-serializer (:p)
  (pprint-logical-block (*output-stream* nil)
    (call-next-method)
    (pprint-newline :mandatory *output-stream*)
    (pprint-newline :mandatory *output-stream*)))


(def-tag-serializer (:li)
  (pprint-logical-block (*output-stream* nil :prefix "* ")
    (call-next-method)
    (pprint-indent :block -2 *output-stream*)
    (pprint-newline :mandatory *output-stream*)))


(def-tag-serializer (:a)
  (let ((url (or (plump:attribute node "href")
                 "")))
    (write-string "[" *output-stream*)
    (call-next-method)
    (format *output-stream* "](~A)"
            url)))


(def-tag-serializer (:blockquote)
  (pprint-logical-block (*output-stream* nil :per-line-prefix "> ")
    (call-next-method)))


(def-tag-serializer (:hr)
  (write-string "***" *output-stream*)
  (terpri *output-stream*)
  (terpri *output-stream*)
  (values t))


(def-tag-serializer (:img)
  (let ((url (or (plump:attribute node "src")
                 "")))
    (format *output-stream* "![](~A)"
            url))
  (values t))


;; code - `some text`

;; <pre><code>
;; ```
;; some
;; text
;; ```

;; ol/li


(defmethod serialize :around (tag node)
  (log:debug "Serializing" tag node)
  (call-next-method))


(defun html2text (text)
  (check-type text string)
  (let* ((document (plump:parse text))
         (*print-pretty* t))
    (with-output-to-string (*output-stream*)
      (serialize nil document))))
