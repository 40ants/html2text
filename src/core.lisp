(defpackage #:html2text/core
   (:nicknames #:html2text)
   (:use #:cl)
   (:import-from #:alexandria
                 #:ensure-list
                 #:make-keyword)
   (:export #:foo
            #:bar
            #:html2text
            #:serialize))
(in-package html2text/core)


(defvar *output-stream* t)

(defparameter *trim-left* t)
(defparameter *trim-right* t)


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
        (block-elements '(:p :style :script)))
    (loop with prev-node-was-block = t
          for idx below (length children)
          for node = (aref children idx)
          for node-tag = (get-node-tag node)
          for next-node = (when (< idx (- (length children)
                                          1))
                            (aref children (+ idx 1)))
          for next-node-is-block = (if next-node
                                       (member (get-node-tag next-node)
                                               block-elements)
                                       t)
          ;; Here we track a type of the previous node,
          ;; to know if we need to trim leading whitespace
          ;; For example, when this HTML "<span>foo</span> bar"
          ;; is transformed into the text, a whitespace before "bar"
          ;; should be keeped. But if we replace "span" with "p",
          ;; then space should be removed.
          do (let ((*trim-left* prev-node-was-block)
                   (*trim-right* next-node-is-block))
               (serialize node-tag
                          node))
             (setf prev-node-was-block
                   (member node-tag block-elements)))
    (call-next-method)))


(defun normalize-whitespaces (string &key
                                       (char #\space)
                                       (trim-left t)
                                       (trim-right t))
  "Returns a string with multiple spaces replaced by one, optionally trimmed."
  (check-type string string)
  (check-type char character)
  (let* ((char-found nil)
         (chars-to-trim (list char))
         (string (if trim-left
                     (string-left-trim chars-to-trim string)
                     string))
         (string (if trim-right
                     (string-right-trim chars-to-trim string)
                     string)))

    (with-output-to-string (stream)
      (loop for c across string do
        (if (char= c char)
            (when (not char-found)
              (write-char c stream)
              (setq char-found t))
            (progn
              (if char-found (setf char-found nil))
              (write-char c stream)))))))


(defmethod serialize ((tag t) (node plump:text-node))
  (let* ((text (plump:text node))
         (normalized-text (normalize-whitespaces text
                                                 :trim-left *trim-left*
                                                 :trim-right *trim-right*))
         (trimmed-text (string-trim '(#\Newline)
                                    normalized-text)))
    
    (write-string trimmed-text
                  *output-stream*)))


(defmacro def-tag-serializer ((&rest tags) &body body)
  (let ((definitions (loop for tag in tags
                           collect `(defmethod serialize ((tag (eql ,tag)) node)
                                      (declare (ignorable node))
                                      ,@body))))
    `(progn ,@definitions)))


(def-tag-serializer (:b :strong)
  (write-string "**" *output-stream*)
  (let ((*trim-left* nil)
        (*trim-right* nil))
    (call-next-method))
  (write-string "**" *output-stream*))


(def-tag-serializer (:em :i :u)
  (write-string "_" *output-stream*)
  (let ((*trim-left* nil)
        (*trim-right* nil))
    (call-next-method))
  (write-string "_" *output-stream*))


(def-tag-serializer (:p)
  (format *output-stream* "~2&")
  (call-next-method))


(def-tag-serializer (:a)
  (let ((url (or (plump:attribute node "href")
                 "")))
    (write-string "[" *output-stream*)
    (call-next-method)
    (format *output-stream* "](~A)"
            url)))


;; These tags are removed completely with content
(def-tag-serializer (:style :script))

;; blockquote
;; > some
;; > text

;; hr
;;
;; ***
;;

;; code - `some text`

;; <pre><code>
;; ```
;; some
;; text
;; ```

;; img

;; ul/ol/li


(defmethod serialize :around (tag node)
  (log:info "Serializing" tag node)
  (call-next-method))


(defun html2text (text)
  (check-type text string)
  (let* ((document (plump:parse text)))
    (with-output-to-string (*output-stream*)
      (serialize nil document))))
