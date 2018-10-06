(defpackage #:html2text/core
  (:nicknames #:html2text)
  (:use #:cl)
  (:shadow #:write)
  (:import-from #:alexandria
                #:last-elt
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
(defparameter *block-elements* '(:p :style :script :ul :ol :li :div :hr :pre))

;; It should be bound to :ul or :ol
;; depending on the context.
(defvar *list-style*)

;; This variable will contain a sequential number for the nest <li> item
;; when rendering :ol list.
(defvar *list-number*)

(defvar *in-pre* nil
  "This variable will be set to 't when rendering content inside <pre> block.")


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


(defparameter *written-newlines* 0)


(defun write (&rest strings)
  ;; First we need to generate newlines requested by previous blocks of text
  ;; (when (> *written-newlines* 0)
  ;;   (log:info "Writing newlines" *written-newlines*)
  ;;   (loop repeat *written-newlines*
  ;;         do (pprint-newline :mandatory *output-stream*)))
  
  (let* ((string (last-elt strings))
         ;; (last-index (when (> end start)
         ;;               (- end 1)))
         ;; (last-char (when last-index
         ;;              (elt string last-index)))
         (num-newlines (loop :for idx
                             :downfrom (- (length string)
                                          1)
                               :to 0
                             :for char = (elt string idx)
                             :unless (char= char #\Newline)
                               :do (return result)
                             :summing 1 :into result)))
    
    ;; (when (and last-char
    ;;            (not (char= last-char #\Newline)))
    ;;   (log:info "Resetting to 0 string")
    ;;   (setf *written-newlines* 0))
    (when num-newlines
      (log:info "Resetting newlines to" num-newlines)
      (setf *written-newlines* num-newlines)))

  (loop for string in strings
        for string-without-newlines = (string-trim '(#\Newline) string)
        do (write-string string-without-newlines
                         *output-stream*))
  
  ;; We need to indicate that something was written
  ;; to be able to put spaces between inline elements.
  (values t))


(defun %write-newlines-if-needed ()
  (when (> *written-newlines* 0)
    (log:info "Writing newlines" *written-newlines*)
    (loop repeat *written-newlines*
          do (pprint-newline :mandatory *output-stream*))))

(defmacro text-block ((&key
                         prefix
                         per-line-prefix)
                      &body body)
  (let ((pprint-options (append (when prefix
                                  (list :prefix prefix))
                                (when per-line-prefix
                                  (list :per-line-prefix per-line-prefix)))))
    `(progn
       (%write-newlines-if-needed)
       (pprint-logical-block (*output-stream* nil
                                              ,@pprint-options)
         ,@body))))

;; (defclass stream-with-newlines-counter (gray:fundamental-character-output-stream)
;;   ((original-stream :initarg :original-stream
;;                     :reader get-original-stream)))


;; (defmethod gray:stream-write-char ((stream stream-with-newlines-counter) char)
;;   (unless (char= char #\Newline)
;;     (log:info "Resetting to 0 in char")
;;     (setf *written-newlines* 0))
;;   (gray:stream-write-char (get-original-stream stream)
;;               char))


;; (defmethod gray:stream-write-string ((stream stream-with-newlines-counter) string &optional start end)
;;   (log:info "Writing string" (subseq string start end))
;;   (let* (;; (last-index (when (> end start)
;;          ;;               (- end 1)))
;;          ;; (last-char (when last-index
;;          ;;              (elt string last-index)))
;;          (num-newlines (loop :for idx
;;                              :downfrom (- (or end
;;                                               (length string))
;;                                           1)
;;                                :to (or start 0)
;;                              :for char = (elt string idx)
;;                              :unless (char= char #\Newline)
;;                                :do (return result)
;;                              :summing 1 :into result)))
    
;;     ;; (when (and last-char
;;     ;;            (not (char= last-char #\Newline)))
;;     ;;   (log:info "Resetting to 0 string")
;;     ;;   (setf *written-newlines* 0))
;;     (when num-newlines
;;       (log:info "Resetting newlines to" num-newlines)
;;       (setf *written-newlines* num-newlines)))
;;   (gray:stream-write-string (get-original-stream stream)
;;                             string
;;                             start
;;                             end))

;; (defmethod gray:stream-line-column ((stream stream-with-newlines-counter))
;;   (gray:stream-line-column (get-original-stream stream)))


;; (defun ensure-empty-line ()
;;   "Ensures there is a one empty line between previosly written block and the next one."
;;   (loop
;;     when (>= *written-newlines* 2)
;;       do (return)
         
;;     do (pprint-newline :mandatory *output-stream*)
;;        (incf *written-newlines*)
;;        (log:info "Incremented to" *written-newlines*)))


(defun ensure-newline ()
  "Ensures there is a one empty line between previosly written block and the next one."
  ;; (error "Need to really print before real write")
  (cond
    ((< *written-newlines* 1)
     ;; (pprint-newline :mandatory *output-stream*)
     (incf *written-newlines*)
     (log:info "Incremented to" *written-newlines*))
    (t (log:info "Newline already written" *written-newlines*)))
  ;; (when (> *written-newlines* 2)
  ;;   (pprint-newline :mandatory *output-stream*)
  ;;   (incf *written-newlines*)
  ;;   (log:info "Incremented to" *written-newlines*))
  )


(defun ensure-empty-line ()
  "Ensures there is a one empty line between previosly written block and the next one."
  ;; (error "Need to really print before real write")
  (when (< *written-newlines* 1)
    ;; (pprint-newline :mandatory *output-stream*)
    (incf *written-newlines*)
    (log:info "Incremented to" *written-newlines*))
  
  (when (< *written-newlines* 2)
    ;; (pprint-newline :mandatory *output-stream*)
    (incf *written-newlines*)
    (log:info "Incremented to" *written-newlines*)))


(defmethod serialize ((tag t) (node plump:nesting-node))
  (let ((children (plump:children node))
        (output-was-produced nil)
        (node-tag (get-node-tag node)))
    
    (loop with prev-node-was-block = t
          for idx below (length children)
          for child-node = (aref children idx)
          for child-node-tag = (get-node-tag child-node)
          for tag-should-be-skipped = (member child-node-tag *tags-to-remove*)
          
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
               (when (serialize child-node-tag
                                child-node)
                 (setf output-was-produced t)
                 (setf prev-node-was-block
                       (or (member child-node-tag *block-elements*)
                           ;; This is a special case because if code
                           ;; is nested inside pre, we are rendering it
                           ;; as a block
                           (and (eql node-tag :pre)
                                (eql child-node-tag :code)))))))
    (or (call-next-method)
        output-was-produced)))


(defun normalize-whitespaces (string &key
                                       (chars-to-trim '(#\Newline #\Space #\Tab))
                                       (trim-left t)
                                       (trim-right t))
  "Returns a string with multiple spaces replaced by one, optionally trimmed."
  (check-type string string)
  (check-type chars-to-trim list)
  (if *in-pre*
      (string-trim '(#\Newline)
                   string)
      ;; else
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
                  (write-char c stream))))))))


(defmethod serialize ((tag t) (node plump:text-node))
  (let* ((text (plump:text node))
         (normalized-text (normalize-whitespaces text)))

    (unless (string= normalized-text "")
      (log:info "Serializing text node" normalized-text)
      
      (text-block ()
        (write normalized-text))
      
      ;; We need to indicate that some value was written to the output
      (values t))))


(defmacro def-tag-serializer ((&rest tags) &body body)
  (let ((definitions (loop for tag in tags
                           collect `(defmethod serialize ((tag (eql ,tag)) node)
                                      (declare (ignorable node))
                                      ,@body))))
    `(progn ,@definitions)))


(def-tag-serializer (:b :strong)
  (write "**")
  (call-next-method)
  (write "**"))


(def-tag-serializer (:em :i :u)
  (write "_")
  (call-next-method)
  (write "_"))


(def-tag-serializer (:p)
  (text-block ()
    (call-next-method)
    (ensure-empty-line)
    ;; (pprint-newline :mandatory *output-stream*)
    ;; (pprint-newline :mandatory *output-stream*)
    ))


(defun get-list-bullet ()
  (case *list-style*
    (:ol
     (incf *list-number*)
     (format nil "~A. " *list-number*))
    (t "* ")))


(def-tag-serializer (:ul)
  (let ((*list-style* :ul))
    (call-next-method)
    (ensure-newline)
    ;; (pprint-newline :mandatory *output-stream*)
    ))


(def-tag-serializer (:ol)
  (let ((*list-style* :ol)
        (*list-number* 0))
    (ensure-empty-line)
    (call-next-method)
    (ensure-empty-line)))


(def-tag-serializer (:li)
  (let ((prefix (get-list-bullet)))
    (text-block (:prefix prefix)
      (call-next-method)
      (pprint-indent :block (- (length prefix)) *output-stream*)
      (ensure-newline))))


(def-tag-serializer (:a)
  (let ((url (or (plump:attribute node "href")
                 "")))
    (write "[")
    (call-next-method)
    (write "](" url ")")))


(def-tag-serializer (:blockquote)
  (text-block (:per-line-prefix "> ")
    (call-next-method)))


(def-tag-serializer (:hr)
  (ensure-empty-line)
  (write "***")
  (ensure-empty-line)
  (values t))


(def-tag-serializer (:img)
  (let ((url (or (plump:attribute node "src")
                 "")))
    (write "![](" url ")"))
  (values t))


(def-tag-serializer (:code)
  (cond
    (*in-pre*
     (ensure-empty-line)
     
     (text-block ()
       (write "```"))

     (break)
     (ensure-newline)
     (break)
     
     (text-block ()
       (call-next-method))
     
     (ensure-newline)
     
     (text-block ()
       (write "```"))
     
     (ensure-empty-line))
    
    (t
     (write "`")
     (call-next-method)
     (write "`")))
  ;; We need to indicate that we've wrote some output
  ;; by returning t
  (values t))


(def-tag-serializer (:pre)
  (let ((*in-pre* t))
    (call-next-method)))


(defmethod serialize :around (tag node)
  (log:debug "Serializing" tag node)
  (call-next-method))


(defun html2text (text)
  (check-type text string)
  (let* ((document (plump:parse text))
         (*print-pretty* t)
         (*written-newlines* 0))
    (with-output-to-string (*output-stream*)
      
      (pprint-logical-block (*output-stream* nil)
        (serialize nil document)))))
