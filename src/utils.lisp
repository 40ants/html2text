(defpackage #:html2text/utils
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:export
   #:write-pretty-string
   #:clean-string))
(in-package html2text/utils)


;; (defun write-pretty-string (string stream &key
;;                                             (start 0)
;;                                             (end (length string)))
;;   "Writes string to a pretty stream, adding
;;    a mandatory newline after each line."
;;   (loop with sub-end = nil
;;         with next-newline = nil
;;         do (setq next-newline
;; 		 (position #\Newline string :start start
;;                                             :end end
;;                                             :test #'eq ))
;; 	   (setq sub-end (if next-newline
;;                              next-newline
;;                              end))
;; 	   (write-string string
;;                          stream
;;                          :start start
;;                          :end sub-end)
;; 	   (when (null next-newline)
;;              (return nil))
;; 	   (pprint-newline :mandatory stream)
;;            ;; Now we need to skip newline and all spaces
;;            ;; because all subsequent lines should be
;;            ;; indented equally.
;; 	   (setq start (+ 1 sub-end))
;;            (loop for i from start below end
;;                  unless (member (aref string i)
;;                                 '(#\Space #\Tab)
;;                                 :test #'char=)

;;                    do (return)
;;                  do (incf start))))


;; (defun clean-string (string)
;;   "Trims spaces and newlines from the ends of the string.
;;    Replaces multiple newlines and spaces with one space."
;;   (string-trim '(#\Newline #\Space)
;;                (regex-replace-all "[ \\n]{2,}" string " ")))


;; Examples
;; 
;; (defun pprint-foo (stream)
;;   (pprint-logical-block (stream nil :prefix "")
;;     (loop for i from 1 upto 5
;;           do (pprint-logical-block (stream nil :prefix "+ ")
;;                (write-pretty-string (format nil "foo ~A~%With second line" i) stream)
;;                (pprint-indent :block -2 stream)
;;                (pprint-newline :mandatory stream)))))


;; (princ (let ((*print-pretty* t))
;;    (with-output-to-string (s)
;;      (pprint-logical-block (s nil)
;;        (pprint-logical-block (s nil)
;;          (loop for i from 1 upto 3
;;                do                             
;;                   (pprint-logical-block (s nil :prefix "- ")
;;                     (format s "Item ~A" i) 
;;                     (when (equal i 2) (pprint-newline :mandatory s)
;;                           (pprint-foo s)) (pprint-indent :block -2 s)
;;                           (pprint-newline :mandatory s))))))))

;; It's output is:
;;
;; - Item 1
;; - Item 2
;;   + foo 1
;;     With second line
;;   + foo 2
;;     With second line
;;   + foo 3
;;     With second line
;;   + foo 4
;;     With second line
;;   + foo 5
;;     With second line

;; - Item 3
