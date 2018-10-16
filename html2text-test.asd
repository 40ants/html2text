(defsystem html2text-test
           :author "Alexander Artemenko"
           :license "BSD"
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:html2text
                        "html2text-test/core"
                        "html2text-test/link-revealer")
           :description "Test system for html2text"

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
