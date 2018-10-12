(defsystem html2text-link-revealer
           :version (:read-file-form "version.lisp-expr")
           :author "Alexander Artemenko"
           :license "BSD"
           :class :package-inferred-system
           :pathname "link-revealer"
           :depends-on ("html2text-link-revealer/utils")
           :description "Reveals original URL going throgh all redirects.")

