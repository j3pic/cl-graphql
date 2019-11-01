;; Based on the GraphQL Specification at: https://graphql.github.io/graphql-spec/June2018/

(asdf:defsystem :cl-graphql
    :author ("Jeremy Phelps")
    :version "1"
    :license "Affero GNU General Public License"
    :description "A GraphQL Engine for Common Lisp"
    :depends-on (:closer-mop :moptilities :alexandria :split-sequence)
    :components
    ((:file "line-counter" :depends-on ("lazy-streams" "better-handler-case" "packages"))
     (:file "tokens" :depends-on ("line-counter" "utils"))
     (:file "lazy-streams" :depends-on ("packages"))
     (:file "better-handler-case" :depends-on ("packages"))
     (:file "utils" :depends-on ("packages"))
     (:file "parser" :depends-on ("tokens" "line-counter" "utils"))
     (:file "packages")))
