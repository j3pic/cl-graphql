;;; This file is meant as an alternative to the Javaesque morass that is
;;; parser.lisp. Rather than writing a CLOS class for each and every of the
;;; very numerous grammar classes, *THE-GRAMMAR* is a direct translation of
;;; the GraphQL spec, from which it may be possible to programmatically
;;; generate the parser.
;;;
;;; What the output of the parser should be is an open question. Should
;;; it represent GraphQL as some kind of S-expression language, or should
;;; it generate a proper parse tree using CLOS objects?
;;;
;;; Some of the definitions are recursive, so care must be taken when
;;; writing the parser to avoid infinite loops. I ran into that problem
;;; in another project where I tried to write a parser that interprets
;;; a grammar.

(in-package :cl-graphql)

(defparameter *the-grammar*
  '((document definition)
    (definition
     executable-definition
     type-system-definition
     type-system-extension)
    (executable-definition
     operation-definition
     fragment-definition)
    (operation-definition
     selection-set
     (operation-type (&optional name) (&optional variable-definitions) (&optional directives) selection-set))
    (operation-type
     (&one-of
      (:name "query")
      (:name "mutation")
      (:name "subscription")))
    (selection-set
     ((:punctuator "{") (&list selection) (:punctuator "}")))
    (selection
     field
     fragment-spread
     inline-fragment)
    (field
     ((&optional alias) name (&optional arguments) (&optional directives) (&optional selection-set)))
    (alias
     (name (:punctuator ":")))
    (arguments
     ((:punctuator "(") (&list argument) (:punctuator ")")))
    (argument
     (name (:punctuator ":") value))
    (fragment-spread
     ((:punctuator "...") fragment-name (&optional directives)))
    (inline-fragment
     ((:punctuator "...") (&optional type-condition) (&optional directives) selection-set))
    (fragment-name
     ((&but-not name ("on"))))
    (type-condition
     ((:name "on") named-type))
    (value
     variable
     int-value
     float-value
     string-value
     boolean-value
     null-value
     enum-value
     list-value
     object-value)
    (boolean-value
     (&one-of
      (:name "true")
      (:name "false")))
    (null-value
     (:name "null"))
    (enum-value
     (&but-not name ("true" "false" "null")))
    (list-value
     ((:punctuator "[") (:punctuator "]"))
     ((:punctuator "[") (&list value) (:punctuator "]")))
    (object-value
     ((:punctuator "{") (:punctuator "}"))
     ((:punctuator "{") (&list object-field) (:punctuator "}")))
    (object-field
     (name (:punctuator ":") value))
    (variable-definitions
     ((:punctuator "(") (&list variable-definition) (:punctuator ")")))
    (variable-definition
     (variable (:punctuator ":") type (&optional default-value)))
    (variable
     ((:punctuator "$") name))
    (default-value
     ((:punctuator "=") value))
    (type
     named-type
     list-type
     non-null-type)
    (named-type
     name)
    (list-type
     ((:punctuator "[") type (:punctuator "]")))
    (non-null-type
     (named-type (:punctuator "!"))
     (list-type (:punctuator "!")))
    (directives
     (&list directive))
    (directive
     ((:punctuator "@") name (&optional arguments)))
    (type-system-definition
     schema-definition
     type-definition
     directive-definition)
    (type-system-extension
     schema-extrnsion
     type-extension)
    (schema-definition
     ((:name "schema") (&optional directives)
      (:punctuator "{") (&list operation-type-definition) (:punctuator "}")))
    (schema-extension
     ((:name "extend") (:name "schema") (&optional directives) (:punctuator "{") (&list operation-type-definition) (:punctuator "}"))
     ((:name "extend") (:name "schema") directives))
    (operation-type-definition
     (operation-type (:punctuator ":") named-type))
    (description
     string-value)
    (type-definition
     scalar-type-definition
     object-type-definition
     interface-type-definition
     union-type-definition
     enum-type-definition
     input-object-type-definition)
    (type-extension
     scalar-type-extension
     object-type-extension
     interface-type-extension
     union-type-extension
     enum-type-extension
     input-object-type-extension)
    (scalar-type-definition
     ((&optional description) (:name "scalar") name (&optional directives)))
    (scalar-type-extension
     ((:name "extend") (:name "scalar") name directives))
    (object-type-definition
     (&optional description) (:name "type") name (&optional implements-interfaces)
     (&optional directives) (&optional fields-definition))
    (object-type-extension
     ((:name "extend") (:name "type") name (&optional implements-interfaces) (&optional directives) fields-definition)
     ((:name "extend") (:name "type") name (&optional implements-interfaces) directives)
     ((:name "extend") (:name "type") name implements-interfaces))
    (implements-interfaces
     ((:name "implements") (&optional (:punctuator "&")) named-type)
     (implements-interfaces (:punctuator "&") named-type))            ;;;;; WARNING!!!!! This definition is recursive.
    (fields-definition
     ((:punctuator "{") (&list field-definition) (:punctuator "}")))
    (field-definition
     ((&optional description) name (&optional arguments-definition) (:punctuator ":") type (&optional directives)))
    (arguments-definition
     ((:punctuator "(") (&list input-value-definition) (:punctuator ")")))
    (input-value-definition
     ((&optional description) name (:punctuator ":") type (&optional default-value) (&optional directives)))
    (interface-type-extension
     ((:name "extend") (:name "interface") name (&optional directives) fields-definition)
     ((:name "extend") (:name "interface") name directives))
    (union-type-definition
     ((:name "extend") (:name "union") name (&optional directives) union-member-types)
     ((:name "extend") (:name "union") name directives))
    (enum-type-definition
     ((&optional description) (:name "enum") (&optional directives) (&optional enum-values-definition)))
    (enum-values-definition
     ((:punctuator "{") (&list enum-value-definition) (:punctuator "}")))
    (enum-value-definition
     ((&optional description) enum-value (&optional directives)))
    (enum-type-extension
     ((:name "extend") (:name "enum") (&optional directives) enum-values-definition)
     ((:name "extend") (:name "enum") name directives))
    (input-object-type-definition
     ((&optional description) (:name "input") (&optional directives) (&optional input-fields-definition)))
    (input-fields-definition
     ((:punctuator "{") (&list input-value-definition) (:punctuator "}")))
    (input-object-type-extension
     ((:name "extend") (:name "input") name (&optional directives) input-fields-definition)
     ((:name "extend") (:name "input") name directives))
    (directive-definition
     ((&optional decription) (:name "directive") (:punctuator "@") name (&optional arguments-definition) (:name "on") directive-locations))

    ;; This one is particularly tricky. A backtracking parser would recurse forever on the second case.
    ;; Instead, a greedy lookahead algorithm is required. 
    
    (directive-locations
     ((&optional (:punctuator "|")) directive-location)
     (directive-locations (:punctuator "|") directive-location))

    
    (directive-location
     executable-directive-location
     type-system-directive-location)
    (executable-directive-location
     (:name "QUERY")
     (:name "MUTATION")
     (:name "SUBSCRIPTION")
     (:name "FIELD")
     (:name "FRAGMENT_DEFINITION")
     (:name "FRAGMENT_SPREAD")
     (:name "INLINE_FRAGMENT"))
    (type-system-directive-location
     (:name "SCHEMA")
     (:name "SCALAR")
     (:name "OBJECT")
     (:name "FIELD_DEFINITION")
     (:name "ARGUMENT_DEFINITION")
     (:name "INTERFACE")
     (:name "UNION")
     (:name "ENUM")
     (:name "ENUM_VALUE")
     (:name "INPUT_OBJECT")
     (:name "INPUT_FIELD_DEFINITION")))
  "This is the complete grammar of GraphQL, translated from the spec. The syntax used here is:

    ((class &rest definition) &rest more-classes)

Each CLASS corresponds to some grammar class in §B.2 of the specification. The DEFINITION is a list 
representing alternative forms that the CLASS can take.

The parser should use the CHOOSE operator to work its way down the grammar. If the text cannot
be parsed as the class currently under consideration, then the FAIL restart should be invoked,
causing the next class to be attempted, except if the failure can be intelligently identified
as being a programming error. In that case, an error should be raised so that the parser
can provide meaningful error messages instead of merely 'unable to parse this, bro!'

The structure of each alternative can take on one of several structures:

    OTHER-CLASS

In this case, when we try to parse a CLASS, we should start by parsing OTHER-CLASS

  (:TOKEN-TYPE \"string\")

In this case, :TOKEN-TYPE should match one of the token types that may be found in a token
stream. Only :NAME and :PUNCTUATOR occur in the grammar. 

The string is to be matched against the token object, after remembering that in the token
stream, either lists or lazy streams will appear instead of strings, and they are likely
to be wrapped in LINE-COUNTED objects

  (TOKEN1 TOKEN2 ... TOKENn)

This form specifies that a sequence of tokens will be expected. These can be tokens or classes.

  (&but-not TOKEN-OR-CLASS (S1 S2 .. Sn))

This specifies that the TOKEN-OR-CLASS will be rejected if its string representation
matches any of S1 .. Sn.

  (&MODIFIER token-or-class)

This form specifies that the TOKEN-OR-CLASS is to be read, but with some caveat. The &MODIFIER
may be:

   &LIST - A list of at least one element conforming to the TOKEN-OR-CLASS is expected.
   &OPTIONAL - The element may not appear at this point in the stream.
   &ONE-OF - The element must be one of the tokens or classes given in this form.
")
