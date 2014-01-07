(module r7rs-compile-time (parse-library-definition
			   define-extended-arity-comparator
			   process-cond-expand
			   fixup-import/export-spec
			   parse-library-name
			   import-transformer
			   read-forms
			   register-r7rs-module
			   locate-library)

(import scheme chicken)

(include "r7rs-compile-time.scm")

)
