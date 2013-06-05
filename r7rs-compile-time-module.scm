(module r7rs-compile-time (parse-library-definition
			   process-cond-expand
			   fixup-import/export-spec
			   parse-library-name
			   read-forms
			   current-source-filename
			   register-r7rs-module
			   locate-library)

(import scheme chicken)

(include "r7rs-compile-time.scm")

)
