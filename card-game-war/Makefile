auto:
	find . -iname "*.clj" | entr make test

test: format
	clojure -X:test 

format:
	@echo "Formatting clj."
	zprint '{:style :community, :parse {:interpose "\n\n"}}' -w src/**/*.clj test/**/*.clj


