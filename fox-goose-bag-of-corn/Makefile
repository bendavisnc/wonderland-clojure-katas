auto:
	find . -iname "*.clj" | entr clojure -X:test 

format:
	@echo "Formatting clj."
	zprint -w src/**/*.clj test/**/*.clj
