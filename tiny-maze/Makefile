auto:
	find . -iname "*.clj" | entr make test

test: format
	clojure -X:test 
# 	clojure -m tiny-maze.solver

format:
	@echo "Formatting clj."
	zprint '{:style [:community :respect-nl :justified]}' -w src/**/*.clj test/**/*.clj


