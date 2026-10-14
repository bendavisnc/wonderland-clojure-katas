find . -iname '*.clj' | entr sh -c "./format.sh ; ./test.sh"
