
test: test-node test-phantom

test-node:
	@./node_modules/mocha/bin/mocha test/parse.js test/exec.js

test-server:
	@node test/server.js

test-phantom:
	@phantomjs test/phantom.js http://localhost:8080

build:
	@./node_modules/.bin/wrup -o build.js -r ./LISP.js

.PHONY: test
