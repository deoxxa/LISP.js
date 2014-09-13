LISP.js
=======

[![Build Status](https://secure.travis-ci.org/arian/LISP.js.png)](http://travis-ci.org/arian/LISP.js)

Parse and execute LISP code in JavaScript.

API
---

For parsing the code, the `parse` function can be used.
This will return an array with all pieces.

``` js
parse('(+ 2 3)'); // ['+', 2, 3]
```

Executing code can be done with the `Context` object:

``` js
var ctx = new Context();
ctx.exec('(+ 3 (- 10 5))'); // 8
```

LISP.js in Node.js
------------------

```js
var Context = require('LISP.js').Context;
var ctx = new Context();
ctx.exec('(+ 1 2)');
```

### Install with NPM

You can install LISP.js with npm:

```
npm install LISP.js
```

or add LISP.js to your `package.json` dependencies:

```js
	"dependencies": {
		"LISP.js": ">=0.0.6"
	}
```

LISP.js in the Browser
----------------------

Building LISP.js for the browser is easy with [wrapup](https://github.com/kamicane/wrapup).

	make build

This command will create a JS file that will export a global `LISP` variable
with `LISP.Context` and `LISP.parse()`.

Alternatively you could use this to have global `parse` and `exec` functions.

	wrup -r parse ./parse -r exec ./exec
