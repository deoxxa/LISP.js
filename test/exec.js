"use strict";

var Context = require('../context');
var expect = require('expect.js');

var tests = {
	// basic calculations
	'(+ 1 3)': {type: "number", value: 4},
	'(+ (- 3 2) 4)': {type: "number", value: 5},
	'(+ (- (+ 2 1) 2) 4)': {type: "number", value: 5},
	'(+ (- (+ 2 1) (+ 1 1)) 4)': {type: "number", value: 5},
	'(+ (- (+ 2 1) 5 (+ 1 1)) 4)': {type: "number", value: 0},
	'(/ 12 4)': {type: "number", value: 3},
	'(* 3 4)': {type: "number", value: 12},

	// gonometry
	'(* 2 (cos 0) (+ 4 6))': {type: "number", value: 20},
	'(tan 0)': {type: "number", value: Math.tan(0)},
	'(sin 0)': {type: "number", value: Math.sin(0)},

	// comparison
	'(<= 3 3)': {type: "boolean", value: true},
	'(<= 2 3)': {type: "boolean", value: true},
	'(<= 3 2)': {type: "boolean", value: false},
	'(<  3 3)': {type: "boolean", value: false},
	'(<  2 3)': {type: "boolean", value: true},
	'(<  3 2)': {type: "boolean", value: false},
	'(>= 3 3)': {type: "boolean", value: true},
	'(>= 2 3)': {type: "boolean", value: false},
	'(>= 3 2)': {type: "boolean", value: true},
	'(>  3 3)': {type: "boolean", value: false},
	'(>  2 3)': {type: "boolean", value: false},
	'(>  3 2)': {type: "boolean", value: true},
	'(=  3 3)': {type: "boolean", value: true},
	'(=  2 3)': {type: "boolean", value: false},
	'(=  3 2)': {type: "boolean", value: false},
	'(eq 3 3)': {type: "boolean", value: true},
	'(eq 2 3)': {type: "boolean", value: false},
	'(eq 3 2)': {type: "boolean", value: false},

	// atoms
	'#t': {type: "boolean", value: true},
	'#f': {type: "boolean", value: false},

	// logic
	'(not #f)': {type: "boolean", value: true},
	'(and #f #t)': {type: "boolean", value: false},
	'(or #f #t)': {type: "boolean", value: true},

	// comparison
	'(if (> 3 1) 1 2)': {type: "number", value: 1},
	'(if (> 1 3) 1 2)': {type: "number", value: 2},
	'(if (> 1 3) 1)': {type: "boolean", value: false},
	'(if #t 3 4)': {type: "number", value: 3},
	'(if #f 3 4)': {type: "number", value: 4},
	'(if 3 4 5)': {type: "number", value: 4},

	// set variables
	'(and (setq \'a 5) a)': {type: "number", value: 5},
	'(let ((\'x 10)) x)': {type: "number", value: 10},
	'(let ((\'x (+ 10 2))) (- x 4))': {type: "number", value: 8},

	// functions
	'(defun foo (n) n)': {type: "function", name: "foo"},
	'(foo 4)': {type: "number", value: 4},
	'(defun bar (n m) (+ n m))': {type: "function", name: "bar"},
	'(bar 4 5)': {type: "number", value: 9},

	// recursive functions
	'(defun fib (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)) ) ))': {type: "function", name: "fib"},
	'(fib 5)': {type: "number", value: 8},

	'\
(defun factorial (N)\n\
  (if (<= N 1)\n\
    1\n\
    (* N (factorial (- N 1)))\n\
  )\n\
)\n\
': {type: "function", name: "factorial"},
	'(factorial 4)': {type: "number", value: 24},

	// stuffz
	'(list 3 4 5)': [{type: "number", value: 3}, {type: "number", value: 4}, {type: "number", value: 5}],
	'(progn 4 5)': {type: "number", value: 5},
	'(progn (setq a (+ 5 7)) (setq b (+ a 8)))': {type: "number", value: 20},

	// attached function
	'(wtf 5 6)': {type: "number", value: 30},
};

describe('execute', function(){
	var ctx = new Context();

	ctx.attach("wtf", function(args, env) {
		if (args.length < 2) {
			throw new Error("not enough arguments for `wtf'");
		}

		var a1 = this.exec(args[0], env);
		if (a1.type !== "number") {
			throw new Error("invalid type for first argument to `wtf'");
		}

		var a2 = this.exec(args[1], env);
		if (a2.type !== "number") {
			throw new Error("invalid type for second argument to `wtf'");
		}

		return {type: "number", value: a1.value * a2.value};
	});

	for (var code in tests) (function(code, expected){
		it('should evaluate `' + code + '` to ' + JSON.stringify(expected), function(){
			expect(ctx.exec(code)).to.eql(expected);
		});
	})(code, tests[code]);
});
