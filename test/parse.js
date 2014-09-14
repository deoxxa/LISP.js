"use strict";

var parser = require('../parser');
var expect = require('expect.js');

var tests = {
	'(+ 1 3)': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":1},{"type":"number","value":3}]},
	'(+ (- 3 2) 4)': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"-"},{"type":"number","value":3},{"type":"number","value":2}]},{"type":"number","value":4}]},
	'(+ (- (+ 2 1) 2) 4)': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"-"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":2},{"type":"number","value":1}]},{"type":"number","value":2}]},{"type":"number","value":4}]},
	'(+ (- (+ 2 1) (+ 1 1)) 4)': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"-"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":2},{"type":"number","value":1}]},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":1},{"type":"number","value":1}]}]},{"type":"number","value":4}]},
	'(+ (- (+ 2 1) 5 (+ 1 1)) 4)': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"-"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":2},{"type":"number","value":1}]},{"type":"number","value":5},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":1},{"type":"number","value":1}]}]},{"type":"number","value":4}]},
	'(   +  4  2)': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"+"},{"type":"number","value":4},{"type":"number","value":2}]},
	'(defun factorial (N) (if (= N 1) 1 (* N (factorial (- N 1)))))': {"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"defun"},{"type":"symbol","eval":true,"name":"factorial"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"N"}]},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"if"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"="},{"type":"symbol","eval":true,"name":"N"},{"type":"number","value":1}]},{"type":"number","value":1},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"*"},{"type":"symbol","eval":true,"name":"N"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"factorial"},{"type":"list","eval":true,"cells":[{"type":"symbol","eval":true,"name":"-"},{"type":"symbol","eval":true,"name":"N"},{"type":"number","value":1}]}]}]}]}]},
};

describe('parser', function() {
	for (var code in tests) {
    (function(code, expected) {
  		it('should parse `' + code + '`', function() {
  			expect(parser.parse(code)).to.eql(expected);
  		});
  	})(code, tests[code]);
  }
});
