"use strict";

var parse = require('./parse');

var Context = function Context() {
	this.procedures = {};
};

Context.prototype.exec = function(expr, env) {
	if (typeof expr == 'string' && expr.charAt(0) == '(') {
		expr = parse(expr);
	} else if (typeof expr == 'number') {
		return expr;
	} else if (!Array.isArray(expr)) {
		expr = [expr];
	}

	if (!env) {
		env = Object.create(atoms);
	}

	var args = [];
	for (var i = 0; i < expr.length; i++) {
		if (expr[i] in env) {
			args.push(env[expr[i]]);
		}	else {
			args.push(expr[i]);
		}
	}

	var fn = args[0];
	args = args.slice(1);

	if (this.procedures.hasOwnProperty(fn)) {
		return this.procedures[fn].call(this, args, env);
	} else if (procedures.hasOwnProperty(fn)) {
		return procedures[fn].call(this, args, env);
	} else {
		return fn;
	}

	return false;
};

var atoms = {
	'nil': false,
	't': true,
};

var procedures = {
	'defun': function(args) {
		var fn = args[0], a = args[1], body = args.slice(2);

		this.procedures[fn] = function(_args, env) {
			var _env = Object.create(env), res = false;

			for (var i = 0; i < a.length; i++) {
				_env[a[i]] = this.exec(_args[i], _env);
			}

			for (var j = 0; j < body.length; j++) {
				res = this.exec(body[j], _env);
			}

			return res;
		};

		return fn;
	},

	'if': function(args, env) {
		var condition = this.exec(args[0], env), res = false;

		if (condition !== false) {
			res = this.exec(args[1], env);
		}	else if (args[2]) {
			res = this.exec(args[2], env);
		}

		return res;
	},

	'setq': function(args, env) {
		var res = false;

		for (var i = 0; i < args.length; i += 2) {
			res = env[args[i]] = this.exec(args[i + 1], env);
		}

		return res;
	},

	'let': function(args, env) {
		env[this.exec(args[0][0][0], env)] = this.exec(args[0][0][1], env);

		return this.exec(args[1], env);
	},

	'+': function(args, env) {
		var res = 0;

		for (var i = 0; i < args.length; i++) {
			res += this.exec(args[i], env);
		}

		return res;
	},

	'-': function(args, env) {
		var res = this.exec(args[0], env);

		for (var i = 1; i < args.length; i++) {
			res -= this.exec(args[i], env);
		}

		return res;
	},

	'*': function(args, env) {
		var res = 1;

		for (var i = 0; i < args.length; i++) {
			res *= this.exec(args[i], env);
		}

		return res;
	},

	'/': function(args, env) {
		var res = this.exec(args[0], env);

		for (var i = 1; i < args.length; i++) {
			res /= this.exec(args[i], env);
		}

		return res;
	},

	// gonometry
	'cos': function(args, env) {
		return Math.cos(this.exec(args[0]), env);
	},

	'sin': function(args, env) {
		return Math.sin(this.exec(args[0]), env);
	},

	'tan': function(args, env) {
		return Math.tan(this.exec(args[0]), env);
	},

	// comparison
	'<=': function(a, env) { return this.exec(a[0], env) <=  this.exec(a[1], env); },
	'<':  function(a, env) { return this.exec(a[0], env) <   this.exec(a[1], env); },
	'>=': function(a, env) { return this.exec(a[0], env) >=  this.exec(a[1], env); },
	'>':  function(a, env) { return this.exec(a[0], env) >   this.exec(a[1], env); },
	'=':  function(a, env) { return this.exec(a[0], env) ==  this.exec(a[1], env); },
	'eq': function(a, env) { return this.exec(a[0], env) === this.exec(a[1], env); },

	// logical
	'not': function(args, env) {
		return !this.exec(args[0], env);
	},

	'and': function(args, env) {
		var res = false;

		for (var i = 0; i < args.length; i++) {
			if ((res = this.exec(args[i], env)) === false) {
				return false;
			}
		}

		return res;
	},

	'or': function(args, env) {
		var res = false;

		for (var i = 0; i < args.length; i++) {
			if ((res = this.exec(args[i], env)) !== false) {
				return res;
			}
		}

		return false;
	},

	// stuff

	'list': function(args, env) {
		for (var i = 0; i < args.length; i++) {
			args[i] = this.exec(args[i], env);
		}

		return args;
	},

	'progn': function(args, env) {
		var res = this.exec(['list'].concat(args), env);

		return res.length ? res[res.length - 1] : false;
	},

	'print': function(args, env) {
		var res = this.exec(['progn'].concat(args), env);

		console.log(res);

		return res;
	},
};

module.exports = Context;
