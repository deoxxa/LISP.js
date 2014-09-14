"use strict";

var parser = require('./parser');

var Context = function Context() {
	this.procedures = Object.create(builtins);
	this.globalenv = Object.create(null);
};

Context.prototype.exec = function(expr, env) {
	if (typeof expr === "string") {
		expr = parser.parse(expr);
	}

	if (expr.type === "string" || expr.type === "number" || expr.type === "boolean") {
		return expr.value;
	}

	// if ((expr.type === "symbol" || expr.type === "list") && (expr.eval === false)) {
	// 	return expr;
	// }

	if (expr.type === "symbol") {
		if (expr.eval === false) {
			return expr;
		}

		if (expr.name in this.procedures) {
			return {
				type: "function",
				fn: this.procedures[expr.name],
			};
		}

		if (expr.name in env) {
			return env[expr.name];
		}

		throw new Error("tried to access undefined symbol `" + expr.name + "'");
	}

	if (expr.type !== "list") {
		throw new Error("invalid expression type: " + expr.type);
	}

	if (expr.eval === false) {
		return expr.cells;
	}

	if (!env) {
		env = this.globalenv;
	}

	var fn = this.exec(expr.cells[0], env),
	    args = expr.cells.slice(1);

	if (typeof fn !== "object" || fn.type !== "function") {
		throw new Error("tried to call a non-function");
	}

	return fn.fn.call(this, args, env);
};

var builtins = {
	'defun': function(args, env) {
		var fname = args[0],
				fargs = args[1],
				fbody = args.slice(2);

		if (typeof fname !== "object" || fname.type !== "symbol") {
			throw new Error("the name parameter of a defun statement must be a symbol");
		}

		if (typeof fargs !== "object" || fargs.type !== "list") {
			throw new error("the argument list parameter of a defun statement must be a list");
		}

		for (var i=0;i<fargs.cells.length;i++) {
			if (typeof fargs.cells[i] !== "object" || fargs.cells[i].type !== "symbol") {
				throw new Error("all elements of a defun argument list must be symbols");
			}
		}

		var fn = function(_args, env) {
			var _env = Object.create(env),
					res = false;

			for (var i = 0; i < fargs.cells.length; i++) {
				_env[fargs.cells[i].name] = this.exec(_args[i], _env);
			}

			for (var j = 0; j < fbody.length; j++) {
				res = this.exec(fbody[j], _env);
			}

			return res;
		};

		env[fname.name] = {
			type: "function",
			fn: fn,
		};

		return fname.name;
	},

	'if': function(args, env) {
		var condition = this.exec(args[0], env),
				res = false;

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
			if (typeof args[i] !== "object" || args[i].type !== "symbol") {
				throw new Error("tried to assign to a non-symbol in setq");
			}

			res = env[args[i].name] = this.exec(args[i + 1], env);
		}

		return res;
	},

	'let': function(args, env) {
		var sym = this.exec(args[0].cells[0].cells[0], env);

		if (typeof sym !== "object" || sym.type !== "symbol") {
			throw new Error("tried assign something to a non-symbol");
		}

		env[sym.name] = this.exec(args[0].cells[0].cells[1], env);

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
		var self = this;

		return args.map(function(e) {
			return self.exec(e, env);
		});
	},

	'progn': function(args, env) {
		var _args = {
			type: "list",
			cells: [{type: "symbol", eval: true, name: "list"}].concat(args),
		};

		var res = this.exec(_args, env);

		return res.length ? res[res.length - 1] : false;
	},

	'print': function(args, env) {
		var res = this.exec(['progn'].concat(args), env);

		console.log(res);

		return res;
	},
};

module.exports = Context;
