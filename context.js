"use strict";

var parser = require('./parser');

var Context = function Context() {
	this.globalenv = Object.create(null);

	for (var k in builtins) {
		this.attach(k, builtins[k]);
	}
};

Context.prototype.attach = function attach(name, fn) {
	this.globalenv[name] = {
		type: "function",
		name: name,
		fn: fn,
	};
};

Context.prototype.exec = function(expr, env) {
	if (typeof expr === "string") {
		expr = parser.parse(expr);
	}

	if (expr.type === "string" || expr.type === "number" || expr.type === "boolean") {
		return expr;
	}

	if (!env) {
		env = this.globalenv;
	}

	if (expr.type === "symbol") {
		if (expr.eval === false) {
			return expr;
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
			throw new Error("the argument list parameter of a defun statement must be a list");
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

		var res = {
			type: "function",
			name: fname.name,
			fn: fn,
		};

		env[fname.name] = res;

		return res;
	},

	'if': function(args, env) {
		var res = {
			type: "boolean",
			value: false,
		};

		if (this.exec(args[0], env).value !== false) {
			res = this.exec(args[1], env);
		}	else if (args[2]) {
			res = this.exec(args[2], env);
		}

		return res;
	},

	'setq': function(args, env) {
		var res = null;

		for (var i = 0; i < args.length; i += 2) {
			if (args[i].type !== "symbol") {
				throw new Error("tried to assign to a non-symbol in setq");
			}

			res = env[args[i].name] = this.exec(args[i+1], env);
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
		var res = this.exec(args[0], env).value;

		for (var i = 1; i < args.length; i++) {
			res += this.exec(args[i], env).value;
		}

		return {type: "number", value: res};
	},

	'-': function(args, env) {
		var res = this.exec(args[0], env).value;

		for (var i = 1; i < args.length; i++) {
			res -= this.exec(args[i], env).value;
		}

		return {type: "number", value: res};
	},

	'*': function(args, env) {
		var res = 1;

		for (var i = 0; i < args.length; i++) {
			res *= this.exec(args[i], env).value;
		}

		return {type: "number", value: res};
	},

	'/': function(args, env) {
		var res = this.exec(args[0], env).value;

		for (var i = 1; i < args.length; i++) {
			res /= this.exec(args[i], env).value;
		}

		return {type: "number", value: res};
	},

	// gonometry
	'cos': function(args, env) {
		return {
			type: "number",
			value: Math.cos(this.exec(args[0], env).value),
		};
	},

	'sin': function(args, env) {
		return {
			type: "number",
			value: Math.sin(this.exec(args[0], env).value),
		};
	},

	'tan': function(args, env) {
		return {
			type: "number",
			value: Math.tan(this.exec(args[0], env).value),
		};
	},

	// comparison
	'<=': function(a, env) {
		return {
			type: "boolean",
			value: this.exec(a[0], env).value <= this.exec(a[1], env).value,
		};
	},
	'<': function(a, env) {
		return {
			type: "boolean",
			value: this.exec(a[0], env).value < this.exec(a[1], env).value,
		};
	},
	'>=': function(a, env) {
		return {
			type: "boolean",
			value: this.exec(a[0], env).value >= this.exec(a[1], env).value,
		};
	},
	'>': function(a, env) {
		return {
			type: "boolean",
			value: this.exec(a[0], env).value > this.exec(a[1], env).value,
		};
	},
	'=': function(a, env) {
		return {
			type: "boolean",
			value: this.exec(a[0], env).value == this.exec(a[1], env).value,
		};
	},
	'eq': function(a, env) {
		return {
			type: "boolean",
			value: this.exec(a[0], env).value === this.exec(a[1], env).value,
		};
	},

	// logical
	'not': function(args, env) {
		return {
			type: "boolean",
			value: !this.exec(args[0], env).value,
		};
	},

	'and': function(args, env) {
		var res = {
			type: "boolean",
			value: false,
		};

		for (var i = 0; i < args.length; i++) {
			res = this.exec(args[i], env);

			if (res.type === "boolean" && res.value === false) {
				return res;
			}
		}

		return res;
	},

	'or': function(args, env) {
		var res = {
			type: "boolean",
			value: false,
		};

		for (var i = 0; i < args.length; i++) {
			res = this.exec(args[i], env);

			if (res.type !== "boolean" || res.value !== false) {
				return res;
			}
		}

		return res;
	},

	// stuff

	'list': function(args, env) {
		var self = this;

		var res = [];
		for (var i=0;i<args.length;i++) {
			res.push(self.exec(args[i], env));
		}

		return res;
	},

	'progn': function(args, env) {
		var _args = {
			type: "list",
			cells: [{type: "symbol", eval: true, name: "list"}].concat(args),
		};

		var res = this.exec(_args, env);

		return res.length ? res[res.length-1] : false;
	},

	'print': function(args, env) {
		var _args = {
			type: "list",
			cells: [{type: "symbol", eval: true, name: "progn"}].concat(args),
		};

		var res = this.exec(_args, env);

		console.log(res);

		return res;
	},
};

module.exports = Context;
