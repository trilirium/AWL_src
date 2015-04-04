
/*

	+---+---+---+---+---+---+
	|	"E_Mut.cpp":
	|	Mutator primaries, devaluations and revaluations.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

#include "Logger.h"

//
// Immutable expression error
//
struct ImmutableError : ExecError {
	Prefix *where;
	Expr *expr;

	ImmutableError (Expr *expr, Prefix *where)
		{ this->where = where; this->expr = expr; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting mutable");
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (" (instead of ");
		log.log_expr (expr);
		log.put_ch (')');
		}	// _report

	};

//
//	Expect reference in prefix context
//
DL_EXPORT Expr *&Prefix::expectR_X (Expr *expr) {
Expr *&r_expr = evalR_X (expr);

if (! isR_X (r_expr))
	Module::report (new ImmutableError (expr, this));

return r_expr;
}	// Prefix::expectR_X

//
//	Mutability test
//

struct P_IsMutable : Prefix {
	P_IsMutable (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		val._fixed = isR_X (evalR_X (args)) ? 1 : 0;
		return T_fixed;
		}	// evalV
	};

//
// Assignment operations
//

struct P_Assign : Prefix {
	bool evaluating;			// (assign by value)

	P_Assign (char const *ident, O_Enum op, bool evaluating):
	Prefix (ident, op)
		{ this->evaluating = evaluating; }

	D_Prefix_evalV;
	};

VType P_Assign::evalV (VDatum &val, Expr *args) {
Expr *dest = get_arg (args);
VType type = T_undef;
bool result;

if (dest) {			// (something to assign to)
	if (evaluating) {	// (assign by value & return new value)
		type = evalV_X (args, val);
		result = type != T_xcpt && dest->mutateV (type, val);
		}
	else {
		result = dest->mutateX (args);
		type = dest->evalV (val, false);
		}
	}	// (dest)

if (! result)
	Module::report (new ImmutableError (dest, this));

return type;
}	// P_Assign::evalV

//
//	Clear mutable
//

struct P_Clear : Prefix {
	P_Clear (char const *ident, O_Enum op): Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_Clear::evalV (VDatum &val, Expr *args) {
Expr *&R_opnd = expectR_X (args);

switch (R_opnd ? R_opnd->evalV (val, false) : T_undef) {
	case T_undef:
		// (fall through)

	case T_fixed: {
		val._fixed = 0;
		mutateR_V (R_opnd, T_fixed, val);
		return T_fixed;
		}

	case T_float: {
		val._float = 0.0;
		mutateR_V (R_opnd, T_float, val);
		return T_float;
		}
	}	// switch (type)

return T_undef;
}	// P_Clear::evalV

//
//	Increment/Decrement mutable
//

struct P_IncDec : Prefix {
	bool dir;			// dir ? decrement : increment
	bool post;			// post ? postincrement : preincrement

	P_IncDec (char const *ident, O_Enum op, bool dir, bool post): Prefix (ident, op)
		{ this->dir = dir; this->post = post; }

	D_Prefix_evalV;
	};

VType P_IncDec::evalV (VDatum &val, Expr *args) {
Expr *&R_opnd = expectR_X (args);

switch (R_opnd ? R_opnd->evalV (val, false) : T_undef) {
	case T_undef:
		val._fixed = 0;
		// (fall through)

	case T_fixed: {
		X_Fixed *p_fixed = Cast (X_Fixed, R_opnd);

		if (p_fixed && p_fixed->refs == 1) {
			// (safe to modify in place!)
			val._fixed =
				post ?
					(dir ? p_fixed->value -- : p_fixed->value ++)
				:
					(dir ? -- p_fixed->value : ++ p_fixed->value)
				;
			return T_fixed;
			}

		S_fixed ival = dir ? val._fixed -- : val._fixed ++;
		mutateR_V (R_opnd, T_fixed, val);

		if (post) val._fixed = ival;
		return T_fixed;
		}

	case T_float: {
		S_float fval = val._float;
		val._float = dir ? fval - 1.0 : fval + 1.0;
		mutateR_V (R_opnd, T_float, val);

		if (post) val._float = fval;
		return T_float;
		}
	}	// switch

return T_undef;
}	// P_IncDec::evalV

//
//	Combined assignments
//

struct P_Combine : Prefix {
	bool inverse;			// return value before/after... (ignored)

	P_Combine (char const *ident, O_Enum op, bool inverse): Prefix (ident, op)
		{ this->inverse = inverse; }

	D_Prefix_evalV;
	};

VType P_Combine::evalV (VDatum &val, Expr *args) {
Prefix *prefix;
if (expect_term (args, false, prefix, args)) {
	Expr *&R_opnd = expectR_X (pop_list (args));

	if (R_opnd) {
		VType type = R_opnd->evalV (val, false);
		type = prefix->eval_comb (type, val, args);
		mutateR_V (R_opnd, type, val);
		return type;
		}

	return T_undef;
	}

// If not a term, just evaluate
return args->evalV (val, true);
}	// P_Combine::evalV

//
//	Reduction
//

struct P_Reduce : Prefix {
	P_Reduce (char const *ident, O_Enum op): Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_Reduce::evalV (VDatum &val, Expr *args) {
Prefix *prefix;
if (expect_term (args, false, prefix, args)) {
	return prefix ? prefix->eval_reduce (val, args) : T_undef;
	}

// If not a term, just evaluate
return args->evalV (val, true);
}	// P_Reduce::evalV

//
//	Swap values
//

struct P_Swap : Prefix {
	P_Swap (char const *ident, O_Enum op): Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_Swap::evalV (VDatum &val, Expr *args) {
Expr *head = get_arg (args);

Expr *&R_head = expectR_X (head);
Expr *&R_tail = expectR_X (args);

if (isR_X (R_head) && isR_X (R_tail))
	{ Expr *temp = R_head; R_head = R_tail; R_tail = temp; }

return T_undef;
}	// P_Swap::evalV

//
//	Devaluate
//

struct P_DeVal : Prefix {
	P_DeVal (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args)
		{ return args ? args->evalV (val, false) : T_undef; }

	Expr *evalX (Expr *args)
		{ return args; }

	};

//
//	Revaluation
//

struct P_ReVal : PrefixR {
	P_ReVal (char const *ident, O_Enum op) : PrefixR (ident, op) {}

	VType evalV (VDatum &val, Expr *args)
		{ return evalV_X_R (evalX_X (args), val); }

	Expr *evalX (Expr *args)
		{ return evalX_X_R (evalX_X (args)); }

	Expr *&evalR (Expr *args)
		{ return evalR_X_R (evalX_X (args)); }
	};

//
//	V-evaluation
//

struct P_EvalV : Prefix {
	P_EvalV (char const *ident): Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args)
		{ return evalV_X (args, val); }
	};

//
//	X-evaluation
//

struct P_EvalX : PrefixX {
	P_EvalX (char const *ident): PrefixX (ident) {}

	Expr *evalX (Expr *args)
		{ return evalX_X (args); }
	};

//
//	Mutable evaluation prefix
//

struct P_EvalR : PrefixR {
	P_EvalR (char const *ident): PrefixR (ident) {}

	Expr *&evalR (Expr *args)
		{ return evalR_X (args); }
	};

//
//	Watch on mutable
//

struct Watched : Expr {
	Expr *expr;

	Watched (Expr *expr)
		{ this->expr = link_expr (expr); }

	VType evalV (VDatum &val, bool full) {
		return expr->evalV (val, full);
		}

	bool mutateV (VType type, VDatum &val) {
		syslog->put_cstr ("Watched!\n");

		return expr->mutateV (type, val);
		}	// mutateV

	};

struct P_Watch : PrefixX {
	P_Watch (char const *ident): PrefixX (ident) {}

	Expr *evalX (Expr *args)
		{ return new ("Watched") Watched (evalX_X (args)); }
	};

//
//	Evaluate as void
//

struct P_Void : Prefix {
	P_Void (char const *ident): Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args)
		{ evalZ_X (args); return T_undef; }
	};

//
//	Bit fragment accessors/mutators
//

// Get fragment
typedef S_fixed (* BFget_func) (S_fixed value, unsigned index);

// Set fragment
typedef S_fixed (* BFset_func) (S_fixed origin, unsigned index, unsigned value);

// Get 8 bit fragment
S_fixed BFget_8bit (S_fixed value, unsigned index) {
return index < 4 ?
	(index ? value >> (index << 3) : value) & 0xFF : 0;
}	// BFget_8bit

// Set 8 bit fragment
S_fixed BFset_8bit (S_fixed origin, unsigned index, unsigned value) {
if (index < 4) {
	index <<= 3;
	unsigned mask = 0xFF << index;
	return (origin & ~mask) | ((value << index) & mask);
	}

return origin;
}	// BFset_8bit

// Get 16 bit fragment
S_fixed BFget_16bit (S_fixed value, unsigned index) {
return index < 2 ?
	(index ? value >> 16 : value) & 0xFFFF :
	0;
}	// BFget_16bit

// Set 16 bit fragment
S_fixed BFset_16bit (S_fixed origin, unsigned index, unsigned value) {
if (index < 2) {
	return index ?
		(origin & 0xFFFF) | (value << 16)
	:
		(origin & 0xFFFF0000) | (value & 0xFFFF);
	}

return origin;
}	// BFset_16bit

// Get 32 bit fragment
S_fixed BFget_32bit (S_fixed value, unsigned index) {
return index ? 0 : value;
}	// BFget_32bit

// Set 32 bit fragment
S_fixed BFset_32bit (S_fixed origin, unsigned index, unsigned value) {
return index ? origin : value;
}	// BFset_32bit

// Partial fixed accessor
struct P_PGetter : Prefix {
	BFget_func getter;

	P_PGetter (char const *ident, O_Enum op, BFget_func getter) : Prefix (ident, op)
		{ this->getter = getter; }

	VType evalV (VDatum &val, Expr *args) {
		S_fixed value = expect_fixed (get_arg (args), 0);
		S_fixed index = expect_fixed (args, 0);

		val._fixed = getter (value, index);
		return T_fixed;
		}	// evalV
	};

// Partial fixed mutator
struct P_PSetter : Prefix {
	BFset_func setter;

	P_PSetter (char const *ident, O_Enum op, BFset_func setter): Prefix (ident, op)
		{ this->setter = setter; }

	VType evalV (VDatum &val, Expr *args) {
		Expr *dest = get_arg(args);
		Expr *&R_opnd = expectR_X (get_arg(dest));
		S_fixed index = expect_fixed (dest, 0);
		S_fixed value = expect_fixed (args, 0);
		X_Fixed *p_fixed;

		if (R_opnd && (p_fixed = Cast (X_Fixed, R_opnd))) {
			if (p_fixed->refs == 1) {
				// (safe to modify in place)
				val._fixed = p_fixed->value = setter (p_fixed->value, index, value);
				}
			else {
				// (slower way to modify)
				val._fixed = setter (p_fixed->value, index, value);
				mutateR_V (R_opnd, T_fixed, val);
				}
			return T_fixed;
			}	// if (valid fixed value)

		return T_undef;
		}	// evalV

	};

// Partial fixed alterer
struct P_PMapper : Prefix {
	BFget_func getter;
	BFset_func setter;

	X_List *operand;
	X_Fixed *operand1;

	P_PMapper (char const *ident, O_Enum op, BFget_func getter, BFset_func setter) : Prefix (ident, op) {
		this->getter = getter, this->setter = setter;
		link_expr (operand = new ("PAlter/operand")
			X_List ((operand1 = new ("PAlter/operand1") X_Fixed (0)), 0));
		}

	void release () {
		unlink_expr (operand);
		Prefix::release ();
		}

	VType evalV (VDatum &val, Expr *args) {
		Expr *dest = get_arg (args);
		Expr *&R_opnd = expectR_X (get_arg (dest));
		S_fixed index = expect_fixed (dest, 0);
		Prefix *prefix = expect_prefix (get_arg (args));
		X_Fixed *p_fixed;

		if (R_opnd && (p_fixed = Cast (X_Fixed, R_opnd))) {
			operand1->value = getter (p_fixed->value, index);
			link_expr (operand->next = args);
			S_fixed result = apply_fixed (prefix, operand, 0);
			unlink_expr (operand->next);
			relink_prefix (prefix);

			val._fixed = setter (p_fixed->value, index, result);
			mutateR_V (R_opnd, T_fixed, val);
			return T_fixed;
			}

		return T_undef;
		}	// evalV
	
	};

static bool init_primaries_mutation (int order) {

//		[Categories]

//^C	Eval
//^B	Evaluators
//^D	Functors for specific argument(s) evaluation.

//^C	Mutable
//^B	Mutable functors
//^D	Functors, returning mutable result.

//^C	Mutator
//^B	Mutator functors
//^D	Functors, changing value(s) of operands.

//		[Types]

//^T	Mutable
//^B	Mutable value.
//^D	Anything evaluating to mutable.
//^D	(Reports error, if argument is immutable.)

//		[Errors]

//^E	ExpectMutable
//^B	Mutable operand expected.
//^D	Expected operand, evaluating to mutable.

//		--------

//^G	is_mut

//^N	is_mut [Predicate]
//^P	is_mut (V: Any) => Bool
//^B	Check mutability.
//^D	Return !true, if \V evaluates to mutable result.

	DefBuiltin (P_IsMutable ("is_mut", Op_Null));

//^G	let set swap

//^N	let [Mutator | Binary]
//^P	let (V: Mutable, X: Any) => Any
//^B	"Lazy" assignment.
//^D	Assigns expression \X (without evaluation) to mutable \V.
//^D	Returns: \V.
//^D	Syntax: \V := \X.

//^N	set [Mutator | Binary]
//^P	set (V: Mutable, X: Any) => Any
//^B	Eager assignment.
//^D	Assigns result of evaluation of expression \X to mutable \V.
//^D	Returns: assigned value.
//^D	Syntax: \V = \X.

	DefBuiltin (P_Assign ("let", Op_Let,		false));
	DefBuiltin (P_Assign ("set", Op_Set,		true));
	DefBuiltin (P_Assign ("set_a", Op_SetA,		true));

//^G	comb reduce

//^N	comb [Mutator | Binary | Meta]
//^P	comb (Operation: Term) => Scalar
//^B	Implements combined assignment.
//^D	Evaluation of assignment combined with unary/binary term \Operation.

	DefBuiltin (P_Combine ("comb", Op_Comb,		false));
	DefBuiltin (P_Combine ("comb_a", Op_CombA,	true));

//^N	reduce [List | Meta]
//^P	reduce (Operation: Term) => Scalar
//^B	Implements list reduction.
//^D	Performs reduction on binary term \Operation.

	DefBuiltin (P_Reduce ("reduce", Op_Reduce));

//^G	clr inc dec inc_p dec_p

//^N	clr [Mutator | Numeric | Unary]
//^P	clr (V: Mutable) => Num
//^B	Clear mutable.
//^D	Resets value of mutable \V (which must belong to numeric type) to 0.
//^D	Returns 0.

	DefBuiltin (P_Clear ("clr", Op_Null));

//^N	inc [Mutator | Numeric | Unary]
//^P	inc (V: Mutable) => Num
//^B	Pre-increment.
//^D	Increments value of mutable \V (which must belong to numeric type) by 1.
//^D	Returns: value of \V after increment.
//^D	Syntax: ++ \V.

//^N	dec [Mutator | Numeric | Unary]
//^P	dec (V: Mutable) => Num
//^B	Pre-decrement.
//^D	Decrements value of mutable \V (which must belong to numeric type) by 1.
//^D	Returns: value of \V after decrement.
//^D	Syntax: -- \V.

//^N	inc_p [Mutator | Numeric | Unary]
//^P	inc_p (V: Mutable) => Num
//^B	Post-increment.
//^D	Increments value of mutable \V (which must belong to numeric type) by 1.
//^D	Returns: value of \V before increment.
//^D	Syntax: \V ++.

//^N	dec_p [Mutator | Numeric | Unary]
//^P	dec_p (V: Mutable) => Num
//^B	Post-decrement.
//^D	Decrements value of mutable \V (which must belong to numeric type) by 1.
//^D	Returns value of \V before decrement.
//^D	Syntax: \V --.

	DefBuiltin (P_IncDec ("inc", Op_Inc,		false, false));
	DefBuiltin (P_IncDec ("inc_p", Op_IncP,		false, true));
	DefBuiltin (P_IncDec ("dec", Op_Dec,		true, false));
	DefBuiltin (P_IncDec ("dec_p", Op_DecP,		true, true));

//^N	swap [Mutator | Binary]
//^P	swap (V: Mutable, W: Mutable) => ()
//^B	Exchange values.
//^D	Exchanges values of mutables \V and \W.
//^D	Syntax: \V :=: \W.

	DefBuiltin (P_Swap ("swap", Op_Swap));

//^G	deval reval

//^N	deval [Eval | Unary]
//^P	deval (X: Any) => Any
//^B	Expression devaluation.
//^D	Inhibit evaluation of expression \X (returning \X proper).
//^D	Syntax: @\X.

	DefBuiltin (P_DeVal ("deval",	Op_DeVal));

//^N	reval [Eval | Unary]
//^P	reval (X: Any) => Any
//^B	Expression revaluation.
//^D	Evaluate result of evaluation of expression \X.
//^D	Syntax: ^\X.

	DefBuiltin (P_ReVal ("reval",	Op_ReVal));

//^G	eval_v eval_x eval_r

//^N	eval_v [Eval | Unary | Debug]
//^P	eval_v (X: Any) => Any
//^B	V-evaluation.
//^D	Internal: V-mode evaluation of expression \X.

//^N	eval_x [Eval | Unary | Debug]
//^P	eval_x (X: Any) => Any
//^B	X-evaluation.
//^D	Internal: X-mode evaluation of expression \X.

//^N	eval_r [Eval | Unary | Debug]
//^P	eval_r (X: Mutable) => Mutable
//^B	R-evaluation.
//^D	Internal: R-mode evaluation of mutable expression \X.

	DefBuiltin (P_EvalV ("eval_v"));
	DefBuiltin (P_EvalX ("eval_x"));
	DefBuiltin (P_EvalR ("eval_r"));

//^G	void

//^N	void [Eval | Unary]
//^P	void (X: Any) => ()
//^B	Evaluate as void.
//^D	Void evaluation of expression \X (ignore returned value).

	DefBuiltin (P_Void ("void"));

//
//
//

	DefBuiltin (P_Watch ("_watch"));

//
//	TTT
//

	DefBuiltin (P_PGetter ("_fp_get_b", Op_Null, BFget_8bit));
	DefBuiltin (P_PGetter ("_fp_get_w", Op_Null, BFget_16bit));
	DefBuiltin (P_PGetter ("_fp_get_l", Op_Null, BFget_32bit));

	DefBuiltin (P_PSetter ("_fp_set_b", Op_Null, BFset_8bit));
	DefBuiltin (P_PSetter ("_fp_set_w", Op_Null, BFset_16bit));
	DefBuiltin (P_PSetter ("_fp_set_l", Op_Null, BFset_32bit));

	DefBuiltin (P_PMapper ("_fp_alt_b", Op_Null, BFget_8bit, BFset_8bit));
	DefBuiltin (P_PMapper ("_fp_alt_w", Op_Null, BFget_16bit, BFset_16bit));
	DefBuiltin (P_PMapper ("_fp_alt_l", Op_Null, BFget_32bit, BFset_32bit));

return true;
}	// init_primaries_mutation

DefSubSystem ("mutation", init_primaries_mutation, 0);

