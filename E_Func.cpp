
/*

	+---+---+---+---+---+---+
	|	"E_Func.cpp":
	|	Implementation of functor operations.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

#include "Logger.h"

//
// Expect prefix
//

// Expect prefixal expression
DL_EXPORT Prefix *Prefix::expect_prefix (Expr *expr) {
VDatum val;
VType type;

if ((type = evalV_X (expr, val)) == T_prefix)
	return val._pfx;

type_error (expr, T_prefix, type, val);
return (Prefix *) 0;
}	// Prefix::expect_prefix

//
// Apply functoral to arguments
//

// TODO: more ways to evaluate?
// TODO: handle exceptions

struct P_Apply : Prefix {

	P_Apply (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Prefix *pfx = expect_prefix (get_arg (args));
		return pfx ? pfx->evalV (val, args) : evalV_X (args, val);
		}	// evalV

	Expr *evalX (Expr *args) {
		Prefix *pfx = expect_prefix (get_arg (args));
		return pfx ? pfx->evalX (args) : evalX_X (args);
		}	// evalX

	};	// P_Apply

//
//	Construction/destruction of terms
//

//
//	Expect term expression
//

DL_EXPORT bool Prefix::expect_term (Expr *expr, bool full, Prefix *&pfx, Expr *&args) {
VDatum val;
VType type;

if (expr && (type = expr->evalV (val, full)) == T_term) {
	pfx = val._term->prefix;
	args = val._term->args;
	return true;
	}

type_error (expr, T_term, type, val);
return false;
}	// Prefix::expect_term

// Construct term expression
struct P_EnTerm : PrefixX {
	P_EnTerm (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		Prefix *prefix = expect_prefix (get_arg (args));

		return new ("Term/enterm") X_Term (prefix, evalX_X (args));
		}	// evalX
	};

// Deconstruct term expression
struct P_DeTerm : PrefixX {
	P_DeTerm (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		Prefix *prefix;

		if (expect_term (args, true, prefix, args))
			return new ("List/determ") X_List (new X_Prefix (prefix), args);

		return UNDEF;
		}	// evalX
	};

//
//	Construction/destruction of blocks
//

DL_EXPORT bool Prefix::expect_block (Expr *expr, bool full, X_Block *&block) {
VDatum val;
VType type;

if (expr && (type = expr->evalV (val, full)) == T_block) {
	block = val._block;
	return true;
	}

type_error (expr, T_block, type, val);
return false;
}	// Prefix::expect_block

// Construct block expression
struct P_EnBlock : PrefixX {
	P_EnBlock (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

static X_Block *list_enblock (Expr *expr) {
X_List *list = expr ? expr->isList () : 0;
if (list) {
	X_Block *block = list_enblock (list->next);
	block->install (list->first);
	return block;
	}
else
	return new ("Block/enblock") X_Block (expr);
}	// list_enblock

Expr *P_EnBlock::evalX (Expr *args) {
Expr *temp = evalX_X (args);
Expr *result = list_enblock (temp);

relink_expr (temp);
relink_expr (args);

return result;
}	// P_EnBlock::evalX

static Expr *list_deblock (X_Block::SNode *node, Expr *tail) {
return
	node ?
		new ("List/deblock") X_List (node->expr, list_deblock (node->next, tail)) :
		tail;
}	// list_deblock

// Deconstruct block expression
struct P_DeBlock : PrefixX {
	P_DeBlock (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_DeBlock::evalX (Expr *args) {
X_Block *block;

Expr *result =
	expect_block (args, true, block) ?
		list_deblock (block->body, block->last) : 0;

relink_expr (args);
return result;
}	// P_DeBlock::evalX

//
//
//	Functional operations
//
//

//
// Functional identity
//

struct P_FunIdent : PrefixX {
	P_FunIdent (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) { return args; }
	};

//
// Functional combination
//

struct P_FunCompose : Prefix {
	P_FunCompose (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	};

struct P_FunComposition : PrefixX {
	Prefix *inner, *outer;

	P_FunComposition (Prefix *inner, Prefix *outer) : 
		PrefixX (0, 0) {
		link_prefix (this->inner = inner);
		link_prefix (this->outer = outer);
		}

	D_Prefix_evalX;
	D_Prefix_log_ex;
	D_Prefix_release;
	};

VType P_FunCompose::evalV (VDatum &val, Expr *args) {
Prefix *inner = expect_prefix (get_arg (args));
Prefix *outer = expect_prefix (args);

val._pfx = new ("Fun/Composition") P_FunComposition (inner, outer);
return T_prefix;
}	// P_FunCompose::evalV

void P_FunComposition::log_ex (Logger &log) {
outer->log_ex (log);
log.put_cstr (" <*> ");
inner->log_ex (log);
}	// P_FunComposition::log_ex

Expr *P_FunComposition::evalX (Expr *args) {
Expr *x_inner = link_expr (inner->evalX (args));
Expr *x_outer = outer->evalX (x_inner);
unlink_expr (x_inner);
return x_outer;
}	// P_FunComposition::evalX

void P_FunComposition::release () {
unlink_prefix (inner);
unlink_prefix (outer);
Prefix::release ();
}	// P_FunComposition::release

//
// Functional boolean negation
//

struct P_FunNegated : PrefixX {
	Prefix *first;

	P_FunNegated (Prefix *first) : PrefixX (0, 0) {
		link_prefix (this->first = first);
		}

	D_Prefix_evalV;
	D_Prefix_log_ex;
	D_Prefix_release;
	};

void P_FunNegated::log_ex (Logger &log) {
log.put_cstr (" <")->put_cstr ("~~")->put_cstr ("> ");

first->log_ex (log);
}	// P_FunNegated::log_ex

VType P_FunNegated::evalV (VDatum &val, Expr *args) {
return_fixed (val, ! apply_bool (first, args));
}	// P_FunNegated::evalV

void P_FunNegated::release () {
unlink_prefix (first);
Prefix::release ();
}	// P_FunNegated::release

struct P_FunNot : Prefix {
	P_FunNot (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Prefix *first = expect_prefix (args);
		val._pfx = new ("Fun/Negated") P_FunNegated (first);
		return T_prefix;
		}	// evalV

	};

//
// Functional boolean binary (AND/OR)
//

struct P_FunConditional : PrefixX {
	bool polarity;

	Prefix *first, *second;

	P_FunConditional (bool polarity, Prefix *first, Prefix *second) :
		PrefixX (0, 0) {
		this->polarity = polarity;

		link_prefix (this->first = first);
		link_prefix (this->second = second);
		}

	D_Prefix_evalV;
	D_Prefix_log_ex;
	D_Prefix_release;
	};

void P_FunConditional::log_ex (Logger &log) {
first->log_ex (log);
log.put_cstr (" <")->put_cstr (polarity ? "||" : "&&")->put_cstr ("> ");
second->log_ex (log);
}	// P_FunConditional::log_ex

VType P_FunConditional::evalV (VDatum &val, Expr *args) {
return_fixed (val,
	apply_bool (first, args) != polarity ?
		apply_bool (second, args) :
	polarity
	);
}	// P_FunConditional::evalV

void P_FunConditional::release () {
unlink_prefix (first);
unlink_prefix (second);
Prefix::release ();
}	// P_FunConditional::release

struct P_FunCond : Prefix {
	bool polarity;

	P_FunCond (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	VType evalV (VDatum &val, Expr *args) {
		Prefix *first = expect_prefix (get_arg (args));
		Prefix *second = expect_prefix (args);

		val._pfx = new ("Fun/Conditional") P_FunConditional (polarity, first, second);
		return T_prefix;
		}	// evalV
	};

//
//	Closures
//

struct P_Closure : Prefix {

	P_Functor *context;			// context user functor
	Expr ** envir;				// environment kept here
	Prefix *core;				// functor to evaluate

	// Allocate & init closure environment
	void init_closure_env () {
		if (context->CFP != SP_NULL) {
			unsigned count = context->local_no;
			Expr ****table = context->local_tab;

			envir = new ("Closure/Environ") Expr * [context->local_no];

			for (unsigned i = 0; i != count; ++ i)
				envir[i] = link_expr (**table[i]);
			}
		else
			envir = 0;
		}	// init_closure_env

	// Release & free closure environment
	void free_closure_env () {
		if (envir) {
			// free all
			unsigned count = context->local_no;
			Expr ****table = context->local_tab;

			for (unsigned i = 0; i != count; ++ i)
				unlink_expr (envir[i]);
			
			delete [] envir;
			}
		}	// free_closure_env

	// TODO: check CFP...
	void bind_environ () {
		unsigned count = context->local_no;
		Expr ****table = context->local_tab;
		for (unsigned i = 0; i != count; ++ i)
			*table[i] = &envir[i];
		}	// bind_environ

	// TODO: check CFP...
	void unbind_environ () {
		unsigned count = context->local_no;
		Expr ****table = context->local_tab;
		for (unsigned i = 0; i != count; ++ i)
			*table[i] = 0;
		}	// unbind_environ

	// Constructor
	P_Closure (P_Functor *context, Prefix *core) :
		Prefix (0, Op_Null) {
		link_prefix (this->context = context);
		link_prefix (this->core = core);

		init_closure_env ();
		}

	void log_ex (Logger &log) {
		unsigned count = context->local_no;
		context->log (log);
		log.put_ch (':');

		log.put_ch ('<');
		for (unsigned i = 0; i != count; ++ i) {
			if (i) log.put_cstr (", ");
			log.log_expr (envir[i]);
			}
		log.put_ch ('>');

		log.put_cstr (" => ");
		core->log_ex (log);
		}	// log_ex

	VType evalV (VDatum &val, Expr *args) {
		bind_environ ();
		VType result = core->evalV (val, args);
		unbind_environ ();
		return result;
		}	// evalV

	Expr *evalX (Expr *args) {
		bind_environ ();
		Expr *result = core->evalX (args);
		unbind_environ ();
		return result;
		}	// evalX

	Expr *&evalR (Expr *args) {
		bind_environ ();
		Expr *&result = core->evalR (args);
		unbind_environ ();
		return result;
		}	// evalR

	void release () {
		free_closure_env ();

		unlink_prefix (context);
		unlink_prefix (core);
		Prefix::release ();
		}

	};	// P_Closure

struct P_Closure_Cons : Prefix {

	P_Closure_Cons (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Prefix *context = expect_prefix (get_arg (args));
		Prefix *core = expect_prefix (args);
		P_Functor *func = Cast (P_Functor, context);

		if (func) {
			val._pfx = new ("Prefix/Closure") P_Closure (func, core);
			return T_prefix;
			}

		return T_undef;
		}	// evalV

	};	// P_Closure_Cons

//
//	Curried evaluation
//

struct P_Curried : Prefix {
	Prefix *func;			// functor to curry
	Expr *arglist;				// curried arguments list prepare
	unsigned argcount;			// total arguments
	Expr ***argrefs;			// argument references

	// Construct curried functor
	P_Curried (bool last_flag, Prefix *func, Expr *arglist);

	// Release
	void release ();

	void log_ex (Logger &log);

	// Set arguments list
	void set_arglist (Expr *args);

	// Unset arguments list
	void unset_arglist ();

	VType evalV (VDatum &val, Expr *args) {
		set_arglist (args);
		VType result = func->evalV (val, arglist);
		unset_arglist ();
		return result;
		}	// evalV

	Expr *evalX (Expr *args) {
		set_arglist (args);
		Expr *result = func->evalX (args);
		unset_arglist ();
		return result;
		}	// evalX

	Expr *&evalR (Expr *args) {
		set_arglist (args);
		Expr *&result = func->evalR (args);
		unset_arglist ();
		return result;
		}	// evalR

	};

P_Curried::P_Curried (bool last_flag, Prefix *func, Expr *arglist) :
	Prefix (0, Op_Null) {
link_prefix (this->func = func);
this->arglist = link_expr (arglist);

// count list elements
unsigned count = 0;
X_List *list;
while (arglist && (list = arglist->isList ())) {
	if (! (list->first)) ++ count;
	if (last_flag && ! (arglist = list->next)) ++ count;
	}

// construct argrefs list
Expr *** _argrefs = argrefs = new ("Curried/argrefs") Expr ** [argcount = count];
arglist = this->arglist;

while (arglist && (list = arglist->isList ())) {
	if (! (list->first))
		*_argrefs ++ = &list->first;
	if (last_flag && ! (arglist = list->next))
		*_argrefs = &list->next;
	}
}	// P_Curried::P_Curried

// Set arguments list
void P_Curried::set_arglist (Expr *args) {
Expr ***argrefs = this->argrefs;
unsigned count = argcount;

while (count --)
	link_expr (** argrefs ++ = get_arg (args));
}	// P_Curried::set_arglist

// Unset arguments list
void P_Curried::unset_arglist () {
Expr ***argrefs = this->argrefs;
unsigned count = argcount;

while (count --) {
	Expr * &core = ** argrefs ++;
	unlink_expr (core);
	core = 0;
	}
}	// P_Curried::unset_arglist

// Release curried
void P_Curried::release () {
unlink_prefix (func);
unlink_expr (arglist);
delete [] argrefs;

Prefix::release ();
}	// P_Curried::release

void P_Curried::log_ex (Logger &log) {
log.put_ch ('<');

func->log_ex (log);

log.put_ch (':');
log.put_ch ('[');
log.log_fixed (argcount);
log.put_ch (']');
log.put_ch (':');

log.log_expr (arglist);

log.put_ch ('>');
}	// log_ex

//
//	Curriing proper
//

struct P_Curry : Prefix {

	P_Curry (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Prefix *func = expect_prefix (get_arg (args));
		Expr *arglist = evalX_X (args);

		if (func) {
			val._pfx = new ("Prefix/Curried") P_Curried (true, func, arglist);
			return T_prefix;
			}

		return T_undef;
		}	// evalV
	};	// P_Curry

//
//	Operation table
//

static bool init_primaries_functional (int order) {

//		[Categories]

//^C	Term
//^B	Term functors
//^D	Functors, operating on terms.

//^C	Block
//^B	Block functors
//^D	Functors, operating on blocks.

//^C	Func
//^B	Functional operations
//^D	Functors, expecting functors as operand(s).

//^C	Composer
//^B	Functional composers
//^D	Functors, constructing and returning functors.

//		[Types]

//^T	Term
//^B	Term value.
//^D	Anything evaluating to term expression.
//^D	(Reports error, if argument is not term.)

//^T	Block
//^B	Block value.
//^D	Anything evaluating to block expression.
//^D	(Reports error, if argument is not block.)

//^T	Func
//^B	Functor reference.
//^D	Anything evaluating to functor reference (including builtins, user functors, classes and virtuals).

//		--------

//^G	is_var is_term is_block is_func

//^N	is_var [Predicate]
//^P	is_var(V: Any) => Bool
//^B	Check for variable.
//^D	Predicate: !true, if argument \V evaluates to variable.

//^N	is_term [Predicate]
//^P	is_term(V: Any) => Bool
//^B	Check for term.
//^D	Predicate: !true, if argument \V evaluates to term.

//^N	is_block [Predicate]
//^P	is_block(V: Any) => Bool
//^B	Check for block.
//^D	Predicate: !true, if argument \V evaluates to block.

//^N	is_func [Predicate]
//^P	is_func(V: Any) => Bool
//^B	Check for functor.
//^D	Predicate: !true, if argument \V evaluates to functor reference.

	DefBuiltin (P_IsType ("is_term", Op_Null, T_term));
	DefBuiltin (P_IsType ("is_block", Op_Null, T_block));
	DefBuiltin (P_IsType ("is_var", Op_Null, T_var));
	DefBuiltin (P_IsType ("is_func", Op_Null, T_prefix));

//^G	apply

//^N	apply [Func]
//^P	apply(F: Func, Args: Any) => Any
//^B	Apply functor to argument(s).
//^D	Invokes functor reference \F with argument(s) \Args, returning result of invocation.

	DefBuiltin (P_Apply ("apply", Op_Apply));

//^G	enterm determ

//^N	enterm [Term]
//^P	enterm(F: Func, A: Any) => Term
//^B	Construct term from functor and argument(s).
//^D	Construct term from functor reference \F and argument(s) \A.
//^D	(Reverse to !determ).

//^N	determ [Term]
//^P	determ(T: Term) => (Func, Any)
//^B	Deconstruct term to functor and argument(s).
//^D	Deconstruct term \T to list of functor reference and argument(s).
//^D	(Reverse to !enterm).

	DefBuiltin (P_EnTerm ("enterm", Op_Null));
	DefBuiltin (P_DeTerm ("determ", Op_Null));

//^G	enblock deblock

//^N	enblock [Block]
//^P	enblock(L: List) => Block
//^B	Construct block from item list.
//^D	Construct block from list of expressions \L.
//^D	(Reverse to !deblock).

//^N	deblock [Block]
//^P	deblock(B: Block) => List
//^B	Deconstruct block to item list.
//^D	Deconstruct block \B to list of expressions.
//^D	(Reverse to !enblock).

	DefBuiltin (P_EnBlock ("enblock", Op_Null));
	DefBuiltin (P_DeBlock ("deblock", Op_Null));

//^N	f_ident [Func]
//^P	f_ident (V: Any) => Any
//^B	Identity.
//^D	Identity functor (for any argument \V returns \V as is).

	DefBuiltin (P_FunIdent ("f_ident", Op_Null));

//^N	f_compose [Func | Composer]
//^P	f_compose (F_f: Func, F_g: Func) => Func
//^B	Functional composition.
//^D	Returns functional composition of \F_f and \F_g.
//^D	(Result of (!f_compose (\F_f, \F_g)) ! (\V) is equal to \F_f ! (\F_g ! (\V)) for any \V.)

	DefBuiltin (P_FunCompose ("f_compose", Op_Null));

//^G	f_not f_and f_or

//^N	f_not [Func | Conditional | Composer]
//^P	f_not (Pred: Func) => Func
//^B	Functional "NOT" predicate composition.
//^D	For predicates \Pred, results in predicate, evaluating to conditional NOT of \Pred.
//^D	( !f_not (\Pred) ! (\Arg) == !c_not (\Pred1 ! \Arg) ).

	DefBuiltin (P_FunNot ("f_not", Op_Null, false));

//^N	f_and [Func | Conditional | Composer]
//^P	f_and (Pred1: Func, Pred2: Func) => Func
//^B	Functional "AND" predicates composition.
//^D	For predicates \Pred1 and \Pred2, results in predicate, evaluating to conditional AND of \Pred1 and \Pred2.
//^D	( !f_and (\Pred1, \Pred2) ! (\Arg) == !c_and (\Pred1 ! \Arg, \Pred2 ! \Arg) ).

//^N	f_or [Func | Conditional | Composer]
//^P	f_or (Pred1: Func, Pred2: Func) => Func
//^B	Functional "OR" predicates composition.
//^D	For predicates \Pred1 and \Pred2, results in predicate, evaluating to conditional OR of \Pred1 and \Pred2.
//^D	( !f_or (\Pred1, \Pred2) ! (\Arg) == !c_or (\Pred1 ! \Arg, \Pred2 ! \Arg) ).

	DefBuiltin (P_FunCond ("f_and", Op_Null, false));
	DefBuiltin (P_FunCond ("f_or", Op_Null, true));

//^N	closure [Func | Composer]
//^P	closure (Context: Func, Callee: Func) => Func
//^B	Closure constructor.
//^D	Create and return closure: \Callee with bound context of \Context.

	DefBuiltin (P_Closure_Cons ("closure", Op_Null));

//^N	f_curry [Func | Composer]
//^P	f_curry (Functor: Func, Arglist: List) => Func
//^B	Functor currying.
//^D	Curry functor: returns callable Functor ! (Arglist).

	DefBuiltin (P_Curry ("f_curry", Op_Null));

return true;
}	// init_primaries_functional

DefSubSystem ("functional", init_primaries_functional, 0);

