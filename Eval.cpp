
/*

	+---+---+---+---+---+---+
	|	"Eval.cpp":
	|	Generic evaluations.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

#include "NameTab.h"

#define HEADER

#include "E_Array.cpp"
#include "E_Hash.cpp"
#include "E_Pattern.cpp"
#include "E_Ring.cpp"
#include "E_Stream.cpp"

#undef HEADER

#include "Logger.h"

//
//	Expr constructor
//

DL_EXPORT Expr::Expr () { refs = 0; }

DL_EXPORT X_Fixed::X_Fixed () { value = 0; }
DL_EXPORT X_Fixed::X_Fixed (S_fixed value) { this->value = value; }

DL_EXPORT X_Float::X_Float () { value = 0.0; }
DL_EXPORT X_Float::X_Float (S_float value) { this->value = value; }

DL_EXPORT X_List::X_List (Expr *first, Expr *next) {
	this->first = link_expr (first);
	this->next = link_expr (next);
	}	// X_List::X_List

DL_EXPORT X_Term::X_Term (Prefix *prefix, Expr *args) {
	link_prefix (this->prefix = prefix);
	this->args = link_expr (args);
	}	// X_Term::X_Term

DL_EXPORT External::External () {};

DL_EXPORT X_Extern::X_Extern (const char *sign, External *external) {
this->sign = sign;
this->external = external;
}	// X_Extern::X_Extern

//
//	Primitive coercions
//

X_List *Expr::isList () { return (X_List *) 0; }
X_List *X_List::isList () { return this; }

X_Term *Expr::isTerm () { return (X_Term *) 0; }
X_Term *X_Term::isTerm () { return this; }

VType X_Term::evalV (VDatum &val, bool full) {
if (full)
	return prefix ? prefix->evalV (val, args) : T_undef;

// (return self)
val._term = this;
return T_term;
}	// X_Term::evalV

Expr *X_Term::evalX () {
return prefix ? prefix->evalX (args) : 0;
}	// X_Term::evalX

bool X_Term::mutateV (VType type, VDatum &val) {
if (prefix)
	return prefix->mutateV (type, val, args);

return false;
}	// X_Term::mutateV

bool X_Term::mutateX (Expr *expr) {
if (prefix)
	return prefix->mutateX (expr, args);

return false;
}	// X_Term::mutateX

//
// Values link/unlink
//

// TODO: correct link/unlink/relink for T_prefix ...

static Expr *Val_X (VType type, VDatum &val) {
switch (type) {
	case T_string:
		return 0;

	case T_list:
		return val._list;

	case T_term:
		return val._term;

	case T_var:
		return val._var;

	case T_block:
		return val._block;

/*
	case T_prefix:
		return new ("Prefix/temp") X_Prefix (val._pfx);
 */

	case T_array:
		return val._array;

	case T_hash:
		return val._hash;

	case T_object:
		return val._object;

	case T_pattern:
		return val._pattern;

	case T_ring:
		return val._ring;

	case T_stream:
		return val._stream;

	case T_scodec:
		return val._scodec;

	case T_xcpt:
		return val._except;

	case T_extern:
		return val._extern;
	}	// switch (type)

return 0;
}	// Val_X

// Link prefix
DL_EXPORT void link_prefix (Prefix *prefix) {
if (prefix) prefix->refs ++;
}	// link_prefix

// Unlink prefix
DL_EXPORT void unlink_prefix (Prefix *prefix) {
if (prefix && ! -- prefix->refs)
	prefix->release ();
}	// unlink_prefix

// Relink prefix
DL_EXPORT void relink_prefix (Prefix *prefix) {
if (prefix && ! prefix->refs)
	prefix->release ();
}	// relink_prefix

// Link value
DL_EXPORT void link_value (VType type, VDatum &val) {
if (type == T_string)
	val._string.link ();
else if (type == T_prefix)
	link_prefix (val._pfx);
else
	link_expr (Val_X (type, val));
}	// link_value

// Unlink value
DL_EXPORT void unlink_value (VType type, VDatum &val) {
if (type == T_string)
	val._string.unlink ();
else if (type == T_prefix)
	unlink_prefix (val._pfx);
else
	unlink_expr (Val_X (type, val));
}	// unlink_value

// Relink value
DL_EXPORT void relink_value (VType type, VDatum &val) {
if (type == T_string)
	val._string.relink ();
else if (type == T_prefix)
	relink_prefix (val._pfx);
else
	relink_expr (Val_X (type, val));
}	// relink_value

// Relock value by 'dir'
void relock_value_by (int dir, VType type, VDatum &val) {
relock_expr (dir, Val_X (type, val));
}	// relock_value_by

//
// List push/pop operations
//

// Pop first element from list (if list)
Expr *pop_list (Expr *&list) {
if (list) {
VDatum val;

if (list->evalV (val, false) == T_list) {
	list = val._list->next;
	Expr *first = val._list->first;

	if (first) {
		first->relock (1);
		relink_value (T_list, val);
		first->relock (-1);
		}
	else
		relink_value (T_list, val);
	return first;
	}
else {
	Expr *expr = list;
	list = 0;
	return expr;
	}
}	// (list)

return 0;
}	// pop_list

//
//	Remove head node from 'list'
//	(storing children to 'first'/'next')
//
void list_behead (X_List *list, Expr *&first, Expr *&next) {
if (list) {
	first = list->first;
	next = list->next;

	if (! list->refs) {
		first->refs ++;
		next->refs ++;

		relink_expr (list);

		first->refs --;
		next->refs --;
		}
	}
}	// list_behead

//
//	Get next functor argument (with tail evaluation)
//

// This is an ugly hack!
// But conflict between (From ..) and just (Num) needs to be resolved in eval_range
static VType arg_type;

DL_EXPORT Expr *get_arg (Expr *&args) {
if (args) {
VDatum val;

arg_type = args->evalV (val, false);
if (arg_type == T_list) {
	relink_expr (args);
	args = val._list->next;
	return val._list->first;
	}

// Otherwise: commit evaluation
arg_type = args->evalV (val, true);
relink_expr (args);				// [TMP] - ???

if (arg_type == T_list) {
	Expr *first;
	list_behead (val._list, first, args);
	return first;
	}

args = 0;
return evalX_V (arg_type, val);
}	// (args)

return 0;
}	// get_arg

//
// Expressions evaluation
//

enum { PD_FIXED_MIN = -3, PD_FIXED_MAX = 3 };

static X_Fixed predef_fixed [PD_FIXED_MAX - PD_FIXED_MIN];

// Fetch 'expr' to 'val'
inline VType fetchV (Expr *expr, VDatum &val) {
return expr ? expr->evalV (val, false) : T_undef;
}	// fetchV

// Expression's full evaluation
VType evalV_X (Expr *expr, VDatum &val) {
return expr ? expr->evalV (val, true) : T_undef;
}	// evalV_X

// Expression's full evaluation (relink expression)
VType evalV_X_R (Expr *expr, VDatum &val) {
VType type = expr ? expr->evalV (val, true) : T_undef;
relink_expr (expr);
return type;
}	// evalV_X_R

// Allocate new fixed value
static Expr *cons_fixed (S_fixed value) {
return
	PD_FIXED_MIN <= value && value < PD_FIXED_MAX ?
		predef_fixed + (value - PD_FIXED_MIN) :
		new ("Fixed/eval") X_Fixed (value);
}	// cons_fixed

// Construct new expression from value
DL_EXPORT Expr *evalX_V (VType type, VDatum &val) {
switch (type) {
	case T_undef:
		return 0;

	case T_fixed:
		return cons_fixed (val._fixed);

	case T_float:
		return new ("Float/eval") X_Float (val._float);

	case T_string:
		return val._string.cons ();

	case T_var:
		return val._var;

	case T_list:
		return val._list;

	case T_term:
		return val._term;

	case T_object:
		return val._object;

	case T_array:
		return val._array;

	case T_hash:
		return val._hash;

	case T_pattern:
		return val._pattern;

	case T_ring:
		return val._ring;

	case T_block:
		return val._block;

	case T_prefix:
		return new ("Prefix/eval") X_Prefix (val._pfx);

	case T_stream:
		return val._stream;

	case T_scodec:
		return val._scodec;

	case T_extern:
		return val._extern;
	}

return 0;
}	// evalX_V

// Evaluate expression
DL_EXPORT Expr *evalX_X (Expr *expr) {
return expr ? expr->evalX () : 0;
}	// evalX_X

// Evaluate expression (relink expression)
Expr *evalX_X_R (Expr *expr) {
Expr *x_expr = expr ? expr->evalX () : 0;
relink_expr (expr);
return x_expr;
}	// evalX_X_R

// Check expressions 'left' && 'right' for identicity
DL_EXPORT bool identX (Expr *left, Expr *right) {
VDatum val;
bool result =
	left ?
		right ?
			left->identV (right->evalV (val, false), val) :
			false :

	right ? false : true;

relink_expr (left); relink_expr (right);
return result;
}	// identX

// Calculate hash code of 'expr'
DL_EXPORT unsigned hashX (Expr *expr) {
unsigned val = expr ? expr->hash() : 0;
relink_expr (expr);
return val;
}	// hashX

// Void evaluation (release result)
DL_EXPORT Except *evalZ_X (Expr *expr) {
VType type;
VDatum val;
relink_value (type = evalV_X (expr, val), val);
return type == T_xcpt ? val._except : 0;
}	// evalZ_X

//
// Expressions assignment
//

static Expr *_no_ref = 0;

// Reference non-referential expression
Expr *&consR_X (Expr *expr) {
_no_ref = expr;
return _no_ref;
}	// consR_X

// Check, is it really referential expression
bool isR_X (Expr *&r_expr) {
return &_no_ref != &r_expr;
}	// isR_X

// Reference evaluation
Expr *&evalR_X (Expr *expr) {
return expr ? expr->evalR() : R_null;
}	// evalR_X

// Reference evaluation (with relink)
Expr *&evalR_X_R (Expr *expr) {
Expr *&ref = evalR_X (expr);
relink_expr (expr);
return ref;
}	// evalR_X_R

// Assign expression to reference
DL_EXPORT bool mutateR_X (Expr *&r_expr, Expr *expr) {
if (&r_expr == &_no_ref) return false;

// Note: r_expr && expr may be parts of same list, for example
if (expr != r_expr) {
	link_expr (expr);
	unlink_expr (r_expr);
	r_expr = expr;
	}

return true;
}	// mutateR_X

// Assign value to reference
DL_EXPORT bool mutateR_V (Expr *&r_expr, VType type, VDatum &val) {
return (r_expr && r_expr->mutateV (type, val))
	|| mutateR_X (r_expr, evalX_V (type, val));
}	// mutateR_V

// Assign expression to mutable
// Note: mutation of undef is considered OK!
DL_EXPORT bool mutateX_X (Expr *m_expr, Expr *expr) {
return m_expr ? m_expr->mutateX (expr) : true;
}	// mutateX_X

// Assign value to mutable
// Note: mutation of undef is considered OK!
DL_EXPORT bool mutateX_V (Expr *m_expr, VType type, VDatum &val) {
return m_expr ? m_expr->mutateV (type, val) : true;
}	// mutateX_V

//
// Immutable Expressions
//

VType Expr::evalV (VDatum &val, bool full) {
return T_undef;
}	// Expr::evalV

// Expr::evalX <-- Expr::evalV
Expr *Expr::evalX () {
VDatum val;
return evalX_V (evalV (val, true), val);
}	// Expr::evalX

Expr *&Expr::evalR () {
return R_null;
}	// Expr::evalR

bool Expr::mutateV (VType type, VDatum &val) {
return false;
}	//  Expr::mutateV

bool Expr::mutateX (Expr *expr) {
return false;
}	// Expr::mutateX

bool Expr::identV (VType type, VDatum &val) {
return type == T_undef;
}	// Expr::identV

unsigned Expr::hash () { return 0; }

//
// Mutable Expressions
//

VType ExprR::evalN (VDatum &val) { return T_undef; }

VType ExprR::evalV (VDatum &val, bool full) {
if (full)
	return fetchV (evalR(), val);

// (if literal evaluation, resort to evalN)
return evalN (val);
}	// ExprR::evalV

Expr *ExprR::evalX () {
return evalR ();
}	// ExprR::evalX

bool ExprR::mutateV (VType type, VDatum &val) {
return mutateR_V (evalR(), type, val);
}	// ExprR::mutateV

bool ExprR::mutateX (Expr *expr) {
return mutateR_X (evalR(), expr);
}	// ExprR::mutateX

//
// Evaluation wrapping
//

void WrapX_V::eval (Expr *args) {
type = args ? args->evalV (val, true) : T_undef;
}	// WrapX_V::eval

void WrapX_V::refuse () {
type = T_undef;
}	// WrapX_V::refuse

DL_EXPORT VType P_Wrapper::evalV (VDatum &val, Expr *args) {
WrapX_V wrapperV (val);

eval (wrapperV, args);

return wrapperV.type;
}	// P_Wrapper::evalV

void WrapX_X::eval (Expr *args) {
result = args ? args->evalX () : 0;
}	// WrapX_X::eval

void WrapX_X::refuse () {
result = 0;
}	// WrapX_X::refuse

DL_EXPORT Expr *P_Wrapper::evalX (Expr *args) {
WrapX_X wrapperX;

eval (wrapperX, args);

return wrapperX.result;
}	// P_Wrapper::evalX

void WrapX_R::eval (Expr *args) {
p_result = args ? &args->evalR () : &R_null;
}	// WrapX_R::eval

void WrapX_R::refuse () {
p_result = &R_null;
}	// WrapX_R::refuse

DL_EXPORT Expr *&P_Wrapper::evalR (Expr *args) {
WrapX_R wrapperR;

eval (wrapperR, args);

return *wrapperR.p_result;
}	// P_Wrapper::evalR

// TODO: mutators for wrapper prefixes ?

//
//	Iterators core...
//

void P_Iterator::evaluate (IterContext &IC, Expr *args) {}

DL_EXPORT VType P_Iterator::evalV (VDatum &val, Expr *args) {
// (iterator core)
struct IterV : IterContext {
	VType type;
	VDatum &val;

	IterV (VDatum &_val) : val(_val) { type = T_undef; }

	bool next () {
		relink_value (type, val);
		type = body->evalV (val, true);
		return type != T_xcpt;
		}

	} __iterV (val);

evaluate (__iterV, args);
return __iterV.type;
}	// P_Iterator::evalV

DL_EXPORT Expr *P_Iterator::evalX (Expr *args) {
// (iterator core)
struct IterX : IterContext {
	Expr *result;

	IterX () { result = 0; }

	bool next () {
		relink_expr (result);
		result = body->evalX ();
		// TODO: check for exception...
		return result;
		}

	} __iterX;

evaluate (__iterX, args);
return __iterX.result;
}	// P_Iterator::evalX

//
//
// Scalar operations
//
//

VType X_Fixed::evalV (VDatum &val, bool full) {
return_fixed (val, value);
}	// X_Fixed::evalV

VType X_Float::evalV (VDatum &val, bool full) {
return_float (val, value);
}	// X_Float::evalV

Expr *X_Fixed::evalX () { return this; }

Expr *X_Float::evalX () { return this; }

bool X_Fixed::mutateV (VType type, VDatum &val) {
if (refs == 1 && type == T_fixed)
	{ value = val._fixed; return true; }

return false;
}	// X_Fixed::mutateV

bool X_Fixed_L::mutateV (VType type, VDatum &val) {
return false;
}	// X_Fixed_L::mutateV

bool X_Float::mutateV (VType type, VDatum &val) {
if (refs == 1 && type == T_float)
	{ value = val._float; return true; }

return false;
}	// X_Float::mutateV

bool X_Float_L::mutateV (VType type, VDatum &val) {
return false;
}	// X_Float_L::mutateV

bool X_Fixed::identV (VType type, VDatum &val) {
return type == T_fixed && value == val._fixed;
}	// X_Fixed::identV

bool X_Float::identV (VType type, VDatum &val) {
return type == T_float &&
	(value == val._float ||
		// NaNs *are* identical
		(value != value && val._float != val._float)
	);
}	// X_Float::identV

unsigned X_Fixed::hash () { return value; }

unsigned X_Float::hash () {
S_fixed hash_float (S_float value);
return hash_float (value);
}	// X_Float::hash

//
// List operations
//

Expr *X_List::evalX () {
Expr *_first = evalX_X (first);
Expr *_next = Cast(Except, first) ? 0 : evalX_X (next);

return (_first != first || _next != next) ?
	// (list is NOT self-evaluating)
	new ("List/eval") X_List (_first, _next) :
	// (list IS self-evaluating)
	this;
}	// X_List::evalX

VType X_List::evalV (VDatum &val, bool full) {
val._list = full ? evalX ()->isList () : this;
return T_list;
}	// X_List::evalV

bool X_List::mutateX (Expr *expr) {
X_List *list = expr ? expr->isList () : 0;
if (list)
	return
		mutateX_X (next, list->next) &&
		mutateX_X (first, list->first);
else
	return
		mutateX_X (next, 0) &&
		mutateX_X (first, expr);
}	// X_List::mutateX

bool X_List::mutateV (VType type, VDatum &val) {
if (type == T_list) {
	VDatum _val;
	return
		mutateX_V (next, fetchV (val._list->next, _val), _val) &&
		mutateX_V (first, fetchV (val._list->first, _val), _val);
	}
else
	return
		mutateX_V (next, T_undef, val) &&
		mutateX_V (first, type, val);
}	// X_List::mutateV

bool X_List::identV (VType type, VDatum &val) {
return type == T_list
	&& identX (first, val._list->first)
	&& identX (next, val._list->next);
}	// X_List::identV

DL_EXPORT unsigned X_List::hash () {
return hashX (first) ^ (hashX (next) << 4);
}	// X_List::hash

//
// Term operations
//

Expr *&X_Term::evalR () {
return prefix ? prefix->evalR (args) : R_null;
}	// X_Term::evalR

bool X_Term::identV (VType type, VDatum &val) {
return type == T_term &&
	prefix == val._term->prefix &&
	identX (args, val._term->args);
}	// X_Term::identV

unsigned X_Term::hash () {
return prefix->hash() ^ hashX (args);
}	// X_Term::hash

//
// Prefixal operations
//

DL_EXPORT VType Prefix::evalV (VDatum &val, Expr *args) {
return T_undef;
}	// Prefix::evalV

// Default: Prefix::evalX <- Prefix::evalV
DL_EXPORT Expr *Prefix::evalX (Expr *args) {
VDatum val;
return evalX_V (evalV (val, args), val);
}	// Prefix::evalX

DL_EXPORT Expr *&Prefix::evalR (Expr *args) {
return R_null;
}	// Prefix::evalR

DL_EXPORT VType Prefix::eval_comb (VType type, VDatum &val, Expr *args) {
return T_undef;
}	// Prefix::eval_comb

DL_EXPORT VType Prefix::eval_reduce (VDatum &val, Expr *args) {
return T_undef;
}	// Prefix::eval_reduce

DL_EXPORT unsigned Prefix::hash () {
return name ? name->hash : 0;
}	// Prefix::hash

//
// Expression prefix
//

DL_EXPORT Expr *PrefixX::evalX (Expr *args) {
return 0;
}	// PrefixX::evalX

DL_EXPORT VType PrefixX::evalV (VDatum &val, Expr *args) {
Expr *expr = evalX (args);
VType type = expr ? expr->evalV (val, false) : T_undef;
if (type == T_fixed || type == T_float)
	relink_expr (expr);

if (type == T_string && expr->refs == 0) {			// Fixed: 08.04.2011
	val._string.refcount_by (1);
	relink_expr (expr);
	val._string.refcount_by (-1);
	}

return type;
}	// PrefixX::evalV

//
// Mutable prefix
//

VType PrefixR::evalV (VDatum &val, Expr *args) {
return fetchV (evalR(args), val);
}	// PrefixR::evalV

Expr *PrefixR::evalX (Expr *args) {
return evalR (args);
}	// PrefixR::evalX

Expr *&PrefixR::evalR (Expr *args) {
return R_null;
}	// PrefixR::evalR

//
//	Constructor for X_Variable
//

X_Variable::X_Variable (AnyVar *name) {
link_name (this->name = name);
}	// X_Variable::X_Variable

//
//
// Scalar operations
//
//

//
// List construction / deconstruction
//

// List constructor: (eval (type, val), tail)
inline VType list_cons (VType type, VDatum &val, Expr *tail) {
if (tail) {
	val._list = new ("List/cons") X_List (evalX_V (type, val), tail);
	return T_list;
	}

return type;
}	// list_cons

DL_EXPORT bool PrefixR::mutateV (VType type, VDatum &val, Expr *args) {
return mutateR_V (evalR(args), type, val);
}	// PrefixR::mutateV

DL_EXPORT bool PrefixR::mutateX (Expr *expr, Expr *args) {
return mutateR_X (evalR(args), expr);
}	// PrefixR::mutateX

DL_EXPORT bool Prefix::mutateV (VType type, VDatum &val, Expr *args) {
return false;
}	// Prefix::mutateV

DL_EXPORT bool Prefix::mutateX (Expr *expr, Expr *args) {
return false;
}	// Prefix::mutateX

//^E	ExpectScalar
//^B	Scalar value expected
//^D	Scalar value must be provided as operand.

// Scalar expected error
struct ExpectScalarError : ExecError {
	VType type;
	VDatum &val;
	Prefix *where;

	ExpectScalarError (VType type, VDatum &val, Prefix *where) :
		val(val), type(type) { this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting scalar instead of ");
		log.log_type_name (type);
		log.put_ch (':');
		log.log_value (type, val);
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
		}
	}	// _report

	};

// Scalarize 'expr':
// extract scalar value from 'expr' (possibly changing it) to (type, val)
// returns 'true' on success
bool Prefix::scalarize (Expr *&expr, VType &type, VDatum &val) {
Expr *first;

if ((type = evalV_X (expr, val)) == T_list) {
	list_behead (val._list, first, expr);
	type = first->evalV (val, false);
	}

else {
	first = expr;
	expr = 0;
	}

switch (type) {
	case T_fixed:
	case T_float:
		relink_expr (first);
		return true;

	case T_string:
		val._string.refcount_by (1);
		relink_expr (first);
		val._string.refcount_by (-1);
		return true;

	default:
		Module::report (new ExpectScalarError (type, val, this));
		return false;
	}	// switch (type)

return false;
}	// Prefix::scalarize

//
// Scalar Unary operations
//

VType P_Unary::eval_Unary (VDatum &val, VType type1, VDatum &val1) {
return T_undef;		// (undefined by default)
}	// P_Unary::eval_Unary

VType P_Unary::evalV (VDatum &val, Expr *args) {
VType type1;
VDatum val1;

if (scalarize (args, type1, val1))
	return list_cons (eval_Unary (val, type1, val1), val, args);

return T_undef;
}	// P_Unary::evalV

VType P_Unary::eval_comb (VType type, VDatum &val, Expr *args) {
// (evaluate, if operand is scalar...)
if (T_scalar (type)) {
	VDatum val1 = val;
	return eval_Unary (val, type, val1);
	}

return T_undef;
}	// P_Unary::eval_comb

//
// Scalar Binary operations
//

VType P_Binary::eval_Binary (VDatum &val,
	VType type1, VDatum &val1, VType type2, VDatum &val2) {
return T_undef;		// (undefined by default)
}	// P_Binary::eval_Binary

VType P_Binary::evalV (VDatum &val, Expr *args) {
Expr *opnd1 = get_arg (args);

VType type1, type2;
VDatum val1, val2;

if (scalarize (opnd1, type1, val1) && (relink_expr (opnd1),
	scalarize (args, type2, val2))) {
	return list_cons (eval_Binary (val, type1, val1, type2, val2), val, args);
	}

relink_expr (args);

return T_undef;
}	// P_Binary::evalV

VType P_Binary::eval_comb (VType type, VDatum &val, Expr *args) {
VType type2;
VDatum val2;

if (T_scalar (type) && scalarize (args, type2, val2)) {
	VDatum val1 = val;
	return list_cons (eval_Binary (val, type, val1, type2, val2), val, args);
	}

return T_undef;
}	// P_Binary::eval_comb

VType P_Binary::eval_reduce (VDatum &val, Expr *args) {
Expr *final;
VType type = evalV_X (final = get_arg (args), val);

while (args) {
	VDatum val1, val2;
	VType type1 = evalV_X (final = get_arg (args), val1);

	val2 = val;				// (value literal copy)
	type = eval_Binary (val, type, val2, type1, val1);
	}

relink_expr (final);

return type;
}	// P_Binary::eval_reduce

//
//	Type check operation
//

VType P_IsType::evalV (VDatum &val, Expr *args) {
VType type = evalV_X (args, val);
relink_value (type, val);

val._fixed = type == t_check;
return T_fixed;
}	// P_IsType::evalV

//
//	Type expect wrapper
//

void P_ExpectType::eval (WrapX &wrapper, Expr *args) {
VDatum val;
VType type;
Expr *expr = get_arg (args);

if ((type = evalV_X (expr, val)) == t_expect) {
	relink_value (type, val);
	wrapper.eval (args);
	}
else {
	type_error (expr, t_expect, type, val);
	wrapper.refuse ();
	}
}	// P_ExpectType::eval

//
// Variable reference
//

Expr *&X_Variable::evalR () {
return name ? name->evalR() : ExprR::evalR();
}	// X_Variable::evalR

VType X_Variable::evalN (VDatum &val) {
val._var = this;
return T_var;
}	// X_Variable::evalN

bool X_Variable::identV (VType type, VDatum &val) {
if (type == T_var)
	return val._var->name == name;

return 0;
}	// X_Variable::identV

unsigned X_Variable::hash () {
return name ? name->hash : 0;
}	// X_Variable::hash

//
// Prefix expressions
//

VType X_Prefix::evalV (VDatum &val, bool full) {
val._pfx = prefix;
return T_prefix;
}	// X_Prefix::evalV

bool X_Prefix::identV (VType type, VDatum &val) {
return type == T_prefix && val._pfx == prefix;
}	// X_Prefix::identV

unsigned X_Prefix::hash () {
return prefix->hash ();
}	// X_Prefix::hash

//
// External resources
//

VType X_Extern::evalV (VDatum &val, bool full) {
val._extern = this;
return T_extern;
}	// X_Extern::evalV

//
//	Comparison functions
//

typedef S_fixed (*FNC_cmp) (int compared);

S_fixed cmp_lt (int cmp) { return cmp < 0; }
S_fixed cmp_gt (int cmp) { return cmp > 0; }
S_fixed cmp_le (int cmp) { return cmp <= 0; }
S_fixed cmp_ge (int cmp) { return cmp >= 0; }

S_fixed cmp_eq (int cmp) { return cmp == 0; }
S_fixed cmp_ne (int cmp) { return cmp != 0; }

S_fixed cmp_cmp (int cmp) { return cmp; }

static S_fixed
	ccc_lt [3] = { 1, 0, 0 },
	ccc_le [3] = { 1, 1, 0 },
	ccc_gt [3] = { 0, 0, 1 },
	ccc_ge [3] = { 0, 1, 1 },
	ccc_eq [3] = { 0, 1, 0 },
	ccc_ne [3] = { 1, 0, 1 },

	ccc_cmp [3] = { -1, 0, 1 };

//
// Statement/block execultion
//

// Execute block statements (excluding result)
Except * X_Block::exec () {
Except *xcpt;
for (SNode *node = body; node; node = node->next)
	if (xcpt = evalZ_X (node->expr))
		return xcpt;

return 0;
}	// X_Block::exec

VType X_Block::evalV (VDatum &val, bool full) {
if (full) {
	Except *xcpt = exec ();
	if (xcpt) {
		val._except = xcpt;
		return T_xcpt;
		}

	return evalV_X (last, val);
	}	// full evaluation

val._block = this;
return T_block;
}	// X_Block::evalV

Expr *X_Block::evalX () {
Except *xcpt = exec ();
return xcpt ? xcpt : evalX_X (last);
}	// X_Block::evalX

bool X_Block::identV (VType type, VDatum &val) {
if (type == T_block) {
	SNode *l_node = body, *r_node = val._block->body;

	while (l_node && r_node) {
		if (! identX (l_node->expr, r_node->expr)) return false;
		l_node = l_node->next, r_node = r_node->next;
		}	// (while)

	return l_node || r_node ?
		false :
		identX (last, val._block->last);
	}	// (T_block)

return false;
}	// X_Block::identV

unsigned X_Block::hash () {
unsigned hash = hashX (last);

for (SNode *node = body; node; node = node->next)
	hash ^= hashX (node->expr);

return hash;
}	// X_Block::hash

//
// Some scalar coercions
//

// Expect fixed value
DL_EXPORT S_fixed Prefix::expect_fixed (Expr *expr, S_fixed defval) {
if (expr) {
	VDatum val;
	VType type = expr->evalV (val, true);
	relink_expr (expr);

	if (T_scalar (type))
		return to_fixed (type, val);
	else
		Module::report (new ExpectScalarError (type, val, this));
	}	// (expr)

// (not defined -- use default)
return defval;
}	// Prefix::expect_fixed

// Expect float value
DL_EXPORT S_float Prefix::expect_float (Expr *expr, S_float defval) {
if (expr) {
	VDatum val;
	VType type = expr->evalV (val, true);
	relink_expr (expr);

	if (T_scalar (type))
		return to_float (type, val);
	else
		Module::report (new ExpectScalarError (type, val, this));
	}	// (expr)

// (not defined -- use default)
return defval;
}	// Prefix::expect_float

// Expect string contents
// (returns true, if OK)
DL_EXPORT bool Prefix::expect_string (Expr *expr, S_string &s_val) {
if (expr) {
	VDatum val;
	VType type = expr->evalV (val, true);

	// TODO: lock string result?
	relink_expr (expr);
	if (T_scalar(type)) {
		coerce_string (type, val);
		s_val = val._string;
		return true;
		}
	else
		Module::report (new ExpectScalarError (type, val, this));
	} // (expr)

s_val.s_clear ();
return false;
}	// Prefix::expect_string

// Expect range
DL_EXPORT void Prefix::expect_range (Expr *range, FixedRange &result) {
Expr *first = get_arg (range);

if (range) {
	result.from = expect_fixed (first, result.from);
	result.to = expect_fixed (range, result.to);
	}
else {
	// Hack: lists like (From .. ) are preserved as lists...
	if (arg_type == T_list)
		result.from = expect_fixed (first, result.from);

	else {
		result.to = expect_fixed (first, result.to);
		result.from = 0;
		}
	}
}	// Prefix::expect_range

// Evaluate range to list
DL_EXPORT X_List *FixedRange::eval () {
return
	new ("Range") X_List (
		new ("Range/From") X_Fixed (from),
		new ("Range/To") X_Fixed (to)
		);
}	// FixedRange::eval

//^E	ExpectBool
//^B	Boolean value expected
//^D	Boolean value must be provided in this context.

// Boolean expected error
struct ExpectBoolError : ExecError {
	VType type;
	VDatum &val;
	Prefix *where;

	ExpectBoolError (VType type, VDatum &val, Prefix *where) :
		val(val), type(type) { this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting boolean instead of ");
		log.log_type_name (type);
		log.put_ch (':');
		log.log_value (type, val);
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		}	// _report

	};

// Evaluate expression as boolean (reporting errors)
// (returns true/1 or false/0)
DL_EXPORT bool Prefix::expect_bool (Expr *expr, bool defval) {
VDatum val;

if (expr) {
VType type = evalV_X (expr, val);
relink_expr (expr);

switch (type) {
	case T_undef:
		return false;			// (indefinite value is false)

	case T_fixed:
		return val._fixed != 0;

	case T_float:
		return val._float != 0.0;

	case T_string:
		return val._string.length != 0;

	default:
		Module::report (new ExpectBoolError (type, val, this));
	}
}

// All other values are true values!
return defval;
}	// Prefix::expect_bool

//
//	Failed to expect needed type (in prefix)
//

// Type expected error
struct ExpectTypeError : ExecError {
	Expr *expr;
	VType ex_type;			// (type expected)

	VType type;				// (result...)
	VDatum &val;

	Prefix *where;

	ExpectTypeError (Expr *expr, VType ex_type, VType type, VDatum &val, Prefix *where) :
		val(val), type(type)
		{ this->expr = expr; this->ex_type = ex_type; this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting ");
		log.log_type_name (ex_type);
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (" (instead of ");
		log.log_expr (expr);
		log.put_ch (':');
		log.log_value (type, val);
		log.put_ch (')');
		}	// _report

	};

// Type expected error
struct WrongExternError : ExecError {
	Expr *expr;
	Prefix *where;
	char const *sign_expected;
	char const *sign_found;

	WrongExternError (Expr *expr, Prefix *where, char const *sign_expected, char const *sign_found) {
		this->expr = expr; this->where = where;
		this->sign_expected = sign_expected; this->sign_found = sign_found;
		}

	void _report (Logger &log) {
		log.put_cstr ("Expecting external ");
		log.log_expr (expr);
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (" type: ");
		log.put_cstr (sign_expected);
		log.put_cstr (" instead of: ");
		log.put_cstr (sign_found);
		}	// _report

	};

// (simple wrapper for error reporting)
void Prefix::type_error (Expr *expr, VType ex_type, VType type, VDatum &val) {
Module::report (new ExpectTypeError (expr, ex_type, type, val, this));
}	// Prefix::type_error

static bool valid_sign (char const *expected, char const *found) {
char ch;

if (expected == found)
	return true;

while ((ch = *expected ++) == *found ++)
	if (! ch) return true;

return false;
}	// valid_sign

// Expect external from argument 'arg'
// (reporting errors)
DL_EXPORT External *Prefix::expect_external (char const *sign, Expr *arg) {
if (arg) {
	VDatum val;
	VType type;

	if ((type = evalV_X (arg, val)) == T_extern) {
		if (valid_sign (sign, val._extern->sign)) {
			External *_external = val._extern->external;
			relink_expr (val._extern);
			return _external;
			}
		else {
			Module::report (new WrongExternError (arg, this, sign, val._extern->sign));
			return 0;
			}
		}

	type_error (arg, T_extern, type, val);
	}

return 0;
}	// Prefix::expect_external

// Evaluate as C-string
DL_EXPORT char *eval_cstring (Expr *expr, Prefix *where) {
S_string str;
if (where->expect_string (expr, str)) {
	char *cstr = str.to_cstring ();
	str.relink ();
	return cstr;
	}

return 0;
}	// eval_cstring

// Assign fixed value to (presumed mutable) 'm_expr'
DL_EXPORT bool assign_fixed (Expr *m_expr, S_fixed value) {
VDatum val;
val._fixed = value;
return mutateX_V (m_expr, T_fixed, val);
}	// assign_fixed

// Assign float value to (presumed mutable) 'm_expr'
DL_EXPORT bool assign_float (Expr *m_expr, S_float value) {
VDatum val;
val._float = value;
return mutateX_V (m_expr, T_float, val);
}	// assign_float

// Evaluate 'func (args)' as predicate
// Return false on failure
DL_EXPORT bool apply_bool (Prefix *func, Expr *args) {
VDatum val;
VType type = func->evalV (val, args);

switch (type) {
	case T_undef:
		return false;			// (indefinite value is false)

	case T_fixed:
		return val._fixed != 0;

	case T_float:
		return val._float != 0.0;

	case T_string:
		return val._string.length != 0;

	default:
		Module::report (new ExpectBoolError (type, val, 0));
	}	// switch (type)

return false;
}	// apply_bool

// Type funccall expected error
struct ExpectCallTypeError : ExecError {
	Prefix *pfx;
	Expr *args;
	VType ex_type;			// (type expected)

	VType type;				// (result...)
	VDatum &val;

	Prefix *where;

	ExpectCallTypeError (Prefix *pfx, Expr *args, VType ex_type, VType type, VDatum &val, Prefix *where) :
		val(val), type(type)
		{ this->pfx = pfx; this->args = args; this->ex_type = ex_type; this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting ");
		log.log_type_name (ex_type);
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (" (instead of ");
		log.log_prefix (pfx);
		log.put_ch ('^');
		log.log_expr (args);
		log.put_ch (':');
		log.log_value (type, val);
		log.put_ch (')');
		}	// _report

	};

// (From "E_String.cpp")
S_fixed parse_fixed (S_string &s_str);

// Evaluate 'func(args)' as fixed value
// (Return 'defval' on type failure.)
DL_EXPORT S_fixed apply_fixed (Prefix *func, Expr *args, S_fixed defval) {
VDatum val;
VType type;

switch (type = func->evalV (val, args)) {
	case T_undef:
		break;

	case T_fixed:
		return val._fixed;

	case T_float:
		return (S_fixed) val._float;

	case T_string:
		return parse_fixed (val._string);

	default:
		Module::report (new ExpectCallTypeError (func, args, T_fixed, type, val, 0));
	}	// switch

return defval;			// (default value)
}	// apply_fixed

// (In "E_String.cpp")
S_float parse_float (S_string &s_str);

// Evaluate 'func(args)' as float value
// Return 'defval' on type failure
DL_EXPORT S_float apply_float (Prefix *func, Expr *args, S_float defval) {
VDatum val;
VType type;

switch (type = func->evalV (val, args)) {
	case T_undef:
		break;

	case T_fixed:
		return (S_float) val._fixed;

	case T_float:
		return val._float;

	case T_string:
		return parse_float (val._string);

	default:
		Module::report (new ExpectCallTypeError (func, args, T_float, type, val, 0));
	}	// switch

return defval;
}	// apply_float

//
//
//	Release expressions
//
//

void Expr::release () {
delete this;
}	// Expr::release

void X_Variable::release () {
unlink_name (name);

Expr::release ();
}	// X_Variable::release

void X_List::release () {
unlink_expr (first);
unlink_expr (next);

Expr::release ();
}	// X_List::release

void X_Term::release () {
unlink_prefix (prefix);
unlink_expr (args);

Expr::release ();
}	// X_Term::release

void X_Prefix::release () {
unlink_prefix (prefix);

Expr::release ();
}	// X_Prefix::release

void X_Block::SNode::release () {
unlink_expr (expr);

delete this;
}	// X_Block::SNode::release

void X_Block::release () {
SNode *node = body;

while (node) {
	SNode *next = node->next;
	node->release ();
	node = next;
	}

unlink_expr (last);
Expr::release ();
}	// X_Block::release

void X_Extern::release () {
// (warning: external alloc/dealloc is controlled elsewhere...)
Expr::release ();
}	// X_Extern::release

//
//	Initialize/uninitialize
//

void init_eval () {
X_Fixed *ptr = predef_fixed + PD_FIXED_MAX - PD_FIXED_MIN;
do {
	(-- ptr)->refs = 1;
	ptr->value = PD_FIXED_MIN + (ptr - predef_fixed);
	} while (ptr != predef_fixed);
}	// init_eval

void shut_eval () {
X_Fixed *ptr = predef_fixed + PD_FIXED_MAX - PD_FIXED_MIN;
do {
	if ((-- ptr)->refs != 1) {
		syslog->put_cstr ("Predef fixed count error: ")->log_fixed (ptr->refs)->
			put_cstr (" (at ")->log_fixed (ptr->value)->put_cstr (").\n");
		}
	} while (ptr != predef_fixed);
}	// shut_eval

//
//	Invoke prefix with args
//

DL_EXPORT void invoke (Prefix *pfx, Expr *args) {
VDatum val;

if (pfx) {
	link_expr (args);
	relink_value (pfx->evalV (val, args), val);
	unlink_expr (args);
	}
}	// invoke


//		[Categories]

//^C	Scalar
//^B	Scalar functors
//^D	Functors, operating on scalar values.

//^C	Nullary
//^B	Nullary functors
//^D	Functors, expecting no arguments.

//^C	Unary
//^B	Unary functors
//^D	Functors, expecting one argument.

//^C	Binary
//^B	Binary functors
//^D	Functors, expecting two arguments.

//^C	Ternary
//^B	Ternary functors
//^D	Functors, expecting three arguments.

//^C	Coercion
//^B	Coercion functors
//^D	Scalar coercion unary functors.

//^C	Compare
//^B	Comparator functors
//^D	Functors, performing ordered compare (returning sign value).

//^C	Constructor
//^B	Constructor functors
//^D	Functors, creating instances of complex types.

//^C	Destructor
//^B	Destructor functors
//^D	Functors, destroying instances of complex types.

//^C	Wrapper
//^B	Wrapper functors
//^D	Functors, evaluating some argument(s) with additional prologue/epilogue.

//^C	Meta
//^B	Meta-operations
//^D	Implicit meta-operations (expecting terms as operands).

//		[Types]

//^T	Scalar
//^B	Scalar value.
//^D	Anything evaluating to scalar (number or string) value.

//^T	Any
//^B	Any type allowed
//^D	No constraints implied.

//		--------

