
/*

	+---+---+---+---+---+---+
	|	"E_Control.cpp":
	|	Control primaries (conditionals and iterators).
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

#include "Logger.h"

//
//	Boolean literal primitives
//

struct P_Boolean : Prefix {
	bool value;			// actual result

	P_Boolean (char const *ident, O_Enum op, bool value) : Prefix (ident, op)
		{ this->value = value; }

	VType evalV (VDatum &val, Expr *args) {
		val._fixed = value;
		return T_fixed;
		}	// evalV
	};

//
//	Integer range check
//

enum {
	MIN_FIXED = 0x80000000,
	MAX_FIXED = 0x7FFFFFFF
	};

struct P_RangeCheck : Prefix {
	bool polarity;		// ? (inside range) : (outside range)

	P_RangeCheck (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	D_Prefix_evalV;
	};

VType P_RangeCheck::evalV (VDatum &val, Expr *args) {
S_fixed value = expect_fixed (get_arg (args), 0);

FixedRange range (MIN_FIXED, MAX_FIXED);
expect_range (args, range);

val._fixed =
	polarity ?
		(range.from <= value && value < range.to):
		(value < range.from || range.to <= value);
return T_fixed;
}	// P_RangeCheck::evalV

//
//	Range constructor
//

struct P_RangeBy : PrefixX {
	P_RangeBy (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
	S_fixed first = expect_fixed (get_arg (args), 0);
	S_fixed offset = expect_fixed (args, 1);
	FixedRange range (first, first + offset);

	return range.eval ();
	}	// evalX
	};

//
//	Assert condition
//

// Assertion failure
struct AssertError : ExecError {
	Expr *asserted;

	AssertError (Expr *asserted) {
		this->asserted = asserted;
		}

	void _report (Logger &log) {
		log.put_cstr ("Assertion failed: ");
		log.log_expr (asserted);
		}	// _report

	};

struct P_Assert : Prefix {
	P_Assert (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		bool result = expect_bool (args, false);
		
		if (! result)
			Module::report (new AssertError (args));

		val._fixed = result;
		return T_fixed;
		}	// evalV
	};

//
//	Conditional inversion
//

struct P_NotCnd : Prefix {
	P_NotCnd (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		val._fixed = !expect_bool (args, false);
		return T_fixed;
		}	// evalV
	};

//
//	Conditional binary
//

struct P_BinaryCond : Prefix {
	bool polarity;			// polarity ? <OR> : <AND>

	P_BinaryCond (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	D_Prefix_evalV;
	};

VType P_BinaryCond::evalV (VDatum &val, Expr *args) {
if (args) {
bool result = expect_bool (get_arg (args), polarity);

if (args) {
	if (result != polarity)
		return args->evalV (val, true);
	else
		val._fixed = polarity;
	}
else	// (no arguments or exception condition)
	val._fixed = (result != polarity);

return T_fixed;
}	// (any args)

else return T_undef;		// (nothing to evaluate)
}	// P_BinaryCond::evalV

//
//	Conditional ternary
//

struct P_TernaryCond : Prefix {
	bool polarity;			// polarity ? <Unless> : <If>

	P_TernaryCond (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	D_Prefix_evalV;
	};

VType P_TernaryCond::evalV (VDatum &val, Expr *args) {
if (args) {
bool result = expect_bool (get_arg (args), polarity);

if (args) {
	X_List *list = args->isList ();
	return
		list ?
			evalV_X (result != polarity ? list->first : list->next, val)
		:
			(result != polarity ? evalV_X (args, val) : T_undef);
	}
else {	// (no arguments or exception condition)
	val._fixed = (result != polarity);
	return T_fixed;
	}
}	// (any args)

return T_undef;
}	// P_TernaryCond::evalV

//
//	Conditional iterator
//

struct P_CondIter : P_Iterator {
	bool polarity;			// polarity ? <Unless> : <If>
	bool postcond;			// postcond ? postconditional : preconditional

	P_CondIter (char const *ident, O_Enum op, bool polarity, bool postcond):
		P_Iterator (ident, op)
		{ this->polarity = polarity; this->postcond = postcond; }

	D_P_Iterator_evaluate;
	};

void P_CondIter::evaluate (IterContext &IC, Expr *args) {
if (args) {
Expr *cond = get_arg (args);
bool polarity = this->polarity;
if (args) {
	IC.start (args);

	if (postcond)
		while (IC.next () && expect_bool (cond, polarity) != polarity);
	else
		while (expect_bool (cond, polarity) != polarity && IC.next ());
	}

else while (expect_bool (cond, polarity) != polarity);
}	// (any args)
}	// P_CondIter::evaluate

//
//	Integer sequence iterator
//

struct P_ForIter : P_Iterator {
	bool dir;		// ? decrement order : increment order

	P_ForIter (char const *ident, O_Enum op, bool dir) : P_Iterator (ident, op)
		{ this->dir = dir; }

	D_P_Iterator_evaluate;
	};

void P_ForIter::evaluate (IterContext &IC, Expr *args) {
Expr *&R_index = expectR_X (get_arg (args));

FixedRange range (0, 0);
expect_range (get_arg (args), range);

if (range.notempty ()) {
VDatum V_index;
S_fixed &i_index = V_index._fixed;

if (args) {
IC.start (args);

if (dir) {
	// (decremental order)
	i_index = range.to;
	do {
		i_index --;
		mutateR_V (R_index, T_fixed, V_index);
		} while (IC.next () && i_index != range.from);
	}	// (decrementing)

else {
	// (incremental order)
	i_index = range.from;
	do {
		mutateR_V (R_index, T_fixed, V_index);
		} while (IC.next () && ++ i_index != range.to);
	}	// (incrementing)
}	// (args)

else {
	// (do nothing, except modifiing index variable)
	i_index = dir ? range.to - 1 : range.from;
	mutateR_V (R_index, T_fixed, V_index);
	}
}	// (i_from < i_to)

}	// P_ForIter::evaluate

//
//	Times iterator
//

struct P_TimesIter : P_Iterator {
	P_TimesIter (char const *ident) : P_Iterator (ident, Op_Times) {}

	D_P_Iterator_evaluate;
	};

void P_TimesIter::evaluate (IterContext &IC, Expr *args) {
S_fixed count = expect_fixed (get_arg (args), 0);

if (args && count >= 0) {
	IC.start (args);
	while (count -- && IC.next ());
	}
}	// P_TimesIter::evaluate

//
//	Repeat forever
//

struct P_EverIter : P_Iterator {
	P_EverIter (char const *ident) : P_Iterator (ident, Op_Null) {}

	D_P_Iterator_evaluate;
	};

void P_EverIter::evaluate (IterContext &IC, Expr *args) {
if (args) {
	IC.start (args);
	while (IC.next ());
	}
}	// P_EverIter::evaluate

//
//	Operation table
//

static bool init_primaries_control (int order) {

//		[Categories]

//^C	Predicate
//^B	Predicate functors
//^D	Functors, returning boolean values.

//^C	Conditional
//^B	Conditional functors
//^D	Functors with conditional evaluation of some argument(s).

//^C	Iterator
//^B	Iterator functors
//^D	Functors with repeated evaluation of some argument(s).

//		[Types]

//^T	Bool
//^B	Boolean value.
//^D	Anything treated as boolean condition.
//^D	As argument: 0, "" and !undef as !false; or anything other as !true.
//^D	As result: exactly 0 (as !false) or 1 (as !true).

//		--------

//^G	false true

//^N	false [Scalar | Nullary | Conditional]
//^P	false () => Bool
//^B	Literal false value.
//^D	Returns !false value (always 0).

//^N	true [Scalar | Nullary | Conditional]
//^P	true () => Bool
//^B	Literal true value.
//^D	Returns !true value (always 1).

	DefBuiltin (P_Boolean ("false", Op_False,	false));
	DefBuiltin (P_Boolean ("true", Op_True,		true));

//^G	assert

//^N	assert [Unary | Conditional]
//^P	assert (Cond: Bool) => Bool
//^B	Assertion check.
//^D	Reports error, if condition \Cond failed (not true).
//^D	Returns \Cond.

	DefBuiltin (P_Assert ("assert", Op_Null));

//^G	c_not c_and c_or

//^N	c_not [Conditional | Unary]
//^P	c_not (Cond: Bool) => Bool
//^B	Conditional "NOT" operation.
//^D	If boolean condition \Cond is !false, returns !true (1); otherwise returns !false (0).
//^D	Equivalent to: !if (\Cond, 0, 1).

	DefBuiltin (P_NotCnd ("c_not", Op_CNot));

//^N	c_and [Conditional | Binary]
//^P	c_and (Cond: Bool, @Body: Any) => 0 | Any
//^B	Conditional short-circuit "AND" operation.
//^D	If boolean condition \Cond is !true,
//^\	evaluates and returns \Body; otherwise returns !false (0).
//^D	Equivalent to: !if (\Cond, \Body, 0).

//^N	c_or [Conditional | Binary]
//^P	c_or (Cond: Bool, @Body: Any) => 1 | Any
//^B	Conditional short-circuit "OR" operation.
//^D	If boolean condition \Cond is !false,
//^\	evaluates and returns \Body; otherwise returns !true (1).
//^D	Equivalent to: !unless (\Cond, \Body, 1).

	DefBuiltin (P_BinaryCond ("c_and", Op_CAnd,	false));
	DefBuiltin (P_BinaryCond ("c_or", Op_COr,	true));

//^G	if unless

//^N	if [Conditional | Ternary]
//^P	if (Cond: Bool, @Then: Any1 [, @Else: Any2]) => Any1 | Any2
//^B	Conditional positive test.
//^D	If boolean \Cond is !true, evaluates and returns \Then;
//^\	otherwise evaluates and returns \Else.

//^N	unless [Conditional | Ternary]
//^P	unless (Cond: Bool, @Else: Any1 [, @Then: Any2]) => Any1 | Any2
//^B	Conditional negative test.
//^D	If boolean \Cond is !false, evaluates and returns \Else;
//^\	otherwise evaluates and returns \Then.

	DefBuiltin (P_TernaryCond ("if", Op_If,			false));
	DefBuiltin (P_TernaryCond ("unless", Op_Unless,	true));

//^G	while until do_while do_until

//^N	while [Iterator | Conditional | Binary]
//^P	while (@Cond: Bool, @Body: Any) => Any
//^B	Iterator with positive precondition.
//^D	While boolean \Cond remains !true, repeatedly evaluates \Body.
//^D	Returns final value of \Body (or !undef, if \Body never was evaluated).

//^N	until [Iterator | Conditional | Binary]
//^P	until (@Cond: Bool, @Body: Any) => Any
//^B	Iterator with negative precondition.
//^D	While boolean \Cond remains !false, repeatedly evaluates \Body.
//^D	Returns final value of \Body (or !undef, if \Body never was evaluated).

	DefBuiltin (P_CondIter ("while", Op_WhilePre,		false, false));
	DefBuiltin (P_CondIter ("until", Op_UntilPre,		true, false));

//^N	do_while [Iterator | Conditional | Binary]
//^P	do_while (@Cond: Bool, @Body: Any) => Any
//^B	Iterator with positive postcondition.
//^D	Repeatedly pre-evaluates \Body, while \Cond remains !true.
//^D	Returns final value of \Body (which is always evaluated at least once).

//^N	do_until [Iterator | Conditional | Binary]
//^P	do_until (@Cond: Bool, @Body: Any) => Any
//^B	Iterator with negative postcondition.
//^D	Repeatedly pre-evaluates \Body, while \Cond remains !false.
//^D	Returns final value of \Body (which is always evaluated at least once).

	DefBuiltin (P_CondIter ("do_while", Op_WhilePost,	false, true));
	DefBuiltin (P_CondIter ("do_until", Op_UntilPost,	true, true));

//^G	for_inc for_dec times ever

//^N	for_inc [Iterator | Ternary]
//^P	for_inc (Index: Mutable, R: Range, @Body: Any) => Any
//^B	Incremental range iterator.
//^D	Increments value of mutable \Index,
//^\	starting from (including) start of range \R up to (excluding) end of range \R,
//^\	evaluating \Body for each value of \Index.
//^D	Returns final value of \Body (or !undef, if \Body never was evaluated).

//^N	for_dec [Iterator | Ternary]
//^P	for_dec (Index: Mutable, R: Range, @Body: Any) => Any
//^B	Decremental range iterator.
//^D	Decrements value of mutable \Index,
//^\	starting from (excluding) end of range \R down to (including) start of range \R,
//^\	evaluating \Body for each value of \Index.
//^D	Returns final value of \Body (or !undef, if \Body never was evaluated).

	DefBuiltin (P_ForIter ("for_inc", Op_ForInc, false));
	DefBuiltin (P_ForIter ("for_dec", Op_ForDec, true));

//^N	times [Iterator | Binary]
//^P	times (Count: Int, @Body: Any) => Any
//^B	Fixed count iterator.
//^D	Evaluates \Body precisely \Count times (never, if \Count <= 0).
//^D	Returns final value of \Body (or !undef, if \Body never was evaluated).

	DefBuiltin (P_TimesIter ("times"));

//^N	ever [Iterator | Unary]
//^P	ever (@Body: Any) => Any
//^B	Iterate forever.
//^D	Evaluates \Body infinitely (or until exception interrupts).
//^D	Returns final value of \Body.

	DefBuiltin (P_EverIter ("ever"));

//^G	inside outside

//^N	inside [Numeric | Predicate]
//^P	inside (Value: Int, R: Range) => Bool
//^B	Check for value inclusion in range.
//^D	True, if integer \Value is inside range \R
//^D	(same as: \R[0] <= \Value && \Value < \R[1]).

//^N	outside [Numeric | Predicate]
//^P	outside (Value: Int, R: Range) => Bool
//^B	Check for value exclusion from range.
//^D	True, if integer \Value is outside range \R
//^D	(same as: \Value < \R[0] || \R[1] <= \Value).

	DefBuiltin (P_RangeCheck ("inside", Op_InRange, true));
	DefBuiltin (P_RangeCheck ("outside", Op_OutRange, false));

//^N	range [Numeric | List]
//^P	range (From: Int, [By: Int]) => Range
//^B	Range by offset.
//^D	Return range \From..\From+\By.
//^D	(If \By omitted, 1 is assumed.)

	DefBuiltin (P_RangeBy ("range", Op_Null));

return true;
}	// init_primaries_control

DefSubSystem ("control", init_primaries_control, 0);

