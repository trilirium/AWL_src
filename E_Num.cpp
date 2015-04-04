
/*

	+---+---+---+---+---+---+
	|	"E_Num.cpp":
	|	Numeric scalar primaries (fixed & float).
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include <math.h>
#include <time.h>

#include "Eval.h"
#include "Logger.h"

//
// Numeric check
//

struct P_IsNum : Prefix {
	P_IsNum (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		VType type = evalV_X (args, val);
		relink_value (type, val);

		val._fixed = type == T_fixed || type == T_float;
		return T_fixed;
		}	// evalV
	};

//
//	Primitive types
//

typedef S_fixed (*opUnary_fixed) (S_fixed);
typedef S_float (*opUnary_float) (S_float);

typedef S_fixed (*opBinary_fixed) (S_fixed, S_fixed);
typedef S_float (*opBinary_float) (S_float, S_float);

typedef S_fixed (*opCmp_fixed) (S_fixed, S_fixed);
typedef S_fixed (*opCmp_float) (S_float, S_float);

//
//	Primitives implementation
//

// Integer division by zero
struct ZeroDivError : ExecError {
	S_fixed numerator;

	ZeroDivError (S_fixed numerator) { this->numerator = numerator; }

	void _report (Logger &log) {
		log.put_cstr ("Zero division of ")->log_fixed (numerator);
		}	// _report

	};

//^E	ZeroDivide
//^B	Integer zero division.
//^D	Attempt to divide integer value by zero.

static void zero_divide (S_fixed numerator) { Module::report (new ZeroDivError (numerator)); }

static S_fixed val_fixed (S_fixed val1) { return val1; }

static S_float val_float (S_float val1) { return val1; }

static S_fixed abs_fixed (S_fixed val1) {
return val1 >= 0 ? val1 : -val1;
}	// abs_fixed

static S_float abs_float (S_float val1) {
return val1 >= 0.0 ? val1 : -val1;
}	// abs_float

static S_fixed sgn_fixed (S_fixed val1) {
return (val1 < 0)? -1 : (val1 > 0)? 1 : 0;
}	// sgn_fixed

static S_float sgn_float (S_float val1) {
return (val1 < 0.0)? -1.0 : (val1 > 0.0)? 1.0 : 0.0;
}	// sgn_float

static S_fixed neg_fixed (S_fixed val1) {
return -val1;
}	// neg_fixed

static S_float neg_float (S_float val1) {
return -val1;
}	// neg_float

static S_fixed not_fixed (S_fixed val1) {
return ~val1;
}	// not_fixed

static S_fixed add_fixed (S_fixed val1, S_fixed val2) {
return val1 + val2;
}	// add_fixed

static S_float add_float (S_float val1, S_float val2) {
return val1 + val2;
}	// add_float

static S_fixed sub_fixed (S_fixed val1, S_fixed val2) {
return val1 - val2;
}	// sub_fixed

static S_float sub_float (S_float val1, S_float val2) {
return val1 - val2;
}	// sub_float

static S_fixed mul_fixed (S_fixed val1, S_fixed val2) {
return val1 * val2;
}	// mul_fixed

static S_float mul_float (S_float val1, S_float val2) {
return val1 * val2;
}	// mul_float

static S_float div_float (S_float val1, S_float val2) {
return val1 / val2;
}	// div_float

static S_fixed idiv_fixed (S_fixed val1, S_fixed val2) {
if (! val2) {
	zero_divide (val1);
	return 0;
	}
return val1 / val2;
}	// idiv_fixed

static S_fixed irem_fixed (S_fixed val1, S_fixed val2) {
if (! val2) {
	zero_divide (val1);
	return 0;
	}
return val1 % val2;
}	// irem_fixed

static S_fixed rdiv_fixed (S_fixed val1, S_fixed val2) {
if (! val2) {
	zero_divide (val1);
	return 0;
	}
return (val1 >= 0 ? val1 : val1 - val2 + 1) / val2;
}	// rdiv_fixed

static S_fixed rrem_fixed (S_fixed val1, S_fixed val2) {
if (! val2) {
	zero_divide (val1);
	return 0;
	}
return val1 >= 0 ? val1 % val2 : val2 - (- val1-1) % val2 - 1;
}	// rrem_fixed

static S_fixed shl_fixed (S_fixed val1, S_fixed val2) {
return val2 >= 0 ? val1 << val2 : val1 >> -val2;
}	// shl_fixed

static S_fixed shr_fixed (S_fixed val1, S_fixed val2) {
return val2 >= 0 ? val1 >> val2 : val1 << -val2;
}	// shr_fixed

static S_fixed sll_fixed (S_fixed val1, S_fixed val2) {
return val2 >= 0 ? (unsigned)val1 << val2 : (unsigned)val1 >> -val2;
}	// sll_fixed

static S_fixed slr_fixed (S_fixed val1, S_fixed val2) {
return val2 >= 0 ? (unsigned)val1 >> val2 : (unsigned)val1 << -val2;
}	// slr_fixed

static S_fixed min_fixed (S_fixed val1, S_fixed val2) {
return val1 < val2 ? val1 : val2;
}	// min_fixed

static S_float min_float (S_float val1, S_float val2) {
return val1 < val2 ? val1 : val2;
}	// min_float

static S_fixed max_fixed (S_fixed val1, S_fixed val2) {
return val1 > val2 ? val1 : val2;
}	// max_fixed

static S_float max_float (S_float val1, S_float val2) {
return val1 > val2 ? val1 : val2;
}	// max_float

static S_fixed cmp_fixed (S_fixed val1, S_fixed val2) {
return val1 < val2 ? -1 : val1 > val2 ? 1 : 0;
}	// cmp_fixed

static S_fixed ucmp_fixed (S_fixed val1, S_fixed val2) {
return
	(unsigned) val1 < (unsigned) val2 ? -1 :
	(unsigned) val1 > (unsigned) val2 ? 1 : 0;
}	// ucmp_fixed

static S_fixed cmp_float (S_float val1, S_float val2) {
return val1 < val2 ? -1 : val1 > val2 ? 1 : 0;
}	// cmp_float

static S_fixed lt_fixed (S_fixed val1, S_fixed val2) {
return val1 < val2;
}	// lt_fixed

static S_fixed lt_float (S_float val1, S_float val2) {
return val1 < val2;
}	// lt_float

static S_fixed gt_fixed (S_fixed val1, S_fixed val2) {
return val1 > val2;
}	// gt_fixed

static S_fixed gt_float (S_float val1, S_float val2) {
return val1 > val2;
}	// gt_float

static S_fixed le_fixed (S_fixed val1, S_fixed val2) {
return val1 <= val2;
}	// le_fixed

static S_fixed le_float (S_float val1, S_float val2) {
return val1 <= val2;
}	// le_float

static S_fixed ge_fixed (S_fixed val1, S_fixed val2) {
return val1 >= val2;
}	// ge_fixed

static S_fixed ge_float (S_float val1, S_float val2) {
return val1 >= val2;
}	// ge_float

static S_fixed eq_fixed (S_fixed val1, S_fixed val2) {
return val1 == val2;
}	// eq_fixed

static S_fixed eq_float (S_float val1, S_float val2) {
return val1 == val2;
}	// eq_float

static S_fixed ne_fixed (S_fixed val1, S_fixed val2) {
return val1 != val2;
}	// ne_fixed

static S_fixed ne_float (S_float val1, S_float val2) {
return val1 != val2;
}	// ne_float

static S_fixed and_fixed (S_fixed val1, S_fixed val2) {
return val1 & val2;
}	// and_fixed

static S_fixed or_fixed (S_fixed val1, S_fixed val2) {
return val1 | val2;
}	// or_fixed

static S_fixed xor_fixed (S_fixed val1, S_fixed val2) {
return val1 ^ val2;
}	// xor_fixed

static S_float sqr_float (S_float val1) {
return sqrt (val1);
}	// sqr_float

static S_float exp_float (S_float val1) {
return exp (val1);
}	// exp_float

static S_float log_float (S_float val1) {
return log (val1);
}	// log_float

static S_float expby_float (S_float val1, S_float val2) {
return pow (val1, val2);
}	// expby_float

static S_float logby_float (S_float val1, S_float val2) {
return log (val2) / log (val1);
}	// logby_float

static S_float pi_float (S_float val1) {
return val1 * M_PI;
}	// pi_float

static S_float sin_float (S_float val1) {
return sin (val1);
}	// sin_float

static S_float cos_float (S_float val1) {
return cos (val1);
}	// cos_float

static S_float tan_float (S_float val1) {
return tan (val1);
}	// tan_float

static S_float asin_float (S_float val1) {
return asin (val1);
}	// asin_float

static S_float acos_float (S_float val1) {
return acos (val1);
}	// acos_float

static S_float atan_float (S_float val1) {
return atan (val1);
}	// atan_float

static S_float sinh_float (S_float val1) {
return sinh (val1);
}	// sinh_float

static S_float cosh_float (S_float val1) {
return cosh (val1);
}	// cosh_float

static S_float tanh_float (S_float val1) {
return tanh (val1);
}	// tanh_float

static S_float rad_float (S_float val1, S_float val2) {
return hypot (val1, val2);
}	// rad_float

static S_float ang_float (S_float val1, S_float val2) {
return val1 == 0.0 && val2 == 0.0 ? 0.0 / 0.0 : atan2 (val2, val1);
}	// ang_float

static S_float floor_float (S_float val1) {
return floor (val1);
}	// floor_float

static S_float ceil_float (S_float val1) {
return ceil (val1);
}	// ceil_float

//
//	Bit search/count
//

// Locate highest bit set in 'val'
static S_fixed bit_set_hi (S_fixed val) {
if (val) {
	unsigned count = 0;
	if (val & 0xFFFF0000)
		{ count += 16; val >>= 16; }
	if (val & 0xFF00)
		{ count += 8; val >>= 8; }
	if (val & 0xF0)
		{ count += 4; val >>= 4; }

	count += 4;
	do { val <<= 1; -- count; }
	while (! (val & 0x10));

	return count;
	}
else
	return -1;
}	// bit_set_hi

// Locate highest bit clear in 'val'
static S_fixed bit_clr_hi (S_fixed val) {
return bit_set_hi (~val);
}	// bit_clr_hi

// Locate lowest bit set in 'val'
static S_fixed bit_set_lo (S_fixed val) {
if (val) {
	unsigned count = 0;
	if (! (val & 0x0000FFFF))
		{ count += 16; val >>= 16; }
	if (! (val & 0x00FF))
		{ count += 8; val >>= 8; }
	if (! (val & 0x0F))
		{ count += 4; val >>= 4; }

	while (! (val & 1)) { val >>= 1; ++ count; }

	return count;
	}
else
	return -1;
}	// bit_set_lo

// Locate lowest bit clear in 'val'
static S_fixed bit_clr_lo (S_fixed val) {
return bit_set_lo (~val);
}	// bit_clr_lo

static unsigned vec_bitcnt [16] =
	{ 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 };

// Count total # of set bits in 'val'
static S_fixed bit_set_count (S_fixed val) {
unsigned count = 0;

unsigned i = 8;
while (i --)
	{ count += vec_bitcnt [val & 0x0F]; val >>= 4; }

return count;
}	// bit_set_count

// Count total # of clear bits in 'val'
static S_fixed bit_clr_count (S_fixed val) {
return bit_set_count (~val);
}	// bit_clr_count

static unsigned vec_bitrev [16] =
	{ 0x0, 0x8, 0x4, 0xC, 0x2, 0xA, 0x6, 0xE, 0x1, 0x9, 0x5, 0xD, 0x3, 0xB, 0x7, 0xF };

// Reverse bit order in 'val'
static S_fixed bit_reverse (S_fixed val) {
unsigned result = 0;

unsigned i = 8;
while (i --)
	{ result = (result << 4) | vec_bitrev [val & 0x0F]; val >>= 4; }

return result;
}	// bit_reverse

//
//	New coercions
//

// (In "E_String.cpp")
S_fixed parse_fixed (S_string &s_str);

// Convert to fixed from ANY scalar type
#define CC_fixed(type, val)	(								\
	type == T_fixed ?		val._fixed :					\
	type == T_float ?		(S_fixed) (val._float) :		\
	type == T_string ?		parse_fixed (val._string) :		\
		0)

// Coerce to fixed from scalar type 'type'
// (wrapper for 'CC_fixed')
S_fixed to_fixed (VType type, VDatum &val) {
return CC_fixed (type, val);
}	// to_fixed

// (In "E_String.cpp")
S_float parse_float (S_string &s_str);

// Convert to float from ANY scalar type
#define CC_float(type, val)	(								\
	type == T_float ?		val._float :					\
	type == T_fixed ?		(S_float) (val._fixed) :		\
	type == T_string ?		parse_float (val._string) :		\
		0)

// Coerce to float from scalar type 'type'
S_float to_float (VType type, VDatum &val) {
return CC_float (type, val);
}	// to_float

static bool _CC_number (VType &type, VDatum &val) {
if (type == T_string) {			// (expected)
	// (try both types)
	S_fixed v_fixed = parse_fixed (val._string);
	S_float v_float = parse_float (val._string);

	val._string.relink ();

	if ((S_float) v_fixed == v_float) {
		type = T_fixed; val._fixed = v_fixed;
		return true;
		}
	else {
		type = T_float; val._float = v_float;
		return false;
		}
	}

// (Just failure case)
val._fixed = 0;
return true;
}	// _CC_number

#define	CC_number(type, val)		(type == T_fixed ? true : type == T_float ? false : _CC_number(type, val))

// Expected numeric, force float
#define	F_float(type, val)	(type == T_fixed ? (S_float) val._fixed : val._float)

// Expected numeric, force fixed
#define	F_fixed(type, val)	(type == T_float ? (S_fixed) val._float : val._fixed)

//
// Unary numeric operations
//

struct P_UnaryNum : P_Unary {
	opUnary_fixed unop_fixed;
	opUnary_float unop_float;

	P_UnaryNum (char const *ident, O_Enum op, opUnary_fixed unop_fixed, opUnary_float unop_float):
		P_Unary (ident, op)
		{ this->unop_fixed = unop_fixed; this->unop_float = unop_float; }

	D_P_Unary_eval;
	};

VType P_UnaryNum::eval_Unary (VDatum &val, VType type1, VDatum &val1) {
if (! unop_float) {
	// (expect fixed operand & result)
	val._fixed = unop_fixed (CC_fixed (type1, val1));
	return T_fixed;
	}

else if (! unop_fixed) {
	// (expect float operand & result)
	val._float = unop_float (CC_float (type1, val1));
	return T_float;
	}

else {
	// (fixed / float dilemma)
	if (CC_number (type1, val1)) {
		// (fixed)
		val._fixed = unop_fixed (val1._fixed);
		return T_fixed;
		}
	else {
		// (float)
		val._float = unop_float (val1._float);
		return T_float;
		}
	}

return T_undef;		// (Just in case)
}	// P_UnaryNum::eval_Unary

//
// Binary numeric operations
//

struct P_BinaryNum : P_Binary {
	opBinary_fixed binop_fixed;
	opBinary_float binop_float;

	P_BinaryNum (char const *ident, O_Enum op, opBinary_fixed binop_fixed, opBinary_float binop_float):
	P_Binary (ident, op)
		{ this->binop_fixed = binop_fixed; this->binop_float = binop_float; }

	D_P_Binary_eval;
	};

VType P_BinaryNum::eval_Binary (VDatum &val,
	VType type1, VDatum &val1, VType type2, VDatum &val2) {

if (! binop_float) {
	// (expect fixed operands & result)
	val._fixed = binop_fixed (CC_fixed (type1, val1), CC_fixed (type2, val2));
	return T_fixed;
	}

else if (! binop_fixed) {
	// (expect float operands & result)
	val._float = binop_float (CC_float (type1, val1), CC_float (type2, val2));
	return T_float;
	}

else {
	// (fixed/float dilemma)
	// (Warning: both CC_numbers must be done -- using '&' instead of '&&')
	if (CC_number (type1, val1) & CC_number (type2, val2)) {
		// (both fixed)
		val._fixed = binop_fixed (val1._fixed, val2._fixed);
		return T_fixed;
		}
	else {
		// (one/both are float)
		val._float = binop_float (F_float (type1, val1), F_float (type2, val2));
		return T_float;
		}
	}

return T_undef;		// (Just in case)
}	// P_BinaryNum::eval_Binary

//
// Compare numeric operations
//

struct P_CmpNum : P_Binary {
	opCmp_fixed cmp_fixed;
	opCmp_float cmp_float;

	P_CmpNum (char const *ident, O_Enum op, opCmp_fixed cmp_fixed, opCmp_float cmp_float):
		P_Binary (ident, op)
		{ this->cmp_fixed = cmp_fixed; this->cmp_float = cmp_float; }

	D_P_Binary_eval;
	};

VType P_CmpNum::eval_Binary (VDatum &val,
	VType type1, VDatum &val1, VType type2, VDatum &val2) {
S_fixed result = 0;

if (! cmp_float) {
	// (expect fixed operands)
	result = cmp_fixed (CC_fixed (type1, val1), CC_fixed (type2, val2));
	}

else if (! cmp_fixed) {
	// (expect float operands)
	result = cmp_float (CC_float (type1, val1), CC_float (type2, val2));
	}

else {
	// (fixed/float dilemma)
	// (Warning: both CC_numbers must be done -- using '&' instead of '&&')
	if (CC_number (type1, val1) & CC_number (type2, val2)) {
		// (both fixed)
		result = cmp_fixed (val1._fixed, val2._fixed);
		}
	else {
		// (one/both are float)
		result = cmp_float (F_float (type1, val1), F_float (type2, val2));
		}
	}

val._fixed = result;
return T_fixed;
}	// P_CmpNum::eval_Binary

//
//	Integer limits
//

#define	MIN_INT		(1 << 31)
#define	MAX_INT		~(1 << 31)

struct P_IntLit : Prefix {
	int which;

	P_IntLit (char const *ident, O_Enum op, int which) : Prefix (ident, op)
		{ this->which = which; }

	VType evalV (VDatum &val, Expr *args) {
		val._fixed =
			which > 0 ? MAX_INT :
			which < 0 ? MIN_INT :
			0;

		return T_fixed;
		}	// evalV

	};

//
//	Special float numbers
//

struct P_FloatLit : Prefix {
	int num;

	P_FloatLit (char const *ident, O_Enum op, int num) : Prefix (ident, op)
		{ this->num = num; }

	VType evalV (VDatum &val, Expr *args) {
		float divisor = 0.0;
		val._float = (float) num / divisor;
		return T_float;
		}	// evalV
	};

//
//	Random numbers
//

struct P_Rand : Prefix {
	P_Rand (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		S_fixed limit = expect_fixed (args, 0);

		val._fixed = limit ? rand() % limit : rand ();
		return T_fixed;
		}	// evalV
	};

struct P_Randomize : Prefix {
	P_Randomize (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		S_fixed seed = args ? expect_fixed (args, 0) : time(0);

		srand (seed);
		val._fixed = seed;
		return T_fixed;
		}	// evalV
	};

// Calculate hash code for floats
S_fixed hash_float (S_float value) {
int exponent = 0;
return (int) (ldexp (frexp (value, &exponent), 24)) ^ (exponent << 8);
}	// hash_float

//
//	Initialization
//

static bool init_primaries_numeric (int order) {

//		[Categories]

//^C	Numeric
//^B	Numeric functors
//^D	Functors, operating on numeric values.

//		[Types]

//^T	Int
//^B	Integer value.
//^D	Anything evaluating to integer. As argument, any scalar value allowed (implicitly coerced to integer).

//^T	Float
//^B	Float value.
//^D	Anything evaluating to float. As argument, any scalar value allowed (implicitly coerced to float).

//^T	Num
//^B	Numeric value.
//^D	Anything evaluating to integer or float. As argument, any scalar value allowed (implicitly coerced to number).

//^T	Sign
//^B	Sign value.
//^D	Anything treated as result of comparison.
//^D	As argument: any scalar value allowed (implicitly coerced to number, then compared with 0).
//^D	As result: exactly -1 (negative/less than), 1 (positive/greater than), or 0 (zero/equal).

//		--------

//^G	is_int is_float is_num

//^N	is_int [Predicate | Numeric]
//^P	is_int (V: Any) => Bool
//^B	Check for integer value.
//^D	Predicate: !true, if argument \V evaluates to integer scalar.

//^N	is_float [Predicate | Numeric]
//^P	is_float (V: Any) => Bool
//^B	Check for float value.
//^D	Predicate: !true, if argument \V evaluates to float scalar.

//^N	is_num [Predicate | Numeric]
//^P	is_num (V: Any) => Bool
//^B	Check for numeric value.
//^D	Predicate: !true, if argument \V evaluates to numeric (int or float) scalar.

	DefBuiltin (P_IsType ("is_int", Op_Null, T_fixed));
	DefBuiltin (P_IsType ("is_float", Op_Null, T_float));
	DefBuiltin (P_IsNum ("is_num", Op_Null));

//^G	expect_int expect_float

//^N	expect_int [Wrapper | Numeric]
//^P	expect_int (V: Any, @Body: Any) => Any
//^B	Expect integer value.
//^D	If argument \V evaluates to integer scalar, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

//^N	expect_float [Wrapper | Numeric]
//^P	expect_float (V: Any, @Body: Any) => Any
//^B	Expect float value.
//^D	If argument \V evaluates to float scalar, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_int", Op_Null, T_fixed));
	DefBuiltin (P_ExpectType ("expect_float", Op_Null, T_fixed));

//^G	neg not

//^N	neg [Scalar | Unary | Numeric]
//^P	neg (A: Num) => Num
//^B	Numeric negation.
//^D	Arithmetic negation of numeric argument \A.
//^D	Syntax: -\A.

//^N	not [Scalar | Unary | Numeric]
//^P	not (I: Int) => Int
//^B	Bitwise complement.
//^D	Bitwise complement of integer argument \I.
//^D	Syntax: ~\I.

	DefBuiltin (P_UnaryNum ("neg", Op_Neg,		neg_fixed, neg_float));
	DefBuiltin (P_UnaryNum ("not", Op_Not,		not_fixed, 0));

//^G	abs sgn

//^N	abs [Scalar | Unary | Numeric]
//^P	abs (A: Num) => Num
//^B	Numeric absolute value.
//^D	Arithmetic absolute value of numeric argument \A.
//^D	(Equivalent to: (\A >= 0 ? \A : -\A), but with \A evaluated once).
//^D	Syntax: +\A.

//^N	sgn [Scalar | Unary | Numeric]
//^P	sgn (A: Num) => Sign
//^B	Numeric sign.
//^D	Arithmetic sign (-1, 0, 1) of numeric argument \A.
//^D	(Equivalent to: (\A > 0 ? 1 : \A < 0 ? -1 : 0), but with \A evaluated once).
//^D	Syntax: <?>\A.

	DefBuiltin (P_UnaryNum ("abs", Op_Abs,		abs_fixed, abs_float));
	DefBuiltin (P_UnaryNum ("sgn", Op_Sign,		sgn_fixed, sgn_float));

//^G	floor ceil

//^N	floor [Scalar | Unary | Numeric]
//^P	floor (X: Float) => Float
//^B	Numeric rounding down.
//^D	Arithmetic "floor" value: round \X down to nearest integer.

//^N	ceil [Scalar | Unary | Numeric]
//^P	ceil (X: Float) => Float
//^B	Numeric rounding up.
//^D	Arithmetic "ceiling" value: round \X up to nearest integer.

	DefBuiltin (P_UnaryNum ("floor", Op_Floor,		0, floor_float));
	DefBuiltin (P_UnaryNum ("ceil", Op_Ceil,		0, ceil_float));

//^G	sqr

//^N	sqr [Scalar | Unary | Numeric]
//^P	sqr (X: Float) => Float
//^B	Numeric square root.
//^D	Square root of \X (NAN, if \X < 0).

	DefBuiltin (P_UnaryNum ("sqr", Op_Sqr,	0, sqr_float));

//^G	exp log exp_by log_by

//^N	exp [Scalar | Unary | Numeric]
//^P	exp (X: Float) => Float
//^B	Numeric exponentiation.
//^D	Exponent of \X: \e raised to power \X.

//^N	log [Scalar | Unary | Numeric]
//^P	log (X: Float) => Float
//^B	Numeric logarithm.
//^D	Natural (base \e) logarithm of \X (NAN, if \X <= 0).

	DefBuiltin (P_UnaryNum ("exp", Op_Exp,	0, exp_float));
	DefBuiltin (P_UnaryNum ("log", Op_Log,	0, log_float));

//^G	pi

//^N	pi [Scalar | Unary | Numeric]
//^P	pi (X: Float) => Float
//^B	Multiply operand by \Pi.
//^D	Value of \X multiplied by \Pi.

	DefBuiltin (P_UnaryNum ("pi", Op_PI,		0, pi_float));

//^G	sin cos tan

//^N	sin [Scalar | Unary | Numeric]
//^P	sin (X: Float) => Float
//^B	Trigonometric sine.
//^D	Sine of \X (with argument in radians).

//^N	cos [Scalar | Unary | Numeric]
//^P	cos (X: Float) => Float
//^B	Trigonometric cosine.
//^D	Cosine of \X (with argument in radians).

//^N	tan [Scalar | Unary | Numeric]
//^P	tan (X: Float) => Float
//^B	Trigonometric tangent.
//^D	Tangent of \X (with argument in radians).

	DefBuiltin (P_UnaryNum ("sin", Op_Sin,	0, sin_float));
	DefBuiltin (P_UnaryNum ("cos", Op_Cos,	0, cos_float));
	DefBuiltin (P_UnaryNum ("tan", Op_Tan,	0, tan_float));

//^G	asin acos atan

//^N	asin [Scalar | Unary | Numeric]
//^P	asin (X: Float) => Float
//^B	Trigonometric arcsine.
//^D	ArcSine of \X (with result in radians).

//^N	acos [Scalar | Unary | Numeric]
//^P	acos (X: Float) => Float
//^B	Trigonometric arccosine.
//^D	ArcCosine of \X (with result in radians).

//^N	atan [Scalar | Unary | Numeric]
//^P	atan (X: Float) => Float
//^B	Trigonometric arctangent.
//^D	ArcTangent of \X (with result in radians).

	DefBuiltin (P_UnaryNum ("asin", Op_ASin,	0, asin_float));
	DefBuiltin (P_UnaryNum ("acos", Op_ACos,	0, acos_float));
	DefBuiltin (P_UnaryNum ("atan", Op_ATan,	0, atan_float));

//^G	sin asin
//^G	cos acos
//^G	tan atan

//^G	sinh cosh tanh

//^N	sinh [Scalar | Unary | Numeric]
//^P	sinh (X: Float) => Float
//^B	Numeric hyperbolic sine.
//^D	Hyperbolic Sine of \X.
//^D	(Equals to: (!exp(\X) - !exp(-\X)) / 2).

//^N	cosh [Scalar | Unary | Numeric]
//^P	cosh (X: Float) => Float
//^B	Numeric hyperbolic cosine.
//^D	Hyperbolic Cosine of \X.
//^D	(Equals to: (!exp(\X) + !exp(-\X)) / 2).

//^N	tanh [Scalar | Unary | Numeric]
//^P	tanh (X: Float) => Float
//^B	Numeric hyperbolic tangent.
//^D	Hyperbolic Tangent of \X.
//^D	(Equals to: !sinh(\X) / !cosh(\X), or (!exp(\X) - !exp(-\X)) / (!exp(\X) + !exp(-\X))).

	DefBuiltin (P_UnaryNum ("sinh", Op_SinH,	0, sinh_float));
	DefBuiltin (P_UnaryNum ("cosh", Op_CosH,	0, cosh_float));
	DefBuiltin (P_UnaryNum ("tanh", Op_TanH,	0, tanh_float));

//^G	add sub

//^N	add [Scalar | Binary | Numeric]
//^P	add (A: Num, B: Num) => Num
//^B	Numeric addition.
//^D	Arithmetic sum of arguments \A and \B.
//^D	Syntax: \A + \B.

//^N	sub [Scalar | Binary | Numeric]
//^P	sub (A: Num, B: Num) => Num
//^B	Numeric subtraction.
//^D	Arithmetic difference of arguments \A and \B.
//^D	Syntax: \A - \B.

	DefBuiltin (P_BinaryNum ("add", Op_Add,	add_fixed, add_float));
	DefBuiltin (P_BinaryNum ("sub", Op_Sub,	sub_fixed, sub_float));

//^G	mul div

//^N	mul [Scalar | Binary | Numeric]
//^P	mul (A: Num, B: Num) => Num
//^B	Numeric multiplication.
//^D	Arithmetic product of arguments \A and \B.
//^D	Syntax: \A * \B.

//^N	div [Scalar | Binary | Numeric]
//^P	div (X: Float, Y: Float) => Float
//^B	Numeric division.
//^D	Arithmetic ratio of arguments \X and \Y.
//^D	Syntax: \X / \Y.

//^G	idiv irem rdiv rrem

//^N	idiv [Scalar | Binary | Numeric]
//^P	idiv (I: Int, J: Int) => Int
//^B	Integer ratio.
//^D	Integer division of arguments \I and \J.
//^D	(Same as !int(\I / \J); always rounding result towards 0).
//^D	Syntax: \I % \J.

//^N	irem [Scalar | Binary | Numeric]
//^P	irem (I: Int, J: Int) => Int
//^B	Integer remainder.
//^D	Integer remainder of arguments \I and \J.
//^D	(Same as \I - \J * (\I % \J)).
//^D	Syntax: \I %% \J.

	DefBuiltin (P_BinaryNum ("mul", Op_Mul,	mul_fixed, mul_float));
	DefBuiltin (P_BinaryNum ("div", Op_Div,	0, div_float));

	DefBuiltin (P_BinaryNum ("idiv", Op_DivI,	idiv_fixed, 0));
	DefBuiltin (P_BinaryNum ("irem", Op_RemI,	irem_fixed, 0));

//^N	rdiv [Scalar | Binary | Numeric]
//^P	rdiv (I: Int, J: Int) => Int
//^B	Integer ratio alternate.
//^D	Integer division of arguments.
//^D	(Same as !idiv (\I, \J), but rounding negative result down).

//^N	rrem [Scalar | Binary | Numeric]
//^P	rrem (I: Int, J: Int) => Int
//^B	Integer remainder alternate.
//^D	Integer remainder of arguments.
//^D	(Same as !irem (\I, \J), but rounding negative result down).

	DefBuiltin (P_BinaryNum ("rdiv", Op_Null,	rdiv_fixed, 0));
	DefBuiltin (P_BinaryNum ("rrem", Op_Null,	rrem_fixed, 0));

//^G	min max

//^N	min [Scalar | Binary | Numeric]
//^P	min (A: Num, B: Num) => Num
//^B	Numeric minimum.
//^D	Arithmetically lesser of arguments: \A ?< \B.
//^D	(Equals to: \A < \B ? \A : \B, but evaluates arguments only once) (!min).
//^D	Syntax: \A ?< \B.

//^N	max [Scalar | Binary | Numeric]
//^P	max (A: Num, B: Num) => Num
//^B	Numeric maximum.
//^D	Arithmetically greater of arguments: \A ?> \B.
//^D	(Equals to: \A > \B ? \A : \B, but evaluates arguments only once) (!max).
//^D	Syntax: \A ?> \B.

	DefBuiltin (P_BinaryNum ("min", Op_Min,	min_fixed, min_float));
	DefBuiltin (P_BinaryNum ("max", Op_Max,	max_fixed, max_float));

//^G	shl shr

//^N	shl [Scalar | Binary | Numeric]
//^P	shl (I: Int, J: Int) => Int
//^B	Bitwise shift left.
//^D	Arithmetic shift: returns \I shifted left by \J bits.
//^D	Note: (\I << -\J == \I >> \J).
//^D	Syntax: \I << \J.

//^N	shr [Scalar | Binary | Numeric]
//^P	shr (I: Int, J: Int) => Int
//^B	Bitwise shift right.
//^D	Arithmetic shift: returns \I shifted right by \J bits.
//^D	Note: (\I >> -\J == \I << \J).
//^D	Syntax: \I >> \J.

	DefBuiltin (P_BinaryNum ("shl", Op_Shl,	shl_fixed, 0));
	DefBuiltin (P_BinaryNum ("shr", Op_Shr,	shr_fixed, 0));

//^N	sll [Scalar | Binary | Numeric]
//^P	sll (I: Int, J: Int) => Int
//^B	Bitwise logical shift left.
//^D	Logic shift: returns \I shifted left by \J bits.

//^N	slr [Scalar | Binary | Numeric]
//^P	slr (I: Int, J: Int) => Int
//^B	Bitwise logical shift right.
//^D	Logic shift: returns \I shifted right by \J bits (no sign expansion).

	DefBuiltin (P_BinaryNum ("sll", Op_Null,	sll_fixed, 0));
	DefBuiltin (P_BinaryNum ("slr", Op_Null,	slr_fixed, 0));

//^N	exp_by [Scalar | Binary | Numeric]
//^P	exp_by (X: Float, Y: Float) => Float
//^B	Numeric rise to power.
//^D	Returns \X raised to power \Y.
//^D	(Same as: !exp (!log(\X) * \Y) ).

//^N	log_by [Scalar | Binary | Numeric]
//^P	log_by (X: Float, Y: Float) => Float
//^B	Numeric logarithm by base.
//^D	Logarithm of \Y base \X.
//^D	(Same as: !log(\Y) / !log(\X) ).

	DefBuiltin (P_BinaryNum ("exp_by", Op_ExpBy,	0, expby_float));
	DefBuiltin (P_BinaryNum ("log_by", Op_LogBy,	0, logby_float));

//^G	and or xor

//^N	and [Scalar | Binary | Numeric]
//^P	and (I: Int, J: Int) => Int
//^B	Bitwise conjunction.
//^D	Bitwise AND of arguments \I and \J.
//^D	Syntax: \I & \J.

//^N	or [Scalar | Binary | Numeric]
//^P	or (I: Int, J: Int) => Int
//^B	Bitwise disjunction.
//^D	Bitwise OR of arguments \I and \J.
//^D	Syntax: \I | \J.

//^N	xor [Scalar | Binary | Numeric]
//^P	xor (I: Int, J: Int) => Int
//^B	Bitwise exclusion.
//^D	Bitwise exclusive OR of arguments \I and \J.
//^D	Syntax: \I ~ \J.

	DefBuiltin (P_BinaryNum ("and", Op_And,	and_fixed, 0));
	DefBuiltin (P_BinaryNum ("or", Op_Or,		or_fixed, 0));
	DefBuiltin (P_BinaryNum ("xor", Op_Xor,	xor_fixed, 0));

//^G	lt le gt ge eq ne

//^N	lt [Scalar | Predicate | Numeric]
//^P	lt (A: Num, B: Num) => Bool
//^B	Numeric "less than" compare.
//^D	Arithmetic "less than" test: true, if \A < \B.
//^D	Syntax: \A < \B.

//^N	le [Scalar | Predicate | Numeric]
//^P	le (A: Num, B: Num) => Bool
//^B	Numeric "less than or equal" compare.
//^D	Arithmetic "less than/equal" test: true, if \A <= \B.
//^D	Syntax: \A <= \B or \A ~> \B.

//^N	gt [Scalar | Predicate | Numeric]
//^P	gt (A: Num, B: Num) => Bool
//^B	Numeric "greater than" compare.
//^D	Arithmetic "greater than" test: true, if \A > \B.
//^D	Syntax: \A > \B.

//^N	ge [Scalar | Predicate | Numeric]
//^P	ge (A: Num, B: Num) => Bool
//^B	Numeric "greater than or equal" compare.
//^D	Arithmetic "greater than/equal" test: true, if \A >= \B.
//^D	Syntax: \A >= \B or \A ~< \B.

//^N	eq [Scalar | Predicate | Numeric]
//^P	eq (A: Num, B: Num) => Bool
//^B	Numeric equality.
//^D	Arithmetic equality test: true, if \A equals to \B.
//^D	Syntax: \A == \B.

//^N	ne [Scalar | Predicate | Numeric]
//^P	ne (A: Num, B: Num) => Bool
//^B	Numeric inequality.
//^D	Arithmetic inequality test: true, if \A not equals to \B.
//^D	Syntax: \A ~= \B or \A <> \B.

	DefBuiltin (P_CmpNum ("lt", Op_LT,		lt_fixed, lt_float));
	DefBuiltin (P_CmpNum ("le", Op_LE,		le_fixed, le_float));
	DefBuiltin (P_CmpNum ("gt", Op_GT,		gt_fixed, gt_float));
	DefBuiltin (P_CmpNum ("ge", Op_GE,		ge_fixed, ge_float));
	DefBuiltin (P_CmpNum ("eq", Op_EQ,		eq_fixed, eq_float));
	DefBuiltin (P_CmpNum ("ne", Op_NE,		ne_fixed, ne_float));

//^G	cmp u_cmp

//^N	cmp [Scalar | Compare | Numeric]
//^P	cmp (A: Num, B: Num) => Sign
//^B	Numeric signed comparison.
//^D	Arithmetic signed compare:
//^\	equivalent to \A < \B ? -1 : \A > \B ? 1 : 0 (with \A and \B evaluated once).
//^D	Syntax: \A <?> \B.

	DefBuiltin (P_CmpNum ("cmp", Op_Cmp,	cmp_fixed, cmp_float));

//^N	u_cmp [Scalar | Compare | Numeric]
//^P	u_cmp (A: Num, B: Num) => Sign
//^B	Numeric unsigned comparison.
//^D	Arithmetic unsigned compare.

	DefBuiltin (P_CmpNum ("u_cmp", Op_Null,	ucmp_fixed, cmp_float));

//^G	rad ang

//^N	rad [Scalar | Binary | Numeric]
//^P	rad (X: Num, Y: Num) => Num
//^B	Numeric polar radius.
//^D	Polar radius of cartesian point (\X, \Y).
//^D	(Same as !sqr(\X*\X + \Y*\Y)).

//^N	ang [Scalar | Binary | Numeric]
//^P	ang (X: Num, Y: Num) => Num
//^B	Numeric polar angle.
//^D	Polar angle of point (\X, \Y), with result in radians.

	DefBuiltin (P_BinaryNum ("rad", Op_Rad,	0,	rad_float));
	DefBuiltin (P_BinaryNum ("ang", Op_Ang,	0,	ang_float));

//^G	bitset_hi bitset_lo bitclr_hi bitclr_lo

//^N	bitset_hi [Scalar | Unary | Numeric]
//^P	bitset_hi (Value: Int) => Int
//^B	Locate highest set bit.
//^D	Return index of highest set bit in \Value
//^\	(or -1, if \Value == 0).

//^N	bitset_lo [Scalar | Unary | Numeric]
//^P	bitset_lo (Value: Int) => Int
//^B	Locate lowest set bit.
//^D	Return index of lowest set bit in \Value
//^\	(or -1, if \Value == 0).

//^N	bitclr_hi [Scalar | Unary | Numeric]
//^P	bitclr_hi (Value: Int) => Int
//^B	Locate highest clear bit.
//^D	Return index of highest clear bit in \Value
//^\	(or -1, if \Value == ~0).

//^N	bitclr_lo [Scalar | Unary | Numeric]
//^P	bitclr_lo (Value: Int) => Int
//^B	Locate lowest clear bit.
//^D	Return index of lowest clear bit in \Value
//^\	(or -1, if \Value == ~0).

	DefBuiltin (P_UnaryNum ("bitset_hi", Op_Null,	bit_set_hi,	0));
	DefBuiltin (P_UnaryNum ("bitset_lo", Op_Null,	bit_set_lo,	0));
	DefBuiltin (P_UnaryNum ("bitclr_hi", Op_Null,	bit_clr_hi,	0));
	DefBuiltin (P_UnaryNum ("bitclr_lo", Op_Null,	bit_clr_lo,	0));

//^G	bitset_count bitclr_count

//^N	bitset_count [Scalar | Unary | Numeric]
//^P	bitset_count (Value: Int) => Int
//^B	Count all set bits.
//^D	Return total count of all set bits in \Value
//^\	(0, if \Value == 0).

//^N	bitclr_count [Scalar | Unary | Numeric]
//^P	bitclr_count (Value: Int) => Int
//^B	Count all clear bits.
//^D	Return total count of all clear bits in \Value
//^\	(0, if \Value == ~0).

	DefBuiltin (P_UnaryNum ("bitset_count", Op_Null,	bit_set_count,	0));
	DefBuiltin (P_UnaryNum ("bitclr_count", Op_Null,	bit_clr_count,	0));

//^N	bitrev [Scalar | Unary | Numeric]
//^P	bitrev (Value: Int) => Int
//^B	Reverse bits order.
//^D	Return bit reversion of \Value.

	DefBuiltin (P_UnaryNum ("bitrev", Op_Null,	bit_reverse,	0));

//^G	int float num

//^N	int [Scalar | Coercion | Numeric]
//^P	int (V: Scalar) => Int
//^B	Coerce scalar to integer.
//^D	Explicit integer coercion: from scalar \V to integer value
//^\	(according to default coercion rules).

//^N	float [Scalar | Coercion | Numeric]
//^P	float (V: Scalar) => Float
//^B	Coerce scalar to float.
//^D	Explicit float coercion: from scalar \V to float value
//^\	(according to default coercion rules).

//^N	num [Scalar | Coercion | Numeric]
//^P	num (V: Scalar) => Num
//^B	Coerce scalar to number.
//^D	Explicit numeric coercion: from scalar \V to integer or float value
//^\	(according to default coercion rules).

	DefBuiltin (P_UnaryNum ("int",		Op_ICvt,	val_fixed, 0));
	DefBuiltin (P_UnaryNum ("float",	Op_FCvt,	0, val_float));
	DefBuiltin (P_UnaryNum ("num",		Op_NCvt,	val_fixed, val_float));

//^G	rand randomize

//^N	rand [Unary | Numeric]
//^P	rand ([N: Int]) => Int
//^B	Random numbers generator.
//^D	Returns next random number, belonging to the range 0..\N.
//^D	If \N is omitted, may return any integer value.

//^N	randomize [Unary | Numeric]
//^P	randomize ([Seed: Int]) => Int
//^B	Randomization.
//^D	Sets random numbers generator to \Seed (affecting following values, returned by !rand).
//^D	If \Seed operand is omitted, chooses random seed for randomization.
//^D	Returns \Seed.

	DefBuiltin (P_Rand ("rand", Op_Null));

	DefBuiltin (P_Randomize ("randomize", Op_Null));

//^G	min_int max_int

//^N	min_int [Numeric | Nullary]
//^P	min_int () => Int
//^B	Integer minimum.
//^D	Returns lowest integer value available.

//^N	max_int [Numeric | Nullary]
//^P	max_int () => Int
//^B	Integer maximum.
//^D	Returns highest integer value available.

	DefBuiltin (P_IntLit ("min_int", Op_Null, -1));
	DefBuiltin (P_IntLit ("max_int", Op_Null, 1));

//^G	inf_pos inf_neg nan

//^N	inf_pos [Numeric | Nullary]
//^P	inf_pos () => Float
//^B	Positive infinity.
//^D	Returns positive infinite value.

//^N	inf_neg [Numeric | Nullary]
//^P	inf_neg () => Float
//^B	Negative infinity.
//^D	Returns negative infinite value.

//^N	nan [Numeric | Nullary]
//^P	nan () => Float
//^B	Not a number.
//^D	Returns numeric indefinite (not a number) value.

	DefBuiltin (P_FloatLit ("inf_pos", Op_Null, 1));
	DefBuiltin (P_FloatLit ("inf_neg", Op_Null, -1));
	DefBuiltin (P_FloatLit ("nan", Op_Null, 0));

return true;
}	// init_primaries_numeric

DefSubSystem ("numeric", init_primaries_numeric, 0);

