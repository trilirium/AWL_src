
/*

	+---+---+---+---+---+---+
	|	'Opcodes.h':
	|	Operation codes for basic AWL builtins.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef	_OPCODES_INCLUDED

#define	_OPCODES_INCLUDED

typedef enum _O_Enum {
	Op_Null,			// (no operation)

	// Numeric Unary
	Op_Abs,				// absolute value
	Op_Neg,				// numeric negation
	Op_Sign,			// signum
	Op_Inv,				// inversion
	Op_Not,				// logical NOT

	Op_Floor,			// floor rounding
	Op_Ceil,			// ceiling rounding
	Op_Sqr,				// square root
	Op_Exp,				// exponent
	Op_Log,				// logarithm
	Op_Sin,				// trigonometric sine
	Op_Cos,				// trigonometric cosine
	Op_Tan,				// trigonometric tangent
	Op_SinH,			// hyperbolic sine
	Op_CosH,			// hyperbolic cosine
	Op_TanH,			// hyperbolic tangent
	Op_ASin,			// trigonometric arcsine
	Op_ACos,			// trigonometric arccosine
	Op_ATan,			// trigonometric arctangent
	Op_ASinH,			// hyperbolic arcsine
	Op_ACosH,			// hyperbolic arccosine
	Op_ATanH,			// hyperbolic arctangent
	Op_Gamma,			// factorial/gamma func
	Op_PI,				// multiply by PI

	Op_ICvt,			// integer coercion
	Op_FCvt,			// float coercion
	Op_NCvt,			// numeric coercion

	// Numeric Binary
	Op_Add,				// addition
	Op_Sub,				// subtraction
	Op_Mul,				// multiplication
	Op_Div,				// division
	Op_DivI,			// integer division
	Op_RemI,			// integer remainder
	Op_Min,				// minimum
	Op_Max,				// maximum

	Op_Beta,			// binomial/beta func

	Op_ExpBy,			// exponent by base
	Op_LogBy,			// logarythm by base

	Op_Rad,				// point polar radius
	Op_Ang,				// point polar angle

	// Numeric Compare
	Op_LT,				// less than
	Op_LE,				// less than or equal
	Op_GT,				// greater than
	Op_GE,				// greater than or equal
	Op_EQ,				// equal
	Op_NE,				// not equal

	Op_Cmp,				// compare

	Op_And,				// logical/bitwise AND
	Op_Or,				// logical/bitwise OR
	Op_Xor,				// logical/bitwise XOR

	Op_Shl,				// bit shift left
	Op_Shr,				// bit shift right

	// String Unary
	Op_SLen,			// string length
	Op_SType,			// string type
	Op_SRev,			// string reverse

	Op_SCvt,			// string coercion

	// String Binary
	Op_SCat,			// string concatenation
	Op_SRep,			// string replication

	Op_SMin,			// string minimum
	Op_SMax,			// string maximum

	Op_SSlice,			// string slice

	Op_SFindFor,		// string find first
	Op_SFindBak,		// string find last

	Op_CharRange,		// character range to string

	// String Compare
	Op_SLT,				// string less than
	Op_SLE,				// string less than or equal
	Op_SGT,				// string greater than
	Op_SGE,				// string greater than or equal
	Op_SEQ,				// string equal
	Op_SNE,				// string not equal

	Op_SCmp,			// string compare

	// Mutation
	Op_Let,				// reference assignment

	Op_Set,				// value assignment
	Op_SetA,			// value exchange
	Op_Swap,			// value swap

	Op_Comb,			// combined assignment
	Op_CombA,			// combined assignment/exchange
	Op_Reduce,			// list reduction

	Op_Inc,				// preincrement
	Op_Dec,				// predecrement
	Op_IncP,			// postincrement
	Op_DecP,			// postdecrement

	// Input/output
	Op_Get,				// get data from stream
	Op_Put,				// put data to stream

	// List
	Op_LLength,			// list length
	Op_LItem,			// list item
	Op_LCat,			// list concatenation
	Op_LRep,			// list repetition
	Op_LRev,			// list reverse

	Op_LRef,			// list self
	Op_LHead,			// list head
	Op_LHeadBy,			// list head with offset
	Op_LTail,			// list tail
	Op_LTailBy,			// list tail with offset

	Op_LCopy,
	Op_LSplit,
	Op_LPush,			// push item(s) to list
	Op_LPop,			// pop item(s) from list

	Op_LLoop,			// loop through list forward
	Op_LLoopR,			// loop through list reverse

	// Ident/differ
	Op_Ident,			// identical
	Op_Differ,			// different
	
	// Evaluation
	Op_DeVal,			// devaluation
	Op_ReVal,			// revaluation

	// Conditionals/predicates
	Op_False,			// False literal
	Op_True,			// True literal
	Op_CNot,			// Logical NOT

	Op_InRange,			// value inside range
	Op_OutRange,		// value outside range

	Op_CAnd,			// conditional AND
	Op_COr,				// conditional OR

	Op_If,				// conditional if
	Op_Unless,			// conditional unless

	// Iterators
	Op_WhilePre,		// loop while before
	Op_WhilePost,		// loop while after
	Op_UntilPre,		// loop until before
	Op_UntilPost,		// loop until after

	Op_ForInc,			// loop FOR incremental
	Op_ForDec,			// loop FOR decremental
	Op_Times,			// loop # times

	// Objective/functional
	Op_With,			// with (object) prefix
	Op_Apply,			// apply functor reference

	// Array related
	Op_ACreate,			// array creation
	Op_ADims,			// array dimensions
	Op_ARank,			// array rank
	Op_AElem,			// array element accessor

	// Hash related
	Op_HCreate,			// hash creation
	Op_HElem,			// hash element accessor
	Op_HLookup,			// hash element lookup
	Op_HRemove,			// hash element remove

	// Pattern related
	Op_RXAlt,			// pattern alternation
	Op_RXCat,			// pattern concatenation
	Op_RXRepInc,		// pattern repetition (incremental)
	Op_RXRepDec,		// pattern repetition (decremental)

	// (-- end of oplist --)

	Op_Total
	} O_Enum;

#endif

