
/*

	+---+---+---+---+---+---+
	|	"E_Debug.cpp":
	|	Debugging / internal tools.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"
#include "Logger.h"

//
//	Query internal attributes of (evaluated) expression
//

struct P_IAttr : Prefix {
	enum I_What { I_Addr, I_Type, I_Refs } attr;

	P_IAttr (char const *ident, I_What attr) : Prefix (ident)
		{ this->attr = attr; }

	D_Prefix_evalV;
	};

VType P_IAttr::evalV (VDatum &val, Expr *args) {
Expr *expr = evalX_X (args);
S_fixed result = 0;

switch (attr) {
	case I_Addr:
		result = (S_fixed) expr;
		break;

	case I_Type:
		result = expr ? expr->evalV (val, false) : T_undef;
		break;

	case I_Refs:
		result = expr ? (S_fixed) expr->refs : -1;
		break;
	}

relink_expr (expr);
val._fixed = result;
return T_fixed;
}	// P_IAttr::evalV

struct P_SRefCnt : Prefix {

	P_SRefCnt (char const *ident) : Prefix (ident) {}

	D_Prefix_evalV;
	};

VType P_SRefCnt::evalV (VDatum &val, Expr *args) {
Expr *expr = evalX_X (args);
X_String *string;
if (string = Cast (X_String, expr)) {
	val._fixed = string->refcount ();
	relink_expr (expr);
	return T_fixed;
	}

relink_expr (expr);
return T_undef;
}	// P_SRefCnt::evalV

//
//	Memory control / debug
//

// block info dump
static void dump_blk (char const *tag, unsigned size, void *ptr) {
syslog->put_cstr (tag)->
	put_ch ('[')->put_hex (8, size)->put_ch (']')->put_hex (8, (unsigned)ptr)->put_nl ();
}	// dump_blk

// block allocation hook
static void alloc_hook (char const *tag, unsigned size, void *ptr) {
syslog->put_ch ('+');
dump_blk (tag, size, ptr);
}	// alloc_hook

// block deallocation hook
static void free_hook (char const *tag, unsigned size, void *ptr) {
syslog->put_ch ('-');
dump_blk (tag, size, ptr);
}	// free_hook

struct P_Memory_Control : Prefix {
	
	P_Memory_Control (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
	bool mode = expect_bool (args, false);

	if (mode)
		MM_sethooks (alloc_hook, free_hook);
	else
		MM_sethooks (0, 0);

	val._fixed = mode;
	return T_fixed;
	}	// evalV

	};	// P_Memory_Control

struct P_Memory_Dump : PrefixX {

	P_Memory_Dump (char const *ident) : PrefixX (ident) {}

	Expr *evalX (Expr *args) {
	unsigned remain_count, remain_size;

	MM_forall (dump_blk, remain_count, remain_size);

	return new X_List (new ("MM_count") X_Fixed (remain_count), new ("MM_size") X_Fixed (remain_size));
	}	// evalX

	};	// P_Memory_Dump

//
//	Initialisation
//

static bool init_primaries_debug (int order) {

//
// (move to other namespace?)
//

	DefBuiltin (P_IAttr ("i_addr", P_IAttr::I_Addr));
	DefBuiltin (P_IAttr ("i_refs", P_IAttr::I_Refs));
	DefBuiltin (P_IAttr ("i_type", P_IAttr::I_Type));
	DefBuiltin (P_SRefCnt ("i_srefs"));

	// Memory control

	DefBuiltin (P_Memory_Control ("i_memctl"));
	DefBuiltin (P_Memory_Dump ("i_memdump"));

return true;
}	// init_primaries_debug	

DefSubSystem ("debug", init_primaries_debug, 0);

