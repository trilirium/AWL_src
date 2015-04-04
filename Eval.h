
//
//
//	"Eval.h": common header for functor implementations
//
//

#include "Defs.h"

#include "Opcodes.h"

extern char primary_pfx[];

// Macro for allocation of built-in functor node
#define	DefBuiltin(ctor)	(new (primary_pfx) ctor)

//
// Prefix: do type check
//

struct P_IsType : Prefix {
	VType t_check;
	
	P_IsType (char const *ident, O_Enum op, VType type) : Prefix (ident, op)
		{ t_check = type; }

	D_Prefix_evalV;
	};

//
// Prefix: expect type, then evaluate core
//

struct P_ExpectType : P_Wrapper {
	VType t_expect;

	P_ExpectType (char const *ident, O_Enum op, VType type) : P_Wrapper (ident, op)
		{ t_expect = type; }

	D_P_Wrapper_eval;
	};

//
// Prefix: Unary scalar operation
//

#define	D_P_Unary_eval						\
	VType eval_Unary (VDatum &val,			\
		VType type1, VDatum &val1)

struct P_Unary : Prefix {
	P_Unary (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	D_Prefix_eval_comb;

	virtual D_P_Unary_eval;
	};

//
// Prefix: Binary scalar operation
//

#define	D_P_Binary_eval						\
	VType eval_Binary (VDatum &val,			\
		VType type1, VDatum &val1, VType type2, VDatum &val2)

struct P_Binary : Prefix {
	P_Binary (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	D_Prefix_eval_comb;
	D_Prefix_eval_reduce;

	virtual D_P_Binary_eval;
	};

//
//	Some common functions
//

// Pop first element from list (if list)
Expr *pop_list (Expr *&list);

// Get next argument
Expr *get_arg (Expr *&args);

// Link value
void link_value (VType type, VDatum &val);

// Unlink value
void unlink_value (VType type, VDatum &val);

// Relink value
void relink_value (VType type, VDatum &val);

// Relock value by 'dir'
void relock_value_by (int dir, VType type, VDatum &val);

// Construct r-expression
Expr *&consR_X (Expr *expr);

#define	R_null	consR_X(0)

// Check, is it R-expression
bool isR_X (Expr *&r_expr);

// Expression's full evaluation
VType evalV_X (Expr *expr, VDatum &val);

// Expression's full evaluation (w. relink)
VType evalV_X_R (Expr *expr, VDatum &val);

// Construct expression from value
Expr *evalX_V (VType type, VDatum &val);

// Evaluate expression
Expr *evalX_X (Expr *expr);

// Evaluate expression (w. relink)
Expr *evalX_X_R (Expr *expr);

// Reference evaluation
Expr *&evalR_X (Expr *expr);

// Reference evaluation (w. relink)
Expr *&evalR_X_R (Expr *expr);

// Assign expression to mutable
bool mutateX_X (Expr *m_expr, Expr *expr);

// Assign value to mutable
bool mutateX_V (Expr *m_expr, VType type, VDatum &val);

// Assign expression to reference
bool mutateR_X (Expr *&r_expr, Expr *expr);

// Assign value to reference
bool mutateR_V (Expr *&r_expr, VType type, VDatum &val);

// Check expressions 'left' && 'right' for identicity
bool identX (Expr *left, Expr *right);

// Calculate hash code of 'expr'
unsigned hashX (Expr *expr);

// Void evaluation (release result)
// Return exception (if any)
Except * evalZ_X (Expr *expr);

// Output expression 
unsigned put_expr (Stream *out, Expr *expr);

//
// Common coercions
//

// Coerce string value to some numeric
// (type T_string expected)
VType coerce_numeric (VDatum &val);

// String coercion (from ANY primary type)
// Resulting type is always T_string
void coerce_string (VType type, VDatum &val);

// Coerce to fixed from scalar type 'type'
S_fixed to_fixed (VType type, VDatum &val);

// Coerce to float from scalar type 'type'
S_float to_float (VType type, VDatum &val);

// Convert to C-string (null-terminated)
char *to_cstring (S_string &s_str);

// Assign fixed value to (presumed mutable) 'm_expr'
bool assign_fixed (Expr *m_expr, S_fixed value);

// Assign float value to (presumed mutable) 'm_expr'
bool assign_float (Expr *m_expr, S_float value);

// Evaluate as C-string
char *eval_cstring (Expr *expr, Prefix *where);

// Evaluate 'func (args)' as predicate
bool apply_bool (Prefix *func, Expr *args);

// Evaluate 'func(args)' as fixed value
S_fixed apply_fixed (Prefix *func, Expr *args, S_fixed defval);

// Evaluate 'func(args)' as float value
S_float apply_float (Prefix *func, Expr *args, S_float defval);

//
//	Comparisons
//

typedef S_fixed (* cmp_fn) (int cmp);

S_fixed
	cmp_lt (int cmp),
	cmp_gt (int cmp),
	cmp_le (int cmp),
	cmp_ge (int cmp),

	cmp_eq (int cmp),
	cmp_ne (int cmp),

	cmp_cmp (int cmp);

// Evaluation wrapper (with arguments)
struct WrapA {

	WrapX &wrapper;
	Expr *args;

	WrapA (WrapX &wrapper, Expr *args):
		wrapper (wrapper), args (args) {}

	Expr *get_arg () { return ::get_arg (args); }

	void eval () { wrapper.eval (args); }

	};

// Invoke prefix with args
void invoke (Prefix *pfx, Expr *args);

// Namespace init
// (Kernel.cpp)
int with_namespace (char const *name, void (*do_init) ());

