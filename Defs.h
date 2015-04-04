
/*

	+---+---+---+---+---+---+
	|	'Defs.h':
	|	AWL common definitions.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef	_DEFS_INCLUDED

#define	_DEFS_INCLUDED

#include "Opcodes.h"

#include <stdlib.h>

// Dynamic cast
#define	Cast(T,X)	(dynamic_cast <T *> (X))

#include "CPUMan.h"
#include "MemMan.h"

//
//	Export/import defs
//

#define DL_EXPORT __declspec(dllexport)
#define DL_IMPORT __declspec(dllimport)

//
// Common structures predeclaration
//

struct Expr;				// generic expression
struct ExprR;				// mutable expression

struct X_String;			// string
struct X_List;				// list
struct X_Term;				// term
struct X_Variable;			// variable

struct X_Block;				// block
struct X_Object;			// object

struct X_Array;				// array
struct X_Hash;				// hash
struct X_Pattern;			// pattern

struct X_Ring;				// ring

struct Prefix;				// prefix
struct PrefixR;				// mutable prefix

union VDatum;				// data value

struct Name;				// name
struct NameTab;				// name table

struct Param;				// functor/class parameter

struct AnyVar;				// generic variable name
struct VarScope;			// - " - scope

struct ModuleVar;			// module variable name
struct ModuleScope;			// - " - scope

struct FunctorVar;			// function local name
struct FunctorScope;		// - " - scope

struct ClassVar;			// class local name
struct ClassScope;			// - " - scope

struct PrefixName;			// prefix name
struct PrefixScope;			// prefix scope

struct WrapX;

struct P_Functor;			// functor definition
struct P_Class;				// class definition
struct P_Virtual;			// virtual definition

struct Module;

struct Stream;				// stream
struct InStream;			// input stream
struct OutStream;			// output stream

struct SCodec;				// stream codec

struct Logger;				// output log

struct Exporter;			// data exporter
struct Importer;			// data importer

struct VarLinker;			// (importer internal)
struct FuncLinker;			// (importer internal)

struct Except;				// exception wrapper

struct Error;				// common error

//
// Syntax term primary
//

typedef struct SynTerm {} *PSynTerm;

//
// Data type value
//

typedef long S_fixed;
typedef double S_float;

// Primitive data types
enum VType {
	T_undef,			// Undefined

	T_fixed,			// Fixed (integer) scalar
	T_float,			// Float (real) scalar
	T_string,			// String scalar

	T_list,				// List
	T_term,				// Term
	T_var,				// Variable
	T_prefix,			// Prefix (functor ref)
	T_block,			// Block

	T_object,			// Object (instance of class)

	T_array,			// Array
	T_hash,				// Hash
	T_pattern,			// Regular expression/pattern
	T_ring,				// Ring/circular list

	T_stream,			// I/O stream
	T_scodec,			// I/O codec

	T_xcpt,				// Exception state (interrupt evaluation)

	T_extern,			// External object
	};

// Is it a scalar type?
#define	T_scalar(type) (T_fixed <= (type) && (type) <= T_string)

//
//	Return macros
//

#define	return_undef()					return T_undef

#define return_fixed(Val,Fixed)			Val._fixed = Fixed; return T_fixed

#define return_float(Val,Float)			Val._float = Float; return T_float

// Pointer to string buffer
// (intentionally typeless)
typedef void *str_ptr;

// Type of characters in buffer
typedef unsigned char C_type;

// String data buffer
struct S_string {
	// Inner string content
	// (see definition in "String.h")
	struct StringData *content;

	// Relative to content
	unsigned offset, length;

	// Allocate new string of length 'len' and type 'type' (with 'tag')
	str_ptr alloc (char const *tag, unsigned len, C_type type);

	// Peek into S_string:
	// returns pointer to string buffer;
	// retrieves length and type
	str_ptr fetch (unsigned &len, C_type &type);

	// Set S_string (from X_String)
	void s_set (X_String *str);

	// Copy S_string from source
	void s_copy (S_string &source);

	// Clear to empty string
	void s_clear ();

	// Initialize (from C-string 'c_str')
	void from_cstr (char *c_str);

	// Compare S_strings for equality/inequality only
	bool equality (S_string &s_op);

	// Convert to C-string (null-terminated)
	char *to_cstring ();

	// Copy to fixed-size buffer
	// (Returns true, if no overflow)
	bool to_buffer (char *buffer, unsigned buflen);

	// Output this string to 'out'
	void put (Stream *out);

	// Log this string to 'log'
	void log (Logger &log);

	// Link string
	void link ();

	// Unlink string
	void unlink ();

	// Relink string
	void relink ();

	S_fixed refcount_by (S_fixed change);

	// Construct new string from self
	X_String *cons ();
	};

#define	D_External_log						\
	void log (Logger &log)

// Generic class for external values
struct External {
	External ();

	virtual D_External_log;				// (output to log)
	};

//
//	Common Data value -- interpretation depends on type
//
union VDatum {
		// case T_fixed:
		S_fixed _fixed;

		// case T_float:
		S_float _float;

		// case T_string:
		S_string _string;

		// case T_list:
		X_List *_list;

		// case T_term:
		X_Term *_term;
		
		// case T_object:
		X_Object *_object;

		// case T_array:
		X_Array *_array;

		// case T_hash:
		X_Hash *_hash;

		// case T_pattern:
		X_Pattern *_pattern;
		
		// case T_ring:
		X_Ring *_ring;

		// case T_var:
		X_Variable *_var;

		// case T_block:
		X_Block *_block;

		// case T_prefix:
		Prefix *_pfx;

		// case T_stream:
		Stream *_stream;

		// case T_codec:
		SCodec *_scodec;

		// case T_xcpt:
		Except *_except;

		// case T_extern:
		struct X_Extern *_extern;
		
		};

//
//	Range container
//

struct FixedRange {
	S_fixed from, to;

	// empty constructor is needed...
	FixedRange () {}

	FixedRange (S_fixed from, S_fixed to) { this->from = from, this->to = to; }

	bool notempty () { return from < to; }
	
	unsigned count () { return to - from; }

	void set (S_fixed from, S_fixed to)
		{ this->from = from; this->to = to; }

	void set_bound (bool end_start, S_fixed value)
		{ end_start ? (to = value) : (from = value); }

	void get (S_fixed &from, S_fixed &to)
		{ from = this->from, to = this->to; }

	void get_ext (S_fixed &start, unsigned &count)
		{ start = from, count = from <= to ? to - from : 0; }

	void log (Logger &log);

	// (evaluate to list...)
	X_List *eval ();

	};

//
//	(Generic) I/O streams
//

#define	EndData			~0

//
//	Generic:
//	Character input/output stream
//
struct CharStream {

	// Read character 'codepoint' from this stream
	// Returns EndData on complete
	virtual unsigned read ();

	// Write character from 'codepoint' to this stream
	virtual bool write (unsigned codepoint);

	};

//
//	Generic:
//	Octet input/output stream
//
struct ByteStream {

	// Get next character (returns EndData on end of stream)
	virtual unsigned fetch ();

	// Emit octet to stream
	virtual bool emit (unsigned char octet);

	};

//
//
//	Definitions of functoral prefixes
//
//

// Evaluate self to value
#define	D_Prefix_evalV					\
	VType evalV (VDatum &val, Expr *args)

// Evaluate self to expression
#define	D_Prefix_evalX					\
	Expr *evalX (Expr *args)

// Evaluate self to reference
#define	D_Prefix_evalR					\
	Expr *&evalR (Expr *args)

// Assign expression to self
#define	D_Prefix_mutateX				\
	bool mutateX (Expr *expr, Expr *args)

// Assign value to self
#define	D_Prefix_mutateV				\
	bool mutateV (VType type, VDatum &val, Expr *args)

// Combined assignment operation:
// evaluate Prefix(r_expr, args...)
#define	D_Prefix_eval_comb				\
	VType eval_comb (VType type, VDatum &val, Expr *args)

// Reduction operation
// (only for binaries)
#define	D_Prefix_eval_reduce			\
	VType eval_reduce (VDatum &val, Expr *args)

// Log prefix (extended)
#define	D_Prefix_log_ex					\
	void log_ex (Logger &log)

// Export prefix declarations
#define D_Prefix_expo_decls				\
	void expo_decls (Exporter &exporter)

// Export prefix definitions
#define	D_Prefix_expo_defs				\
	void expo_defs (Exporter &exporter)

// Import prefix definition
#define	D_Prefix_impo_pfx_def			\
	void impo_pfx_def (Importer &importer, VarLinker *var_table, FuncLinker *func_table)

// Enter prefix scope(s) context
// (scope is: 'modify' ? modifiable : read only)
#define	D_Prefix_scope_in				\
	void scope_in (bool modify)

// Leave prefix scope(s) context
#define	D_Prefix_scope_out				\
	void scope_out (bool modify)

// Cleanup prefix (before release)
#define	D_Prefix_cleanup				\
	void cleanup ()

// Release prefix
#define	D_Prefix_release				\
	void release ()

// Default prefix
// (immutable; value evaluation)
struct Prefix : SynTerm {
	PrefixName *name;	// (prefix name node)
	unsigned refs;		// (prefix reference count)

	// Used for modules init only!
	Prefix (bool unused, char *ident);
	
	Prefix (char const *ident, unsigned opcode = 0);

	// Calculate hash code
	unsigned hash ();

	// Lookup prefix in scope stack by 'ident'
	static Prefix *lookup (char *ident);

	// Locate prefix by opcode 'op'
	static Prefix *locate (O_Enum op);

	void log (Logger &log);

	virtual D_Prefix_evalX;
	virtual D_Prefix_evalV;
	virtual D_Prefix_evalR;

	virtual D_Prefix_mutateX;
	virtual D_Prefix_mutateV;

	virtual D_Prefix_eval_comb;
	virtual D_Prefix_eval_reduce;

	virtual D_Prefix_log_ex;

	virtual D_Prefix_expo_decls;
	virtual D_Prefix_expo_defs;

	virtual D_Prefix_impo_pfx_def;

	virtual D_Prefix_scope_in;
	virtual D_Prefix_scope_out;

	virtual D_Prefix_cleanup;
	virtual D_Prefix_release;

	// Convert to scalar...
	bool scalarize (Expr *&expr, VType &type, VDatum &val);

	// Expect mutable expression
	Expr *&expectR_X (Expr *expr);

	//
	// Expect certain operand types for functor
	//	(with error reporting...)
	//

	// Expected type failure
	void type_error (Expr *expr, VType ex_type, VType type, VDatum &val);

	// Expect (and return) fixed value
	S_fixed expect_fixed (Expr *expr, S_fixed defval);

	// Expect (and return) float value
	S_float expect_float (Expr *expr, S_float defval);

	// Expect string contents
	// (returns true, if OK)
	bool expect_string (Expr *expr, S_string &s_val);

	// Expect range:
	void expect_range (Expr *range, FixedRange &result);

	// Expect boolean
	bool expect_bool (Expr *expr, bool defval);

	// Expect term
	// 'full'? evaluation mode
	bool expect_term (Expr *expr, bool full, Prefix *&pfx, Expr *&args);

	// Expect block
	// 'full'? evaluation mode
	bool expect_block (Expr *expr, bool full, X_Block *&block);

	// Expect prefixal (functor) expression
	Prefix *expect_prefix (Expr *expr);

	// Exprect array expression
	X_Array *expect_array (Expr *expr);

	// Exprect hash expression
	X_Hash *expect_hash (Expr *expr);

	// Expect pattern expression
	X_Pattern *expect_pattern (Expr *expr);

	// Expect ring expression
	X_Ring *expect_ring (Expr *expr);

	// Exprect object expression
	X_Object *expect_object (Expr *expr);

	// Expect external (with signature 'sign')
	External *expect_external (char const *sign, Expr *expr);

	// Expect functor/class reference
	P_Class *expect_class (Prefix *prefix);

	// Expect functor/virtual reference
	P_Virtual *expect_virtual (Prefix *prefix);
	};

// Expression prefix
// (immutable; expression evaluation)
struct PrefixX : Prefix {
	PrefixX (char const *ident, unsigned opcode = 0);

	D_Prefix_evalX;
	D_Prefix_evalV;
	};

// Mutable prefix
struct PrefixR : Prefix {
	PrefixR (char const *ident, unsigned opcode = 0);

	D_Prefix_evalX;
	D_Prefix_evalV;
	D_Prefix_evalR;

	D_Prefix_mutateX;
	D_Prefix_mutateV;
	};

// Empty stack pointer (in XStack)
#define	SP_NULL		~0

#define	D_P_Wrapper_eval						\
	void eval (WrapX &wrapper, Expr *args)

//
//	Wrapper prefix
//
struct P_Wrapper : PrefixR {
	P_Wrapper (char const *ident, unsigned opcode = 0);

	virtual D_P_Wrapper_eval = 0;

	D_Prefix_evalV;
	D_Prefix_evalX;
	D_Prefix_evalR;
	};

//
//	Generic iterator prefix
//

#define	D_P_Iterator_evaluate		\
	void evaluate (IterContext &IC, Expr *args)

struct P_Iterator : Prefix {
	// Internal iterator context
	struct IterContext {
		Expr *body;

		// Start process:
		void start (Expr *body) { this->body = body; }

		// Return true on interrupt/exception:
		virtual bool next () = 0;
		};

	P_Iterator (char const *ident, unsigned opcode = 0) : Prefix (ident, opcode) {}

	D_Prefix_evalV;
	D_Prefix_evalX;

	// Virtual...
	virtual void evaluate (IterContext &IC, Expr *args);
	};

//
// User-defined functor prefix
//

struct P_Functor : PrefixR {
	FunctorScope *var_scope;	// (functor variables scope)
	PrefixScope *prefix_scope;	// (functor prefixes scope)

	Param *params;				// (parameters)
	Expr *body;					// (definition body)

	unsigned local_no;			// (# of locals)
	Expr ****local_tab;			// (table of locals, [local_no])
	unsigned CFP;				// (current frame pointer; none if != SP_NULL)

	// Constructor
	P_Functor (char *ident);

	// Start functor definition
	void def_open ();

	// End functor definition (started with 'def_open')
	void def_close ();

	// Do functor 'params'/'body' binding
	void def_bind (Param *params, Expr *body);

	// Binding functor locals to stack slots (at FP)
	void select (unsigned FP);

	// Enter new frame
	unsigned enter (Expr *args);

	// Leave current frame, returning to SFP
	void leave (unsigned SFP);

	// Get currently defined functor
	static P_Functor *self ();

	D_Prefix_evalV;
	D_Prefix_evalX;
	D_Prefix_evalR;

	D_Prefix_log_ex;

	D_Prefix_expo_decls;
	D_Prefix_expo_defs;
	D_Prefix_impo_pfx_def;

	D_Prefix_scope_in;
	D_Prefix_scope_out;

	D_Prefix_cleanup;
	D_Prefix_release;
	};

//
// User-defined class prefix
//

struct P_Class : Prefix {
	P_Class *super;				// (superclass, if defined)

	ClassScope *var_scope;		// (class variables scope)
	PrefixScope *prefix_scope;	// (class prefixes scope)

	Param *params;				// (parameters to init instance)

	Expr *ctor;					// (constructor, if defined)
	Expr *dtor;					// (destructor, if defined)

	// NOTE: member_no && member_tab INCLUDE members of superclass.
	// In member_tab superclass members are FIRST.

	unsigned member_no;		// (total # of members)
	Expr ****member_tab;	// (table of members, [member_no])

	// NOTE: virtual_no && virtual_tab INCLUDE virtuals of superclass
	// In virtuals_tab superclass virtuals are FIRST.

	P_Virtual *virtuals;		// (own virtuals list)
	unsigned virtual_no;		// (total # of virtuals)
	P_Functor **virtual_tab;	// (table of virtuals, [virtual_no])

	X_Object *COP;				// (current instance of class)
	
	// Constructor
	P_Class (char *ident, Prefix *super);
	
	// Start class definition
	void def_open ();

	// Bind list of virtuals, originated by class
	void def_virtuals (P_Virtual *virtuals);

	// End class definition, started by 'def_open'
	void def_close ();

	// Conclude class binding
	// (params, constructor, destructor)
	void def_bind (Param *params, Expr *ctor, Expr *dtor);

	// Evaluate 'wrapper' with 'object' as current instance of this class
	// (recursive)
	void with_eval (X_Object *object, struct WrapX &wrapper, Expr *args);

	// Bind this class members to 'object'; from 'root' to self
	void bind_members (X_Object *object, P_Class *root);

	// Bind list of virtuals to class
	P_Virtual *&R_bind_virtual (P_Virtual *last);

	// Locate virtual of class by its ordinal
	P_Virtual *locate (unsigned ordinal);

	// Check, whether the instance of virtual in self differs from super
	bool virtual_redefined (unsigned ordinal);

	// (Get currently defined class)
	static P_Class *self ();

	// Detect current subclass
	P_Class *current_subclass ();

	// Initialise  instance of the class
	void init (X_Object *object, Expr *args);

	// Deinitialise instance of the class
	void shut (X_Object *object);

	D_Prefix_evalV;

	D_Prefix_log_ex;

	D_Prefix_expo_decls;
	D_Prefix_expo_defs;
	D_Prefix_impo_pfx_def;

	D_Prefix_scope_in;
	D_Prefix_scope_out;

	D_Prefix_cleanup;
	D_Prefix_release;
	};

//
//	Virtual functor
//

struct P_Virtual : Prefix {
	P_Class *classref;			// Originator class of this virtual
	unsigned ordinal;			// Ordinal from root
	P_Virtual *next;			// Next in virtuals list

	P_Virtual (char *ident, P_Virtual *next) : Prefix (ident)
		{ this->next = next; }

	// Instantiate this virtual for current class
	static P_Functor *instantiate (Prefix *prefix);

	// Devirtualization of virtual self for class 'classref'
	P_Functor *devirt (P_Class *classref);

	D_Prefix_log_ex;

	D_Prefix_evalV;

	D_Prefix_release;
	};

//
// Explicit namespaces
//

struct P_Space : Prefix {
	VarScope *var_scope;			// (variables scope)
	PrefixScope *prefix_scope;	// (prefixes scope)

	P_Space (char const *ident);

	D_Prefix_scope_in;
	D_Prefix_scope_out;

	D_Prefix_release;
	};
	
//
//
// Expressions
//
//

// Put expression to stream
#define	D_Expr_put				\
	unsigned put (Stream *out)

// Output to log
#define	D_Expr_log				\
	void log (Logger &log)

// Export with exporter
#define	D_Expr_expo			\
	void expo (Exporter &exporter)

// Evaluate self to value
// (full evaluation, if 'full')
#define	D_Expr_evalV			\
	VType evalV (VDatum &val, bool full)

// Evaluate self to expression
// (ALWAYS full evaluation)
#define	D_Expr_evalX			\
	Expr *evalX ()

// Evaluate self to reference
#define	D_Expr_evalR			\
	Expr *&evalR()

// Assign value to self
#define	D_Expr_mutateV			\
	bool mutateV (VType type, VDatum &val)

// Assign expression to self
#define	D_Expr_mutateX			\
	bool mutateX (Expr *expr)

// Calculate hash code
#define	D_Expr_hash				\
	unsigned hash ()

// Identity comparison with (pre-calculated) value
#define	D_Expr_identV			\
	bool identV (VType type, VDatum &val)

// Release expression
#define	D_Expr_release			\
	void release ()

// Convert self to X_List
#define	D_Expr_isList			\
	X_List *isList ()

// Convert self to X_Term
#define	D_Expr_isTerm			\
	X_Term *isTerm ()

//
// Generic expression
//
struct Expr : SynTerm {
	unsigned refs;			// (reference counter)

	// (trivial, moved to Eval.cpp)
	Expr ();

	virtual D_Expr_put;

	virtual D_Expr_log;

	virtual D_Expr_expo;

	virtual	D_Expr_isList;
	virtual	D_Expr_isTerm;

	virtual D_Expr_evalX;
	virtual D_Expr_evalV;
	virtual D_Expr_evalR;

	virtual D_Expr_mutateX;
	virtual D_Expr_mutateV;

	virtual D_Expr_identV;
	virtual D_Expr_hash;

	virtual D_Expr_release;

	// Change link counter (never release)
	void relock (int dir) { refs += dir; }

	// Change link counter (release and return true, if unlinked)
	bool relink (int dir) {
		return (! (refs += dir)) ? (release (), true) : false;
		}
	};

#define	UNDEF		((Expr *) 0)

// Link expression
inline Expr *link_expr (Expr *expr) {
if (expr) expr->relink(1);
return expr;
}	// link_expr

// Unlink expression
inline bool unlink_expr (Expr *expr) {
return expr ? expr->relink(-1) : false;
}	// unlink_expr

// Check expression linkage (release it, if not linked)
inline bool relink_expr (Expr *expr) {
return expr ? expr->relink(0) : false;
}	// relink_expr

// Lock/unlock expression
inline Expr *relock_expr (int dir, Expr *expr) {
if (expr) expr->refs += dir;
return expr;
}	// relock_expr

// Link prefix
void link_prefix (Prefix *prefix);

// Unlink prefix
void unlink_prefix (Prefix *prefix);

// Relink prefix
void relink_prefix (Prefix *prefix);

// Link name
Name *link_name (Name *name);

// Unlink name
void unlink_name (Name *name);

//
// Mutable expression
//

// Non-full evaluation of this mutable
#define	D_ExprR_evalN				\
	VType evalN (VDatum &val)

struct ExprR : Expr {
	D_Expr_evalX;
	D_Expr_evalV;
	D_Expr_mutateX;
	D_Expr_mutateV;

	virtual D_ExprR_evalN;
	};

//
// Evaluation wrappers for expressions
//

#define	D_WrapX_eval			\
	void eval (Expr *args)

#define	D_WrapX_refuse			\
	void refuse ()

// Abstract wrapper
struct WrapX {
	virtual D_WrapX_eval = 0;
	virtual D_WrapX_refuse = 0;
	};

// Evaluate wrapper as value
struct WrapX_V : WrapX {
	// (result)
	VType type;
	VDatum &val;

	WrapX_V (VDatum &_val) : val(_val) {}

	D_WrapX_eval;
	D_WrapX_refuse;
	};

// Evaluate wrapper as expression
struct WrapX_X : WrapX {
	// (result)
	Expr *result;

	D_WrapX_eval;
	D_WrapX_refuse;
	};

// Evaluate wrapper as mutable
struct WrapX_R : WrapX {
	// (result)
	Expr **p_result;

	D_WrapX_eval;
	D_WrapX_refuse;
	};

//
// Functor/class parameter list
//

struct Param : SynTerm {
	AnyVar *var;				// link to variable
	unsigned ordinal;			// parameter #
	unsigned flags;				// parameter flags
	Expr *init;					// default initialiser
	struct Param *next;			// next in list

	Param (AnyVar *var, unsigned flags, Expr *init, struct Param *next) {
		this->var = var; this->flags = flags;
		this->init = link_expr (init);
		this->next = next;
		}
	};

//
// Scalar data items
//

// Fixed int scalar value
struct X_Fixed : Expr {
	S_fixed value;

	// (trivial, moved to Eval.cpp)
	X_Fixed ();
	X_Fixed (S_fixed value);

	D_Expr_evalV;
	D_Expr_evalX;

	D_Expr_mutateV;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_put;

	D_Expr_expo;
	};

// Fixed literal (immutable)
struct X_Fixed_L : X_Fixed {
	X_Fixed_L (S_fixed value) : X_Fixed (value) {}

	D_Expr_mutateV;
	};

// Float scalar value
struct X_Float : Expr {
	S_float value;

	// (trivial, moved to Eval.cpp)
	X_Float ();
	X_Float (S_float value);

	D_Expr_evalV;
	D_Expr_evalX;

	D_Expr_mutateV;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_put;

	D_Expr_expo;
	};

// Float literal
struct X_Float_L : X_Float {
	X_Float_L (S_float value) : X_Float (value) {}

	D_Expr_mutateV;
	};

// String scalar constant
struct X_String : Expr {
	// Inner string content
	// (see definition in "String.h")
	struct StringData *content;
	unsigned offset, length;	// (length and offset in 'content')

	// Construct width 'len' characters of type 'type'
	X_String (unsigned len, C_type type);

	// Construct new string from C-string
	X_String (char const *str);

	// Construct from 'buffer' of 'len' characters of type 'type'
	// (new style string constructor)
	X_String (char *buffer, unsigned len, C_type type);

	// Create another fragment of existing string
	X_String (X_String *string, unsigned offset, unsigned length);

	// Create copy of existing string
	X_String (S_string &s_str);

	// References to string buffer
	S_fixed refcount ();

	// Force change string buffer refcount
	S_fixed refcount_by (S_fixed change);
	
	// X_String peek:
	// returns pointer to string buffer;
	// retrieves length 'len' and type 'type'
	str_ptr fetch (unsigned &len, C_type &type);

	// Make unique (mutable) copy of self
	X_String *uniq ();

	D_Expr_put;

	D_Expr_evalV;
	D_Expr_evalX;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_expo;

	D_Expr_release;
	};

//
//	Lists
//

struct X_List : Expr {
	Expr *first, *next;

	// (moved to: Eval.cpp)
	X_List (Expr *first, Expr *next);

	D_Expr_isList;

	D_Expr_evalV;
	D_Expr_evalX;

	D_Expr_mutateX;
	D_Expr_mutateV;

	D_Expr_put;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_log;

	D_Expr_expo;

	D_Expr_release;
	};

//
//	Terms
//

struct X_Term : Expr {
	Prefix *prefix;
	Expr *args;

	X_Term (Prefix *prefix, Expr *args);

	D_Expr_isTerm;

	D_Expr_evalV;
	D_Expr_evalX;
	D_Expr_evalR;

	D_Expr_mutateX;
	D_Expr_mutateV;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_log;

	D_Expr_expo;

	D_Expr_release;
	};

//
//	Prefix (functor) references
//	

struct X_Prefix : Expr {
	Prefix *prefix;

	X_Prefix (Prefix *prefix)
		{ link_prefix (this->prefix = prefix); }

	D_Expr_evalV;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_expo;

	D_Expr_release;
	};

//
// Variables
//

// Variable reference
struct X_Variable : ExprR {
	AnyVar *name;			// (name referenced by...)

	X_Variable (AnyVar *name);

	D_Expr_evalR;
	D_ExprR_evalN;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_log;

	D_Expr_expo;

	D_Expr_release;
	};

//
// Block
//

// Block expression
struct X_Block : Expr {

	// Sequence node
	struct SNode : SynTerm {
		Expr *expr;			// (expression)
		SNode *next;		// (next node, if not NULL)

		SNode (Expr *expr, SNode *next) {
			this->expr = link_expr (expr);
			this->next = next;
			}

		void release ();
		} *body;

	Expr *last;			// (last expression in block)

	X_Block (Expr *last)
		{ body = 0; this->last = link_expr (last); }

	/* Add new statement to head of block */
	void install (Expr *expr)
		{ body = new ("SNode") SNode (expr, body); }

	// Execute block statements (excluding result)
	Except *exec ();

	D_Expr_evalV;
	D_Expr_evalX;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_log;

	D_Expr_expo;

	D_Expr_release;
	};

//
//	Objects
//

struct X_Object : Expr {
	P_Class *classref;				// (object class)
	Expr **data;				// (pointer to list of members)

	X_Object (P_Class *classref);

	// Evaluate wrapper with this object as instance of 'class'
	// (recursive)
	void eval_with (P_Class *classref, WrapX &wrapper, Expr *args);

	D_Expr_evalV;

	D_Expr_identV;
	D_Expr_hash;

	D_Expr_log;

	D_Expr_expo;

	D_Expr_release;
	};

//
//	External object wrapper
//

struct X_Extern : Expr {
	char const *sign;				// (signature)
	External *external;				// (proper)

	X_Extern (char const *sign, External *external);

	D_Expr_log;
	D_Expr_evalV;
	D_Expr_release;
	};

//
//	Exception
//

struct Except : Expr {
	unsigned type;					// exception type/service
	Expr *value;					// exception value

	Except (unsigned type, Expr *value)
		{ this->type = type; this->value = link_expr (value); }

	D_Expr_evalV;
	D_Expr_log;
	D_Expr_release;

	// (unmake exception, V-context)
	VType unexcept_V (VDatum &val);

	// (unmake exception, X-context)
	Expr *unexcept_X ();
	
	// trap exception
	void trap (Prefix *pfx);
	};

//
//	Modules
//

#define	D_Module_open						\
	void open ()

#define	D_Module_close						\
	void close ()

#define	D_Module_release					\
	void release ()

//
// Modular x_flags:
//

enum XF_flags {
	XF_dialog	= 1 << 0,				// (dialog mode)
	XF_explicit	= 1 << 1,				// (explicit variable declarations)
	XF_cancel	= 1 << 2,				// (cancel evaluation)
	};

struct Module : Prefix {
	Module *parent;		// (parent/caller module, if present)

	unsigned lines;		// # of source lines
	unsigned stmts;		// # of source statements
	unsigned warns;		// # of warnings
	unsigned errors;	// # of errors

	unsigned flags;		// global output flags
	unsigned x_flags;	// module execution flags
	unsigned sig_no;	// signal received (if any)

	// Module argument(s)
	Expr *arguments;

	Module (char *ident, unsigned flags, unsigned x_flags) : Prefix (false, ident) {
		this->parent = parent;
		this->flags = flags;
		this->x_flags = x_flags;

		lines = stmts = 0;

		errors = warns = 0;

		arguments = 0;
		sig_no = 0;
		}	// Module

	// Current module instance:
	static Module *current;

	// Log module info
	void log (Logger &log);

	// Called on entering this module:
	void prolog ();

	// Called on leaving this module:
	static Module *epilog ();

	// Execute top-level statement
	static void exec_stmt (Expr *expr);

	// Report module error
	static void report (Error *error);

	// Check flag(s)
	bool check_flags (unsigned flags) {
	return (x_flags & flags) != 0;
	}	// check_flags

	// Set/reset flag(s)
	void set_flags (unsigned flags, bool on_off) {
	on_off ?
		(x_flags |= flags) :
		(x_flags &= ~flags);
	}	// set_flags

	// Check explicit declarations mode
	static bool check_explicit ();

	// Control explicit declarations mode
	static void set_explicit (bool on_off);

	// Check cancel mode
	static bool check_cancel ();

	// Control cancel mode
	static void set_cancel (bool on_off);

	// Change module flags
	// (temporary/permanent)
	VType change_flags (unsigned new_flags, Expr *expr, VDatum &val);

	// Export module definitions
	static bool export_all (Exporter &exporter, Module *module);

	// Import module definitions
	static bool import_all (Importer &importer, Module *module);

	virtual D_Module_open;
	virtual D_Module_close;
	virtual D_Module_release;
	};

// Root module
struct RootModule : Module {
	ModuleScope *var_scope;		// global variables
	PrefixScope *prefix_scope;	// global prefixes

	RootModule (char *ident, unsigned flags, unsigned x_flags, Expr *args);

	D_Module_open;
	D_Module_close;
	D_Module_release;

	D_Prefix_expo_decls;
	D_Prefix_expo_defs;
	};

//
//	Line number nodes
//

struct X_LineNo : Expr {
	unsigned line_no;
	Expr *expr;

	X_LineNo (unsigned line_no, Expr *expr);

	D_Expr_evalV;
	D_Expr_evalX;
	D_Expr_evalR;
	
	D_Expr_log;

	D_Expr_release;
	};

// Variables definition statement
struct DefVars : Expr {
	DefVars () {}

	D_Expr_log;
	D_Expr_evalV;
	};

// Functor definition statement
struct DefFunctor : Expr {
	P_Functor *func;

	DefFunctor (P_Functor *func) { this->func = func; }

	D_Expr_log;

	D_Expr_evalV;

	D_Expr_expo;

	D_Expr_release;
	};

// Functor group definition statement
struct GroupDefFunctor : Expr {
	P_Functor *func;
	struct GroupDefFunctor *next;

	GroupDefFunctor (P_Functor *func, struct GroupDefFunctor *next)
		{ this->func = func; this->next = next; }

	D_Expr_log;

	D_Expr_evalV;

	D_Expr_expo;

	D_Expr_release;
	};

// Class definition statement
struct DefClass : Expr {
	P_Class *classref;

	DefClass (P_Class *classref) { this->classref = classref; }

	D_Expr_log;

	D_Expr_evalV;

	D_Expr_expo;

	D_Expr_release;
	};

// Class group definition statement
struct GroupDefClass : Expr {
	P_Class *classref;
	struct GroupDefClass *next;

	GroupDefClass (P_Class *classref, struct GroupDefClass *next)
		{ this->classref = classref; this->next = next; }

	D_Expr_evalV;
	D_Expr_release;
	};

//
//	System exceptions
//

// Signal received
struct SysX_Signal : Expr {
	int signo;

	SysX_Signal (int signo) { this->signo = signo; }

	D_Expr_log;
	};

//
//	Version info
//

#define	Version(major,minor,patch)	(((((major) << 8) | (minor)) << 8) | (patch))

#define	Version_Maj(version)	((version >> 16) & 0xFF)
#define	Version_Min(version)	((version >> 8) & 0xFF)
#define	Version_Pat(version)	((version) & 0xFF)

//
// Dynamic file info
//

struct DynamicModInfo {
	char const *dmi_name;				// Module short name
	char const *dmi_description;		// Module description
	unsigned dmi_version;				// Module version
	char const *dmi_date;				// Module build date

	int (* dmi_entry_point) ();			// Entry point func
	int (* dmi_leave_point) ();			// Leave point func

	void log_info (Logger &log);
	};

// ("Dynaload.cpp")
int dynaload_module (char const *file_name, char const *entry_info, DynamicModInfo *dmi);

//
//	Common externals
//

// ("Main.cpp")
extern X_Hash *environment;

// ("Main.cpp")
extern unsigned main_version;
extern char main_date [];

// ("AWL_lex.h")
extern bool include_file (char *file);

//
//	Subsystem init/shut
//

struct SubSystem {
	char const *name;				// (title name for subsystem)

	// Initialize subsystem
	// (expect to return true on success...)
	bool (*do_init) (int order);

	// Shutdown subsystem
	// (expect to return true on success...)
	bool (*do_shut) (int order);

	// Constructor
	SubSystem (char const *name, bool (*do_init) (int order), bool (*do_shut) (int order));

	// Head of list
	static struct SubSystem *list_start;
	
	// Next item in list
	struct SubSystem *next;
	
	// Initialise all subsystems
	// (reverse order)
	static void init_subsystems ();

	// Shutdown all subsystems
	// (direct order)
	static void shut_subsystems ();

	};

#define	DefSubSystem(name,init,shut)	static struct SubSystem _subsystem_##__LINE__ (name, init, shut)

//
//	Error handling
//

#define	D_Error__report				\
	void _report (Logger &log)

// Errors classes
enum EC_type {
	EC_warn,			// (warning)
	EC_error,			// (execution error)
	EC_syntax,			// (syntax error)
	EC_fatal,			// (fatal error)
	EC_intern,			// (internal failure)
	};

// Generic error
struct Error {
	EC_type type;

	void report (Logger &log, Module *module);

	virtual D_Error__report;
	};

// Evaluation errors
struct ExecError : Error {
	ExecError ();

	D_Error__report;
	};

// Warnings
struct WarningError : Error {
	WarningError ();

	D_Error__report;
	};

// Fatal errors
struct FatalError : Error {
	D_Error__report;
	};

// System internal errors
struct InternalError : Error {
	D_Error__report;
	};

//
//	Specific errors
//

#endif

