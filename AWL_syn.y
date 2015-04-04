
/*

	+---+---+---+---+---+---+
	|	'AWL_syn.y':
	|	AWL syntax analizer core.
	|
	|	AWL source.
	|	Written by trilirium
	+---+---+---+---+---+---+

 */

%{

#include <stdlib.h>
#include <string.h>

#include "Defs.h"
#include "Opcodes.h"

#include "NameTab.h"
#include "Logger.h"

// Syntax error
struct SyntaxError : Error {
	char *message;
	
	SyntaxError (char *message) { type = EC_syntax; this->message = message; }

	void _report (Logger &log) {
		log.put_cstr ("Parser error: ");
		log.put_cstr (message);
		}	// _report
	};

unsigned SyntaxErrorCount = 0;

#define	YYSTYPE PSynTerm

#define	YYINITDEPTH	800

// Macro: (Re)Qualify SynTerm as pointer to Type
#define	Q(Type,Term)	((Type *) (Term))

// Declare 'yylex'
int yylex (YYSTYPE *val_p, void *loc_p);

#define YYLEX_PARAM (&yychar)
// #define yylex yylex_r

// Handle all parser errors
static void yyerror (char *message) {
Module::report (new SyntaxError (message));
++ SyntaxErrorCount;

if (! (Module::current->check_flags (XF_dialog)))
	Module::set_cancel (true);
}	// yyerror

%}

%pure_parser

/*
 * definitions of tokens
 */

/* semantic tokens */
%token	L_IDENT L_FIXED L_FLOAT L_STRING

%{

// Lexer interface variables

extern char LV_ident[];			// identifier
extern long LV_long;			// long literal
extern double LV_double;		// double literal

extern char *LV_string;			// string literal
extern unsigned LV_strlen;		// string length
extern int LV_strtype;			// is string UC16??

// Make term for unary expression:
//	{ <OP1> op } => { f_op (OP1) }
//	{ op <OP1> } => { f_op (OP1) }
static Expr *make_Unary (O_Enum op, Expr *op1) {
return new ("Term/unary") X_Term (Prefix::locate (op), op1);
}	// make_Unary

// Make term for binary expression:
//	{ <OP1> op <OP2> } => { f_op (OP1, OP2) }
static Expr *make_Binary (O_Enum op, Expr *op1, Expr *op2) {
return make_Unary (op, new ("List/binary") X_List (op1, op2));
}	// make_Binary

// Make term for ternary expression:
//	{ <OP1> op <OP2> op <OP3> } => { f_op (OP1, OP2, OP3) }
static Expr *make_Ternary (O_Enum op, Expr *op1, Expr *op2, Expr *op3) {
return make_Binary (op, op1, new ("List/ternary") X_List (op2, op3));
}	// make_Ternary

// Make term for unary combined assignment
//	{ <OP1> =: op } => { comb (f_op (OP1)) }
static Expr *make_UnaryMeta (O_Enum a_op, Prefix *prefix, Expr *op1) {
return make_Unary (a_op, new ("Term/comb") X_Term (prefix, op1));
}	// make_UnaryMeta

// Make term for binary combined assignment
//	{ <OP1> = op : <OP2> } => { comb (f_op (OP1, OP2)) }
static Expr *make_BinaryMeta (O_Enum a_op, Prefix *prefix, Expr *op1, Expr *op2) {
return make_UnaryMeta (a_op, prefix, new ("List/comb") X_List (op1, op2));
}	// make_BinaryMeta

//
// Building macros
//	(with qualified expression operands)
//

#define	Undef	0

#define	op_Self(op)		Prefix::locate (op)

// Construct unary: op (opnd1)
#define	op_Unary(op,opnd1)							\
	make_Unary (op, Q(Expr,opnd1))

// Construct binary: op (opnd1, opnd2)
#define	op_Binary(op,opnd1,opnd2)					\
	make_Binary (op, Q(Expr,opnd1), Q(Expr,opnd2))

// Construct ternary: op (opnd1, opnd2, opnd3)
#define	op_Ternary(op,opnd1,opnd2,opnd3)				\
	make_Ternary (op, Q(Expr,opnd1), Q(Expr,opnd2), Q(Expr,opnd3))

// Construct unary combined: a_op (prefix ! opnd1)
#define	op_UnaryMeta(a_op,prefix,opnd1)					\
	make_UnaryMeta (a_op, Q(Prefix,prefix), Q(Expr,opnd1))

// Construct binary combined: a_op (prefix ! (opnd1, opnd2))
#define	op_BinaryMeta(a_op,prefix,opnd1,opnd2)			\
	make_BinaryMeta (a_op, Q(Prefix,prefix), Q(Expr,opnd1), Q(Expr,opnd2))

// Construct list node
#define	c_List(first,next)							\
	new ("List/src") X_List (Q(Expr,first), Q(Expr,next))

// Construct term node
#define	c_Term(prefix,args)							\
	new ("Term/src") X_Term (Q(Prefix,prefix), Q(Expr,args))

//
//	Functors/classes/scopes prologue/epilogue
//

/* Open functor 'functor' */
#define	open_functor(functor)								\
	(Q(P_Functor,functor)->def_open ())

static void done_functor (P_Functor *funcdef, Param *params, Expr *body) {
funcdef->def_close ();
funcdef->def_bind (params, body);
}	// done_functor

/* Close functor 'functor'
   (opened by 'open_functor') */
#define	close_functor(functor,params,body)					\
	(done_functor (Q(P_Functor,functor), Q(Param,params), Q(Expr,body)))

/* Open class 'class' */
#define	open_class(class)							\
	(Q(P_Class,class)->def_open ())

/* Define virtuals originated by class 'class' */
#define class_originates(class,virtuals)			\
	(Q(P_Class,class)->def_virtuals (Q(P_Virtual,virtuals)))

static void done_class (P_Class *classdef, Param *params, Expr *ctor, Expr *dtor) {
classdef->def_close ();
classdef->def_bind (params, ctor, dtor);
}	// done_class

/* Close class 'class' */
#define	close_class(class,params,ctor,dtor)			\
	(done_class (Q(P_Class,class), Q(Param,params), Q(Expr,ctor), Q(Expr,dtor)))

// Functor body [re]definition
// TODO: report error on invalid functor
//	report error on [re]definition
void set_functor_body (Prefix *pfx, Expr *body) {
P_Functor *functor = Cast(P_Functor, pfx);

if (functor) {
	unlink_expr (functor->body);
	link_expr (functor->body = body);
	}
}	// set_functor_body

//
// Context enter/leave
//

// Prefix scope enter
void context_enter (Prefix *prefix) {
if (prefix) prefix->scope_in (false);
}	// context_enter

// Prefix scope leave
void context_leave (Prefix *prefix) {
if (prefix) prefix->scope_out (false);
}	// context_leave

P_Functor *static_devirt (Prefix *p_virtual, Prefix *p_class);

//
// Functor/class link lists
//

static GroupDefFunctor *group_FDef = 0;

// Pop first functor definition from list
static P_Functor *pop_FDef () {
if (group_FDef) {
	GroupDefFunctor *fdef = group_FDef;
	group_FDef = fdef->next;
	return fdef->func;
	}
}	// pop_FDef

// Construct node of F_List
#define	c_FDefNode(functor,next)							\
	new ("GroupDefFunctor") GroupDefFunctor					\
		(Q(P_Functor,functor), Q(GroupDefFunctor,next))

// Eval
Expr *eval_expr = 0;

%}

/*
 * operations & delimiters
 */

/*	'++', '--'	*/
%token	L_Inc L_Dec
/*	'%%'	*/
%token	L_RemI
/*	'<<', '>>'	*/
%token	L_Shl L_Shr
/*	'?<', '?>', '<?>'	*/
%token	L_Min L_Max L_Cmp
/*	'<=', '>='	*/
%token	L_Le L_Ge
/*	'==', '<>'	*/
%token	L_Eq L_Ne

/*	'&&', '||'	*/
%token	L_CAnd L_COr
/*	'~~' */
%token	L_CNot
/*	':>', '<:'	*/
%token	L_Get L_Put
/*	'??'		*/
%token L_Loop
/*	'..'		*/
%token	L_Range
/*	'!!'		*/
%token	L_Qual
/*	'::'		*/
%token	L_With
/*	':='		*/
%token	L_Let
/*	'->'		*/
%token	L_Arrow

/*	'[+]', '[*]', '[~]' */
%token L_LCat L_LRep L_LRev
/*	'[<]', '[>]' */
%token L_LHead L_LTail
/*	'[<-]', '[->]' */
%token L_LPush L_LPop
/*	'[=]' */
%token L_LReduce
/*	'[==]', '[<>]' */
%token L_LEq L_LNe

/* code eval "brackets */
%token L_EvalBeg L_EvalEnd

%token L_Reserve

/*
 *	(start token)
 */

%start	PROG

%%

/*
 	--------------------------------------------------------------
 	Syntax root
 	--------------------------------------------------------------
 */


SYNTAX_ERR:
	error { $$ = Undef; }		/* syntax error recovery */
 ;

RECOVER:	{ yyerrok }
 ;

 
/*{ Program proper }*/
PROG:	ROOT_STMT_SEQ
 |		EVAL_CODE
 ;

/*{ Sequence of root statements }*/
ROOT_STMT_SEQ:
 |	ROOT_STMT

 |	ROOT_STMT ';' ROOT_STMT_SEQ

 |	SYNTAX_ERR ';' RECOVER ROOT_STMT_SEQ
 ;

ROOT_STMT:	STMT					/* root statement -> execute */
	{ Module::exec_stmt (Q(Expr, $1)); }
 ;

/*
 	--------------------------------------------------------------
 	Rules for statements
 	--------------------------------------------------------------
 */

/*{ Single statement }*/
STMT:
	EXPR			/*{ any stand-alone expression is statement }*/

 |	FC_DEF			/*{ functor or class definition }*/
 |	S_DEF			/*{ scope definition }*/

 |					/*{ module locals declaration }*/
	LOCAL_DEF
	{
		Module::set_explicit (true);
		$$ = new ("DefVars") DefVars ();
		}
 ;

/*{ Functor/class definition }*/
FC_DEF:
		F_DEF		/*{ functor definition }*/
	{ $$ = new ("DefFunctor") DefFunctor (Q(P_Functor, $1)); }

 |		F_GROUP_DEF

 |		C_DEF		/*{ class definition }*/
	{ $$ = new ("DefClass") DefClass (Q(P_Class, $1)); }
	
		/* Probably, make group definition for classes? */

 |		V_DEF		/*{ virtual definition }*/
	{ $$ = 0; }
 |		F_REDEF		/*{ predeclared functor re-definition }*/
	{ $$ = 0; }
 ;

/*{ Statement sequence: statements, separated with ';' }*/
O_STMT_ERR:
			{ $$ = Undef; }
 |	error	{ $$ = Undef; yyerrok; }
 |	STMT
 ;

/*
 	--------------------------------------------------------------
 	Functor && class definitions
 	--------------------------------------------------------------
 */

/*{ Parameter or local }*/
LOCAL:	L_IDENT			{ $$ = AnyVar::install (LV_ident, true); }
 ;

/*{ Parameter evaluation flag: '@', if lazy }*/
PARAM_FLAG:
			{ $$ = 0; }
 | '@'		{ $$ = (PSynTerm) 1;	/* ugly, but works... */ }
 ;

/*{ Parameter default initialiser (optional) }*/
PARAM_INIT:
							{ $$ = 0; }
 |	'=' C_EXPR				{ $$ = $2; }
 ;

/*{ Parameter list }*/
PARAM_LIST:
		{ $$ = 0; }

 |		PARAM_FLAG LOCAL PARAM_INIT PARAM_LIST
		{ $$ = new ("Param") Param
			(Q(AnyVar, $2), $1 != 0, Q(Expr, $3), Q(Param, $4)); }
 ;

/*{ Optional parameters list }*/
O_PARAM_LIST:
		{ $$ = 0; }

 |		'(' error ')'			{ yyerrok; $$ = 0; }
 |		'(' PARAM_LIST ')'		{ $$ = $2; }
 ;

/*{ Locals list (no return value) }*/
LOCAL_LIST:
 |		LOCAL LOCAL_LIST
 ;

/*{ Local definition }*/
LOCAL_DEF:
	':' '[' LOCAL_LIST ']'
 |	':' '[' error ']' 			{ yyerrok; }
 ;

/*{ Optional locals list (no return value) }*/
O_LOCAL_LIST:
 |		LOCAL_DEF
 ;

/*{ Functor definition header:
	([optional] parameters + [optional] locals).

	Returns parameters list. }*/
F_DEF_HEAD:
	O_PARAM_LIST O_LOCAL_LIST			{ $$ = $1; }
 ;

/*{ Functor id }*/
F_NAME:	L_IDENT
		{ $$ = new ("P_Functor") P_Functor (LV_ident); }
 ;

/*{ Functor id list }*/
F_NAME_LIST:
		F_NAME					{ $$ = c_FDefNode ($1, 0); }
 |		F_NAME F_NAME_LIST		{ $$ = c_FDefNode ($1, $2); }
 ;

/*{ Functor body }*/
F_BODY:
	EXPR

 |	error	{ yyerrok; $$ = Undef; }
 ;

/*{ Functor single definition }*/
F_DEF:		'!' F_NAME
			{ open_functor ($$ = $2); }

			/* functor header & body */
			F_DEF_HEAD '=' F_BODY
			{ close_functor ($$ = $3, $4, $6); }
 ;

/*{ Functor group definition }*/
F_GROUP_DEF:	'!'
				'{' F_NAME_LIST '}'
				{ group_FDef = Q(GroupDefFunctor, $3); }
				'='
				'{' F_GROUP_SEQ '}'
				{ $$ = $3; }
 ;

/*{ Functor group element }*/
F_GROUP_NODE:
			{ open_functor ($$ = pop_FDef ()); }
			F_DEF_HEAD '=' EXPR
			{ close_functor ($1, $2, $4); }
 ;

/*{ Functor group sequence }*/
F_GROUP_SEQ:
			F_GROUP_NODE
 |			F_GROUP_SEQ ',' F_GROUP_NODE
 ;

/*{ Virtual definition }*/
V_DEF:		'!' '#' PREFIX

			{ open_functor ($$ = P_Virtual::instantiate (Q(Prefix, $3))); }
			F_DEF_HEAD '=' EXPR
			{ close_functor ($4, $5, $7); }
 ;

/*{ Optional functor/class def }*/
O_FC_DEF:

 |	FC_DEF { Module::exec_stmt (Q(Expr, $1)); }

 |	error

 ;

/*{ List of definitions }*/
FC_LIST:
	O_FC_DEF

 |	FC_LIST ',' O_FC_DEF
 ;

/*{ List of definitions (optional) }*/
O_FC_LIST:

 |	'{' FC_LIST '}'
 ;

/*{ Optional class superclass }*/
O_SUPER:				{ $$ = 0; }

 |	'[' PREFIX ']'		{ $$ = $2; }
 ;

/*{ Optional class constructor }*/
O_CTOR:					{ $$ = 0; }

 |	'=' C_EXPR			{ $$ = $2; }
 ;

/*{ Optional class destructor }*/
O_DTOR:					{ $$ = 0; }

 |	'~' C_EXPR			{ $$ = $2; }
 ;

/*{ Virtuals declaration list }*/
VIRT_LIST:				{ $$ = 0; }

 |	VIRT_LIST L_IDENT
	{ $$ = new ("P_Virtual") P_Virtual (LV_ident, Q(P_Virtual, $1)); }
 ;

/*{ Optional class virtuals list }*/
O_VIRTUALS:				{ $$ = 0; }

 |	'#'
	'{' VIRT_LIST '}'	{ $$ = $3; }
 ;

/*{ Class definition }*/
C_DEF:	/*{ class header }*/
		L_Qual O_SUPER L_IDENT
		{ open_class ($$ = new ("P_Class") P_Class (LV_ident, Q(Prefix, $2))); }

			/*{ optional parameter list }*/
		O_PARAM_LIST
			/*{ optional locals list }*/
		O_LOCAL_LIST
			/*{ optional constructor }*/
		O_CTOR
			/*{ optional destructor }*/
		O_DTOR
			/*{ optional virtuals }*/
		O_VIRTUALS

			/*{ (virtuals binding must be done BEFORE local defs!) }*/
		{ class_originates ($4, $9); }

			/*{ all local functors/classes/virtuals definitions }*/
		O_FC_LIST

		{ close_class (($$ = $4), $5, $7, $8); }
 ;

/*{ Scope definition -- reserved }*/
S_DEF:	'!' '%' L_IDENT
		{ context_enter (Q (Prefix, $$ = new ("P_Space") P_Space (LV_ident))); }
		'{' ROOT_STMT_SEQ '}'
		{ context_leave (Q (Prefix, $4)); }
 ;

/*{ Functor [re]refinition }*/
F_REDEF:	'!' PREFIX L_Let
			{ context_enter (Q(Prefix, $2)); } EXPR { context_leave (Q(Prefix, $2)); }
			{ set_functor_body (Q(Prefix, $2), Q(Expr, $5)); }
 ;

/*
 	--------------------------------------------------------------
 	Rules for expressions
 	--------------------------------------------------------------
 */

/*{ Scalar literal }*/
L_LIT:
	/*{ fixed point literal }*/
	L_FIXED		{ $$ = new ("Fixed/src") X_Fixed_L (LV_long); }

	/*{ floating point literal }*/
 |	L_FLOAT		{ $$ = new ("Float/src") X_Float_L (LV_double); }

	/*{ string literal }*/
 |	L_STRING	{
				if (LV_strtype < 0)
					$$ = new ("Fixed/char") X_Fixed_L (LV_string[0] | (LV_string[1] << 8));
				else
					$$ = new ("String/src") X_String (LV_string, LV_strlen, LV_strtype);

				LV_strtype = 0;
				}
 ;

/*{ Prefix reference }*/
PREFIX:	L_IDENT			{ $$ = Prefix::lookup (LV_ident); }
 ;

/*{ Variable reference }*/
VARIABLE:	L_IDENT
	{ $$ = new ("Variable/src") X_Variable (AnyVar::install (LV_ident, false)); }
 ;

/* TODO: error reset????
EXPR_ERR:
	EXPR

 |	error { $$ = Undef; yyerrok; }
 ;
 */

/*{ List of any expressions (comma-separated) }*/
XP_SEQ:
			{ $$ = Undef; }

 |	EXPR			/*{ single expression }*/

 |	error ',' XP_SEQ
	{ yyerrok; $$ = c_List (Undef, $3); }

 |	EXPR ',' XP_SEQ			/*{ delimited by comma }*/
	{ $$ = c_List ($1, $3); }
 ;

/* List of closed expressions (not separated) */
CXP_SEQP:
	C_EXPR

 |	C_EXPR CXP_SEQP
			{ $$ = c_List ($1, $2); }
 ;

/*{ List of closed expressions (with termination) }*/
CXP_SEQT:
	':'		{ $$ = 0; }

 |	C_EXPR CXP_SEQT
			{ $$ = c_List ($1, $2); }
 ;

/*{ List of closed expressions (any) }*/
CXP_SEQ:
				{ $$ = 0; }
 |	CXP_SEQP

 |	CXP_SEQT

/* TTT
 |	error		{ yyerrok; $$ = Undef; }
 */
 ;

/*{ Closed expression (allowed with prefix) }*/
C_EXPR_PFX:
		L_LIT					/*{ literal }*/

 |		VARIABLE				/*{ variable reference }*/

 |		'(' XP_SEQ ')'			/*{ list of any expressions }*/
			{ $$ = $2; }

 |		'(' error ')'			/*{ error handler }*/
			{ yyerrok; $$ = Undef; }
 ;

/*{ Block expression }*/
BLOCK:	O_STMT_ERR			{ $$ = new ("Block/src") X_Block (Q(Expr, $1)); }

 |		STMT ';' BLOCK		{ Q(X_Block, $$ = $3)->install (Q(Expr, $1)); }

 |		SYNTAX_ERR ';' RECOVER BLOCK		{ $$ = $4; }
 ;

/*{ Closed expression }*/
C_EXPR:
		C_EXPR_PFX

 |		'[' CXP_SEQ ']'				/*{ list of closed expressions }*/
			{ $$ = $2; }

 |		'[' error ']'				/*{ list of closed expressions -- error handler! }*/
			{ yyerrok; $$ = Undef; }

 |		'{' BLOCK '}'				/*{ block expression }*/
			{ $$ = $2; }

 |		'!' '.'						/*{ current class reference }*/
			{ $$ = new ("Prefix") X_Prefix (P_Class::self ()); }
 ;

XP_ATERM:
		/*{ (closed expression) }*/
		C_EXPR

		/*{ prefixed expression }*/
 |		PREFIX C_EXPR_PFX					/*[ $1:($2) ]*/
 			{ $$ = c_Term ($1, $2); }

		/*{ prefixed expression (explicit) }*/
 |		PREFIX '^' XP_ATERM					/*[ $1:($3) ]*/
			{ $$ = c_Term ($1, $3); }

		/*{ list compose expression }*/
 |		C_EXPR L_With XP_ATERM				/*[ ($1, $3) ]*/
			{ $$ = c_List ($1, $3); }

		/*{ prefixed with list compose }*/
 |		PREFIX C_EXPR_PFX L_With XP_ATERM		/*[ $1:($2, $4) ]*/
			{ $$ = c_Term ($1, c_List ($2, $4)); }

 ;

/*{ Expression: terminal (postfix) }*/
XP_TERM:
		XP_ATERM

		/*{ item of list }*/
 |		XP_TERM '[' EXPR ']'	/*[ l_item($3, $1) ]*/
			{ $$ = op_Binary (Op_LItem, $3, $1); }

		/*{ element of array }*/
 |		XP_TERM '{' XP_SEQ '}'	/*[ a_elem($1, $3) ]*/
			{ $$ = op_Binary (Op_AElem, $1, $3); }

		/*{ tail of list }*/
 |		XP_TERM	L_LTail			/*[ l_tail($1) ]*/
			{ $$ = op_Unary (Op_LTail, $1); }
 ;

/*{ Expression: N-terminal }*/
XP_NTERM:	XP_TERM

	/*{ (WITH object wrapper) }*/
 |		XP_TERM '.' XP_NTERM		/*[ with($1, $3) ]*/
			{ $$ = op_Binary (Op_With, $1, $3); }

	/*{ (apply functor ref to argument(s)) }*/
 |		XP_TERM '!' XP_NTERM		/*[ apply($1, $3) ]*/
			{ $$ = op_Binary (Op_Apply, $1, $3); }

	/*{ (hash element accessor) }*/
 |		XP_TERM L_Arrow XP_NTERM	/*[ h_elem($1, $3) ]*/
			{ $$ = op_Binary (Op_HElem, $1, $3); }

	/*{ (namespace qualification) }*/
 |		PREFIX L_Qual				/*[ $1 . $4 ]*/
			{ context_enter (Q(Prefix, $1)); }
		XP_NTERM
			{ context_leave (Q(Prefix, $1)); $$ = $4; }

	/*{ (static devirtualisation) }*/
 |		PREFIX '#' PREFIX
			{ $$ = new ("Prefix/Devirt") X_Prefix
				(static_devirt (Q(Prefix, $3), Q(Prefix, $1))); }
 ;

/*{ Expression: terminal }*/
XP_PTERM:		XP_NTERM

		/*{ head of list }*/
 |		L_LHead XP_PTERM			/*[ l_head($2) ]*/
			{ $$ = op_Unary (Op_LHead, $2); }
 ;

/*{ Expression: unary postfix }*/
XP_POST:
	XP_PTERM

	/*{ (post increment) }*/
 |	XP_POST L_Inc		/*[ inc_p($1) ]*/
		{ $$ = op_Unary (Op_IncP, $1); }
	/*{ (post decrement) }*/
 |	XP_POST L_Dec		/*[ dec_p($1) ]*/
		{ $$ = op_Unary (Op_DecP, $1); }

	/*{ (string slice) }*/
 |	XP_POST '$' '[' EXPR ']'	/*[ s_slice($4, $1) ]*/
		{ $$ = op_Binary (Op_SSlice, $4, $1); }
 ;

/*{ Expression: anonimous functor definition }*/
F_LAMBDA:	'!'
			{ open_functor ($$ = new ("FunctorLambda") P_Functor (0)); }

			/* functor header & body */
			F_DEF_HEAD '=' C_EXPR
			{ close_functor ($$ = $2, $3, $5); }
 ;

/*{ Expression: unary prefix }*/
XP_PRE:
	/*{ (unary postfix) }*/
	XP_POST

	/*{ (pre increment) }*/
 |	L_Inc XP_PRE		/*[ inc($2) ]*/
		{ $$ = op_Unary (Op_Inc, $2); }
	/*{ (pre decrement) }*/
 |	L_Dec XP_PRE		/*[ dec($2) ]*/
		{ $$ = op_Unary (Op_Dec, $2); }

	/*{ (numeric absolute value) }*/
 |	'+' XP_PRE			/*[ abs($2) ]*/
		{ $$ = op_Unary (Op_Abs, $2); }
	/*{ (numeric negation) }*/
 |	'-' XP_PRE			/*[ neg($2) ]*/
		{ $$ = op_Unary (Op_Neg, $2); }
	/*{ (bitwise complement) }*/
 |	'~' XP_PRE			/*[ not($2) ]*/
		{ $$ = op_Unary (Op_Not, $2); }
	/*{ (logical NOT) }*/
 |	L_CNot XP_PRE		/*[ c_not($2) ]*/
		{ $$ = op_Unary (Op_CNot, $2); }
	/*{ (numeric sign) }*/
 |	L_Cmp XP_PRE		/*[ sgn($2) ]*/
		{ $$ = op_Unary (Op_Sign, $2); }

	/*{ (string length) }*/
 |	'#' '$' XP_PRE		/*[ s_len($3) ]*/
		{ $$ = op_Unary (Op_SLen, $3); }

	/*{ (string type) }*/
 |	'+' '$' XP_PRE		/*[ s_type($3) ]*/
		{ $$ = op_Unary (Op_SType, $3); }

	/*{ (string reverse) }*/
 |	'~' '$' XP_PRE		/*[ s_rev($3) ]*/
		{ $$ = op_Unary (Op_SRev, $3); }

	/*{ (list length) }*/
 |	'#' XP_PRE			/*[ l_len($2) ]*/
		{ $$ = op_Unary (Op_LLength, $2); }

	/*{ (list copy) }*/
 |	L_LCat XP_PRE		/*[ l_copy($2) ]*/
		{ $$ = op_Unary (Op_LCopy, $2); }

	/*{ (list reverse) }*/
 |	L_LRev XP_PRE		/*[ l_rev($2) ]*/
		{ $$ = op_Unary (Op_LRev, $2); }

	/*{ (devaluation) }*/
 |	'@' XP_PRE			/*[ deval($2) ]*/
		{ $$ = op_Unary (Op_DeVal, $2); }

	/*{ (revaluation) }*/
 |	'^' XP_PRE			/*[ reval($2) ]*/
		{ $$ = op_Unary (Op_ReVal, $2); }

	/*{ (functor reference) }*/
 |	'!' PREFIX
		{ $$ = new ("Prefix/src") X_Prefix (Q(Prefix, $2)); }

	/*{ (anonimous functor definition) }*/
 |	F_LAMBDA
		{ $$ = new ("Lambda/src") X_Prefix (Q(Prefix, $1)); }
 ;

/*{ Expression: multiplicative }*/
XP_MUL:
	/*{ (basic prefix) }*/
	XP_PRE

	/*{ (numeric multiplication) }*/
 |	XP_MUL '*' XP_PRE		/*[ mul($1, $3) ]*/
	{ $$ = op_Binary (Op_Mul,  $1, $3); }
	/*{ (numeric division) }*/
 |	XP_MUL '/' XP_PRE		/*[ div($1, $3) ]*/
	{ $$ = op_Binary (Op_Div,  $1, $3); }
	/*{ (integer division) }*/
 |	XP_MUL '%' XP_PRE		/*[ idiv($1, $3) ]*/
	{ $$ = op_Binary (Op_DivI, $1, $3); }
	/*{ (integer remainder) }*/
 |	XP_MUL L_RemI XP_PRE	/*[ irem($1, $3) ]*/
	{ $$ = op_Binary (Op_RemI, $1, $3); }

	/*{ (bit shift left) }*/
 |	XP_MUL L_Shl XP_PRE		/*[ shl($1, $3) ]*/
	{ $$ = op_Binary (Op_Shl, $1, $3); }
	/*{ (bit shift right) }*/
 |	XP_MUL L_Shr XP_PRE		/*[ shr($1, $3) ]*/
	{ $$ = op_Binary (Op_Shr, $1, $3); }

	/*{ (string replication) }*/
 |	XP_MUL '*' '$' XP_PRE	/*[ s_rep($1, $4) ]*/
	{ $$ = op_Binary (Op_SRep, $1, $4); }

	/*{ (incremental repetition pattern) }*/
 |	XP_MUL '*' L_Inc '$' C_EXPR		/*[ rx_rep_inc($5, $1) ]*/
	{ $$ = op_Binary (Op_RXRepInc, $5, $1); }
	/*{ (decremental repetition pattern) }*/
 |	XP_MUL '*' L_Dec '$' C_EXPR		/*[ rx_rep_dec($5, $1) ]*/
	{ $$ = op_Binary (Op_RXRepDec, $5, $1); }

	/*{ (list replication) }*/
 |	XP_MUL L_LRep XP_PRE	/*[ l_rep($3, $1) ]*/
	{ $$ = op_Binary (Op_LRep, $3, $1); }

	/*{ (string search backward) }*/
 |	XP_MUL L_Shl '$' XP_PRE		/*[ s_findlast($1, $4) ]*/
	{ $$ = op_Binary (Op_SFindBak, $1, $4); }
	/*{ (string search forward) }*/
 |	XP_MUL L_Shr '$' XP_PRE		/*[ s_findfirst($1, $4) ]*/
	{ $$ = op_Binary (Op_SFindFor, $1, $4); }
 ;

/*{ Expression: additive }*/
XP_ADD:
	/*{ (basic multiplicative) }*/
	XP_MUL

	/*{ (numeric addition) }*/
 |	XP_ADD '+' XP_MUL		/*[ add($1, $3) ]*/
	{ $$ = op_Binary (Op_Add, $1, $3); }
	/*{ (numeric subtraction) }*/
 |	XP_ADD '-' XP_MUL		/*[ sub($1, $3) ]*/
	{ $$ = op_Binary (Op_Sub, $1, $3); }

	/*{ (string concatenation) }*/
 |	XP_ADD '+' '$' XP_MUL	/*[ s_cat($1, $4) ]*/
	{ $$ = op_Binary (Op_SCat, $1, $4); }

	/*{ (list concatenation) }*/
 |	XP_ADD L_LCat XP_MUL	/*[ l_cat($1, $3) ]*/
	{ $$ = op_Binary (Op_LCat, $1, $3); }
 ;

/*{ Expression: maximum/minimum }*/
XP_MUM:
	/*{ (basic additive) }*/
	XP_ADD

	/*{ (numeric minimum) }*/
 |	XP_MUM L_Min XP_ADD			/*[ min($1, $3) ]*/
	{ $$ = op_Binary (Op_Min, $1, $3); }
	/*{ (numeric maximum) }*/
 |	XP_MUM L_Max XP_ADD			/*[ max($1, $3) ]*/
	{ $$ = op_Binary (Op_Max, $1, $3); }

	/*{ (string minimum) }*/
 |	XP_MUM L_Min '$' XP_ADD		/*[ s_min($1, $4) ]*/
	{ $$ = op_Binary (Op_SMin, $1, $4); }
	/*{ (string maximum) }*/
 |	XP_MUM L_Max '$' XP_ADD		/*[ s_max($1, $4) ]*/
	{ $$ = op_Binary (Op_SMax, $1, $4); }
 ;

/*{ Expression: comparative }*/
XP_CMP:
	/*{ (basic maximum/minimum) }*/
	XP_MUM

	/*{ (numeric less than) }*/
 |	XP_MUM '<'  XP_MUM			/*[ lt($1, $3) ]*/
	{ $$ = op_Binary (Op_LT, $1, $3); }
	/*{ (numeric less than or equal) }*/
 |	XP_MUM L_Le XP_MUM			/*[ le($1, $3) ]*/
	{ $$ = op_Binary (Op_LE, $1, $3); }
	/*{ (numeric greater than) }*/
 |	XP_MUM '>'  XP_MUM			/*[ gt($1, $3) ]*/
	{ $$ = op_Binary (Op_GT, $1, $3); }
	/*{ (numeric greater than or equal) }*/
 |	XP_MUM L_Ge XP_MUM			/*[ ge($1, $3) ]*/
	{ $$ = op_Binary (Op_GE, $1, $3); }

	/*{ (numeric equality) }*/
 |	XP_MUM L_Eq XP_MUM			/*[ eq($1, $3) ]*/
	{ $$ = op_Binary (Op_EQ, $1, $3); }
	/*{ (numeric inequality) }*/
 |	XP_MUM L_Ne XP_MUM			/*[ ne($1, $3) ]*/
	{ $$ = op_Binary (Op_NE, $1, $3); }

	/*{ (numeric compare) }*/
 |	XP_MUM L_Cmp XP_MUM			/*[ cmp($1, $3) ]*/
	{ $$ = op_Binary (Op_Cmp, $1, $3); }

	/*{ (string less than) }*/
 |	XP_MUM '<' '$'  XP_MUM		/*[ s_lt($1, $4) ]*/
	{ $$ = op_Binary (Op_SLT, $1, $4); }
	/*{ (string less than or equal) }*/
 |	XP_MUM L_Le	'$' XP_MUM		/*[ s_le($1, $4) ]*/
	{ $$ = op_Binary (Op_SLE, $1, $4); }
	/*{ (string greater than) }*/
 |	XP_MUM '>' '$'  XP_MUM		/*[ s_gt($1, $4) ]*/
	{ $$ = op_Binary (Op_SGT, $1, $4); }
	/*{ (string greater than or equal) }*/
 |	XP_MUM L_Ge '$' XP_MUM		/*[ s_ge($1, $4) ]*/
	{ $$ = op_Binary (Op_SGE, $1, $4); }

	/*{ (string equality) }*/
 |	XP_MUM L_Eq '$' XP_MUM		/*[ s_eq($1, $4) ]*/
	{ $$ = op_Binary (Op_SEQ, $1, $4); }
	/*{ (string inequality) }*/
 |	XP_MUM L_Ne '$' XP_MUM		/*[ s_ne($1, $4) ]*/
	{ $$ = op_Binary (Op_SNE, $1, $4); }

	/*{ (string compare) }*/
 |	XP_MUM L_Cmp '$' XP_MUM		/*[ s_cmp($1, $4) ]*/
	{ $$ = op_Binary (Op_SCmp, $1, $4); }

	/*{ (identity: structural equality) }*/
 |	XP_MUM L_LEq XP_MUM			/*[ ident($1, $3) ]*/
	{ $$ = op_Binary (Op_Ident, $1, $3); }
	/*{ (difference: structural inequality) }*/
 |	XP_MUM L_LNe XP_MUM			/*[ differ($1, $3) ]*/
	{ $$ = op_Binary (Op_Differ, $1, $3); }

 ;

/*{ Expression: bitwise/logical }*/
XP_BIT:
	/*{ (basic comparative) }*/
	XP_CMP

	/*{ (bitwise/logical AND) }*/
 |	XP_BIT '&' XP_CMP			/*[ and($1, $3) ]*/
	{ $$ = op_Binary (Op_And, $1, $3); }
	/*{ (bitwise/logical OR) }*/
 |	XP_BIT '|' XP_CMP			/*[ or($1, $3) ]*/
	{ $$ = op_Binary (Op_Or,  $1, $3); }
	/*{ (bitwise/logical XOR) }*/
 |	XP_BIT '~' XP_CMP			/*[ xor($1, $3) ]*/
	{ $$ = op_Binary (Op_Xor, $1, $3); }

	/*{ (patterns concatenation) }*/
 |	XP_BIT '&' '$' XP_CMP	/*[ rx_cat($1, $4) ]*/
	{ $$ = op_Binary (Op_RXCat, $1, $4); }
	/*{ (patterns alternation) }*/
 |	XP_BIT '|' '$' XP_CMP	/*[ rx_alt($1, $4) ]*/
	{ $$ = op_Binary (Op_RXAlt, $1, $4); }

 ;

/*{ optional bit expression }*/
O_XP_BIT:		{ $$ = Undef; }
 |	XP_BIT
 ;

/*{ Expression: conditional/iterative }*/
XP_CND:
	/*{ (basic bitwise) }*/
	XP_BIT

	/*{ (values range) }*/
 |	O_XP_BIT L_Range O_XP_BIT		/*[ ($1, $3) ]*/
		{ $$ = new ("Range") X_List (Q(Expr, $1), Q(Expr, $3)); }

	/*{ (conditional AND clause) }*/
 |	XP_BIT L_CAnd XP_CND			/*[ c_and($1, $3) ]*/
	{ $$ = op_Binary (Op_CAnd, $1, $3); }
	/*{ (conditional OR clause) }*/
 |	XP_BIT L_COr XP_CND				/*[ c_or($1, $3) ]*/
	{ $$ = op_Binary (Op_COr, $1, $3); }

	/*{ (binary conditional IF clause) }*/
 |	XP_BIT '?' XP_CND ':'				/*[ if($1, $3, ) ]*/
		{ $$ = op_Ternary (Op_If, $1, $3, Undef); }
	/*{ (binary conditional UNLESS clause) }*/
 |	XP_BIT '~' '?' XP_CND ':'			/*[ unless($1, $4, ) ]*/
		{ $$ = op_Ternary (Op_Unless, $1, $4, Undef); }

	/*{ (ternary conditional IF clause) }*/
 |	XP_BIT '?' XP_CND ':' XP_CND		/*[ if($1, $3, $5) ]*/
		{ $$ = op_Ternary (Op_If, $1, $3, $5); }
	/*{ (ternary conditional UNLESS clause) }*/
 |	XP_BIT '~' '?' XP_CND ':' XP_CND	/*[ unless($1, $4, $6) ]*/
		{ $$ = op_Ternary (Op_Unless, $1, $4, $6); }

	/*{ (precondition loop WHILE clause) }*/
 |	XP_BIT L_Loop XP_CND				/*[ while($1, $3) ]*/
		{ $$ = op_Binary (Op_WhilePre, $1, $3); }
	/*{ (postcondition loop WHILE clause) }*/
 |	L_Loop C_EXPR XP_BIT				/*[ do_while($3, $2) ]*/
		{ $$ = op_Binary (Op_WhilePost, $3, $2); }

	/*{ (precondition loop UNTIL clause) }*/
 |	XP_BIT '~' L_Loop XP_CND			/*[ until($1, $4) ]*/
		{ $$ = op_Binary (Op_UntilPre, $1, $4); }
	/*{ (postcondition loop UNTIL clause) }*/
 |	'~' L_Loop C_EXPR XP_BIT			/*[ do_until($4, $3) ]*/
		{ $$ = op_Binary (Op_UntilPost, $4, $3); }

	/*{ (incremental FOR loop clause) }*/
 |	L_Loop C_EXPR '=' C_EXPR L_Inc ':' XP_CND			/*[ for_inc($2, $4, $7) ]*/
		{ $$ = op_Ternary (Op_ForInc, $2, $4, $7); }

	/*{ (decremental FOR loop clause) }*/
 |	L_Loop C_EXPR '=' C_EXPR L_Dec ':' XP_CND			/*[ for_dec($2, $4, $7) ]*/
		{ $$ = op_Ternary (Op_ForDec, $2, $4, $7); }

	/*{ (TIMES loop clause) }*/
 |	L_Loop C_EXPR ':' XP_CND							/*[ times($2, $4) ]*/
		{ $$ = op_Binary (Op_Times, $2, $4); }
 ;

/*{ Unary operations
	(for combined assignment) }*/
OPC_UN:
	/*{ (combine assignment with absolute value) }*/
	'+'			{ $$ = op_Self (Op_Abs); }		/*[ !abs ]*/
	/*{ (combine assignment with negation) }*/
 |	'-'			{ $$ = op_Self (Op_Neg); }		/*[ !neg ]*/
	/*{ (combine assignment with bit inversion) }*/
 |	'~'			{ $$ = op_Self (Op_Not); }		/*[ !not ]*/
	/*{ (combine assignment with string reverse) }*/
 |	'~' '$'		{ $$ = op_Self (Op_SRev); }		/*[ !s_rev ]*/
 ;

/*{ Binary operations
	(for combined assignment / reduction) }*/
OPC_BIN:
	/*{ (combine assignment with addition) }*/
	'+'			{ $$ = op_Self (Op_Add); }		/*[ !add ]*/
	/*{ (combine assignment with subtraction) }*/
 |	'-'			{ $$ = op_Self (Op_Sub); }		/*[ !sub ]*/

	/*{ (combine assignment with multiplication) }*/
 |	'*'			{ $$ = op_Self (Op_Mul); }		/*[ !mul ]*/
	/*{ (combine assignment with division) }*/
 |	'/'			{ $$ = op_Self (Op_Div); }		/*[ !div ]*/
	/*{ (combine assignment with integer division) }*/
 |	'%'			{ $$ = op_Self (Op_DivI); }		/*[ !idiv ]*/
	/*{ (combine assignment with integer remainder) }*/
 |	L_RemI		{ $$ = op_Self (Op_RemI); }		/*[ !irem ]*/

	/*{ (combine assignment with numeric minimum) }*/
 |	L_Min		{ $$ = op_Self (Op_Min); }		/*[ !min ]*/
	/*{ (combine assignment with numeric maximum) }*/
 |	L_Max		{ $$ = op_Self (Op_Max); }		/*[ !max ]*/

	/*{ (combine assignment with bit shift left) }*/
 |	L_Shl		{ $$ = op_Self (Op_Shl); }		/*[ !shl ]*/
	/*{ (combine assignment with bit shift right) }*/
 |	L_Shr		{ $$ = op_Self (Op_Shr); }		/*[ !shr ]*/

	/*{ (combine assignment with bit AND) }*/
 |	'&'			{ $$ = op_Self (Op_And); }		/*[ !and ]*/
	/*{ (combine assignment with bit OR) }*/
 |	'|'			{ $$ = op_Self (Op_Or); }		/*[ !or ]*/
	/*{ (combine assignment with bit XOR) }*/
 |	'~'			{ $$ = op_Self (Op_Xor); }		/*[ !xor ]*/

	/*{ (combine assignment with string concatenation) }*/
 |	'+' '$'		{ $$ = op_Self (Op_SCat); }		/*[ !s_cat ]*/
	/*{ (combine assignment with string replication) }*/
 |	'*' '$'		{ $$ = op_Self (Op_SRep); }		/*[ !s_rep ]*/

	/*{ (combine assignment with string minimum) }*/
 |	L_Min '$'	{ $$ = op_Self (Op_SMin); }		/*[ !s_min ]*/
	/*{ (combine assignment with string maximum) }*/
 |	L_Max '$'	{ $$ = op_Self (Op_SMax); }		/*[ !s_max ]*/

 ;

/* Expression: assignment/exchange */
XP_EXX:
	/*{ (basic conditional) }*/
	XP_CND

	/*{ (standard assignment) }*/
 |	XP_PTERM '=' XP_EXX							/*[ set($1, $3) ]*/
		{ $$ = op_Binary (Op_Set, $1, $3); }
	/*{ (alternate assignment) }*/
 |	'=' XP_PTERM ':' XP_EXX						/*[ set_a($2, $4) ]*/
		{ $$ = op_Binary (Op_SetA, $2, $4); }

	/*{ (list copy) }*/
 |	XP_PTERM L_LReduce XP_EXX					/*[ l_copy ($1, $3) ]*/
		{ $$ = op_Binary (Op_LCopy, $1, $3); }

	/*{ (assignment combined with unary) }*/
 |	XP_PTERM '=' ':' OPC_UN						/*[ comb ($4:($1)) ]*/
		{ $$ = op_UnaryMeta (Op_Comb, $4, $1); }
	/*{ (assignment combined with unary - alternate syntax) }*/
 |	'=' XP_PTERM ':' OPC_UN						/*[ comb_a ($4:($2)) ]*/
		{ $$ = op_UnaryMeta (Op_CombA, $4, $2); }

	/*{ (list reduction combined with binary) }*/
 |	L_LReduce OPC_BIN XP_PTERM					/*[ reduce ($2:($3)) ]*/
		{ $$ = op_UnaryMeta (Op_Reduce, $2, $3); }

	/*{ (assignment combined with binary) }*/
 |	XP_PTERM '=' OPC_BIN ':' XP_EXX				/*[ comb ($3:($1, $5) ]*/
		{ $$ = op_BinaryMeta (Op_Comb, $3, $1, $5); }
	/*{ (assignment combined with binary - alternate syntax) }*/
 |	'=' XP_PTERM OPC_BIN ':' XP_EXX				/*[ comb_a ($3:($2, $5) ]*/
		{ $$ = op_BinaryMeta (Op_CombA, $3, $2, $5); }

	/*{ (lazy assignment) }*/
 |	XP_PTERM L_Let XP_EXX						/*[ let($1, $3) ]*/
		{ $$ = op_Binary (Op_Let, $1, $3); }

	/*{ (exchange mutables) }*/
 |	XP_PTERM L_Let ':' XP_PTERM					/*[ swap($1, $4) ]*/
		{ $$ = op_Binary (Op_Swap, $1, $4); }

	/*{ (get (mutable) arguments from input stream) }*/
 |	XP_PTERM L_Get XP_EXX						/*[ f_get($1, $3) ]*/
		{ $$ = op_Binary (Op_Get, $1, $3); }
	/*{ (put arguments (as strings) to output stream) }*/
 |	XP_PTERM L_Put XP_EXX						/*[ f_put($1, $3) ]*/
		{ $$ = op_Binary (Op_Put, $1, $3); }

	/*{ (get (mutable) arguments from standard input) }*/
 |	L_Get XP_PRE								/*[ f_get((), $2) ]*/
		{ $$ = op_Binary (Op_Get, Undef, $2); }
	/*{ (put arguments (as strings) to standard output) }*/
 |	L_Put XP_PRE								/*[ f_put((), $2) ]*/
		{ $$ = op_Binary (Op_Put, Undef, $2); }

	/*{ (push arguments to list) }*/
 |	XP_PTERM L_LPush XP_EXX						/*[ l_push($1, $3) ]*/
		{ $$ = op_Binary (Op_LPush, $1, $3); }
	/*{ (pop (mutable) arguments from list) }*/
 |	XP_PTERM L_LPop XP_EXX						/*[ l_pop($1, $3) ]*/
		{ $$ = op_Binary (Op_LPop, $1, $3); }

 ;

/*{ Any expression: root }*/
 EXPR:	XP_EXX
 ;

/*
	Implementing "eval"
 */

EVAL_CODE:	L_EvalBeg EXPR L_EvalEnd
		{ eval_expr = Q(Expr, $2); }

%%

