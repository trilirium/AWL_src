
/*

	+---+---+---+---+---+---+
	|	"Kernel.cpp":
	|	Interpreter kernel.
	|	Implements scoping system, user functors & classes, etc.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Defs.h"
#include "Eval.h"

#include "Logger.h"
#include "NameTab.h"

#define HEADER

#include "E_Hash.cpp"

#include "ExpImp.cpp"

#undef HEADER

#include "stdlib.h"
#include "time.h"

//
// Generic stack template
// TODO: make stacks extensible, if needed
//

struct EStackFault : FatalError {
	char *stack;

	EStackFault (char *stack) { this->stack = stack; }

	D_Error__report;
	};

void EStackFault::_report (Logger &log) {
log.put_cstr ("Stack fault of ");
log.put_cstr (stack);
}	// EStackFault::_report

template <typename E_type, char *_tag> struct E_stack {
	E_type **stack;
	unsigned count, total;

	// (failure in E_stack)
	void fault () { Module::report (new EStackFault (_tag)); }

	// (constructor)
	E_stack (unsigned total) {
		stack = new (_tag) E_type * [this->total = total];
		count = 0;
		}

	// (destructor)
	~ E_stack () {
		if (count) fault ();
		delete [] stack;
		}

	// Check, is this stack empty
	bool empty () { return count == 0; }

	// Push 'elem' into stack
	void open (E_type *elem) {
		if (count == total) fault ();
		else stack [count ++] = elem;
		}	// open

	// Pop 'elem' from stack
	void close (E_type *elem) {
		if (! (count && stack [-- count] == elem))
			fault ();
		}	// close

	// Peek last element in stack
	E_type *peek () { return count ? stack [count - 1] : 0; }

	// Scan list back to find 'predicate' condition return non-null value
	void *backscan (void * (*predicate) (E_type *elem, void const *key), void const *key) {
		unsigned index = count;
		while (index) {
			void *result = predicate (stack[-- index], key);
			if (result) return result;
			}

		return 0;
		}	// backscan

	};	// (E_stack)

//
// Variables stack
//

char tag_VarStack [] = "Stack [VarScope]";
class E_stack <VarScope, tag_VarStack> *VarStack;

static void *variable_lookup (VarScope *scope, void const *ident) {
return scope ? scope->lookup ((char *) ident) : 0;
}	// variable_lookup

static void *variable_check (VarScope *scope, void const *not_needed)
	{ return scope && ! scope->locked ? scope : 0; }

// Undeclared variable
struct NoGlobalError : ExecError {
	char const *ident;

	NoGlobalError (char const *ident) { this->ident = ident; }

	void _report (Logger &log) {
		log.put_cstr ("Undeclared variable: ");
		log.put_cstr (ident);
		}	// _report

	};

// Install variable
AnyVar *AnyVar::install (char const *ident, bool is_local) {
extern ModuleScope *module_var_scope;

if (! is_local) {
AnyVar *name = (AnyVar *) VarStack->backscan (variable_lookup, ident);
if (name) return name;

if (Module::check_explicit ()) {
	// (variable installation failure...)
	Module::report (new NoGlobalError (ident));
	return 0;
	}
}	// (! is_local)

// TODO: use unlocked backscan instead of just peek ...
return (is_local ? (VarScope *) VarStack->backscan (variable_check, 0) : module_var_scope)->install (ident);
}	// AnyVar::install

VType DefVars::evalV (VDatum &val, bool full) {
return T_undef;
}	// DefVars::evalV

//
// Prefixes stack
//

char tag_PrefixStack [] = "Stack [PrefixScope]";
class E_stack <PrefixScope, tag_PrefixStack> *PrefixStack;

static void *prefix_lookup (PrefixScope *scope, void const *ident) {
return scope ? scope->lookup ((char *) ident) : 0;
}	// prefix_lookup

static void *prefix_check (PrefixScope *scope, void const *not_needed)
	{ return scope && ! scope->locked ? scope : 0; }

// Absent prefix
struct NoPrefixError : ExecError {
	char const *ident;

	NoPrefixError (char const *ident) { this->ident = ident; }

	void _report (Logger &log) {
		log.put_cstr ("Undefined functor: ");
		log.put_cstr (ident);
		}	// _report

	};

// Lookup prefix in stack
Prefix *Prefix::lookup (char *ident) {
PrefixName *name = (PrefixName *) PrefixStack->backscan (prefix_lookup, ident);
if (name) return name->prefix;

Module::report (new NoPrefixError (ident));
return 0;
}	// Prefix::lookup

#include "Opcodes.h"

static Prefix *prefix_tab [Op_Total];

#undef LINK_NAMES

// Redefined prefix
struct RePrefixError : ExecError {
	char const *ident;
	Prefix *prefix;

	RePrefixError (char const *ident, Prefix *prefix)
		{ this->ident = ident; this->prefix = prefix; }

	void _report (Logger &log) {
		log.put_cstr ("Redefined functor: ");
		log.put_cstr (ident);
		}	// _report

	};

// Locate prefix by opcode
Prefix *Prefix::locate (O_Enum op) {
return op < Op_Total ? prefix_tab[op] : 0;
}	// Prefix::locate

DL_EXPORT Prefix::Prefix (char const *ident, unsigned op) {
refs = 0;

if (ident) {
#if 0
	name = PrefixStack->peek ()->install (ident);
#else
	name = ((PrefixScope *) PrefixStack->backscan (prefix_check, 0))->install (ident);
#endif

#ifdef LINK_NAMES
	link_name (name);
#endif

	if (name->prefix) {
		// This is name is busy -- report error!
		Module::report (new RePrefixError (ident, name->prefix));
		name = 0;
		return;
		}

	name->prefix = this;
	++ refs;
	}
else name = 0;		// (nameless prefix)

if (op && op < Op_Total)
	prefix_tab[op] = this;
}	// Prefix::Prefix

// Module init only
Prefix::Prefix (bool unused, char *ident) {
name = new ("Modular/Name") PrefixName (ident);
name->prefix = this;
name->table = 0;
name->hash = 0;
name->ordinal = 0;
name->next = 0;
}	// Prefix::Prefix

//
//	moved from: "Defs.h"
//

DL_EXPORT PrefixX::PrefixX (char const *ident, unsigned opcode) : Prefix (ident, opcode) {}

DL_EXPORT PrefixR::PrefixR (char const *ident, unsigned opcode) : Prefix (ident, opcode) {}

DL_EXPORT P_Wrapper::P_Wrapper (char const *ident, unsigned opcode) : PrefixR (ident, opcode) {}

// Look for Prefix in opcodes table
unsigned opcode_lookup (Prefix *pfx) {
for (unsigned opcode = 0; opcode != Op_Total; ++ opcode)
	if (prefix_tab[opcode] == pfx)
		return opcode;

return Op_Null;			// not found
}	// opcode_lookup

// Default enter/leave methods of prefix: have no effect

DL_EXPORT void Prefix::scope_in (bool modify) {}
DL_EXPORT void Prefix::scope_out (bool modify) {}

//
//	Parameter list handling
//

// Initialise parameters list for functor/class (during definition)
static void params_init (Param *param, unsigned offset) {
while (param) {
	param->ordinal = offset + link_name (param->var)->ordinal;
	param = param->next;
	}
}	// params_init

// Initialise parameter list with functor/class argument(s)
static void params_pass (Param *param, Expr *args, Expr **base) {
while (param) {
	Expr *arg = param->next ? get_arg (args) : args;
	if (! arg) arg = param->init;
	base [param->ordinal] =
		arg ? link_expr (param->flags ? arg : arg->evalX ()) : 0;
	param = param->next;
	}
}	// params_pass

//
//	Functors
//

char tag_FunctorStack [] = "Stack [Functor]";
class E_stack <P_Functor, tag_FunctorStack> *FunctorStack;

void P_Functor::scope_in (bool modify) {
FunctorStack->open (this);

VarStack->open (var_scope);
PrefixStack->open (prefix_scope);
}	// P_Functor::scope_in

void P_Functor::scope_out (bool modify) {
PrefixStack->close (prefix_scope);
VarStack->close (var_scope);

FunctorStack->close (this);
}	// P_Functor::scope_out

P_Functor *P_Functor::self () {
return FunctorStack->peek ();
}	// P_Functor::self

// Constructor for Functor
// (must be followed by P_Functor::define afterwards)
P_Functor::P_Functor (char *ident) : PrefixR (ident) {
var_scope = new ("VarScope/Functor") FunctorScope (this);
prefix_scope = new ("PrefixScope/Functor") PrefixScope (this);

params = 0;
body = 0;
}	// P_Functor::P_Functor

// Bind functor variable
// (expected to be in current functor scope)
static bool bindFunctorVar (FunctorVar *var) {
P_Functor::self ()->local_tab[var->ordinal] = &var->p_expr;
return true;
}	// bindFunctorVar

// Start functor definition
void P_Functor::def_open () {
scope_in (true);
}	// P_Functor::def_open

// End functor definition
void P_Functor::def_close () {
// Create locals reference table
local_tab = new ("P_Functor::local_tab")
	Expr *** [local_no = var_scope->counter];

// Bind locals
var_scope->forall (bindFunctorVar);

scope_out (true);

var_scope->locked = prefix_scope->locked = true;
}	// P_Functor::def_close

// Conclude functor binding
// ('params' && 'body')
void P_Functor::def_bind (Param *params, Expr *body) {
this->params = params;
// Setup parameter list
params_init (params, 0);

this->body = link_expr (body);

CFP = SP_NULL;			// (functor is inactive)
}	// P_Functor::def_bind

VType DefFunctor::evalV (VDatum &val, bool full) {
val._pfx = func;
return T_prefix;
}	// DefFunctor::evalV

static VType list_make
	(VType type0, VDatum &val0, VType type1, VDatum &val1) {
val0._list = new ("List/make") X_List
	(evalX_V (type0, val0), evalX_V (type1, val1));
return T_list;
}	// list_make

VType GroupDefFunctor::evalV (VDatum &val, bool full) {
val._pfx = func;

if (next) {
	VDatum _val;
	return list_make (T_prefix, val, next->evalV (_val, true), _val);
	}

return T_prefix;
}	// GroupDefFunctor::evalV

//
// Functors && expression stack
//

// TODO:
// make expression stack inflatable/deflatable!!!!
enum { XS_capacity = 1024 };

static Expr * XS_stack [XS_capacity];
static unsigned XSP = 0;		// (current stack pointer)

// Open new XStack frame (of 'count' elements)
// Returns new frame pointer
static unsigned XS_enter (unsigned count) {
if (XSP + count <= XS_capacity) {
	unsigned _XSP = XSP;
	while (count --) XS_stack[XSP ++] = 0;	// (clean up...)
	return _XSP;
	}
else {
	syslog->put_cstr ("Fatal: XStack overflow!\n");
	exit(1);
	}
}	// XS_enter

// Close opened XStack frame (of 'count' elements)
// Returns old frame pointer
static unsigned XS_leave (unsigned count) {
while (count --)
	unlink_expr (XS_stack[-- XSP]);
return XSP;
}	// XS_leave

//
// Functor evaluation
//

// Binding functor locals to stack slots (at FP)
void P_Functor::select (unsigned FP) {
unsigned no = local_no;
Expr ****table = local_tab;

if (FP != SP_NULL) {
	Expr **sp = XS_stack + FP;
	while (no --) **table ++ = sp ++;
	}
else
	while (no --) **table ++ = 0;

CFP = FP;
}	// P_Functor::select

// Enter new frame
unsigned P_Functor::enter (Expr *args) {
// (saved frame pointer)
unsigned SFP = CFP;
// (new frame pointer)
unsigned NFP = XS_enter (local_no);

params_pass (params, args, XS_stack + NFP);

select (NFP);
return SFP;
}	// P_Functor::enter

// Leave to old frame
void P_Functor::leave (unsigned SFP) {
select (SFP);

XS_leave (local_no);
}	// P_Functor::leave

VType P_Functor::evalV (VDatum &val, Expr *args) {
unsigned SFP = enter (args);

VType type = evalV_X (body, val);

if (type == T_xcpt && val._except->type == 1)
	type = val._except->unexcept_V (val);

relock_value_by (1, type, val);
if (type == T_string)
	val._string.refcount_by (1);

leave (SFP);

if (type == T_string)
	val._string.refcount_by (-1);
relock_value_by (-1, type, val);
return type;
}	// P_Functor::evalV

Expr *P_Functor::evalX (Expr *args) {
unsigned SFP = enter (args);

Except *except;
Expr *result = evalX_X (body);

if (except = Cast (Except, result)) {
	if (except->type == 1)
		result = except->unexcept_X ();
	}

if (result) result->relock (1);

leave (SFP);

if (result) result->relock (-1);
return result;
}	// P_Functor::evalX

Expr *&P_Functor::evalR (Expr *args) {
unsigned SFP = enter (args);

Expr *&result = evalR_X (body);

leave (SFP);
return result;
}	// P_Functor::evalR

//
//	Classes
//

char tag_ClassStack [] = "Stack [Class]";
class E_stack <P_Class, tag_ClassStack> *ClassStack;

void P_Class::scope_in (bool modify) {
if (super)
	super->scope_in (false);

ClassStack->open (this);

VarStack->open (var_scope);
PrefixStack->open (prefix_scope);
}	// P_Class::scope_in

void P_Class::scope_out (bool modify) {
PrefixStack->close (prefix_scope);
VarStack->close (var_scope);

ClassStack->close (this);

if (super)
	super->scope_out (false);
}	// P_Class::scope_out

P_Class *P_Class::self () {
return ClassStack->peek ();
}	// P_Class::self

// Constructor for Class
// (must be accompanied by P_Class::define afterwards)
P_Class::P_Class (char *ident, Prefix *pfx_super): Prefix (ident) {
super = expect_class (pfx_super);

var_scope = new ("VarScope/Class") ClassScope (this);
prefix_scope = new ("PrefixScope/Class") PrefixScope (this);
}	// P_Class::P_Class

// Temporary: used by P_Class::define && bindClassVar
static unsigned super_member_no;

static bool bindClassVar (ClassVar *class_var) {
P_Class::self ()->member_tab [super_member_no + class_var->ordinal] =
	&class_var->p_expr;
return true;
}	// bindClassVar

static void inherit_members (unsigned no,
	Expr ****self_table, Expr ****super_table) {
while (no --) *self_table ++ = *super_table ++;
}	// inherit_members

// Open class definition
void P_Class::def_open () {
scope_in (true);

link_prefix (super);
}	// P_Class::def_open

// Close class definition
void P_Class::def_close () {
super_member_no = super ? super->member_no : 0;

// Create members reference table; then bind members
member_tab = new ("P_Class::member_tab")
	Expr *** [member_no = super_member_no + var_scope->counter];

// Inherit members' bindings of superclass
if (super_member_no)
	inherit_members (super_member_no, member_tab, super->member_tab);

// Bind members
var_scope->forall (bindClassVar);

scope_out (true);

var_scope->locked = prefix_scope->locked = true;

// Just for completeness...
super_member_no = 0;
}	// P_Class::def_close

// Conclude class binding
// (params, constructor, destructor)
void P_Class::def_bind (Param *params, Expr *ctor, Expr *dtor) {
this->params = params;

// (always called after constructor...)
COP = 0;	// (no active class instance)

// Setup parameter list
params_init (params, super ? super->member_no : 0);

// Link constructor & destructor
this->ctor = link_expr (ctor);
this->dtor = link_expr (dtor);
}	// P_Class::def_close

VType DefClass::evalV (VDatum &val, bool full) {
val._pfx = classref;
return T_prefix;
}	// DefClass::evalV

// TODO: GroupDefClass ...

// Bind this class members to 'object'; from 'root' to self
void P_Class::bind_members (X_Object *object, P_Class *root) {
unsigned offset = root ? root->member_no : 0;
if (offset != member_no) {
	unsigned count = member_no - offset;
	Expr ****members = member_tab + offset;

	if (object) {
		Expr **entries = object->data + offset;
		while (count --) **members ++ = entries ++;
		}
	else
		while (count --) **members ++ = 0;
	}
}	// P_Class::bind_members

// Unbinding all members back
static void unbind_members (P_Class *classref) {
while (classref) {
	P_Class *super = classref->super;
	classref->bind_members (classref->COP, super);
	classref = super;
	}
}	// unbind_members

//
//	Initialise  instance of the class
//	Note: object unbinding is required afterwards
//

void P_Class::init (X_Object *object, Expr *args) {
// (if superclass, initialise it by first argument)
if (super)
	super->init (object, get_arg (args));

// (assigning class arguments to parameters)
// (Note: must be done BEFORE object binding)
params_pass (params, args, object->data);

X_Object *prev = COP;
COP = object;

bind_members (object, super);

// (calling constructor)
evalZ_X (ctor);

COP = prev;
}	// P_Class::init

void P_Class::shut (X_Object *object) {
// Note: all internals are bound to 'object': now un-binding them

X_Object *prev = COP;
COP = object;

// (calling destructor)
evalZ_X (dtor);

if (super) super->shut (object);

bind_members (COP = prev, super);
}	// P_Class::shut

// Recursive: Evaluate 'wrapper' with 'object' as current instance of this class
// Note: 'object' may be NULL
void P_Class::with_eval (X_Object *object, WrapX &wrapper, Expr *args) {
X_Object *prev = COP;
COP = object;

if (super)		// (recurse to superclass)
	super->with_eval (object, wrapper, args);

else			// (end of recursion -- evaluate wrapper)
	wrapper.eval (args);

// Members rollback -- current class only
bind_members (COP = prev, super);
}	// X_Object::with_eval

//
//	Evaluate class
//

VType P_Class::evalV (VDatum &val, Expr *args) {
X_Object *object = new ("Object") X_Object (this);

// initialisation
init (object, args);

// unbinding from object
unbind_members (this);

val._object = object;
return T_object;
}	// P_Class::evalV

//
//	Virtuals
//

// Locate virtual of class by ordinal
P_Virtual *P_Class::locate (unsigned ordinal) {
if (ordinal < virtual_no) {
	unsigned virtuals_super = super ? super->virtual_no : 0;

	if (ordinal < virtuals_super)
		return super->locate (ordinal);

	ordinal -= virtuals_super;

	P_Virtual *virtref;
	for (virtref = virtuals; virtref && ordinal --; virtref = virtref->next);
	return virtref;
	}

return (P_Virtual *) 0;
}	// P_Virtual::locate

// Check, whether the instance of virtual in self differs from super
bool P_Class::virtual_redefined (unsigned ordinal) {
if (ordinal < virtual_no) {
	unsigned virtuals_super = super ? super->virtual_no : 0;

	return ordinal >= virtuals_super ||
		super->virtual_tab[ordinal] != virtual_tab[ordinal];
	}

return false;			// (must not happen)
}	// P_Class::virtual_redefined

P_Virtual *&P_Class::R_bind_virtual (P_Virtual *last) {
// Attention: reversing virtuals list order...

if (last) {
	R_bind_virtual (last->next) = last;
	link_prefix (last);
	last->classref = this;
	last->ordinal = virtual_no ++;
	return last->next;
	}
else
	return virtuals;
}	// P_Class::R_bind_virtual

void inherit_virtuals (unsigned self_no, P_Functor **self_tab,
	unsigned super_no, P_Functor **super_tab) {
self_no -= super_no;
while (super_no --) link_prefix (*self_tab ++ = *super_tab ++);
while (self_no --) *self_tab ++ = 0;
}	// inherit_virtuals

// Bind list of virtual definitions to class
void P_Class::def_virtuals (P_Virtual *virtuals) {

unsigned virtuals_super = super ? super->virtual_no : 0;
virtual_no = virtuals_super;

R_bind_virtual (virtuals) = 0;

if (virtual_no) {
	// allocate virtuals table
	virtual_tab = new ("VFT") P_Functor * [virtual_no];

	inherit_virtuals (virtual_no, virtual_tab,
		virtuals_super, super ? super->virtual_tab : 0);
	}

else virtual_tab = 0;
}	// P_Class::def_virtuals

// Instantiate virtual for current (sub)class
P_Functor *P_Virtual::instantiate (Prefix *prefix) {
P_Class *classref = P_Class::self ();
P_Virtual *virtref = classref->expect_virtual (prefix);

if (virtref && classref) {
	// TODO: check, is really 'virtref' in tables of class or ancestors???

	P_Functor *&entry = classref->virtual_tab [virtref->ordinal];
	unlink_prefix (entry);
	entry = new ("Virtual/Def") P_Functor (0);
	link_prefix (entry);

	return entry;
	}

return (P_Functor *) 0;
}	// P_Virtual::instantiate

// TODO: other ways of evaluation

// Detect current subclass
inline P_Class *P_Class::current_subclass () { return COP ? COP->classref : 0; }

// Devirtualization of virtual self for class 'classref'
P_Functor *P_Virtual::devirt (P_Class *classref) {
return classref ? classref->virtual_tab [ordinal] : 0;
}	// P_Virtual::devirt

P_Functor *static_devirt (Prefix *pfx_virtual, Prefix *pfx_class) {
P_Virtual *virtref = pfx_class->expect_virtual (pfx_virtual);
return virtref ? virtref->devirt (pfx_class->expect_class (pfx_class)) : 0;
}	// static_devirt

VType P_Virtual::evalV (VDatum &val, Expr *args) {
P_Functor *func = devirt (classref->current_subclass ());
return func ? func->evalV (val, args) : T_undef;
}	// P_Virtual::evalV

//
//	Modules
//

Module *Module::current = 0;

// (called from lexer...)
void sync_lines (unsigned lines) {
Module::current->lines += lines;
}	// sync_lines

// Execute top-level statement
void Module::exec_stmt (Expr *expr) {
if (expr)
if (! check_cancel ()) {
	cpu_capture ();

	VDatum val;
	link_expr (expr);
	VType type = expr->evalV (val, true);

	if (type == T_xcpt) {
		val._except->trap (0);
		type = T_undef;
		}

	syslog->log_stmt (*current, expr, type, val);

	relink_value (type, val);
	unlink_expr (expr);

	cpu_release ();

	syslog->log_prompt (*current);
	}
else relink_expr (expr);
}	// Module::exec_stmt

// TODO: module/include stack...

// Current module variables scope
ModuleScope *module_var_scope = 0;

void Module::open () {}

void Module::close () {}

RootModule::RootModule (char *ident, unsigned flags, unsigned x_flags, Expr *args) :
	Module (ident, flags, x_flags) {
prefix_scope = new ("ModulePrefixScope") PrefixScope (this);
var_scope = new ("ModuleVarScope") ModuleScope (this);

Module::arguments = link_expr (args);
}	// RootModule::RootModule

void RootModule::open () {
PrefixStack->open (prefix_scope);
VarStack->open (var_scope);

module_var_scope = var_scope;
}	// RootModule::open

void RootModule::close () {
module_var_scope = 0;

VarStack->close (var_scope);
PrefixStack->close (prefix_scope);

unlink_expr (Module::arguments);
Module::arguments = 0;
}	// RootModule::close

// Open source module (nesting)
void Module::prolog () {
parent = current;

syslog->log_prolog (*this, true);

syslog->log_prompt (*this);

open ();

current = this;
}	// Module::prolog

// Close source module
Module *Module::epilog () {
Module *module = current;

current = module->parent;

module->close ();
module->release ();

syslog->log_epilog (*module, true);

return module;
}	// Module::epilog

//
//	Errors (from "Defs.h")
//

DL_EXPORT ExecError::ExecError () { type = EC_error; }

#if 0

WarningError::WarningError () { type = EC_warn; }

#endif

// Report module error
DL_EXPORT void Module::report (Error *error) {
error->report (*syslog, current);
delete error;
}	// Module::report

// Control explicit declarations mode

bool Module::check_explicit () { return current->check_flags (XF_explicit); }
void Module::set_explicit (bool on_off) { current->set_flags (XF_explicit, on_off); }

// Control module cancel mode

bool Module::check_cancel () { return current->check_flags (XF_cancel); }
void Module::set_cancel (bool on_off) { current->set_flags (XF_cancel, on_off); }

//
//	Name spaces
//

// Constructor for namespace
P_Space::P_Space (char const *ident): Prefix (ident) {
var_scope = new ("VarScope/Space") ModuleScope (Module::current);
prefix_scope = new ("PrefixScope/Space") PrefixScope (this);
}	// P_Space::P_Space

void P_Space::scope_in (bool modify) {
VarStack->open (var_scope);
PrefixStack->open (prefix_scope);
}	// P_Space::scope_in

void P_Space::scope_out (bool modify) {
PrefixStack->close (prefix_scope);
VarStack->close (var_scope);

var_scope->locked = prefix_scope->locked = true;
}	// P_Space::scope_out

//
//	Release objects
//

bool log_release = false;

// Release variables scope
static void var_scope_release (NameTab *var_scope) {
if (var_scope) delete var_scope;
}	// var_scope_release

DL_EXPORT void Prefix::release () {
if (log_release) {
	syslog->put_cstr ("Release: ");
	log (*syslog);
	syslog->put_ch ('\n');
	}

delete this;
}	// Prefix::release

DL_EXPORT void Prefix::cleanup () {
#ifdef LINK_NAMES
unlink_name (name);
#endif
}	// Prefix::cleanup

// Internal: cleanup prefix
static bool prefix_cleanup (PrefixName *prefix_name) {
if (prefix_name->prefix)
	prefix_name->prefix->cleanup ();
return true;
}	// prefix_cleanup

// TODO:
// Clean up virtuals as well as metods!!!

// Cleanup prefix scope
static void prefix_scope_cleanup (PrefixScope *prefix_scope) {
if (prefix_scope) prefix_scope->forall (prefix_cleanup);
}	// prefix_scope_cleanup

// Release prefixes scope
static void prefix_scope_release (PrefixScope *prefix_scope) {
if (prefix_scope) {
	prefix_scope_cleanup (prefix_scope);
	delete prefix_scope;
	}
}	// prefix_scope_release

// Release list of parameters
static void params_release (Param *param) {
while (param) {
	Param *next = param->next;

	unlink_name (param->var);
	unlink_expr (param->init);
	delete param;

	param = next;
	}
}	// params_release

struct FunctorCFPFault : FatalError {
	P_Functor *functor;

	FunctorCFPFault (P_Functor *functor)
		{ this->functor = functor; }

	D_Error__report;
	};

void FunctorCFPFault::_report (Logger &log) {
log.put_cstr ("Bad CFP closing functor ");
log.log_functor (functor);
}	// FunctorCFPFault::_report

void P_Functor::cleanup () {
#ifdef LINK_NAMES
unlink_name (name);
#endif

unlink_expr (body);
body = 0;

prefix_scope_cleanup (prefix_scope);
}	// P_Functor::cleanup

void P_Functor::release () {
// TODO: unnededed...
unlink_expr (body);

params_release (params);

var_scope_release (var_scope);
prefix_scope_release (prefix_scope);

delete [] local_tab;

if (CFP != SP_NULL)
	Module::report (new FunctorCFPFault (this));

Prefix::release ();
}	// P_Functor::release

void X_Object::release () {
P_Class *classref = this->classref;

classref->bind_members (this, 0);
classref->shut (this);

unsigned no = classref->member_no;
Expr **ptr = data;

while (no --) unlink_expr (*ptr ++);
delete [] data;

Expr::release ();
}	// X_Object::release

// Release list of virtuals
static void virtuals_release (P_Virtual *virtref) {
while (virtref) {
	P_Virtual *next = virtref->next;
	unlink_prefix (virtref);
	virtref = next;
	}
}	// virtuals_release

// Release virtual table
static void virtable_release (unsigned virtual_no, P_Functor **virtual_tab) {
while (virtual_no)
	unlink_prefix (virtual_tab [-- virtual_no]);

delete [] virtual_tab;
}	// virtable_release

struct ClassCOPFault : FatalError {
	P_Class *classref;

	ClassCOPFault (P_Class *classref)
		{ this->classref = classref; }

	D_Error__report;
	};

void ClassCOPFault::_report (Logger &log) {
log.put_cstr ("Bad COP closing class ");
log.log_class (classref);
}	// ClassCOPFault::_report

void P_Class::cleanup () {
unlink_expr (ctor);
unlink_expr (dtor);
ctor = dtor = 0;

// TODO: make virtuals cleanup here...

prefix_scope_cleanup (prefix_scope);

#ifdef LINK_NAMES
unlink_name (name);
#endif
}	// Class::cleanup

void P_Class::release () {
unlink_prefix (super);

params_release (params);

var_scope_release (var_scope);
prefix_scope_release (prefix_scope);

delete [] member_tab;

if (COP)
	Module::report (new ClassCOPFault (this));

// TODO: this is unneeded?
unlink_expr (ctor);
unlink_expr (dtor);

virtuals_release (virtuals);

// move this to P_Class::cleanup ???
virtable_release (virtual_no, virtual_tab);

Prefix::release ();
}	// P_Class::release

void P_Virtual::release () {
Prefix::release ();
}	// P_Virtual::release

void P_Space::release () {
var_scope_release (var_scope);
prefix_scope_release (prefix_scope);

Prefix::release ();
}	// P_Space::release

void DefFunctor::release () {
// TODO: release functor, if definition failed...

Expr::release ();
}	// DefFunctor::release

void GroupDefFunctor::release () {
GroupDefFunctor *node = this, *next;
do {
	next = node->next;
	delete node;
	}
while (node = next);
}	// GroupDefFunctor::release

void DefClass::release () {
Expr::release ();
}	// DefClass::release

// TODO: GroupDefClass ...

void Module::release () {
name->release ();
}	// Module::release

void RootModule::release () {
var_scope_release (var_scope);
prefix_scope_release (prefix_scope);

Module::release ();
}	// RootModule::release

//
//	Global init/shutdown
//

// Initialise appropriate namespace
DL_EXPORT int with_namespace (char const *name, void (*do_init) ()) {
P_Space *name_space = DefBuiltin (P_Space (name));

name_space->scope_in (true);
do_init ();
name_space->scope_out (true);

return 1;			// TODO: fail if namespace is busy
}	// with_namespace

// ("Eval.h")
void init_eval ();
void shut_eval ();

DL_EXPORT char primary_pfx[] = "primary_prefix";

static PrefixScope *system_prefix_scope;

// All subsystems
SubSystem *SubSystem::list_start = 0;

// Constructor for subsystem
SubSystem::SubSystem (char const *name,
	bool (*do_init) (int order), bool (*do_shut) (int order)) {
this->name = name;
this->do_init = do_init, this->do_shut = do_shut;

next = list_start;
list_start = this;
}	// SubSystem::SubSystem

// Initialise all subsystems
// (reverse order)
void SubSystem::init_subsystems () {
int order = 0;
SubSystem *final = 0;

while (final != list_start) {
	SubSystem *subsys;
	for (subsys = list_start; subsys->next != final; subsys = subsys->next);
	if (* subsys->do_init) (* subsys->do_init) (order ++);
	final = subsys;
	}
}	// SubSystem::init_subsystems

// Shutdown all subsystems
// (direct order)
void SubSystem::shut_subsystems () {
int order = 0;

for (SubSystem *subsys = list_start; subsys; subsys = subsys->next) {
	if (* subsys->do_shut) (* subsys->do_shut) (order ++);
	}
}	// SubSystem::shut_subsystems

// System initialisation
unsigned system_init () {
VarStack = new ("Stack/VarScope") class E_stack <VarScope, tag_VarStack> (16);
PrefixStack = new ("Stack/PrefixScope") class E_stack <PrefixScope, tag_PrefixStack> (16);

FunctorStack = new ("Stack/Functor") class E_stack <P_Functor, tag_FunctorStack> (8);
ClassStack = new ("Stack/Class") class E_stack <P_Class, tag_ClassStack> (8);

/* Global scope open */
system_prefix_scope = new ("SystemPrefixScope") PrefixScope (0);
PrefixStack->open (system_prefix_scope);

init_eval ();

SubSystem::init_subsystems ();

return system_prefix_scope->counter;
}	// system_init

// System termination
void system_term () {
SubSystem::shut_subsystems ();

shut_eval ();

/* Global scope close */
PrefixStack->close (system_prefix_scope);

prefix_scope_cleanup (system_prefix_scope);
delete system_prefix_scope;

delete ClassStack;
delete FunctorStack;

delete PrefixStack;
delete VarStack;
}	// system_term

// System prefix by ordinal
Prefix *find_by_ordinal (unsigned ordinal) {
return Cast (PrefixName, system_prefix_scope->find_ordinal (ordinal))->prefix;
}	// find_by_ordinal

//
// Show functor/class scope
//

struct P_ShowScope : Prefix {
	bool flag;			// (flag ? prefixes : variables) scope
	bool order;			// (if true, force ordering)

	P_ShowScope (char const *ident, bool flag, bool order) : Prefix (ident)
		{ this->flag = flag; this->order = order; }

	D_Prefix_evalV;
	};

VType P_ShowScope::evalV (VDatum &val, Expr *args) {
bool dir;
if (order) dir = expect_bool (get_arg (args), false);

Prefix *prefix = args ? expect_prefix (args) : 0;

NameTab *scope;

if (prefix) prefix->scope_in (false);
flag ? (scope = PrefixStack->peek ()) : (scope = VarStack->peek ());
if (prefix) prefix->scope_out (false);

syslog->log_nametab (scope, order ? (dir ? 1 : -1) : 0);

relink_prefix (prefix);

val._fixed = scope->counter;
return T_fixed;
}	// P_ShowScope::evalV

VType Module::change_flags (unsigned new_flags, Expr *expr, VDatum &val) {
unsigned old_flags = flags;

if (expr) {			// temporary change
	VType type = evalV_X (expr, val);
	flags = new_flags | OF_tr_stmt;
	syslog->log_stmt (*this, expr, type, val);
	flags = old_flags;
	return type;
	}

else {				// permanent change
	flags = new_flags;
	if ((new_flags ^ old_flags) & OF_tr_mod)
		(new_flags & OF_tr_mod) ?
			syslog->log_prolog (*this, false):
			syslog->log_epilog (*this, false);

	val._fixed = old_flags;
	return T_fixed;
	}
}	// Module::change_flags

//
//	Control trace mode
//

struct P_Trace : Prefix {
	P_Trace (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
	unsigned new_flags =
		expect_fixed (get_arg (args), OF_tr_expr|OF_tr_vals|OF_tr_mod);
	return Module::current->change_flags (new_flags, args, val);
	}	// evalV

	};

//
//	Cancel current module
//

struct P_Cancel : Prefix {
	P_Cancel (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
	unsigned mode = expect_fixed (args, 1);
	Module::current->set_cancel (mode);
	return T_undef;
	}	// evalV

	};

//
//	Get module info
//

struct P_Module : Prefix {
	P_Module (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
	if (Module::current) {
		val._pfx = Module::current;
		return T_prefix;
		}

	return T_undef;
	}	// evalV

	};

//
//	Query arguments list
//

struct P_Arguments : PrefixX {
	P_Arguments (char const *ident) : PrefixX (ident) {}

	Expr *evalX (Expr *args) { return Module::current->arguments; }
	};

//
//	Query environment hash
//

struct P_Environ : PrefixX {
	P_Environ (char const *ident) : PrefixX (ident) {}

	Expr *evalX (Expr *args) { return environment; }
	};

//
//	Query interpreter version
//

struct P_Version : PrefixX {
	unsigned &version;
	char const *date;

	P_Version (char const *ident, unsigned &version, char const *date) :
		PrefixX (ident), version(version), date(date) {}

	Expr *evalX (Expr *args) {
		return
			new ("List/version") X_List (
				new ("Version.major") X_Fixed (Version_Maj(version)),
				new ("List/version") X_List (
					new ("Version.minor") X_Fixed (Version_Min(version)),
					new ("Version.patch") X_Fixed (Version_Pat(version))
				)
			);
		}	// evalX

	};

// File include failure
struct IncludeError : ExecError {
	char *filename;

	IncludeError (char *filename)
		{ this->filename = filename; }

	void _report (Logger &log) {
		log.put_cstr ("Failed to include file: ");
		log.put_cstr (filename);
		}	// _report

	};

struct P_Include : Prefix {

	P_Include (char const *ident) : Prefix (ident) {}

	D_Prefix_evalV;
	};

VType P_Include::evalV (VDatum &val, Expr *args) {
S_string filename;
if (expect_string (get_arg (args), filename)) {
	char *c_name = filename.to_cstring ();
	int result;
	if (c_name) {
		if (! include_file (c_name))
			Module::report (new IncludeError (c_name));
		result = 0;
		}
	else
		result = 0;

	delete c_name;

	val._fixed = result;
	return T_fixed;
	}

return T_undef;
}	// P_Include::evalV

//
//	Export/import module data
//

struct P_Export : Prefix {
	P_Export (char const *ident) : Prefix (ident) {}

	D_Prefix_evalV;
	};

VType P_Export::evalV (VDatum &val, Expr *args) {
S_string filename;
if (expect_string (get_arg (args), filename)) {
	char *c_name = filename.to_cstring ();
	Exporter exporter (c_name);

	if (exporter.valid ())
		val._fixed = Module::export_all (exporter, Module::current);
	else {
		syslog->put_cstr ("Module export failure!\n");
		val._fixed = 0;
		}

	delete c_name;
	return T_fixed;
	}

return T_undef;
}	// P_Export::evalV

struct P_Import : Prefix {
	P_Import (char const *ident) : Prefix (ident) {}

	D_Prefix_evalV;
	};

VType P_Import::evalV (VDatum &val, Expr *args) {
S_string filename;
if (expect_string (get_arg (args), filename)) {
	char *c_name = filename.to_cstring ();
	Importer importer (c_name);

	if (importer.valid ())
		val._fixed = Module::import_all (importer, Module::current);
	else {
		syslog->put_cstr ("Module import failure!\n");
		val._fixed = 0;
		}

	delete c_name;
	return T_fixed;
	}

return T_undef;
}	// P_Import::evalV

int exec_code (char *code_start, unsigned code_len, bool is_expr);

struct P_EvalCode : PrefixX {
	P_EvalCode (char const *ident) : PrefixX (ident) {}

	D_Prefix_evalX;
	};

Expr *P_EvalCode::evalX (Expr *args) {
S_string source;
if (expect_string (get_arg (args), source)) {
	unsigned char type;
	unsigned len;
	str_ptr buf_ptr = source.fetch (len, type);

	if (!type)
		exec_code ((char *)buf_ptr, len, true);

	source.relink ();
	
	extern Expr *eval_expr;
	return eval_expr;
	}

return UNDEF;
}	// P_EvalCode::evalX

struct P_EvalSource : PrefixX {
	P_EvalSource (char const *ident) : PrefixX (ident) {}

	D_Prefix_evalX;
	};

Expr *P_EvalSource::evalX (Expr *args) {
S_string source;
if (expect_string (get_arg (args), source)) {
	unsigned char type;
	unsigned len;
	str_ptr buf_ptr = source.fetch (len, type);

	if (!type)
		exec_code ((char *)buf_ptr, len, false);

	source.relink ();
	}

return UNDEF;
}	// P_EvalSource::evalX

//
//	Running clock...
//

struct P_Clock : Prefix {
	P_Clock (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
		val._fixed = clock ();
		return T_fixed;
		}	// evalV
	};

// Warning: i86 - specific !!!
extern unsigned long long rdtsc() {
unsigned long long ticks;
__asm__ volatile (".byte 0x0f, 0x31" : "=A" (ticks));
return ticks;
}	// rdtsc

struct P_Ticks : Prefix {
	P_Ticks (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
		unsigned long long count = rdtsc();
		unsigned shift = expect_fixed (args, 1);
		val._fixed = count >> shift;
		return T_fixed;
		}	// evalV

	};

//
//	Dynamic module loading interface
//

struct P_DynaLoad : Prefix {

	P_DynaLoad (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
	S_string file_name, entry_symbol;
	if (expect_string (get_arg (args), file_name) &&
		expect_string (get_arg (args), entry_symbol)) {
	DynamicModInfo dmi;

	char const *c_file_name = file_name.to_cstring ();
	char const *c_entry_symbol = entry_symbol.to_cstring ();

	int retval = dynaload_module (c_file_name, c_entry_symbol, &dmi);

	if (retval == 0)
		dmi.log_info (*syslog);

	if (dmi.dmi_entry_point) (dmi.dmi_entry_point) ();

	delete c_file_name;
	delete c_entry_symbol;

	val._fixed = retval;
	return T_fixed;
	}	// arguments valid
	
	return T_undef;
	}	// evalV
	};

//
//	Functor post-define
//

struct P_FuncPostDef : Prefix {
	P_FuncPostDef (char const *ident) : Prefix (ident) {}

	VType evalV (VDatum &val, Expr *args) {
		// P_Functor *functor = P_Functor::self ();
		Prefix *functor = expect_prefix (get_arg (args));
		P_Functor *func = Cast (P_Functor, functor);
		if (func) {
			// something to redefine...
			unlink_expr (func->body);
			link_expr (func->body = args);
			}
		};
	};

//
//	... Startup ...
//

static bool init_primaries_kernel (int order) {

//		[Categories]

//^C	Debug
//^B	Debugging functors
//^D	Functors for debugging/tracing.

//^C	System
//^B	System operations
//^D	Miscellaneous system functors.

//		--------

//^N	x_trace [Debug]
//^P	x_trace (Mode: Int [, Expr: Any]) => Any
//^B	Debug local/global trace mode.
//^D	If expression \Expr is defined: evaluates it, and outputs expression and/or result in trace mode \Mode.
//^D	If expr is omitted: sets module trace mode to \Mode.

	DefBuiltin (P_Trace ("x_trace"));

//^N	x_vars [Debug]
//^P	x_vars ([Scope: Func]) => Int
//^B	Show scope variables.
//^D	Lists all variables, defined in \Scope (belonging to user functor or class; defaults to module global scope).
//^D	Returns total # of variables in scope.

//^N	x_funcs [Debug]
//^P	x_funcs ([Scope: Func]) => Int
//^B	Show scope functors/classes.
//^D	Lists all functors/classes, defined in \Scope (belonging to user functor or class; defaults to module global scope).
//^D	Returns total # of functors/classes in scope.

	DefBuiltin (P_ShowScope ("x_vars",	false, false));
	DefBuiltin (P_ShowScope ("x_funcs",	true, false));

	DefBuiltin (P_ShowScope ("x_vars_ord", false, true));
	DefBuiltin (P_ShowScope ("x_funcs_ord",	true, true));

// TODO...

	DefBuiltin (P_Cancel ("x_cancel"));

	DefBuiltin (P_Module ("x_module"));

//^N	_arguments [System]
//^P	_arguments () => List
//^B	Module arguments.
//^D	Returns current module arguments (as list of string values, or !undef, when empty).

	DefBuiltin (P_Arguments ("_arguments"));

//^N	_environ [System]
//^P	_environ () => Hash
//^B	System environment.
//^D	Returns system environment (as hash of key/value pairs).

	DefBuiltin (P_Environ ("_environ"));

//^N	_version [System]
//^P	_version () => (Int, Int, Int)
//^B	Interpreter version.
//^D	Returns interpreter version (as \Major, \Minor, \Patchlevel).

	DefBuiltin (P_Version ("_version", main_version, main_date));

//^N	include [System]
//^P	include (Filename: String) => Int
//^B	Include module.
//^D	Include source file \Filename
//^\	(evaluating all top-level expressions and module definitions).

	DefBuiltin (P_Include ("include"));

//
//	Timing...
//

	DefBuiltin (P_Clock ("__clock"));
	DefBuiltin (P_Ticks ("__ticks"));

//
//	TODO: describe all these
//

	DefBuiltin (P_Export ("_export"));
	DefBuiltin (P_Import ("_import"));

	DefBuiltin (P_EvalCode ("_parse_code"));
	DefBuiltin (P_EvalSource ("_parse_source"));

	DefBuiltin (P_DynaLoad ("_dynamic_load"));

return true;
}	// init_primaries_kernel

DefSubSystem ("kernel", init_primaries_kernel, 0);

