
/*

	+---+---+---+---+---+---+
	|	"E_Object.cpp":
	|	Implementation of class/object builtins.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

#include "Logger.h"

X_Object::X_Object (P_Class *classref) {
if (this->classref = classref) {
	unsigned no = classref->member_no;
	Expr **ptr = data = new ("Object/data") Expr * [no];
	while (no --) *ptr ++ = 0;
	}
else	data = 0;
}	// X_Object::X_Object

VType X_Object::evalV (VDatum &val, bool full) {
val._object = this;
return T_object;
}	// X_Object::evalV

bool X_Object::identV (VType type, VDatum &val) {
P_Class *classref = this->classref;
if (type == T_object && classref == val._object->classref) {
	if (val._object == this) return true;

	unsigned no = classref->member_no;
	Expr **ptr1 = data, **ptr2 = val._object->data;

	while (no --)
		if (! identX (*ptr1 ++, *ptr2 ++)) return false;

	return true;
	}

return false;
}	// X_Object::identV

// Expect object expression
DL_EXPORT X_Object *Prefix::expect_object (Expr *expr) {
VDatum val;
VType type;

if ((type = evalV_X (expr, val)) == T_object)
	return val._object;

type_error (expr, T_object, type, val);
return 0;
}	// Prefix::expect_object

unsigned X_Object::hash () {
unsigned no = classref->member_no;
unsigned hash = 0;				// TODO: hash value of class???
Expr **ptr = data;

while (no --)
	hash ^= hashX (*ptr ++);

return hash;
}	// X_Object::hash

//
// Predicates
//

// Class expected error
struct ExpectClassError : ExecError {
	Prefix *prefix;
	Prefix *where;

	ExpectClassError (Prefix *prefix, Prefix *where)
		{ this->prefix = prefix; this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting class");
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (" (instead of ");
		log.log_prefix (prefix);
		log.put_ch (')');
		}	// _report

	};

// Expect class reference
P_Class *Prefix::expect_class (Prefix *prefix) {
P_Class *classref;

if (prefix) {
	if (classref = Cast (P_Class, prefix)) return classref;
	else Module::report (new ExpectClassError (prefix, this));
	}

return 0;
}	// Prefix::expect_class

// Virtual expected error
struct ExpectVirtualError : ExecError {
	Prefix *prefix;
	Prefix *where;

	ExpectVirtualError (Prefix *prefix, Prefix *where)
		{ this->prefix = prefix; this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("Expecting virtual");
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (" (instead of ");
		log.log_prefix (prefix);
		log.put_ch (')');
		}	// _report

	};

// Expect virtual reference
P_Virtual *Prefix::expect_virtual (Prefix *prefix) {
P_Virtual *virtref;

if (prefix) {
	if (virtref = Cast (P_Virtual, prefix)) return virtref;
	else Module::report (new ExpectVirtualError (prefix, this));
	}

return 0;
}	// Prefix::expect_virtual

//
// Determine the class to which object belongs
//

struct P_ClassOf : Prefix {
	P_ClassOf (char const *ident, O_Enum op) : Prefix (ident, op) {}
	
	D_Prefix_evalV;
	};

VType P_ClassOf::evalV (VDatum &val, Expr *args) {
X_Object *object = expect_object (args);

if (object) {
	val._pfx = object->classref;
	relink_expr (object);
	return T_prefix;
	}

return T_undef;
}	// P_ClassOf::evalV

//
// Determine current instance of the class
//

struct P_Self : Prefix {
	P_Self (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_Self::evalV (VDatum &val, Expr *args) {
Prefix *pfx = expect_prefix (args);
P_Class *classref = expect_class (pfx);

relink_prefix (pfx);

return classref && (val._object = classref->COP) ? T_object : T_undef;
}	// P_Self::evalV

//
// Determine superclass of the class
//

struct P_SuperOf : Prefix {
	P_SuperOf (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_SuperOf::evalV (VDatum &val, Expr *args) {
Prefix *pfx = expect_prefix (args);
P_Class *classref = expect_class (pfx);

relink_prefix (pfx);

if (classref && classref->super) {
	val._pfx = classref->super;
	return T_prefix;
	}

return T_undef;
}	// P_SuperOf::evalV

//
// Evaluiate expression with instance of object
//

struct P_With : P_Wrapper {
	P_With (char const *ident, O_Enum op) : P_Wrapper (ident, op) {}

	D_P_Wrapper_eval;
	};

void P_With::eval (WrapX &wrapper, Expr *args) {
X_Object *object = expect_object (get_arg (args));

if (object) {
	P_Class *classref = object->classref;
	classref->bind_members (object, 0);
	classref->with_eval (object, wrapper, args);
	relink_expr (object);
	}

else	wrapper.eval (args);
}	// P_With::eval

//
// Evaluate expression without object
//

struct P_WithOut : P_Wrapper {
	P_WithOut (char const *ident, O_Enum op) : P_Wrapper (ident, op) {}

	D_P_Wrapper_eval;
	};

void P_WithOut::eval (WrapX &wrapper, Expr *args) {
P_Class *classref = expect_class (expect_prefix (get_arg (args)));

if (classref) {
	classref->bind_members (0, 0);
	classref->with_eval (0, wrapper, args);
	}

else	wrapper.eval (args);
}	// P_WithOut::eval

//
// Get originator of virtual
//

struct P_OrgVirt : Prefix {
	P_OrgVirt (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_OrgVirt::evalV (VDatum &val, Expr *args) {
P_Virtual *virtref = expect_virtual (expect_prefix (get_arg (args)));

if (virtref) {
	val._pfx = virtref->classref;
	return T_prefix;
	}

return T_undef;
}	// P_OrgVirt::evalV

//
// Explicit devirtualization
//

struct P_DeVirt : Prefix {
	P_DeVirt (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_DeVirt::evalV (VDatum &val, Expr *args) {
P_Virtual *virtref = expect_virtual (expect_prefix (get_arg (args)));
P_Class *classref = args ?
	expect_class (expect_prefix (args)) :
	P_Class::self ();

if (virtref) {
	val._pfx = virtref->devirt (classref);
	return T_prefix;
	}

return T_undef;
}	// P_DeVirt::evalV

//
// Functor class predicate
//

struct P_FunctorPred : Prefix {
	bool (* check) (Prefix *pfx);

	P_FunctorPred (char const *ident, O_Enum op, bool (* check) (Prefix *pfx)) : Prefix (ident, op)
		{ this->check = check; }

	D_Prefix_evalV;
	};

VType P_FunctorPred::evalV (VDatum &val, Expr *args) {
Prefix *pfx = expect_prefix (args);

if (pfx) {
	val._fixed = check (pfx);
	return T_fixed;
	}

return T_undef;
}	// P_FunctorPred::evalV

bool check_class (Prefix *pfx) { return Cast (P_Class, pfx) != 0; }

bool check_virtual (Prefix *pfx) { return Cast (P_Virtual, pfx) != 0; }

// TODO:  check class interrelations...

static bool init_primaries_object (int order) {

//		[Categories]

//^C	Object
//^B	Object/class functors
//^D	Functors, operating on classes and/or class instances.

//		[Types]

//^T	Object
//^B	Object value.
//^D	Anything evaluating to instance of arbitrary class.
//^D	(Reports error, if argument is not class instance.)

//^T	Class
//^B	Class reference.
//^D	Subset of !Func: anything evaluating to class reference.

//^T	Virtual
//^B	Virtual reference.
//^D	Subset of !Func: anything evaluating to virtual reference.

//		--------

//^G	is_object expect_object is_class is_virtual

//^N	is_object [Predicate | Object]
//^P	is_object (V: Any) => Bool
//^B	Check for object value.
//^D	Predicate: !true, if argument \V evaluates to instance of any class.

	DefBuiltin (P_IsType ("is_object", Op_Null, T_object));

//^N	expect_object [Wrapper | Object]
//^P	expect_object (V: Any, @Body: Any) => Any
//^B	Expect object value.
//^D	If argument \V evaluates to any class instance, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_object", Op_Null, T_object));

//^N	is_class [Predicate | Object]
//^P	is_class (F: Func) => Bool
//^B	Check for class reference.
//^D	Predicate: !true, if functor \F is reference to any class.

	DefBuiltin (P_FunctorPred ("is_class", Op_Null, check_class));

//^N	is_virtual [Predicate | Object]
//^P	is_virtual (F: Func) => Bool
//^B	Check for virtual reference.
//^D	Predicate: !true, if functor \F is reference to virtual of any class.

	DefBuiltin (P_FunctorPred ("is_virtual", Op_Null, check_virtual));

//^G	class_of self super_of

//^N	class_of [Object]
//^P	class_of (Object: Object) => Class
//^B	Retrieve class of object.
//^D	Evaluates object \Object, and returns class, to which \Object belongs.
//^D	(Returns !undef, if \Object is not a class instance.)

	DefBuiltin (P_ClassOf ("class_of", Op_Null));

//^N	self [Object]
//^P	self (Class: Class) => Object
//^B	Retrieve current class instance.
//^D	Evaluates \Class, and returns its current instance.
//^D	(Returns !undef, if \Class is not class reference, or has no current instance.)

	DefBuiltin (P_Self ("self", Op_Null));

//^N	super_of [Object]
//^P	super_of (Class: Class) => Class
//^B	Retrieve superclass of class.
//^D	Returns class, from which \Class is derived (or !undef, if \Class has no ancestors).

	DefBuiltin (P_SuperOf ("super_of", Op_Null));

//^G	with without

//^N	with [Object | Wrapper]
//^P	with (Instance: Object, @Body: Any) => Any
//^B	Class current instance wrapper.
//^D	Evaluates and returns \Body, with object \Instance temporarily defined as current instance of own class (and its ancestors).

	DefBuiltin (P_With ("with", Op_With));

//^N	without [Object | Wrapper]
//^P	without (Class: Class, @Body: Any) => Any
//^B	Class deinstantiation wrapper.
//^D	Evaluates and returns \Body, with tempoparily undefined current instance of class \Class.

	DefBuiltin (P_WithOut ("without", Op_Null));

//^G	originator devirt

//^N	originator [Object]
//^P	originator (Virtual: Virtual) => Class
//^B	Originator of virtual method.
//^D	Returns originator class of virtual functor \Virtual.

	DefBuiltin (P_OrgVirt ("originator", Op_Null));

//^N	devirt [Object]
//^P	devirt (Virtual: Virtual, Class: Class) => Func
//^B	Explicit devirtualization.
//^D	Returns proper version of virtual method \Virtual, implemented in class \Class.

	DefBuiltin (P_DeVirt ("devirt", Op_Null));

return true;
}	// init_primaries_object

DefSubSystem ("object", init_primaries_object, 0);

