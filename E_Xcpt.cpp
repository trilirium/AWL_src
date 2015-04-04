
/*

	+---+---+---+---+---+---+
	|	"E_Xcpt.cpp":
	|	Exceptions processing:
	|	throw, try, catch primitives.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"
#include "Logger.h"

//
//	Exception errors
//

// No exception to handle error
struct ExceptionAbsentError : ExecError {

	ExceptionAbsentError () {}

	void _report (Logger &log) {
		log.put_cstr ("No exception present");
		}	// _report

	};

// Uncaught exception
struct ExceptionUncaughtError : ExecError {
	Expr *value;
	unsigned type;

	ExceptionUncaughtError (Expr *value, unsigned type)
		{ this->value = value; this->type = type; }

	void _report (Logger &log) {
		log.put_cstr (type ? "Return outside functor: " : "Uncaught exception: ");
		log.log_expr (value);
		}	// _report

	};

//
//	Exception state
//

VType Except::evalV (VDatum &val, bool full) {
val._except = this;
return T_xcpt;
}	// Except::evalV

void Except::release () {
unlink_expr (value);
delete this;
}	// Except::release

void Except::log (Logger &log) {
log.put_cstr ("Exception:");
log.log_fixed (type);
log.put_ch (':');
log.log_expr (value);
}	// Except::log

VType Except::unexcept_V (VDatum &val) {
VType type = value->evalV (val, false);
relock_value_by (1, type, val);
if (type == T_string)
	val._string.refcount_by (1);
unlink_expr (this);
if (type == T_string)
	val._string.refcount_by (-1);
relock_value_by (-1, type, val);

return type;
}	// Except::unexcept_V

Expr *Except::unexcept_X () {
Expr *value = this->value;
this->value = 0;

release ();

return value;
}	// Except::unexcept_X

// Trap exception: report && release
void Except::trap (Prefix *pfx) {
Module::report (new ExceptionUncaughtError (value, type));

release ();
}	// Except::trap

//
//	Throw an exception
//

struct P_XcptThrow : Prefix {
	unsigned mode;

	P_XcptThrow (char const *ident, unsigned mode) : Prefix (ident, Op_Null)
		{ this->mode = mode; }

	D_Prefix_evalV;
	};

// If double exception???

VType P_XcptThrow::evalV (VDatum &val, Expr *args) {
Expr *exception = evalX_X (args);

link_expr (val._except = new ("Exception") Except (mode, exception));
return T_xcpt;
}	// P_XcptThrow::evalV

//
//	Exception handling frame
//

struct P_XcptTry : Prefix {
	bool mode;			// (0: try; 1: try_catch)

	P_XcptTry (char const *ident, bool mode) : Prefix (ident, Op_Null)
		{ this->mode = mode; }

 	D_Prefix_evalV;
	};

VType P_XcptTry::evalV (VDatum &val, Expr *args) {
VType type = evalV_X (mode ? get_arg(args) : args, val);

if (type == T_xcpt) {
	type = val._except->unexcept_V (val);

	if (mode) {
		mutateX_V (args, type, val);
		return T_undef;
		}
	else
		return type;
	}
else
	return type;
}	// P_XcptTry::evalV

//
//	Signals handling
//

#include <signal.h>

static void on_signal (int signum) {
// TEMP: check for running exception...

#if 0
Prefix::exception = link_expr (new ("Signal") SysX_Signal (signum));
#endif

// (restore handler)
signal (signum, on_signal);
}	// on_signal

void init_sighandlers () {
return;			// (TTT)

signal (SIGINT, on_signal);
signal (SIGTERM, on_signal);
signal (SIGBREAK, on_signal);

}	// init_sighandlers

//
//	Operation table
//

static bool init_primaries_except (int order) {

//		[Categories]

//^C	Exception
//^B	Exceptions processing
//^D	Functors, responsible for raising and handling exceptions.

//		[Types]

//^T	Exception
//^B	Exception (pseudo-type)
//^D	Exception thrown: function never provide return value.

//		--------

//^G	try try_catch throw

//^N	throw [Exception]
//^P	throw (Exception: Any) => Exception
//^B	Throw exception.
//^D	Throw any definite \Exception, to be caught and processed somewhere in caller.

	DefBuiltin (P_XcptThrow ("throw", 0));
	DefBuiltin (P_XcptThrow ("raise", 0));

//^N	return [Exception]
//^P	return (Value: Any) => Exception
//^B	Return from functor.
//^D	Return from current functor (with return \Value) to its caller.

	DefBuiltin (P_XcptThrow ("return", 1));

//^N	try [Exception | Wrapper]
//^P	try (Body: Any) => Any
//^B	Exception frame wrapper.
//^D	Evaluate \Body in exception frame.
//^D	If exception was thrown, return thrown value.
//^D	Returns result of \Body, if no exception occured.

//^N	try_catch [Exception | Wrapper]
//^P	try (Body: Any, Var: Mutable) => Any
//^B	Exception frame wrapper.
//^D	Evaluate \Body in exception frame.
//^D	If exception was thrown, assign thrown value to \Var, and return !undef.
//^D	Returns result of \Body, if no exception occured.

	DefBuiltin (P_XcptTry ("try", 0));
	DefBuiltin (P_XcptTry ("try_catch", 1));

return true;
}	// init_primaries_except

DefSubSystem ("exception", init_primaries_except, 0);

