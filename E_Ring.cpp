
/*

	+---+---+---+---+---+---+
	|	"E_Ring.cpp":
	|	Implementation of ring builtins.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef HEADER

#include "Eval.h"

#endif

struct X_Ring : Expr {
	Expr *self;						// Actual node content

	struct X_Ring *prev, *next;		// Prev/next node

	// Ring constructor: single element
	X_Ring (Expr *elem);

	// Check ring for active links
	// (returns true, if active links present)
	bool check ();

	// Count elements in ring
	unsigned count ();

	// Seek ring forward/backward
	// (by 'by' elements)
	X_Ring *seek (int by);

	// Insert new element before ring
	X_Ring *insert_before (Expr *elem);

	// Insert new element after ring
	X_Ring *insert_after (Expr *elem);

	// Remove current element from ring
	Expr *remove ();

	// Move ring element by 'step'
	void move (int by);

	// Reverse ring
	void reverse ();

	// Extract (and return) subring from ring
	// (from self to 'final')
	X_Ring *split (X_Ring *final);

	// Join rings (self with 'other')
	// (insert 'other' before)
	X_Ring *join_before (X_Ring *other);

	// Join rings (self with 'other')
	// (insert 'other' after)
	X_Ring *join_after (X_Ring *other);

	//
	//	(virtuals)
	//
	D_Expr_evalV;

	D_Expr_put;

	D_Expr_log;

	D_Expr_release;
	};


#ifndef HEADER

// Implementation

#include "Logger.h"

// Ring constructor: single element
X_Ring::X_Ring (Expr *elem) {
self = link_expr (elem);

prev = next = this;
}	// X_Ring::X_Ring

// Seek ring forward/backward
// (by 'by' elements)
X_Ring *X_Ring::seek (int by) {
X_Ring *node = this;

if (by < 0) {
	do node = node->prev;
	while (++ by);
	}
else if (by > 0) {
	do node = node->next;
	while (-- by);
	}

return node;
}	// X_Ring::seek

// Count elements in ring
unsigned X_Ring::count () {
unsigned count = 0;
X_Ring *node = this;

do count ++;
while ((node = node->next) != this);

return count;
}	// X_Ring::count

// Insert new element before ring
X_Ring *X_Ring::insert_before (Expr *elem) {
X_Ring *node = new ("Ring/insert") X_Ring (elem);
node->next = this;
node->prev = prev;
prev->next = node;
prev = node;

return node;
}	// X_Ring::insert_before

// Insert new element after ring
X_Ring *X_Ring::insert_after (Expr *elem) {
X_Ring *node = new ("Ring/insert") X_Ring (elem);
node->prev = this;
node->next = next;
next->prev = node;
next = node;

return node;
}	// X_Ring::insert_after

// Remove current element from ring
Expr *X_Ring::remove () {
if (next != this) {
	X_Ring *saved = next;

	prev->next = next;
	next->prev = prev;

	prev = next = this;

	saved->release ();			// (check && release)
	}

Expr *value = self;

if (! refs) {
	relock_expr (-1, self);
	delete this;
	}

return value;
}	// X_Ring::remove

// Extract fragment from ring
// (from self to 'final')
X_Ring *X_Ring::split (X_Ring *final) {

// Extracting this..'final'

X_Ring *prev = this->prev, *next = final->next;

this->prev = final;
final->next = this;

prev->next = next;
next->prev = prev;

// TODO: release all...

return prev;
}	// X_Ring::split

// Join rings (self with 'other')
// (insert 'other' before self)
X_Ring *X_Ring::join_before (X_Ring *other) {
X_Ring *before = this->prev, *after = other->next;

before->next = after;
after->prev = before;

this->prev = other;
other->next = this;

return before;
}	// X_Ring::join_before

// Join rings (self with 'other')
// (insert 'other' after self)
X_Ring *X_Ring::join_after (X_Ring *other) {
X_Ring *after = this->next, *before = other->prev;

after->prev = before;
before->next = after;

this->next = other;
other->prev = this;

return after;
}	// X_Ring::join_after

// Reverse ring
void X_Ring::reverse () {
X_Ring *node = this, *next;

do {
	next = node->next;
	node->next = node->prev;
	node->prev = next;
	}
while ((node = next) != this);
}	// X_Ring::reverse

// Move ring element by 'step'
void X_Ring::move (int by) {
if (by < 0) {
	X_Ring *before = this;
	do before = before->prev;
	while (++ by);
	
	if (before != this) {
		prev->next = next;
		next->prev = prev;

		next = before;
		prev = before->prev;
		prev->next = this;
		before->prev = this;
		}
	}
else if (by > 0) {
	X_Ring *after = this;
	do after = after->next;
	while (-- by);
	
	if (after != this) {
		next->prev = prev;
		prev->next = next;

		prev = after;
		next = after->next;
		next->prev = this;
		after->next = this;
		}
	}
}	// X_Ring::move

// Evaluate
VType X_Ring::evalV (VDatum &val, bool full) {
val._ring = this;
return T_ring;
}	// X_Ring::evalV

void X_Ring::log (Logger &log) {
X_Ring *node = this;

log.put_cstr ("[");
	do {
		log.log_expr (node->self);
		log.put_cstr (" > ");
		} while ((node = node->next) != this);

log.put_cstr ("]");
}	// X_Ring::log

Logger *Logger::log_ring (X_Ring *ring) {
ring->log (*this);

return this;
}	// Logger::log_ring

// Check ring for active links
// (returns true, if active links present)
bool X_Ring::check () {
X_Ring *node = this;

do if (node->refs)
	return true;
while ((node = node->next) != this);

return false;
}	// X_Ring::check

static void delete_ring (X_Ring *start) {
X_Ring *node = start, *next;

do {
	next = node->next;
	unlink_expr (node->self);
	delete node;
	}
while ((node = next) != start);
}	// delete_ring

// Release ring
void X_Ring::release () {
if (! check ())
	delete_ring (this);
}	// X_Ring::release

// Put ring
unsigned X_Ring::put (Stream *out) {
unsigned count = 0;
X_Ring *node = this;

do	count += put_expr (out, node->self);
while ((node = node->next) != this);

return count;
}	// X_Ring::put

//
//
//

// Expect ring expression
X_Ring *Prefix::expect_ring (Expr *expr) {
VDatum val;
VType type;

if ((type = evalV_X (expr, val)) == T_ring)
	return val._ring;

type_error (expr, T_ring, type, val);
return (X_Ring *) 0;
}	// Prefix::expect_ring

//
//	Create new ring
//

struct P_Ring_Create : PrefixX {
	P_Ring_Create (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		Expr *elem = evalX_X (args);

		return new ("Ring") X_Ring (elem);
		}

	};

//
//	Insert new element after/before
//

struct P_Ring_Insert : PrefixX {
	bool dir;			// ? after : before

	P_Ring_Insert (char const *ident, bool dir) : PrefixX (ident, Op_Null)
		{ this->dir = dir; }

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));
		Expr *elem = evalX_X (args);
		
		if (ring)
			(ring->*(dir ? &X_Ring::insert_after : &X_Ring::insert_before)) (elem);

		return ring;
		}

	};

//
//	Extract current element from ring
//

struct P_Ring_Extract : PrefixX {
	P_Ring_Extract (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (args);

		if (ring) return ring->remove ();

		return UNDEF;
		}
	};

//
//	Seek ring forward/backward
//

struct P_Ring_Seek : PrefixX {
	bool dir;			// ? backward : forward

	P_Ring_Seek (char const *ident, bool dir) : PrefixX (ident, Op_Null)
		{ this->dir = dir; }

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));
		S_fixed shift = expect_fixed (args, 1);

		if (ring)
			return ring->seek (dir ? - shift : shift);

		return UNDEF;
		}
	};

//
//	Ring elements accessor
//

struct P_Ring_Elem : PrefixR {
	P_Ring_Elem (char const *ident) : PrefixR (ident, Op_Null) {}

	Expr *&evalR (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));
		S_fixed index = expect_fixed (args, 0);

		if (ring)
			return ring->seek (index)->self;

		return R_null;
		}
	};

//
//	Reverse ring
//

struct P_Ring_Reverse : PrefixX {
	P_Ring_Reverse (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (args);

		if (ring) ring->reverse ();

		return ring;
		}
	};

//
//	Count elements in ring
//

struct P_Ring_Count : Prefix {
	P_Ring_Count (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));
		
		if (ring) {
			return_fixed (val, ring->count ());
			}

		return T_undef;
		}
	};

//
//	Move ring element forward/backward
//

struct P_Ring_Move : PrefixX {
	bool dir;			// ? backward : forward

	P_Ring_Move (char const *ident, bool dir) : PrefixX (ident, Op_Null)
		{ this->dir = dir; }

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));

		if (ring) {
			S_fixed shift = expect_fixed (args, 1);
			ring->move (dir ? - shift : shift);
			return ring;
			}

		return UNDEF;
		}	// evalX
	};

//
//	Ring iterator
//

struct P_Ring_Loop : P_Iterator {
	bool dir;			// ? backward : forward

	P_Ring_Loop (char const *ident, bool dir) : P_Iterator (ident, Op_Null)
		{ this->dir = dir; }

	D_P_Iterator_evaluate;
	};

void P_Ring_Loop::evaluate (IterContext &IC, Expr *args) {
Expr *&R_var = expectR_X (get_arg (args));
X_Ring *ring = expect_ring (get_arg (args));

if (ring && args) {
	X_Ring *node = ring;
	IC.start (args);

	do {
		mutateR_X (R_var, node->self);

		if (! IC.next ()) break;

		node = dir ? node->prev : node->next;
		} while (node != ring);
	}
}	// P_Ring_Loop::evaluate

//
//	Ring to list conversion
//

struct P_Ring_List : PrefixX {
	bool dir;			// ? backward : forward

	P_Ring_List (char const *ident, bool dir) : PrefixX (ident, Op_Null)
		{ this->dir = dir; }

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));

		if (ring) {
		X_List *list = 0;
		X_Ring *node = ring;

		do {
			node = dir ? node->next : node->prev;
			list = new ("X_List") X_List (node->self, list);
			} while (node != ring);

		return list;
		}

		return UNDEF;
		}	// evalX
	};

//
//	Split ring
//

struct P_Ring_Split : PrefixX {

	P_Ring_Split (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));

		if (ring) {
			S_fixed off_first = expect_fixed (get_arg (args), 0);
			S_fixed off_last = expect_fixed (args, 0);

			return ring->seek (off_first)->split (ring->seek (off_last));
			}

		return UNDEF;
		}	// evalX

	};

//
//	Join rings
//

struct P_Ring_Join : PrefixX {
	bool dir;

	P_Ring_Join (char const *ident, bool dir) : PrefixX (ident, Op_Null)
		{ this->dir = dir; }

	Expr *evalX (Expr *args) {
		X_Ring *ring = expect_ring (get_arg (args));

		if (ring) {
			X_Ring *other = expect_ring (args);
			if (other)
				return (ring->*(dir ? &X_Ring::join_after : &X_Ring::join_before)) (other);
			}

		return UNDEF;
		}	// evalX

	};

//
//	Initialisation...
//

static bool init_primaries_ring (int order) {

//		[Categories]

//^C	Ring
//^B	Ring operations
//^D	Functors, operating on rings.

//		[Types]

//^T	Ring
//^B	Ring value.
//^D	Anything evaluating to ring.
//^D	(Reports error, if argument is not ring.)

//		[Errors]

//^E	ExpectRing
//^B	Ring operand expected.
//^D	Expected operand, evaluating to ring.

//		--------

//^N	ring [Ring | Constructor]
//^P	ring (Val: Any) => Ring
//^B	Create ring.
//^D	Create and return new ring, containing single element \Val.

	DefBuiltin (P_Ring_Create ("ring"));
	DefBuiltin (P_Ring_Create ("r_create"));

//^G	r_ins_before r_ins_after

//^N	r_ins_before [Ring | Mutator]
//^P	r_ins_before (Ring: Ring, Val: Any) => Ring
//^B	Insert element before start of ring.
//^D	Insert element \Val before start of ring \Ring (== at end).
//^D	Return ring.

//^N	r_ins_after [Ring | Mutator]
//^P	r_ins_after (Ring: Ring, Val: Any) => Ring
//^B	Insert element after start of ring.
//^D	Insert element \Val after start of ring \Ring (== at beginning).
//^D	Return ring.

	DefBuiltin (P_Ring_Insert ("r_ins_before", false));
	DefBuiltin (P_Ring_Insert ("r_ins_after", true));

//^N	r_del [Ring | Mutator]
//^P	r_del (Ring: Ring) => Any
//^B	Remove ring start element.
//^D	Remove start element of ring \Ring (severing it from the rest of the ring).
//^D	Return element removed.

	DefBuiltin (P_Ring_Extract ("r_del"));

//^N	r_elem [Ring | Mutable]
//^P	r_elem (Ring: Ring, Offset: Int) => Mutable
//^B	Ring element accessor.
//^D	Get access to element of \Ring with relative \Offset.

	DefBuiltin (P_Ring_Elem ("r_elem"));

//^G	r_seek r_seek_r

//^N	r_seek [Ring]
//^P	r_seek (Ring: Ring, Offset: Int) => Ring
//^B	Seek ring cyclically forward.
//^D	Seek ring \Ring forward (cyclically) by \Offset elements.
//^D	Note: when \Offset < 0, equivalent to !r_seek_r (\Ring, -\Offset).

//^N	r_seek_r [Ring]
//^P	r_seek_r (Ring: Ring, Offset: Int) => Ring
//^B	Seek ring cyclically backward.
//^D	Seek ring \Ring backward (cyclically) by \Offset elements.
//^D	Note: when \Offset < 0, equivalent to !r_seek (\Ring, -\Offset).

	DefBuiltin (P_Ring_Seek ("r_seek", false));
	DefBuiltin (P_Ring_Seek ("r_seek_r", true));

//^N	r_count [Ring]
//^P	r_count (Ring: Ring) => Int
//^B	Count elements in ring.
//^D	Count elements in \Ring.

	DefBuiltin (P_Ring_Count ("r_count"));

//^N	r_reverse [Ring | Mutator]
//^P	r_reverse (Ring: Ring) => Ring
//^B	Reverse ring order.
//^D	Reverse elements order in \Ring.

	DefBuiltin (P_Ring_Reverse ("r_reverse"));

//^G	r_loop r_loop_r

//^N	r_loop [Ring | Iterator]
//^P	r_loop (Var: Mutable, Ring: Ring, Body: Any) => Any
//^B	Iterate forward through ring.
//^D	For each element of \Ring (moving forward), set \Var to element content, and evaluate \Body.
//^D	Return result of final evaluation of \Body.

//^N	r_loop_r [Ring | Iterator]
//^P	r_loop_r (Var: Mutable, Ring: Ring, Body: Any) => Any
//^B	Iterate backward through ring.
//^D	For each element of \Ring (moving backward), set \Var to element content, and evaluate \Body.
//^D	Return result of final evaluation of \Body.

	DefBuiltin (P_Ring_Loop ("r_loop", false));
	DefBuiltin (P_Ring_Loop ("r_loop_r", true));

//^G	r_list r_list_r

//^N	r_list [Ring | List | Constructor]
//^P	r_list (Ring: Ring) => List
//^B	Convert ring to open list (in direct order).
//^D	Return ring \Ring, converted to list in direct order.

//^N	r_list_r [Ring | List | Constructor]
//^P	r_list_r (Ring: Ring) => List
//^B	Convert ring to list (in reverse order).
//^D	Return ring \Ring, converted to list in reverse order.

	DefBuiltin (P_Ring_List ("r_list", false));
	DefBuiltin (P_Ring_List ("r_list_r", true));

//
//
//

	DefBuiltin (P_Ring_Split ("r_split"));

	DefBuiltin (P_Ring_Join ("r_join_before", false));
	DefBuiltin (P_Ring_Join ("r_join_after", true));

	DefBuiltin (P_Ring_Move ("r_move_before", false));
	DefBuiltin (P_Ring_Move ("r_move_after", true));

return true;
}	// init_primaries_ring

DefSubSystem ("ring", init_primaries_ring, 0);

#endif

