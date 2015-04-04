
/*

	+---+---+---+---+---+---+
	|	"E_Pattern.cpp":
	|	Implemenation of regular expression patterns.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef	HEADER

#include "Eval.h"

#endif

// Predicting possible range of matched string context.
// Returns false, if range is finite, or true, if not
#define	D_Pattern_predict		\
	bool _predict (FixedRange &range)

// Match regular expression against string
// Returns true (and sets 'length') on success; false on failure
#define	D_Pattern_match			\
	bool _match (struct Matcher &context)

// Check patterns for identity
#define	D_Pattern_ident			\
	bool _ident (struct X_Pattern *pattern)

// Enumerate all subopatterns
#define	D_Pattern_enum											\
	void _enum (struct Matcher &context, X_Pattern *parent,				\
		void (*callee) (X_Pattern *self, X_Pattern *parent, struct Matcher &context))

// Generic pattern
struct X_Pattern : Expr {

	virtual D_Pattern_predict;
	virtual D_Pattern_match;
	virtual D_Pattern_ident;
	virtual D_Pattern_enum;

	bool match_repcount (Matcher &context, int count);

	// Match replication of self
	virtual bool match_reprange (Matcher &context, FixedRange &range, bool dir, S_fixed *p_repcount);

	D_Expr_evalV;
	D_Expr_hash;
	D_Expr_identV;
	D_Expr_release;
	};

#ifndef HEADER

#include "String.h"

#include "Logger.h"

#define HEADER

#include "Unicode.cpp"

#undef HEADER

#include <string.h>

// Max # of match registers
enum { RegsMax = 8 };

// (In "String.cpp")
// Compare string data
int s_cvt_compare (unsigned count,
	str_ptr p_left, C_type t_left, str_ptr p_right, C_type t_right);

//
//	Pattern matching context
//

struct Matcher {
	str_ptr base;				// string base
	C_type type;				// string type

	unsigned length;			// context length
	unsigned offset;			// context offset (from base)

	unsigned p_start;			// permanent context start
	unsigned p_limit;			// permanent context limit

	FixedRange _register [RegsMax];		// match registers
	unsigned reg_count;
	FixedRange *_regextra;

	// constructor for 'Matcher': from S_string
	Matcher (S_string &string) {
		base = string.fetch (length, type);
		p_start = offset = 0;
		p_limit = length;

		queue = 0;

		// reset all registers
		for (unsigned i = 0; i != RegsMax; ++ i)
			{ _register[i].from = _register[i].to = -1; }

		reg_count = RegsMax;
		_regextra = 0;
		}	// Matcher

	// reset & clip matcher
	void clip (unsigned offset, unsigned length) {
		this->offset = offset;
		this->length = length;

		// reset queue
		queue = 0;

		// clear registers
		for (unsigned i = 0; i != RegsMax; ++ i)
			{ _register[i].from = _register[i].to = -1; }
		}	// clip

	// destructor for 'Matcher'
	~Matcher () {
	if (queue)	// (must not happen!)
		syslog->put_cstr ("Error: Matcher context not empty!!!\n");
	}	// ~Matcher

	// Pattern node to match
	struct MatchNode {
		X_Pattern *self;		// self pattern
		MatchNode *next;		// trailer next

		virtual bool match_self (Matcher &context);

		bool match_next (Matcher &context);
		} *queue;				// trailing context

	// Add match node to queue
	void enqueue (X_Pattern *pattern, MatchNode &node) {
		node.self = pattern;
		node.next = queue;
		queue = &node;
		}	// enqueue

	// Remove match node from queue
	void dequeue () { queue = queue->next; }

	// Try to match
	bool match () { return queue ? queue->match_self (*this) : true; }

	// Match character 'code'
	bool match_char (unsigned short code) {
		return length && UC_read (type, s_offset(base, offset, type)) == code;
		}	// match_char

	// Match string "_str[_length]" (of '_type')
	bool match_string (str_ptr _str, unsigned _length, C_type _type) {
	return
		_length <= length &&
		s_cvt_compare(_length, _str, _type, s_offset(base, offset, type), type) == 0;
	}	// match_string

	// Match backreference (at '_loc', of '_length')
	bool match_backref (unsigned _loc, unsigned _length) {
	// TODO: smarter match (?)
	return
		_length <= length &&
		s_mcomp (s_offset(base, _loc, type), s_offset(base, offset, type), _length, type) == 0;
	}	// match_backref

	// Test character at offset 'at', using 'predicate(arg)'
	bool check_at (unsigned at, Prefix *predicate, X_Fixed *arg) {
		arg->value = UC_read (type, s_offset(base, at, type));
		return apply_bool (predicate, arg);
		}	// check_at

	// Match character with 'predicate'
	bool match_ctest (Prefix *predicate, X_Fixed *arg, bool polarity) {
	return length ? (check_at (offset, predicate, arg) == polarity) : false;
	}	// match_ctest

	//
	//	TODO: change length to p_length ????
	//

	// Match character boundary (alignment ? ending : starting) for 'polarity'
	bool match_cbound (Prefix *predicate, X_Fixed *arg,
		bool polarity, bool alignment) {

	// Note: (A ? false : B ? C : true) => (! A && (B ? C : true)) => ! A && (! B || C)
	return
		alignment ?
			// (Check for span ending boundary)
			(offset > p_start && check_at(offset-1, predicate, arg) == polarity) &&
			(offset >= p_limit || (check_at(offset, predicate, arg) != polarity))
		:
			// (Check for span starting boundary)
			(offset < p_limit && check_at(offset, predicate, arg) == polarity) &&
			(offset <= p_start || (check_at(offset-1, predicate, arg) != polarity))
		;
	}	// match_cbound

	// Match string anchor
	//	(origin ? after : before)
	bool match_string_anchor (bool origin, X_String *string) {
	unsigned s_length;
	C_type s_type;
	str_ptr s_ptr = string->fetch (s_length, s_type);

	return origin ?
		// (Check for string anchor after):
		s_length <= p_limit - offset &&
			s_cvt_compare (s_length, s_ptr, s_type, s_offset(base, offset, type), type) == 0
	:
		// (Check for string anchor before):
		s_length <= offset - p_start &&
			s_cvt_compare (s_length, s_ptr, s_type, s_offset(base, offset - s_length, type), type) == 0
	;
	}	// match_string_anchor

	// Match position relative to context beginning/end
	bool match_offset (unsigned expect_offset, bool align) {
	// Check for offset:
	return expect_offset ==
		(align ?
		// ... from context end :
			p_limit - offset :
		// ... from context beginning :
			offset - p_start
		);
	}	// match_offset

	// advance scanner by 'displace'
	bool advance (unsigned displace);

	// Find span of characters 'code'
	// (not more than 'limit')
	// Returns # of characters actually spanned
	unsigned span_char (unsigned short code, unsigned limit) {
	unsigned pos = offset;
	limit += offset;

	while (pos != limit && UC_read (type, s_offset(base, pos, type)) == code)
		++ pos;

	return pos - offset;
	}	// span_char

	// Find span of characters, matching test 'pred' for 'polarity'
	// (not more than 'limit')
	// Returns # of characters actually spanned
	unsigned span_ctest (Prefix *pred, X_Fixed *arg, bool polarity, unsigned limit) {
	unsigned pos = offset;
	limit += offset;

	while (pos != limit && check_at (pos, pred, arg) == polarity)
		++ pos;

	return pos - offset;
	}	// span_ctest

	// Advance by first/last (depending on 'direction') offset in 'range'
	bool range_span (FixedRange &range, bool direction, S_fixed *p_repcount) {
	if (range.from <= range.to) {
		if (direction) {
			for (S_fixed offset = range.to; offset >= range.from; -- offset) {
				if (advance (offset)) {
					if (p_repcount) *p_repcount = offset;
					return true;
					}
				}
			}
		else {
			for (S_fixed offset = range.from; offset <= range.to; ++ offset) {
				if (advance (offset)) {
					if (p_repcount) *p_repcount = offset;
					return true;
					}
				}
			}
		}	// (not empty range)

	return false;
	}	// range_span

	// Get register 'reg_no'
	FixedRange get_register (unsigned reg_no) {
	if (reg_no < RegsMax)
		return _register[reg_no];
	
	return FixedRange (-1, -1);			// (when fail...)
	}	// get_register

	// Mark end/start of register 'reg_no'
	void mark_register (bool end_start, unsigned reg_no) {
	if (reg_no < RegsMax)
		_register[reg_no].set_bound (end_start, offset);
	}	// mark_register

	// Reset register 'reg_no'
	void reset_register (unsigned reg_no) {
	if (reg_no < RegsMax)
		_register[reg_no].set (-1, -1);
	}	// reset_register

	// Save register to mutable
	void save_register (unsigned reg_no, Expr *store) {
		mutateX_X (store, _register[reg_no].eval ());
	}	// save_register

	};

// Check 'string' for matching 'pattern'
//	(returning 'length' on success)
static bool test_match (S_string &string, X_Pattern *pattern, unsigned &length) {
Matcher context (string);

if (pattern->_match (context)) {
	length = context.offset;
	return true;
	}

return false;
}	// test_match

// Find matching 'pattern' in 'string'
//	'dir' ? decrementing (from end) : incrementing (from begin)
//	(Sets 'range' on success)
static bool find_match (S_string &string, X_Pattern *pattern, bool direction, FixedRange &range) {
Matcher context (string);

if (direction) {
	// Decremental context search:
	context.offset = context.length;
	context.length = 0;

	for (;;) {
	unsigned offset = context.offset;
	if (pattern->_match (context)) {
		range.set (offset, context.offset);
		return true;
		}

	// (decrement position)
	if (! context.offset --) break;
	++ context.length;
	}	// for ()
	}	// (decremental scan)

else {
	// Incremental context search:
	for (;;) {
	unsigned offset = context.offset;
	if (pattern->_match (context)) {
		range.set (offset, context.offset);
		return true;
		}

	// (increment position)
	if (! context.length --) break;
	++ context.offset;
	}	// for ()
	}	// (incremental scan)

return false;
}	// find_match

//
//	Define list of found matches
//

// List of match definitions
struct MatchList {
	S_string &string;		// context to find in
	X_Pattern *pattern;		// pattern to look for
	unsigned limit;			// (not more than 'limit' matches)

	// Match definition
	struct MatchDef {
		FixedRange where;	// (context mathed)
		MatchDef *prev;		// (next in chain)
		} *last;			// (last node of list)

	// Called, when list building complete
	// (returns any boolean value -- normally, true on non-empty list)
	virtual bool complete (bool dir) { return last != 0; }

	MatchList (S_string &string, X_Pattern *pattern, unsigned limit) :
		string(string) {

		this->limit = limit;
		this->pattern = pattern;

		last = 0;
		}	// (constructor)

	// Build match list (string, pattern) incrementally.
	// (Limit by 'count' matches; invoke MatchList.action (string) when finished)
	// Returns: result of 'complete'
	bool build_inc (Matcher &context) {
		MatchDef m_node;

		for (;;) {
			unsigned c_offset = context.offset;
			unsigned c_length = context.length;
			if (pattern->_match (context)) {	// (match found!)
			unsigned length = context.offset;	// (match length)

			m_node.where.set (c_offset, length);
			m_node.prev = last;
			last = &m_node;

			if (limit && ! -- limit) break;

			// Restore context

			if (length -= c_offset)
				{ c_offset += length; c_length -= length; }
			else	// (empty match: increment position)
				{ c_offset ++; if (! c_length --) break; }

			context.clip (c_offset, c_length);

			// (Recursion:)
			bool result = build_inc (context);

			last = m_node.prev;
			return result;
			}	// (match found)

			context.offset = c_offset;
			context.length = c_length;

			// (increment match position)
			if (! context.length --) break;
			++ context.offset;
			}	// for (;;)

 		return complete (false);		// (incremental list complete)
		}	// build_inc

	// Build match list (string, pattern) decrementally.
	// (Limit by 'count' matches; invoke MatchList.action (string) when finished)
	// Returns: result of 'complete'
	bool build_dec (Matcher &context) {
		MatchDef m_node;

		context.offset = context.length;
		context.length = 0;

		for (;;) {
			unsigned c_offset = context.offset;
			if (pattern->_match (context)) {	// (match found!)

			m_node.where.set (c_offset, context.offset);
			m_node.prev = last;
			last = &m_node;

			if (limit && ! -- limit) break;

			context.clip (context.p_start, c_offset);

			// (Recursion:)
			bool result = build_dec (context);

			last = m_node.prev;
			return result;
			}

			context.offset = c_offset;

			// (decrement match position)
			if (! context.offset --) break;
			++ context.length;
			}	// for (;;)

		return complete (true);		// (decremental list complete)
		}	// build_dec

	// Find total nodes count in list
	unsigned total () {
		unsigned count = 0;

		for (MatchDef *node = last; node; node = node->prev)
			count ++;

		return count;
		}	// total

	};		// struct MatchList

//
//	Patterns
//

VType X_Pattern::evalV (VDatum &val, bool full) {
val._pattern = this;
return T_pattern;
}	// X_Pattern::evalV

void X_Pattern::release () { Expr::release (); }

Logger *Logger::log_pattern (X_Pattern *pattern) {
if (pattern) pattern->log (*this);

return this;
}	// Logger::log_pattern

bool X_Pattern::_predict (FixedRange &range) {
range.set (0, 0);
return false;
}	// X_Pattern::_predict

bool X_Pattern::_match (Matcher &context) {
return context.match ();				/// ??????
}	// X_Pattern::_match

// Check patterns for identity
bool X_Pattern::identV (VType type, VDatum &val) {
if (type == T_pattern) {
	X_Pattern *pattern = val._pattern;

	if (pattern == this) return true;

	return _ident (pattern);
	}

return false;
}	// X_Pattern::identV

// Check prefix for identity
bool prefix_ident (Prefix *pfx, Prefix *pfx_alt) {
return pfx == pfx_alt;
}	// prefix_ident

// Identity of pattern
bool X_Pattern::_ident (X_Pattern *pattern) {
return this == pattern;
}	// X_Pattern::_ident

// Hash code of pattern
unsigned X_Pattern::hash () {
return ~0;
}	// X_Pattern::hash

// Advance by 'displace'
// (assuming dispalce <= context.length)
bool Matcher::advance (unsigned displace) {
offset += displace;
length -= displace;

if (match ()) return true;

offset -= displace;
length += displace;

return false;
}	// Matcher::advance

// Enumerate subpatterns (generic)
void X_Pattern::_enum (Matcher &context, X_Pattern *parent,
	void (*callee) (X_Pattern *self, X_Pattern *parent, Matcher &context)) {
(*callee) (this, parent, context);
}	// X_Pattern::_enum

//
//	Nodes matching
//

// Remove/try matching self node
bool Matcher::MatchNode::match_self (Matcher &context) {
context.queue = next;
bool result = self->_match (context);
context.queue = this;

return result;
}	// Matcher::MatchNode::match_self

// Remove self & try matching next nodes
bool Matcher::MatchNode::match_next (Matcher &context) {
context.queue = next;
bool result = context.match ();
context.queue = this;

return result;
}	// Matcher::MatchNode::match_next

// Repetition modifier:
//	match self precisely 'count' times
struct QRepFixed : Matcher::MatchNode {
	unsigned count;

	QRepFixed (unsigned count) { this->count = count; }

	bool match_self (Matcher &context) {
		if (count) {
			-- count;
			bool result = self->_match (context);
			++ count;
			return result;
			}
		else
			return MatchNode::match_self (context);
		}	// match_self

	};	// QRepFixed

// Repetition modifier:
//	match self from 0 to 'limit' times
//	direction ? (find max count) : (find min count)
struct QRepLimit : Matcher::MatchNode {
	bool direction;
	unsigned limit;

	QRepLimit (bool direction, unsigned count)
		{ this->direction = direction; limit = count; }

	bool match_self (Matcher &context) {
		if (direction) {
		// try to match max count of self

		if (limit) {
			-- limit;
			bool result = self->_match (context);
			++ limit;
			if (result) return true;
			}

		return match_next (context);
		}
		else {
		// try to match min count of self

		if (match_next (context))
			return true;

		if (limit) {
			-- limit;
			bool result = self->_match (context);
			++ limit;
			return result;
			}

		return false;
		}
		}	// match_self
	};	// QRepLimit

// Repetition modifier:
//	match self from 0 to 'limit' times (with counter)
//	direction ? (find max count) : (find min count)
struct QRepLimitCount : Matcher::MatchNode {
	bool direction;
	unsigned limit;
	unsigned counter;			// (matched actually)

	QRepLimitCount (bool direction, unsigned count)
		{ this->direction = direction; limit = count; counter = 0; }

	bool match_self (Matcher &context) {
		if (direction) {
		// try to match max count of self

		if (limit) {
			-- limit;
			bool result = self->_match (context);
			++ limit;
			if (result) { ++ counter; return true; }
			}

		return match_next (context);
		}
		else {
		// try to match min count of self

		if (match_next (context))
			return true;

		if (limit) {
			-- limit;
			bool result = self->_match (context);
			++ limit;
			if (result) ++ counter;
			return result;
			}

		return false;
		}
		}	// match_self
	};	// QRepLimitCount

// Repetition modifier:
//	match self unlimited number of times
//	direction ? (find max count) : (find min count)
struct QRepInfin : Matcher::MatchNode {
	bool direction;

	QRepInfin (bool direction) { this->direction = direction; }

	bool match_self (Matcher &context) {
		return
			direction ?
				// try to match max count
			((self->_match (context)) || match_next (context))
			:
				// try to match min count
			(match_next (context) || (self->_match (context)));
		}	// match_self

	};	// QRepInfin

// Repetition modifier:
//	match self unlimited number of times (with counter)
//	direction ? (find max count) : (find min count)
struct QRepInfinCount : Matcher::MatchNode {
	bool direction;
	unsigned counter;

	QRepInfinCount (bool direction) { this->direction = direction; counter = 0; }

	bool match_self (Matcher &context) {
		return
			direction ?
				// try to match max count
			((self->_match (context) ? (++ counter, true) : false) ||
				match_next (context))
			:
				// try to match min count
			(match_next (context) ||
				(self->_match (context) ? (++ counter, true) : false));
		}	// match_self

	};	// QRepInfinCount

// Match exact repetition of pattern ('count' times)
bool X_Pattern::match_repcount (Matcher &context, int count) {
if (count > 0)
	if (count > 1) {
		QRepFixed repeat_fixed (count - 1);
		context.enqueue (this, repeat_fixed);
		bool result = context.match ();
		context.dequeue ();
		return result;
		}
	else	// (count == 1)
		return _match (context);
else
	return context.match ();
}	// X_Pattern::match_repcount

// Match repetition of pattern
// (in range 'range', incremental/decremental depending from direction)
bool X_Pattern::match_reprange (Matcher &context, FixedRange &range, bool direction,
	S_fixed *p_repcount) {
int from = range.from;
bool result;

if (range.to >= 0) {
if (range.notempty ()) {
	// (match finite repeat range)
	unsigned limit = range.to - from;
	if (p_repcount) {	// (match with counter)
		QRepLimitCount repeat_limit_c (direction, limit);
		context.enqueue (this, repeat_limit_c);
		if (result = match_repcount (context, from))		// (matched!)
			*p_repcount = from + repeat_limit_c.counter;
		context.dequeue ();
		return result;
		}
	else {		// (simple match)
		QRepLimit repeat_limit (direction, limit);
		context.enqueue (this, repeat_limit);
		result = match_repcount (context, from);
		context.dequeue ();
		return result;
		}
	}	// (-- match finite)
else {	// (match exact repeat count)
	if (match_repcount (context, from)) {
		if (p_repcount) *p_repcount = from;
		return true;
		}

	return false;
	}	// (-- match exact)
}
else {
	// (match infinite repetitions)
	if (p_repcount) {	// (match with counter)
		QRepInfinCount repeat_infin_c (direction);
		context.enqueue (this, repeat_infin_c);
		if (result = match_repcount (context, from))
			*p_repcount = from + repeat_infin_c.counter;
		context.dequeue ();
		return result;
		}
	else {		// (simple match)
		QRepInfin repeat_infin (direction);
		context.enqueue (this, repeat_infin);
		result = match_repcount (context, from);
		context.dequeue ();
		return result;
		}
	}
}	// X_Pattern::match_reprange

//
//	Literal character pattern
//

struct Pattern_Char : X_Pattern {
	C_long code;

	Pattern_Char (C_long code) { this->code = code; }

	void log (Logger &log) {
		// TODO: correct this...
		if (' ' <= code && code < 0x7F)
			log.put_ch ('\'')->log_wch (code)->put_ch ('\'');
		else
			log.put_hex (4, code);
		}	// log

	bool _predict (FixedRange &range) {
		range.set (1, 1);
		return false;
		}	// _predict

	bool _match (Matcher &context) {
		return context.match_char (code)? context.advance (1) : false;
		}	// _match

	// Match repetition of character
	// (in range 'range', incremental/decremental depending from direction)
	bool match_reprange (Matcher &context, FixedRange &range, bool direction, S_fixed *p_repcount) {
	FixedRange _range = range;
	unsigned count = context.length;
	if (_range.to >= 0 && _range.to <= count)
		count = _range.to;

	_range.to = context.span_char (code, count);

	return context.range_span (_range, direction, p_repcount);
	}	// match_reprange

	// Hash code of pattern
	unsigned hash () { return ~code; }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Char *expect = Cast (Pattern_Char, pattern);
		return expect && expect->code == code;
		}	// _ident

	};

//
//	Literal string pattern
//

struct Pattern_String : X_Pattern {
	X_String *string;

	Pattern_String (S_string &s_str)
		{ link_expr (string = s_str.cons ()); }

	void log (Logger &log) { log.log_string (string); }

	bool _predict (FixedRange &range) {
		unsigned len;
		C_type type;
		string->fetch (len, type);

		range.set (len, len);
		return false;
		}	// _predict

	bool _match (Matcher &context) {
		unsigned len;
		C_type type;
		str_ptr str = string->fetch (len, type);

		return context.match_string (str, len, type) ?
			context.advance (len) : false;
		}	// _match

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_String *expect = Cast (Pattern_String, pattern);
		return expect && identX (expect->string, string);
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return ~(string->hash ()); }

	void release () {
		unlink_expr (string);
		X_Pattern::release ();
		}	// release
	};

//
//	String boundary anchor pattern
//

struct Pattern_StringBound : X_Pattern {
	X_String *string;
	bool polarity, origin;

	Pattern_StringBound (S_string &s_str, bool polarity, bool origin) {
		this->polarity = polarity;
		this->origin = origin;

		link_expr (string = s_str.cons ());
		}

	bool _match (Matcher &context) {
		return context.match_string_anchor (origin, string) == polarity ?
			context.match () : false;
		}	// _match

	bool _predict (FixedRange &range) {
		range.set (0, 0);
		return false;
		}	// _predict

	void log (Logger &log) {
		log.put_ch ('<');
		if (!polarity) log.put_ch ('~');
		log.put_ch ('=');
		log.log_string (string);
		log.log_flag (origin);
		log.put_ch ('>');
		}	// log

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_StringBound *expect = Cast (Pattern_StringBound, pattern);
		return expect && identX (expect->string, string) &&
			expect->polarity == polarity && expect->origin == origin;
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return ~(string->hash ()); }

	void release () {
		unlink_expr (string);
		X_Pattern::release ();
		}	// release
	};

//
//	Any character pattern
//

struct Pattern_Any : X_Pattern {

	Pattern_Any () {}

	void log (Logger &log) { log.put_cstr ("<.>"); }

	bool _predict (FixedRange &range) {
		range.set (1, 1);
		return false;
		}	// _predict

	bool _match (Matcher &context) {
		return context.length ? context.advance (1) : false;
		}	// _match

	// Match repetition of character
	// (in range 'range', incremental/decremental depending from direction)
	bool match_reprange (Matcher &context, FixedRange &range, bool direction, S_fixed *p_repcount) {
	FixedRange _range = range;
	unsigned count = context.length;
	if (_range.to < 0 || _range.to > count)
		_range.to = count;

	return context.range_span (_range, direction, p_repcount);
	}	// match_reprange

	// Hash code of pattern
	unsigned hash () { return 0; }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Any *expect = Cast (Pattern_Any, pattern);
		return expect;
		}	// _ident

	};

//
//	Span character [un]matching predicate
//

struct Pattern_CTest : X_Pattern {
	Prefix *pred;			// test predicate
	bool polarity;			// test polarity
	X_Fixed *arg;			// argument of 'pred'

	Pattern_CTest (Prefix *pred, bool polarity) {
		link_prefix (this->pred = pred);
		this->polarity = polarity;
		link_expr (arg = new ("X_Fixed") X_Fixed (0));
		}

	void log (Logger &log) {
		log.put_ch ('<')->log_flag (polarity)->log_prefix (pred)->put_ch ('>');
		}

	bool _predict (FixedRange &range) {
		range.set (1, 1);
		return false;
		}	// _predict

	bool _match (Matcher &context) {
		return context.match_ctest (pred, arg, polarity) ?
			context.advance (1) : false;
		}	// _match

	// Match repetition
	// (in range 'range', incremental/decremental depending from direction)
	bool match_reprange (Matcher &context, FixedRange &range, bool direction, S_fixed *p_repcount) {
	FixedRange _range = range;
	unsigned count = context.length;
	if (_range.to >= 0 && _range.to <= count)
		count = _range.to;

	_range.to = context.span_ctest (pred, arg, polarity, count);

	return context.range_span (_range, direction, p_repcount);
	}	// match_reprange

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_CTest *expect = Cast (Pattern_CTest, pattern);
		return expect
			&& prefix_ident (pred, expect->pred)
			&& polarity == expect->polarity;
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return pred->hash(); }

	void release () {
		unlink_expr (arg);
		unlink_prefix (pred);
		X_Pattern::release ();
		}	// release
	};

//
//	Character start/end boundary anchor
//

struct Pattern_CBound : X_Pattern {
	Prefix *pred;			// test predicate
	bool polarity;			// test polarity (test for true / for false)
	bool align;				// ? match at end : match at beginning
	X_Fixed *arg;			// argument of 'pred'

	Pattern_CBound (Prefix *pred, bool polarity, bool align) {
		link_prefix (this->pred = pred);
		this->polarity = polarity;
		this->align = align;
		link_expr (arg = new ("X_Fixed") X_Fixed (0));
		}

	void log (Logger &log) {
		log.put_ch ('<')->put_ch ('@')->log_flag (! align)->put_ch (':')->
			log_flag (polarity)->log_prefix (pred)->put_ch ('>');
		}

	bool _predict (FixedRange &range) {
		range.set (0, 0);
		return false;
		}	// _predict

	bool _match (Matcher &context) {
		return
			context.match_cbound (pred, arg, polarity, align) ?
				context.match () : false;
		}	// _match

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_CBound *expect = Cast (Pattern_CBound, pattern);
		return expect
			&& prefix_ident (pred, expect->pred)
			&& polarity == expect->polarity
			&& align == expect->align;
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return pred->hash(); }

	void release () {
		unlink_expr (arg);
		unlink_prefix (pred);
		X_Pattern::release ();
		}	// release
	};

//
//	Context position anchor
//

struct Pattern_Offset : X_Pattern {
	unsigned offset;
	bool align;				// ? offset from end : offset from start

	Pattern_Offset (bool align, unsigned offset) {
		this->align = align;
		this->offset = offset;
		}

	void log (Logger &log) {
		log.put_ch ('<')->put_ch ('@')->log_flag (! align)->log_fixed (offset)->put_ch ('>');
		}

	bool _predict (FixedRange &range) {
		range.set (0, 0);
		return false;
		}	// _predict

	bool _match (Matcher &context) {
		return
			context.match_offset (offset, align) ?
				context.match () : false;
		}	// _match

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Offset *expect = Cast (Pattern_Offset, pattern);
		return expect
			&& offset == expect->offset
			&& align == expect->align;
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return offset; }

	void release ()
		{ X_Pattern::release (); }
	};

//
//	Failure pattern
//

struct Pattern_Fail : X_Pattern {

	void log (Logger &log) { log.put_cstr ("<!>"); }

	bool _match (Matcher &context) { return false; }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Fail *expect = Cast (Pattern_Fail, pattern);
		return expect;
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return 0; }

	};

//
//	Null pattern
//

struct Pattern_Null : X_Pattern {

	void log (Logger &log) { log.put_cstr ("<>"); }

	bool _match (Matcher &context) { context.match (); }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Null *expect = Cast (Pattern_Null, pattern);
		return expect;
		}	// _ident

	};

//
//	Evaluation pattern
//

struct Pattern_Eval : X_Pattern {

	Expr *do_eval;			// (expression to evaluate)

	Pattern_Eval (Expr *do_eval) { link_expr (this->do_eval = do_eval); }

	void log (Logger &log)
		{ log.put_cstr ("<... ")->log_expr (do_eval)->put_cstr (" ...>"); }

	bool _match (Matcher &context) { evalZ_X (do_eval); context.match (); }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Eval *expect = Cast (Pattern_Eval, pattern);
		return expect && identX (do_eval, expect->do_eval);
		}	// _ident

	// Hash code of pattern
	unsigned hash () { return hashX (do_eval); }

	void release () {
		unlink_expr (do_eval);

		X_Pattern::release ();
		}	// release
	};

//
//	Alternation of patterns
//

struct Pattern_Alt : X_Pattern {
	X_Pattern *first;
	X_Pattern *second;

	Pattern_Alt (X_Pattern *_first, X_Pattern *_second) {
		link_expr (first = _first);
		link_expr (second = _second);
		}

	void log (Logger &log) {
		log.put_ch ('<')->log_pattern (first)->put_cstr (" | ")->log_pattern (second)->put_ch ('>');
		}

	bool _predict (FixedRange &range) {
		FixedRange range1, range2;
		bool inf_first = first->_predict (range1);
		bool inf_second = second->_predict (range2);

		range.set (
			range1.from < range2.from ? range1.from : range2.from,
			range1.to > range2.to ? range1.to : range2.to
			);

		return inf_first || inf_second;
		}	// _predict

	bool _match (Matcher &context) {
		return
			first->_match (context) ||
			second->_match (context);
		}	// _match

	// Hash code of pattern
	unsigned hash ()
		{ return hashX (first) ^ hashX (second); }

	void _enum (Matcher &context, X_Pattern *parent,
		void (*callee) (X_Pattern *self, X_Pattern *parent, Matcher &context)) {
		first->_enum (context, this, callee);
		second->_enum (context, this, callee);

		X_Pattern::_enum (context, parent, callee);
		}	// _enum

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Alt *expect = Cast (Pattern_Alt, pattern);
		return expect
			&& identX (first, expect->first)
			&& identX (second, expect->second);
		}	// _ident

	void release () {
		unlink_expr (first);
		unlink_expr (second);

		X_Pattern::release ();
		}	// release

	};

//
//	Concatenation of patterns
//

struct Pattern_Cat : X_Pattern {
	X_Pattern *head;
	X_Pattern *tail;

	Pattern_Cat (X_Pattern *_head, X_Pattern *_tail) {
		link_expr (head = _head);
		link_expr (tail = _tail);
		}

	void log (Logger &log) {
		log.put_ch ('<')->log_pattern (head)->put_cstr (" & ")->log_pattern (tail)->put_ch ('>');
		}

	bool _predict (FixedRange &range) {
		FixedRange range1, range2;
		bool inf_head = head->_predict (range1);
		bool inf_tail = tail->_predict (range2);

		range.set (range1.from + range2.from, range1.to + range2.to);
		return inf_head || inf_tail;
		}	// _predict

	bool _match (Matcher &context) {
		Matcher::MatchNode node;
		context.enqueue (tail, node);

		bool result = head->_match (context);

		context.dequeue ();
		return result;
		}	// _match

	// Hash code of pattern
	unsigned hash ()
		{ return hashX (head) ^ hashX (tail); }

	void _enum (Matcher &context, X_Pattern *parent,
		void (*callee) (X_Pattern *self, X_Pattern *parent, Matcher &context)) {
		head->_enum (context, this, callee);
		tail->_enum (context, this, callee);

		X_Pattern::_enum (context, parent, callee);
		}	// _enum

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Cat *expect = Cast (Pattern_Cat, pattern);
		return expect
			&& identX (head, expect->head)
			&& identX (tail, expect->tail);
		}	// _ident

	void release () {
		unlink_expr (head);
		unlink_expr (tail);

		X_Pattern::release ();
		}	// release

	};

//
//	Repetition of pattern
//

struct Pattern_Rep : X_Pattern {
	X_Pattern *body;		// pattern to repeat

	FixedRange range;		// matches range
	bool dir;				// direction? decremental : incremental

	Expr *rep_count;		// storage for repeat count (optional)

	Pattern_Rep (X_Pattern *body, FixedRange &range, bool dir) {
		link_expr (this->body = body);

		this->range = range;
		this->dir = dir;

		rep_count = 0;
		}

	Pattern_Rep (Expr *rep_count, X_Pattern *body, FixedRange &range, bool dir) {
		link_expr (this->body = body);

		this->range = range;
		this->dir = dir;
		
		if (rep_count) link_expr (this->rep_count = evalX_X (rep_count));
		}

	void log (Logger &log) {
		log.put_ch ('<')->log_pattern (body)->put_cstr (" * ");

		if (range.from != range.to) {
			if (range.from >= 0) log.log_fixed (range.from);
			log.put_cstr ("..");
			if (range.to >= 0) log.log_fixed (range.to);
			log.log_flag (! dir);
			}
		else
			log.log_fixed (range.from);

		if (rep_count)
			log.put_cstr (" -> ")->log_expr (rep_count);

		log.put_ch ('>');
		}	// log

	bool _predict (FixedRange &_range) {
		bool inf = body->_predict (_range);
		_range.from *= range.from;
		_range.to *= range.to;
		return inf || _range.to < 0;
		}	// _predict

	bool _match (Matcher &context) {
		if (rep_count) {
			S_fixed counted = 0;
			if (body->match_reprange (context, range, dir, &counted)) {
				assign_fixed (rep_count, counted);
				return true;
				}
			return false;
			}

		return body->match_reprange (context, range, dir, 0);
		}	// _match

	// Hash code of pattern
	unsigned hash ()
		{ return hashX (body) ^ range.from ^ range.to; }

	void _enum (Matcher &context, X_Pattern *parent,
		void (*callee) (X_Pattern *self, X_Pattern *parent, Matcher &context)) {
		body->_enum (context, this, callee);

		X_Pattern::_enum (context, parent, callee);
		}	// _enum

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Rep *expect = Cast (Pattern_Rep, pattern);
		return expect
			&& identX (body, expect->body)
			&& identX (rep_count, expect->rep_count)
			&& dir == expect->dir
				&& range.from == expect->range.from
				&& range.to == expect->range.to;
		}	// _ident

	void release () {
		unlink_expr (body);
		unlink_expr (rep_count);

		X_Pattern::release ();
		}

	};

//
//	Register store/recall
//

// (Register store finalization)
struct StoreFinalize : Matcher::MatchNode {
	unsigned reg_no;
	Expr *storage;

	StoreFinalize (unsigned reg_no, Expr *storage) {
		this->reg_no = reg_no;
		this->storage = storage;
		}

	bool match_self (Matcher &context) {
		context.mark_register (true, reg_no);
		if (storage) context.save_register (reg_no, storage);
		return match_next (context);
		};

	};

//
//	Store match to register
//

struct Pattern_Store : X_Pattern {
	unsigned reg_no;			// # of register
	Expr *storage;				// storage mutable
	X_Pattern *body;			// match & store

	Pattern_Store (X_Pattern *body, unsigned reg_no, Expr *storage) {
		link_expr (this->body = body);
		link_expr (this->storage = storage);
		this->reg_no = reg_no;
		}

	void log (Logger &log) {
		log.put_ch ('<')->
			put_ch ('#')->log_fixed (reg_no)->put_cstr (" = ")->
			log_pattern (body)->put_ch ('>');
		}	// log

	bool _match (Matcher &context) {
		StoreFinalize node (reg_no, storage);
		context.enqueue (0, node);
		context.mark_register (false, reg_no);

		bool result = body->_match (context);

		if (! result)
			context.reset_register (reg_no);

		context.dequeue ();
		return result;
		}	// _match

	bool _predict (FixedRange &_range)
		{ return body->_predict (_range); }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Store *expect = Cast (Pattern_Store, pattern);
		return expect
			&& reg_no == expect->reg_no
			&& identX (body, expect->body);
		}	// _ident

	// Hash code of pattern
	unsigned hash ()
		{ return hashX (body) ^ reg_no; }

	void release () {
		unlink_expr (body);
		unlink_expr (storage);

		X_Pattern::release ();
		}	// release

	};

//
//	Recall match from register
//

struct Pattern_Recall : X_Pattern {
	unsigned reg_no;			// # of register

	Pattern_Recall (unsigned reg_no)
		{ this->reg_no = reg_no; }

	void log (Logger &log) {
		log.put_ch ('<')->put_ch ('#')->put_ch ('?')->log_fixed (reg_no)->put_ch ('>');
		}	// log

	bool _match (Matcher &context) {
		FixedRange _range = context.get_register (reg_no);

		if (_range.from < 0 || _range.to < 0 || _range.to < _range.from)
			return false;		// (not set yet)

		unsigned len = _range.to - _range.from;

		return context.match_backref (_range.from, len) ?
			context.advance (len) : false;
		}	// _match

	bool _predict (FixedRange &_range) {
		// (not much predictable!)
		_range.from = _range.to = 0;
		return true;
		}	// _predict

	// Hash code of pattern
	unsigned hash ()
		{ return reg_no; }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Recall *expect = Cast (Pattern_Recall, pattern);
		return expect
			&& reg_no == expect->reg_no;
		}	// _ident

	};

//
//	Extend register space (TODO)
//

struct Pattern_RegExtend : X_Pattern {
	unsigned reg_total;			// # of register
	FixedRange *reg_extra;
	X_Pattern *body;

	Pattern_RegExtend (unsigned reg_total, X_Pattern *body) {
		this->reg_total = reg_total;
		reg_extra = new FixedRange [reg_total];
		link_expr (this->body = body);
		}

	void log (Logger &log) {
		log.put_ch ('<')->log_pattern (body)->put_ch ('#')->
			put_ch ('!')->log_fixed (reg_total)->put_ch ('>');
		}	// log

	bool _match (Matcher &context) {
		unsigned reg_count_sv = context.reg_count;
		FixedRange *reg_extra_sv = context._regextra;

		context.reg_count = reg_total;
		context._regextra = reg_extra;

		bool result = body->_match (context);

		context.reg_count = reg_count_sv;
		context._regextra = reg_extra_sv;
		return result;
		}	// _match

	// Hash code of pattern
	unsigned hash ()
		{ return body->hash (); }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_RegExtend *expect = Cast (Pattern_RegExtend, pattern);
		return expect
			&& reg_total == expect->reg_total
			&& identX (body, expect->body);
		}	// _ident

	bool _predict (FixedRange &_range) {
		return body->_predict (_range);
		}	// _predict

	};

//
//	Force non-zero match
//

// (Register store finalization)
struct DenullNode : Matcher::MatchNode {
	unsigned pos;

	DenullNode (Matcher &context) {
		// (preserve offset)
		pos = context.offset;
		}

	bool match_self (Matcher &context) {
		// (check preserved offset)
		return (context.offset > pos) && match_next (context);
		};

	};

struct Pattern_Denull : X_Pattern {
	X_Pattern *body;

	Pattern_Denull (X_Pattern *body)
		{ link_expr (this->body = body); }

	void log (Logger &log) {
		log.put_ch ('<')->put_cstr ("~'': ")->log_pattern (body)->put_ch ('>');
		}	// log

	bool _match (Matcher &context) {
		DenullNode node (context);
		context.enqueue (0, node);

		bool result = body->_match (context);

		context.dequeue ();
		return result;
		}	// _match

	bool _predict (FixedRange &_range) {
		bool inf = body->_predict (_range);
		if (! _range.from) ++ _range.from;		// (never match empty context)
		return inf;
		}	// _predict

	// Hash code of pattern
	unsigned hash ()
		{ return body->hash (); }

	// Ident check
	bool _ident (X_Pattern *pattern) {
		Pattern_Denull *expect = Cast (Pattern_Denull, pattern);
		return expect && identX (body, expect->body);
		}	// _ident

	// Release
	void release () {
		unlink_expr (body);
		X_Pattern::release ();
		}

	};

//
//	Pattern evaluator
//

//
//	Common...
//

// Expect 'expr' to be pattern...
X_Pattern *Prefix::expect_pattern (Expr *expr) {
VDatum val;
VType type;

switch (type = evalV_X (expr, val)) {
	case T_pattern:
		return val._pattern;

	case T_fixed:
		return new ("Pattern/Char[Auto]") Pattern_Char (val._fixed);

	case T_string:
		if (val._string.length == 1) {
			// (one-character pattern)
			C_type type;
			unsigned len;
			str_ptr data = val._string.fetch (len, type);
			unsigned code = UC_read (type, data);
			val._string.relink ();

			return new ("Pattern/Char[Auto]") Pattern_Char (code);
			}
		else {
			// (string pattern)
			X_Pattern *result = new ("Pattern/String[Auto]") Pattern_String (val._string);
			val._string.relink ();
			return result;
			}
	}	// switch (expr)

type_error (expr, T_pattern, type, val);
return new ("Pattern/Stub") Pattern_Fail ();
}	// Prefix::expect_pattern

//
//	Static constructors for patterns
//
//	(args: argument list, where: prefix context)
//

// Construct null pattern
static X_Pattern *rx_Null (Expr *args, Prefix *where) {
return new ("Pattern/Null") Pattern_Null ();
}	// rx_Null

// Construct unconditional failure pattern
static X_Pattern *rx_Fail (Expr *args, Prefix *where) {
return new ("Pattern/Fail") Pattern_Fail ();
}	// rx_Fail

// Construct literal character pattern
static X_Pattern *rx_Char (Expr *args, Prefix *where) {
return new ("Pattern/Char") Pattern_Char (where->expect_fixed (args, 0));
}	// rx_Char

// Construct literal string pattern
static X_Pattern *rx_String (Expr *args, Prefix *where) {
S_string string;
if (where->expect_string (args, string)) {
	X_Pattern *pattern = new ("Pattern/String") Pattern_String (string);
	string.relink ();
	return pattern;
	}

return (X_Pattern *) 0;
}	// rx_String

// Construct any character pattern
static X_Pattern *rx_Any (Expr *args, Prefix *where) {
return new ("Pattern/Any") Pattern_Any ();
}	// rx_Any

// Construct character test pattern
//	('in_ex' ? inclusive : exclusive)
static X_Pattern *rx_CTest (Expr *args, Prefix *where, bool in_ex) {
Prefix *c_pred = where->expect_prefix (args);
return c_pred ? new ("Pattern/CTest") Pattern_CTest (c_pred, in_ex) : 0;
}	// rx_CTest

// Construct sequence anchor pattern
//	('in_ex' ? inclusive : exclusive)
// 	('origin' ? ending : starting)
static X_Pattern *rx_CBound (Expr *args, Prefix *where, bool in_ex, bool origin) {
Prefix *c_pred = where->expect_prefix (args);
return c_pred ? new ("Pattern/CBound") Pattern_CBound (c_pred, in_ex, origin) : 0;
}	// rx_CBound

// Construct position anchor pattern
//	('origin' ? from end : from start)
static X_Pattern *rx_Offset (Expr *args, Prefix *where, bool origin) {
unsigned offset = where->expect_fixed (args, 0);
return new ("Pattern/Offset") Pattern_Offset (origin, offset);
}	// rx_Offset

// Construct string behind/ahead assertion pattern
//	('origin' ? string behind : string ahead)
//	('polarity' ? present : not present)
static X_Pattern *rx_StrBound (Expr *args, Prefix *where, bool polarity, bool origin) {
S_string string;
if (where->expect_string (args, string)) {
	X_Pattern *pattern = new ("Pattern/StrBound") Pattern_StringBound (string, polarity, origin);
	string.relink ();
	return pattern;
	}

return (X_Pattern *) 0;
}	// rx_StrBound

// Construct alternation pattern
static X_Pattern *rx_Alt (Expr *args, Prefix *where) {
X_Pattern *pattern = where->expect_pattern (get_arg (args));
return new ("Pattern/Alt") Pattern_Alt (pattern, where->expect_pattern (args));
}	// rx_Alt

// Construct concatenation pattern
static X_Pattern *rx_Cat (Expr *args, Prefix *where) {
X_Pattern *pattern = where->expect_pattern (get_arg (args));
return new ("Pattern/Cat") Pattern_Cat (pattern, where->expect_pattern (args));
}	// rx_Cat

// Construct repetition pattern
static X_Pattern *rx_Rep (Expr *args, Prefix *where) {
FixedRange range (0, -1);
where->expect_range (get_arg (args), range);
bool dir = where->expect_bool (get_arg (args), false);
return new ("Pattern/Rep") Pattern_Rep (where->expect_pattern (args), range, dir);
}	// rx_Rep

// Construct (incremental/decremental) repetition pattern
static X_Pattern *rx_RepDir (Expr *args, Prefix *where, bool dir) {
FixedRange range (0, -1);
where->expect_range (get_arg (args), range);
return new ("Pattern/Rep") Pattern_Rep (where->expect_pattern (args), range, dir);
}	// rx_RepDir

// Construct repetition pattern (saving repeat count)
static X_Pattern *rx_RepSv (Expr *args, Prefix *where) {
Expr *mut_repeat = get_arg (args);
FixedRange range (0, -1);
where->expect_range (get_arg (args), range);
bool dir = where->expect_bool (get_arg (args), false);
return new ("Pattern/Rep") Pattern_Rep (mut_repeat, where->expect_pattern (args), range, dir);
}	// rx_RepSv

// Construct register storage pattern
static X_Pattern *rx_RegStore (Expr *args, Prefix *where) {
unsigned reg_no = where->expect_fixed (get_arg (args), 0);
return new ("Pattern/Store") Pattern_Store (where->expect_pattern (args), reg_no, 0);
}	// rx_RegStore

// Construct register storage pattern, with mutable
static X_Pattern *rx_RegStoreTo (Expr *args, Prefix *where) {
Expr *mut_storage = get_arg (args);
unsigned reg_no = where->expect_fixed (get_arg (args), 0);
return new ("Pattern/Store") Pattern_Store (where->expect_pattern (args), reg_no, mut_storage);
}	// rx_RegStoreTo

// Construct register recall pattern
static X_Pattern *rx_RegRecall (Expr *args, Prefix *where) {
unsigned reg_no = where->expect_fixed (args, 0);
return new ("Pattern/Recall") Pattern_Recall (reg_no);
}	// rx_RegRecall

// Construct denullificator
static X_Pattern *rx_Denull (Expr *args, Prefix *where) {
X_Pattern *pattern = where->expect_pattern (args);
return new ("Pattern/Denull") Pattern_Denull (pattern);
}	// rx_Denull

// Construct pattern to evaluate something
static X_Pattern *rx_Eval (Expr *args, Prefix *where) {
Expr *do_eval = get_arg (args);
return new ("Pattern/Evaluate") Pattern_Eval (do_eval);
}	// rx_Eval

//
//	Pattern constructor
//

// Basic typedefs for pattern constructor

// (no booleans):
typedef X_Pattern * (*rx_ctor0) (Expr *args, Prefix *where);

// (one boolean):
typedef X_Pattern * (*rx_ctor1) (Expr *args, Prefix *where, bool opt_0);

// (two booleans):
typedef X_Pattern * (*rx_ctor2) (Expr *args, Prefix *where, bool opt_0, bool opt_1);

struct P_RX_Create : Prefix {
	unsigned options;			// (set of flags: see below)
	union {
		rx_ctor0 opt_0;			// no options
		rx_ctor1 opt_1;			// one option
		rx_ctor2 opt_2;			// two options
		} dispatch;			// (depends from options...)

	// Constructor (#0)
	P_RX_Create (char const *ident, rx_ctor0 ctor_0) :
		Prefix (ident, Op_Null)
			{ dispatch.opt_0 = ctor_0; options = 1; }

	// Constructor (#0 / with opcode)
	P_RX_Create (char const *ident, O_Enum opcode, rx_ctor0 ctor_0) :
		Prefix (ident, opcode)
			{ dispatch.opt_0 = ctor_0; options = 1; }

	// Constructor (#1)
	P_RX_Create (char const *ident, rx_ctor1 ctor_1, bool opt_0) :
		Prefix (ident, Op_Null)
			{ dispatch.opt_1 = ctor_1; options = 2 | (opt_0); }

	// Constructor (#1 / with opcode)
	P_RX_Create (char const *ident, O_Enum opcode, rx_ctor1 ctor_1, bool opt_0) :
		Prefix (ident, opcode)
			{ dispatch.opt_1 = ctor_1; options = 2 | (opt_0); }

	// Constructor (#2)
	P_RX_Create (char const *ident, rx_ctor2 ctor_2, bool opt_0, bool opt_1) :
		Prefix (ident, Op_Null)
			{ dispatch.opt_2 = ctor_2; options = 4 | (opt_1 << 1) | (opt_0); }

	// Constructor (#2 / with opcode)
	P_RX_Create (char const *ident, O_Enum opcode, rx_ctor2 ctor_2, bool opt_0, bool opt_1) :
		Prefix (ident, opcode)
			{ dispatch.opt_2 = ctor_2; options = 4 | (opt_1 << 1) | (opt_0); }

	// Evaluation: dispatch to real constructors
	VType evalV (VDatum &val, Expr *args) {
		val._pattern =
			options & 4 ?		// (2 booleans)
				dispatch.opt_2 (args, this, (options & 1), (options & 2) >> 1) :
			options & 2 ?		// (1 booleans)
				dispatch.opt_1 (args, this, (options & 1)) :
								// (no booleans)
				dispatch.opt_0 (args, this);

		return T_pattern;
		}	// evalV
	};

//
//	Calculating pattern possible range
//

struct P_RX_Length : PrefixX {
	P_RX_Length (char const *ident) : PrefixX (ident, Op_Null) {}

	D_Prefix_evalX;
	};

Expr *P_RX_Length::evalX (Expr *args) {
X_Pattern *pattern = expect_pattern (args);

if (pattern) {
	FixedRange range;
	bool infinite = pattern->_predict (range);
	relink_expr (pattern);

	return new ("RX/Length") X_List
		(new ("RX/Range.from") X_Fixed (range.from),
		infinite ? 0 : new ("RX/Range.to") X_Fixed (range.to));
	}	// (pattern)

return UNDEF;
}	// P_RX_Length::evalX

//
//	Enumerate patterns
//

struct P_RX_Enum : Prefix {
	P_RX_Enum (char const *ident) : Prefix (ident, Op_Null) {}

	D_Prefix_evalV;
	};

void call_it (X_Pattern *self, X_Pattern *parent, struct Matcher &context) {
// TODO: call enumerator
}	// call_it

VType P_RX_Enum::evalV (VDatum &val, Expr *args) {
X_Pattern *pattern = expect_pattern (get_arg (args));

if (pattern) {
	S_string unused;
	Matcher _context (unused);
	pattern->_enum (_context, 0, call_it);
	}	// (pattern)

return T_undef;
}	// P_RX_Enum::evalV

//
//	Matching pattern
//

struct P_RX_Match : Prefix {
	P_RX_Match (char const *ident) : Prefix (ident, Op_Null) {}

	D_Prefix_evalV;
	};

VType P_RX_Match::evalV (VDatum &val, Expr *args) {
X_Pattern *pattern = expect_pattern (get_arg (args));
S_string str;

if (expect_string (args, str)) {
	unsigned length;

	val._fixed = test_match (str, pattern, length) ? (S_fixed) length : -1;

	relink_expr (pattern);
	str.relink ();

	return T_fixed;
	}

return T_undef;
}	// P_RX_Match::evalV

//
//	Finding pattern in string
//

struct P_RX_Search : PrefixX {
	bool direction;

	P_RX_Search (char const *ident, bool dir) : PrefixX (ident, Op_Null)
		{ direction = dir; }

	D_Prefix_evalX;
	};

Expr *P_RX_Search::evalX (Expr *args) {
X_Pattern *pattern = expect_pattern (get_arg (args));

S_string str;

if (expect_string (args, str)) {
	FixedRange range;
	bool found = find_match (str, pattern, direction, range);

	relink_expr (pattern);
	str.relink ();

	return found ? range.eval () : 0;
	}

return UNDEF;
}	// P_RX_Search::evalV

//
//	Pattern ranges list constructor
//

struct MatchListBuilder : MatchList {
	unsigned skip;
	Expr *result;

	MatchListBuilder (S_string &string, X_Pattern *pattern, FixedRange &range) :
		MatchList (string, pattern, range.to)
			{ result = 0; skip = range.from; }

	// Finished recursion:
	// create list to 'result'
	bool complete (bool dir) {
		result = 0;

		unsigned count = total ();
		if (skip < count) count -= skip;
		else return false;

		for (MatchDef *match = last; count --; match = match->prev)
			result = new ("List/matcher") X_List (match->where.eval (), result);
		
		return true;
		}
	};	// MatchListBuilder

struct P_RX_Locate : PrefixX {
	P_RX_Locate (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
	X_Pattern *pattern = expect_pattern (get_arg (args));

	// TODO: fix problem, when "source" is constructed????
	S_string source;

	if (expect_string (get_arg (args), source)) {
		FixedRange min_max;
		expect_range (get_arg (args), min_max);
		bool dir = expect_bool (get_arg (args), false);

		MatchListBuilder list_builder (source, pattern, min_max);
		Matcher context (source);

		bool found = dir ? list_builder.build_dec (context) : list_builder.build_inc (context);

		relink_expr (pattern);
		source.relink ();

		return list_builder.result;
		}

	return UNDEF;
	}	// evalX

	};

//
//	Split string by pattern matches list
//

struct MatchListSplitter : MatchList {
	unsigned skip;
	S_string source;
	Expr *&result;

	MatchListSplitter (S_string &string, X_Pattern *pattern, FixedRange &range, Expr *&result) :
		MatchList (string, pattern, range.to), result (result)
			{ skip = range.from; source = string; }

	// Add string fragment source$[from..to] node to 'result'
	void add_list_node (S_fixed from, S_fixed to) {
		X_String *strnode = source.cons ();
		strnode->offset += from;
		strnode->length = to - from;

		if (result)
			result = new ("List/split") X_List (strnode, result);
		else
			result = strnode;
		}	// add_list_node

	// Finished recursion: split string to array
	// Returns false, when no matches
	bool complete (bool dir) {
		unsigned src_len;
		C_type src_type;
		source.fetch (src_len, src_type);

		result = 0;

		unsigned count = total ();
		if (skip < count) count -= skip;
		else { add_list_node (0, src_len); return false; }

		if (dir) {
			// (split matches incrementally)
			unsigned prev = 0;
			for (MatchDef *match = last; count --; match = match->prev) {
				FixedRange &range = match->where;
				add_list_node (prev, range.from);
				prev = range.to;
				}	// for (match)
			// (final (last) split)
			add_list_node (prev, src_len);
			}
		else {
			// (split matches decrementally)
			unsigned next = src_len;
			for (MatchDef *match = last; count --; match = match->prev) {
				FixedRange &range = match->where;
				add_list_node (range.to, next);
				next = range.from;
				}	// for (match)
			// (final (first) split)
			add_list_node (0, next);
			}

		return true;
		}	// complete
	};	// MatchListSplitter

struct P_RX_Split : PrefixX {
	P_RX_Split (char const *ident) : PrefixX (ident, Op_Null) {}

	D_Prefix_evalX;
	};

Expr *P_RX_Split::evalX (Expr *args) {
	X_Pattern *pattern = expect_pattern (get_arg (args));

	S_string source;

	if (expect_string (get_arg (args), source)) {
		FixedRange min_max;
		expect_range (get_arg (args), min_max);
		bool dir = expect_bool (get_arg (args), false);
		Expr *result;

		MatchListSplitter splitter (source, pattern, min_max, result);
		Matcher context (source);

		bool found = dir ? splitter.build_dec (context) : splitter.build_inc (context);

		relink_expr (pattern);

		/* if (found) */ source.relink ();
		// (otherwise, result is identical to source)

		return result;
		}

	return 0;
}	// P_RX_Split::evalX

//
//	Replace pattern matches
//

struct MatchListReplacer : MatchList {
	unsigned skip;
	S_string source, &replace, &result;

	MatchListReplacer (S_string &string, X_Pattern *pattern,
			FixedRange &range, S_string &replace, S_string &result) :
		MatchList (string, pattern, range.to), replace (replace), result (result)
			{ skip = range.from; source = string; }

	// Finished recursion: create replacement string
	bool complete (bool dir) {
		unsigned src_len, repl_len;
		C_type src_type, repl_type;
		str_ptr
			src_ptr = source.fetch (src_len, src_type),
			repl_ptr = replace.fetch (repl_len, repl_type);

		unsigned count = total ();
		if (skip < count) count -= skip;
		else {		// (no matches to replace!)
			result.s_copy (source);
			return false;
			}

		unsigned range_total = 0;
		for (MatchDef *match = last; count --; match = match->prev)
			range_total += match->where.count();

		count = total () - skip;
		unsigned res_len = src_len - range_total + count * repl_len;
		str_ptr res_ptr = result.alloc ("String/replace", res_len, src_type);

		// TODO: use smart copy instead!
		if (dir) {
			// (replace matches incrementally)
			unsigned prev = 0;
			for (MatchDef *match = last; count --; match = match->prev) {
				FixedRange &range = match->where;
				unsigned count = range.from - prev;
				s_mcopy (res_ptr, src_ptr, count, src_type);
				s_forward (res_ptr, count, src_type);
				s_forward (src_ptr, range.to - prev, src_type);
				s_mcopy (res_ptr, repl_ptr, repl_len, src_type);
				s_forward (res_ptr, repl_len, src_type);
				prev = range.to;
				}	// for (match)
			// (final (last) replacement)
			s_mcopy (res_ptr, src_ptr, src_len - prev, src_type);
			}
		else {
			// (replace matches decrementally)
			unsigned next = src_len;
			s_forward (res_ptr, res_len, src_type);
			s_forward (src_ptr, src_len, src_type);
			for (MatchDef *match = last; count --; match = match->prev) {
				FixedRange &range = match->where;
				unsigned count = next - range.to;
				s_backward (res_ptr, count, src_type);
				s_backward (src_ptr, count, src_type);
				s_mcopy (res_ptr, src_ptr, count, src_type);
				s_backward (src_ptr, range.count(), src_type);
				s_backward (res_ptr, repl_len, src_type);
				s_mcopy (res_ptr, repl_ptr, repl_len, src_type);

				next = range.from;
				}	// for (match)
			// (final (first) replacement)
			s_backward (res_ptr, next, src_type);
			s_backward (src_ptr, next, src_type);
			s_mcopy (res_ptr, src_ptr, next, src_type);
			}
		}	// complete
	};	// MatchListReplacer

struct P_RX_Replace : Prefix {
	P_RX_Replace (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Pattern *pattern = expect_pattern (get_arg (args));

	S_string source, replace;

	if (expect_string (get_arg (args), source) &&
		expect_string (get_arg (args), replace)) {
		FixedRange min_max;
		expect_range (get_arg (args), min_max);
		bool dir = expect_bool (get_arg (args), false);

		MatchListReplacer replacer (source, pattern, min_max, replace, val._string);
		Matcher context (source);

		bool found = dir ? replacer.build_dec (context) : replacer.build_inc (context);

		relink_expr (pattern);

		if (found) source.relink ();
		// (otherwise, result is identical to source)

		replace.relink ();

		return T_string;
		}

	return T_fixed;
	}	// evalV

	};

//
//	Initialisation
//

static bool init_primaries_pattern (int order) {

//		[Categories]

//^C	Pattern
//^B	Pattern functors
//^D	Functors, operating on patterns.

//		[Types]

//^T	Pattern
//^B	Pattern value.
//^D	Anything evaluating to pattern.
//^D	(Allows string, which is implicitly converted to string pattern.)
//^D	(Reports error, if argument is not pattern.)

//		[Errors]

//^E	ExpectPattern
//^B	Pattern operand expected.
//^D	Expected operand, evaluating to pattern.

//		--------

//^N	is_pattern [Predicate | Pattern]
//^P	is_pattern (V: Any) => Bool
//^B	Check for pattern value.
//^D	Predicate: !true, if argument \V evaluates to pattern.

	DefBuiltin (P_IsType ("is_pattern", Op_Null, T_pattern));

//^N	expect_pattern [Wrapper | Pattern]
//^P	expect_pattern (V: Any, @Body: Any) => Any
//^B	Expect pattern value.
//^D	If argument \V evaluates to pattern, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_pattern", Op_Null, T_pattern));

//^G	rx_null

//^N	rx_null [Constructor | Pattern]
//^P	rx_null () => Pattern
//^B	Null pattern.
//^D	Construct pattern, matching empty string anywhere.

	DefBuiltin (P_RX_Create ("rx_null", rx_Null));

//^G	rx_char rx_string

//^N	rx_char [Constructor | Pattern]
//^P	rx_char (Code: Int) => Pattern
//^B	Literal character pattern.
//^D	Construct pattern, matching literal character with code \Code.

	DefBuiltin (P_RX_Create ("rx_char", rx_Char));

//^N	rx_string [Constructor | Pattern]
//^P	rx_string (Text: String) => Pattern
//^B	Literal string pattern.
//^D	Construct pattern, matching literal string \Text.

	DefBuiltin (P_RX_Create ("rx_string", rx_String));

//^G	rx_any rx_any_in rx_any_ex

//^N	rx_any [Constructor | Pattern]
//^P	rx_any () => Pattern
//^B	Any character pattern.
//^D	Construct pattern, matching single arbitrary character.

	DefBuiltin (P_RX_Create ("rx_any", rx_Any));

//^N	rx_any_in [Constructor | Pattern]
//^P	rx_any_in (CPred: Func) => Pattern
//^B	Character predicate check (inclusive).
//^D	Construct pattern, matching any character, for which application of \CPred results in !true.

//^N	rx_any_ex [Constructor | Pattern]
//^P	rx_any_ex (CPred: Func) => Pattern
//^B	Character predicate check (exclusive).
//^D	Construct pattern, matching any character, for which application of \CPred results in !false.

	DefBuiltin (P_RX_Create ("rx_any_in", rx_CTest, true));
	DefBuiltin (P_RX_Create ("rx_any_ex", rx_CTest, false));

//^G	rx_before_in rx_before_ex rx_after_in rx_after_ex

//^N	rx_before_in [Constructor | Pattern]
//^P	rx_before_in (CPred: Func) => Pattern
//^B	Character predicate sequence start (inclusive).
//^D	Construct pattern, matching start of characters sequence, for which application of \CPred results in !true.

//^N	rx_before_ex [Constructor | Pattern]
//^P	rx_before_ex (CPred: Func) => Pattern
//^B	Character predicate sequence start (exclusive).
//^D	Construct pattern, matching start of characters sequence, for which application of \CPred results in !false.

//^N	rx_after_in [Constructor | Pattern]
//^P	rx_after_in (CPred: Func) => Pattern
//^B	Character predicate sequence end (inclusive).
//^D	Construct pattern, matching end of characters sequence, for which application of \CPred results in !true.

//^N	rx_after_ex [Constructor | Pattern]
//^P	rx_after_ex (CPred: Func) => Pattern
//^B	Character predicate sequence end (exclusive).
//^D	Construct pattern, matching end of characters sequence, for which application of \CPred results in !false.

	DefBuiltin (P_RX_Create ("rx_before_in", rx_CBound, true, false));
	DefBuiltin (P_RX_Create ("rx_before_ex", rx_CBound, false, false));
	DefBuiltin (P_RX_Create ("rx_after_in", rx_CBound, true, true));
	DefBuiltin (P_RX_Create ("rx_after_ex", rx_CBound, false, true));

//^G	rx_at_start rx_at_end

//^N	rx_at_start [Constructor | Pattern]
//^P	rx_at_start (Offset: Int) => Pattern
//^B	Start position anchor.
//^D	Construct pattern, matching at offset \Offset relative to beginning.

//^N	rx_at_end [Constructor | Pattern]
//^P	rx_at_end (Offset: Int) => Pattern
//^B	End position anchor.
//^D	Construct pattern, matching at offset \Offset relative to end.

	DefBuiltin (P_RX_Create ("rx_at_start", rx_Offset, false));
	DefBuiltin (P_RX_Create ("rx_at_end", rx_Offset, true));

//^G	rx_is_before rx_is_after rx_not_before rx_not_after

//^N	rx_is_before [Constructor | Pattern]
//^P	rx_is_before (ContextBefore: String) => Pattern
//^B	Look-before positive assertion anchor.
//^D	Construct pattern, matching empty string preceded by \ContextBefore (which is NOT included in match).

//^N	rx_not_before [Constructor | Pattern]
//^P	rx_not_before (ContextBefore: String) => Pattern
//^B	Look-before negative assertion anchor.
//^D	Construct pattern, matching empty string NOT preceded by \ContextBefore.

	DefBuiltin (P_RX_Create ("rx_is_before", rx_StrBound, true, false));
	DefBuiltin (P_RX_Create ("rx_not_before", rx_StrBound, false, false));

//^N	rx_is_after [Constructor | Pattern]
//^P	rx_is_after (ContextAfter: String) => Pattern
//^B	Look-after positive assertion anchor.
//^D	Construct pattern, matching empty string followed by \ContextAfter (which is NOT included in match).

//^N	rx_not_after [Constructor | Pattern]
//^P	rx_not_after (ContextAfter: String) => Pattern
//^B	Look-after negative assertion anchor.
//^D	Construct pattern, matching empty string NOT followed by \ContextAfter.

	DefBuiltin (P_RX_Create ("rx_is_after", rx_StrBound, true, true));
	DefBuiltin (P_RX_Create ("rx_not_after", rx_StrBound, false, true));

//^G	rx_alt rx_cat rx_rep

//^N	rx_alt [Constructor | Pattern]
//^P	rx_alt (First: Pattern, Second: Pattern) => Pattern
//^B	Alternation of patterns.
//^D	Construct pattern, matching either \First or \Second.

	DefBuiltin (P_RX_Create ("rx_alt", Op_RXAlt, rx_Alt));

//^N	rx_cat [Constructor | Pattern]
//^P	rx_cat (Head: Pattern, Tail: Pattern) => Pattern
//^B	Concatenation of patterns.
//^D	Construct pattern, matching concatenation of \Head and \Tail.

	DefBuiltin (P_RX_Create ("rx_cat", Op_RXCat, rx_Cat));

//^N	rx_rep [Constructor | Pattern]
//^P	rx_rep (Range: Range, Greedy: Bool, Body: Pattern) => Pattern
//^B	Repetition of pattern.
//^D	Construct pattern, matching \Body, repeated minimum Range[0] and maximum Range[1] times.
//^D	If \Greedy is !true, attempts to find maximum number of repetitions (tries to find minimum otherwise).

	DefBuiltin (P_RX_Create ("rx_rep", rx_Rep));
	DefBuiltin (P_RX_Create ("rx_rep_count", rx_RepSv));

//^N	rx_rep_inc [Constructor | Pattern]
//^P	rx_rep_inc (Range: Range, Body: Pattern) => Pattern
//^B	Repetition of pattern (incremental).
//^D	Construct pattern, matching \Body, repeated minimum Range[0] and maximum Range[1] times (incrementally).

//^N	rx_rep_dec [Constructor | Pattern]
//^P	rx_rep_dec (Range: Range, Body: Pattern) => Pattern
//^B	Repetition of pattern (decremental).
//^D	Construct pattern, matching \Body, repeated minimum Range[0] and maximum Range[1] times (decrementally).

	DefBuiltin (P_RX_Create ("rx_rep_inc", Op_RXRepInc, rx_RepDir, false));
	DefBuiltin (P_RX_Create ("rx_rep_dec", Op_RXRepDec, rx_RepDir, true));

//^G	rx_fail

//^N	rx_fail [Constructor | Pattern]
//^P	rx_fail () => Pattern
//^B	Failure pattern.
//^D	Construct pattern, always failing to match anything.

	DefBuiltin (P_RX_Create ("rx_fail", rx_Fail));

//
//
//

	DefBuiltin (P_RX_Create ("rx_denull", rx_Denull));

//^G	rx_store rx_recall

//^N	rx_store [Constructor | Pattern]
//^P	rx_store (RegNo: Int, Body: Pattern) => Pattern
//^B	Register store pattern.
//^D	Construct pattern, matching \Body (and storing successful match to register \RegNo).

//^N	rx_recall [Constructor | Pattern]
//^P	rx_recall (RegNo: Int) => Pattern
//^B	Register recall pattern.
//^D	Construct pattern, matching context previously stored (by rx_store) in register \RegNo.

	DefBuiltin (P_RX_Create ("rx_store", rx_RegStore));
	DefBuiltin (P_RX_Create ("rx_recall", rx_RegRecall));

//^N	rx_store_to [Constructor | Pattern]
//^P	rx_store_to (Var: Mut, RegNo: Int, Body: Pattern) => Pattern
//^B	Register store pattern (with storage mutable).
//^D	Construct pattern, matching \Body (and storing successfull match to register \RegNo).
//^D	On match success, match range is stored to \Var.

	DefBuiltin (P_RX_Create ("rx_store_to", rx_RegStoreTo));

//
//	TODO
//

	DefBuiltin (P_RX_Create ("rx_eval", rx_Eval));


//^N	rx_length [Pattern]
//^P	rx_length (Context: Pattern) => Range
//^B	Calculate length range of pattern.
//^D	Returns range of context lengths, which pattern \Context may match.
//^D	(If context length is unlimited, Range[1] is undefined.)

	DefBuiltin (P_RX_Length ("rx_length"));

//^G	rx_match rx_findfirst rx_findlast

//^N	rx_match [String | Pattern]
//^P	rx_match (Context: Pattern, Str: String) => Int
//^B	Pattern matching.
//^D	Match \Str with pattern \Context.
//^D	Return length of pattern matched (or -1, if match failed).

	DefBuiltin (P_RX_Match ("rx_match"));

//^N	rx_findfirst [String | Pattern]
//^P	rx_findfirst (Context: Pattern, Str: String) => Range
//^B	Search forward for pattern.
//^D	Find first pattern matching \Context in string \Str.
//^D	Returns offset range, where pattern is found (or !undef, if no pattern found).

//^N	rx_findlast [String | Pattern]
//^P	rx_findlast (Context: Pattern, Str: String) => Range
//^B	Search backward for pattern.
//^D	Find last pattern matching \Context in string \Str.
//^D	Returns offset range, where pattern is found (or !undef, if no pattern found).

	DefBuiltin (P_RX_Search ("rx_findfirst", false));
	DefBuiltin (P_RX_Search ("rx_findlast", true));

//^G	rx_locate rx_split rx_replace

//^N	rx_split [String | Pattern]
//^P	rx_split (Divider: Pattern, Source: String, Range: Range, Direction: Bool) => List
//^B	Split string by pattern.
//^D	Split string \Source (at places where pattern \Divider is matched).
//^D	Argument \Range defines range of occurences, \Direction defines order (from start/from end).

	DefBuiltin (P_RX_Split ("rx_split"));

//^N	rx_locate [String | Pattern]
//^P	rx_locate (Context: Pattern, Source: String, Range: Range, Direction: Bool) => List
//^B	Locate set of patterns in string.
//^D	Find subset of occurences of \Context in \Source.
//^D	Argument \Range defines range of occurences, \Direction defines order (from start/from end).

	DefBuiltin (P_RX_Locate ("rx_locate"));

//^N	rx_replace [String | Pattern]
//^P	rx_replace (Context: Pattern, Source: String, Replacer: String, Range: Range, Direction: Bool) => String
//^B	Replace set of patterns in string.
//^D	Find subset of occurences of \Context in \Source, replace them with \Replacer, and return result.
//^D	Argument \Range defines range of occurences, \Direction defines order (from start/end).

	DefBuiltin (P_RX_Replace ("rx_replace"));

// TODO...

	DefBuiltin (P_RX_Enum ("rx_enum"));

return true;
}	// init_primaries_pattern

DefSubSystem ("pattern", init_primaries_pattern, 0);

#endif
