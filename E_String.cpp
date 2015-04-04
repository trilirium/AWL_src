
/*

	+---+---+---+---+---+---+
	|	"E_String.cpp":
	|	String scalar primaries.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include <cstring>

#include "Eval.h"

#include "Logger.h"

#ifdef TARGET_UNIX
#include <cstdio>
#endif

#include "String.h"

#define HEADER

#include "E_Stream.cpp"

#include "Unicode.cpp"

#undef HEADER

// Set S_string (from 'str')
DL_EXPORT void S_string::s_set (X_String *str) {
content = str->content;
offset = str->offset, length = str->length;
}	// S_string::s_set

// Clear entire string
DL_EXPORT void S_string::s_clear () {
content = 0;
offset = length = 0;
}	// S_string::s_clear

// Copy S_string (from 'source')
DL_EXPORT void S_string::s_copy (S_string &source) {
content = source.content;
length = source.length;
offset = source.offset;
}	// S_string::s_copy

// Convert self to C-string (null-terminated)
DL_EXPORT char *S_string::to_cstring () {
unsigned len;
C_type type;
str_ptr src = fetch (len, type);

// (Impossible to convert not type0 string!)
if (type) return 0;

char *res = new ("C/String") char [len + 1];
memcpy (res, src, len);
res [len] = '\0';
return res;
}	// S_string::to_cstring

// Copy to fixed-size buffer as Cstring
// (Returns true, if no overflow)
DL_EXPORT bool S_string::to_buffer (char *buffer, unsigned buflen) {
unsigned len;
C_type type;
str_ptr src = fetch (len, type);

// Impossible to convert not type0 string!
if (type) return false;

bool res = len < buflen;
if (! res) len = buflen - 1;
memcpy (buffer, src, len);
buffer [len] = '\0';

return res;
}	// S_string::to_buffer

// Construct new string from self
X_String *S_string::cons () {
X_String *string = new ("String/cons") X_String (*this);
string->offset = offset; string->length = length;			// ?????
return string;
}	// S_string::cons

// X_String evaluation
VType X_String::evalV (VDatum &val, bool full) {
val._string.s_set (this);
return T_string;
}	// X_String::evalV

// X_String evaluation
Expr *X_String::evalX () {
return this;
}	// X_String::evalX

static unsigned factorX = 7103, factorY = 2393, factorZ = 1009;

// String hash code
// Warning: hash code not affected by CP!
unsigned X_String::hash () {
unsigned len;
C_type type;
str_ptr src = fetch (len, type);

unsigned hash = len * factorZ;

while (len --)
	hash ^= factorX*s_read_inc (type, src) + factorY*hash;

return hash;
}	// X_String::hash

// String identity test
// Warning: string identity not affected by CP!
bool X_String::identV (VType type, VDatum &val) {
if (type == T_string) {
unsigned v_len;
C_type v_type;
str_ptr v_ptr = val._string.fetch (v_len, v_type);

if (v_len == length)
	if (v_len) {
		unsigned s_len;
		C_type s_type;
		str_ptr s_ptr = fetch (s_len, s_type);
		return s_type == v_type &&
			s_mcomp (s_ptr, v_ptr, v_len, v_type) == 0;
		}
else	// empty strings are equal
	return true;
}

return false;
}	// X_String::identV

//
//	Output expressions
//

static void output_str (Stream *out, str_ptr source, C_type type, unsigned len) {
if (type) {
	while (len --)
		out->put_wch (UC_read_inc (type, source));
	}
else
	out->put_data ((char *) source, len);
}	// output_str

void S_string::put (Stream *out) {
unsigned len;
C_type type;
str_ptr source = fetch (len, type);
output_str (out, source, type, len);
}	// S_string::put

unsigned X_String::put (Stream *out) {
unsigned len;
C_type type;
str_ptr source = fetch (len, type);
output_str (out, source, type, len);
return 1;
}	// X_String::put

#define	HEADER

#include "Coerce.cpp"

#undef HEADER

//
//	Accumulate output to string
//

struct FormatString : FormatGen {
	char *buffer;
	unsigned len, size;

	FormatString (char *buffer, unsigned size) {
		this->buffer = buffer;
		len = 0, this->size = size;
		}

	// Initialise S_string from accumulator
	void cons (S_string &s_str, C_type type, char const *tag) {
		str_ptr ptr = s_str.alloc (tag, len, type);
		// (TTT: if type != 0 ???)
		if (len) memcpy (ptr, buffer, len);
		}	// cons

	// TODO: make extendable accumulator...

	// (Put character 'ch')
	void putc (char ch) {
		if (len != size) buffer[len ++] = ch;
		}	// putc

	// (Put C-string 'str')
	void puts (char const *str) {
		unsigned slen = strlen(str);
		if (len + slen <= size) {
			memcpy (buffer + len, str, slen);
			len += slen;
			}
		}	// puts

	// (Put repeated character 'ch'*['count'])
	void putr (char ch, unsigned count) {
		if (len + count <= size) {
			memset (buffer + len, ch, count);
			len += count;
			}
		}	// putr

	// (Put fragment from memory 'src'['len'])
	void putm (char *src, unsigned cnt) {
		if (len + cnt <= size) {
			memcpy (buffer + len, src, cnt);
			len += cnt;
			}
		}	// putm

	};

// Format to stream
struct FormatOutput : FormatGen {
	Stream *stream;

	FormatOutput (Stream *stream) {
		this->stream = stream;
		}

	// (Put character 'ch')
	void putc (char ch) {
		stream->put_wch (ch);
		}	// putc

	// (Put C-string 'str')
	void puts (char const *str) {
		stream->put_cstr (str);
		}	// puts

	// (Put repeated character 'ch'*['count'])
	void putr (char ch, unsigned count) {
		while (count --) stream->put_wch (ch);
		}	// putr

	// (Put fragment from memory 'src'['len'])
	void putm (char *src, unsigned cnt) {
		while (cnt --) stream->put_wch (*src ++);
		}	// putm

	};

// Standart coercions

static void _coerce_fixed (FormatGen &formatter, S_fixed fixed_val)
	{ formatter.format_signed (fixed_val, 10, false); }

static void _coerce_float (FormatGen &formatter, S_float float_val)
	{ formatter.format_float (float_val, FC_format_g, 8, 1); }

// Put fixed value to stream
DL_EXPORT void Stream::put_fixed (S_fixed fixed_val) {
FormatOutput out (this);
_coerce_fixed (out, fixed_val);
}	// Stream::put_fixed

// Put float value to stream
DL_EXPORT void Stream::put_float (S_float float_val) {
FormatOutput out (this);
_coerce_float (out, float_val);
}	// Stream::put_float

// String coercion (from ANY scalar type)
// Resulting type is always T_string
void coerce_string (VType type, VDatum &val) {
if (type != T_string) {
	char _buffer [40];
	FormatString _sf (_buffer, sizeof(_buffer));

	switch (type) {
		case T_fixed:
			_coerce_fixed (_sf, val._fixed);
			break;

		case T_float:
			_coerce_float (_sf, val._float);
			break;

		default:
			return;			// string already, nothing to do
		}

	_sf.cons (val._string, 0, "Num/coerce");
	}
}	// coerce_string

// Coerce to float from scalar type 'type'
X_String *to_string (VType type, VDatum &val) {
coerce_string (type, val);
return val._string.cons ();
}	// to_string

//	Generic input buffer for coercion
struct ParseString : ParseGen {
	// (string info)
	C_type type;			// (type of string)
	unsigned len;			// (length of string)
	str_ptr base;			// (base pointer)

	ParseString (S_string &str) {
		base = str.fetch (len, type);
		}

	// (returns 0 on end of parse buffer)
	unsigned get_ch () {
		if (len) { -- len; return UC_read_inc (type, base); }
		return 0;
		}	// get_ch

	};

//
//	Expect string type (0, 1 or 2)
//

S_fixed check_string_type (S_fixed type) {
// ??? Report error on non-valid type?

return type <= 0 ? 0 : type >= 2 ? 2 : 1;
}	// check_string_type

//
//	Report different-typed strings
//	(now obsolete)
//

// Character types mismatch
struct CharTypeError : ExecError {
	S_string &left, &right;
	Prefix *where;

	CharTypeError (S_string &left, S_string &right, Prefix *where) : left(left), right(right)
		{ this->where = where; }

	void _report (Logger &log) {
		log.put_cstr ("String types mismatch");
		if (where) {
			log.put_cstr (" in ");
			log.log_prefix (where);
			}
		log.put_cstr (": ");
		left.log (log);
		log.put_cstr (" <> ");
		right.log (log);
		}	// _report

	};

static void string_mismatch (S_string &s_left, S_string &s_right, Prefix *where) {
Module::report (new CharTypeError (s_left, s_right, where));
}	// string_mismatch

//
//	Copy different-type blocks
//	(with possible conversion)
//

// Find greater character type of (type_1, type_2)
#define s_common_type(type_1,type_2) (type_1 > type_2 ? type_1 : type_2)

// Copy characters with (possible) conversion
#define	s_cvt_copy(count,to_ptr,to_type,from_ptr,from_type)						\
	(to_type == from_type ? (void)s_mcopy(to_ptr, from_ptr, count, to_type) :	\
	 convert_chars(count, to_ptr, to_type, from_ptr, from_type))

// Compare string data
int s_cvt_compare (unsigned count,
	str_ptr p_left, C_type t_left, str_ptr p_right, C_type t_right) {

// TODO: equal compare?

// TODO:
// respect codemapping on need...

while (count --) {
	C_long left = UC_read_inc (t_left, p_left);
	C_long right = UC_read_inc (t_right, p_right);

	if (left != right)
		return left < right ? -1 : 1;
	}

return 0;
}	// s_cvt_compare

// Convert character buffer
// (assuming to_type != from_type)
static void convert_chars (unsigned count,
	str_ptr to_ptr, C_type to_type, str_ptr from_ptr, C_type from_type) {

// TODO: optimize

while (count --)
	UC_write_inc (to_type, to_ptr, UC_read_inc (from_type, from_ptr));
}	// convert_chars

// Fill character buffer
static void fill_chars (unsigned count, str_ptr to_ptr, C_type to_type, C_long code) {
// TODO: optimize...

if (to_type) {
	while (count --)
		UC_write_inc (to_type, to_ptr, code);
	}
else {
	// TODO: respect codemapping?
	memset (to_ptr, code, count);
	}
}	// fill_chars

//
//	Make string empty
//

struct P_String_Empty : Prefix {
	P_String_Empty (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		val._string.s_clear ();
		return T_string;
		}	// evalV
	};

//
//	Get string length
//

static unsigned len_string (S_string &s_str) {
unsigned len = s_str.length;
s_str.relink ();
return len;
}	// len_string

struct P_String_Len : P_Unary {
	P_String_Len (char const *ident, O_Enum op) : P_Unary (ident, op) {}

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		coerce_string (type1, val1);
		val._fixed = len_string (val1._string);
		return T_fixed;
		}	// eval_Unary
	};	// P_String_Len

//
//	Get string type
//

static unsigned type_string (S_string &s_str) {
unsigned l_str;
C_type t_str;

s_str.fetch (l_str, t_str);
s_str.relink ();
return t_str;
}	// type_string

struct P_String_Type : P_Unary {
	P_String_Type (char const *ident, O_Enum op) : P_Unary (ident, op) {}

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		coerce_string (type1, val1);
		val._fixed = type_string (val1._string);
		return T_fixed;
		}	// eval_Unary
	};	// P_String_Type

//
//	String concatenation
//

// (Returns false on type mismatch error)
static void cat_string (S_string &s_left, S_string &s_right, S_string &s_cat) {
unsigned l_left, l_right;
C_type t_left, t_right;

str_ptr
	p_left = s_left.fetch (l_left, t_left),
	p_right = s_right.fetch (l_right, t_right);

if (! l_left) s_cat.s_copy (s_right);

else if (! l_right) s_cat.s_copy (s_left);

// check for adjanced fragments of same string
else if (p_right == s_offset (p_left, l_left, t_left)) {
	s_cat.s_copy (s_left);
	s_cat.length += l_right;
	}

else {		// (l_left && l_right)
	C_type t_cat = s_common_type (t_left, t_right);
	str_ptr p_cat = s_cat.alloc ("String/cat", l_left + l_right, t_cat);

	s_cvt_copy (l_left, p_cat, t_cat, p_left, t_left);
	s_cvt_copy (l_right, s_offset(p_cat, l_left, t_cat), t_cat, p_right, t_right);

	s_left.relink ();
	s_right.relink ();
	}	// (l_left && l_right)
}	// cat_string

struct P_String_Cat : P_Binary {
	P_String_Cat (char const *ident, O_Enum op) : P_Binary (ident, op) {}

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		coerce_string (type1, val1);
		coerce_string (type2, val2);

		cat_string (val1._string, val2._string, val._string);
		return T_string;
		}	// eval_Binary
	};	// P_String_Cat

//
//	String replication
//

#undef	OVER_COPY

static void rep_string (S_string &s_src, int count, S_string &s_rep) {
if (count <= 0) s_rep.s_clear ();

else if (count == 1)
	{ s_rep.s_copy (s_src); return; }

else {
unsigned l_str;
C_type t_str;
str_ptr p_src = s_src.fetch (l_str, t_str);

if (l_str) {
	str_ptr p_rep = s_rep.alloc ("String/rep", l_str * count, t_str);
	if (l_str == 1)
		fill_chars (count, p_rep, t_str, UC_read(t_str, p_src));

	else {
	// (Output string type is the same, as input)

#ifndef OVER_COPY
	// (use normal copy)
	while (count --) {
		s_mcopy (p_rep, p_src, l_str, t_str);
		s_forward(p_rep, l_str, t_str);
		}
#else
	// (try self-overlapping copy ...)
	s_mcopy (p_rep, p_src, l_str, t_str);
	s_mcopy (s_offset(p_rep, l_str, t_str), p_rep, l_str * (count-1), t_str);
#endif
	}
	}
else s_rep.s_clear ();
}

s_src.relink ();
}	// rep_string

struct P_String_Rep : P_Binary {
	P_String_Rep (char const *ident, O_Enum op) : P_Binary (ident, op) {}

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {

		coerce_string (type1, val1);
		S_fixed count = to_fixed (type2, val2);

		rep_string (val1._string, count, val._string);
		return T_string;
		}	// eval_Binary
	};	// P_String_Rep

//
//	String comparisons
//

#define	i_min(lval,rval)	((lval) < (rval) ? (lval) : (rval))
#define	i_max(lval,rval)	((lval) > (rval) ? (lval) : (rval))
#define	i_cmp(lval,rval)	((lval) < (rval) ? -1 : (lval) > (rval) ? 1 : 0)

// Compare S_strings for equality/inequality only
// (faster than 'cmp_string')
DL_EXPORT bool S_string::equality (S_string &s_op) {
if (&s_op == this)
	return true;			// (definitely equal)

unsigned l_left, l_right;
C_type t_left, t_right;

str_ptr p_left = s_op.fetch (l_left, t_left), p_right = fetch (l_right, t_right);

return l_left == l_right &&
	! s_cvt_compare (l_left, p_left, t_left, p_right, t_right);
}	// S_string::equality

// Returns result of string compare (<0, 0, >0)
static int cmp_string (S_string &s_left, S_string &s_right) {
if (&s_left == &s_right)
	return 0;			// (definitely equal)

unsigned l_left, l_right;
C_type t_left, t_right;

str_ptr
	p_left = s_left.fetch (l_left, t_left),
	p_right = s_right.fetch (l_right, t_right);

int result = l_left && l_right ?
	s_cvt_compare
		(i_min (l_left, l_right), p_left, t_left, p_right, t_right) :
	0;

return result ? result : i_cmp (l_left, l_right);
}	// cmp_string

struct P_String_Compare : P_Binary {
	cmp_fn func_cmp;

	P_String_Compare (char const *ident, O_Enum op, cmp_fn func_cmp) : P_Binary (ident, op)
		{ this->func_cmp = func_cmp; }

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		coerce_string (type1, val1);
		coerce_string (type2, val2);

		val._fixed = func_cmp (cmp_string (val1._string, val2._string));

		val1._string.relink ();
		val2._string.relink ();
		return T_fixed;
		}	// eval_Binary
	};	// P_String_Compare

//
//	String maximum/minimum
//

struct P_String_MinMax : P_Binary {
	bool max_min;		// ? Maximum : Minimum

	P_String_MinMax (char const *ident, O_Enum op, bool max_min) : P_Binary (ident, op)
		{ this->max_min = max_min; }

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		coerce_string (type1, val1);
		coerce_string (type2, val2);

		int result = cmp_string (val1._string, val2._string);
		val._string = (max_min ? result >= 0 : result <= 0) ?
			( val2._string.relink (), val1._string ):
			( val1._string.relink (), val2._string );
		return T_string;
		}	// eval_Binary
	};	// P_String_MinMax

//
//	Coerce scalar to string
//

struct P_String_Coerce : P_Unary {
	P_String_Coerce (char const *ident, O_Enum op) : P_Unary (ident, op) {}

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		coerce_string (type1, val1);
		val._string = val1._string;
		return T_string;
		}	// eval_Unary
	};	// P_String_Coerce

//
//	Slice string
//

// (character to fill)
static C_long filler = ' ';		// TODO: change filler

#define fill_string(to_ptr,len,type)		\
		fill_chars (len, to_ptr, type, filler)

// Slice string 'src' by range (from..to) to 'slice'
static void slice_string (S_string &s_src, S_fixed from, S_fixed to,
	S_string &s_slice) {
if (from > to)
	s_slice.s_clear ();

else {
unsigned l_slice = to - from;

if (0 <= from && to <= (S_fixed) s_src.length) {
	// (share part of existing string)
	s_slice.content = s_src.content;
	s_slice.offset = s_src.offset + from;
	s_slice.length = l_slice;
	}

else {
// (create new string)
unsigned l_src;
C_type t_src;
str_ptr p_src = s_src.fetch (l_src, t_src);
str_ptr p_slice = s_slice.alloc ("String/slice", l_slice, t_src);

if (0 < to && from < (S_fixed) l_src) {
	if (from < 0) {
		// (start padding)
		fill_string (p_slice, -from, t_src);
		s_forward (p_slice, -from, t_src);
		from = 0;
		}
	if (to > (S_fixed) l_src) {
		// (end padding)
		fill_string (s_offset (p_slice, l_src - from, t_src), to - l_src, t_src);
		to = l_src;
		}

	if (to != from)
		// (anything to copy)
		s_mcopy (p_slice, s_offset (p_src, from, t_src), to - from, t_src);
	}
else
	// (fill empty string)
	fill_string (p_slice, l_slice, t_src);

s_src.relink ();
}	// (new string)
}
}	// slice_string

struct P_String_Slice : Prefix {
	P_String_Slice (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		S_string string;
		Expr *x_range = get_arg (args);

		if (expect_string (args, string)) {
			// (default range...)
			FixedRange range (0, string.length);
			expect_range (x_range, range);

			slice_string (string, range.from, range.to, val._string);
			return T_string;
			}

		return T_undef;
		}	// evalV
	};	// P_String_Slice

//
// String searching
//

// Search string 's_src' for substring 's_ctx'
// (Returns offset, or -1, if not found)

// TODO:
// implement much better search method: Boyer-Moore or the like...

// TODO:
// use comparator instead mcmp...

static S_fixed search_string (bool dir, S_string &s_src, S_string &s_ctx) {
unsigned l_src, l_ctx;
C_type t_src, t_ctx;

str_ptr p_src = s_src.fetch (l_src, t_src), p_ctx = s_ctx.fetch (l_ctx, t_ctx);
str_ptr p_org = p_src;

if (! l_ctx)
	// (empty string is always found at beginning/end)
	return dir ? l_src : 0;

// TODO: look for first char...

if (l_src >= l_ctx) {
	// (actual search needed...)
	unsigned count = l_src + 1 - l_ctx;

	if (dir) {
		// (decremental search order)
		s_forward (p_src, l_src - l_ctx, t_src);
		while (count --)
			if (! s_cvt_compare (l_ctx, p_src, t_src, p_ctx, t_ctx))
				goto found;
			else s_dec (t_src, p_src);
		}
	else {
		// (incremental search order)
		while (count --)
			if (! s_cvt_compare (l_ctx, p_src, t_src, p_ctx, t_ctx))
				goto found;
			else s_inc (t_src, p_src);
		}
	}	// (l_src >= l_ctx)

return -1;			// (not found)

found:		// (found!)
	return s_diff (p_org, p_src, t_src);		// (relative index)
}	// search_string

struct P_String_Search : P_Binary {
	bool dir;		// dir ? backward : forward

	P_String_Search (char const *ident, O_Enum op, bool dir) : P_Binary (ident, op)
		{ this->dir = dir; }

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {

		coerce_string (type1, val1);
		coerce_string (type2, val2);

		val._fixed = search_string (dir, val1._string, val2._string);

		val1._string.relink ();
		val2._string.relink ();
		return T_fixed;
		}	// eval_Binary
	};	// P_String_Search

//
//	Character encoding/decoding
//

//
//	Data formats supported:
//
//	signed	unsigned
//	-1		1		byte
//	-2		2		word
//	-4		4		long
//

// TODO: fix...

// Decode string data to numeric value (according to type)
static S_fixed decode_fixed (short type, unsigned char *ptr, int len) {
S_fixed value = 0;
unsigned count = type >= 0 ? type : - type;

if (len > 0) {
while (count --) {
	value <<= 8;
	if (len -- > 0) value |= *ptr ++;
	}

if (type < 0) {
	unsigned maxint = 1 << (-type << 3);
	if (value & (maxint >> 1)) value -= maxint;
	}
}

return value;
}	// decode_fixed

struct P_String_DC : Prefix {
	short type;

	P_String_DC (char const *ident, O_Enum op, short type) : Prefix (ident, op)
		{ this->type = type; }

	VType evalV (VDatum &val, Expr *args) {
	S_string string;

	if (expect_string (get_arg (args), string)) {
		S_fixed offset = expect_fixed (args, 0);
		unsigned len;
		C_type type;
		str_ptr p_src = string.fetch (len, type);

		val._fixed = decode_fixed (this->type,
			(unsigned char *) s_offset (p_src, offset, type),
				s_scale (len - offset, type));
		string.relink ();
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

// Encode numeric value to string data (according to type)
static void encode_fixed (S_fixed value, unsigned char *ptr, unsigned len) {
ptr += len;
while (len --) { * --ptr = value; value >>= 8; }
}	// encode_fixed

struct P_String_EC : Prefix {
	unsigned short type;

	P_String_EC (char const *ident, O_Enum op, short type) : Prefix (ident, op)
		{ this->type = type; }

	VType evalV (VDatum &val, Expr *args) {
	if (args) {
		S_fixed value = expect_fixed (args, 0);
		encode_fixed (value,
			(unsigned char *) val._string.alloc ("String/EC", type, 0), type);
		return T_string;
		}

	return T_undef;
	}	// evalV
	};

struct P_String_Retype : P_Binary {
	P_String_Retype (char const *ident, O_Enum op) : P_Binary (ident, op) {}

	D_P_Binary_eval;
	};

VType P_String_Retype::eval_Binary (VDatum &val, VType type1, VDatum &val1,
	VType type2, VDatum &val2) {
C_type type = check_string_type (to_fixed (type1, val1));

coerce_string (type2, val2);

unsigned l_str;
C_type t_str;

str_ptr str = val2._string.fetch (l_str, t_str);

if (t_str != type) {
	// (conversion required)
	str_ptr res = val._string.alloc ("String/retype", l_str, type);
	s_cvt_copy (l_str, res, type, str, t_str);
	val2._string.relink ();
	}

else val._string.s_copy (val2._string);		// (no actual conversion)

return T_string;
}	// P_String_Retype::eval_Binary

//
//	Strings character-based mapping
//

void string_cmap (Prefix *mapper, S_string &s_src, S_string &s_dst) {
unsigned len;
C_type type;
str_ptr src = s_src.fetch (len, type);

if (len && mapper) {
	str_ptr dst = s_dst.alloc ("String/cmap", len, type);
	X_Fixed *arg = new X_Fixed (0);
	link_expr (arg);
	VDatum val;

	while (len --) {
		C_long code = UC_read_inc (type, src);

		arg->value = code;
		if (mapper->evalV (val, arg) == T_fixed)
			code = val._fixed;

		UC_write_inc (type, dst, code);
		}	// while (len)

	unlink_expr (arg);
	s_src.relink ();
	}	// (len)

else s_dst.s_copy (s_src);
}	// string_cmap

struct P_String_Map : Prefix {
	P_String_Map (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Prefix *mapper = expect_prefix (get_arg (args));
		S_string s_src;
		if (expect_string (args, s_src)) {
			s_src.link ();
			link_prefix (mapper);
			string_cmap (mapper, s_src, val._string);
			unlink_prefix (mapper);
			s_src.unlink ();
			return T_string;
			}

		return T_undef;
		}	// evalV
	};	// P_String_Map

//
//	String create
//

void string_create (C_type type, unsigned length, Prefix *mapper, S_string &s_dst) {
if (length && mapper) {
	str_ptr dst = s_dst.alloc ("String/create", length, type);
	X_Fixed *arg = new X_Fixed (0);
	link_expr (arg);
	VDatum val;

	for (unsigned index = 0; index != length; ++ index) {
		arg->value = index;
		C_long code = (mapper->evalV (val, arg) == T_fixed) ? val._fixed : 0;
		UC_write_inc (type, dst, code);
		}	// while (len)

	unlink_expr (arg);
	}	// (len)

else s_dst.s_clear ();
}	// string_create

struct P_String_Create : Prefix {
	P_String_Create (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		C_type t_res = check_string_type (expect_fixed (get_arg (args), 0));
		unsigned length = expect_fixed (get_arg (args), 1);
		Prefix *mapper = expect_prefix (args);

		link_prefix (mapper);
		string_create (t_res, length, mapper, val._string);
		unlink_prefix (mapper);

		return T_string;
		}	// evalV

	};	// P_String_Create

//
//	String list join
//

//	Warning: join *does not* support coercion

// Count # of substrings in 'opnd'
// (retrieve summary length in 'len')
static unsigned string_join_collect (Expr *opnd, unsigned &join_len, C_type &join_type) {
unsigned count = 0;

while (opnd) {
	Expr *next;
	X_List *list = opnd->isList ();

	if (list) {
		count ++;
		opnd = list->first;
		next = list->next;
		}
	else
		next = 0;

	X_String *string = Cast(X_String, opnd);
	if (string) {
		unsigned len;
		C_type type;

		string->fetch (len, type);
		join_len += len;
		join_type = s_common_type (type, join_type);
		}

	opnd = next;
	}	// while (opnd)

return count;
}	// string_join_collect

// Join all substrings in 'opnd' to 's_dst'
// (separating with 's_sep')
static void string_join (S_string &s_sep, Expr *opnd, S_string &s_dst, Prefix *where) {
if (! opnd) { s_dst.s_clear (); return; }

unsigned l_sep;
C_type t_sep;
str_ptr p_sep = s_sep.fetch (l_sep, t_sep);

unsigned len = 0;			// (length of result)
C_type type = t_sep;		// (type of result)
unsigned count = string_join_collect (opnd, len, type);		// (total count of items)

if (! count) {			// ???
	X_String *string = Cast (X_String, opnd);
	if (string) s_dst.s_set (string);
	else s_dst.s_clear ();
	return;
	}

str_ptr p_dst = s_dst.alloc ("String/join", len + l_sep*count, type);

while (opnd) {
	Expr *next;
	X_List *list = opnd->isList ();

	if (list) {
		opnd = list->first;
		next = list->next;
		}
	else
		next = 0;

	X_String *string = Cast(X_String, opnd);
	if (string) {		// (copy element)
		unsigned l_src;
		C_type t_src;
		str_ptr p_src = string->fetch (l_src, t_src);

		s_cvt_copy (l_src, p_dst, type, p_src, t_src);
		s_forward(p_dst, l_src, type);
		}

	if (list) {			// (add separator)
		s_cvt_copy (l_sep, p_dst, type, p_sep, t_sep);
		s_forward(p_dst, l_sep, type);
		}

	opnd = next;
	}	// (opnd list)

}	// string_join

struct P_String_Join : Prefix {
	P_String_Join (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	S_string s_sep;
	if (expect_string (get_arg (args), s_sep)) {
		Expr *list = args ? args->evalX () : 0;
		string_join (s_sep, list, val._string, this);

		s_sep.relink ();
		relink_expr (list);

		return T_string;
		}

	return T_undef;
	}	// evalV
	};

//
//	String construct charrange
//

static void string_range (S_string &s_res, C_type t_res, FixedRange range, bool direction) {
str_ptr p_res = s_res.alloc ("String/range", range.to - range.from, t_res);

while (range.notempty ())
	UC_write_inc (t_res, p_res, direction ? -- range.to : range.from ++);
}	// string_range

struct P_String_Range : Prefix {
	P_String_Range (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		C_type t_res = check_string_type (expect_fixed (get_arg (args), 0));
		bool direction = expect_bool (get_arg (args), false);

		FixedRange range (0, 0);
		expect_range (args, range);

		if (range.notempty ())
			string_range (val._string, t_res, range, direction);
		else
			val._string.s_clear ();
		return T_string;
		}	// evalV
	};

//
//	String reverse
//

static void string_reverse (S_string &s_src, S_string &s_res) {
unsigned len;
C_type type;

str_ptr p_src = s_src.fetch (len, type);

if (len > 1) {
	str_ptr p_dst = s_res.alloc ("String/reverse", len, type);

	s_forward (p_dst, len, type);
	while (len --)
		s_write_dec (type, p_dst, s_read_inc (type, p_src));

	s_src.relink ();
	}
else
	s_res.s_copy (s_src);		// (same string)
}	// string_reverse

struct P_String_Rev : P_Unary {
	P_String_Rev (char const *ident, O_Enum op) : P_Unary (ident, op) {}

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		coerce_string (type1, val1);
		string_reverse (val1._string, val._string);
		return T_string;
		}	// eval_Unary
	};

//
//	Get character code from string
//

// Get character by index
C_long string_ord (S_string &source, int index) {
unsigned length;
C_type type;
str_ptr p_src = source.fetch (length, type);

if (index < 0) index += length;			// (from the end...)
if (0 <= index && index < length) {
	if (index) s_forward (p_src, index, type);
	return UC_read (type, p_src);
	}

return ~0;
}	// string_ord

struct P_String_Ord : Prefix {
	P_String_Ord (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	S_string source;

	if (expect_string (get_arg (args), source)) {
		S_fixed offset = expect_fixed (args, 0);
		val._fixed = string_ord (source, offset);
		source.relink ();

		return val._fixed == ~0 ? T_undef : T_fixed;
		}

	return T_undef;
	}	// evalV

	};

//
//	Construct string from charlist
//

static void string_chars (C_type type, Expr *source, S_string &result, Prefix *where) {
X_List *list;
unsigned count = 0;

Expr *expr = source;
while (expr) {
	list = expr->isList();
	expr = list ? list->next : 0;
	++ count;
	}	// while (source)

str_ptr dest = result.alloc ("String/chars", count, type);

while (count --) {
	list = source->isList();
	C_long code = where->expect_fixed (list ? list->first : source, 0);
	source = list ? list->next : 0;

	UC_write_inc (type, dest, code);
	}	// while (source)
}	// string_chars

struct P_String_Chars : Prefix {
	P_String_Chars (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		unsigned type = expect_fixed (get_arg (args), 0);		// TODO: check type
		Expr *list = evalX_X (args);

		string_chars (type, list, val._string, this);
		relink_expr (list);
		return T_string;
		}	// evalV
	};

//
//	String predicate span/break
//

static unsigned string_span_pred (S_string &s_src, Prefix *pred_span,
	bool direction, bool polarity) {
unsigned l_src;
C_type t_src;
str_ptr p_src = s_src.fetch (l_src, t_src);
str_ptr p_org = p_src;

// Exception trapping in apply_bool???

X_Fixed *arg = new X_Fixed (0);
link_expr (arg);

if (direction) {
	s_forward (p_src, l_src, t_src);

	while (l_src --) {
		arg->value = UC_read_dec (t_src, p_src);

		if (apply_bool (pred_span, arg) != polarity)
			{ s_inc (t_src, p_src); break; }
		}
	}

else {
	while (l_src --) {
		arg->value = UC_read_inc (t_src, p_src);

		if (apply_bool (pred_span, arg) != polarity)
			{ s_dec (t_src, p_src); break; }
		}
	}

unlink_expr (arg);
return s_diff (p_org, p_src, t_src);
}	// string_span_pred

struct P_String_Span : Prefix {
	bool direction;
	bool polarity;

	P_String_Span (char const *ident, O_Enum op, bool direction, bool polarity) :
		Prefix (ident, op) {
		this->direction = direction;
		this->polarity = polarity;
		}

	VType evalV (VDatum &val, Expr *args) {
	S_string string;

	if (expect_string (get_arg (args), string)) {
		Prefix *pfx = expect_prefix (args);
		if (pfx) {
			link_prefix (pfx);
			string.link ();
			val._fixed = string_span_pred (string, pfx, direction, polarity);
			string.unlink ();
			unlink_prefix (pfx);
			return T_fixed;
			}
		}

	return T_undef;
	}	// evalV
	};

//
//	String predicate count
//

static unsigned string_count_pred (S_string &s_src, Prefix *pred_count, bool polarity) {
unsigned l_src;
C_type t_src;
str_ptr p_src = s_src.fetch (l_src, t_src);

unsigned total = 0;

// Exception trapping in apply_bool???

X_Fixed *arg = new X_Fixed (0);
link_expr (arg);

while (l_src --) {
	arg->value = UC_read_inc (t_src, p_src);
	if (apply_bool (pred_count, arg) == polarity)
		++ total;
	}

unlink_expr (arg);

return total;
}	// string_count_pred

struct P_String_Count : Prefix {
	bool polarity;

	P_String_Count (char const *ident, O_Enum op, bool polarity) :
		Prefix (ident, op)
		{ this->polarity = polarity; }

	VType evalV (VDatum &val, Expr *args) {
	S_string string;

	if (expect_string (get_arg (args), string)) {
		Prefix *pfx = expect_prefix (args);
		if (pfx) {
			link_prefix (pfx);
			string.link ();
			val._fixed = string_count_pred (string, pfx, polarity);
			string.unlink ();
			unlink_prefix (pfx);
			return T_fixed;
			}
		}

	return T_undef;
	}	// evalV
	};

//
//	String predicate filter
//

static void string_filter_pred (S_string &s_src, Prefix *pred_filter,
	bool polarity, S_string &s_res) {
unsigned l_src;
C_type t_src;
str_ptr p_src = s_src.fetch (l_src, t_src);

if (! l_src) { s_res.s_clear (); return; }

unsigned char *bitvec = new unsigned char [(l_src + 7) >> 3];
unsigned char *bitptr = bitvec, bitmask = 1, bitval = 0;
unsigned count = 0;

// Exception trapping in apply_bool???

X_Fixed *arg = new X_Fixed (0);
link_expr (arg);

while (l_src --) {
	arg->value = UC_read_inc (t_src, p_src);
	if (apply_bool (pred_filter, arg) == polarity)
		{ bitval |= bitmask; ++ count; }

	if (! (bitmask <<= 1))
		{ bitmask = 1; *bitptr ++ = bitval; bitval = 0; }
	}	// (l_src)

if (bitmask != 1) *bitptr = bitval;

unlink_expr (arg);

str_ptr p_res = s_res.alloc ("String/filter", count, t_src);
p_src = s_src.fetch (l_src, t_src);
bitptr = bitvec; bitmask = 0;

while (l_src --) {
	C_long code = UC_read_inc (t_src, p_src);

	if (! bitmask)
		{ bitmask = 1; bitval = *bitptr ++; }

	if (bitval & bitmask)
		UC_write_inc (t_src, p_res, code);

	bitmask <<= 1;
	}

delete [] bitvec;
}	// string_filter_pred

struct P_String_Filter : Prefix {
	bool polarity;

	P_String_Filter (char const *ident, O_Enum op, bool polarity) :
		Prefix (ident, op)
		{ this->polarity = polarity; }

	VType evalV (VDatum &val, Expr *args) {
	S_string string;

	if (expect_string (get_arg (args), string)) {
		Prefix *pfx = expect_prefix (args);
		if (pfx) {
			link_prefix (pfx);
			string.link ();
			string_filter_pred (string, pfx, polarity, val._string);
			string.unlink ();
			unlink_prefix (pfx);
			return T_string;
			}
		}

	return T_undef;
	}	// evalV
	};

//
//	String iterator (forward/reverse)
//

struct P_String_Loop : P_Iterator {
	bool direction;

	P_String_Loop (char const *ident, O_Enum op, bool direction) :
		P_Iterator (ident, op)
		{ this->direction = direction; }

	D_P_Iterator_evaluate;
	};

void P_String_Loop::evaluate (IterContext &IC, Expr *args) {
Expr *&R_index = expectR_X (get_arg (args));

S_string string;

if (expect_string (get_arg (args), string)) {
unsigned l_src;
C_type t_src;
str_ptr p_src = string.fetch (l_src, t_src);

string.link ();

if (l_src && args) {
VDatum V_char;
S_fixed &I_char = V_char._fixed;
IC.start (args);

if (direction) {
	s_forward(p_src, l_src, t_src);

	while (l_src --) {
	I_char = UC_read_dec (t_src, p_src);
	mutateR_V (R_index, T_fixed, V_char);

	if (! IC.next ()) break;
	}	// while (l_src)
	}

else while (l_src --) {
	I_char = UC_read_inc (t_src, p_src);
	mutateR_V (R_index, T_fixed, V_char);

	if (! IC.next ()) break;
	}	// while (l_src)
}	// (l_src)

string.unlink ();
}	// (expect_string)
}	// P_String_Loop::evaluate

//
//	Charset predicate composer
//

struct P_CheckCC_Nul : Prefix {
	bool polarity;			// ? inclusive : exclusive

	P_CheckCC_Nul (bool _polarity) : Prefix (0)
		{ polarity = _polarity; }

	void log_ex (Logger &log) {
		log.put_ch ('<')->put_cstr ("cc_nul ")->put_ch (polarity ? '+' : '-')->put_ch ('>');
		}	// log_ex

	VType evalV (VDatum &val, Expr *args) {
		val._fixed = ! polarity;
		return T_fixed;
		}	// evalV

	};

struct P_CheckCC_Chr : Prefix {
	C_long code;			// character code to test
	bool polarity;			// ? inclusive : exclusive

	P_CheckCC_Chr (C_long _code, bool _polarity) : Prefix (0) {
		code = _code;
		polarity = _polarity;
		}

	void log_ex (Logger &log) {
		log.put_ch ('<')->put_cstr ("cc_chr ")->put_ch (polarity ? '+' : '-')->log_fixed (code)->put_ch ('>');
		}	// log_ex

	VType evalV (VDatum &val, Expr *args) {
		S_fixed charval = expect_fixed (args, 0);
		val._fixed = (charval == code) == polarity;
		return T_fixed;
		}	// evalV

	};

// TODO: use bitvec???

struct P_CheckCC_Str : Prefix {
	S_string charset;
	bool polarity;			// ? inclusive : exclusive

	P_CheckCC_Str (S_string &_charset, bool _polarity) : Prefix (0) {
		charset = _charset;
		charset.link ();
		polarity = _polarity;
		}

	void log_ex (Logger &log) {
		log.put_ch ('<')->put_cstr ("cc_str ")->put_ch (polarity ? '+' : '-');
		charset.log (log);
		log.put_ch ('>');
		}	// log_ex

	VType evalV (VDatum &val, Expr *args) {
		S_fixed charval = expect_fixed (args, 0);

		unsigned l_charset;
		C_type t_charset;
		str_ptr p_charset = charset.fetch (l_charset, t_charset);

		while (l_charset) {
			if (charval == UC_read_inc (t_charset, p_charset)) break;
			l_charset --;
			}

		return_fixed (val, (l_charset == 0) != polarity);
		}	// evalV

	void release () {
		charset.unlink ();
		Prefix::release ();
		}	// release
	};

struct P_CC_String : Prefix {
	bool polarity;			// ? exclusive : inclusive

	P_CC_String (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	VType evalV (VDatum &val, Expr *args) {
	S_string string;

	if (expect_string (args, string)) {
		unsigned len;
		C_type type;
		str_ptr ptr = string.fetch (len, type);

		val._pfx =
			(! len) ?
				(Prefix *) (new ("CC/Null") P_CheckCC_Nul (polarity)) :
			(len == 1) ?
				(Prefix *) (new ("CC/Char") P_CheckCC_Chr (UC_read (type, ptr), polarity)) :

				(Prefix *) (new ("CC/String") P_CheckCC_Str (string, polarity));

		return T_prefix;
		}

	return T_undef;
	}	// evalV
	};

//
//	Charclass check predicates
//

// Test: latin lowercase letter?
static bool ischar_lower (char ch)
	{ return ('a' <= ch && ch <= 'z'); }

// Test: latin uppercase letter?
static bool ischar_upper (char ch)
	{ return ('A' <= ch && ch <= 'Z'); }

// Test: latin letter?
static bool ischar_alpha (char ch)
	{ return ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z'); }

// Test: decimal digit?
static bool ischar_digit (char ch)
	{ return ('0' <= ch && ch <= '9'); }

// Test: octal digit?
static bool ischar_odigit (char ch)
	{ return ('0' <= ch && ch <= '7'); }

// Test: hexadecimal digit?
static bool ischar_xdigit (char ch) {
return
	('0' <= ch && ch <= '9') ||
	('A' <= ch && ch <= 'F') ||
	('a' <= ch && ch <= 'f');
}

// Test: blank space?
static bool ischar_blank (char ch) {
return (ch == ' ') || (ch == '\n') || (ch == '\r') || (ch == '\t');
}

// Test: printable?
static bool ischar_print (char ch) {
return (' ' <= ch) && (ch < '\x7f');
}

struct P_CC_Func : Prefix {
	bool (* ischar_test) (char ch);			// predicate

	P_CC_Func (char const *ident, bool (* ischar_test) (char ch)) : Prefix (ident, Op_Null)
		{ this->ischar_test = ischar_test; }

	VType evalV (VDatum &val, Expr *args) {
		S_fixed charval = expect_fixed (args, 0);
		val._fixed = ischar_test (charval);
		return T_fixed;
		}	// evalV

	};

//
//	String case conversion
//

// TODO: use charmaps for conversion...

// (Character to lowercase)
static C_long lcase_ch (C_long ch)
	{ return 'A' <= ch && ch <= 'Z' ? ch - 'A' + 'a' : ch; }

// (Character to uppercase)
static C_long ucase_ch (C_long ch)
	{ return 'a' <= ch && ch <= 'z' ? ch - 'a' + 'A' : ch; }

// (Character case invert)
static C_long icase_ch (C_long ch) {
	return
		'a' <= ch && ch <= 'z' ? ch - 'a' + 'A' :
		'A' <= ch && ch <= 'Z' ? ch - 'A' + 'a' :
		ch;
	}	// icase_ch

// Change string case
static void recase_string (C_long (*convert) (C_long ch), bool first_only,
	S_string &s_src, S_string &s_res) {
unsigned len;
C_type type;
str_ptr p_src = s_src.fetch (len, type);
str_ptr p_res = s_res.alloc ("String/recase", len, type);

if (len) {
	if (first_only) {
		// (convert first character only)
		UC_write_inc (type, p_res, convert (UC_read_inc (type, p_src)));
		s_mcopy (p_res, p_src, len-1, type);		// (copy the remainder)
		}
	else {
		// (convert all characters)
		while (len --)
			UC_write_inc (type, p_res, convert (UC_read_inc (type, p_src)));
		}
	}
}	// recase_string

struct P_String_ReCase : P_Unary {
	// Character convertor
	C_long (*ccase_ch) (C_long ch);

	// ? only first character : all characters
	bool first_only;

	P_String_ReCase (char const *ident, O_Enum op, C_long (*ccase_ch) (C_long ch), bool first_only) :
		P_Unary (ident, op)
		{ this->ccase_ch = ccase_ch; this->first_only = first_only; }

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		coerce_string (type1, val1);
		recase_string (ccase_ch, first_only, val1._string, val._string);
		return T_string;
		}	// eval_Unary

	};

//
//	Parse/unparse numbers
//

// Parse number by fixed 'base'
struct P_ParseFixed : P_Unary {
	unsigned base;

	P_ParseFixed (char const *ident, O_Enum op, unsigned base) :
		P_Unary (ident, op) { this->base = base; }

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		coerce_string (type1, val1);

		ParseString parser (val1._string);
		val._fixed = parser.parse_fixed (base);
		val1._string.relink ();
		return T_fixed;
		}	// eval_Unary

	};

// Parse number by parameter base
struct P_ParseFixedBase : P_Binary {
	P_ParseFixedBase (char const *ident, O_Enum op) :
		P_Binary (ident, op) {}

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		int base = to_fixed (type1, val1);
		coerce_string (type2, val2);

		ParseString parser (val2._string);
		val._fixed = parser.parse_fixed (base);
		val2._string.relink ();
		return T_fixed;
		}	// eval_Binary

	};

// Parse fixed number
S_fixed parse_fixed (S_string &s_str) {
ParseString parser (s_str);
return parser.parse_fixed (10);
}	// parse_fixed

// Parse float by parameter base
struct P_ParseFloat : P_Binary {
	P_ParseFloat (char const *ident, O_Enum op) :
		P_Binary (ident, op) {}

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		coerce_string (type1, val1);
		int flags = to_fixed (type2, val2);

		ParseString parser (val1._string);
		val._float = parser.parse_float ();
		val1._string.relink ();
		return T_float;
		}	// eval_Binary

	};

// Parse float number
S_float parse_float (S_string &s_str) {
ParseString parser (s_str);
return parser.parse_float ();
}	// parse_float

//
//	Formatting
//

// Format fixed number by defined base
struct P_FormatNum : P_Unary {
	bool upcase;
	unsigned base;

	P_FormatNum (char const *ident, O_Enum op, unsigned base, bool upcase) :
		P_Unary (ident, op) { this->base = base; this->upcase = upcase; }

	VType eval_Unary (VDatum &val, VType type1, VDatum &val1) {
		unsigned value = to_fixed (type1, val1);

		char buffer[32];
		FormatString form (buffer, sizeof(buffer));
		form.format_fixed (value, base, upcase);
		form.cons (val._string, 0, "Format/Num");
		return T_string;
		}	// eval_Unary

	};

// Format fixed number by any base
struct P_FormatNumBase : P_Binary {
	bool upcase;

	P_FormatNumBase (char const *ident, O_Enum op, bool upcase) :
		P_Binary (ident, op) { this->upcase = upcase; }

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		int base = to_fixed (type1, val1);
		unsigned value = to_fixed (type2, val2);

		char buffer[32];
		FormatString form (buffer, sizeof(buffer));
		form.format_fixed (value, base, upcase);
		form.cons (val._string, 0, "Format/NumBase");
		return T_string;
		}	// eval_Binary

	};

// Format float number
struct P_FormatFloat : P_Binary {
	FC_format format_f;

	P_FormatFloat (char const *ident, O_Enum op, FC_format format_f) :
		P_Binary (ident, op) { this->format_f = format_f; }

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
	S_float value = to_float (type1, val1);
	int argument = to_fixed (type2, val2);

	int p_shift = argument >> 16;
	int n_digits = argument & 0xffff;
	if (n_digits & 0x8000) n_digits -= 0x10000;

	char buffer[36];
	FormatString form (buffer, sizeof(buffer));
	form.format_float (value, format_f, n_digits, p_shift);
	form.cons (val._string, 0, "Format/Float");

	return T_string;
	}	// eval_Binary

	};

//
//	Detect common string prefix/suffix
//

void string_common (bool dir, S_string &s_res, S_string &src_1, S_string &src_2) {
unsigned len_1, len_2;
C_type type_1, type_2;
C_long ch_1, ch_2;
unsigned total = 0;

bool flag;
str_ptr ptr_1 = src_1.fetch (len_1, type_1);
str_ptr ptr_2 = src_2.fetch (len_2, type_2);
unsigned len = (flag = len_1 < len_2) ? len_1 : len_2;

if (dir) {
	s_forward (ptr_1, len_1, type_1);
	s_forward (ptr_2, len_2, type_2);

	while (len &&
		(ch_1 = UC_read_dec (type_1, ptr_1)) == (ch_2 = UC_read_dec (type_2, ptr_2)))
			(-- len, ++ total);
	}
else {
	while (len &&
		(ch_1 = UC_read_inc (type_1, ptr_1)) == (ch_2 = UC_read_inc (type_2, ptr_2)))
			(-- len, ++ total);
	}

if (total) {
	S_string *src_p =
		flag ? (src_2.relink (), &src_1) : (src_1.relink (), &src_2);

	s_res.content = src_p->content;
	s_res.offset = src_p->offset + (dir ? len : 0);
	s_res.length = total;
	}
else
	s_res.s_clear ();
}	// string_common

struct P_String_Common : P_Binary {
	bool dir;	// ? common suffix : common prefix

	P_String_Common (char const *ident, O_Enum op, bool dir) :
		P_Binary (ident, op) { this->dir = dir; }

	VType eval_Binary (VDatum &val,
		VType type1, VDatum &val1, VType type2, VDatum &val2) {
		coerce_string (type1, val1);
		coerce_string (type2, val2);

		string_common (dir, val._string, val1._string, val2._string);

		return T_string;
		}	// eval_Binary
	};

//
//	Generic stream encode/decode
//

//
//	Stream interface: character/octet counter
//

struct CountCharS : CharStream {
	unsigned count;
	
	CountCharS () { count = 0; }

	unsigned read () { return EndData; }

	bool write (unsigned codepoint) { ++ count; return true; }

	unsigned done () { return count; }
	};

struct CountByteS : ByteStream {
	unsigned count;

	CountByteS () { count = 0; }

	// Get next character (returns EndData on end of stream)
	unsigned fetch () { return EndData; }

	// Emit octet to stream
	bool emit (unsigned char octet) { ++ count; return true; }

	unsigned done () { return count; }
	};

//
//	Stream interface to strings
//

struct StringCharS : CharStream {
	str_ptr curptr, limit;
	C_type type;

	StringCharS (C_type type, str_ptr start, unsigned length) {
		this->type = type;
		limit = s_offset ((curptr = start), length, type);
		}	// StringCharS

	// Read character 'codepoint' from this stream
	// Returns EndData on complete
	unsigned read ()
		{ return curptr != limit ? UC_read_inc (type, curptr) : EndData; }

	// Write character from 'codepoint' to this stream
	// (Return false on failure)
	bool write (unsigned codepoint)
		{ return curptr != limit ? (UC_write_inc (type, curptr, codepoint), true) : false; }

	};	// StringCharS

struct StringByteS : ByteStream {
	C_byte *curptr, *limit;

	StringByteS (str_ptr start, unsigned length)
		{ limit = (curptr = (C_byte *) start) + length; }

	// Get next character (returns EndData on end of stream)
	unsigned fetch ()
		{ return curptr != limit ? *curptr ++ : EndData; }

	// Emit octet to stream
	bool emit (unsigned char octet)
		{ return curptr != limit ? ((*curptr ++ = octet), true) : false; }

	};

SCodec *expect_scodec (Expr *expr, Prefix *where);

// Encode string with codec
// (string => byte stream)
static unsigned encode_string (SCodec *codec, S_string &input, S_string &output) {
if (codec) {
	unsigned src_length;
	C_type src_type;
	str_ptr src_ptr = input.fetch (src_length, src_type);

	StringCharS char_source (src_type, src_ptr, src_length);
	CountByteS byte_count;

	codec->encode (char_source, byte_count);
	unsigned count = byte_count.done ();

	str_ptr content = output.alloc ("String/encode", count, 0);

	StringCharS char_read (src_type, src_ptr, src_length);
	StringByteS byte_write (content, count);

	codec->encode (char_read, byte_write);

	return count;
	}	// (codec)

return 0;
}	// encode_string

struct P_String_Encode : Prefix {
	P_String_Encode (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	SCodec *codec = expect_scodec (get_arg (args), this);

	S_string source;
	if (codec && expect_string (args, source)) {
		encode_string (codec, source, val._string);

		source.relink ();
		relink_expr (codec);
		return T_string;
		}

	return T_undef;
	}	// evalV
	};

// Decode string with codec
// (byte stream => string)
static unsigned decode_string (SCodec *codec, S_string &input, S_string &output,
	C_type out_type) {
if (codec) {
	unsigned src_length;
	C_type src_type;
	str_ptr src_ptr = input.fetch (src_length, src_type);

	StringByteS byte_source (src_ptr, s_scale(src_length, src_type));
	CountCharS char_count;

	codec->decode (byte_source, char_count);
	unsigned count = char_count.done ();

	str_ptr out_ptr = output.alloc ("String/decode", count, out_type);

	StringByteS byte_read (src_ptr, s_scale(src_length, src_type));
	StringCharS char_write (out_type, out_ptr, count);

	codec->decode (byte_read, char_write);

	return count;
	}	// (codec)

return 0;
}	// decode_string

struct P_String_Decode : Prefix {
	P_String_Decode (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {

	SCodec *codec = expect_scodec (get_arg (args), this);

	S_string source;
	if (codec && expect_string (get_arg (args), source)) {
		S_fixed type = check_string_type (expect_fixed (args, 0));
		decode_string (codec, source, val._string, type);

		relink_expr (codec);
		source.relink ();
		return T_string;
		}

	return T_undef;
	}	// evalV
	};

//
//	String mutators
//

// Unicalize string data
static X_String *string_uniq_copy (Expr *expr, Prefix *pfx) {
Expr * &string_R = pfx->expectR_X(expr);
X_String *string;

if (string = Cast (X_String, string_R)) {
	X_String *copy = string->uniq();
	if (copy != string)
		mutateR_X (string_R, copy);
	return copy;
	}
else {
	VDatum val;
	VType type = string_R->evalV (val, true);
	pfx->type_error (expr, T_string, type, val);
	}

return 0;
}	// string_uniq_copy

struct P_StringPoke : PrefixX {
	P_StringPoke (char const *ident, O_Enum op) :
		PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_StringPoke::evalX (Expr *args) {
X_String *string;

if (string = string_uniq_copy(get_arg(args), this)) {
	unsigned len;
	C_type type;

	str_ptr p_src = string->fetch (len, type);

	S_fixed offset = expect_fixed (get_arg (args), 0);
	S_fixed code = expect_fixed (args, 0);

	if (offset < len)
		UC_write (type, s_offset(p_src, offset, type), code);
	}

return string;
}	// P_StringPoke::evalX

struct P_StringPatch : PrefixX {
	P_StringPatch (char const *ident, O_Enum op) :
		PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_StringPatch::evalX (Expr *args) {
X_String *string;

if (string = string_uniq_copy(get_arg (args), this)) {
	S_string substr;
	unsigned len, sublen;
	C_type type, subtype;

	str_ptr p_src = string->fetch (len, type);

	FixedRange range;
	expect_range (get_arg (args), range);

	if (expect_string (args, substr)) {
		str_ptr p_ref = substr.fetch (sublen, subtype);
		if (type == subtype && sublen == range.count() && range.from + sublen <= len)
			// TODO: coerce...
			// s_mcopy (s_offset (p_src, range.from, type), p_ref, sublen, type);
			s_cvt_copy (sublen, s_offset (p_src, range.from, type), type, p_ref, subtype);
		}
	}

return string;
}	// P_StringPatch::evalX

//
//	Initialisation
//

static bool init_primaries_string (int order) {

//		[Categories]

//^C	String
//^B	String functors
//^D	Functors, operating on string values.

//^C	Character
//^B	Character functors
//^D	Functors, operating on character codes.

//^C	Encoder
//^B	Encoding functors
//^D	Functors, converting numeric values to strings.

//^C	Decoder
//^B	Decoding functors
//^D	Functors, converting strings to numeric values.

//		[Types]

//^T	String
//^B	String value.
//^D	Anything evaluating to string. As argument, any scalar value allowed (implicitly coerced to string).

//^T	StringType
//^B	Type of string.
//^D	Integer value, type of string
//^D	Type 0: 8 bits/char, 1: 16 bits/char, 2: 32 bits/char.

//		--------

//^G	is_string expect_string

//^N	is_string [Predicate | String]
//^P	is_string (V: Any) => Bool
//^B	Check for string value.
//^D	Predicate: !true, if argument \V evaluates to string scalar.

	DefBuiltin (P_IsType ("is_string", Op_Null, T_string));

//^N	expect_string [Wrapper | String]
//^P	expect_string (V: Any, @Body: Any) => Any
//^B	Expect string value.
//^D	If argument \V evaluates to string, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_string", Op_Null, T_string));

//^G	s_empty

//^N	s_empty [Scalar | Nullary | String]
//^P	s_empty () => String
//^B	Empty string literal.
//^D	Always returns empty string.

	DefBuiltin (P_String_Empty ("s_empty", Op_Null));

//^G	s_type s_retype

//^N	s_type [Scalar | Unary | String]
//^P	s_type (S: String) => StringType
//^B	Query string type.
//^D	Returns type of string \S
//^D	(8 bit chars / 16 bit chars / 32 bit chars).

	DefBuiltin (P_String_Type ("s_type", Op_SType));

//^N	s_retype [Scalar | Binary | String]
//^P	s_retype (Type: StringType, S: String) => String
//^B	Convert string to different type.
//^D	Convert string \S to characters \Type
//^D	(8 bit chars / 16 bit chars / 32 bit chars).
//^D	Conversion is defined by active !codepage.

	DefBuiltin (P_String_Retype ("s_retype", Op_Null));

//^G	s_len s_cat s_rep

//^N	s_len [Scalar | Unary | String]
//^P	s_len (S: String) => Int
//^B	Get string length.
//^D	Returns length of string \S (0, if \S is empty).
//^D	Syntax: #$\S.

	DefBuiltin (P_String_Len ("s_len", Op_SLen));

//^N	s_cat [Scalar | Binary | String]
//^P	s_cat (S: String, T: String) => String
//^B	String concatenation.
//^D	Returns strings \S and \T concatenated together.
//^D	Syntax: \S +$ \T.

	DefBuiltin (P_String_Cat ("s_cat", Op_SCat));

//^N	s_rep [Scalar | Binary | String]
//^P	s_rep (S: String, N: Int) => String
//^B	String replication.
//^D	Returns \S replicated exactly \N times.
//^D	(Returns \S, if \N == 1; or empty string, if \N == 0.)
//^D	Syntax: \S *$ \N.

	DefBuiltin (P_String_Rep ("s_rep", Op_SRep));

//^G	s_lt s_le s_gt s_ge s_eq s_ne

//^N	s_lt [Scalar | Predicate | String]
//^P	s_lt (S: String, T: String) => Bool
//^B	String "less than" compare.
//^D	String less than test: true, if \S is alphabetically before \T.
//^D	Syntax: \S <$ \T.

//^N	s_le [Scalar | Predicate | String]
//^P	s_le (S: String, T: String) => Bool
//^B	String "less than or equal" compare.
//^D	String less than/equal test: true, if \S is alphabetically before or equal to \T.
//^D	Syntax: \S <=$ \T.

//^N	s_gt [Scalar | Predicate | String]
//^P	s_gt (S: String, T: String) => Bool
//^B	String "greater than" compare.
//^D	String greater than test: true, if \S is alphabetically after \T.
//^D	Syntax: \S >$ \T.

//^N	s_ge [Scalar | Predicate | String]
//^P	s_ge (S: String, T: String) => Bool
//^B	String "greater than or equal" compare.
//^D	String greater than/equal test: true, if \S is alphabetically after or equal to \T.
//^D	Syntax: \S >=$ \T.

//^N	s_eq [Scalar | Predicate | String]
//^P	s_eq (S: String, T: String) => Bool
//^B	String equality.
//^D	String equality test: true, if \S is identical to \T.
//^D	Syntax: \S ==$ \T.

//^N	s_ne [Scalar | Predicate | String]
//^P	s_ne (S: String, T: String) => Bool
//^B	String inequality.
//^D	String inequality test: true, if \S is different from \T.
//^D	Syntax: \S <>$ \T.

	DefBuiltin (P_String_Compare ("s_lt", Op_SLT,	cmp_lt));
	DefBuiltin (P_String_Compare ("s_le", Op_SLE,	cmp_le));
	DefBuiltin (P_String_Compare ("s_gt", Op_SGT,	cmp_gt));
	DefBuiltin (P_String_Compare ("s_ge", Op_SGE,	cmp_ge));
	DefBuiltin (P_String_Compare ("s_eq", Op_SEQ,	cmp_eq));
	DefBuiltin (P_String_Compare ("s_ne", Op_SNE,	cmp_ne));

//^G	s_cmp

//^N	s_cmp [Scalar | Compare | String]
//^P	s_cmp (S: String, T: String) => Sign
//^B	String signed comparison.
//^D	Returns -1, if \S is alphabetically before \T;
//^\	1, if \S is alphabetically after \T; 0 if \S is equal to \T (\S and \T evaluated once).
//^D	Syntax: \S <?>$ \T.

	DefBuiltin (P_String_Compare ("s_cmp", Op_SCmp,	cmp_cmp));

//^G	s_min s_max

//^N	s_min [Scalar | Binary | String]
//^P	s_min (S: String, T: String) => String
//^B	String minimum.
//^D	Returns either \S or \T, which is first alphabetically.
//^D	Syntax: \S ?<$ \T.

//^N	s_max [Scalar | Binary | String]
//^P	s_max (S: String, T: String) => String
//^B	String maximum.
//^D	Returns either \S or \T, which is last alphabetically.
//^D	Syntax: \S ?>$ \T.

	DefBuiltin (P_String_MinMax ("s_min", Op_SMin,	false));
	DefBuiltin (P_String_MinMax ("s_max", Op_SMax,	true));

//^G	s_slice

//^N	s_slice [Scalar | Binary | String]
//^P	s_slice (R: Range, S: String) => String
//^B	String slice.
//^D	Returns fragment of string \S sliced by range \R.
//^D	Range \R defaults to entire string (0 .. #$\S).
//^D	If \R exceeds limits (either \R[0] < 0 or \R[1] > #$\S),
//^\	returned string is padded with spaces at begin and/or end.
//^D	Length of result is always \R[1] - \R[0].
//^D	Syntax: \S $[\R].

	DefBuiltin (P_String_Slice ("s_slice", Op_SSlice));

//^G	s_findfirst s_findlast

//^N	s_findfirst [Scalar | Binary | String]
//^P	s_findfirst (Src: String, Ctx: String) => Int
//^B	Search string forward for context.
//^D	Searches forward string \Src, looking for the first occurence of context \Ctx.
//^D	Returns: offset of \Ctx in \Src (starting from 0), or -1, if \Ctx was not found.
//^D	Syntax: \Src >>$ \Ctx.

//^N	s_findlast [Scalar | Binary | String]
//^P	s_findlast (Src: String, Ctx: String) => Int
//^B	Search string backward for context.
//^D	Searches backward string \Src, looking for the last occurence of context \Ctx.
//^D	Returns: offset of \Ctx in \Src (starting from 0), or -1, if \Ctx was not found.
//^D	Syntax: \Src <<$ \Ctx.

	DefBuiltin (P_String_Search ("s_findfirst", Op_SFindFor, false));
	DefBuiltin (P_String_Search ("s_findlast", Op_SFindBak, true));

//^G	string

//^N	string [Scalar | Coercion | String]
//^P	string (V: Scalar) => String
//^B	Coerce scalar to string.
//^D	Explicit string coercion: from scalar \V to string value
//^\	(according to default coercion rules).

	DefBuiltin (P_String_Coerce ("string", Op_SCvt));

//^G	dc_sb dc_b dc_sw dc_w dc_l

//^N	dc_sb [String | Unary | Decoder]
//^P	dc_sb (S: String, [Offset: Int]) => Int
//^B	Decode signed byte.
//^D	Decode (from string fragment to 8 bit signed integer):
//^\	convert 1 byte of string \S at offset \Offset (defaults to 0) to signed integer.

//^N	dc_b [String | Unary | Decoder]
//^P	dc_b (S: String, [Offset: Int]) => Int
//^B	Decode unsigned byte.
//^D	Decode (from string fragment to 8 bit unsigned integer):
//^\	convert 1 byte of string \S at offset \Offset (defaults to 0) to unsigned integer.

//^N	dc_sw [String | Unary | Decoder]
//^P	dc_sw (S: String, [Offset: Int]) => Int
//^B	Decode signed word.
//^D	Decode (from string fragment to 16 bit signed integer):
//^\	convert 2 bytes of string \S at offset \Offset (defaults to 0) to signed integer.

//^N	dc_w [String | Unary | Decoder]
//^P	dc_w (S: String, [Offset: Int]) => Int
//^B	Decode unsigned word.
//^D	Decode (from string fragment to 16 bit unsigned integer):
//^\	convert 2 bytes of string \S at offset \Offset (defaults to 0) to unsigned integer.

//^N	dc_l [String | Unary | Decoder]
//^P	dc_l (S: String, [Offset: Int]) => Int
//^B	Decode long.
//^D	Decode (from string fragment to 32 bit signed/unsigned integer):
//^\	convert 4 bytes of string \S at offset \Offset (defaults to 0) to integer.

	DefBuiltin (P_String_DC ("dc_sb", Op_Null, -1));
	DefBuiltin (P_String_DC ("dc_sw", Op_Null, -2));
	DefBuiltin (P_String_DC ("dc_b", Op_Null, 1));
	DefBuiltin (P_String_DC ("dc_w", Op_Null, 2));
	DefBuiltin (P_String_DC ("dc_l", Op_Null, 4));

//^G	ec_b ec_w ec_l

//^N	ec_b [String | Unary | Encoder]
//^P	ec_b (Code: Int) => String
//^B	Encode byte.
//^D	Encode (from 8 bit signed/unsigned to string): 
//^\	convert integer \Code to string of length 1.

//^N	ec_w [String | Unary | Encoder]
//^P	ec_w (Code: Int) => String
//^B	Encode word.
//^D	Encode (from 16 bit signed/unsigned to string): 
//^\	convert integer \Code to string of length 2.

//^N	ec_l [String | Unary | Encoder]
//^P	ec_l (Code: Int) => String
//^B	Encode long.
//^D	Encode (from 32 bit signed/unsigned to string): 
//^\	convert integer \Code to string of length 4.

	DefBuiltin (P_String_EC ("ec_b", Op_Null, 1));
	DefBuiltin (P_String_EC ("ec_w", Op_Null, 2));
	DefBuiltin (P_String_EC ("ec_l", Op_Null, 4));

//^G	ec_b dc_b dc_sb
//^G	ec_w dc_w dc_sw
//^G	ec_l dc_l

//^G	s_map s_create

//^N	s_map [String | Unary]
//^P	s_map (Mapper: Func, S: String) => String
//^B	String character translation.
//^D	Returns new string, created by applying functor \Mapper to character codes in string \S.
//^D	(\Mapper is invoked as \Mapper ! (OldCode: Int) => NewCode and expected to return integer.)

	DefBuiltin (P_String_Map ("s_map", Op_Null));

//^N	s_create [String | Constructor]
//^P	s_create (Type: StringType, Length: Int, Mapper: Func) => String
//^B	String constructor.
//^D	Returns new string (with type \Type and length \Length), created by applying functor \Mapper to values in range 0 .. \Length.

	DefBuiltin (P_String_Create ("s_create", Op_Null));

//^G	s_join

//^N	s_join [String]
//^P	s_join (Separator: String, Items: List) => String
//^B	Join string list to string.
//^D	Returns result of concatenation of all items of \Items (with strings \Separator inserted between items).
//^D	Returns empty string, if \List is empty.
//^D	Warning: list \Items is expected to contain only strings (no implicit coercions).

	DefBuiltin (P_String_Join ("s_join", Op_Null));

//^G	s_range

//^N	s_range [String]
//^P	s_range (Type: StringType, Direction: Bool, FromChar: Int, ToChar: Int) => String
//^B	Create string from character range.
//^D	Result contains all characters from \FromChar up to \ToChar-1 (unless \Direction),
//^\	or all characters from \ToChar-1 down to \FromChar (if \Direction).
//^D	\Type defines type of result.

	DefBuiltin (P_String_Range ("s_range", Op_Null));

//^G	s_rev

//^N	s_rev [String | Unary]
//^P	s_rev (Str: String) => String
//^B	String reverse.
//^D	Returns reversion of string \Str.

	DefBuiltin (P_String_Rev ("s_rev", Op_SRev));

//^G	s_span_in s_span_ex s_rspan_in s_rspan_ex

//^N	s_span_in [String]
//^P	s_span_in (Source: String, Pred: Func) => Int
//^B	String span forward (inclusive).
//^D	Scans string \Source forward from start, while evaluation of \Pred returns !true.
//^D	Returns offset of first character, where \Pred evaluates to !false (or \Source length, if never).

//^N	s_span_ex [String]
//^P	s_span_ex (Source: String, Pred: Func) => Int
//^B	String span forward (exclusive).
//^D	Scans string \Source forward from start, while evaluation of \Pred returns !false.
//^D	Returns offset of first character, where \Pred evaluates to !true (or \Source length, if never).

	DefBuiltin (P_String_Span ("s_span_in", Op_Null, false, true));
	DefBuiltin (P_String_Span ("s_span_ex", Op_Null, false, false));

//^N	s_rspan_in [String]
//^P	s_rspan_in (Source: String, Pred: Func) => Int
//^B	String span backward (inclusive).
//^D	Scans string \Source backward from end, while evaluation of \Pred returns !true.
//^D	Returns offset of last character, where \Pred still evaluates to !true (or 0, if never).

//^N	s_rspan_ex [String]
//^P	s_rspan_ex (Source: String, Pred: Func) => Int
//^B	String span backward (exclusive).
//^D	Scans string \Source backward from end, while evaluation of \Pred returns !false.
//^D	Returns offset of last character, where \Pred still evaluates to !false (or 0, if never).

	DefBuiltin (P_String_Span ("s_rspan_in", Op_Null, true, true));
	DefBuiltin (P_String_Span ("s_rspan_ex", Op_Null, true, false));

//^G	s_filter_in s_filter_ex

//^N	s_filter_in [String]
//^P	s_filter_in (Source: String, Pred: Func) => String
//^B	String filter (inclusive).
//^D	Returns string, containing all characters in \Source, for which \Pred evaluates to !true
//^\	(or empty string, if \Pred is always !false).

//^N	s_filter_ex [String]
//^P	s_filter_ex (Source: String, Pred: Func) => String
//^B	String filter (exclusive).
//^D	Returns string, containing all characters in \Source, for which \Pred evaluates to !false
//^\	(or empty string, if \Pred is always !true).

	DefBuiltin (P_String_Filter ("s_filter_in", Op_Null, true));
	DefBuiltin (P_String_Filter ("s_filter_ex", Op_Null, false));

//^G	s_count_in s_count_ex

//^N	s_count_in [String]
//^P	s_count_in (Source: String, Pred: Func) => Int
//^B	String count (inclusive).
//^D	Returns total # of all characters in \Source, for which \Pred evaluates to !true (0, if none).

//^N	s_count_ex [String]
//^P	s_count_ex (Source: String, Pred: Func) => Int
//^B	String count (exclusive).
//^D	Returns total # of all characters in \Source, for which \Pred evaluates to !false (0, if none).

	DefBuiltin (P_String_Count ("s_count_in", Op_Null, true));
	DefBuiltin (P_String_Count ("s_count_ex", Op_Null, false));

//^G	cc_excl cc_incl

//^N	cc_excl [String | Func | Conditional | Composer]
//^P	cc_excl (CharSet: String) => Func
//^B	Charset predicate exclusive composer.
//^D	Create and return predicate, returning !true for characters present in \CharSet
//^\	(and !false for all other).

//^N	cc_incl [String | Func | Conditional | Composer]
//^P	cc_incl (CharSet: String) => Func
//^B	Charset predicate inclusive composer.
//^D	Create and return predicate, returning !false for characters present in \CharSet
//^\	(and !true for all other).

	DefBuiltin (P_CC_String ("cc_excl", Op_Null, false));
	DefBuiltin (P_CC_String ("cc_incl", Op_Null, true));

//^G	s_loop s_loop_r

//^N	s_loop [String | Iterator]
//^P	s_loop (Var: Mutable, Source: String, Body: Any) => Any
//^B	String direct iterator.
//^D	Evaluate \Body for all characters in \Source (from first to last),
//^\	assigning current character code to \Var on each iteration.

//^N	s_loop_r [String | Iterator]
//^P	s_loop_r (Var: Mutable, Source: String, Body: Any) => Any
//^B	String reverse iterator.
//^D	Evaluate \Body for all characters in \Source (from last to first),
//^\	assigning current character code to \Var on each iteration.

	DefBuiltin (P_String_Loop ("s_loop", Op_Null, false));
	DefBuiltin (P_String_Loop ("s_loop_r", Op_Null, true));

//^G	s_ord

//^N	s_ord [String]
//^P	s_ord (S: String, [Index: Int]) => Int
//^B	Get character code.
//^D	Returns code of character in \String with offset \Index (defaults to 0).
//^D	Returns !undef, if index out of range.

	DefBuiltin (P_String_Ord ("s_ord", Op_Null));

//^G	s_ucase s_lcase s_icase
//^G	s_ucfirst s_lcfirst s_icfirst

//^N	s_ucase [String | Unary]
//^P	s_ucase (S: String) => String
//^B	Upper case string.
//^D	Return string \S, converted to upper case.

//^N	s_lcase [String | Unary]
//^P	s_lcase (S: String) => String
//^B	Lower case string.
//^D	Return string \S, converted to lower case.

//^N	s_icase [String | Unary]
//^P	s_icase (S: String) => String
//^B	Invert string case.
//^D	Return string \S, with case inverted.

	DefBuiltin (P_String_ReCase ("s_ucase", Op_Null, ucase_ch, false));
	DefBuiltin (P_String_ReCase ("s_lcase", Op_Null, lcase_ch, false));
	DefBuiltin (P_String_ReCase ("s_icase", Op_Null, icase_ch, false));

//^N	s_ucfirst [String | Unary]
//^P	s_ucfirst (S: String) => String
//^B	Upper case string first character.
//^D	Return string \S, with only first character converted to upper case.

//^N	s_lcfirst [String | Unary]
//^P	s_lcfirst (S: String) => String
//^B	Lower case string first character.
//^D	Return string \S, with only first character converted to lower case.

//^N	s_icfirst [String | Unary]
//^P	s_icfirst (S: String) => String
//^B	Invert string case first character.
//^D	Return string \S, with with only first character case inverted.

	DefBuiltin (P_String_ReCase ("s_ucfirst", Op_Null, ucase_ch, true));
	DefBuiltin (P_String_ReCase ("s_lcfirst", Op_Null, lcase_ch, true));
	DefBuiltin (P_String_ReCase ("s_icfirst", Op_Null, icase_ch, true));

//^G	n_hex n_oct n_bin n_dec n__base

//^N	n_hex [String | Unary]
//^P	n_hex (S: String) => Int
//^B	Parse string as hexadecimal value.
//^D	Interpret string \S as hexadecimal integer, returning result.

//^N	n_dec [String | Unary]
//^P	n_dec (S: String) => Int
//^B	Parse string as decimal value.
//^D	Interpret string \S as decimal integer, returning result.

//^N	n_oct [String | Unary]
//^P	n_oct (S: String) => Int
//^B	Parse string as octal value.
//^D	Interpret string \S as octal integer, returning result.

//^N	n_bin [String | Unary]
//^P	n_bin (S: String) => Int
//^B	Parse string as binary value.
//^D	Interpret string \S as binary integer, returning result.

//^N	n__base [String | Unary]
//^P	n__base (B: Int, S: String) => Int
//^B	Parse string as value by base.
//^D	Interpret string \S as base \B encoded integer, returning result.

	DefBuiltin (P_ParseFixed ("n_hex", Op_Null, 16));
	DefBuiltin (P_ParseFixed ("n_dec", Op_Null, 10));
	DefBuiltin (P_ParseFixed ("n_oct", Op_Null, 8));
	DefBuiltin (P_ParseFixed ("n_bin", Op_Null, 2));

	DefBuiltin (P_ParseFixedBase ("n__base", Op_Null));

//^N	n_float [String | Binary]
//^P	n_float (S: String, F: Int) => Int
//^B	Parse string as float value.
//^D	Interpret string \S as float number, returning result.

	DefBuiltin (P_ParseFloat ("n_float", Op_Null));

//^G	s_hex s_oct s_bin s_dec s__base

//^N	s_hex [String | Unary]
//^P	s_hex (N: Int) => String
//^B	Convert signed integer to hexadecimal string.
//^D	Convert value \N to hexadecimal string, returning result.

//^N	s_dec [String | Unary]
//^P	s_dec (N: Int) => String
//^B	Convert signed integer to decimal string.
//^D	Convert value \N to decimal string, returning result.

//^N	s_oct [String | Unary]
//^P	s_oct (N: Int) => String
//^B	Convert signed integer to octal string.
//^D	Convert value \N to octal string, returning result.

//^N	s_bin [String | Unary]
//^P	s_bin (N: Int) => String
//^B	Convert signed integer to binary string.
//^D	Convert value \N to binary string, returning result.

//^N	s__base [String | Unary]
//^P	s__base (B: Int, N: Int) => String
//^B	Convert signed integer to string by base.
//^D	Convert value \N to binary string, base \B, returning result.

	DefBuiltin (P_FormatNum ("s_hex", Op_Null, 16, false));
	DefBuiltin (P_FormatNum ("s_Hex", Op_Null, 16, true));
	DefBuiltin (P_FormatNum ("s_dec", Op_Null, 10, false));
	DefBuiltin (P_FormatNum ("s_oct", Op_Null, 8, false));
	DefBuiltin (P_FormatNum ("s_bin", Op_Null, 2, false));

	DefBuiltin (P_FormatNumBase ("s__base", Op_Null, false));
	DefBuiltin (P_FormatNumBase ("s__Base", Op_Null, true));

//^G	s_ffloat s_efloat s_gfloat

//^N	s_ffloat [String | Binary]
//^P	s_ffloat (Val: Float, Prec: Int) => String
//^B	Convert float to string (F-format).
//^D	Convert value \Val to string with precision \Prec, returning result.

//^N	s_efloat [String | Binary]
//^P	s_efloat (Val: Float, Capacity: Int) => String
//^B	Convert float to string (E-format).
//^D	Convert value \Val to string with \Capacity significant digits, returning result.

//^N	s_gfloat [String | Binary]
//^P	s_gfloat (Val: Float, Capacity: Int) => String
//^B	Convert float to string (G-format).
//^D	Convert value \Val to string with \Capacity significant digits, returning result.

	DefBuiltin (P_FormatFloat ("s_ffloat", Op_Null, FC_format_f));
	DefBuiltin (P_FormatFloat ("s_efloat", Op_Null, FC_format_e));
	DefBuiltin (P_FormatFloat ("s_gfloat", Op_Null, FC_format_g));

//^G	s_poke s_patch

//^N	s_poke [String | Mutator]
//^P	s_poke (Str: Mutable, Index: Int, Code: Int) => String
//^B	Replace character in string mutable.
//^D	Replace charcter (at \Index) in mutable string \Str with code \Code (returning result).

//^N	s_patch [String | Mutator]
//^P	s_patch (Str: Mutable, Where: Range, Replacer: String) => String
//^B	Replace fragment in string mutable.
//^D	Replace fragment (at \Range) in mutable string \Str with string \Replacer (returning result).

	DefBuiltin (P_StringPoke ("s_poke", Op_Null));
	DefBuiltin (P_StringPatch ("s_patch", Op_Null));

//^G	s_common_head s_common_tail

//^N	s_common_head [String | Scalar]
//^P	s_common_head (Str1: String, Str2: String) => String
//^B	Common string head.
//^D	Find and return common beginning of strings \Str1 and \Str2.

//^N	s_common_tail [String | Scalar]
//^P	s_common_tail (Str1: String, Str2: String) => String
//^B	Common string tail.
//^D	Find and return common ending of strings \Str1 and \Str2.

	DefBuiltin (P_String_Common ("s_common_head", Op_Null, false));
	DefBuiltin (P_String_Common ("s_common_tail", Op_Null, true));

//
//	Character codes
//

//^G	cc_lower cc_upper cc_alpha cc_digit cc_odigit cc_xdigit cc_blank

//^N	cc_lower [Character | Predicate]
//^P	cc_lower (CharCode: Int) => Bool
//^B	Check code for lowercase letter.
//^D	True, if \CharCode is lowercase letter.

//^N	cc_upper [Character | Predicate]
//^P	cc_upper (CharCode: Int) => Bool
//^B	Check code for uppercase letter.
//^D	True, if \CharCode is uppercase letter.

//^N	cc_alpha [Character | Predicate]
//^P	cc_alpha (CharCode: Int) => Bool
//^B	Check code for any letter.
//^D	True, if \CharCode is letter.

//^N	cc_digit [Character | Predicate]
//^P	cc_digit (CharCode: Int) => Bool
//^B	Check code for decimal digit.
//^D	True, if \CharCode is decimal digit.

//^N	cc_odigit [Character | Predicate]
//^P	cc_odigit (CharCode: Int) => Bool
//^B	Check code for octal digit.
//^D	True, if \CharCode is octal digit.

//^N	cc_xdigit [Character | Predicate]
//^P	cc_xdigit (CharCode: Int) => Bool
//^B	Check code for hex digit.
//^D	True, if \CharCode is hexadecimal digit.

//^N	cc_blank [Character | Predicate]
//^P	cc_blank (CharCode: Int) => Bool
//^B	Check code for blank character.
//^D	True, if \CharCode is blank char.

	DefBuiltin (P_CC_Func ("cc_lower", ischar_lower));
	DefBuiltin (P_CC_Func ("cc_upper", ischar_upper));
	DefBuiltin (P_CC_Func ("cc_alpha", ischar_alpha));
	DefBuiltin (P_CC_Func ("cc_digit", ischar_digit));
	DefBuiltin (P_CC_Func ("cc_odigit", ischar_odigit));
	DefBuiltin (P_CC_Func ("cc_xdigit", ischar_xdigit));
	DefBuiltin (P_CC_Func ("cc_blank", ischar_blank));
	DefBuiltin (P_CC_Func ("cc_print", ischar_print));

//^G	s_encode s_decode

//^N	s_encode [String | Codec]
//^P	s_encode (Encoder: Codec, Raw: String) => String
//^B	Encode raw string with codec.
//^D	Encode source string \Raw (using codec \Encoder), returning (cooked) result.

//^N	s_decode [String | Codec]
//^P	s_decode (Decoder: Codec, Cooked: String) => String
//^B	Decode cooked string with codec.
//^D	Decode string \Cooked (using codec \Decoder), returning (raw) result.

	DefBuiltin (P_String_Encode ("s_encode", Op_Null));
	DefBuiltin (P_String_Decode ("s_decode", Op_Null));

//^N	s_chars [String]
//^P	s_chars (Type: StringType, Code: Int, ...) => String
//^B	Make string from character list.
//^D	Make and return string (of character \Type) from list of characters with \Code (s).

	DefBuiltin (P_String_Chars ("s_chars", Op_Null));

return true;
}	// init_primaries_string

DefSubSystem ("string", init_primaries_string, 0);

