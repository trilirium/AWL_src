
/*

	+---+---+---+---+---+---+
	|	"Unicode.cpp":
	|	Character conversions, code pages & case mappings.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef HEADER

#include "Eval.h"

#include "String.h"

#include "Logger.h"

#endif

#include <mem.h>

//
//	Abstract code page definition
//

struct UC_CodePage {

	//	Abstract interfaceD

	UC_CodePage ();

	//	Encode character
	//	(Unicode -> CP code)
	virtual C_byte UC_encode (C_long code) = 0;

	//	Decode character
	//	(CP code -> Unicode)
	virtual C_long UC_decode (C_byte code) = 0;

	//	Dump to log
	virtual void UC_log (Logger &log) = 0;

	//
	//	(Global) Active instance
	//

	static struct UC_CodePage *active;
	};

//
//	Codepage mapped string reads/writes
//

// Conditionally Apply 'method' of 'object' to 'operand', if 'object' is defined
#define	UC_Apply(object,method,operand)						\
	((object) ? (object)->method(operand) : (operand))

// Decode (encoded) value
#define UC_Decode(enc_val)		UC_Apply (UC_CodePage::active, UC_decode, enc_val)

// Read single element of 'type' from 'ptr'
#define	UC_read(type,ptr)							\
	( (type) ?										\
		(type > 1) ? *((C_long *) ptr) :			\
			*((C_word *) ptr) :						\
				UC_Decode (*((C_byte *) ptr))		\
	)

// Read single element of 'type' from 'ptr' (with post-increment)
#define	UC_read_inc(type,ptr)						\
	( (type) ?										\
		(type > 1) ? *((C_long *&) ptr)++ :			\
			*((C_word *&) ptr)++ :					\
				UC_Decode (*((C_byte *&) ptr)++)	\
	)

// Read single element of 'type' from 'ptr' (with pre-decrement)
#define	UC_read_dec(type,ptr)						\
	( (type) ?										\
		(type > 1) ? * --((C_long *&) ptr) :		\
			* --((C_word *&) ptr) :					\
				UC_Decode (* --((C_byte *&) ptr))	\
	)

// Encode (decoded) value
#define UC_Encode(dec_val)		UC_Apply (UC_CodePage::active, UC_encode, dec_val)

// Write element 'code' of 'type' to 'ptr'
#define UC_write(type,ptr,code)							\
	( (type) ?											\
		(type > 1) ? (*((C_long *) ptr) = code) :		\
			(*((C_word *) ptr) = code) :				\
		(*((C_byte *) ptr) = UC_Encode (code))			\
	)

// Write element 'code' of 'type' to 'ptr' (with post-increment)
#define UC_write_inc(type,ptr,code)						\
	( (type) ?											\
		(type > 1) ? (*((C_long *&) ptr)++ = code) :	\
			(*((C_word *&) ptr)++ = code) :				\
		(*((C_byte *&) ptr)++ = UC_Encode (code))		\
	)

// Write element 'code' of 'type' to 'ptr' (with pre-decrement)
#define UC_write_dec(type,ptr,code)						\
	( (type) ?											\
		(type > 1) ? (* --((C_long *&) ptr) = code) :	\
			(* --((C_word *&) ptr) = code) :			\
		(* --((C_byte *&) ptr) = UC_Encode (code))		\
	)

//
//	Abstract case map definition
//

//	Conversion type
enum CM_type {
	CM_Lower,			// (lower case)
	CM_Upper,			// (upper case)
	CM_Title,			// (title case)
	CM_Invert,			// (invert case)
	CM_Compare			// (compare case)
	};

// (Abstract)
struct CaseMap_DefRange {
	C_long min, max;			// characters range: min..max
	int shift;					// character shift (lower -> upper)

	CaseMap_DefRange (C_long min, C_long max, int shift)
		{ this->min = min, this->max = max, this->shift = shift; }

	// convert code
	C_long convert (enum CM_type type, C_long code);

	CaseMap_DefRange *next;		// (next in chain)
	};

struct UC_CaseMap {

	//	Abstract interface

	//
	//	Compare two code points (a, b)
	//	Return:
	//		< 0	: (a <$ b)
	//		> 0	: (a >$ b)
	//		  0 : (a ==$ b)
	//

	int UC_compare (C_long a, C_long b);

	// (List of ranges)
	CaseMap_DefRange *range_list;

	// (Constructor)
	UC_CaseMap ();

	// Install new entry
	void install (CaseMap_DefRange *entry) {
		entry->next = range_list;
		range_list = entry;
		}	// install

	// (Destructor)
	~UC_CaseMap ();

	// Map character
	C_long convert (enum CM_type type, C_long code) {
		for (CaseMap_DefRange *range = range_list; range; range = range->next)
			code = range->convert (type, code);

		return code;
		}	// convert

	//
	//	(Global) Active instance
	//

	static struct UC_CaseMap *active;
	};

#ifndef HEADER

//	(constructor)
UC_CaseMap::UC_CaseMap () { range_list = 0; }

//	(destructor)
UC_CaseMap::~UC_CaseMap () {
CaseMap_DefRange *entry;
while (entry = range_list) {
	range_list = entry->next;
	delete entry;
	}
}	// UC_CaseMap::~UC_CaseMap

// convert code
C_long CaseMap_DefRange::convert (enum CM_type type, C_long code) {
switch (type) {
	case CM_Lower:
		if (min + shift <= code && code < max + shift)
			code -= shift;
		break;

	case CM_Upper:
		if (min <= code && code < max)
			code += shift;
		break;

	case CM_Invert:
		if (min <= code && code < max)
			code += shift;
		else if (min + shift <= code && code < max + shift)
			code -= shift;
		break;
	}	// switch (type)

return code;
}	// CaseMap_DefRange::convert

UC_CodePage::UC_CodePage () {
}

// Active code page
UC_CodePage *UC_CodePage::active = 0;

// Active case map
UC_CaseMap *UC_CaseMap::active = 0;

// Type expected error
struct BadRangeError : ExecError {
	S_fixed from, to, len;

	BadRangeError (S_fixed from, S_fixed to, S_fixed len)
		{ this->from = from; this->to = to; this->len = len; }

	void _report (Logger &log) {
		log.put_cstr ("Bad codepage range: ")->
			log_fixed (from)->put_cstr ("..")->log_fixed (to)->
			put_cstr (" [")->log_fixed (len)->put_ch (']')->put_nl ();
		}	// _report

	};

//
//	Implement codepage
//

struct UC_CodePage_Map : UC_CodePage {
	// [Internals]

	// Description string
	char *title;

	// maps character range [0x80..0xFF]
	C_word decode_table [0x80];

	unsigned slot_count;		// (used slots in unmap table)
	C_byte (*encode_table) [0x100];

	// unmap table
	// ...

	// Constructor from argument list
	UC_CodePage_Map (Expr *args, Prefix *where);

	// Destructor
	~UC_CodePage_Map ();

	//	Encode character
	//	(Unicode -> CP code)
	C_byte UC_encode (C_long code);

	//	Decode character
	//	(CP code -> Unicode)
	C_long UC_decode (C_byte code);

	// Output to log
	void UC_log (Logger &log);
	
	};	// UC_CodePage_Map

// Initialise from argument list
UC_CodePage_Map::UC_CodePage_Map (Expr *args, Prefix *where) {
unsigned count = 0;
Expr *elem;

// Reset table to default (identity mapping)
for (int i = 0x80; i != 0x100; ++ i)
	{ decode_table[i - 0x80] = i; }

// Parse description
S_string codepage;
title =
	where->expect_string (get_arg (args), codepage) ?
		codepage.to_cstring () : 0;

while (elem = get_arg (args)) {
	// (parse entry)
	S_fixed code_from = where->expect_fixed (get_arg (elem), -1);
	S_fixed code_to = where->expect_fixed (get_arg (elem), -1);
	S_fixed len = where->expect_fixed (elem, 1);

	// Map range
	if (0x80 <= code_from && code_from < 0x100) {
		code_from -= 0x80;

		if (0 < len && code_from + len <= 0x100) {
		if (0 <= code_to && code_to < 0x10000) {
			// (valid range map)
			while (len --) decode_table[code_from ++] = code_to ++;
			}
		else {
			// (mark unmapped range)
			while (len --) decode_table[code_from ++] = 0xFFFF;
			}

		++ count;
		continue;
		}
		}

	Module::report (new BadRangeError (code_from, code_to, len));
	}	// while (elem)

//	Intitialise (un)mapping table
C_byte val_count [0x100];
memset (val_count, 0, 0x100);
int i, j;

// Reset table to default (identity mapping)
for (i = 0; i != 0x80; ++ i) {
	C_word code = decode_table[i];
	if (code != 0xFFFF)
		++ val_count[(C_byte)(code >> 8)];
	}

// Slots count and allocation
slot_count = 0;
for (j = 0; j != 0x100; ++ j)
	val_count[j] = val_count[j] ? ++ slot_count : 0;

// Allocate unmapping table
encode_table = new ("Unmap_tab") C_byte [slot_count + 1][0x100];
memset (encode_table, 0, (slot_count + 1) << 8);

// Load unmapping table
for (j = 0; j != 0x100; ++ j)
	encode_table[0][j] = val_count[j];

for (i = 0; i != 0x80; ++ i) {
	C_word code = decode_table[i];
	if (code != 0xFFFF) {
		unsigned index = encode_table[0][(C_byte)(code >> 8)];
		encode_table[index][(C_byte) code] = i + 0x80;
		}
	}
}	// UC_CodePage_Map::UC_CodePage_Map

// Destructor
UC_CodePage_Map::~UC_CodePage_Map () {
delete title;
delete encode_table;
}	// UC_CodePage_Map::~UC_CodePage_Map

// Output to log
void UC_CodePage_Map::UC_log (Logger &log) {
// (Log title)
log.put_cstr ("Codepage: \"")->put_cstr (title)->put_cstr ("\"")->put_nl ();

// (Log mapping table)
C_word *tab_ptr = decode_table;

log.put_cstr (" / ");
for (unsigned col = 0; col != 8; ++ col)
	log.put_cstr ("  \t+")->put_hex (1, col)->put_ch ('|')->put_hex (1, 8 + col);
log.put_nl ();

for (unsigned row = 0; row != 16; ++ row) {
	log.put_ch (' ')->put_hex (2, 0x80 + row*8)->put_ch (':');

	for (unsigned col = 0; col != 8; ++ col) {
		C_word code = *tab_ptr ++;

		log.put_ch ('\t');
		if (code == 0x80 + (row << 3) + col) {
			// (identity mapping)
			log.put_ch ('<')->put_hex (2, code)->put_ch ('>');
			}
		else if (code == 0xFFFF)
			// (unmapped codepoint)
			log.put_cstr (" ** ");
		else
			log.put_hex (4, code);
		}	// for (col)

	log.put_nl ();
	}	// for (row)

// (Log unmap table)
log.put_cstr ("\n[Unmapping]\n");

for (unsigned slot = 0; slot != slot_count+1; ++ slot) {
	C_byte *table_slot = encode_table[slot];

	if (slot) {
		log.put_cstr ("\n\tCode slot: #")->log_fixed (slot)->put_cstr (" +[");
		for (unsigned index = 0; index != 0x100; ++ index)
			if (encode_table[0][index] == slot)
				{ log.put_hex (4, index << 8); break; }
		log.put_ch (']');
		}
	else
		log.put_cstr ("\n\tRoot slot:");
	log.put_nl ();

	log.put_ch ('\t');
	for (unsigned col = 0; col != 0x10; ++ col) {
		log.put_ch ('+')->put_hex (1, col)->put_ch (' ');
		}
	log.put_nl ();

	for (unsigned row = 0; row != 0x10; ++ row) {
		unsigned lastcol = 0x10;
		while (lastcol && ! table_slot[lastcol - 1]) -- lastcol;

		log.put_cstr ("  ")->put_hex (2, row << 4)->put_cstr (":\t");

		for (unsigned col = 0; col != lastcol; ++ col) {
			C_byte value = *table_slot ++;
			if (value)
				log.put_hex (2, value);
			else
				log.put_cstr ("..");

			log.put_ch (' ');
			}

		table_slot += 0x10 - lastcol;
		log.put_nl ();
		}
	}
}	// UC_CodePage_Map::UC_log

//	Decode character
//	(CP code -> Unicode)
C_long UC_CodePage_Map::UC_decode (C_byte code) {
return code < 0x80 ? code : decode_table[code - 0x80];
}	// UC_CodePage_Map::UC_decode

//	Encode character
//	(Unicode -> CP code)
C_byte UC_CodePage_Map::UC_encode (C_long code) {
if (code < 0xFFFF) {
	unsigned offset = encode_table[0][code >> 8];
	if (offset)
		return encode_table[offset][code & 0xFF];
	}

return 0;		// (Cannot be encoded!)
}	// UC_CodePage_Map::UC_encode

//
//	Functor defs
//

//
//	Active codepage wrapper
//

struct P_CodePage : P_Wrapper {
	P_CodePage (char const *ident, O_Enum op) : P_Wrapper (ident, op) {}

	void eval (WrapX &wrapper, Expr *args) {
		Expr *cp_table = get_arg (args);
		UC_CodePage_Map code_map (cp_table, this);

		struct UC_CodePage *outer_map = UC_CodePage::active;
		UC_CodePage::active = &code_map;

		wrapper.eval (args);

		UC_CodePage::active = outer_map;
		}	// eval

	};	// P_CodePage

//
//	Dumping
//

void show_codemap (Logger &logger, UC_CodePage *codepage) {
if (codepage)
	codepage->UC_log (logger);
else
	logger.put_cstr ("[Undefined codepage]\n");
}	// show_codemap

struct P_DumpCodepage : PrefixX {

	P_DumpCodepage (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		show_codemap (*syslog, UC_CodePage::active);
		return 0;
		}	// evalX

	};	// P_DumpCodepage

//
//	Encode/decode character via codepage
//
struct P_EncDecChar : Prefix {
	bool flag;		// ? encode : decode

	P_EncDecChar (char const *ident, bool opmode) : Prefix (ident, Op_Null) { this->flag = flag; }

	VType evalV (VDatum &val, Expr *args) {
		S_fixed code = expect_fixed (args, 0);

		if (code >= 0) {
			val._fixed = flag ? UC_Encode(code) : UC_Decode(code);
			return T_fixed;
			}

		return T_undef;
		}	// evalV

	};	// P_EncDecChar

//
//	Active case map wrapper
//

// Install entry to casemap
struct P_CaseMapDef : Prefix {

	P_CaseMapDef (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		UC_CaseMap *map = UC_CaseMap::active;
		unsigned count = 0;
		Expr *entry;

		if (map) {
		while (entry = get_arg (args)) {
		S_fixed min_code = expect_fixed (get_arg (entry), 0);
		S_fixed max_code = expect_fixed (get_arg (entry), 0);
		S_fixed shift = expect_fixed (entry, 0);

		map->install (new ("CaseMapper") CaseMap_DefRange (min_code, max_code, shift));
		++ count;
		}	// while

		}	// (map)
		
		val._fixed = count;
		return T_fixed;
		}	// evalV

	};	// P_CaseMapDef

// Dump active casemap
struct P_CaseMapDump : Prefix {

	P_CaseMapDump (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		UC_CaseMap *map = UC_CaseMap::active;

		if (map) {
			for (CaseMap_DefRange *entry = map->range_list; entry; entry = entry->next)
				syslog->put_cstr ("CaseRange: ")->
					put_hex (4, entry->min)->put_ch (':')->put_hex (4, entry->max)->
					put_ch ('@')->log_fixed (entry->shift)->put_nl ();
			}
		}

	};	// P_CaseMapDump

//
//	Map character case
//

struct P_RecaseChar : Prefix {
	CM_type type;

	P_RecaseChar (char const *ident, CM_type type) : Prefix (ident, Op_Null)
		{ this->type = type; }

	VType evalV (VDatum &val, Expr *args) {
		C_long code = expect_fixed (args, 0);
		val._fixed = (UC_CaseMap::active) ? UC_CaseMap::active->convert (type, code) : code;
		return T_fixed;
		}	// evalV

	};	// P_RecaseChar

//
//	Unicode collation
//

typedef unsigned short weight_vec[3];

struct Collate {
	enum { SlotsTotal = 256 };
	static struct CollatorSlot *slots [SlotsTotal];

	static struct CollatorSlot * install_slot (unsigned slot_index, struct CollatorSlot *slot);

	static void expand_char_weight (C_long code, Expr * &output);

	static void dump_slot (unsigned slot_index);

	static int compare_codepoints (C_long code_left, C_long code_right, unsigned level, int displace);

	};	// Collate

struct CollatorSlot *Collate::slots [SlotsTotal];

//
//	Define collation slot
//

struct CollatorSlot {
	// array counts
	unsigned index_count, weight_count;

	// array data
	unsigned *cindex_array;				// characters -> windex
	unsigned *windex_array;				// weight index
	weight_vec *weight_array;			// actual weights

	// (constructor)
	CollatorSlot (Prefix *where,
		unsigned index_count, Expr *index_list,
		unsigned weights_count, Expr *weights_list);

	// (destructor)
	~CollatorSlot ();
	};	// CollatorSlot

// Install 'slot_data' to 'slot_index'
// (returning previous)
CollatorSlot *Collate::install_slot (unsigned slot_index, CollatorSlot *slot_data) {
if ((slot_index >>= 8) < SlotsTotal) {
	CollatorSlot *prev = slots[slot_index];
	slots[slot_index] = slot_data;
	return prev;
	}

return 0;
}	// Collate::install_slot

// Dump data at 'slot_index'
void Collate::dump_slot (unsigned slot_index) {
if ((slot_index >> 8) < SlotsTotal) {
	CollatorSlot *slot = slots[slot_index >> 8];

	if (slot) {
	unsigned index_count = slot->index_count;
	unsigned weight_count = slot->weight_count;
	syslog->put_ch ('[')->
		put_ch ('#')->put_hex (4, slot_index)->put_cstr (": ")->
		log_fixed (index_count)->put_cstr (" / ")->log_fixed (weight_count)->
		put_ch (']')->put_nl ();

	weight_vec *weight_array = slot->weight_array;
	unsigned *windex_array = slot->windex_array;
	unsigned *cindex_array = slot->cindex_array;

	for (unsigned i = 0; i != 256; ++ i) {
		syslog->put_hex (4, slot_index + i)->put_cstr (":\t");

		unsigned cindex_start = cindex_array[i];
		unsigned cindex_count = cindex_array[i+1] - cindex_start;

		while (cindex_count --) {
			unsigned windex = windex_array[cindex_start ++];
			weight_vec &weight = weight_array[windex];
			syslog->
				put_ch ('[')->
					put_hex (4, weight[0])->
						put_ch ('.')->
					put_hex (4, weight[1])->
						put_ch ('.')->
					put_hex (4, weight[2])->
				put_ch (']');
			}	// while (cindex_count)

		syslog->put_nl ();
		};	// for (i)
	syslog->put_nl ();
	}
}
}	// Collate::dump_slot

void Collate::expand_char_weight (C_long code, Expr * &output) {
unsigned slot_index;

if ((slot_index = code >> 8) < SlotsTotal) {
	CollatorSlot *slot = slots[slot_index];

	if (slot) {
	code &= 0xFF;
	weight_vec *weight_array = slot->weight_array;
	unsigned *windex_array = slot->windex_array;
	unsigned *cindex_array = slot->cindex_array;
	unsigned i_end = cindex_array[code + 1], i_start = cindex_array[code];

	while (i_end != i_start) {
		unsigned windex = windex_array [-- i_end];
		weight_vec &weight = weight_array[windex];

		Expr *Weight = 
			new ("Char_Weight") X_List (
			new ("Char_Weight/0") X_Fixed (weight[0]),
			new ("Char_Weight") X_List
				(
				new ("Char_Weight/1") X_Fixed (weight[1]),
				new ("Char_Weight/2") X_Fixed (weight[2])
				)
			);

		output = new ("WeightNode") X_List (Weight, output);
		}
	}
	}
}	// Collate::expand_char_weight

// Collator slot constructor
CollatorSlot::CollatorSlot (Prefix *where,
	unsigned index_count, Expr *index_list,
	unsigned weights_count, Expr *weights_list) {
unsigned i, j;
Expr *arg;

// TODO: verify source data!

// (Store weight data)
weight_vec *entry = weight_array = new weight_vec [this->weight_count = weights_count];
weight_vec weight;
weight[0] = weight[1] = weight[2] = 0;

for (i = 0; i != weights_count; ++ i) {
	weight_vec &entry = weight_array[i];
	Expr *arg = get_arg (weights_list);

	if (arg) {		// (cumulative)
		// (difference from prev)
		entry[0] = weight[0] += where->expect_fixed (get_arg (arg), 0);
		// (XOR with prev)
		entry[1] = weight[1] ^= where->expect_fixed (get_arg (arg), 0);
		// (XOR with prev)
		entry[2] = weight[2] ^= where->expect_fixed (arg, 0);
		}
		else		// (clear entry?)
			entry[0] = entry[1] = entry[2] = 0;
	}	// for (i)

// (Store index data)
windex_array = new unsigned [this->index_count = index_count];
cindex_array = new unsigned [256 + 1];

j = 0;
for (i = 0; i != 256; ++ i) {
	cindex_array [i] = j;

	Expr *arg = get_arg (index_list);
	if (arg) {
		Expr *x_arg;
		while (x_arg = get_arg (arg)) {
			if (j != index_count) windex_array [j ++] = where->expect_fixed (x_arg, 0);
			}
		}
	}	// for (i)

cindex_array [i] = j;
}	// CollatorSlot::CollatorSlot

// Collator slot destructor
CollatorSlot::~CollatorSlot () {
delete [] weight_array;
delete [] windex_array;
delete [] cindex_array;
}	// CollatorSlot::~CollatorSlot

struct P_LoadCollation : Prefix {
	P_LoadCollation (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		// (initial character code)
		unsigned char_base = expect_fixed (get_arg (args), 0);
		CollatorSlot *slot = 0;

		if (args) {
			Expr *weight_index_list = get_arg (args);
			Expr *weight_list = get_arg (args);
			unsigned weights_count = expect_fixed (get_arg (args), 0);
			unsigned index_count = expect_fixed (args, 0);
			slot = new ("Collate::slot") CollatorSlot (this, index_count, weight_index_list, weights_count, weight_list);
			}

		slot = Collate::install_slot (char_base, slot);
		if (slot) delete slot;

		val._fixed = char_base;
		return T_fixed;
		}	// evalV

	};	// P_LoadCollation

struct P_DumpCollation : Prefix {
	P_DumpCollation (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		// (initial character code)
		unsigned char_base = expect_fixed (get_arg (args), 0);
		Collate::dump_slot (char_base);
		val._fixed = char_base;
		return T_fixed;
		}	// evalV

	};	// P_DumpCollation

Expr *expand_weight_list (S_string &s_src) {
Expr *result = 0;

unsigned len;
C_type type;
str_ptr src = s_src.fetch (len, type);

s_forward (src, len, type);
while (len --) {
	C_long code = UC_read_dec (type, src);
	Collate::expand_char_weight (code, result);
	}

return result;
}	// expand_weight_list

struct P_ListWeights : PrefixX {
	P_ListWeights (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		S_string src;
		if (expect_string (args, src)) {
			Expr *result = expand_weight_list (src);
			src.relink ();
			return result;
			}
		
		return 0;
		}	// evalX

	};	// P_ListWeights

//
//	Compare code points according to collation level
//

enum {
	UC_LT_MIN = -31,			// less than threshold
	UC_GT_MAX =  31,			// greater than threshold
	};

//
// For algorithm details:
//	http://www.unicode.org/reports/tr10/
//
//	(Still lots to be done.)
//

int Collate::compare_codepoints (C_long code_left, C_long code_right, unsigned level, int displace) {
unsigned index_left = code_left >> 8, index_right = code_right >> 8;
CollatorSlot
	*slot_left = index_left < SlotsTotal ? slots[index_left] : 0,
	*slot_right = index_right < SlotsTotal ? slots[index_right] : 0;

if (slot_left && slot_right && level < 3) {
	unsigned *cindex_left = slot_left->cindex_array + (code_left & 0xFF);
	unsigned *cindex_right = slot_right->cindex_array + (code_right & 0xFF);

	unsigned si_left = *cindex_left, ei_left = cindex_left[1];
	unsigned si_right = *cindex_right, ei_right = cindex_right[1];

	if (displace)
		if (displace < 0) {
			displace = -displace - 1;
			if (ei_left - si_left < displace)
				// (left displace too big!)
				return UC_LT_MIN;
			else si_left += displace;
			}
		else {
			-- displace;
			if (ei_right - si_right < displace)
				// (right displace too big!)
				return UC_GT_MAX;
			else si_right += displace;
			}

	unsigned *windex_left = slot_left->windex_array, *windex_right = slot_right->windex_array;
	weight_vec *weight_left = slot_left->weight_array, *weight_right = slot_right->weight_array;

	while (si_left != ei_left && si_right != ei_right) {
		int left = weight_left[windex_left[si_left ++]][level];
		int right = weight_right[windex_right[si_right ++]][level];

		if (left != right)
			if (! left)
				// zero left weight: skip it & recheck right
				-- si_right;
			else if (! right)
				// zero right weight: skip it & recheck left
				-- si_left;
			else
				// real difference found!
				return left > right ? UC_GT_MAX : UC_LT_MIN;
		}	// (done)

	if (si_right != ei_right)
		// left finished (and right not):
		// return right displacement + 1 (> 0)
		return 1 + si_right - *cindex_right;

	if (si_left != ei_left)
		// right finished (and left not):
		// return left displacement - 1 (< 0)
		return *cindex_left - si_left - 1;

	return 0;		// (total equality)
	}

// (fall back to codepoints compare)
return
	code_left < code_right ?
		UC_LT_MIN :
	code_left > code_right ?
		UC_GT_MAX :
	0;
}	// Collate::compare_codepoints

//
//	Collation key definition
//

struct CollationKey {
	bool debug;					// (force debug output)

	unsigned total;				// (indices to check)
	short index_list[4];		// (actual index list)

	// (used by default)
	static CollationKey default_key;
	};	// CollationKey

CollationKey CollationKey::default_key;

//
//	Compare strings according to collation key
//

static void Debug_begin (C_long code_left, C_long code_right, unsigned level, int displace) {
syslog->put_cstr ("Compare: ")->
	put_hex (4, code_left)->put_cstr (" : ")->put_hex (4, code_right)->
	put_cstr (" [")->log_fixed (level)->put_cstr ("/")->log_fixed (displace)->put_cstr ("]");
}	// Debug_begin

static void Debug_end (int result) {
syslog->put_cstr (" -> ")->put_hex (4, result)->put_cstr (".\n");
}	// Debug_end

//
//	Compare strings 's_left' && 's_right', according to collation 'key'
//
int collation_compare_strings (CollationKey &key, S_string &s_left, S_string &s_right) {
unsigned total = key.total;
short *indices = key.index_list;
bool debug = key.debug;

// (multi-pass compare -- according to 'key')
while (total --) {
	int index = *indices ++;
	int displace = 0;

	C_long code_left, code_right;
	C_type type_left, type_right;
	unsigned len_left, len_right;

	str_ptr src_left = s_left.fetch (len_left, type_left);
	str_ptr src_right = s_right.fetch (len_right, type_right);

	if (len_left && len_right) {
		code_left = UC_read_inc (type_left, src_left);
		code_right = UC_read_inc (type_right, src_right);
		-- len_left; -- len_right;
		}
	else
		return len_left ? 1 : len_right ? -1 : 0;

	for (;;) {
		if (debug) Debug_begin (code_left, code_right, index, displace);
		displace = Collate::compare_codepoints (code_left, code_right, index, displace);
		if (debug) Debug_end (displace);

		if (displace < 0)
			if (displace < UC_LT_MIN)
				// definitely less than!
				return -1;
			else
				// weights remaining in left -- read new right code
				if (len_right) {
					code_right = UC_read_inc (type_right, src_right);
					-- len_right;
					}
				else	// (left is greater!)
					return 1;
		else if (displace > 0)
			if (displace > UC_GT_MAX)
				// definitely greater than!
				return 1;
			else
				// weights remaining in right -- read new left code
				if (len_left) {
					code_left = UC_read_inc (type_left, src_left);
					-- len_left;
					}
				else	// (right in greater!)
					return -1;
		else
			// (exactly equal: fetch new codes)
		if (len_left && len_right) {
			code_left = UC_read_inc (type_left, src_left);
			-- len_left;
			code_right = UC_read_inc (type_right, src_right);
			-- len_right;
			}
		else
			if (len_left != len_right)
				return len_left ? 1 : -1;
			else
				// finished compare: go to next index
				break;
		}	// for (;;)

	}	// while (total)

// consider equal
return 0;
}	// collation_compare_strings

//
//	Read collation 'key' from 'args' (in functor 'where')
//

static void expect_collation_key (CollationKey &key, Expr *args, Prefix *where) {
key.debug = false;

key.total = 0;
while (key.total != 4 && args) {
	Expr *arg = get_arg (args);
	if (arg)
		key.index_list [key.total ++] = where->expect_fixed (arg, 0);
	else
		key.debug = true;
	}
}	// expect_collation_key

static Expr *list_collation_key (CollationKey &key) {
unsigned i = key.total;
Expr *result = 0;

while (i) {
	result = new ("Key/List") X_List (new X_Fixed (key.index_list [-- i]), result);
	}

return result;
}	// list_collation_key

//
//	Collation key wrapper
//

struct P_WithCollationKey : P_Wrapper {
	P_WithCollationKey (char const *ident, O_Enum op) : P_Wrapper (ident, op) {}

	void eval (WrapX &wrapper, Expr *args) {
		CollationKey active_key;
		expect_collation_key (active_key, get_arg (args), this);

		CollationKey prev_key = CollationKey::default_key;
		CollationKey::default_key = active_key;

		wrapper.eval (args);

		CollationKey::default_key = prev_key;
		}	// eval

	};	// P_WithCollationKey

//
//	Acquire active collation key
//

struct P_GetCollationKey : PrefixX {
	P_GetCollationKey (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		return list_collation_key (CollationKey::default_key);
		}	// evalX

	};	// P_GetCollationKey

//
//	Compare Unicode characters (according to weight index)
//

struct P_UC_Compare : Prefix {
	P_UC_Compare (char const *ident, bool opmode) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		C_long code_left = expect_fixed (get_arg (args), 0);
		C_long code_right = expect_fixed (get_arg (args), 0);
		int level_displace = expect_fixed (args, 0);

		val._fixed = Collate::compare_codepoints (code_left, code_right, level_displace & 0xFFFF, level_displace >> 8);
		return T_fixed;
		}	// evalV

	};	// P_UC_Compare

//
//	Compare Unicode strings (according to collation key)
//

struct P_UC_CompareString : Prefix {
	P_UC_CompareString (char const *ident, bool opmode) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		CollationKey compare_key;
		expect_collation_key (compare_key, get_arg (args), this);

		S_string src_left, src_right;
		if (expect_string (get_arg (args), src_left) &&
			expect_string (get_arg (args), src_right)) {

			val._fixed = collation_compare_strings (compare_key, src_left, src_right);

			src_left.relink ();
			src_right.relink ();
			return T_fixed;
			}
	
		return T_undef;
		}	// evalV

	};	// P_UC_CompareString

void clear_collation_map () {
}	// clear_collation_map

//
//
//	Initialise subsysytem
//
//

static bool init_primaries_unicode (int order) {

//^T	CodePageEntry
//^B	Definition of code page entry
//^D	Code page entry: (\CharIndex: Int, \Unicode: Int, \Counter: Int).

//^N	U_WithCodePage [Unicode | Wrapper]
//^P	U_WithCodePage ((Title: String, EntryList: CodePageEntry, ...), Body: Any) => Any
//^B	Codepage definition wrapper.
//^D	Set active codepage for evaluation of \Body.
//^D	Codepage is defined by \Title and list of entries \EntryList.

	DefBuiltin (P_CodePage ("U_WithCodePage", Op_Null));

//^N	U_encode [Unicode | Character | Debug]
//^P	U_encode (UniCode: Int) => Int
//^B	Convert character (Unicode to Codepage).
//^D	Convert Unicode character \UniCode to index in active codepage.

	DefBuiltin (P_EncDecChar ("U_encode", false));

//^N	U_decode [Unicode | Character | Debug]
//^P	U_decode (CharCode: Int) => Int
//^B	Convert character (Codepage to Unicode).
//^D	Convert character \CharCode from index in active codepage to Unicode.

	DefBuiltin (P_EncDecChar ("U_decode", true));

//^N	U_CP_dump [Unicode | Debug]
//^P	U_CP_dump () => Int
//^B	Dump current codepage.
//^D	Output active codepage information (including encode/decode tables) to log.

	DefBuiltin (P_DumpCodepage ("U_CP_dump"));

//^G	U_lower U_upper U_invert

//^N	U_lower [Unicode | Character | Debug]
//^P	U_lower (UniCode: Int) => Int
//^B	Convert character code to lower case.
//^D	Returns character \UniCode converted to lower case.

//^N	U_upper [Unicode | Character | Debug]
//^P	U_upper (UniCode: Int) => Int
//^B	Convert character code to upper case.
//^D	Returns character \UniCode converted to upper case.

//^N	U_invert [Unicode | Character | Debug]
//^P	U_invert (UniCode: Int) => Int
//^B	Invert case of character code.
//^D	Returns case inversion of character \UniCode (upper case <-> lower case).

	DefBuiltin (P_RecaseChar ("U_lower", CM_Lower));
	DefBuiltin (P_RecaseChar ("U_upper", CM_Upper));
	DefBuiltin (P_RecaseChar ("U_invert", CM_Invert));

	DefBuiltin (P_LoadCollation ("U_load_collation"));
	DefBuiltin (P_DumpCollation ("U_dump_collation"));

//^N	U_list_weights [Unicode | Character | Debug]
//^P	U_list_weights (Source: String) => List
//^B	List of string character weights.
//^D	Returns (open) list of collation weights in \Source.

	DefBuiltin (P_ListWeights ("U_list_weights"));

	DefBuiltin (P_WithCollationKey ("U_with_collator", Op_Null));
	DefBuiltin (P_GetCollationKey ("U_get_collator", Op_Null));

//^N	U_code_cmp [Unicode | Compare]
//^P	U_code_cmp (UniLeft: Int, UniRight: Int, Level: Int) => Sign
//^B	Compare character weights.
//^D	Returns result of comparison of characters \UniLeft and \UniRight (at \Level).

	DefBuiltin (P_UC_Compare ("U_code_cmp", Op_Null));

//^N	U_s_cmp [Unicode | Compare]
//^P	U_s_cmp (KeyIndex: List, StrLeft: String, StrRight: String) => Sign
//^B	Compare strings.
//^D	Returns result of comparison of strings \StrLeft and \StrRight, according to \KeyIndex.

	DefBuiltin (P_UC_CompareString ("U_s_cmp", Op_Null));

//^T	CaseMapEntry
//^B	Definition of case map entry
//^D	CaseMap entry: (\CodeLower: Int, \CaseUpper: Int, \Counter: Int).

//^N	U_load_casemap [Unicode | Character]
//^P	U_load_casemap (Entry: CaseMapEntry, ...) => Int
//^B	Load list of case map entries.
//^D	Load list of case map entries.

	DefBuiltin (P_CaseMapDef ("U_load_casemap"));

//^N	U_dump_casemap [Unicode | Character | Debug]
//^P	U_dump_casemap (Entry: CaseMapEntry, ...) => Int
//^B	Dump list of case map entries.
//^D	Dump list of case map entries.

	DefBuiltin (P_CaseMapDump ("U_dump_casemap"));

	UC_CaseMap::active = new ("Unicode_CaseMap") UC_CaseMap ();

	return true;
}	// init_primaries_unicode

//
//
//	Shutdown subsysytem
//
//

static bool shut_primaries_unicode (int order) {
clear_collation_map ();

delete UC_CaseMap::active;

return true;
}	// shut_primaries_unicode

DefSubSystem ("unicode", init_primaries_unicode, shut_primaries_unicode);

#endif

