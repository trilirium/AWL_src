
/*

	+---+---+---+---+---+---+
	|	"StringR.cpp":
	|	StringData internals.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include <string.h>

#include "Defs.h"
#include "String.h"

//
//	TODO: make NULL string reusable
//	TODO: use fixed buffer for 1-length strings...
//

//
//	String content data
//	(used internally in X_String)
//
struct StringData {
	unsigned refs;			// (reference count for self)
	unsigned len;			// (length & type)
	char data[0];			// (if string length > 0, allocated on demand)

	// Allocate new instance
	static StringData *alloc (unsigned len, C_type type, char const *tag);

	// Release allocated instance
	static void release (StringData *&self);
	};

#define	TYPE_FLAG0	0x80000000
#define TYPE_FLAG1	0x40000000

StringData *StringData::alloc (unsigned len, C_type type, char const *tag) {
if (len) {
	StringData *data = (StringData *) operator new
		(sizeof (StringData) + (len << type), tag);
	data->len = len;
	if (type) {
		data->len |= TYPE_FLAG0;
		if (type > 1)
			data->len |= TYPE_FLAG1;
		} 
	data->refs = 0;
	return data;
	}
else return 0;
}	// StringData::alloc

#define	GET_TYPE(len)	(len & TYPE_FLAG0 ? (len & TYPE_FLAG1 ? 2 : 1) : 0)

void StringData::release (StringData *&self) {
operator delete (self);
self = 0;		// (for extra safety)
}	// StringData::release

// Construct new string : (len, type)
X_String::X_String (unsigned len, C_type type) {
if (len) {
	content = StringData::alloc (len, type, "alloc");
// tt	content->type = type;
	content->refs = 1;
	}
else
	content = 0;

offset = 0, length = len;
}	// X_String::X_String

// Construct new string (from C-string 'c_str')
DL_EXPORT X_String::X_String (char const *c_str) {
unsigned len = c_str ? strlen (c_str) : 0;
if (len) {
	content = StringData::alloc (len, 0, "c_str");
// tt	content->type = 0;
	content->refs = 1;
	memcpy (content->data, c_str, len);
	}
else
	content = 0;

offset = 0, length = len;
}	// X_String::X_String

// New-style constructor
X_String::X_String (char *source, unsigned len, C_type type) {
if (len) {
	content = StringData::alloc (len, type, "source");
// tt	content->len = len;			// ?????
	content->refs = 1;
	memcpy (content->data, source, len << type);
	}
else
	content = 0;

offset = 0, length = len;
}	// X_String::X_String

// Construct new string
// (from fragment of existing string)
X_String::X_String (X_String *string, unsigned offset, unsigned length) {
if (content = string->content)
	content->refs ++;

this->offset = offset, this->length = length;
}	// X_String::X_String

// Create another fragment of existing string
X_String::X_String (S_string &s_str) {
if (content = s_str.content)
	content->refs ++;

this->offset = s_str.offset, this->length = s_str.length;
}	// X_String::X_String

// Destruct string
void X_String::release () {
if (content && !-- content->refs)
	StringData::release (content);

Expr::release ();
}	// X_String::release

// Return buffer refcount
S_fixed X_String::refcount () {
return content ? (S_fixed) content->refs : -1;
}	// X_String::refcount

// Force change string buffer refcount
S_fixed X_String::refcount_by (S_fixed change) {
return content ? (S_fixed)(content->refs += change) : -1;
}	// X_String::refcount_by

// Link s_string
DL_EXPORT void S_string::link () {
if (content) content->refs ++;
}	// S_string::link

// Unlink s_string
DL_EXPORT void S_string::unlink () {
if (content && !-- content->refs)
	StringData::release (content);
}	// S_string::unlink

// Relink s_string
DL_EXPORT void S_string::relink () {
if (content && !content->refs)
	StringData::release (content);
}	// S_string::relink

S_fixed S_string::refcount_by (S_fixed change) {
return content ? (S_fixed)(content->refs += change) : -1;
}	// S_string::refcount_by

// Allocate new string of length 'len' and type 'type' (with allocation 'tag')
// (returns string pointer)
DL_EXPORT str_ptr S_string::alloc (char const *tag, unsigned len, C_type type) {
content = StringData::alloc (len, type, tag);
offset = 0; length = len;

return len ? content->data : 0;
}	// S_string::alloc

// Initialize (from C-string 'c_str')
DL_EXPORT void S_string::from_cstr (char *c_str) {
unsigned len = c_str ? strlen (c_str) : 0;
str_ptr ptr = alloc ("c_str", len, 0);
if (len) memcpy (ptr, c_str, len);
}	// S_string::from_cstr

// Fetch pointer to string
// (retrieves length 'len' and type 'type')
DL_EXPORT str_ptr S_string::fetch (unsigned &len, C_type &type) {
return (len = length) ?
	s_offset (content->data, offset, (type = GET_TYPE(content->len)))
	: (type = 0, (str_ptr) 0);
}	// S_string::fetch

// X_String fetch:
// returns pointer to string buffer;
// retrieves length 'len' and type 'type'
DL_EXPORT str_ptr X_String::fetch (unsigned &len, C_type &type) {
return (len = length) ?
	s_offset (content->data, offset, (type = GET_TYPE(content->len)))
	: (type = 0, (str_ptr) 0);
}	// X_String::fetch

// Make unique copy of X_String
// TODO: must be checked...
X_String *X_String::uniq () {

// if non-uniq string -- make copy
X_String *result = refs == 1 ? this :
	new ("X_String/uniq") X_String (this, offset, length);

StringData *r_content = result->content;
// if non-uniq string buffer -- make copy
if (r_content && r_content->refs != 1) {
	C_type type = GET_TYPE(r_content->len);
	if (length) {
		StringData *p_content = r_content;
		r_content = StringData::alloc (length, type, "String/uniq");
		r_content->refs = 1;
		s_mcopy (r_content->data, s_offset (p_content->data, offset, type), length, type);
		if (! -- p_content->refs)
			StringData::release (p_content);
		}
	else
		r_content = 0;
	offset = 0;
	}
result->content = r_content;

return result;
}	// X_String::uniq

