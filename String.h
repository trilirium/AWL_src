
/*

	+---+---+---+---+---+---+
	|	"String.h":
	|	Common string pointer macros.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

//
//	String character types
//

//	1 byte character: type 0
typedef	unsigned char C_byte;

//	2 byte character: type 1
typedef	unsigned short C_word;

//	4 byte character: type 2
typedef	unsigned C_long;

//
//	Macro defs
//

// Scale string 'len' for elements 'type'
#define	s_scale(len,type)				\
	((len) << (type))

// Offset by 'off' from 'base' for elements 'type'
#define	s_offset(base,off,type)			\
	((C_byte *)(base) + ((off) << (type)))

// Difference between pointers 'ptr1' & 'ptr2' (of elements 'type')
#define s_diff(ptr1,ptr2,type) 			\
	(((C_byte *)ptr2 - (C_byte *)ptr1) >> (type))

// Increment 'ptr' (to elements 'type')
#define	s_inc(type,ptr)									\
	( (type) ?											\
		(type > 1) ? (void *)(((C_long *&) ptr) ++) :	\
			(void *)(((C_word *&) ptr) ++) :			\
		(void *)(((C_byte *&) ptr) ++) )

// Decrement 'ptr' (to elements 'type')
#define	s_dec(type,ptr)									\
	( (type) ?											\
		(type > 1) ? (void *)(((C_long *&) ptr) --) :	\
			(void *)(((C_word *&) ptr) --) :			\
		(void *)(((C_byte *&) ptr) --) )

// Read single element of 'type' from 'ptr'
#define	s_read(type,ptr)						\
	( (type) ?									\
		(type > 1) ? *((C_long *) ptr) :		\
			*((C_word *) ptr) :					\
		*((C_byte *) ptr) )

// Read single element of 'type' at 'base[off]'
#define	s_read_at(type,base,off)				\
	s_read(type, s_offset(base, off, type))

// Read single element of 'type' from 'ptr', with post-increment
#define	s_read_inc(type,ptr)					\
	( (type) ?									\
		(type > 1) ? *((C_long *&) ptr)++ :		\
			*((C_word *&) ptr)++ :				\
		*((C_byte *&) ptr)++ )

// Read single element of 'type' from 'ptr', with pre-decrement
#define	s_read_dec(type,ptr)					\
	( (type) ?									\
		(type > 1) ? * --((C_long *&) ptr) :	\
			* --((C_word *&) ptr) :				\
		* --((C_byte *&) ptr) )

// Write element 'code' of 'type' to 'ptr'
#define s_write(type,ptr,code)							\
	( (type) ?											\
		(type > 1) ? (*((C_long *) ptr) = code) :		\
			(*((C_word *) ptr) = code) :				\
		(*((C_byte *) ptr) = code) )

// Write element 'code' of 'type' to 'ptr' (with post-increment)
#define s_write_inc(type,ptr,code)						\
	( (type) ?											\
		(type > 1) ? (*((C_long *&) ptr)++ = code) :	\
			(*((C_word *&) ptr)++ = code) :				\
		(*((C_byte *&) ptr)++ = code) )

// Write element 'code' of 'type' to 'ptr' (with pre-decrement)
#define s_write_dec(type,ptr,code)						\
	( (type) ?											\
		(type > 1) ? (* --((C_long *&) ptr) = code) :	\
			(* --((C_word *&) ptr) = code) :			\
		(* --((C_byte *&) ptr) = code) )

// Advance 'ptr' (of character 'type') forward by 'off' chars
#define	s_forward(ptr,off,type)							\
	(ptr = s_offset(ptr, off, type))

// Advance 'ptr' (of character 'type') backward by 'off' chars
#define	s_backward(ptr,off,type)						\
	(ptr = s_offset(ptr, -off, type))

// Copy substring of 'len' characters 'type', from 'from_ptr' to 'to_ptr'
#define	s_mcopy(to_ptr,from_ptr,len,type)				\
	(memcpy (to_ptr, from_ptr, s_scale(len, type)))

// Compare substring of 'len' characters 'type', 'left_ptr' with 'right_ptr'
#define	s_mcomp(left_ptr,right_ptr,len,type)				\
	(memcmp (left_ptr, right_ptr, s_scale(len, type)))

