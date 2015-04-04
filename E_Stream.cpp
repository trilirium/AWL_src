
/*

	+---+---+---+---+---+---+
	|	"E_Stream.cpp":
	|	Implementation of stream input/output primitives.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef HEADER

#include "Eval.h"

#include "Logger.h"

#ifdef TARGET_UNIX
	#include "memory.h"
#else
	#include "mem.h"
#endif

#include "String.h"

#include <fcntl.h>

#endif

// Stream runtime flags
enum f_flags {
	f_EOF = 1,			// EOF reached on read
	f_ERR = 2,			// Error occured
	};

// Stream opmode flags
enum stream_f {
	stream_read		= 1 << 16,		// (stream is readable)
	stream_write	= 1 << 17,		// (stream is writeable)
	stream_seek		= 1 << 18,		// (stream is seekable)
	};

//	Generic I/O subsystem
//	(defined in IOSys.cpp)
struct IOSystem;

// Generic stream object
struct Stream : Expr {
	IOSystem *io_layer;	// (use for stream I/O)

	unsigned o_flags;	// (stream access flags)
	unsigned flags;		// (stream runtime flags)

	// Common:

	// Open file stream
	// (for 'filename' with 'mode')
	bool open_file (Prefix *where, Expr *filename, unsigned mode);

	// Open file descriptor 'fd'
	// (for 'name', with 'mode')
	bool open_fd (int fd, char const *name, unsigned mode);

	// Close stream
	bool close ();

	// Low-level lseek
	int lseek (int position, int whence);

	// Tell current stream offset (from start)
	int tell ();

	// Seek absolute (from start/end, depending on positive/negative 'relpos')
	int seek (int relpos);

	// Seek relative (by positive/negative 'offset')
	int skip (int offset);

	// Read from (input) stream to 'in_buf' (total 'in_count' bytes)
	int read (void *in_buf, unsigned in_count);

	// Write to (output) stream from 'out_buf' (total 'out_count' bytes)
	int write (const void *out_buf, unsigned out_count);

	// Sync stream buffers
	int sync ();

	// End of stream reached?
	bool is_at_end ();

	// Console device?
	bool is_console ();

	// Query stream info
	Expr *query_info ();

	// Get current file size
	int get_size ();

	// Set current file size (expand/truncate)
	int set_size (unsigned new_size);

	// Control buffer bound to stream
	int buf_resize (unsigned size);

	// Allocate buffer
	void allocate_buf (unsigned size, unsigned origin);

	// Release buffer
	void release_buf ();

	//
	//	For input:
	//

	// Get raw octet from stream
	bool get_octet (char &octet);

	// Get (wide) character from stream
	bool get_wch (unsigned &w_ch);

	// Get raw data block from stream
	unsigned get_data (char *in_buf, unsigned in_len);

	// Get string list to mutables
	unsigned get_args (Expr *args);

	//
	//	For output:
	//

	// Put raw octet to stream
	bool put_octet (char octet);

	// Put (wide) character to stream
	bool put_wch (unsigned w_ch);

	// Put C-string 'str' (8 bit, null-terminated)
	void put_cstr (const char *str);

	// Put raw data block to stream
	void put_data (char const *out_buf, unsigned out_len);

	// Put coerced fixed value to stream
	void put_fixed (S_fixed fixed_val);

	// Put coerced float value to stream
	void put_float (S_float float_val);

	// Put list of functor/object arguments
	unsigned put_args (Expr *args);

	//
	// (Trivial) Constructor
	//
	Stream ();

	Stream (bool phase);

	//
	//	(virtuals)
	//

	D_Expr_evalV;
	D_Expr_log;
	D_Expr_release;
	};

//
//	Input stream definition
//
struct InStream : Stream {
	InStream (int fd, char const *name);
	};

//
//	Output stream definition
//
struct OutStream : Stream {
	OutStream (int fd, char const *name);
	};

//
//	Generic stream codec object
//

struct SCodec : Expr {

	// Encode 'input' to 'output'
	virtual unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input'
	virtual unsigned decode (ByteStream &input, CharStream &output);

	D_Expr_evalV;
	D_Expr_log;
	
	virtual void log_ex (Logger &log);
	};

//
//	Stream globals:
//

extern InStream *sysin;			// System standard input
extern OutStream *sysout;		// System standard output
extern OutStream *syserr;		// System standard errors

#ifndef HEADER

//
// (Trivial) Constructor
//

DL_EXPORT Stream::Stream () {
	io_layer = 0;
	o_flags = flags = 0;
}	// Stream::Stream

DL_EXPORT Stream::Stream (bool phase) {}

//
//	Predicates to check stream attributes
//

// Is readable stream?
bool stream_can_read (Stream *stream) {
// (not write only => readable)
return (stream->o_flags & _O_ACCMODE) != _O_WRONLY;
}	// stream_can_read

// Is writeable stream?
bool stream_can_write (Stream *stream) {
// (not read only => writeable)
return (stream->o_flags & _O_ACCMODE) != _O_RDONLY;
}	// stream_can_write

// Is seekable stream?
bool stream_can_seek (Stream *stream) {
// TODO!
return 1;
}	// stream_can_seek

//
//	Stream/codec evaluations
//

//
//	Input stream constructor
//

InStream::InStream (int fd, char const *name) : Stream ()
	{ open_fd (fd, name, O_RDONLY|O_BINARY); o_flags |= stream_read; }

//
//	Output stream constructor
//

OutStream::OutStream (int fd, char const *name) : Stream ()
	{ open_fd (fd, name, O_WRONLY|O_BINARY); o_flags |= stream_write; }

// Stream evaluation
VType Stream::evalV (VDatum &val, bool full) {
val._stream = this;
return T_stream;
}	// Stream::evalV

// Expecting input/output stream as 'expr'
Stream *expect_stream (Expr *expr, Stream *def_stream, Prefix *where,
	bool (*checker) (Stream *stream)) {
if (expr) {
	VDatum val;
	VType type;
	if ((type = expr->evalV (val, true)) == T_stream) {
		return val._stream;		// TODO...
		
		// TODO: stream compatibility failure
		if (! checker || checker (val._stream)) return val._stream;
		}

	else where->type_error (expr, T_stream, type, val);
	return 0;
	}	// (expr)

return def_stream;
}	// expect_stream

// Codec evaluation
VType SCodec::evalV (VDatum &val, bool full) {
val._scodec = this;
return T_scodec;
}	// SCodec::evalV

// Expecting stream codec as 'expr'
SCodec *expect_scodec (Expr *expr, Prefix *where) {
if (expr) {
	VDatum val;
	VType type;
	if ((type = expr->evalV (val, true)) == T_scodec)
		return val._scodec;

	where->type_error (expr, T_scodec, type, val);
	}	// (expr)

return 0;
}	// expect_scodec

//
//	Output expression 
//

// Generic expression output handler
unsigned Expr::put (Stream *out) {
return 0;			// (nothing to output)
}	// Expr::put

// Output any expression
unsigned put_expr (Stream *out, Expr *expr) {
return expr ? expr->put (out) : 0;
}	// put_expr

// Put sequence of arguments 'args'
// Returns # of arguments displayed so far
unsigned Stream::put_args (Expr *args) {
unsigned count = 0;

Expr *expr;
while (expr = pop_list (args)) {
	VDatum val;
	VType type = evalV_X (expr, val);

	switch (type) {
	// (handle primary types)

	case T_fixed:
		put_fixed (val._fixed);
		break;

	case T_float:
		put_float (val._float);
		break;

	case T_string:
		val._string.put (this);
		break;

	default:
		link_value (type, val);
		count += put_expr (this, evalX_V (type, val));
		unlink_value (type, val);

		continue;
	}	// switch (type)

	// (only if primary type...)
	++ count;
	relink_value (type, val);
	}	// while ()

return count;
}	// Stream::put_args

// Fixed value to stream
unsigned X_Fixed::put (Stream *out) {
out->put_fixed (value);
return 1;
}	// X_Fixed::put

// Float value to stream
unsigned X_Float::put (Stream *out) {
out->put_float (value);
return 1;
}	// X_Float::put

//
//	Logging
//

Logger *Logger::log_stream (Stream *stream) {
if (stream) stream->log (*this);

return this;
}	// Logger::log_stream

Logger *Logger::log_scodec (SCodec *scodec) {
if (scodec) scodec->log (*this);

return this;
}	// Logger::log_scodec

void SCodec::log (Logger &log) {
log.put_cstr ("<<")->put_cstr ("SCodec:");
log_ex (log);
log.put_cstr (">>");
}	// SCodec::log

//
//
//

#define HEADER

#include "IOSys.cpp"

#undef HEADER

//
//	Trivial I/O stub
//

static IOSystem null_io;

// Close stream
bool Stream::close () {
if (io_layer && io_layer != &null_io) {
	if (io_layer->close ()) {
		delete io_layer;
		io_layer = &null_io;

		return true;
		}
	}

return false;
}	// Stream::close

// Release stream
void Stream::release () {
close ();
delete this;
}	// Stream::release

// Seek in stream (absolutely) to 'relpos' (from start/end)
// Note: m.b. buffered
int Stream::seek (int relpos) {
return io_layer->seek (relpos);
}	// Stream::seek

// Seek in stream (relatively) by offset (forward/backward)
// Note: m.b. buffered
int Stream::skip (int offset) {
return io_layer->skip (offset);
}	// Stream::skip

// Tell current position in stream
// Note: m.b. buffered
int Stream::tell () {
return io_layer->tell ();
}	// Stream::tell

// Output character 'ch' to stream
// Note: m.b. buffered
DL_EXPORT bool Stream::put_octet (char octet) {
return io_layer->put_octet (octet);
}	// Stream::put_octet

// Output wide character 'wch' to stream
// Note: m.b. buffered
DL_EXPORT bool Stream::put_wch (unsigned wch) {
if (wch >= 0x100) {
	if (wch >= 0x10000)
		if (! put_octet (wch >> 16)) return false;

	if (! put_octet (wch >> 8)) return false;
	}

if (! put_octet (wch)) return false;

return true;
}	// Stream::put_wch

// Output raw data "buf[len]" to stream
void Stream::put_data (char const *buf, unsigned len) {
io_layer->write (buf, len);
}	// Stream::put_data

// Output C-string 'str' to stream
DL_EXPORT void Stream::put_cstr (const char *str) {
io_layer->write (str, strlen (str));
}	// Stream::put_cstr

// Input character from stream
bool Stream::get_octet (char &octet) {
return io_layer->get_octet (octet);
}	// Stream::get_octet

// Input wide character 'wch' from stream
bool Stream::get_wch (unsigned &wch) {
char octet;

if (get_octet (octet))
	{ wch = octet; return true; }

return false;
}	// Stream::get_wch

// Input raw data "buf[len]" from stream
unsigned Stream::get_data (char *buf, unsigned len) {
return io_layer->read (buf, len);
}	// Stream::get_data

// Sync stream
int Stream::sync () {
return io_layer->sync ();
}	// Stream::sync

// Check for end of file
bool Stream::is_at_end () {
return io_layer->is_at_eos ();
}	// Stream::is_at_end

// Get current file size
int Stream::get_size () {
return io_layer->get_length ();
}	// Stream::get_size

// Resize file
int Stream::set_size (unsigned size) {
return io_layer->set_length (size);
}	// Stream::set_size

// Check for console
bool Stream::is_console () {
return io_layer->is_console ();
}	// Stream::is_console

// Log stream
void Stream::log (Logger &log) {
log.put_ch ('<')->put_cstr ("Stream: ")->put_hex (4, o_flags);
if (io_layer) io_layer->log (log);
log.put_ch ('>');
}	// Stream::log

// Query stream info
Expr *Stream::query_info () {
return new X_List (new X_Fixed (o_flags), io_layer->get_attr ());
}	// Stream::query_info

//
//	Default stream interfaces
//

struct P_Stream_Default : Prefix {
	Stream *stream;

	P_Stream_Default (char const *ident, O_Enum op, Stream *stream) :
		Prefix (ident, op)
	{ this->stream = stream; }

	VType evalV (VDatum &val, Expr *args) {
		val._stream = stream;
		return T_stream;
		}	// evalV

	};	// P_Stream_Default

//
//
//	Input/output functors
//
//

//
//
//	Default streams redirection
//
//

static Stream *def_input = 0;
static Stream *def_output = 0;

struct P_WithStream : P_Wrapper {
	Stream * &def_stream;

	P_WithStream (char const *ident, unsigned opcode, Stream * &stream) :
		P_Wrapper (ident, opcode), def_stream (stream) {}

	void eval (WrapX &wrapper, Expr *args) {
	Stream *stream = expect_stream (get_arg (args), 0, this, 0);

	if (stream) {
		Stream *save_stream = def_stream;
		def_stream = stream;

		link_expr (stream);
		wrapper.eval (args);
		unlink_expr (stream);

		def_stream = save_stream;
		}

	else
		wrapper.eval (args);
	}	// eval

	};	// P_WithStream

//
//
//	Input from stream
//
//

//
// Read chars until 'brk_ch'
// Result:
//		1:	break char reached
//		-1:	end of file reached
//		0:	buffer filled
//

static int read_buffer_until (Stream *in, char *buffer, unsigned &buf_size, char brk_ch) {
unsigned i = 0;
char ch;

while (i != buf_size) {
	if (in->get_octet (ch))
		if (ch == brk_ch)
			{ buf_size = i; return 1; }
		else buffer [i ++] = ch;
	else { buf_size = i; return -1; }
	}

return 0;
}	// read_buffer_until

// Read string until 'brk_ch'
// Returns true (on break char) or false (on EOF)
static bool read_string_until (Stream *in, S_string &s_input, char brk_ch) {
char InBuf [128];			// (initial buffer)
int result;

// Read text record
struct InRec {
	char *buffer;			// (stored char data)
	unsigned len;			// (length of stored char data)
	InRec *prev;			// (previous buffer in chain)
	} _Init, *_last;

_Init.buffer = InBuf;
_Init.len = sizeof(InBuf);
_Init.prev = 0;

_last = &_Init;

// (reading from input)
while ((result = read_buffer_until (in, _last->buffer, _last->len, brk_ch)) == 0) {
	// allocate & chain input buffers
	struct InRec *new_rec = new ("Input/Record") InRec;
	new_rec->buffer = new ("Input/Buffer") char [new_rec->len = (_last->len / 2) * 3];
	new_rec->prev = _last;
	_last = new_rec;
	}

// (done reading -- calculate total length)
struct InRec *rec;
unsigned length = 0;

for (rec = _last; rec; rec = rec->prev)
	length += rec->len;

// (allocate & copy characters)
str_ptr p_data = s_input.alloc ("String/read", length, 0);
s_forward (p_data, length, 0);

while (rec = _last) {
	s_backward (p_data, rec->len, 0);
	memcpy (p_data, rec->buffer, rec->len);

	if (_last = rec->prev) {
		delete [] rec->buffer;
		delete rec;
		}
	}	// (while)

return result == 1;				// (break char reached?)
}	// read_string_until

//
// Reading string from input
//

X_String *get_string (Stream *input) {
S_string s_input;

if (! input->is_console () && input->is_at_end ()) return 0;

read_string_until (input, s_input, '\n');

if (! s_input.length && (input->flags & f_EOF)) return 0;
return s_input.cons ();
}	// get_string

//
// Functional version of stream get line
//

struct P_Stream_GetLine : PrefixX {
	P_Stream_GetLine (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
	Stream *input = expect_stream (args, def_input, this, stream_can_read);

	if (input) {
		X_String *string = get_string (input);
		return string;
		}

	return 0;
	}	// evalX

	};	// P_Stream_GetLine

//
// Reading list of strings from input
//

unsigned Stream::get_args (Expr *args) {
unsigned count = 0;
Expr *expr;

while (expr = pop_list (args)) {
	X_String *string = get_string (this);
	if (! string) break;
	expr->mutateX (string);
	++ count;
	}

return count;
}	// Stream::get_args

struct P_Stream_Get : Prefix {
	P_Stream_Get (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Stream *input = expect_stream (get_arg (args), def_input, this, stream_can_read);

		if (input) {
			val._fixed = input->get_args (args);
			return T_fixed;
			}

		return T_undef;
		}	// evalV

	};	// P_Stream_Get

//
//	Reading fixed number of characters from input
//

struct P_Stream_Read : Prefix {
	P_Stream_Read (char const *ident, O_Enum op): Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_Stream_Read::evalV (VDatum &val, Expr *args) {
Stream *input = expect_stream (get_arg (args), def_input, this, stream_can_read);
S_fixed length = expect_fixed (args, 0);

if (input) {
	// TODO: realloc on need...
	str_ptr data = val._string.alloc ("String/read", length, 0);
	unsigned count = input->get_data ((char *)data, length);

	val._string.length = count;
	return T_string;
	}

return T_undef;
}	// P_Stream_Read::evalV

//
//	Get character code from stream
//

struct P_Stream_GetChar : Prefix {
	P_Stream_GetChar (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *input = expect_stream (args, def_input, this, stream_can_read);

	if (input) {
		C_long code;
		if (input->get_wch (code))
			{ val._fixed = code; return T_fixed; }
		return T_undef;
		}

	return T_undef;
	}	// evalV

	};	// P_Stream_GetChar

//
//
//	Output to stream
//
//

struct P_Stream_Put : Prefix {
	P_Stream_Put (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *output = expect_stream (get_arg (args), def_output, this, stream_can_write);

	if (output) {
		val._fixed = output->put_args (args);
		return T_fixed;
		}

	return T_undef;
	}	// evalV

	};	// P_Stream_Put

//
// Functional version of stream put line
//

struct P_Stream_PutLine : Prefix {
	P_Stream_PutLine (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *output = expect_stream (get_arg (args), def_output, this, stream_can_write);

	if (output) {
		S_string string;
		if (expect_string (args, string)) {
			string.put (output);
			val._string.s_copy (string);
			return T_string;
			}
		}

	return T_undef;
	}	// evalV

	};	// P_Stream_PutLine

//
//	Put character code to stream
//

struct P_Stream_PutChar : Prefix {
	P_Stream_PutChar (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *output = expect_stream (get_arg (args), def_output, this, stream_can_write);

	if (output) {
		S_fixed code = expect_fixed (args, 0);
		output->put_wch (code);		// TODO: check retval

		val._fixed = code;
		return T_fixed;
		}

	return T_undef;
	}	// evalV

	};	// P_Stream_PutChar

//
//
//	Generic stream operations
//
//

//
//	Open file stream
//

struct P_File_Open : Prefix {
	unsigned def_mode;				// (default open mode)

	P_File_Open (char const *ident, O_Enum op, unsigned def_mode):
		Prefix (ident, op) { this->def_mode = def_mode; }

	D_Prefix_evalV;
	};

VType P_File_Open::evalV (VDatum &val, Expr *args) {
Stream *stream = new ("Stream/File") Stream ();

Expr *filename = get_arg (args);
unsigned mode = expect_fixed (args, def_mode);
stream->open_file (this, filename, mode);

if (stream->io_layer) {
	val._stream = stream;
	return T_stream;
	}

// (On failure to init I/O layer)
delete stream;
return T_undef;
}	// P_File_Open::evalV

//
//	Close file stream
//

// TODO: check stream to be closed

struct P_Stream_Close : Prefix {
	P_Stream_Close (char const *ident, O_Enum op): Prefix (ident, op) {}

	D_Prefix_evalV;
	};

//	TODO: make return value

VType P_Stream_Close::evalV (VDatum &val, Expr *args) {
Stream *stream = expect_stream (args, 0, this, 0);

if (stream) {
	if (stream == sysin || stream == sysout || stream == syserr);	// (can't close these!)
	else {
		stream->close ();
		}
	}

return T_undef;			// (stream closed)
}	// P_Stream_Close::evalV

//
//	Stream seek (absolutely to position)
//

struct P_Stream_Seek : Prefix {
	P_Stream_Seek (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Stream *stream = expect_stream (get_arg (args), 0, this, stream_can_seek);

		if (stream) {
			S_fixed position = expect_fixed (args, 0);
			val._fixed = stream->seek (position);
			return T_fixed;
			}

		return T_undef;			// (stream closed)
		}	// evalV
	};

//
//	Stream skip (relatively by offset)
//

struct P_Stream_Skip : Prefix {
	P_Stream_Skip (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Stream *stream = expect_stream (get_arg (args), 0, this, stream_can_seek);

		if (stream) {
			S_fixed offset = expect_fixed (args, 0);
			val._fixed = stream->skip (offset);
			return T_fixed;
			}

		return T_undef;			// (stream closed)
		}	// evalV
	};

//
//	Tell current stream position
//

struct P_Stream_Tell : Prefix {
	P_Stream_Tell (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Stream *stream = expect_stream (args, 0, this, stream_can_seek);

		if (stream) {
			val._fixed = stream->tell ();
			return T_fixed;
		}

		return T_undef;			// (stream closed)
		}	// evalV
	};

//
//	Sync stream
//

struct P_Stream_Sync : Prefix {
	P_Stream_Sync (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Stream *stream = expect_stream (args, 0, this, 0);

		if (stream) {
			val._fixed = stream->sync ();
			return T_fixed;
			}

		return T_undef;
		}	// evalV
	};

//
//	Set/unset/change I/O buffer size for stream
//

struct P_Stream_BufMode : Prefix {
	P_Stream_BufMode (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *stream = expect_stream (get_arg (args), 0, this, 0);

	if (stream) {
		S_fixed size = expect_fixed (args, 0);
		val._fixed = stream->buf_resize (size > 0 ? size : 0);
		return T_fixed;
		}

	return T_undef;
	}	// evalV

	};	// P_Stream_BufMode

//
//	Stream predicate check
//

struct P_Stream_Check : Prefix {
	bool (Stream::* do_check) ();

	P_Stream_Check (char const *ident, O_Enum op, bool (Stream::* do_check) ()): Prefix (ident, op)
		{ this->do_check = do_check; }
	
	VType evalV (VDatum &val, Expr *args) {
	Stream *stream = expect_stream (args, 0, this, 0);

	if (stream) {
		val._fixed = (stream->*do_check) ();
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

// Get file length
struct P_Stream_GetLength : Prefix {
	P_Stream_GetLength (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *stream = expect_stream (args, 0, this, 0);

	if (stream) {
		val._fixed = stream->get_size ();
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

// Set file length
struct P_Stream_SetLength : Prefix {
	P_Stream_SetLength (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	Stream *stream = expect_stream (get_arg (args), 0, this, 0);
	if (stream) {
		unsigned size = expect_fixed (args, 0);
		val._fixed = stream->set_size (size);
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

// Get stream info
struct P_Stream_Info : PrefixX {

	P_Stream_Info (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
	Stream *stream = expect_stream (get_arg (args), 0, this, 0);
	if (stream)
			return stream->query_info ();

	return 0;
	}	// evalX
	};

//
//
//	I/O codecs
//
//

unsigned CharStream::read () { return EndData; }

bool CharStream::write (unsigned codepoint) { return false; }

unsigned ByteStream::fetch () { return EndData; }

bool ByteStream::emit (unsigned char octet) { return false; }

void SCodec::log_ex (Logger &log) { log.put_cstr ("???"); }

unsigned SCodec::encode (CharStream &input, ByteStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.read ()) != EndData && output.emit (cval))
	++ count;

return count;
}	// SCodec::encode

unsigned SCodec::decode (ByteStream &input, CharStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.fetch ()) != EndData && output.write (cval))
	++ count;

return count;
}	// SCodec::decode

//
//	Codec for UTF8
//
struct UTF8_SCodec : SCodec {
	void log_ex (Logger &log) { log.put_cstr ("UTF8"); }

	// Encode 'input' to 'output' in UTF8
	unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input' in UTF8
	unsigned decode (ByteStream &input, CharStream &output);
	};

unsigned UTF8_SCodec::encode (CharStream &input, ByteStream &output) {
unsigned shift;
unsigned cval;
unsigned count = 0;

while ((cval = input.read ()) != EndData) {

if (cval < 0x80)				// (1 octet)
	{ output.emit(cval); continue; }

else if (cval < 0x800)			// (2 octets)
	output.emit (((cval >> (shift =  6))) | 0xc0);
else if (cval < 0x10000)		// (3 octets)
	output.emit (((cval >> (shift = 12))) | 0xe0);
else if (cval < 0x200000)		// (4 octets)
	output.emit (((cval >> (shift = 18))) | 0xf0);
else if (cval < 0x4000000)		// (5 octets -- not to happen really)
	output.emit (((cval >> (shift = 24))) | 0xf8);
else if (cval < 0x80000000)		// (6 octets -- not to happen really)
	output.emit (((cval >> (shift = 30))) | 0xfc);
else {}							// (illegal?)

do
	output.emit (((cval >> (shift -= 6)) & 0x3f) | 0x80);
while (shift);

count ++;
}	// while (input)

return count;
}	// UTF8_SCodec::encode

unsigned UTF8_SCodec::decode (ByteStream &input, CharStream &output) {
unsigned extra;
unsigned cval;
unsigned count = 0;

while ((cval = input.fetch ()) != EndData) {

if      (! (cval & 0x80))			// (1 octet)
	{ output.write (cval); continue; }
else if (! (cval & 0x40))
	{ /* error to signal? */ }
else if (! (cval & 0x20))			// (2 octets)
	{ extra = 1; cval &= 0x1f; }
else if (! (cval & 0x10))			// (3 octets)
	{ extra = 2; cval &= 0x0f; }
else if (! (cval & 0x08))			// (4 octets)
	{ extra = 3; cval &= 0x07; }
else if (! (cval & 0x04))			// (5 octets -- not to happen really)
	{ extra = 4; cval &= 0x03; }
else if (! (cval & 0x02))			// (6 octets -- not to happen really)
	{ extra = 5; cval &= 0x01; }

while (extra --) {
	cval <<= 6;
	cval |= input.fetch () & 0x3f;		// (check errors?)
	}

output.write (cval);
count ++;
}	// while

return count;
}	// UTF8_SCodec::decode

//
//	16-bit get/put
//

// Put 16-bit 'cval', with endiannness
static void put_w16 (bool endianness, C_long cval, ByteStream &output) {
if (endianness) {
	output.emit (cval >> 8);
	output.emit (cval);
	}
else {
	output.emit (cval);
	output.emit (cval >> 8);
	}
}	// put_w16

// Get 16-bit 'cval', with endiannness
static bool get_w16 (bool endianness, C_long &cval, ByteStream &input) {
C_long val[2];

if ((val[0] = input.fetch ()) != EndData &&
    (val[1] = input.fetch ()) != EndData) {
		cval = endianness ? ((val[0] << 8) | val[1]) : ((val[1] << 8) | val[0]);
		return true;
		}
else
	return false;
}	// get_w16

//
//	Codec for UTF16
//

struct UTF16_SCodec : SCodec {
	bool endianness;			// ? Big Endian : Little Endian

	UTF16_SCodec (bool endianness)
		{ this->endianness = endianness; }

	void log_ex (Logger &log) {
		log.put_cstr ("UTF16/")->put_cstr (endianness ? "BE" : "LE");
		}

	// Encode 'input' to 'output' in UTF16
	unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input' in UTF16
	unsigned decode (ByteStream &input, CharStream &output);
	};

unsigned UTF16_SCodec::encode (CharStream &input, ByteStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.read ()) != EndData) {
if (cval >= 0x10000) {
	// (encode surrogates...)
	cval -= 0x10000;
	put_w16 (endianness, 0xD800 | (cval >> 10), output);		// high surrogate
	put_w16 (endianness, 0xDC00 | (cval & 0x3FF), output);		// low surrogate
	}
else
	put_w16 (endianness, cval, output);

count ++;
}	// while (input)

return count;
}	// UTF16_SCodec::encode

unsigned UTF16_SCodec::decode (ByteStream &input, CharStream &output) {
unsigned count = 0;
C_long cval;

while (get_w16 (endianness, cval, input)) {
	if (0xD800 <= cval && cval < 0xDC00) {
		// (surrogate high)
		C_long cval1;
		if (get_w16 (endianness, cval1, input) &&
			0xDC00 <= cval1 && cval1 < 0xE000)
			// (surrogate low)
			output.write (0x10000 | ((cval - 0xD800) << 10) | (cval1 - 0xDC00));
		}
	else
		output.write (cval);

	count ++;
	}	// while (input)

}	// UTF16_SCodec::decode

//
//	Codec for UTF32
//

struct UTF32_SCodec : SCodec {
	bool endianness;			// ? Big Endian : Little Endian

	UTF32_SCodec (bool endianness)
		{ this->endianness = endianness; }

	void log_ex (Logger &log) {
		log.put_cstr ("UTF32/");
		log.put_cstr (endianness ? "BE" : "LE");
		}

	// Encode 'input' to 'output' in UTF32
	unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input' in UTF32
	unsigned decode (ByteStream &input, CharStream &output);
	};

unsigned UTF32_SCodec::encode (CharStream &input, ByteStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.read ()) != EndData) {
	if (endianness) {
		put_w16 (true, cval >> 16, output);
		put_w16 (true, cval, output);
		}
	else {
		put_w16 (false, cval, output);
		put_w16 (false, cval >> 16, output);
		}

	++ count;
	}

return count;
}	// UTF32_SCodec::encode

unsigned UTF32_SCodec::decode (ByteStream &input, CharStream &output) {
unsigned count = 0;
C_long cval[2];

while (get_w16 (endianness, cval[0], input) &&
	   get_w16 (endianness, cval[1], input)) {
	   output.write
		(endianness ?
			(cval[0] << 16) | cval[1] : (cval[1] << 16) | cval[0]);

	++ count;
	}

return count;
}	// UTF32_SCodec::decode

//
//	Codec for Simple Multi-Byte Compression
//

struct SMBC_SCodec : SCodec {
	SMBC_SCodec () {}

	void log_ex (Logger &log) {
		log.put_cstr ("SMBC");
		}

	// Encode 'input' to 'output' in UTF32
	unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input' in UTF32
	unsigned decode (ByteStream &input, CharStream &output);
	};

unsigned SMBC_SCodec::encode (CharStream &input, ByteStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.read ()) != EndData) {
	if (cval < 0x100) {
		if (cval >= 0xF8)
			output.emit (0xFF);
		output.emit (cval);
		}
	else if (cval < 0x10000) {
		output.emit (0xFE);
		output.emit (cval & 0xFF);
		output.emit ((cval >> 8) & 0xFF);
		}
	else if (cval < 0x1000000) {
		output.emit (0xFD);
		output.emit (cval & 0xFF);
		output.emit ((cval >> 8) & 0xFF);
		output.emit ((cval >> 16) & 0xFF);
		}
	else {
		output.emit (0xFC);
		output.emit (cval & 0xFF);
		output.emit ((cval >> 8) & 0xFF);
		output.emit ((cval >> 16) & 0xFF);
		output.emit ((cval >> 24) & 0xFF);
		}

	++ count;
	}

return count;
}	// SMBC_SCodec::encode

unsigned SMBC_SCodec::decode (ByteStream &input, CharStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.fetch ()) != EndData) {
	switch (cval) {
		case 0xFF:
			cval = input.fetch ();
			break;

		case 0xFE:
			cval = input.fetch ();
			cval |= input.fetch () << 8;
			break;

		case 0xFD:
			cval = input.fetch ();
			cval |= input.fetch () << 8;
			cval |= input.fetch () << 16;
			break;

		case 0xFC:
			cval = input.fetch ();
			cval |= input.fetch () << 8;
			cval |= input.fetch () << 16;
			cval |= input.fetch () << 24;
			break;
		}	// switch

	output.write (cval);
	}	// while
}	// SMBC_SCodec::decode

//
//	Codec for codepage
//

struct CodePage_SCodec : SCodec {
	C_long mapping [0x100];			// map encoding to Unicode
	char error;						// error code
	X_String *title;				// symbolic name

	CodePage_SCodec (S_string &source, S_string &s_title, char error);

	void log_ex (Logger &log) {
		log.put_cstr ("CodePage: ");
		title->log (log);
		}

	// Encode 'input' to 'output'  with codepage
	unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input' with codepage
	unsigned decode (ByteStream &input, CharStream &output);

	void release () { unlink_expr (title); Expr::release (); }

	};

CodePage_SCodec::CodePage_SCodec (S_string &source, S_string &s_title, char error) {
link_expr (title = s_title.cons ());

unsigned len;
C_type type;
str_ptr p_src = source.fetch (len, type);

for (unsigned i = 0; i != 0x100; ++ i)
	mapping[i] = i < len ? s_read_inc (type, p_src) : i;

this->error = error;
}	// CodePage_SCodec::CodePage_SCodec

unsigned CodePage_SCodec::encode (CharStream &input, ByteStream &output) {
unsigned cval;
unsigned count = 0;

while ((cval = input.read ()) != EndData) {
	unsigned i;
	for (i = 0; i != 0x100; ++ i)
		if (mapping[i] == cval) break;

	output.emit (i != 0x100 ? i : error);

	++ count;
	}

return count;
}	// CodePage_SCodec::encode

unsigned CodePage_SCodec::decode (ByteStream &input, CharStream &output) {
unsigned count = 0;
unsigned cval;

while ((cval = input.fetch()) != EndData) {
	output.write (mapping[cval]);

	++ count;
	}

return count;
}	// CodePage_SCodec::decode

//
//	Hex dump codec
//

struct HexDump_SCodec : SCodec {

	HexDump_SCodec () {}

	void log_ex (Logger &log) { log.put_cstr ("HexDump"); }

	// Encode 'input' to 'output'  with codepage
	unsigned encode (CharStream &input, ByteStream &output);

	// Decode 'output' from 'input' with codepage
	unsigned decode (ByteStream &input, CharStream &output);

	// (value to hexdigit)
	char to_hex (unsigned val)
		{ return (val < 10 ? '0' : 'A' - 10) + val; }

	// (hexdigit to value)
	unsigned from_hex (char val) {
		return
			('0' <= val && val <= '9') ? val - '0' :
			('A' <= val && val <= 'F') ? val - 'A' + 10 :
			('a' <= val && val <= 'f') ? val - 'a' + 10 :
			0;
		}
	};

unsigned HexDump_SCodec::encode (CharStream &input, ByteStream &output) {
unsigned cval;
unsigned count = 0;
char buffer [8];
unsigned i;

while ((cval = input.read ()) != EndData) {
	i = 0;
	while (cval) {
		buffer[i ++] = to_hex (cval & 0x0f);
		cval >>=4;
		}

	output.emit ('[');
	while (i) output.emit (buffer [-- i]);
	output.emit (']');

	++ count;
	}

return count;
}	// HexDump_SCodec::encode

unsigned HexDump_SCodec::decode (ByteStream &input, CharStream &output) {
unsigned count = 0;
unsigned cval;
unsigned value = 0;

while ((cval = input.fetch ()) != EndData) {
	switch (cval) {
		case '[':	value = 0;
					break;

		case ']':	output.write (value);
					++ count;
					break;

		default:
				value <<= 4;
				value |= from_hex (cval);
		}
	}

return count;
}	// HexDump_SCodec::decode

//
//	Codecs constructors
//

// UTF8 constructor
static SCodec *make_codec_UTF8 (Expr *args, Prefix *where) {
return new ("SCodec [UTF8]") UTF8_SCodec ();
}	// make_codec_UTF8

// UTF16 (BE/LE) constructor
static SCodec *make_codec_UTF16 (Expr *args, Prefix *where) {
bool endianness = where->expect_bool (args, false);
return new ("SCodec [UTF16]") UTF16_SCodec (endianness);
}	// make_codec_UTF16

// UTF32 (BE/LE) constructor
static SCodec *make_codec_UTF32 (Expr *args, Prefix *where) {
bool endianness = where->expect_bool (args, false);
return new ("SCodec [UTF32]") UTF32_SCodec (endianness);
}	// make_codec_UTF16

// SMBC constructor
static SCodec *make_codec_SMBC (Expr *args, Prefix *where) {
return new ("SCodec [SMBC]") SMBC_SCodec ();
}	// make_codec_SMBC

// Codepage constructor
static SCodec *make_codec_codepage (Expr *args, Prefix *where) {
S_string title, source;

if (where->expect_string (get_arg(args), title) &&
    where->expect_string (get_arg(args), source)) {
	char err_code = where->expect_fixed (args, 0);
	return new ("SCodec [codepage]") CodePage_SCodec (source, title, err_code);
	}

return 0;
}	// make_codec_codepage

// Hexdump constructor
static SCodec *make_codec_hexdump (Expr *args, Prefix *where) {
return new ("SCodec [Hexdump]") HexDump_SCodec;
}	// make_codec_hexdump

struct P_SCodec : Prefix {
	SCodec * (*make_codec) (Expr *args, Prefix *where);

	P_SCodec (char const *ident, SCodec * (*make_codec) (Expr *args, Prefix *where)) :
		Prefix (ident, Op_Null)
		{ this->make_codec = make_codec; }

	VType evalV (VDatum &val, Expr *args) {
		val._scodec = make_codec (args, this);
		return T_scodec;
		}	// evalV
	};	// P_SCodec

//
//	Initialisation...
//

#include <fcntl.h>

static bool init_primaries_stream (int order) {

//		[Categories]

//^C	Stream
//^B	Stream functors
//^D	Stream input/output and other related functors.

//^C	Codec
//^B	Codec related functors
//^D	Input/output codec functors.

//		[Types]

//^T	Stream
//^B	Stream reference.
//^D	Anything evaluating to input/output stream reference.

//^T	Codec
//^B	Codec reference.
//^D	Anything evaluating to input/output codec reference.

//		--------

// Set up default streams:

	def_input = sysin;
	def_output = sysout;

//^N	is_stream [Stream | Predicate]
//^P	is_stream (V: Any) => Bool
//^B	Check for stream.
//^D	Predicate: !true, if \V evaluates to stream.

	DefBuiltin (P_IsType ("is_stream", Op_Null, T_stream));

//^G	f_in f_out f_err

//^N	f_in [Stream | Nullary]
//^P	f_in () => Stream
//^B	Standard input.
//^D	Returns default input stream.

//^N	f_out [Stream | Nullary]
//^P	f_out () => Stream
//^B	Standard output.
//^D	Returns default output stream.

//^N	f_err [Stream | Nullary]
//^P	f_err () => Stream
//^B	Standard error output.
//^D	Returns default error output stream.

	DefBuiltin (P_Stream_Default ("f_in", Op_Null, sysin));
	DefBuiltin (P_Stream_Default ("f_out", Op_Null, sysout));
	DefBuiltin (P_Stream_Default ("f_err", Op_Null, syserr));

//^G	f_get f_put

//^N	f_get [Stream | Mutator]
//^P	f_get ([In: Stream], Line: Mutable, ...) => Int
//^B	Get line(s) from stream (as strings).
//^D	Reads lines from the input stream \In (or default input, if \In is undefined).
//^D	Assigns all strings values read to mutables from list of \Line.
//^D	(Final EOLs are removed.)
//^D	Returns: number of lines successfully read and stored.

	DefBuiltin (P_Stream_Get ("f_get", Op_Get));

//^N	f_put [Stream]
//^P	f_put ([Out: Stream], { Value: Any, } ...) => Int
//^B	Put value(s) to stream (as strings).
//^D	Evaluates and writes values from list of \Value
//^D	to the output stream \Out (or default output, if \Out is undefined).
//^D	Returns: number of values successfully written.

	DefBuiltin (P_Stream_Put ("f_put", Op_Put));

//^G	f_open f_create f_close

//^N	f_open [Stream | Constructor]
//^P	f_open (FileName: String [, AccessMode: Int]) => Stream
//^B	Open file as stream.
//^D	Opens system file \FileName with \AccessMode (as for C "open" library func).
//^D	Defaults \AccessMode to O_RDONLY|O_BINARY.
//^D	Returns new stream or !undef, if open failed.

	DefBuiltin (P_File_Open ("f_open", Op_Null, O_RDONLY|O_BINARY));

//^N	f_create [Stream | Constructor]
//^P	f_create (FileName: String [, AccessMode: Int]) => Stream
//^B	Open file as stream.
//^D	Opens system file \FileName with \AccessMode (as for C "open" library func).
//^D	Defaults \AccessMode to O_WRONLY|O_CREAT|O_BINARY.
//^D	Returns new stream or !undef, if open failed.

	DefBuiltin (P_File_Open ("f_create", Op_Null, O_WRONLY|O_CREAT|O_BINARY));

//^N	f_close [Stream | Destructor]
//^P	f_close (Stream: Stream) => ()
//^B	Close opened stream.
//^D	Close previously opened \Stream.

	DefBuiltin (P_Stream_Close ("f_close", Op_Null));

//^N	f_getline [Stream]
//^P	f_getline (Stream: Stream) => String
//^B	Get line from stream.
//^D	Read and return new line from input \Stream.

	DefBuiltin (P_Stream_GetLine ("f_getline", Op_Null));

//^N	f_putline [Stream]
//^P	f_putline (Stream: Stream, String: String) => String
//^B	Put string to stream.
//^D	Output \String to output \Stream.
//^D	Returns \String.

	DefBuiltin (P_Stream_PutLine ("f_putline", Op_Null));

//^N	f_read [Stream]
//^P	f_read (Stream: Stream, Count: Int) => String
//^B	Read string from stream.
//^D	Reads exactly \Count characters (including EOLs) from input \Stream (returning string result).

	DefBuiltin (P_Stream_Read ("f_read", Op_Null));

//^G	f_getc f_putc

//^N	f_getc [Stream]
//^P	f_getc (Stream: Stream) => Int
//^B	Get character from stream.
//^D	Returns code of next character read from input stream \Stream (or !undef on EOF).

//^N	f_putc [Stream]
//^P	f_putc (Stream: Stream, Code: Int) => Int
//^B	Put character to stream.
//^D	Writes character with \Code to output stream \Stream.
//^D	Returns \Code.

	DefBuiltin (P_Stream_GetChar ("f_getc", Op_Null));
	DefBuiltin (P_Stream_PutChar ("f_putc", Op_Null));

//^G	f_tell f_seek f_skip

//^N	f_tell [Stream]
//^P	f_tell (Stream: Stream) => Int
//^B	Tell current position in stream.
//^D	Returns current pointer position in seekable stream \Stream.

	DefBuiltin (P_Stream_Tell ("f_tell", Op_Null));

//^N	f_seek [Stream]
//^P	f_seek (Stream: Stream, Position: Int) => Int
//^B	Seek absolutely in stream.
//^D	Seek in seekable stream \Stream to \Position, relative to start (if >=0) or to end (if < 0).
//^D	Returns new stream position.

	DefBuiltin (P_Stream_Seek ("f_seek", Op_Null));

//^N	f_skip [Stream]
//^P	f_skip (Stream: Stream, Offset: Int) => Int
//^B	Seek relatively in stream.
//^D	Seek in seekable stream \Stream relative by \Offset (either positive or negative).
//^D	Returns new stream position.

	DefBuiltin (P_Stream_Skip ("f_skip", Op_Null));

//^G	f_sync f_bufmode

//^N	f_sync [Stream]
//^P	f_sync (Stream: Stream) => Int
//^B	Sync stream.
//^D	Sync stream \Stream, flushing and/or discarding I/O buffers.

	DefBuiltin (P_Stream_Sync ("f_sync", Op_Null));

//^N	f_bufmode [Stream]
//^P	f_bufmode (Stream: Stream, BufSize: Int) => Int
//^B	Set stream buffer size.
//^D	Set size of internal I/O buffer of stream \Stream to \BufSize.
//^D	(Disable buffering completely, if 0).

	DefBuiltin (P_Stream_BufMode ("f_bufmode", Op_Null));

//^N	f_at_end [Stream | Predicate]
//^P	f_at_end (Stream: Stream) => Bool
//^B	Check for end of stream.
//^D	Return true, if \Stream has reached end.

	DefBuiltin (P_Stream_Check ("f_at_end", Op_Null, &Stream::is_at_end));

//^N	f_is_console [Stream | Predicate]
//^P	f_is_console (Stream: Stream) => Bool
//^B	Check for console device.
//^D	Return true, if \Stream is console device.

	DefBuiltin (P_Stream_Check ("f_is_console", Op_Null, &Stream::is_console));

//^N	f_get_length [Stream]
//^P	f_get_length (Stream: Stream) => Int
//^B	Get stream file length.
//^D	Return current length of file for \Stream (or -1, if \Stream is not a file).

	DefBuiltin (P_Stream_GetLength ("f_get_length", Op_Null));

//^N	f_set_length [Stream]
//^P	f_set_length (Stream: Stream, Length: Int) => Int
//^B	Set stream file length.
//^D	Set new length of file for \Stream to \Length (expanding or truncating file).

	DefBuiltin (P_Stream_SetLength ("f_set_length", Op_Null));

//^G	with_input with_output

//^N	with_input [Stream | Wrapper]
//^P	with_input (Input: Stream, @Body: Any) => Any
//^B	Default input stream wrapper.
//^D	Evaluates \Body, with input stream \Input defined as default input.

//^N	with_output [Stream | Wrapper]
//^P	with_output (Output: Stream, @Body: Any) => Any
//^B	Default output stream wrapper.
//^D	Evaluates \Body, with output stream \Output defined as default output.

	DefBuiltin (P_WithStream ("with_input", Op_Null, def_input));
	DefBuiltin (P_WithStream ("with_output", Op_Null, def_output));

//^N	f_attr [Stream]
//^P	f_attr (Stream: Stream) => List
//^B	Get stream info.
//^D	Return list of attributes and properties of \Stream.

	DefBuiltin (P_Stream_Info ("f_attr", Op_Null));

//
//	Stream codecs
//

//^G	codec_UTF8 codec_UTF16 codec_UTF32 codec_SMBC codec_codepage codec_hexdump

//^N	codec_UTF8 [Constructor | Codec]
//^P	codec_UTF8 () => Codec
//^B	UTF-8 codec.
//^D	Creates codec for UTF-8 data encoding/decoding.

//^N	codec_UTF16 [Constructor | Codec]
//^P	codec_UTF16 (Endianness: Bool) => Codec
//^B	UTF-16 codec.
//^D	Creates codec for UTF-16 data encoding/decoding (\Endianness ? Big Endian : Little Endian).

//^N	codec_UTF32 [Constructor | Codec]
//^P	codec_UTF32 (Endianness: Bool) => Codec
//^B	UTF-32 codec.
//^D	Creates codec for UTF-32 data encoding/decoding (\Endianness ? Big Endian : Little Endian).

//^N	codec_SMBC [Constructor | Codec]
//^P	codec_SMBC () => Codec
//^B	SMBC codec.
//^D	Creates codec for SMBC data encoding/decoding.

//^N	codec_codepage [Constructor | Codec]
//^P	codec_codepage (CodePage: String) => Codec
//^B	Code page codec.
//^D	Creates codec for data encoding/decoding with codepage defined with \Codepage.

//^N	codec_hexdump [Constructor | Codec]
//^P	codec_hexdump () => Codec
//^B	Hexdump codec.
//^D	Creates codec for data encoding/decoding as plain hexdigits sequences.

	DefBuiltin (P_SCodec ("codec_UTF8", make_codec_UTF8));
	DefBuiltin (P_SCodec ("codec_UTF16", make_codec_UTF16));
	DefBuiltin (P_SCodec ("codec_UTF32", make_codec_UTF32));
	DefBuiltin (P_SCodec ("codec_SMBC", make_codec_SMBC));
	DefBuiltin (P_SCodec ("codec_codepage", make_codec_codepage));
	DefBuiltin (P_SCodec ("codec_hexdump", make_codec_hexdump));

return true;
}	// init_primaries_stream

DefSubSystem ("stream", init_primaries_stream, 0);

#endif

