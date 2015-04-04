
/*

	+---+---+---+---+---+---+
	|	"E_System.cpp":
	|	OS interface primitives.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

#include "Logger.h"

//
//	Utilities for making lists
//

// Make expression node
static Expr *mkNode (char const *tag, Expr *node, Expr *next) {
return next ? new (tag) X_List (node, next) : node;
}	// mkNode

// Make fixed value list node
Expr * mkFixed (char const *tag, int value, Expr *next) {
return mkNode (tag, new (tag) X_Fixed (value), next);
}	// mkFixed

// Make float value list node
Expr * mkFloat (char const *tag, double value, Expr *next) {
return mkNode (tag, new (tag) X_Float (value), next);
}	// mkFloat

// Make C-string list node
Expr * mkCString (char const *tag, char *string, Expr *next) {
S_string sstr;
sstr.from_cstr (string);
return mkNode (tag, new (tag) X_String (sstr), next);
}	// mkCString

//
//	Expect filename
//

char *expect_filename (Expr *arg, Prefix *where) {
// TODO: special treatment of list arguments...

S_string filename;

if (where->expect_string (arg, filename)) {
	char *c_filename = filename.to_cstring ();
	filename.relink ();
	return c_filename;
	}

return 0;			// (no valid filename)
}	// expect_filename

void free_filename (char const *c_filename) {
if (c_filename) delete c_filename;
}	// free_filename

//
//	File I/O layer
//

#define HEADER

#include "IOSys.cpp"
#include "E_Stream.cpp"

#undef HEADER

#include <fcntl.h>
#include <sys/stat.h>
#include <mem.h>
#include <unistd.h>

//
//	I/O subsystem for file/descriptor
//

struct IOSys_File : IOSystem {
	int fd;
	char const *name;
	int s_errno;

	unsigned get_errno () { return errno; }

	IOSys_File (char *filename, unsigned filemode) {
		fd = ::open (filename, filemode);
		s_errno = get_errno ();
		name = filename;
		}

	IOSys_File (int fd, char const *name) {
		this->fd = fd;
		s_errno = 0;
		this->name = name;
		}

	// Close I/O subsystem
	// (return true on success)
	bool close () {
		if (name && fd > 2) {
			free_filename (name); name = 0;
			}

		if (fd >= 0 && fd > 2) {
			::close (fd); fd = -1;
			return true;
			}

		return true;
		}	// close

	// Read from input stream to 'in_buf' ('in_count' bytes)
	int read (void *in_buf, unsigned in_count) {
		int count = ::_read (fd, in_buf, in_count);		// (low-level data read)

		if (count < 0)
			s_errno = errno;

		return count;
		}	// read

	// Write to output stream from 'out_buf' ('out_count' bytes)
	int write (const void *out_buf, unsigned out_count) {
		int count = ::_write (fd, out_buf, out_count);	// (low-level data write)

		if (count < 0)
			s_errno = errno;

		return count;
		}	// write

	// Seek in stream
	int _lseek (int position, int whence) {
		int result = ::lseek (fd, position, whence);	// (low-level seek)

		if (result < 0)
			s_errno = errno;

		return result;
		}	// _lseek

	// Skip by offset (backward/forward)
	int skip (int offset) {
		return _lseek (offset, SEEK_CUR);
		}	// skip

	// Seek to position (from beginning / from end)
	int seek (int position) {
		return
			position >= 0 ?
				_lseek (position, SEEK_SET)
		:
				_lseek (-position - 1, SEEK_END);
		}	// seek

	// Tell current position
	int tell () {
		return _lseek (0, SEEK_CUR);
		}	// tell

	// Put single octet
	bool put_octet (char octet) {
		return write (&octet, 1) == 1;
		}	// put_octet

	// Get single octet
	bool get_octet (char &octet) {
		return read (&octet, 1) == 1;
		}	// get_octet

	// Sync
	bool sync () { return true; }

	// Check for end
	bool is_at_eos () { return ::eof (fd); }

	// Get current stream length
	int get_length () { return ::filelength (fd); }

	// Set current stream length
	int set_length (unsigned length) { ::chsize (fd, length); }

	// Check for console
	bool is_console () { return ::isatty (fd); }

	// Logging stream
	void log (Logger &log)
		{ log.put_ch ('"')->put_cstr (name)->put_ch ('"'); }

	// Getting stream info
	Expr *get_attr ()
		{ return new X_List (new ("Stream_Name") X_String (name), new ("Stream_FD") X_Fixed (fd)); }

	};	// IOSys_File

// Open file stream
// (for 'filename' with 'mode')
bool Stream::open_file (Prefix *where, Expr *filearg, unsigned mode) {
char *filename = expect_filename (filearg, where);
IOSys_File *io_file = new ("IOSys::file") IOSys_File (filename, mode);
if (io_file->fd >= 0) {
	io_layer = io_file;
	o_flags = mode;

	if (mode == O_RDONLY | mode == O_RDWR)
		o_flags |= stream_read;
	if (mode == O_WRONLY | mode == O_RDWR)
		o_flags |= stream_write;

	o_flags |= stream_seek;				// ???
	return true;
	}

return false;
}	// Stream::open_file

// Open file stream
// (for 'fd' with 'name')
bool Stream::open_fd (int fd, char const *name, unsigned mode) {
io_layer = new ("IOSys::defstream") IOSys_File (fd, name);
o_flags = mode;

return true;
}	// Stream::open_fd

//
//	Generic file operation primitive
//

struct P_FileFunc : Prefix {

	//
	//	System functions dispatch
	//

	enum sysFunc_T {
		type_Fn,
		type_FnFn,
		type_FnIv
		} sysType;

	union {
		int (* sysFunc_Fn)		(char *filename);
		int (* sysFunc_FnFn)	(char *filename1, char *filename2);
		int (* sysFunc_FnIv)	(char *filename, int value);
		} sysFunc;

	//
	//	Constructors
	//

	// Constructor... (*) (char *filename)
	P_FileFunc (char const *ident, int (* arg_Fn) (char *filename)) :
		Prefix (ident, Op_Null)
			{ sysType = type_Fn; sysFunc.sysFunc_Fn = arg_Fn; }

	// Constructor... (*) (char *filename, char *filename)
	P_FileFunc (char const *ident, int (* arg_FnFn) (char *filename1, char *filename2)) :
		Prefix (ident, Op_Null)
			{ sysType = type_FnFn; sysFunc.sysFunc_FnFn = arg_FnFn; }

	// Constructor... (*) (char *filename, char *filename)
	P_FileFunc (char const *ident, int (* arg_FnIv) (char *filename, int value)) :
		Prefix (ident, Op_Null)
			{ sysType = type_FnIv; sysFunc.sysFunc_FnIv = arg_FnIv; }

	//
	//	Evaluation
	//

	VType evalV (VDatum &val, Expr *args) {
		int retval = -1;

		char *filename1;
		if (filename1 = expect_filename (get_arg (args), this)) {
			switch (sysType) {

				case type_Fn:		// op (Fn)
					retval = sysFunc.sysFunc_Fn (filename1);
					break;

				case type_FnFn:
					{ char *filename2;
					if (filename2 = expect_filename (args, this)) {
						retval = sysFunc.sysFunc_FnFn (filename1, filename2);
						free_filename (filename2);
						}
					}
					break;

				case type_FnIv:
					{ S_fixed value = expect_fixed (args, -1);
					retval = sysFunc.sysFunc_FnIv (filename1, value);
					}
					break;

				}	// switch (sysType)
		
		free_filename (filename1);
		}	// (valid filename)

	val._fixed = retval;
	return T_fixed;
	}	// evalV

	};	// P_FileFunc

//
//	File functions
//

#include "io.h"

// Check 'file' access 'mode'
int filefunc_access (char *file, int mode)
	{ return _access (file, mode); }

// Change 'file' access 'mode'
int filefunc_chmod (char *file, int mode)
	{ return _chmod (file, mode); }

// Remove 'file'
int filefunc_remove (char *file)
	{ return remove (file) == 0 ? 0 : errno; }

// Rename 'from_file' to 'to_file'
int filefunc_rename (char *from_file, char *to_file)
	{ return rename (from_file, to_file) == 0 ? 0 : errno; }

// Change current directory to 'directory'
int filefunc_chdir (char *directory)
	{ return chdir (directory) == 0 ? 0 : errno; }

// Make new 'directory'
int filefunc_mkdir (char *directory)
	{ return mkdir (directory) == 0 ? 0 : errno; }

// Remove old 'directory'
int filefunc_rmdir (char *directory)
	{ return rmdir (directory) == 0 ? 0 : errno; }

//
//	Get current directory
//

struct P_GetDirectory : Prefix {

	P_GetDirectory (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		enum { MAXPATH = 128 };
		char dirbuf[MAXPATH+1];
		char *dirptr = _getcwd (dirbuf, MAXPATH);

		val._string.from_cstr (dirbuf);
		return T_string;
		}	// evalV

	};

//
//	Directory iterator
//

#include "dirent.h"

struct P_DirectoryIterator : P_Iterator {

	P_DirectoryIterator (char const *ident) : P_Iterator (ident, Op_Null) {}

	void evaluate (IterContext &IC, Expr *args) {
	char *dirname;
	if (dirname = expect_filename (get_arg (args), this)) {
		Expr *&R_var = expectR_X (get_arg (args));
		DIR *dirhandle = opendir (dirname);

		if (dirhandle) {
		struct dirent *entry;

		IC.start (args);
		while ((entry = readdir (dirhandle))) {
			mutateR_X (R_var,
				new ("XString") X_String (entry->d_name, entry->d_namlen, 0));

			if (! IC.next ()) break;
			}	// while

		closedir (dirhandle);
		}	// (valid dir handle)

		free_filename (dirname);
		}	// dirname
	}	// evaluate

	};	// P_DirectoryIterator

//
//	Time control
//

#include "time.h"

//
//	Get current clock time
//
struct P_GetTime : Prefix {
	P_GetTime (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		time_t cur_time = 0;
		time (&cur_time);
		val._fixed = cur_time;
		return T_fixed;
		}	// evalV
	};

//
//	Split given time value (or current time)
//
struct P_SplitTime : PrefixX {
	bool isUTC;			// ? UTC time : local time

	P_SplitTime (char const *ident, bool isUTC) :
		PrefixX (ident, Op_Null), isUTC(isUTC) {}

	Expr *evalX (Expr *args) {
		time_t v_time = 0;
		if (args)
			v_time = expect_fixed (args, 0);
		else
			time (&v_time);

		struct tm *s_time = isUTC ? gmtime(&v_time) : localtime(&v_time);

		return
			mkFixed	("Date.Year",		1900 + s_time->tm_year,
			mkFixed	("Date.Month",		1 + s_time->tm_mon,
			mkFixed	("Date.Day",		s_time->tm_mday,
			mkFixed	("Date.Hour",		s_time->tm_hour,
			mkFixed	("Date.Min",		s_time->tm_min,
			mkFixed	("Date.Sec",		s_time->tm_sec,
			mkFixed	("Date.Weekday",	s_time->tm_wday,
			mkFixed	("Date.Yearday",	s_time->tm_yday,
			mkFixed	("Date.is_DST",		s_time->tm_isdst,
				0)))))))));
		}	// evalX
	};

//
//	Construct time value
//

static void s_year (struct tm &time, int year)
	{ time.tm_year = year - 1900; }

static void s_month (struct tm &time, int month)
	{ time.tm_mon = month - 1; }

static void s_day (struct tm &time, int day)
	{ time.tm_mday = day; }

static void s_hour (struct tm &time, int hour)
	{ time.tm_hour = hour; }

static void s_min (struct tm &time, int min)
	{ time.tm_min = min; }

static void s_sec (struct tm &time, int sec)
	{ time.tm_sec = sec; }

static void (* s_funclist []) (struct tm &time, int val) =
	{ s_year, s_month, s_day, s_hour, s_min, s_sec, 0 };

struct P_ConsTime : Prefix {
	P_ConsTime (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		struct tm time;
		void (** s_funcptr) (struct tm &time, int val) = s_funclist;

		while (*s_funcptr) {
			S_fixed val = expect_fixed (get_arg (args), 0);
			(* s_funcptr ++) (time, val);
			}
			
		val._fixed = mktime (&time);
		return T_fixed;
		}	// evalV
	};

//
//	Stat file ot stream
//

#include <sys/stat.h>

// (TODO: stat stream...)

struct P_StatFile : PrefixX {
	P_StatFile (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		struct stat status;
		Expr *result = 0;

		char *filename;
		if ((filename = expect_filename (get_arg (args), this))
			&& stat (filename, &status) >= 0) {
		result = 
			mkFixed	("Stat.Device",	status.st_dev,
			mkFixed	("Stat.Inode",	status.st_ino,
			mkFixed	("Stat.RDev",	status.st_rdev,
			mkFixed	("Stat.Mode",	status.st_mode,
			mkFixed	("Stat.Links",	status.st_nlink,
			mkFixed	("Stat.UID",	status.st_uid,
			mkFixed	("Stat.GID",	status.st_uid,
			mkFixed	("Stat.Size",	status.st_size,
			mkFixed	("Stat.ATime",	status.st_atime,
			mkFixed	("Stat.MTime",	status.st_mtime,
			mkFixed	("Stat.CTime",	status.st_ctime,
				0)))))))))));

		free_filename (filename);
		}	// (filename)

	return result;
	}	// evalX

	};	// P_StatFile

//
//	Get locale settings
//

#include <locale.h>

struct P_GetLocale : PrefixX {
	P_GetLocale (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		struct lconv *LC = localeconv ();

		return
			mkCString	("Locale.PosSign",		LC->positive_sign,
			mkCString	("Locale.NegSign",		LC->negative_sign,
			mkFixed		("Locale.FracDig",		LC->int_frac_digits,
			mkFixed		("Locale.Digits",		LC->frac_digits,
			mkFixed		("Locale.Precedes",		LC->p_cs_precedes,
			mkFixed		("Locale.Sep",			LC->p_sep_by_space,
			mkFixed		("Locale.SignPos",		LC->p_sign_posn,
			mkFixed		("Locale.SignNeg",		LC->n_sign_posn,
				0))))))));
		}	// evalX

	};	// P_GetLocale

//
//	Process control
//

#include <process.h>

char **get_argvec (Expr *args, Prefix *where) {
Expr *arg;

unsigned count = 0;

struct Node {
	char *arg_string;
	struct Node *next;
	
	Node (char *arg_str, struct Node *next)
		{ arg_string = arg_str; this->next = next; }

	} *node_list = 0;

while (arg = get_arg (args)) {
	S_string arg_str;
	if (where->expect_string (arg, arg_str)) {
		node_list = new ("argnode") Node (arg_str.to_cstring (), node_list);
		++ count;
		arg_str.relink ();
		}
	}

char **result = new ("argvec") char * [count + 1];
result += count;
*result = 0;
while (count --) {
	*-- result = node_list->arg_string;
	node_list = node_list->next;
	}

return result;
}	// get_argvec

// Execute or spawn process
struct P_ExecSpawn : Prefix {
	bool opmode;		// ? spawn : exec

	P_ExecSpawn (char const *ident, bool opmode) : Prefix (ident, Op_Null) { this->opmode = opmode; }

	VType evalV (VDatum &val, Expr *args) {
	char *exec_name = expect_filename (get_arg (args), this);

	char **args_vec = get_argvec (args, this);

/*
	val._fixed = opmode ?
		spawnvp (exec_name, args_vec):
		execvp (exec_name, args_vec);
 */
 
	val._fixed = spawnvp (0, exec_name, args_vec);
	
	return T_fixed;
	}	// evalV

	};

//
//	Initialisation...
//

static bool init_primaries_system (int order) {

//		[Categories]

//^C	OS
//^B	OS Interaction
//^D	OS Interaction.

//		[Types]

//		--------

//^N	f_access [OS]
//^P	f_access (FileName: String, Mode: Int) => Int
//^B	Verify access to file.
//^D	Check, is file \FileName accessible by \Mode.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_access", filefunc_access));

//^N	f_remove [OS]
//^P	f_remove (FileName: String) => Int
//^B	Remove file permanently.
//^D	Remove file \FileName from file system.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_remove", filefunc_remove));

//^N	f_rename [OS]
//^P	f_rename (FileName: String, NewName: String) => Int
//^B	Rename or move file.
//^D	Rename (or move) file \FileName to \NewName.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_rename", filefunc_rename));

//^N	f_chdir [OS]
//^P	f_chdir (DirName: String) => Int
//^B	Change current directory.
//^D	Change current directory to \DirName.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_chdir", filefunc_chdir));

//^N	f_mkdir [OS]
//^P	f_mkdir (DirName: String) => Int
//^B	Create new directory.
//^D	Create new directory \DirName.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_mkdir", filefunc_mkdir));

//^N	f_rmdir [OS]
//^P	f_rmdir (DirName: String) => Int
//^B	Remove old directory.
//^D	Remove existing directory \DirName.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_rmdir", filefunc_rmdir));

//^N	f_chmod [OS]
//^P	f_chmod (FileName: String, FileMode: Int) => Int
//^B	Change file access mode.
//^D	Change access mode of file \FileName to \FileMode.
//^D	Returns system error status (ERRNO).

	DefBuiltin (P_FileFunc ("f_chmod", filefunc_chmod));

//^N	get_curdir [OS]
//^P	get_curdir () => String
//^B	Get current directory.
//^D	Get and return current working directory.

	DefBuiltin (P_GetDirectory ("get_curdir"));

//^N	dir_loop [OS | Iterator]
//^P	dir_loop (DirName: String, FileName: Mutable, Body: Any) => Any
//^B	Directory iterator.
//^D	Iterate through directory \DirName, setting \FileName to each file, and evaluating \Body.
//^D	Returns final result of \Body.

	DefBuiltin (P_DirectoryIterator ("dir_loop"));

//^N	f_stat [OS]
//^P	f_stat (FileName: String) => List
//^B	Get file information.
//^D	Returns filesystem information about file \FileName.
//^D	Result is: (Device: Int, Inode: Int, Mode: Int, Links: Int, UID: Int, GID: Int, Size: Int, ATime: Int, MTime: Int, CTime: Int)

	DefBuiltin (P_StatFile ("f_stat"));

//^G	time local_time utc_time

//^N	time [OS]
//^P	time () => Int
//^B	Get system time.
//^D	Returns current system time (seconds since Epoch).

	DefBuiltin (P_GetTime ("time"));

//^N	local_time [OS]
//^P	local_time ([Time: Int]) => List
//^B	Split time to local time.
//^D	Convert \Time in Epoch seconds (defaults to current time) to locat time components.
//^D	Result is: (\Year, \Month, \Day, \Hour, \Min, \Sec, \Weekday, \Yearday, \isDST).

//^N	utc_time [OS]
//^P	utc_time ([Time: Int]) => List
//^B	Split time to UTC time.
//^D	Convert \Time in Epoch seconds (defaults to current time) to UTC time components.
//^D	Result is: (\Year, \Month, \Day, \Hour, \Min, \Sec, \Weekday, \Yearday, \isDST).

	DefBuiltin (P_SplitTime ("local_time", 0));
	DefBuiltin (P_SplitTime ("utc_time", 1));

//^N	cons_time [OS]
//^P	cons_time (Year: Int, Month: Int, Day: Int, Hour: Int, Min: Int, Sec: Int) => Int
//^B	Construct time from list.
//^D	Convert time in (Year:Month:Day), (Hour.Min.Sec) format to Epoch seconds.

	DefBuiltin (P_ConsTime ("cons_time"));

	DefBuiltin (P_GetLocale ("get_locale"));

	DefBuiltin (P_ExecSpawn ("_exec", false));
	DefBuiltin (P_ExecSpawn ("_spawn", true));

return true;
}	// init_primaries_system

DefSubSystem ("system", init_primaries_system, 0);

