
//
//	"Dynaload.cpp":
//
//	Module dynamic loader
//

#include "Defs.h"
#include "Logger.h"

#include <windows.h>

// Module load error
// (To be done)
struct DynaloadError : ExecError {
	char *filename;
	int code;

	DynaloadError (char *filename, int code)
		{ this->code = code; this->filename = filename; }

	D_Error__report;
	};

//
//	Dump module information
//

static void log_modname (Logger &log, char const *file_name) {
log.put_cstr ("Dynamic module '")->put_cstr (file_name)->put_cstr ("': ");
}	// log_modname

static void log_loadOK (Logger &log, HINSTANCE handle) {
log.put_cstr ("loaded OK @")->put_hex (8, (unsigned) handle)->put_ch ('.')->put_nl ();
}	// log_loadOK

static void log_loadFail (Logger &log) {
log.put_cstr ("load failure [error: ")->put_hex (4, GetLastError ())->put_cstr ("].")->put_nl ();
}	// log_loadFail

static void log_entry (Logger &log, char const *entry_info, void *entry_addr) {
log.put_cstr ("Entry point '")->put_cstr (entry_info)->
	put_cstr ("': found @")->put_hex (8, (unsigned) entry_addr)->put_ch ('.')->put_nl ();
}	// log_entry

void DynamicModInfo::log_info (Logger &log) {
log.put_cstr ("Module '")->put_cstr (dmi_name)->put_cstr ("': ")->put_cstr (dmi_description)->put_nl ();
log.put_cstr ("(version ")->log_version (dmi_version);
log.put_cstr (" from ")->put_cstr (dmi_date)->put_cstr (").")->put_nl ();
}	// DynamicModInfo::log_info

//
//	Dynamic loading
//

int dynaload_module (char const *file_name, char const *entry_info, DynamicModInfo *dmi) {
log_modname (*syslog, file_name);

if (file_name && entry_info) {

HINSTANCE HLibrary = LoadLibrary (file_name);
if (HLibrary) {
	log_loadOK (*syslog, HLibrary);

	DynamicModInfo *entry_point = (DynamicModInfo *) GetProcAddress (HLibrary, entry_info);
	if (entry_point) {
		log_entry (*syslog, entry_info, entry_point);
		*dmi = *entry_point;
		return 0;
		}
	}
else { log_loadFail (*syslog); }
}

return -1;		// (any error)
}	// dynaload_module

