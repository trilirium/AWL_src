
/*

	+---+---+---+---+---+---+
	|	"char_symdef.cpp":
	|	Symbolic chars definitions.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

//
//	Unicode extended
//	(http://webdesign.about.com/library/bl_htmlcodes.htm)
//

#include <string.h>

struct char_symdef {
	char const *symdef;
	unsigned u_code;
	};

static char_symdef symdef_main [] = {
	// Combining diacritics

	" `",		0x300,		// grave combining
	" '",		0x301,		// acute combining
	" ^",		0x302,		// circumflex combining
	" ~",		0x303,		// tilde combining
	" -",		0x304,		// macron combining
	" !",		0x305,		// overline combining
	" @",		0x306,		// breve combining
	" .",		0x307,		// dot above combining
	" :",		0x308,		// umlaut combining

	//
	//	Latin letters variants
	//

	// "A" variations

	"`A",		0xC0,		// A grave
	"`a",		0xE0,		// a grave

	"'A",		0xC1,		// A acute
	"'a",		0xE1,		// a acute

	"^A",		0xC2,		// A circumflex
	"^a",		0xE2,		// a circumflex

	"~A",		0xC3,		// A tilde
	"~a",		0xE3,		// a tilde

	":A",		0xC4,		// A umlaut
	":a",		0xE4,		// a umlaut

	"%A",		0xC5,		// A ring
	"%a",		0xE5,		// a ring

	"AE",		0xC6,		// AE ligature
	"ae",		0xE6,		// ae ligature

	"-A",		0x100,		// A macron
	"-a",		0x101,		// a macron

	"@A",		0x102,		// A breve
	"@a",		0x103,		// a breve

	",A",		0x104,		// A ogonek
	",a",		0x105,		// a ogonek

	"'AE",		0x1FC,		// AE ligature acute
	"'ae",		0x1FD,		// AE ligature acute

	// "B" variations

	".B",		0x1E02,		// B dot
	".b",		0x1E03,		// b dot

	// "C" variations

	",C",		0xC7,		// C cedilla
	",c",		0xE7,		// c cedilla

	"'C",		0x106,		// C acute
	"'c",		0x107,		// c acute

	"^C",		0x108,		// C circumflex
	"^c",		0x109,		// c circumflex

	".C",		0x10A,		// C dot
	".c",		0x10B,		// c dot

	">C",		0x10C,		// C hachek
	">c",		0x10D,		// c hachek

	// "D" variations

	",D",		0x1E10,		// D cedilla
	",d",		0x1E11,		// d cedilla

	">D",		0x10E,		// D hachek
	">d",		0x10E,		// d hachek

	".D",		0x1E0A,		// D dot
	".d",		0x1E0B,		// d dot

	"=D",		0xD0,		// ETH icelandic
	"=d",		0xF0,		// eth icelandic

	"-D",		0x110,		// D stroke
	"-d",		0x111,		// d stroke

	"DZ",		0x1F1,		// DZ ligature
	"Dz",		0x1F2,		// Dz ligature
	"dz",		0x1F3,		// dz ligature

	">DZ",		0x1C4,		// DZ hachek
	">Dz",		0x1C5,		// Dz hachek
	">dz",		0x1C6,		// dz hachek

	// "E" variations

	"`E",		0xC8,		// E grave
	"`e",		0xE8,		// e grave

	"'E",		0xC9,		// E acute
	"'e",		0xE9,		// e acute

	"^E",		0xCA,		// E circumflex
	"^e",		0xEA,		// e circumflex

	":E",		0xCB,		// E umlaut
	":e",		0xEB,		// e umlaut

	"-E",		0x112,		// E macron
	"-e",		0x113,		// e macron

	"@E",		0x114,		// E breve
	"@e",		0x115,		// e breve

	".E",		0x116,		// E dot
	".e",		0x117,		// e dot

	",E",		0x118,		// E ogonek
	",e",		0x119,		// e ogonek

	">E",		0x11A,		// E hachek
	">e",		0x11B,		// e hachek

	// Ezh, Ezh + hachek ????

	// "F" variations

	".F",		0x1E1E,		// F dot
	".f",		0x1E1F,		// f dot

	// f-hook ????

	"ff",		0xFB00,		// ff ligature
	"fi",		0xFB01,		// fi ligature
	"fl",		0xFB02,		// fl ligature
	"ffi",		0xFB03,		// ffi ligature
	"ffl",		0xFB04,		// ffl ligature
	"ft",		0xFB05,		// ft ligature

	// "G" variations

	"'G",		0x1F4,		// G acute
	"'g",		0x1F5,		// g acute

	"^G",		0x11C,		// G circumflex
	"^g",		0x11D,		// g circumflex

	"@G",		0x11E,		// G breve
	"@g",		0x11F,		// g breve

	".G",		0x120,		// G dot
	".g",		0x121,		// g dot

	",G",		0x122,		// G cedilla
	",g",		0x123,		// g cedilla

	"-G",		0x1E4,		// G stroke
	"-g",		0x1E5,		// g stroke

	">G",		0x1E6,		// G hachek
	">g",		0x1E7,		// g hachek

	// "H" variations

	"^H",		0x124,		// H circumflex
	"^h",		0x125,		// h circumflex

	"-H",		0x126,		// H stroke
	"-h",		0x127,		// h stroke

	// "I" variations

	"`I",		0xCC,		// I grave
	"`i",		0xEC,		// i grave

	"'I",		0xCD,		// I acute
	"'i",		0xED,		// i acute

	"^I",		0xCE,		// I circumflex
	"^i",		0xEE,		// i circumflex

	":I",		0xCF,		// I umlaut
	":i",		0xEF,		// i umlaut

	"~I",		0x128,		// I tilde
	"~i",		0x129,		// i tilde

	"-I",		0x12A,		// I macron
	"-i",		0x12B,		// i macron

	"@I",		0x12C,		// I breve
	"@i",		0x12D,		// i breve

	",I",		0x12E,		// I ogonek
	",i",		0x12F,		// i ogonek

	".I",		0x130,		// I dot
	".i",		0x131,		// i dotless (!!)

	"IJ",		0x132,		// IJ ligature
	"ij",		0x133,		// ij ligature

	// "J" variations

	"^J",		0x134,		// J circumflex
	"^j",		0x135,		// j circumflex

	// "K" variations

	",K",		0x136,		// K cedilla
	",k",		0x137,		// k cedilla

	"_K",		0x138,		// small K

	">K",		0x1E8,		// K hachek
	">k",		0x1E9,		// k hachek

	"'K",		0x1E30,		// K acute
	"'k",		0x1E31,		// k acute

	// "L" variations

	"'L",		0x139,		// L acute
	"'l",		0x13A,		// l acute

	",L",		0x13B,		// L cedilla
	",l",		0x13C,		// l cedilla

	">L",		0x13D,		// L hachek
	">l",		0x13E,		// l hachek

	"*L",		0x13F,		// L middle dot
	"*l",		0x140,		// l middle dot

	"/L",		0x141,		// L stroke
	"/l",		0x142,		// l stroke

	"LJ",		0x1C7,		// LJ ligature
	"Lj",		0x1C8,		// Lj ligature
	"lj",		0x1C9,		// lj ligature

	// "M" variations

	".M",		0x1E40,		// M dot
	".m",		0x1E41,		// m dot

	// "N" variations

	"~N",		0xD1,		// N tilde
	"~n",		0xF1,		// n tilde

	"'N",		0x143,		// N acute
	"'n",		0x144,		// n acute

	",N",		0x145,		// N cedilla
	",n",		0x146,		// n cedilla

	">N",		0x147,		// N hachek
	">n",		0x148,		// n hachek

	"/n",		0x149,		// n apostrophe

	"NG",		0x14A,		// ENG
	"ng",		0x14B,		// eng

	"NJ",		0x1CA,		// NJ ligature
	"Nj",		0x1CB,		// Nj ligature
	"nj",		0x1CC,		// nj ligature

	// "O" variations

	"`O",		0xD2,		// O grave
	"`o",		0xF2,		// o grave

	"'O",		0xD3,		// O acute
	"'o",		0xF3,		// o acute

	"^O",		0xD4,		// O circumflex
	"^o",		0xF4,		// o circumflex

	"~O",		0xD5,		// O tilde
	"~o",		0xF5,		// o tilde

	":O",		0xD6,		// O umlaut
	":o",		0xF6,		// o umlaut

	"/O",		0xD8,		// O slash
	"/o",		0xF8,		// o slash

	"-O",		0x14C,		// O macron
	"-o",		0x14D,		// o macron

	"@O",		0x14E,		// O breve
	"@o",		0x14F,		// o breve

	"\"O",		0x150,		// O double acute
	"\"o",		0x151,		// o double acute

	"OE",		0x152,		// OE ligature
	"oe",		0x153,		// oe ligature

	"'/O",		0x1FE,		// O acute slash
	"'/o",		0x1FF,		// o acute slash

	// "P" variations

	".P",		0x1E56,		// P dot
	".p",		0x1E57,		// p dot

	// "R" variations

	"'R",		0x154,		// R acute
	"'r",		0x155,		// r acute

	",R",		0x156,		// R cedilla
	",r",		0x157,		// r cedilla

	">R",		0x158,		// R hachek
	">r",		0x159,		// r hachek

	"|r",		0x27C,		// r long leg

	// "S" variations

	"'S",		0x15A,		// S acute
	"'s",		0x15B,		// s acute

	"^S",		0x15C,		// S circumflex
	"^s",		0x15D,		// s circumflex

	",S",		0x15E,		// S cedilla
	",s",		0x15F,		// s cedilla

	">S",		0x160,		// S hachek
	">s",		0x161,		// s hachek

	"-s",		0x17F,		// s long

	"sz",		0xDF,		// sz ligature

	".S",		0x1E60,		// S dot
	".s",		0x1E61,		// s dot

	// "T" variations

	"=T",		0xDE,		// THORN
	"=t",		0xFE,		// thorn

	",T",		0x162,		// T cedilla
	",t",		0x163,		// t cedilla

	">T",		0x164,		// T hachek
	">t",		0x165,		// t hachek

	"-T",		0x166,		// T stroke
	"-t",		0x167,		// t stroke

	".T",		0x1E6A,		// T dot
	".t",		0x1E6B,		// t dot

	// "U" variations

	"`U",		0xD9,		// U grave
	"`u",		0xF9,		// u grave

	"'U",		0xDA,		// U acute
	"'u",		0xFA,		// u acute

	"^U",		0xDB,		// U circumflex
	"^u",		0xFB,		// u circumflex

	":U",		0xDC,		// U umlaut
	":u",		0xFC,		// u umlaut

	"~U",		0x168,		// U tilde
	"~u",		0x169,		// u tilde

	"-U",		0x16A,		// U macron
	"-u",		0x16B,		// u macron

	"@U",		0x16C,		// U breve
	"@u",		0x16D,		// u breve

	"%U",		0x16E,		// U ring
	"%u",		0x16F,		// u ring

	"\"U",		0x170,		// U double acute
	"\"u",		0x171,		// u double acute

	",U",		0x172,		// U ogonek
	",u",		0x173,		// u ogonek

	// "W" variations

	"^W",		0x174,		// W circumflex
	"^w",		0x175,		// w circumflex

	"`W",		0x1E80,		// W grave
	"`w",		0x1E81,		// w grave

	"'W",		0x1E82,		// W acute
	"'w",		0x1E83,		// w acute

	":W",		0x1E84,		// W umlaut
	":w",		0x1E85,		// w umlaut

	// "Y" variations

	"`Y",		0x1EF2,		// Y grave
	"`y",		0x1EF3,		// y grave

	"^Y",		0x176,		// Y circumflex
	"^y",		0x177,		// y circumflex

	"'Y",		0xDD,		// Y acute
	"'y",		0xFD,		// y acute

	":Y",		0x178,		// Y umlaut
	":y",		0xFF,		// y umlaut

	// "Z" variations

	"'Z",		0x179,		// Z acute
	"'z",		0x17A,		// z acute

	".Z",		0x17B,		// Z dot
	".z",		0x17C,		// z dot

	">Z",		0x17D,		// Z hachek
	">z",		0x17E,		// z hachek

	// Special graphic representations

	"!!",		0xA1,		// inverse exclamation
	"??",		0xBF,		// inverse question

	"-",		0x2013,		// short dash
	"--",		0x2014,		// long dash

	"...",		0x2026,		// ellipsis

	"*",		0x2022,		// bullet

	"%%",		0x2030,		// per mille

	"<<",		0xAB,		// left chevron quote
	">>",		0xBB,		// right chevron quote

	"(tm)",		0x2122,		// trademark
	"(C)",		0xA9,		// copyright
	"(R)",		0xAE,		// registered

	"_%",		0xB0,		// degree
	"/",		0x2044,		// fraction slash
	"1/4",		0xBC,		// fraction 1/4
	"1/2",		0xBD,		// fraction 1/2
	"3/4",		0xBE,		// fraction 3/4

	"_pound",	0xA3,		// pound sign
	"_euro",	0x20AC,		// euro sign
	"_yen",		0xA5,		// yen sign
	"_crcy",	0xA4,		// currency sign

	//
	//	Greek letters
	//
	
	"Alpha",	0x0391,
	"Beta",		0x0392,
	"Gamma",	0x0393,
	"Delta",	0x0394,
	"Epsilon",	0x0395,
	"Zeta",		0x0396,
	"Eta",		0x0397,
	"Theta",	0x0398,
	"Iota",		0x0399,
	"Kappa",	0x039A,
	"Lambda",	0x039B,
	"Mu",		0x039C,
	"Nu",		0x039D,
	"Xi",		0x039E,
	"Omicron",	0x039F,
	"Pi",		0x03A0,
	"Rho",		0x03A1,
	"Sigma",	0x03A3,
	"Tau",		0x03A4,
	"Upsilon",	0x03A5,
	"Phi",		0x03A6,
	"Chi",		0x03A7,
	"Psi",		0x03A8,
	"Omega",	0x03A9,

	"alpha",	0x03B1,
	"beta",		0x03B2,
	"gamma",	0x03B3,
	"delta",	0x03B4,
	"epsilon",	0x03B5,
	"zeta",		0x03B6,
	"eta",		0x03B7,
	"theta",	0x03B8,
	"iota",		0x03B9,
	"kappa",	0x03BA,
	"lambda",	0x03BB,
	"mu",		0x03BC,
	"nu",		0x03BD,
	"xi",		0x03BE,
	"omicron",	0x03BF,
	"pi",		0x03C0,
	"rho",		0x03C1,
	"sigma",	0x03C3,
	"tau",		0x03C4,
	"upsilon",	0x03C5,
	"phi",		0x03C6,
	"chi",		0x03C7,
	"psi",		0x03C8,
	"omega",	0x03C9,

	};		// symdef_main

// Look for symdef
int symdef_lookup (char *from, unsigned len) {
unsigned total = sizeof(symdef_main) / sizeof(char_symdef);

for (unsigned i = 0; i != total; ++ i) {
	char const *symdef = symdef_main[i].symdef;
	unsigned l = strlen(symdef);
	
	if (l == len && strncmp(from, symdef, l) == 0)
		return symdef_main[i].u_code;
	}

return -1;			// (not found)
}	// symdef_lookup

