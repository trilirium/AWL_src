
#
#
#	Make file for AWL
#
#

#	(common compiler flags)
CFLAGS = -DTARGET_WIN32

#	(C++ compiler)
CC = g++ -c $(CFLAGS)

#	(Lex)
LEX = flex

#	(Yacc)
YACC = bison -d

#	(Object files directory)
OBJDIR = ../obj

$(OBJDIR)/Main.o:	Main.cpp
	$(CC) Main.cpp -o $(OBJDIR)/Main.o

$(OBJDIR)/Syntor.o: Syntor.cpp AWL_syn.y
	$(YACC) AWL_syn.y
	$(CC) Syntor.cpp -o $(OBJDIR)/Syntor.o
	mv AWL_syn.tab.* $(OBJDIR)

$(OBJDIR)/Lexer.o: Lexer.cpp AWL_lex.l $(OBJDIR)/Syntor.o
	$(LEX) AWL_lex.l
	$(CC) Lexer.cpp -o $(OBJDIR)/Lexer.o
	mv lex.yy.c $(OBJDIR)

$(OBJDIR)/IOSys.o: IOSys.cpp
	$(CC) IOSys.cpp -o $(OBJDIR)/IOSys.o

$(OBJDIR)/CPUMan.o:	CPUMan.cpp CPUMan.h
	$(CC) CPUMan.cpp -o $(OBJDIR)/CPUMan.o

$(OBJDIR)/MemMan.o:	MemMan.cpp MemMan.h
	$(CC) MemMan.cpp -o $(OBJDIR)/MemMan.o

$(OBJDIR)/Kernel.o:	Kernel.cpp
	$(CC) Kernel.cpp -o $(OBJDIR)/Kernel.o

$(OBJDIR)/Dynaload.o:	Dynaload.cpp
	$(CC) Dynaload.cpp -o $(OBJDIR)/Dynaload.o

$(OBJDIR)/NameTab.o:	NameTab.cpp
	$(CC) NameTab.cpp -o $(OBJDIR)/NameTab.o

$(OBJDIR)/Logger.o:	Logger.cpp Logger.h
	$(CC) Logger.cpp -o $(OBJDIR)/Logger.o

$(OBJDIR)/ExpImp.o:	ExpImp.cpp
	$(CC) ExpImp.cpp -o $(OBJDIR)/ExpImp.o

$(OBJDIR)/Eval.o:	Eval.cpp Eval.h
	$(CC) Eval.cpp -o $(OBJDIR)/Eval.o

$(OBJDIR)/StringR.o:	StringR.cpp Eval.h
	$(CC) StringR.cpp -o $(OBJDIR)/StringR.o

$(OBJDIR)/E_Debug.o:	E_Debug.cpp
	$(CC) E_Debug.cpp -o $(OBJDIR)/E_Debug.o

$(OBJDIR)/E_Num.o:	E_Num.cpp
	$(CC) E_Num.cpp -o $(OBJDIR)/E_Num.o

$(OBJDIR)/E_String.o:	E_String.cpp String.h
	$(CC) E_String.cpp -o $(OBJDIR)/E_String.o

$(OBJDIR)/E_Mut.o:	E_Mut.cpp
	$(CC) E_Mut.cpp -o $(OBJDIR)/E_Mut.o

$(OBJDIR)/E_Control.o:	E_Control.cpp
	$(CC) E_Control.cpp -o $(OBJDIR)/E_Control.o

$(OBJDIR)/Coerce.o:	Coerce.cpp
	$(CC) Coerce.cpp -o $(OBJDIR)/Coerce.o

$(OBJDIR)/E_List.o:	E_List.cpp
	$(CC) E_List.cpp -o $(OBJDIR)/E_List.o

$(OBJDIR)/E_Func.o:	E_Func.cpp
	$(CC) E_Func.cpp -o $(OBJDIR)/E_Func.o

$(OBJDIR)/E_Stream.o:	E_Stream.cpp
	$(CC) E_Stream.cpp -o $(OBJDIR)/E_Stream.o

$(OBJDIR)/E_Object.o:	E_Object.cpp
	$(CC) E_Object.cpp -o $(OBJDIR)/E_Object.o

$(OBJDIR)/E_Array.o:	E_Array.cpp
	$(CC) E_Array.cpp -o $(OBJDIR)/E_Array.o

$(OBJDIR)/E_Hash.o:	E_Hash.cpp
	$(CC) E_Hash.cpp -o $(OBJDIR)/E_Hash.o

$(OBJDIR)/E_Pattern.o:	E_Pattern.cpp
	$(CC) E_Pattern.cpp -o $(OBJDIR)/E_Pattern.o

$(OBJDIR)/E_Ring.o:	E_Ring.cpp
	$(CC) E_Ring.cpp -o $(OBJDIR)/E_Ring.o

$(OBJDIR)/E_Xcpt.o:	E_Xcpt.cpp Defs.h
	$(CC) E_Xcpt.cpp -o $(OBJDIR)/E_Xcpt.o

$(OBJDIR)/E_System.o:	E_System.cpp Defs.h
	$(CC) E_System.cpp -o $(OBJDIR)/E_System.o

$(OBJDIR)/Sortable.o:	Sortable.cpp
	$(CC) Sortable.cpp -o $(OBJDIR)/Sortable.o

$(OBJDIR)/Unicode.o:	Unicode.cpp Defs.h
	$(CC) Unicode.cpp -o $(OBJDIR)/Unicode.o

$(OBJDIR)/char_symdef.o:	char_symdef.cpp
	$(CC) char_symdef.cpp -o $(OBJDIR)/char_symdef.o

OBJS =	$(OBJDIR)/Main.o $(OBJDIR)/Lexer.o $(OBJDIR)/Syntor.o					\
	$(OBJDIR)/IOSys.o $(OBJDIR)/CPUMan.o $(OBJDIR)/MemMan.o					\
	$(OBJDIR)/char_symdef.o $(OBJDIR)/Dynaload.o						\
	$(OBJDIR)/Kernel.o $(OBJDIR)/NameTab.o $(OBJDIR)/Logger.o $(OBJDIR)/ExpImp.o			\
	$(OBJDIR)/StringR.o $(OBJDIR)/Eval.o $(OBJDIR)/Coerce.o $(OBJDIR)/Unicode.o			\
	$(OBJDIR)/Sortable.o								\
	$(OBJDIR)/E_Num.o $(OBJDIR)/E_String.o $(OBJDIR)/E_Control.o $(OBJDIR)/E_List.o			\
	$(OBJDIR)/E_Mut.o $(OBJDIR)/E_Stream.o $(OBJDIR)/E_Object.o $(OBJDIR)/E_Func.o			\
	$(OBJDIR)/E_Array.o $(OBJDIR)/E_Hash.o $(OBJDIR)/E_Ring.o $(OBJDIR)/E_Pattern.o			\
	$(OBJDIR)/E_Xcpt.o $(OBJDIR)/E_System.o $(OBJDIR)/E_Debug.o

AWL = ../bin/awl.exe

$(AWL):	$(OBJS)
	g++ $(OBJS) -static -o $(AWL)
	strip $(AWL)
	objdump -p $(AWL) | perl ../bin/defmaker.pl awl.exe > ../bin/awl.def
	# dlltool -d ../bin/awl.def -l ../bin/awl.a

awl:	$(AWL)

#
#	(other tasks)
#

clean:
	rm $(OBJS) $(OBJDIR)/AWL_syn.tab.c $(OBJDIR)/AWL_syn.tab.h $(OBJDIR)/lex.yy.c

ARCPATH = ../_arch

ARC = D:/Programs/7za/7za.exe

ARCFILTER = Makefile *.cpp *.h *.l *.y

arc:
	$(ARC) a $(ARCPATH)/AWL_src $(ARCFILTER)
tarc:
	$(ARC) t $(ARCPATH)/AWL_src.7z

wc:
	wc *.c *.cpp *.h *.l *.y

