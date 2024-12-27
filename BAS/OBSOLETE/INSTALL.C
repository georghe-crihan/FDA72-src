#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <direct.h>
#include <process.h>
#include <malloc.h>
#include <io.h>

main(argc,argv,envp)
unsigned argc;
unsigned char *argv[];
unsigned char *envp[];
{
    unsigned char ownpath[64];
    unsigned char apeflg=0,rusflg=0;
    unsigned char language[2];
    unsigned char insdir[]="?:\\FDA";
    unsigned char machinsflg=0;

    {
	unsigned char i;

	for (i=0; (argv[0])[i]; i++);
	for (; (argv[0])[i]!='\\'; i--);
	ownpath[++i]=0;
	do
	{
	    i--;
	    ownpath[i]=(argv[0])[i];
	}
	while (i);
    }

    {
	unsigned char userinput[3];

	printf("\n"
	       "╔═══════════════════════════════════════════════════╗\n"
	       "║           *** Floppy Disk Analyser ***            ║\n"
	       "║               Installation program                ║\n"
	       "║                Программа установки                ║\n"
	       "║                                                   ║\n"
	       "║        Copyright (C) MTC Medincom 1991-97         ║\n"
	       "║                                                   ║\n"
	       "║ 141120 Moscow region, Fryazino-5, box 108, Russia ║\n"
	       "║ Ph. in USA: 216-646-1864,  E-mail: sokolov@en.com ║\n"
	       "║                                                   ║\n"
	       "║ 141120 Московская область, Фрязино-5, а/я 108     ║\n"
	       "║ Тел. в США: 216-646-1864,  E-mail: sokolov@en.com ║\n"
	       "╚═══════════════════════════════════════════════════╝\n"
	       "\n"
	       "Select version to install:   Выберите вариант установки:\n"
	       "\n"
	       "1. English Professional      1. Профессиональный англоязычный\n"
	       "2. English Automatic         2. Автоматический англоязычный\n"
	       "3. Russian Automatic         3. Автоматический русскоязычный\n"
	       "E. Exit from program         E. Выход из программы\n"
	       "\n"
	       "Enter letter or digit:       Введите букву или цифру:\n->");
	fgets(userinput,3,stdin);
	if (ferror(stdin))
	    goto userinputerr;
	switch (userinput[0])
	{
	    case '1': break;
	    case '2': apeflg=1; strcpy(language,"E"); break;
	    case '3': apeflg=1; rusflg=1; strcpy(language,"R"); break;
	    default: exit(1);
	}
    }

    {
	unsigned char *msg;
	unsigned char userinput[4];
	unsigned char ch;

	if (rusflg==0)
	    msg="\nEnter drive letter for installation (Enter without letter - no installation):";
	else
	    msg="\nВведите букву диска для установки (Enter без буквы - отказ от установки):";
	printf(msg);
	fgets(userinput,4,stdin);
	if (ferror(stdin))
	    goto userinputerr;
	if (userinput[0]==0 || userinput[0]=='\n')
	    exit(1);
	if (userinput[1]!=0 && userinput[1]!='\n' && userinput[1]!=':')
	    goto invaldspec;
	if (userinput[2]!=0 && userinput[2]!='\n')
	    goto invaldspec;
	ch=toupper(userinput[0]);
	if (ch<'A')
	    goto invaldspec;
	if (ch>'Z')
	    goto invaldspec;
	insdir[0]=ch;
    }

    {
	FILE *filedesc;
	unsigned char *outstr;
	unsigned char *strlist[34]=
	{
	    "@ECHO OFF\nCOPY ",
	    ownpath,
	    "",
	    "PACK.DAT/B ",
	    insdir,
	    "\\PACKFILE.ZIP/B > NUL\nCOPY ",
	    ownpath,
	    "UNPACKER.DAT/B ",
	    insdir,
	    "\\PKUNZJR.COM/B > NUL\n",
	    insdir,
	    "\\PKUNZJR ",
	    insdir,
	    "\\PACKFILE.ZIP ",
	    insdir,
	    "\\ > NUL\nDEL ",
	    insdir,
	    "\\PKUNZJR.COM\nDEL ",
	    insdir,
	    "\\PACKFILE.ZIP\nCOPY ",
	    ownpath,
	    "README.TXT/B ",
	    insdir,
	    "\\README.TXT/B > NUL\nCOPY ",
	    ownpath,
	    "WHATELSE.TXT/B ",
	    insdir,
	    "\\WHATELSE.TXT/B > NUL\n",
	    "REN ",
	    insdir,
	    "\\FDA_.* FDA_",
	    language,
	    ".*\n",
	    NULL
	};
	unsigned length=1;
	unsigned char i;
	unsigned char spec[19]="";

	if (access(insdir,0)==0)
	    goto alreadyins;
	if (mkdir(insdir))
	    goto installerr;
	strcat(spec,insdir);
	strcat(spec,"\\INSTALL.BAT");
	filedesc=fopen(spec,"wt");
	if (filedesc==NULL)
	    goto installerr;
	if (apeflg==0)
	{
	    strlist[2]="PRO";
	    strlist[28]=NULL;
	}
	else
	    strlist[2]="AUTO";
	for (i=0; strlist[i]!=NULL; i++)
	    length=length+strlen(strlist[i]);
	outstr=malloc(length);
	if (outstr==NULL)
	    goto outofmemory;
	*outstr=0;
	for (i=0; strlist[i]!=NULL; i++)
	    strcat(outstr,strlist[i]);
	fprintf(filedesc,outstr);
	if (ferror(filedesc))
	    goto installerr;
	if (fclose(filedesc))
	    goto installerr;
	free(outstr);
	if (system(spec))
	    goto installerr;
	if (unlink(spec))
	    goto installerr;
    }

    {
	unsigned char *msg;
	unsigned char userinput[3];

	if (rusflg==0)
	{
	    msg="\n"
		"Installation complete\n"
		"\n"
		" Now you can machine-install the program. In this case you\n"
		"won't need to insert keydisk each time you are running the\n"
		"program but the number of machine-installations is limited\n"
		"\n"
		"Install program on machine (Y/N)?";
	}
	else
	{
	    msg="\n"
		"Установка завершена\n"
		"\n"
		" Теперь вы можете привязать программу к машине.  В  этом  случае\n"
		"вам не потребуется вставлять ключевую дискету при каждом запуске\n"
		"программы, однако число привязок к машине ограничено\n"
		"\n"
		"Привязать программу к машине (Y/N)?";
	}
	printf(msg);
	fgets(userinput,3,stdin);
	if (ferror(stdin))
	    goto userinputerr;
	if ((userinput[0] & 0xDF)=='Y')
	    machinsflg=1;
    }

    if (machinsflg)
    {
	unsigned char spec[17]="";

	strcat(spec,insdir);
	strcat(spec,"\\FDA");
	if (apeflg)
	{
	    strcat(spec,"_");
	    strcat(spec,language);
	}
	strcat(spec,".EXE");
	if (spawnl(P_WAIT,spec,spec,"/M",NULL)==-1)
	    goto installerr;
    }

    exit(0);

    {
	unsigned char *msg,*emsg,*rmsg;

userinputerr:
	emsg="User input redirection error\n";
	rmsg="Ошибка переадресации пользовательского ввода\n";
	goto errhandle;
invaldspec:
	emsg="Invalid drive specification\n";
	rmsg="Неправильная спецификация диска\n";
	goto errhandle;
alreadyins:
	emsg="Program is already installed on this disk\n";
	rmsg="Программа уже установлена на этот диск\n";
	goto errhandle;
installerr:
	emsg="Disk error during installation\n";
	rmsg="Дисковая ошибка во время установки\n";
	goto errhandle;
outofmemory:
	emsg="Out of memory\n";
	rmsg="Недостаточно памяти\n";
errhandle:
	if (rusflg==0)
	    msg=emsg;
	else
	    msg=rmsg;
	printf(msg);
	exit(2);
    }
}
