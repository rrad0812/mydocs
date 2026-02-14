# C PROGRAMMING

## Scripts for SMALLCOM

Al Stevens, juni '89

Last month we built a generic interpreter of a C-like language that can be used to support communications scripts or editor macro processing. The language is called S, and the interpreter is called SI. We included an example shell program that you can now discard. The shell was to show how you surround the interpreter engine with a user interface and intrinsic functions to give purpose to the interpreted code. The SI interpreter engine has no permanent dedicated shell because the different custom shell programs that you might develop give each use of SI its purpose. Therefore, an SI implementation will always include the development of a custom shell program.

This month we will build a shell that uses SI to implement a communications script processor into the SMALLCOM program. The shell and the interpreter program link with the rest of SMALLCOM through a hook in smallcom.c. We will need all the code used to build SMALLCOM from months past, the files named interp.h and interp.c from May 1989, and the script.c file included this month as Listing One. The complete source code package dates back to September, 1988. Each succeeding month is a complete toolset, usable in many other programs, and yet the code from each month adds to the program we are building.

A reader asked if I have the communications program already completed and am parcelling it out in little chunks. The answer is no. This program is developing as you watch it. I am about four months ahead of you, though; these words are being written in February for the June issue. This incremental approach has its pros and cons. On the up side, the code is new and known to work with the latest versions of the Microsoft and Turbo C compilers, and we build new layers of software drawing on our experiences with the old. On the down side, I sometimes find that I have coded myself into a corner. In the process of adding functions to the existing program I sometimes wish I could make little changes to the earlier code. In the real world, you would do just that --modify the program to fit the purpose of the hour. This, however, is not the real world. In the continuing saga of this column and its project, I am trying to preserve the code already published. That is often frustrating. As I learn new and better ways to do things, I want to go into that old code and fix it up. Maybe a future column can be dedicated to revisiting old code and giving it a polish. I have always believed that all software would be better if we were allowed to write it twice --once to learn how it should work and once to do the things we wish we had done the first time out. The three-piece suits tell me that my approach is not practical when viewed from the perch of the bottom line.

Every now and then I have no choice but to fix some old code from columns past. For example, this month I learned that I needed the file pointer for the SMALLCOM log file to be external, rather than static, so that I could use it from within the script processor. Listing Two is junehook.c, the code that has the changes we need to add the script interpreter to SMALLCOM. We need to make two changes to smallcom.c from the March '89 column. The first makes the log file pointer external. The second adds the script processor's hook to the program. There are no apologies for the second one; that is the modus operandi described for this project in April.

The SMALLCOM program was built with the Compact memory model in March. The only advantage to that is that long documents are permitted in the integrated editor. The script interpreter uses integer offsets as pointers. The address and pointer operators are significantly easier to implement in the small model, and so we will change the SMALLCOM model to the small one. The .MAK file for Microsoft C users and the environment setup for Turbo C users must be changed to the small model. In smallcom.c change the MAXLINES assignment from 800 to 200. This assignment is in the comeditor function on about line 474. Documents that you edit from within SMALLCOM are now limited to 200 lines. This is no loss because on-line messages should be kept short anyway.

## The Script Language

Our script language is S, in the syntax and grammar described last month. You will recognize it as a subset of C. To be a communications script language, S must be extended by the addition of intrinsic functions built into its shell. The script.c shell program has several such functions as described next. When you write a script, you build a text file that contains what looks like a C program, give the file a name with the extension .SCR, and put its name into the SMALLCOM phone directory. The directory was described in April, and it has a file maintenance screen that lets you add script names to an entry associated with a phone number. The file name goes in the entry associated with the service that uses the script, and you do not put the .SCR extension in the directory record. When you call a service that has a script associated with it, the script interpreter shell is executed.

The S interpreter provides the management of generic S data types and control flow of the script program. The intrinsic functions in the custom shell integrate the script and the communications program. Be aware of how the intrinsic functions are called. SI recognizes that a program is calling an intrinsic function by finding the name in the global array of INTRINSIC structures named ffs. The structure contains the ASCII name of the function and the address of the script.h function to be executed. The intrinsic function mechanism uses a generic parameter-passing technique where the address of an array of integers is passed to the intrinsic function. SI has built that array from the parameters that the interpreted call to the function is passing. The integers in the array represent integer values or pointers.

If the name of an array of character pointers is passed, the integer is taken to mean a pointer to a pointer. The straight character or integer pointers are really integer offsets to the locations in the SI token buffer. The pointers to pointers are integer offsets from the interpreter's data segment to an array of pointers that is built when the function call is interpreted.

The script.c program is written to be compiled with one of the small data models, so the pointer parameter integer offsets can be cast as pointers and the shell works fine. Each intrinsic function knows what kind and how many parameters it expects. Here's the rub. If you should ever want to use the SI interpreter in a large data model program, you would need to coerce those integer offsets into far pointers by using the segment value assigned to the token buffer. You would do this in the intrinsic functions in your shell. You would also need to change the way that interp.c deals with the address-of and pointer operators since these are managed by brute-force casts of integers to pointers. It just seemed easier at the time.

In the paragraphs that follow I describe the communications script intrinsic functions as if they had ANSI-style prototypes. They do not. In practice, you simply call the functions without declaring them. This convention is closer to the style permitted by the original K&R definition of the C language. Remember, though, that SI functions are implicitly defined as returning whatever they actually return at run time. It is up to your SI program to know what is coming back when it calls a function.

void logon(char*filename);  
This function turns on logging of the data received from the remote computer across the serial line. The parameter specifies a file name. If logging is already on, the current log file is closed, and this new file is created if it does not exist or is opened for appending if it does exist. You would use this function in a script where you wanted to capture the incoming data in a named file, perhaps a file of messages for a particular online forum.

void logoff(void);  
This function turns logging off.

void upload(char*filename, charprotocol);  
Call this function to start uploading a file to the remote computer. The first parameter is the file name and the second is 'a' for an ASCII upload and 'x' for an XModem upload. In a script you might use the ASCII mode of this function when it is time to send a message to a forum. You would assume the user has created a message by using the SMALLCOM editor or another, and that the file name is the name you include in the script.

void download(char*filename, char protocol);  
Call this function to start down-loading a fi e from the remote computer. The first parameter is the file name to be created locally and the second is 'a' for an ASCII upload and 'x' for an XModem upload.

void hangup(void);  
This function breaks the connection between the local and remote computers.

void quit(int code);  
This function tells the program to terminate SMALLCOM. If the code parameter is zero, the termination returns to the SMALLCOM program. If the code is non-zero, the quit function stuffs characters into the BIOS keyboard read-ahead buffer to cause SMALLCOM to return to DOS.

void sendstring(char *string);  
This function sends a string of characters to the remote computer. You can code the string as a literal within the call or use a pointer.

void sendchar(char ch);  
This function sends a single character to the remote computer. The character can be a literal or a char or int variable.

int waitforstrings(char *str[]);  
This function reads the input stream and watches for the appearance of one of the strings pointed to by the array. The function returns an integer relative to zero as the subscript to the string that was seen. If 60 seconds pass without one of the strings coming in, the function returns oxffff(or -1; a good reason to have the unary operator supported by SI, which we do not). You would use this function in a script where your logic might be altered depending on what comes in. The strings "You have mail waiting" and "No mail today" can imply different paths in the script. Here is an example of how that sequence would appear:

```c
char *str[] = {
    "You have mail waiting",
    "No mail today"
};
int answer;
answer = waitforstrings(str);
if (answer = = 1)
    ...
```

int waitfor(char *str);  
This function waits for the appearance of the specified string. It returns 0 if the string is found and -1 if the 60-second timeout occurs. You would use this form when there are no choices, such as when you know the remote service always asks for the password. An example of the use of this function follows.

```c
if (waitfor("PASSWORD:") = = 0xffff)   ....
```

void system(char *command);  
This function is the equivalent of the Turbo C and Microsoft C system functions. The string is a DOS command that is to be executed. You might use this to delete or rename a message file after it has been transmitted. For example:

```c
system("del ddjforum.xmt");
```

void message(char *msg);  
This command writes a message at the bottom of the screen in the status line. It is a way for the script to keep the user advised of its progress. The earlier status line is saved and restored when the script is done. If you use this function, you should use it throughout the script to maintain consistency in your program. message("Receiving Messages");.

## Building the Program

Modify the smallcom.prj or smallcom .mak files from when we originally built SMALLCOM. Add the script.c and interp.c code modules. The example that follows is a script that talks to a ProComm host-mode computer.

## An Example of a Script

Listing Three is procomm.scr, an example script. A casual browser of the magazine will think they are looking at a non-ANSI C program. Instead, this is a script that lets a SMALLCOM program call another computer that is running ProComm, the shareware communications program. ProComm has a host mode that turns it into a miniature bulletin board system. When ProComm is in that mode (activated by Alt-Q) and a call comes in, the caller is asked to enter a name and password. Then ProComm displays a menu of file-related commands that the caller can use. These include upload and download commands. This environment is handy for testing a script, and so our procomm.scr script exercises this feature.

To run the script, you must put its name, procomm, in the directory entry of the remote system that is running ProComm. If you do not have two computers, two modems, and two phone lines, maybe you can arrange for a friend to help you out. Call the ProComm computer and watch it go. The password programmed into the script must match the one programmed for the ProComm host mode. Let's step through this script and see what it does.

The first item of interest is the array of character pointers named strs. This array illustrates how you express the list of strings that the intrinsic waitforstrings function expects. In this case, we will wait for one of the two strings after we send the password. If the password is correct, the script will key on the "choice?" string. The "denied" string tells us that ProComm does not like our password.

Next are some character pointers initialized to point to strings that ProComm sends and that we will wait for unconditionally. These strings could be coded into the calls to waitfor as string literals, but because several of them are used more than once, I coded them individually to save token space. For consistency, I put the strings that get sent by sendstring into external pointers, too. This is a good coding practice. It gets all the data values that might be changed for another script up where they can be found without a search through the script's code. If you call a lot of RBBS or FIDO bulletin boards and want to automate the signon, you'll be building a lot of similar scripts.

SI has no #define preprocessor command, so instead we use initialized int variables with identifiers of upper case. The main function of procomm.scr posts a message about signing on and calls the signon function. If that function returns a true value, the sign on procedure was a success. The signon function waits for the "name" prompt and sends the caller's name. It waits for the "password" prompt and sends the password. Then it waits for either the "choice" or "denied" messages and determines from this to return a true or false value. If main gets the false return it posts a message about being denied access and is done. Otherwise it calls downloadops to download a file from the ProComm system, uploadops to upload a file, posts a sign off message and sends a 'g' to the ProComm system to tell it "goodbye."

The downloadops and uploadops functions post messages, wait for the strings from ProComm that tell them what to do, send the answers, and call the intrinsic download and upload functions to transfer files. The downloadops function calls the intrinsic system function to rename the file it received.

## Software Development '89

I attended the Software Development '89 conference in Burlingame, Calif., south of San Francisco. SD89 was Miller Freeman's second annual conference for software developers. The dominant development platform is still C, but object-oriented programming is rapidly gaining momentum.

The new C product of note is TopSpeed C from Jensen & Partners International (Mountain View, Calif.) TopSpeed C is the "Turbo C that was to be." Among its developers are founders of Borland, and while they were building what was to be the Turbo C compiler, Borland's management jerked the rug from under them by buying and adapting Wizard C into Turbo C. The original team then pulled out and took their compiler code with them to form JPI. From the unfailing perspective of perfect hindsight we see that Borland's change of direction was a good move. Nearly two years after the announcement of Turbo C, JPI's TopSpeed C is not yet ready to ship. But what they are showing is impressive. Watch this one.

Bill Gates, Microsoft's youthful CEO, gave the welcome address. SD89 presented the perfect audience for Gates because he did what most of us dream of --wrote programs and made a billion dollars. Of course, there's more to it than that, but when a billionaire speaks, folks tend to listen. The point of his address was that OS/2 is the wave of the future, but that there will always be MS-DOS. Surprise.

Gates maintains that there is no future in developing text-based applications. According to him, the MS-DOS Windows and OS/2 Presentation Manager graphical interfaces are where the opportunities lie. It's easy to figure out the message. When a billionaire programmer tells programmers (many of them presidents of multi-hundred dollar software companies) what code to write to make a lot more money, the masses pay attention. But let's not forget that Gates has an axe to grind; Microsoft has an investment in the realization of his advice. Before you jump on the bandwagon though, be advised -- the learning and cost ramps for those pretty pictures are mighty steep.

His only real news was that Microsoft's full compiler products (MSC to us) will soon have integrated environments after the fashion of the Quick language products.

Gates pitched incremental compiles/ link, a feature that QuickC 2.0 already has to some extent. This is where the environment compiles only the function just changed. He said that hypertext-like editors would be big stuff some day, too. You will be able to put the cursor on a function name, press a key, and jump to that function in its source file. You will then be able to change the function's name and have the integrated environment fix all references to it in all related code modules and automatically recompile and link. I heard a strange sound. Terry Colligan, president of Rational Systems Inc., was snickering into his fist. Instant-C has had those features for years.

Gates talked about Microsoft's presence in the international arena. He made a point of their relationship with the USSR. It seems Microsoft sells a lot of software over there. (I don't feel so guilty now about writing this column on a Toshiba laptop.) But then he told us about how one of their overseas sales organizations unloaded their last 500 copies of QuickC 1.0 on the Soviets. For the first time since the Rosenbergs were executed I felt sorry for a communist.

Wednesday's bright spot was Liz Oakley, vivacious publisher of Programmer's Journal, sitting on the floor in the corridor outside of the Microsoft hospitality suite. Liz was holding court for a seated circle of enchanted followers (me included). The denizens of the more dignified inner suite closed the door to block out the spectacle.

On Thursday Microsoft bowed out of the Codeview/Turbo Debugger shootout. David Intersimone of Borland graciously consented to objectively represent both views. He gave a brief rundown of the features shared by both debuggers and then treated us to a dazzling demonstration of the Turbo Debugger. Code-who? Lest we forget, CodeView was a trailblazer, setting the standard in source debuggers for others to shoot at. Since then it has been left in the dust, and we tend to forget how much we loved it in its early days. No doubt a next-generation CodeView is in the wings, if only to silence the muffled titters heard whenever the two debuggers are compared. In the meantime Turbo Debugger is a must-have for serious debugging.

Borland hosted a grand party on Thursday night. There was food, drink, T-shirts, and a competent jazz quintet of Borland employees -- guitar, flute, keyboards, bass, and drums. In the second set, Philippe Kahn, the flamboyant leader of Borland, sat in and played the tenor saxophone. Don't give up the day gig, Philippe.

## Listing 1

```c
/* -------- junehook.c ------------ */

/*
 * make these changes to smallcom.c to install the script
 * processor program
 */

/* ------- making logfp external ---------- */
FILE *logfp;
static FILE *uploadfp, *downloadfp, *cfg;


/* ------- the hook to script processors ---------- */
extern void script(void);
void (*script_processor)(void) = script;
```

## Listing 2

```c
/* ---------- script.c -------------- */

/*
 * The SI shell to implement interpreted scripts in SMALLCOM
 */

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <ctype.h>
#include <dos.h>
#include <setjmp.h>
#include "window.h"
#include "serial.h"
#include "interp.h"
#include "modem.h"

#if COMPILER == MSOFT
#define MK_FP(s,o) ((void far *) \
    (((unsigned long)(s) << 16) | (unsigned)(o)))
#endif

void upload_ASCII(FILE *);
void download_ASCII(FILE *);
void upload_xmodem(FILE *);
void download_xmodem(FILE *);
int waitforstring(char **, int, int);
char *prompt_line(char *, int, char *);
void reset_prompt(char *, int);

/* ----------- intrinsic interpreter functions ---------- */
static int si_logon(int *);
static int si_logoff(int *);
static int si_upload(int *);
static int si_download(int *);
static int si_hangup(int *);
static int si_quit(int *);
static int si_sendstring(int *);
static int si_sendchar(int *);
static int si_waitforstrings(int *);
static int si_waitfor(int *);
static int si_system(int *);
static int si_message(int *);

INTRINSIC ffs[] = {
    "logon",            si_logon,
    "logoff",           si_logoff,
    "upload",           si_upload,
    "download",         si_download,
    "hangup",           si_hangup,
    "quit",             si_quit,
    "sendstring",       si_sendstring,
    "sendchar",         si_sendchar,
    "waitforstrings",   si_waitforstrings,
    "waitfor",          si_waitfor,
    "system",           si_system,
    "message",          si_message,
    NULL,               NULL
};

extern INTRINSIC *infs = ffs;
extern FILE *logfp;

/* ------------------ error messages ----------------------- */
char *erm[]={  "Unexpected end of file", "Unrecognized",
               "Duplicate ident",        "Symbol table full",
               "Out of heap memory",     "Undeclared ident",
               "Syntax Error",           "Unmatched {}",
               "Unmatched ()",           "Missing",
               "Not a function",         "Misplaced break",
               "Out of place",           "Too many strings",
               "Token buffer overflow",  "Divide by zero"    };

static FILE *fp;
static char *prompt = NULL;
extern char scriptfile[];
extern char *tokenbf;

jmp_buf errorjmp;

/* ---------- process the named script file --------- */
void script()
{
    if ((fp = fopen(scriptfile, "r")) != NULL) {
        if (setjmp(errorjmp) == 0)  {
            loadsource();
            interpret();
        }
        fclose(fp);
        if (prompt != NULL)
            reset_prompt(prompt, 25);
        if (tokenbf != NULL)
            free(tokenbf);
    }
}

/* ----------- syntax error in script language --------- */
void sierror(enum errs erno, char *s, int line)
{
    char msg[80];
    sprintf(msg, "SI Error: %s %s line %d\n",s,erm[erno],line);
    error_message(msg);
    longjmp(errorjmp, 1);
}

/* ---------- get a character of script source code -------- */
int getsource(void)
{
    return getc(fp);
}

/* -------- unget a character of script source code ------- */
void ungetsource(int c)
{
    ungetc(c, fp);
}

/* ------------ intrinsic functions -------------- */

/* ---------- turn logging on ----------------- */
static int si_logon(int *ptr)
{
    si_logoff(ptr);
    logfp = fopen((char *) *ptr, "ab");
    return 0;
}

/* ---------- turn logging off ----------------- */
static int si_logoff(int *ptr)
{
    if (logfp)
        fclose(logfp);
    logfp = NULL;
    return 0;
}

/* ----------- upload a file ----------------- */
static int si_upload(int *ptr)
{
    FILE *up;
    int x = wherex();
    int y = wherey();

    if ((up = fopen((char *) ptr[0], "rb")) != NULL)    {
        if (toupper(ptr[1]) == 'A')
            upload_ASCII(up);
        else if (toupper(ptr[1]) == 'X')
            upload_xmodem(up);
        fclose(up);
    }
    gotoxy(x,y);
    return 0;
}

/* --------- download a file ------------- */
static int si_download(int *ptr)
{
    FILE *dn;
    int x = wherex();
    int y = wherey();

    if ((dn = fopen((char *) ptr[0], "wb")) != NULL)    {
        if (toupper(ptr[1]) == 'A')
            download_ASCII(dn);
        else if (toupper(ptr[1]) == 'X')
            download_xmodem(dn);
        fclose(dn);
    }
    gotoxy(x,y);
    return 0;
}

/* ---------- hangup the call ------------ */
static int si_hangup(int *ptr)
{
    disconnect();
    return 0;
}

/* ----------- terminate the program ---------- */
static int si_quit(int *ptr)
{
    int far *bp = MK_FP(0x40, 0x1a); /* BIOS read-ahead buff */

    if (*ptr)   {
        *bp++ = 0x1e;   /* next off pointer */
        *bp++ = 0x22;   /* next on pointer  */
        *bp++ = 27;     /* Esc key          */
        *bp   = 'y';    /* 'y' for Yes      */
    }
    longjmp(errorjmp, 1);
}

/* ---------- send a string to the callee --------- */
static int si_sendstring(int *ptr)
{
    char *cp = (char *) *ptr;

    while (*cp)
        writecomm(*cp++);
    return 0;
}

/* ---------- send a character to the callee --------- */
static int si_sendchar(int *ptr)
{
    writecomm(*ptr);
    return 0;
}

/* --- wait for one of a set of strings from the callee --- */
static int si_waitforstrings(int *ptr)
{
    return waitforstring((char **) ptr[0], 60, 0);
}

/* ------- wait for a string from the callee ------- */
static int si_waitfor(int *ptr)
{
    static char *ws[] = {NULL, NULL};

    ws[0] = (char *) *ptr;
    return waitforstring(ws, 60, 0);
}

/* ---------- execute a system (DOS) command ----------- */
static int si_system(int *ptr)
{
    char cmd[80];
    sprintf(cmd, "%s >nul", (char *) *ptr);
    system(cmd);
    return 0;
}

/* --------- display a message to the user ------------ */
static int si_message(int *ptr)
{
    int x = wherex();
    int y = wherey();
    prompt = prompt_line((char *) *ptr, 25, prompt);
    gotoxy(x,y);
    return 0;
}
```

## Listing 3

```c
/*
 * PROCOMM.SCR : A SMALLCOM script that calls a ProComm system,
 *               downloads a file, and uploads another file
 */

/* ----- key strings that ProComm sends ----- */
char *strs[] = {
    "choice?",
    "Denied"
};
char *Name      = "Name:";
char *Password  = "Password:";
char *choice    = "choice?";
char *procedure = "procedure.";
char *spec      = "spec?";

/* -------- strings that SMALLCOM sends -------------- */
char *name      = "Al Stevens\r";
char *password  = "PASSWORD\r";
char *filename1 = "test1.fil";
char *filename2 = "test2.fil";

/* -------- poor man's #define or enum ---------- */
int CHOICE    = 0;
int DENIED    = 1;
int XMODEM    = 'X';
int UPLOAD    = 'u';
int DOWNLOAD  = 'd';
int GOODBYE   = 'g';

/* -------- main entrance to the script --------- */
main()
{
    message("  --> Signing on to ProComm <--");
    if (signon())   {
        downloadops();
        uploadops();
        waitfor(choice);
        message(" Signing Off");
        sendchar(GOODBYE);
    }
    else
        message(" Not allowed access! ");
    hangup();
}

/* ---------- sign on and send the password ----------- */
signon()
{
    waitfor(Name);
    sendstring(name);
    waitfor(Password);
    sendstring(password);
    if (waitforstrings(strs) == CHOICE)
        return 1;
    return 0;
}

/* --------- download a file ---------- */
downloadops()
{
    message("  Downloading");
    sendchar(DOWNLOAD);
    waitfor(choice);
    sendchar(XMODEM);
    waitfor(spec);
    sendstring(filename1);
    sendchar('\r');
    waitfor(procedure);
    download(filename1, XMODEM);
    system("del download.fil");
    system("ren test1.fil download.fil");
}

/* --------- upload a file ---------- */
uploadops()
{
    waitfor(choice);
    message("  Uploading");
    sendchar(UPLOAD);
    waitfor(choice);
    sendchar(XMODEM);
    waitfor(spec);
    sendstring(filename2);
    sendchar('\r');
    waitfor(procedure);
    upload(filename2, XMODEM);
}
```
