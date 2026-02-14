
# C programiranje 09

## A Phone Directory and XMODEM for SMALLCOM

Al Stevens, april '89

This month adds two features to the SMALLCOM communications program introduced in this column last month. The features are a telephone directory and the XModem file transfer protocols. Most communications programs include support for both capabilities. As this project proceeds from month to month, I add features to the program by initializing hooking function pointers. Whenever adding features this way, I will provide the lines of code that you must change in order to turn on the hooks.

Those of you who download the code from the CompuServe online service should be alerted to my methods. Each source code file that you download is written just as it appears in the issue where the code is originally published. I use the column to document minor program changes required for subsequent months and do not change the uploaded code. Why do it this way? There are two reasons: First, the management of those files is a significant administrative burden borne by the already overworked and loyal staff at DDJ. Second, I planned this project so that anyone could bail out at any time and still have usable software. Each month presents a finished tool or a complete program. The tools and programs are built upon those of the previous months.

You might not need all the enhancements of the programs as they progress, and I might get run over by a pie wagon. The code you download should therefore not be dependent on the requirements of later months. It follows then that the programs and the columns that describe them are a matched set. You need both.

The Hooks

Listing One, page 132, hooks.c, is a replacement for a small block of code near the beginning of smallcom.c. Its location and what it replaces should be obvious from the comments. I do not give line number references because you might have typed the code in, and your numbers and mine would probably not agree. Hooks.c provides the function prototypes for the new features and initializes the hooking function pointers. You will see hooks to Kermit protocols in the upload and download function pointers. These are stubbed functions. Kermit is not in this issue.

The Phone Directory

Listing Two, page 132, is phonedir.c. This source file adds the phone directory to SMALLCOM. A phone directory is just what you might expect: a directory of numbers that you call frequently. Because different online services and electronic bulletin boards use different connection protocols (baud rate, stop bits, and so on), the phone directory allows you to specify those parameters for each entry.

The phone directory is called by the Directory selection on the SMALLCOM menu. By initializing the phone_directory function pointer in smallcom.c to the phdirectory function in phone-dir.c, we enable the feature. The phdirectory function opens a full-screen window, reads the directory from the file named phone.dir, and fills the window with text from the directory records by calling the text_window function. The phone.dir file does not exist until you build a directory with at least one entry. Each record in the file is a null-terminated string with fixed position fields. The fields are separated by spaces. This way the strings are given to the text_window function in the file format. The get_directory function allocates a block of memory for each string and builds the array of string pointers expected by the text_window function.

Once the phone directory is displayed, the program calls the select_window function to allow a menu-like cursor to select an entry from the directory. You move the cursor up and down with the arrow keys and use other keys to select what you want to do with the entry pointed to by the cursor.

select_window returns with the number of the entry selected or zero if you press the Esc key. The call to select_window includes the address of the dirproc function. select_window will, therefore, call the dirproc function when you press a key other than the Help key (F1), the Enter key, an arrow key, or the Esc key. The dirproc function processes the Del key, which deletes an entry; the Ins key, which inserts a new entry; the F2 key, which writes the modified directory to the phone.dir file; and the F3 key, which modifies the directory entry where the cursor points.

Inserting or modifying a directory entry uses the data entry window template functions from the window library. A window is popped up over the directory, and you modify its contents by using the data entry features.

When you press the Enter key, the selected directory record updates the current phone number and line protocols, and the program returns to the SMALLCOM screen where you will see the newly selected phone number at the bottom of the screen. If you pop down the Parameters menu, you will see that the parameters associated with the selection are now selected. You can make these values the startup default by choosing the Write Parameters selection from the Parameters menu.

The SMALLCOM phone directory feature is an example of a simple application of the data entry library where the values in a flat file database are maintained with a data entry template. Notice that there are help window mnemonics in the directory_template FIELD array. As with last month's code, I am not republishing the smallcom.hlp text file to add help text for the phone directory (or for the file transfer protocol data entry template explained later). You may compose help windows that please you for these templates. The directory and its data fields should be obvious to you without help windows, but you might want to add help text if you give the executable SMALLCOM program to less sophisticated users than yourself.

Each directory record includes a name for a script file. Scripts are sequences of interpreted commands that a communications program uses to automate routine dialogues with online services. Each script would be specific to the service it supports because the services use different command languages. The directory process posts the script's name into a global string variable named scriptfile. Later, when we add the script processor, we will use that variable to retrieve the script file.

File Transfer Protocols

Here's the problem. When you send data across phone lines, errors called "line hits" sometimes occur. A line hit might add a bogus character, skip a good character, or change the binary configuration of a character. Line hits can occur as a natural byproduct of noisy, voice-grade circuits. These errors cannot always be detected by the serial hardware in the computers. So, if your data cannot endure errors, you must put error-detecting and error-correcting logic into the file transfer software. The handshaking and data formats that manage error detection and correction in serial communications are called "file transfer protocols."

What kind of data cannot endure errors? Three examples are executable computer programs, compressed files, and some encrypted files. These are binary files that must be received intact in order for them to be properly used at their destinations. Binary files must be transmitted with a protocol that either supports an 8-bit word length as the XModem protocol does or translates byte values greater than 127 into a pair of 7-bit bytes as the Kermit protocol can do. ASCII text files that contain the source code for sizable programs should always be transmitted with an error-correcting protocol. A garbled character in a program's source code can be difficult to spot, can go undetected by the compiler, and can wreak havoc when the program is run.

What kind of data can endure some margin of error? Text messages can usually be sent without error correction because their readers are humans who can perform their own visual error correction. You can read the garbled message "Climatv is what wi expect, weath$r is whit we gt" and know what was sent. That example is extreme. Most transmissions get off without a hitch. The ASCII protocol is usually acceptable for text that is meant for people to read. When the hits get too severe for you to read the message, hang up the phone and try another time. Don't bother switching to an error-correcting protocol. If the line hits are so bad that you can't read the message, no error-correcting protocol would work either.

Most communications programs allow you to select from a set of file-transfer protocols so that they are compatible with the various online services throughout the world. We will add that capability to SMALLCOM with a menu that selects from the ASCII, XModem, and Kermit protocols.

Listing Three, page 133, is protocol.c. It lets you select a specific file-transfer protocol when you are about to upload or download a file. It opens a small window and writes the selections for three protocols: ASCII, XModem, and Kermit. ASCII is the only protocol that was included in the SMALLCOM program last month. XModem is added this month. I might address Kermit in a later month.

If you want to add still another protocol, you would add an entry to the window menu's display, code corresponding function addresses to the up_protocol and down_protocol function pointer arrays in hooks.c, and write the functions to implement the new protocol. The protocol name should start with a unique letter. I've built the protocol selection menu to respond to the usual menu cursor selection and also to select a protocol from the menu when you press the first letter of the protocol's name. If you add a protocol, you must insert a keystroke test for the correct first letter into the prot_key function toward the beginning of protocol.c.

Suppose, for example, that you wanted to add the CompuServe B protocol to SMALLCOM. Users of CompuServe might want to do that, and the way to do it is paved by an article in the June 1986 issue of DDJ. The article, "The CompuServe B Protocol: A Better Way to Send Files" by Levi Thomas and Nick Turner, briefly describes the protocol. Their description is supported by a C program written by Steve Wilhite. I have not used this code, but it implements the CompuServe B protocol in a way that looks as if it would be readily adaptable to the SMALLCOM architecture. The authors of the article say the code is in the public domain, but Wilhite put a copyright notice in the source file. Rather than mess with that contradiction and attempt to incorporate the code into SMALLCOM, I will retreat in temerity to the usual reference book position and leave the more difficult task as an exercise for the reader.

XModem

Almost anyone who uses modems and online services knows about XModem. It is a file-transfer protocol designed in the early eighties by Ward Christensen, who placed it --and CP/M programs that implemented it --into the public domain. It is a simple and effective protocol that provides a measure of error correction during the transmission of critical files between computers. For comprehensive discussions of the XModem protocol, see the books on serial communications I cited in the last two months or get a copy of the June 1985 DDJ and read "Christensen Protocols in C" by Donald Krantz. (The editorial and article content of all past issues are available in bound volumes, by year, from M&T Publishing.)

Writers who tackle XModem are quick to criticize it and then hasten to congratulate Christensen who generously gave it to the world. Discussions of its deficiencies are a favorite pastime with some writers who do not adequately acknowledge XModem's original purpose, which was to get around the line hit problems people were having trying to push binary data around between CP/M microcomputers. There was nothing about XModem that pushed the technology when it was published. It is a simple protocol similar to ones that were being used in asynchronous and bisynchronous communications applications in the mini-computers of the middle seventies. It was destined to become a standard, not because it revolutionized anything or was the absolutely best way to solve the problem but because it was there --working, reliable, and available to anyone who needed and wanted it. If Christensen had not put it into the public domain, we might never have heard of it. Because he did, it became a roaring success and remains in use everywhere.

In a nutshell (a big nutshell), here is how the XModem protocol works. The sending XModem program transmits a file to a receiving XModem program. To manage error detection, the sending program adds something to the data that relates to the data records themselves. The original XModem computes and transmits a simple 8-bit checksum of the data characters. The receiving program calculates the checksum too, and if the calculated checksum is not the same as the one transmitted by the sender, an error exists in either the data characters or the checksum itself. A later improvement to XModem substitutes a Cyclic (or Cyclical --the authors disagree. One author, Campbell, disagrees with himself) Redundancy Check (CRC) value for the checksum, thus tightening up the error detection.

To manage error correction, the receiving program tells the sending program whether or not the data were received correctly. The sending program then either sends more data or resends the data record that was in error. This handshake would not be efficient if it were used on the entire file at one clip. So, the XModem protocol breaks the file into fixed length, 128-byte blocks and sends them one at a time.

The sending program cannot begin sending until it knows that the receiver is ready to receive. The sender waits for the receiver to send a special character that gives the go-ahead. The sender and receiver might be two different implementations of XModem. Older versions did not support the CRC, so newer ones must be able to support either the CRC or the checksum. If the sender is an older version, it simply waits for a NAK (an international transmission control protocol) (0x15) from the receiver. If the sender is a newer version it waits for either the NAK or a 'C.' If it gets a NAK, it uses the checksum. If it gets the 'C,' it uses the CRC. If the receiver is an older version, it sends the NAK when it is ready to receive. If it is a newer version, it does not know yet whether the sender can support the CRC or not. So it sends the 'C' hoping for the best. If it does not receive the SOH (0x01) character, which identifies the start of a block, it tries a few more times and then sends the NAK.

When the sender knows that the receiver is ready, the sender sends a block. Each block contains the SOH, the 1-byte block number (a serial number relative to one), the ones-complement of the block number, 128 bytes of data, and either the 1-byte checksum or the 2-byte CRC. Then it waits for a response from the receiver. After the last block, the sender sends the single EOT (0x04) character to the receiver.

The receiver reads all this stuff and looks at it. The first byte must be the SOH or EOT character. If the EOT character is received, the receiver sends the ACK (0x06) character, closes the file it is building, and assumes the transmission is complete. Otherwise the receiver looks at the rest of the packet. The block number must be either the previous block number plus one or, in the case of a retransmission, the previous block number. The next byte must be the ones-complement of the block number. The 128-data bytes can be anything at all. If the checksum mode is being used, the checksum must be the 8-bit sum of the bytes in the 128-data field. If the CRC mode is being used, the two CRC bytes must be the CRC that is computed from the 128-data bytes plus two more bytes of a zero value. If all this is true, the receiver writes the data block to the file it is building and sends the ACK character to the sender. If any of this is not true, or if the receiver times out while waiting for any of the above from the sender, the receiver sends the NAK (0x15) character to the sender.

If the sender sees the ACK character, it proceeds with the next block. Otherwise, it resends the block that was just NAK'd.

The receiver works with timeouts. While it is waiting for the SOH, it sets a ten second timeout. While waiting for each of the other bytes of the packet, it sets a one second timeout. Timeouts and error detections are all handled with NAKs. After so many consecutive NAKs, both the sender and receiver give up and declare the transmission a bust. The one-second timeout can be too short for use with services that go through a network. The network's packet overhead on a busy day can exceed one second, causing the receiver to time out. If this gets to be a problem, increase the timeout to some larger value, perhaps five seconds.

Listing Four, page 134, is xmodem.c, the functions that implement the XModem upload and download file-transfer protocols. Notice the handling of the external xonxoff_enabled variable. While XModem is sending and receiving data, the XON and XOFF protocols must be disabled. The data being transmitted can consist of any bytes of any value, any of which could look like the XOFF code. If XON/XOFF protocols were in force and a block number or checksum looked like the XOFF, the serial transmission function would think it was being told by the other side to quit transmitting. It would then wait, perhaps forever, for the XON signal.

Notice also the test_wordlen function. For XModem to operate properly, the serial port must be programmed for a word length of eight data bits. This function prevents the upload or download from starting if eight data bits are not programmed. Christensen specifies that the protocol can use seven bits by clearing the most significant bit of the block numbers and checksum at both ends of the line, but this would work only with ASCII files. Other files need the full eight bits to be transmitted.

I am not going to attempt to explain why the CRC algorithm works because I do not know, but here is the procedure it uses. The algorithm builds a 16-bit CRC from a string of 8-bit bytes. The CRC starts with a zero value. The following steps occur for each byte in the string. The byte is concatenated to the right of the CRC to form a 24-bit value. This value is shifted left one bit eight times. For each one-bit shift, if the most significant bit of the 24-bit value before the shift is a one, the value 0x1021 is XORed with the leftmost 16 bits (the CRC part) of the new 24-bit value. When all bytes have been processed this way, the leftmost 16 bits are the CRC. This algorithm works with XModem and should work with Kermit as well.

The CRC is built by the compute_crc function. You pass the function --a string pointer --and the number of bytes in the string. A better and more mathematical explanation of the CRC can be found in most of the references already cited. Perhaps you will understand it, although that is not necessary. The algorithm works and we know how to build it. I looked at how others coded the algorithm and wrote the one you see here. Then I tested it by transmitting files back and forth between SMALLCOM and another computer that was running ProComm.

The XModem protocol was originally described in a paper published by Christensen in the fall of 1982. In January of 1985 John Byrns published a description of the logic that replaced the checksum with a CRC. That paper included a C language CRC algorithm that does not work with today's compilers. From the looks of it the code would not work with many older compilers either. Both papers can be found on BBSs and online services around the country. Other descriptions are found in the books and articles I've already mentioned.

To compile and build the phone directory and XModem into SMALLCOM, add phonedir, prototype, and XModem entries to the smallcom.prj file for Turbo C, and smallcom.mak and smallcom.lnk for Microsoft C. These files were in last month's column.

Not Just Another Turbo C Book

Most of the C books in the bookstore have "Turbo C" in their titles. Many of these books are about the C language itself with little information specific to Turbo C. (Modesty prevents me from pointing out the one outstanding exception.) Now my "boss" at DDJ, Kent Porter, has written a book that has Turbo C in its title, and is about C, Turbo C, the PC, and good programming in general. There are chapters on how DOS disks and files work, BIOS, the TC video library, popup windows, menus, mice, graphics, expanded memory management, data structures, interrupt functions, and plenty more. There is lots of C code to explain the lessons, and it all works by using the unique features of Turbo C.

I saw the manuscript before publication. I really like the book, and I am not saying that just because Kent is my editor. As I write this column, the book is not yet available but by the time you read the column, the book should be available. Its title is Stretching Turbo C 2.0 and it is published by Brady Books.

## LISTING 1

```c
/* ------- the hook to the phone directory ---------- */
extern void phdirectory(void);
static void (*phone_directory)(void) = phdirectory;
/* ------- the hook to script processors ---------- */
void (*script_processor)(void);
/* ------- hooks to file transfer protocols --------- */
extern int select_protocol(void);
static int (*select_transfer_protocol)(void) = select_protocol;
/* ----- up to five upload function pointers ----- */
void upload_xmodem(FILE *);
void upload_kermit(FILE *);
static void (*up_protocol[5])(FILE *file_pointer) = {
    upload_ASCII, upload_xmodem, upload_kermit,NULL,NULL
};
/* ----- up to five download function pointers ----- */
void download_xmodem(FILE *);
void download_kermit(FILE *);
static void (*down_protocol[5])(FILE *file_pointer) = {
    download_ASCII, download_xmodem, download_kermit,NULL,NULL
};
```

## LISTING 2

```c
/* --------- phonedir.c ---------- */
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <mem.h>
#include <ctype.h>
#include "window.h"
#include "entry.h"
#include "help.h"
#include "modem.h"

#define DIRECTORY "phone.dir"
#define MAX_ENTRIES 50
#define WRITEDIR F2
#define MODIFYDIR F3

void phdirectory(void);
char scriptfile[13];
static void get_directory(void);
static void put_directory(void);
static int dirproc(int, int);
static void bld_default(int);
static void build_dir(int);
static int enter_directory(int);
static void select_directory(int);
char *prompt_line(char *, int, char *);
void reset_prompt(char *, int);
static int edit_directory(void);
static int direrror(int);
static void field_terminate(FIELD *fld, int termchar);

extern int PARITY,STOPBITS,WORDLEN,BAUD;
extern char PHONENO[];
extern char spaces[];
extern struct wn wkw;   /* the directory window structure */
extern void (*phone_directory)() = phdirectory;
/* -------- phone directory file record ------------ */
struct {
    char ol_name[21];    /* callee's name             */
    char ol_phone[24];   /* phone number              */
    char ol_parity[8];   /* none/odd/even             */
    char ol_stopbits[4]; /* 1 or 2                    */
    char ol_wordlen[3];  /* 7 or 8                    */
    char ol_baud[6];     /* 110,150,300,600,1200,2400 */
    char ol_script[9];   /* name of script file       */
} pd;
static char hdr[] =
    " Name                 "
    "Phone Number           "
    "Parity Stop Len Baud  Script";
static char select_prompt[] =
    "\030\031\021\304\331:Select Esc:Return "
    "F2:Write Directory F3:Modify "
    "Ins:Insert Del:Delete";
static char enter_prompt[] =
    " F2:Write Changes to Directory    "
    " Esc:Ignore Entry   F1:Help";
static char *pds[MAX_ENTRIES+1];
static int pct;
static FILE *fp;
static char nmmask[] = "____________________";
static char phmask[] = "____________________";
static char prmask[] = "____";
static char sbmask[] = "_";
static char wlmask[] = "_";
static char bdmask[] = "____";
static char scmask[] = "________";
/* ------- data entry template for the directory ------- */
FIELD directory_template[] = {
    {3, 16, 1, pd.ol_name,     nmmask, "name"},
    {4, 16, 1, pd.ol_phone,    phmask, "phone"},
    {5, 16, 1, pd.ol_parity,   prmask, "parity"},
    {6, 16, 1, pd.ol_stopbits, sbmask, "stopbits"},
    {7, 16, 1, pd.ol_wordlen,  wlmask, "wordlen"},
    {8, 16, 1, pd.ol_baud,     bdmask, "baud"},
    {9, 16, 1, pd.ol_script,   scmask, "script"},
    {0}
};
/* -------- data entry error messages --------- */
static char *ermsgs[] = {
    "Parity must be None, Odd, or Even",
    "Stop Bits must 1 or 2",
    "Word Length must be 7 or 8",
    "Baud Rate must be 110,150,300,600,1200,2400"
};
/* ------ manage the telephone directory ------ */
void phdirectory(void)
{
    int s = 1;
    char *ttl, *sel;
    set_help("directry");
    ttl = prompt_line(hdr, 1, NULL);
    sel = prompt_line(select_prompt, 25, NULL);
    establish_window(1,2,80,24,TEXTFG,TEXTBG,TRUE);
    get_directory();
    text_window(pds, 1);
    while (pct &&
          (s=select_window(s,SELECTFG,SELECTBG,dirproc))!=0)
        if (pct && pds[s-1] != spaces+1)    {
            select_directory(s-1);
            break;
        }
    delete_window();
    reset_prompt(sel, 25);
    reset_prompt(ttl, 1);
}
/* -------- select the directory entry for the dialer ------- */
static void select_directory(int n)
{
    char *cp = scriptfile;
    movmem(pds[n], &pd, sizeof pd);
    strncpy(PHONENO, pd.ol_phone, 20);
    BAUD     = atoi(pd.ol_baud);
    STOPBITS = *pd.ol_stopbits - '0';
    WORDLEN  = *pd.ol_wordlen  - '0';
    PARITY   = (*pd.ol_wordlen == 'N' ? 0 :
                *pd.ol_wordlen == 'O' ? 1 : 2);
    establish_window(30,11,50,13,HELPFG,HELPBG,TRUE);
    gotoxy(2,2);
    cputs("Initializing Modem");
    initmodem();
    delete_window();
    setmem(scriptfile, sizeof scriptfile, '\0');
    strncpy(scriptfile, pd.ol_script, 8);
    while (*cp && *cp != ' ')
        cp++;
    strcpy(cp, ".scr");
}
/* ------ read the phone directory ----------- */
static void get_directory(void)
{
    if (pct == 0 && (fp = fopen(DIRECTORY, "r")) != NULL)   {
        while (fread(&pd, sizeof pd, 1, fp) != 0)   {
            build_dir(pct++);
            if (pct == MAX_ENTRIES)
                break;
        }
        pds[pct++] = spaces+1;
        pds[pct] = NULL;
        fclose(fp);
    }
    if (pct == 0)
        dirproc(INS, 1);
}
/* ------- build a default phone directory entry -------- */
static void bld_default(int n)
{
    static char *prs[] = {"None", "Odd ", "Even"};
    setmem(&pd, sizeof pd-1, ' ');
    strncpy(pd.ol_parity, prs[PARITY], 4);
    *pd.ol_stopbits = STOPBITS + '0';
    *pd.ol_wordlen  = WORDLEN + '0';
    sprintf(pd.ol_baud, "%4d", BAUD);
    pd.ol_baud[4] = ' ';
    build_dir(n);
}
/* --------- build a directory entry for display ----------- */
static void build_dir(int n)
{
    if ((pds[n] = malloc(sizeof pd)) != NULL)
        movmem(&pd, pds[n], sizeof pd);
}
/* ------- write the phone directory ---------- */
static void put_directory(void)
{
    int i;
    fp = fopen(DIRECTORY, "w");
    for (i = 0; i < pct; i++)
        if (pds[i] != spaces+1)
            fwrite(pds[i], sizeof pd, 1, fp);
    fclose(fp);
}
/* ---------- process a directory entry ------------- */
static int dirproc(int c, int lineno)
{
    int i, j;
    switch (c)  {
        case DEL:
            if (pds[lineno-1] != spaces+1)  {
                free(pds[lineno-1]);
                for (j = lineno-1; j < pct; j++)
                    pds[j] = pds[j+1];
                if (--pct)  {
                    text_window(pds, wkw.wtop);
                    for (i = pct+2; i <= wkw.wtop+wkw.ht; i++)
                        writeline(2, i, spaces+1);
                    if (lineno-1 == pct)
                        --lineno;
                }
                else
                    clear_window();
            }
            break;
        case INS:
            if (pct == MAX_ENTRIES)
                break;
            i = pct;
            if (i)
                while (i >= lineno) {
                    pds[i] = pds[i-1];
                    --i;
                }
            bld_default(i);
            pct++;
        case MODIFYDIR:
            if (pds[lineno-1] != spaces+1)  {
                movmem(pds[lineno-1], &pd, sizeof pd);
                enter_directory(lineno-1);
            }
            break;
        case WRITEDIR:
            put_directory();
            break;
    }
    wkw.wy = lineno - wkw.wtop + 1;
    return (pct == 0);
}
/* ------- data entry for a directory record ---------- */
static int enter_directory(int lineno)
{
    int s = 1;
    char *p = prompt_line(enter_prompt, 25, NULL);
    establish_window(20,5,56,15,ENTRYFG,ENTRYBG,TRUE);
    window_title(" Telephone Directory Entry ");
    gotoxy(3,3), cputs("Name:");
    gotoxy(3,4), cputs("Phone:");
    gotoxy(3,5), cputs("Parity:");
    gotoxy(3,6), cputs("Stop Bits:");
    gotoxy(3,7), cputs("Word Length:");
    gotoxy(3,8), cputs("Baud Rate:");
    gotoxy(3,9), cputs("Script:");
    field_terminate(directory_template, '\0');
    while (s != WRITEDIR && s != ESC)   {
        s = data_entry(directory_template, FALSE, s);
        if (s == WRITEDIR)
            s = edit_directory();
    }
    field_terminate(directory_template, ' ');
    *(((char *)(&pd)) + sizeof pd - 1) = '\0';
    delete_window();
    reset_prompt(p, 25);
    if (s == WRITEDIR)  {
        movmem(&pd, pds[lineno], sizeof pd);
        put_directory();
    }
    text_window(pds,wkw.wtop ? wkw.wtop : 1);
    return (s != ESC);
}
/* -------- validate the directory entry -------- */
static int edit_directory(void)
{
    int i;
    static int bds[] = {110,150,300,600,1200,2400};
    *pd.ol_parity = toupper(*pd.ol_parity);
    if (*pd.ol_parity != 'N' &&
            *pd.ol_parity != 'O' &&
            *pd.ol_parity != 'E')
        return direrror(3);
    if (*pd.ol_stopbits != '1' && *pd.ol_stopbits != '2')
        return direrror(4);
    if (*pd.ol_wordlen != '7' && *pd.ol_wordlen != '8')
        return direrror(5);
    for (i = 0; i < 6; i++)
        if (atoi(pd.ol_baud) == bds[i])
            break;
    if (i == 6)
        return direrror(6);
    return WRITEDIR;
}
/* ------- post a directory entry error ---------- */
static int direrror(int n)
{
    error_message(ermsgs[n-3]);
    return n;
}
/* -------- set field terminators to null or space ------- */
static void field_terminate(FIELD *fld, int termchar)
{
    for (;fld->frow;fld++)
        *(fld->fbuff+strlen(fld->fmask)) = termchar;
}
```

## LISTING 3

```c
/* ------------- protocol.c --------------- */

#include <stdio.h>
#include <conio.h>
#include <ctype.h>
#include "window.h"
#include "help.h"
#include "menu.h"

static char *prots[] = {
    " ASCII",
    " Xmodem",
    " Kermit",
    NULL
};

/* ----- translate A,X,K keystrokes for protocol menu ----- */
static int protkey(int ky, int lnno)
{
    ky = tolower(ky);
    return ky=='a' ? 1 : ky=='x' ? 2 : ky=='k' ? 3 : ERROR;
}

/* --- file transfer protocol for uploads and downloads --- */
int select_protocol(void)
{
    extern MENU *mn;
    MENU *holdmn;
    static int rtn = 0;
    holdmn = mn;
    mn = NULL;
    set_help("protocol");
    establish_window(25,7,55,11,MENUFG,MENUBG,TRUE);
    window_title(" Select Transfer Protocol ");
    text_window(prots, 1);
    rtn = select_window(rtn?rtn:1,SELECTFG,SELECTBG,protkey);
    delete_window();
    mn = holdmn;
    return rtn ? rtn-1 : 0;
}

/* ---- These are stubs, to be replaced later ---- */
void upload_kermit(FILE *fd)
{
    error_message("Upload KERMIT not implemented");
}

void download_kermit(FILE *fd)
{
    error_message("Download KERMIT not implemented");
}
```

## LISTING 4

```c
/* -------------- xmodem.c --------------- */
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <mem.h>
#include "window.h"
#include "serial.h"

#define RETRIES  12
#define CRCTRIES 2
#define PADCHAR  0x1a
#define SOH      1
#define EOT      4
#define ACK      6
#define NAK      0x15
#define CAN      0x18
#define CRC      'C'
/* -------- external data ---------- */
extern int TIMEOUT;
extern int WORDLEN;
extern int xonxoff_enabled;
/* --------- local data ------------ */
static int tries;   /* retry counter */
static char bf [130]; /* i/o buffer  */
/* -------- prototypes ------------- */
extern int keyhit(void);
static void receive_error(int, int);
static void xmodem_msg(char *);
static void test_wordlen(void);
unsigned compute_crc(char *, int);
/* --------- error messages ----------- */
static char *errs[] = {
    "Timed Out         ",
    "Invalid SOH       ",
    "Invalid block #   ",
    "Invalid chksum/crc"
};
/* ---------- upload with xmodem protocol ------------- */
void upload_xmodem(FILE *fd)
{
    int i, chksum, eof = FALSE, ans = 0, ln, crcout = 0;
    unsigned crc;
    char bno = 1;
    xonxoff_enabled = FALSE;
    establish_window(20,10,52,14,MENUFG,MENUBG,TRUE);
    window_title("XMODEM Upload (CHKSUM)");
    tries = 0;
    test_wordlen();
    /* ----- wait for the go-ahead from the receiver ------ */
    TIMEOUT = 6;
    while (tries++ < RETRIES && crcout != NAK && crcout != CRC)
        crcout = readcomm();
    if (crcout == CRC)
        window_title(" XMODEM Upload (CRC)  ");
    TIMEOUT = 10;
    /* -------- send the file to the receiver ----------- */
    while (tries < RETRIES &&
            !eof && ans != CAN && !timed_out())     {
        /* ---- read the next data block ----- */
        setmem(bf, 128, PADCHAR);
        if ((ln = fread(bf, 1, 128, fd)) < 128)
            eof = TRUE;
        if (ln == 0)
            break;
        gotoxy(2, 2);
        cprintf("Block %d  ",bno);
        chksum = 0;
        if (keyhit())
            if (getch() == ESC) {
                writecomm(CAN);
                break;
            }
        writecomm(SOH);      /* SOH           */
        writecomm(bno);      /* block number  */
        writecomm(~bno);     /* 1s complement */
        /* ------- send the data block ------ */
        for (i = 0; i < 128; i++)   {
            writecomm(bf[i]);
            chksum += bf[i];        /* checksum calculation */
        }
        /* -- send error-correcting value (chksum or crc) -- */
        if (crcout == NAK)
            writecomm(chksum & 255);
        else    {
            crc = compute_crc(bf, 130);
            writecomm((crc >> 8) & 255);
            writecomm(crc & 255);
        }
        /* ----- read ACK, NAK, or CAN from receiver ----- */
        ans = readcomm();
        if (ans == ACK) {
            bno++;
            tries = 0;
            gotoxy(2, 4);
            cprintf("        ");
        }
        if (ans == NAK) {
            eof = FALSE;
            gotoxy(2, 4);
            cprintf("%2d tries", ++tries);
            /* ---- position to previous block ----- */
            if (fseek(fd, -128L, 1) == -1)
                fseek(fd, 0L, 0);
        }
    }
    if (eof)    {
        writecomm(EOT);                 /* send the EOT    */
        readcomm();                     /* wait for an ACK */
        xmodem_msg("Transfer Completed");
    }
    else
        xmodem_msg("Transfer Aborted");
    xonxoff_enabled = TRUE;
}
/* ---------- download with xmodem protocol ------------- */
void download_xmodem(FILE *fd)
{
    int blk=0, soh= 0, bn, nbn, i, crcin = TRUE, fst = TRUE;
    unsigned chksum, cs, cs1;
    xonxoff_enabled = FALSE;
    establish_window(20,10,52,14,MENUFG,MENUBG,TRUE);
    window_title("XMODEM Download (CHKSUM)");
    /* - send Cs then NAKs until the sender starts sending - */
    tries = 0;
    test_wordlen();
    TIMEOUT = 6;
    while (soh != SOH && tries < RETRIES)   {
        crcin = (tries++ < CRCTRIES);
        writecomm(crcin ? CRC : NAK);
        soh = readcomm();
        if (!timed_out() && soh != SOH)
            sleep(6);
    }
    if (crcin)
        window_title(" XMODEM Download (CRC)  ");
    while (tries < RETRIES) {
        if (timed_out())
            receive_error(0, NAK);
        /* -- Receive the data and build the file -- */
        gotoxy(2,2);
        cprintf("Block %d   ", blk + 1);
        if (!fst)   {
            TIMEOUT = 10;
            soh = readcomm();
            if (timed_out())
                continue;
            if (soh == CAN)
                break;
            if (soh == EOT) {
                writecomm(ACK);
                break;
            }
        }
        fst = FALSE;
        TIMEOUT = 1;
        bn  = readcomm();       /* block number */
        nbn = readcomm();       /* 1's complement */
        chksum = 0;
        /* ---- data block ----- */
        for (i = 0; i < 128; i++)   {
            *(bf + i) = readcomm();
            if (timed_out())
                break;
            chksum = (chksum + (*(bf + i)) & 255) & 255;
        }
        if (timed_out())
            continue;
        /* ---- checksum or crc from sender ---- */
        cs = readcomm() & 255;
        if (crcin)  {
            cs1 = readcomm() & 255;
            cs = (cs << 8) + cs1;
        }
        if (timed_out())
            continue;
        if (soh != SOH) {       /* check the SOH */
            receive_error(1, NAK);
            continue;
        }
        /* --- same as previous block number? --- */
        if (bn == blk)
            fseek(fd, -128L, 1);
        /* --- no, next sequential block number? --- */
        else if (bn != blk + 1) {
            receive_error(2, CAN);
            break;
        }
        blk = bn;
        /* --- test the block # 1s complement --- */
        if ((nbn & 255) != (~blk & 255))    {
            receive_error(2, NAK);
            continue;
        }
        if (crcin)
            chksum = compute_crc(bf, 130);
        /* --- test chksum or crc vs one sent --- */
        if (cs != chksum)   {
            receive_error(6, NAK);
            continue;
        }
        soh = bn = nbn = cs = 0;
        tries = 0;
        /* --- write the block to disk --- */
        fwrite(bf, 128, 1, fd);
        if (keyhit())
            if (getch() == ESC) {
                writecomm(CAN);
                break;
            }
        writecomm(ACK);
    }
    if (soh == EOT)
        xmodem_msg("Transfer Complete");
    else
        xmodem_msg("Transfer Aborted");
    TIMEOUT = 10;
    xonxoff_enabled = TRUE;
}
/* ------------- send a nak ------------ */
static void receive_error(erno, rtn)
{
    ++tries;
    if (TIMEOUT == 1)   {
        gotoxy(2,4);
        cprintf("%s (%d tries)", errs[erno], tries);
    }
    writecomm(rtn);
}
/* ------ test for valid word length -------- */
static void test_wordlen(void)
{
    if (WORDLEN != 8)   {
        gotoxy(2,4);
        cprintf("Must be 8 Data Bits");
        tries = RETRIES;
    }
}
/* --------- final message about xmodem transfer -------- */
static void xmodem_msg(char *s)
{
    gotoxy(2,3);
    cprintf(s);
    putch(BELL);
    sleep(3);
    delete_window();
}
/* --------- compute the crc ------------ */
unsigned compute_crc(char *bf, int len)
{
    int i;
    long crc = 0;
    while (len--)   {
        crc |= (*bf++) & 255;
        for (i = 0; i < 8; i++) {
            crc <<= 1;
            if (crc & 0x1000000L)
                crc ^= 0x102100L;
        }
    }
    return (unsigned) (crc >> 8);
}
```
