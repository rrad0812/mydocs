
# C programiranje 02

## Jedenje slona: projekat počinje

Al Stevens

Ovog meseca inauguriramo projekat kolone "C programiranje", funkciju u kojoj ćemo razviti komunikacioni program za koji bi trebalo da bude potrebno nekoliko meseci da se završi. Da bismo započeli projekat, moramo uzeti u obzir zahteve programa. Po mom mišljenju, dve procedure su neophodne tokom razvoja računarskog sistema: prva je analiza zahteva; drugi je koncept inkrementalne implementacije.

Analiza zahteva rezultira izjavom šta program mora da radi i kako mora da radi. Izražava se na jeziku koji razumeju i programeri i korisnici. Bez toga nikad ne znate kada ste gotovi. Takva analiza se bavi dve oblasti: funkcionalnim zahtevima i zahtevima performansi. Odložićemo korak funkcionalnih zahteva za kasnije i za sada ćemo se koncentrisati na zahteve performansi.

Inkrementalna implementacija znači da gradite sistem malo po malo i da ga isporučujete korisniku u malim koracima. Naredna povećanja će imati koristi od iskustava koja su im prethodila, ne samo od vašeg, već i od iskustva korisnika. Većina vladinih projekata ignoriše ovu mudrost, preferirajući da automatizujete celu zapadnu hemisferu, da to uradite u kratkom roku, a zatim da ga uključite odjednom. Postoje izuzeci. Džim Touls, menadžer građevinarstva i inženjera u svemirskom centru Kenedi, zna bolje i tako kaže. "Ako želite da pojedete slona, ne radite to u jednom zalogaju." Neće dozvoliti nikome od onih kompjuterskih ljudi da mu odjednom gurnu veliki sistem. Jim preferira manje ujede slona.

Naš projekat će imati inkrementalni pristup. Prvo ćemo razviti alate za podršku zahtevima performansi. Zatim ćemo izgraditi minimalni sistem oko tih alata. Razvoj i upotreba tog prvog zalogaja će nam reći kako da nastavimo sa sledećim.

## Zahtevi za performanse

Učinak programa kolone "C programiranje" mora da prati ove smernice:

- To mora biti on-line, interaktivni PC program (da li postoji neka druga vrsta?).
- Koristiće iskačuće menije.
- Imaće prozore pomoći osetljive na kontekst.
- Unos podataka će biti podržan od strane biblioteke unosa opšte namene orijentisane na prozore.
- Postojaće paket za uređivanje teksta opšte namene za unos svih tekstualnih podataka.
- Komunikacioni deo programa pretpostavlja Haies-kompatibilan modem.
- Ksmodem protokoli za prenos podataka će biti podržani (možda i Kermit u kasnijem porastu).
- Sve funkcije prozora (unos podataka, uređivač, pomoć, meniji) će koristiti zajedničku biblioteku video prozora.

Za one koji ne znaju šta je video prozor, evo kratkog opisa. Video prozor je pravougaona oblast ekrana koja ima ivicu, ponekad naslov i tekst koji se prikazuje unutar ivice. Prozori se pojavljuju jedan na drugom. Kada prozor iskoči (nestane), nestane, a video ekrani - možda drugi prozori - koji su bili ispod njega ponovo su vidljivi.

## Prozorska biblioteka

Da bismo započeli projekat C kolone, počećemo sa bibliotekom prozora koja podržava ostatak programa. Ovaj paket je na dnu našeg dizajna. U ovom nastojanju kreiramo program odozdo prema gore. Ovo nije uvek u pravom smeru, ali ovde imamo prednost jer su naši zahtevi u skladu sa softverom koji sam objavio na drugim mestima. Oni od vas koji su čitali moje knjige prepoznaće sličnosti. Ovaj projekat će koristiti podskup tog softvera modifikovanog za potrebe ovog programa. Biblioteka prozora će biti dovoljno opšta da je možete koristiti za većinu drugih aplikacija orijentisanih na prozore, ali će joj nedostajati mnoge lepe funkcije koje se nalaze u knjigama i mnogim komercijalnim bibliotekama prozora. Izostavio sam ih jer ovde nisu potrebni. Želimo najefikasniji mogući program.

Funkcije prozora koriste funkcije konzole Turbo C tekstualnog režima za upravljanje postavljanjem prozora i prikazom podataka. Iz tog razloga će vam trebati Turbo C, verzija 1.5 ili novija.

Listing jedan, strana 112, je vindov.h, koji sadrži prototipove globalnih funkcija, neke #define izjave, strukture prozora i informacije o konfiguraciji. Nekoliko reči o konfiguraciji: Mnogi programi dolaze sa instalacionim programom koji vas vodi kroz izbor boja ekrana i slično. Nećemo uključiti takvu funkciju jer je ovo program za programere, koji programeri prave i koji programeri koriste. Programeri mogu da rukuju parametrima konfiguracije promenom izvornog koda i kompajliranjem. Da bih olakšao taj proces, koristiću #define makroe za stavke konfiguracije gde god je to moguće. Videćete blok takvih makroa na dnu liste jedan. Ovi makroi vam omogućavaju da konfigurišete boje ekrana programa. Postoji osam stavki ekrana koje se mogu prilagoditi. Ovo će vam dati nagovještaj o tome šta dolazi u paketu prozora u narednim mjesecima. Osam stavki su: ekrani za prikaz podataka, blokovi podataka (na primer, označeni blokovi u uređivaču teksta), prozori pomoći, meniji, trake za biranje menija, prozori za unos podataka, polja u prozorima za unos podataka i poruke o grešci. Kao što je ovde objavljeno, konfiguracija koristi uzorak crno-belih za sve stavke. Po želji možete koristiti i druge boje. Globalni simboli za boje su dati u Turbo C conio.h. Mogući globalni simboli su CRNI, PLAVI, ZELENI, CIJAN, CRVENI, MAGENTA, SMEĐA, SVETLOSIVA, TAMNOSIVA, SVETLO PLAVA, SVETLO ZELENA, SVETLO CIJAN, SVETLO CRVENA, SVETLO MAGENTA, ŽUTA i BELA.

Obratite pažnju na ezoterične prototipove i strukturu ispod komentara "interne Turbo C stvari". Oni pružaju pristup internoj video logici Turbo C i kompatibilni su sa Turbo C, verzijom 1.5. Ako Borland promeni ove konstrukcije u budućoj verziji, moraćemo da izvršimo prilagođavanja. Koristio sam ove konstrukcije za direktno čitanje i pisanje sa ekrana van domena onoga što dozvoljavaju tekstualne video funkcije u Turbo C-u. One nas oslobađaju od asemblerskog jezika i statusnih registara video retrace. Ovo je veoma upitno, neprenosivo, rizično programiranje hakerskog mentaliteta. Ovim se upozoravate da nikada ne koristite takve prakse u svom kodu. volim to.

Listing dva, strana 112, je vindov.c, biblioteka funkcija prozora. Biblioteka prozora pruža šest osnovnih funkcija za podršku prozorima. Koncept prozora predviđa da se poslednji postavljeni prozor adresira bilo kojim narednim prozorskim operacijama, tako da kada se prozor izbriše, onaj koji je uspostavljen pre nego što postane trenutni prozor. Evo opisa svake od funkcija.

**estabilish_window**  
Pozovite ovu funkciju da biste uspostavili i prikazali prozor. Očekuje sedam celih brojeva u svojoj listi parametara. Prva četiri parametra identično označavaju položaj ekrana prozora u koordinatama znakova u odnosu na jedan. Četiri koordinate su leva, gornja, desna i donja. Najveći mogući prozor je, dakle, 1,1,80,25. Sledeća dva parametra daju boju prednjeg plana (teksta) i pozadine prozora. Ovde će raditi globalne vrednosti boje koje su gore navedene. Poslednji parametar je TRUE ili FALSE da kaže funkcijama prozora da li treba da sačuvaju i vrate video memoriju ispod prozora. Ovaj parametar govori da li je prozor iskačući prozor ili ne. Obezbeđen je kao pogodnost za uštedu prostora na hrpi kada prozori ne moraju da čuvaju ono što pokrivaju.

**window__title**  
Ova funkcija će prikazati naslov u sredini gornje ivice trenutnog prozora. Prosledite mu adresu niza koji ima naslov koji želite da se prikaže.

**clear__window**  
Ova funkcija briše trenutni prozor i ponovo prikazuje njegovu ivicu. Ako ga koristite, moraćete da prepišete naslov sa vindov__title.

**delete__window**  
Ova funkcija zatvara trenutni prozor. Ako je drugi prozor uspostavljen neposredno pre trenutnog, taj drugi prozor postaje aktuelan. Ako je prozor uspostavljen sa poslednjim parametrom za uspostavljanje__vindov postavljenim na vrednost TRUE, onda se prozor briše i zamenjuje onim što je bilo ispod njega. U suprotnom prozor ostaje vidljiv, iako funkcije prozora nemaju više podataka o tome.

**text__window**  
Ova funkcija prikazuje redove teksta u trenutnom prozoru iz niza pokazivača znakova. Svaki pokazivač pokazuje na string, a poslednji pokazivač sadrži NULL vrednost pokazivača. Prosledite adresu niza (pokazivač na pokazivač karaktera) funkciji. Drugi parametar je celobrojni indeks u odnosu na onaj koji govori funkciji koji će unos u nizu biti prikazan u gornjem redu prozora. Ako niz ima više linija nego što može da stane u prozor, prikaz se zaustavlja kada se prozor popuni.

**select__window**  
Ova funkcija se koristi za pravljenje menija i birača podataka iz prozora. Pretpostavlja se da ste već uspostavili prozor i upisali izbore u njega pomoću prozora za tekst. Prodajete mu ceo broj u odnosu na onaj koji kaže koji izbor je istaknut kada se meni prvi put prikaže. Sledeća dva celobrojna parametra određuju boje prednjeg plana i pozadine trake za isticanje. Poslednji parametar je pokazivač funkcije. Kada se funkcija pozove, ona prikazuje meni i dozvoljava korisniku da se pomera gore i dole pomoću tastera za stranicu i strelice. Korisnik takođe može da izvrši izbor pritiskom na taster Enter. Kada se pritisne Enter, funkcija vraća broj, u odnosu na jedan, izbora. Ako se pritisne taster Esc, funkcija vraća nulu. Ako korisnik pritisne taster koji nije namenjen za pejdžing, skrolovanje ili biranje, poziva se funkcija u parametru pokazivača funkcije (osim ako je pokazivač NULL, u kom slučaju se ne poziva). Ovo obezbeđuje tastere za pomoć, izbore funkcijskih tastera i tako dalje. Funkcije menija koje će se dodati kasnije će koristiti ovu funkciju.

**window.c**  
Uključuje nekoliko drugih funkcija opšte namene koje će se koristiti u celom programu i imaju opštu primenu na bilo koji drugi program koji biste mogli da razvijete pomoću ove biblioteke. Evo opisa tih funkcija:

**error__message**  
Ovo je uslužna funkcija poruke o grešci koja prikazuje poruku o grešci u prozoru, pišti i čeka da korisnik pritisne taster Esc da bi potvrdio grešku. Prosledite mu pokazivač na poruku koja je završena nultom.

**hidecursor**  
Turbo C funkcije prozora ostavljaju kursor u prozoru koji je u upotrebi. Ova funkcija koristi BIOS da sakrije kursor u onim trenucima kada bi njegov prikaz odvukao pažnju korisnika.

**clear__screen**  
Šta još? Ova funkcija briše ekran.

**get__key**  
Ova funkcija čita znak sa tastature. On prevodi funkcijske tastere u ekvivalent njihovog koda za skeniranje sa najvažnijim skupom bitova. Ove vrednosti odgovaraju onima definisanim u vindov.h. Funkcija getkei takođe prati funkcijski taster pomoći ako je on programiran i poziva funkciju pomoći ako je obezbeđen. Ove funkcije će biti objašnjene u kasnijoj koloni kada instaliramo funkcije prozora pomoći osetljive na kontekst.

**scroll__window**  
Ova funkcija zahteva objašnjenje. Poziva se da pomera trenutni prozor gore ili dole jedan red. On bira jedan od dva načina da to uradi na osnovu promenljive __video.snov. Biblioteka teksta Turbo C je veoma lepo odredila da li će ekran prikazati sneg videa kada se završi direktno čitanje i upisivanje video zapisa. Ovu odluku donosi testiranjem poboljšanog grafičkog adaptera, koji ne pada, adaptera za monohromatski ekran, koji ne, ili grafičkog adaptera u boji, koji radi. Ide dalje i odlučuje da Compak verzija CGA ne pada, što, naravno, ne pada. Rezultat njegove odluke je zabeležen u "__video.snov". To će odlučiti da neke mašine sneže kada ne padnu. Primer je laptop Toshiba T1000. Sve je ovo u redu, ali kada koristimo tekstualne funkcije Turbo C za pomeranje ekrana na snežnoj mašini, pomeranje je presporo. To je zato što svaki znak koji se čita ili upisuje dok se pomera tekst za pomeranje mora da sačeka da se pojavi video retrace. Dakle, ako dozvolimo Turbo C da skroluje umesto nas, performanse trpe. Iz tog razloga koristimo BIOS video usluge za pomeranje snob ekrana. Možda se pitate zašto to ne radimo uvek. Ah, hirovi nesavršenog sveta! Kada se BIOS pomera, on koristi dosadno zatamnjenje ekrana. Ova smetnja je draža od snega ili sporo, pa se prihvata kao manje od tri zla.

In the next several months we'll add the help functions, menu manager, data entry screens, and a text editor. After that we'll get into the communications part of the program and its ultimate purpose.

### C Kroket broj 3 3: Kako se uči C

(A reminder: crotchets are things that irk people. See last month's "C Programming" for an explanation and crotchets number 1 and number 2.)

My best friend and companion, Judy, is enrolled in a computer science program at a community college in Virginia. She just completed an introductory class in C. The instructor admitted that he was new to C and gave assignments for the development of a small program. He provided examples of how certain things were to be coded. Here is a fragment.

```c
while (fgets(line[lineno ++], 100, fp));
```

The class was told that this example is how you read a file of text into memory. Notice particularly the semicolon. Most of us can figure out what's happening here. The while is being used to loop until all the lines of a file are read into an array. So what's wrong with this picture? Plenty.

1. There is no boundary checking. If the file has more lines than the array, the program will probably crash.

2. The semicolon is on the wrong line. It should be indented below the while statement to tell the reader that it is an intended null statement rather than the accident it appears to be.

3. The instructor never explained the notion of an operational statement as a component of a conditional expression within a while, nor did he address the use of the null statement, which is there merely to give the while something to do until its condition is FALSE.

4. The idea that the statement that reads the file also returns the end-of-file condition and can be tested when it is executed was not explained.

5. The level at which the class is offered suggests that the students are not ready for such concepts as auto-increments that occur at the same time the subscripting integer is used as a subscript.

6. The constant 100 is a danger spot. If line is a two-dimensional array of strings, the constant can be coded with the size of operator. At least the constant should be equated to a global symbol for better reading and easier maintenance.

Here is a clearer presentation of the same logic with boundary checking added.

```c
while (lineno <MAXLINES) { rtn = fgets(line[lineno], LENGTH, fp); if (rtn = NULL) break; lineno+ +; }
```

Sure, it uses several lines where one will do, and sure, few seasoned C programmers really write code like that. This crotchet is not for expert C programmers who only program. They are forever encouraged to use the language to its fullest. This crotchet is for those of you who would teach. C encourages tight and concise expressions and for that we hold it dear. The new C programmer, however, needs to move carefully and slowly into such advanced usage. This freedom of syntax is the chief object of criticism of C by devotees of other languages but ducators do not need to teach it so. Programmers can learn nice, readable statement sequences like the one above and approach the tighter, more elegant side of the language at their own pace. What the students were taught was that the instructor's example would read the text into the array. They never learned why.

I plan to devote a future column to the problems of teaching C.

C kroket broj 4: Zamerke zbog naknada za nadogradnju

I like Turbo C. And someday when I get a new corrected version, I might begin to like QuickC. While doing research for books on these compilers, I spent a lot of time on the CompuServe and BIX on-line services. There is no better way to clear up a compiler or language problem than by using the related forums of these services. Lately a common theme has been running through the discussions, one that wastes connect time and deserves comment. When Borland or another vendor releases a new version of a C compiler, they usually charge a nominal upgrade fee to registered users. Such a fee seems reasonable when you consider the value of a C compiler. Many of you will remember what we used to pay for C compilers that had far fewer features, The common complaint, however, is that the programmers feel ripped off by the fee. They argue that since the upgrade includes the correction of known bugs, the vendor should provide it for free. We all know that those multipage full color ads (do you believe Turboman?) and those thousands of programmer person-hours are not free to the compiler vendors. The revenue to continue promoting and improving the product has to come from somewhere. Yet the grumbles persist. One such forum conversation went on for several days about a $10 upgrade charge.

My pal Bill Chaney gets hot and says such folks are "so tight they wouldn't spend a nickel to see a stink bug eat a bale of hay." Easy, Bill....

## The C Programska biblioteka

The book to read this month is C Programmer's Guide to Serial Communications by Joe Campbell (Howard W. Sams & Company, 1987). There are over 650 pages of text and code dealing with the subject of asynchronous serial communications on microcomputers. Campbell presents the subject matter in clear but advanced language. This is no book for beginners. You need to be a C programmer at the very least, and it helps to already have a passing acquaintance with the murky depths of serial communications.

Campbell's writing style is refreshing and makes for good if not light reading. He likes to use words that will send many of us scrambling for Webster's, and he makes no attempt to conceal his contempt for what he considers an inferior design. His treatment of the Xmodem and Kermit data-transmission protocols leave the reader with the impression that in his view the world would be a far better place if Joe Campbell had been allowed to design them. Mostly his criticisms are valid and to the point even when obviously born from the precious perspective of clear hindsight, but if I were Ward Christensen or a few others my ears would be burning. In the preface to the book, Campbell refers to his "humble recognition of the demands of [the subject of data communications]," and that is the last evidence of humility you'll find in this book. An occasional lapse in humility, however, is OK when the apparent arrogance is justified or substantiated by equally apparent intelligence and expert knowledge, and Campbell delivers in this book. His description of Kermit is the first one I've read that didn't require at least a second reading to understand.

The book discusses communications theory, hardware, software, and the C language implementations of these concepts. It is a healthy treatment of these subjects and a necessary addition to the library of those involved in C projects where computers talk to one another through serial interfaces. Recently I downloaded a C source code archive file that purports to implement the Kermit protocol in a Unix environment. l wanted to port the undocumented code to the PC and learn from it. To my chagrin I found that the one source file in the archive is incomplete, abruptly ending in the middle of a function, With the explanations in Campbell's book I can now attempt to provide the missing code. Campbell's book does have C functions that implement the Xmodem protocol, but does not have equivalent code for Kermit. Drat.

Until I read this book I never fully understood why serial input-output and the RS-232 "standard" had been so confused for so many years. When I was consulting in 1978, the microcomputer was new to the business world and plug-and-go appliance computer systems were rare. I earned a substantial part of my living with a break-out box, some cable stock, DB25 connectors, and a soldering iron. Every installation had some new serial printer that needed to be connected to some hacked-together microcomputer and nothing ever fit. In the chapter on RS-232 control Campbell explains that RS-232 was never intended to be used for printer handshaking. How come I never knew that? The original RS-232 intentions notwithstanding, most printers back then could be bought with a serial port. This option allowed users of smart terminals and modems to slave a printer to the auxiliary serial port on their terminal. As a consequence, the designers of early microcomputers put serial printer interfaces in their machines to accept those printers already in use. The Centronics parallel standard for printers was around but few systems used it. Maybe the UARTS and line driver chips were cheaper or more available than parallel drivers. Whatever the reason for the neglect of the Centronics interface, when IBM adopted it for the PC they started a trend that eventually sent me happily out of the 1200 baud connectivity business. I could have used Campbell's book back then.

Campbell earns my respect for his acknowledgment of Cole Porter and Johnny Mercer, two popular music composers from the pre-MTV era. They certainly weren't programmers and probably never heard of C, RS-232, or even MIDI, but their legacy to our culture is appreciated among those of us who dawdle too long and too late in the silent ones and zeros.

Next month we will add some features to the window library, look at another book, and kick some more crotchets around. Until then, stay fit and keep coding.

_C PROGRAMMING_ by Al Stevens [LISTING ONE]

```c
/*---------- window.h -----------*/

void establish_window(int,int,int,int,int,int,int);
void window_title(char *);
void clear_window(void);
void delete_window(void);
void scroll_window(int);
void text_window(char **, int);
int select_window(int, int, int, int (*func)(int,int));
int getkey(void);
void hidecursor(void);
void set_cursor_type(unsigned);
void clear_screen(void);
void writeline(int, int, char *);
void current_window(void);
void error_message(char *);

#define MAX_WINDOWS 10      /* maximum windows open at once */

#define TRUE        1
#define FALSE       0
#define ERROR      -1

#define BELL        7
#define ESC        27
#define SHIFT_HT  143
#define CTRL_T     20
#define CTRL_B      2
#define CTRL_D      4
#define ALT_D     160
#define ALT_F     161
#define ALT_E     146
#define ALT_O     152
#define ALT_S     159

#define F1        187
#define F2        188
#define F3        189
#define F4        190
#define F5        191
#define F6        192
#define F7        193
#define F8        194
#define F9        195
#define F10       196
#define ALT_F7    238

#define HOME      199
#define UP        200
#define PGUP      201
#define BS        203
#define FWD       205
#define END       207
#define DN        208
#define PGDN      209
#define INS       210
#define DEL       211

#define CTRL_HOME 247
#define CTRL_BS   243
#define CTRL_FWD  244
#define CTRL_END  245

/* --------- window definition structure ------------ */
struct wn {
   int lf,tp,rt,bt;        /* window position */
   int ht,wd;              /* window dimensions */
   int wx, wy;             /* window cursor */
   int wtop;               /* top text line */
   int wlines;             /* total text lines */
   int fg,bg;              /* window colors */
   char *wsave;            /* video memory save buffer */
   char **wtext;           /* pointer to text */
};

/* ------ internal Turbo C stuff ------- */
void far * pascal __vptr(int, int);
void pascal __vram(void far *, void far *, int);
extern struct {
   char filler1[4];
   char attribute;
   char filler2[5];
   char snow;
} _video;

/* ------------ window colors --------------- */
#define TEXTFG  WHITE   /* data display screen */
#define TEXTBG  BLACK
#define BLOCKFG BLACK   /* data blocks */
#define BLOCKBG WHITE
#define HELPBG  WHITE   /* help windows */
#define HELPFG  BLACK
#define MENUBG  WHITE   /* menus */
#define MENUFG  BLACK
#define SELECTBG BLACK   /* menu selector bars */
#define SELECTFG WHITE
#define ENTRYFG WHITE    /* data entry windows */
#define ENTRYBG BLACK
#define FIELDFG BLACK    /* data entry fields  */
#define FIELDBG WHITE
#define ERRORFG BLACK   /* error messages */
#define ERRORBG WHITE
```

[LISTING TWO]

```c
/* ----------------------- window.c --------------------- */

#include <stdio.h>
#include <alloc.h>
#include <string.h>
#include <conio.h>
#include <mem.h>
#include <dos.h>
#include <stdlib.h>
#include "window.h"

/* --------- window border characters ---------------- */
#define NW   '\332'
#define NE   '\277'
#define SE   '\331'
#define SW   '\300'
#define SIDE '\263'
#define LINE '\304'

int editing;
static union REGS rg;

/* --------- window definition structure ------------ */
struct wn wdo [MAX_WINDOWS];
int curr_wnd;   /* current window */
struct wn wkw;   /* a working window structure */

static void upline(void);
static void downline(void);
static void firstline(void);
static void lastline(void);
static void dline(int, int, int);

/* ----------- establish a new window -------------- */
void establish_window(left,top,right,bottom,foreg,backg,save)
{
   if (curr_wnd < MAX_WINDOWS)   {
      if (curr_wnd)
         wdo[curr_wnd-1] = wkw;
      setmem(&wkw, sizeof(wkw), 0);
      wkw.lf = left;
      wkw.tp = top;
      wkw.rt = right;
      wkw.bt = bottom;
      wkw.fg = foreg;
      wkw.bg = backg;
      wkw.wd = right+1-left;
      wkw.ht = bottom-top-1;
      if (save)   {
         if ((wkw.wsave=malloc((wkw.ht+2)*wkw.wd*2)) == NULL)
            return;
         gettext(left, top, right, bottom, wkw.wsave);
      }
      wdo[curr_wnd++] = wkw;
      current_window();
      clear_window();
   }
}

/* ------- initialize the working window as current -------- */
void current_window()
{
   window(wkw.lf,wkw.tp,wkw.rt,wkw.bt);
   hidecursor();
   if (wkw.fg || wkw.bg)   {
      textcolor(wkw.fg);
      textbackground(wkw.bg);
    }
}

/* ----------- set a window's title -------------- */
void window_title(char *ttl)
{
   writeline((wkw.wd-strlen(ttl)) / 2, 1, ttl);
}

/* ------------ remove a window ------------------ */
void delete_window()
{
   if (curr_wnd)   {
      if (wkw.wsave)   {
         puttext(wkw.lf,wkw.tp,wkw.rt,wkw.bt,wkw.wsave);
         free(wkw.wsave);
      }
      setmem(wdo+curr_wnd-1, sizeof (struct wn), 0);
      --curr_wnd;
      if (curr_wnd)   {
         wkw = wdo[curr_wnd-1];
         current_window();
      }
   }
}

/* ---- clear the window area and display the border ----- */
void clear_window()
{
   int height, width, y = 1;
   char line1[81], line2[81];

   height = wkw.ht;
   width = wkw.wd;
   setmem(line1 + 1, width-1, LINE);
   setmem(line2 + 1, width-1, ' ');
   *line1 = NW;
   line1[width-1] = NE;
   line1[width] = '\0';
   *line2 = SIDE;
   line2[width-1] = SIDE;
   line2[width] = '\0';
   line1[width] = line2[width] = '\0';
   writeline(1, y++, line1);
   while (height--)
      writeline(1, y++, line2);
   *line1 = SW;
   line1[width-1] = SE;
   writeline(1, y, line1);
}

/* --------- scroll the window. d: 1 = up, 0 = dn ---------- */
void scroll_window(d)
{
   if (_video.snow == 0)
      movetext(wkw.lf+1, wkw.tp+1+d,
           wkw.rt-1, wkw.bt-2+d,
           wkw.lf+1, wkw.tp+2-d);
   else   {
      rg.h.ah = d ? 6 : 7;
      rg.h.al = 1;
      rg.h.bh = _video.attribute;
      rg.h.cl = wkw.lf;
      rg.h.ch = wkw.tp;
      rg.h.dl = wkw.rt-2;
      rg.h.dh = wkw.bt-2;
      int86(16, &rg, &rg);
   }
}

/* ---------- display text in a window --------------- */
void text_window(char *txt[], int ln)
{
   int height = wkw.ht;

   wkw.wtext = txt;
   wkw.wtop = ln;
   wkw.wy = 1;
   while (height-- && txt[ln-1])
      dline(ln++, wkw.fg, wkw.bg);
   wkw.wlines = 0;
   while (*txt++)
      wkw.wlines++;
}

static int lineno;
/* -------- page and scroll through a window of text -------- */
int
select_window(int ln,int foreg,int backg,int (*func)(int,int))
{
   int c = 0;
   int frtn;
   int height, dln, ptop;

   if (ln > wkw.wtop + wkw.ht-1 || ln < wkw.wtop)
      text_window(wkw.wtext, ln);
   else
      wkw.wy = ln - wkw.wtop + 1;
   while (TRUE)   {
      lineno = wkw.wtop + wkw.wy - 1;
      ptop = wkw.wtop;
      dline(lineno, foreg, backg);
      if (wkw.wx == 0)
         hidecursor();
      else
         gotoxy(wkw.wx, wkw.wy+1);
      c = getkey();
      if (c == '\r' || c == ESC)
         break;
      switch (c)   {
         case CTRL_HOME:
            firstline();
            break;
         case CTRL_END:
            lastline();
            break;
         case PGUP:
            wkw.wtop -= wkw.ht;
            if (wkw.wtop < 1)
               wkw.wtop = 1;
            break;
         case PGDN:
            wkw.wtop += wkw.ht;
            if (wkw.wtop > wkw.wlines - (wkw.ht-1))   {
               wkw.wtop = wkw.wlines - (wkw.ht-1);
               if (wkw.wtop < 1)
                  wkw.wtop = 1;
            }
            break;
         case UP:
            upline();
            break;
         case DN:
            downline();
            break;
         default:
            if (!editing && wkw.wlines <= wkw.ht)   {
               if (c == HOME)   {
                  firstline();
                  break;
               }
               if (c == END)   {
                  lastline();
                  break;
               }
            }
            if (func)   {
               frtn = (*func)(c, lineno);
               if (frtn == ERROR)
                  putch(BELL);
               else if (frtn)   {
                  wkw.wy = frtn;
                  return frtn;
               }
               c = 0;
            }
            break;
      }
      switch (c)   {
         case HOME:
         case CTRL_HOME:
         case END:
         case CTRL_END:
         case PGUP:
         case PGDN:  if (wkw.wtop != ptop)   {
                     height = wkw.ht;
                     dln = wkw.wtop;
                     while (height-- && wkw.wtext[dln-1])
                        dline(dln++, wkw.fg, wkw.bg);
                     break;
                  }
         default:    dline(lineno, wkw.fg, wkw.bg);
                     break;
      }
   }
   return c == ESC ? 0 : lineno;
}

/* ---------- move up one line --------------- */
static void upline()
{
   if (lineno > 1)   {
      if (wkw.wy == 1)   {
         if (wkw.wtop > 1)   {
            --wkw.wtop;
            scroll_window(0);
         }
      }
      else
         --wkw.wy;
   }
   else if (wkw.wlines <= wkw.ht)
      lastline();
}

/* ----------- move down one line ------------- */
static void downline()
{
   if (lineno < wkw.wlines)   {
      if (wkw.wy == wkw.ht)   {
         scroll_window(1);
         wkw.wtop++;
      }
      else
         wkw.wy++;
   }
   else if (wkw.wlines <= wkw.ht)
      firstline();
}

/* -------- move to the first line --------- */
static void firstline()
{
   wkw.wtop = wkw.wy = 1;
}

/* -------- move to the last line --------- */
static void lastline()
{
   wkw.wtop = wkw.wlines - (wkw.ht-1);
   if (wkw.wtop < 1)
      wkw.wtop = 1;
   wkw.wy = wkw.ht;
   if (wkw.wy > wkw.wlines)
      wkw.wy = wkw.wlines;
}

char spaces[80] =
"                                                                               ";
/* ------- display a line of text, highlight or normal ------ */
static void dline(ln, foreg, backg)
{
   if (foreg || backg)   {
      textcolor(foreg);
      textbackground(backg);
      --ln;
      writeline(2, ln-wkw.wtop+3, *(wkw.wtext + ln));
      if (strlen(*(wkw.wtext + ln)) < wkw.wd-2)
         writeline(2+strlen(*(wkw.wtext + ln)),
                 ln-wkw.wtop+3,
                 spaces + 79 - wkw.wd +
                 strlen(*(wkw.wtext + ln)) + 2 );
   }
}

/* --------- write a line of text to video window ----------- */
void writeline(int x, int y, char *str)
{
   int cl[80], *cp = cl;

   while (*str)
      *cp++ = (*str++ & 255) | (_video.attribute << 8);
      __vram(__vptr(x+wkw.lf-1,y+wkw.tp-1),MK_FP(_DS,cl),cp-cl);
}

/* ------- use BIOS to hide the cursor ---------- */
void hidecursor()
{
   rg.h.ah = 2;
   rg.x.dx = 0x1900;
   rg.h.bh = 0;
   int86(0x10, &rg, &rg);
}

/* ----------- use BIOS to set the cursor type -------------- */
void set_cursor_type(unsigned t)
{
   rg.x.ax = 0x0100;
   rg.x.bx = 0;
   rg.x.cx = t;
   int86(0x10, &rg, &rg);
}

/* ----------- use BIOS to clear the screen ---------------- */
void clear_screen()
{
   window(1,1,80,25);
   gotoxy(1,1);
   rg.h.al = ' ';
   rg.h.ah = 9;
   rg.x.bx = 7;
   rg.x.cx = 2000;
   int86(0x10, &rg, &rg);
}

void (*helpfunc)(void);
int helpkey;

/* ------------- read the keyboard ---------------- */
int getkey()
{
   int c;

   if ((c = getch()) == 0)
      c = getch() | 128;
   if (c == helpkey && helpfunc)   {
      (*helpfunc)();
      c = getkey();
   }
   return c;
}

/* ------- write an error message ------------- */
void error_message(char *ermsg)
{
   int lf = (80-strlen(ermsg)+2)/2;
   int rt = lf+max(strlen(ermsg)+2,15);
   establish_window(lf, 11, rt, 14, ERRORFG, ERRORBG, TRUE);
   gotoxy(2,2);
   cputs(ermsg);
   gotoxy(2,3);
   cputs("(Press [Esc])");
   hidecursor();
   do
      putch(BELL);
   while (getkey() != ESC);
   delete_window();
}
```
