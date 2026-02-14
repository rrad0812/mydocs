
# C programiranje 08

## TINYCOMM Begets SMALLCOM

Al Stevens, mart '89

Prošlog meseca smo dodali funkcije našoj biblioteci C alata za podršku serijskim portovima i modemima i objasnili – tako ukratko – osnove serijske komunikacije. Da bismo ilustrovali upotrebu ovih alata i principa, napravili smo komunikacioni program pod nazivom TINICOMM. Taj program ne koristi nijednu funkciju prozora, menija i pomoći iz naše tekuće zbirke biblioteke. Umesto toga, njegova prezentacija je bila fokusirana na sažetu demonstraciju upotrebe serijskih i modemskih funkcija. Ovog meseca program TINICOMM stvara potomstvo koje se zove SMALLCOM i koje koristi prozore, meni i alate za unos podataka iz ranijih kolona da podrži korisnički interfejs. SMALLCOM ima više funkcija koje se nalaze u komercijalnom komunikacionom programu – karakteristike, kao što su otpremanje i preuzimanje datoteka, konfiguraciona datoteka serijskog porta koja se može promeniti iz programa, uređivač, kuke za telefonski imenik i skripte, kuke za protokole za prenos datoteka i automatsko prepoznavanje i reakcija na rezultate Haies modema kodova.

### SMALLCOM izvorni kod

Liste za SMALLCOM su [Listing 1](#listing-1), "smallcom.c", [Listing 2](#listing-2) "smallcom.prj", [Listing 3](#listing-3), "smallcom.mak", i [Listing 4](#listing-4) "smallcom.lnk". "Smallcom.c" je izvorni kod za program. Pored toga, biće vam potrebna većina izvornih programa biblioteke objavljenih u ovoj rubrici od septembra kada je projekat započeo. Smallcom.prj je datoteka Turbo C projekta za pravljenje programa iz Turbo C okruženja. Postavite kompaktni memorijski model i definišite ove globalne makroe ili kao #define izjave u "windov.hW ili u okviru opcije Compiler Defines (Alt-O/C/D) okruženja Turbo C kao što je prikazano ovde:

```c
TURBOC=1;MSOFT=2;COMPILER=TURBOC
```

"Smallcom.mak" i "smallcom.lnk" su Microsoft C make datoteka i komandna datoteka linkera za pravljenje programa. Oni pretpostavljaju da se Microsoft C nalazi na putanji izvršavanja DOS-a, da se MSC biblioteke nalaze u poddirektorijumu \LIB i da su promenljive okruženja LIB i INCLUDE pravilno podešene.

### Kuke

Ranije sam pomenuo kuke u programu. Ove kuke će se koristiti u narednim mesecima za dodavanje telefonskog imenika, on-line servisnih skripti i KSModem i Kermit protokola za prenos datoteka. Zakačilice su pokazivači funkcija koji u početku imaju NULL vrednosti. Dok dodajemo funkcije, mi ćemo inicijalizovati pokazivače funkcija sa adresama funkcija za funkcije koje želimo da dodamo. Ova tehnika povezivanja nam omogućava da planiramo proširenje uz očuvanje većine postojećeg koda.

Prekidanje telefonskog imenika se izvršava izborom menija imenika. Zakačivanje procesora skripte se izvršava kada izvorni poziv uspostavi vezu sa udaljenim procesorom. Kasnije ćemo odlučiti šta je zapravo proces skripte. Za sada je dovoljno da znamo da kada nam zatreba, biće tu kada poziv prođe. Skripte su obično povezane sa određenim onlajn uslugama, tako da će SMALLCOM povezati skripte sa telefonskim brojevima u imeniku. Zakačice protokola za prenos datoteka se javljaju na dva nivoa. Viša kuka je poziv funkcije koja će omogućiti korisniku da izabere protokol za otpremanje ili preuzimanje. Adresa funkcije će biti u zakačici funkcije select_transfer_protocol. Ta funkcija mora da vrati indeks u niz pokazivača funkcija i biće obezbeđena kasnije. Postoji niz za otpremanje koji se zove up_protocol i jedan za preuzimanja koji se zove dovn_protocol. Ova dva niza će sadržati adrese funkcija koje implementiraju različite protokole. Prvi unosi u oba niza su adrese funkcija ASCII protokola, koje su uključene u ovo prvo izdanje SMALLCOM-a. Drugi će biti dodati kasnije.

### Više komunikacionih procesa

Da bismo koristili modem na načine koji su potrebni za SMALLCOM, moramo da promenimo njegov string za inicijalizaciju sa vrednosti korišćene prošlog meseca. Izmenite definiciju INITMODEM u modem.h na ovu vrednost:

```sh
AT&C1E-0M1S7=60S11=55V1X3S0=0\r
```

Pogledajte sada kraj liste jedan, smallcom.c. Postoje neke dodatne funkcije za upravljanje modemom i serijskim portom. Ove funkcije nisu bile potrebne u prošlomesečnom TINICOMM programu. Možda biste želeli da ih premestite u serial.c i modem.c prema potrebi. Ovde ću objasniti te funkcije.

Funkcija testcarrier koristi makro nosioca da vidi da li je modem izgubio signal za otkrivanje nosioca. Testiranjem ovog signala, program može utvrditi da je udaljeni procesor prekinut. Međutim, prvo morate da konfigurišete svoj modem za normalan rad signala za otkrivanje nosioca. Mnogi modemi će opciono potvrditi ovaj signal u svakom trenutku bez obzira na vezu. Pogledajte da li vaš modem ima dip prekidač za isključivanje signala i koristite ga kao podrazumevanu opciju. Zapamtite, nisu svi modemi isti i neće svi imati isto podešene modeme.

Funkcija čekanja veze se koristi kada program čeka na dolazni poziv ili čeka odgovor na izvorni poziv. Kada dođe do dolaznog poziva, funkcija može da oseti brzinu prenosa pozivaoca i prema tome prilagodi brzinu lokalnog prenosa. Ovaj proces je omogućen zahvaljujući kodovima rezultata koje je vratio modem kada uspostavi vezu.

Funkcija vaitforconnect poziva funkciju vaitforresult, koja čeka i vraća kod rezultata modema. Modem vraća nizove CONNECT, CONNECT 1200 ili CONNECT 2400 u zavisnosti od brzine prenosa pozivaoca. Za upućeni poziv, modem će vratiti NO CARRIER ako pozvana strana odgovori bez signala za otkrivanje operatera ili NO ANSVER ako se pozvana strana ne javi. Funkcija vaitforresult prevodi ove nizove u celobrojne kodove rezultata.

Funkcija vaitforresult poziva funkciju vaitforstring opšte namene. Ova funkcija će nam biti važna kasnije kada uđemo u obradu skripte. Ovoj funkciji prosleđujete adresu niza znakova, od kojih svaki ukazuje na string. Funkcija prati serijski ulazni tok da bi videla da li se tok podudara sa bilo kojim od nizova i vraća pomak odgovarajućeg niza ili -1 ako vrednost TIMEOUT istekne pre nego što dođe do bilo kakvog podudaranja. Ova upotreba funkcije vaitforstring koristi niz pokazivača na šifre rezultata modema. vaitforstring vam omogućava da koristite obrnutu kosu crtu kao znak džokera u argumentima stringa.

### Help prozori

U decembru smo uveli prozore pomoći u našu kolekciju alata i ilustrovali njihovu upotrebu uključivanjem ove funkcije u TVRP mali procesor teksta. Prozori pomoći se snimaju u datoteci koja je imenovana funkcijom load_help i identifikovana mnemonikom prozora pomoći koja se nalazi u strukturama FIELD ekrana za unos podataka -- strukturama MENU -- i pozivima funkcije set_help. SMALLCOM uključuje ove mnemonike, ali ja ne objavljujem tekst za prozore pomoći. Nema više šta da se nauči iz više teksta pomoći, tako da bi služili samo da se iskoristi dragoceni prostor u časopisu. Nove prozore pomoći možete prilagoditi svojim željama ili ih možete izostaviti. SMALLCOM uključuje većinu TVRP-a kao svoj integrisani uređivač, tako da možete kopirati decembarski tvrp.hlp u datoteku koju ćete nazvati smallcom.hlp i dodati joj prozore pomoći specifične za SMALLCOM. Mnemotehnika se nalazi u izvornom kodu u smallcom.c.

### Konfiguraioni fajl

Kada se SMALLCOM pokrene, on traži datoteku pod nazivom smallcom.cfg. Ako ta datoteka postoji, SMALLCOM čita njen sadržaj u promenljive koje određuju podrazumevani serijski port i parametre modema - koji port se koristi, paritet, broj stop bitova, dužina reči, brzina prenosa, da li treba da se koristi pulsno ili tonsko biranje i koji je podrazumevani telefonski broj za pozive koje je uputio SMALLCOM. Ako datoteka ne postoji, program koristi vrednosti kodirane u te promenljive kada je SMALLCOM kompajliran. Traka menija SMALLCOM uključuje iskačući meni koji vam omogućava da promenite sve osim telefonskog broja i da upišete sve uključujući telefonski broj u novu kopiju konfiguracione datoteke. Kako stvari sada stoje, to vam omogućava da konfigurišete sve osim telefonskog broja.

### Format ekrana SMALLCOM

SMALLCOM pruža veliki prozor za prikazivanje teksta prosleđenog između procesora i koristi softver za upravljanje menijima iz naše biblioteke prozora. Traka menija na vrhu ekrana ima izbore pod nazivom „Datoteka“, „Poveži“, „Parametri“, „Uređivač“ i „Direktorijum“. Možete doći do jednog od ovih izbora pritiskom na F10 i korišćenjem tastera sa strelicom nadesno i nalevo da biste se kretali napred-nazad između iskačućih menija, ili možete da pritisnete taster Alt zajedno sa prvim slovom željenog izbora. „Datoteka“, „Poveži“ i „Parametri“ biraju iskačući meniji za dalje izbore. Izbori „Editor“ i „Directori“ osvetljavaju izbor na traci menija i omogućavaju vam da pritisnete Enter da biste izvršili uređivač teksta ili telefonski imenik. Donji deo ekrana ima statusnu traku koja pokazuje šta se dešava. Poruka „On/Off Line“ govori da li ste povezani sa udaljenim računarom. Poruka „Direktno“ kaže da ste izabrali direktnu, null-modemsku vezu. Ako registrujete tekst, pojavljuje se poruka „Logging“. Ako ste u režimu odgovora modema i čekate na poziv, pojavljuje se poruka „Odgovaram“. Dok otpremate ili preuzimate datoteku, pojavljuje se poruka „Otpremanje“ ili „Preuzimanje“ sa onoliko putanje i naziva datoteke koliko može da stane na statusnu traku. Prikazan je trenutni broj telefona -- onaj koji će biti pozvan izborom "Pozovi" u meniju "Poveži".

Iskačući meni „Parametri“ vam omogućava da podesite i promenite parametre serijskog porta i modema. Njihove početne vrednosti su zabeležene u datoteci smallcom.cfg. Koristite taster Enter za kretanje kroz važeća podešavanja za svaki parametar. Sve promene koje unesete u meni su na snazi sve dok program radi. Ako koristite izbor „Vrite Parameters“, trenutna podešavanja se upisuju u datoteku smallcom.cfg i biće podrazumevane vrednosti sledeći put kada pokrenete program.

Ne možete komunicirati preko SMALLCOM-a dok se ne povežete sa drugim računarom. Drugi računar može da koristi SMALLCOM, može biti onlajn usluga ili sistem oglasne table, ili može biti drugi komunikacioni program kao što je Procomm. Iskačući meni „Poveži“ ima izbore za uspostavljanje i prekid veze. Kada izaberete „Pozovite“, SMALLCOM bira trenutni telefonski broj (prikazano u statusnoj liniji na dnu ekrana sa desne strane) i čeka da udaljeni računar odgovori. Izbor „Odgovori na poziv“ stavlja modem u režim odgovora. Kada stigne poziv, modem će odgovoriti na telefon i vratiti statusnu poruku koja govori o brzini prenosa pozivaoca. Izbor "Prekini" prekida vezu. Komanda "Direktna veza" pretpostavlja da je veza direktna pomoću null modemskog kabla i da nisu uključene nikakve modemske komande.

Kada se uspostavi jedna od ovih veza, kursor se nalazi u prozoru sa podacima i možete kucati poruke i čitati poruke koje stižu sa udaljenog računara. Možete da koristite iskačući meni „Datoteka“ za otpremanje i preuzimanje datoteka i za uključivanje i isključivanje sistemske prijave. Sistemski dnevnik je datoteka pod nazivom smallcom.log koja beleži sve što je program poslao i primio. Ako smallcom.log postoji kada uključite opciju, novi tekst se dodaje datoteci.

### Echoes

Kada dva računara razgovaraju preko telefonskih linija, jedan od njih je uputio poziv, a drugi je odgovorio. Dve uloge su donekle različite. Pozivalac će očekivati da pozvani sistem odzove sve znakove koje pozivalac pošalje i neće prikazati znakove lokalno dok se kucaju. Pozvani sistem ne očekuje od pozivaoca da eho i na taj način prikazuje sopstvene karaktere dok se kucaju. Prema tome, SMALLCOM mora zapamtiti da li je pokrenuo ili je odgovorio na poziv i da ne eho ili eho shodno tome. (Eho je vraćanje karaktera koji je upravo primljen. Ovo ponašanje sam izveo empirijski posmatrajući ponašanje drugih komunikacionih programa.) Ako pozovete računar i otpremite tekstualnu datoteku, telefonska sekretarica će ponoviti svaki znak jer ne zna da ne kucate. Međutim, ako se pozvanom računaru kaže da preuzme datoteku, on ne ponavlja znakove koje ste poslali, jer pretpostavlja da je prenos datoteke u toku. Ovo su pravila koja SMALLCOM poštuje. Stoga, ako pozvani računar preuzima i pozivalac kuca, pozivalac neće prikazati znakove na svom ekranu jer se ne šalje eho. Nasuprot tome, ako pozivalac otprema, a pozvani sistem ne preuzima, tekst se prikazuje na obe lokacije tokom prenosa. Ako pozivalac otprema, a pozvani sistem preuzima, nijedan računar ne prikazuje tekst. Kada izaberete režim „Direktna veza“, svaki računar prikazuje sopstvene pritiske na tastere i ne vraća ništa drugom. Zbunjen? I ja sam bio kada sam sve ovo razradio.

U komunikacijskom žargonu ove procedure se nazivaju poludupleksnim i punim dupleksnim prenosom, a neki komunikacioni programi vam omogućavaju da konfigurišete jedan ili drugi. Moj cilj je bio da dozvolim programu da odredi pravilan način rada na osnovu njegovog prepoznavanja okolnosti.

Ovi problemi sa odjekom odnose se na kucanje, a takođe se odnose i na ASCII protokol za prenos datoteka jer taj protokol primaocu izgleda baš kao kucanje. Ako ne kažete drugom računaru da šaljete datoteku, ne vidite nikakvu razliku. ASCII protokol za prenos se obično koristi za slanje tekstualnih poruka koje su pripremljene van mreže, ali se može koristiti i za otpremanje datoteka koje nemaju kritičan sadržaj. Kada to uradite, prijemni program zna da je prenos datoteke u toku jer mu kažete da preuzme datoteku. Kada uđemo u binarne protokole za prenos datoteka, nikakvi problemi odjeka neće nas smetati jer oba računara moraju biti potpuno svesna šta se dešava.

### Urednik

Izbor urednika na traci menija poziva SMALLCOM uređivač teksta, koji je integrisana verzija malog TVRP procesora teksta iz decembra. Sve TVRP komande su dostupne i možete uređivati tekstualnu datoteku do 800 redova. Možete pozvati ovaj uređivač dok ste na mreži ili van mreže. Može se koristiti za pregledavanje poruka koje ste preuzeli ili za sastavljanje odgovora. Obično ćete ga koristiti dok ste van mreže da biste uštedeli troškove povezivanja.

### Telefonski imenik

Ako izaberete unos u direktorijumu na traci menija SMALLCOM, ništa se neće desiti jer je ta funkcija za sada isključena sa NULL u pokazivaču funkcije kuke. Sledećeg meseca ćemo dodati telefonski imenik. Omogućava dodavanje, menjanje i brisanje unosa i odabir unosa kao trenutne za biranje. Dok ne dobijemo tu funkciju, morate čvrsto kodirati broj telefona u PHONENO string u februarskom modemu.c.

### Diskusije sa čitaocima

Neki čitaoci pronađu vremena da mi pišu u časopisu ili ostave poruke za mene na CompuServe-u. (Moj CIS ID je 71101,1262.) Kada pitanje ili komentar čitaoca pokrene neko pitanje ili izazove misao koja bi mogla da zainteresuje druge, ja ću to adresirati ovde.

Čitalac je pitao zašto ponovo izmišljam točak. Zašto još jedan paket prozora, drugi paket pomoći, drugi menadžer menija, drugi uređivač, drugi program za komunikaciju? Zašto išta od ovoga? Moj odgovor mu je bio da je poenta projekta kolumne „Programiranje C“ prvo da vam, čitaocima, pruži kolekciju alata jezika C koje možete da koristite u svojim aplikacijama, a drugo da pokažem primerom kako su ovi alati programirani u C-u. Ne pokušavam da zamenim programe koje možda već koristite, programe koji rade sve i više od onoga što ovaj softver radi. Ako je sve što želite je ono što Procomm radi, trebalo bi da dobijete Procomm. To je dobar program i košta mnogo manje od vremena koje ćete posvetiti radu i učenju softverskih alata u ovoj koloni. Ako, s druge strane, želite da naučite kako se razvijaju programi poput Procomm-a i da u isto vreme prikupite alate koji ulaze u takav razvoj, onda ste na pravom mestu. Takve lekcije i kolekcije softverskih alata su okosnica ove kolumne i u skladu su sa 12-godišnjim nasleđem DDJ-a.

Jedan čitalac je pametno spojio jedan od mojih krošea sa jednim od mojih programa i predložio da vežbam ono što propovedam. Pošto nisam ni propovednik ni učitelj, praktikujem ono što praktikujem i nudim te prakse kao primere stvari koje bi mogle biti od koristi programerima. Ponekad se moje prakse programiranja udaljavaju od disciplina koje su udaljeni gurui smatrali ispravnim programskim navikama. Ponekad prekršim svoja pravila da bih obavio posao. Ne vezujem se tako dobro za dogmatizam kao za pragmatizam.

### LISTING 1

```c
/* ------ smallcom.c ---------- */
#include <conio.h>
#include <stdio.h>
#include <mem.h>
#include <string.h>
#include <ctype.h>
#include <dos.h>
#include <stdlib.h>
#include "window.h"
#include "editor.h"
#include "menu.h"
#include "entry.h"
#include "serial.h"
#include "modem.h"
#include "help.h"

#define ANSWERTIMEOUT 60
#define MAXSTRINGS 15
#define carrier() (inp(MODEMSTATUS) & 0x80)
#define LOGFILE  "smallcom.log"
#define HELPFILE "smallcom.hlp"
#define CFGFILE  "smallcom.cfg"
#define ALT_P 153
#define ALT_C 174
#define CTRL_C 3
#define WILDCARD '?'
static union REGS rg;
static FILE *logfp, *uploadfp, *downloadfp, *cfg;
static int running=1,connected,answering,savebaud;
int filecount;
extern int direct_connection, TIMEOUT, inserting;
extern char spaces[];
extern struct wn wkw;
extern MENU *mn;
/* ---------- prototypes ----------- */
void fileedit(char *);
static void displaycount(void);
static void smallmenu(int);
static void logserial(int);
static int upload(int, int);
static int download(int, int);
static int call(int, int);
static int directory(int, int);
static int comeditor(int, int);
static answer(int, int);
static directcon(int, int);
static int loginput(int, int);
static int hangup(int, int);
static int quit(int, int);
static int prm(int, int);
static void loadp(void);
static int savep(int, int);
static void set_parameters(void);
static int get_filename(char *);
static void notice(char *);
static void statusline(void);
static void putch_window(int);
void upload_ASCII(FILE *);
void download_ASCII(FILE *);
int keyhit(void);
char *prompt_line(char *, int, char *);
void reset_prompt(char *, int);
static int testcarrier(void);
static int waitforconnect(void);
static void initcom(void);
int waitforresult(void);
int waitforstring(char **, int, int);
static void waitforcall(void);
static void resetline(void);
/* ------- the hook to the phone directory ---------- */
static void (*phone_directory)(void) = NULL;
/* ------- the hook to script processors ---------- */
void (*script_processor)(void); /* filled in by directory */
/* ------- hooks to file transfer protocols --------- */
static int (*select_transfer_protocol)(void) = NULL;
/* ----- up to five upload function pointers ----- */
static void (*up_protocol[5])(FILE *file_pointer) = {
    upload_ASCII, NULL, NULL, NULL, NULL
};
/* ----- up to five download function pointers ----- */
static void (*down_protocol[5])(FILE *file_pointer) = {
    download_ASCII, NULL, NULL, NULL, NULL
};
/* --------- Files menu ------------ */
static char *fselcs[] = {
    "Log Input On/Off",
    "Upload a File",
    "Download a File",
    "Quit",
    NULL
};
static char *fhelps[] = {"log","upload","download","quitcom"};
/* ----------- Connect menu -------------- */
static char *cselcs[] = {
    "Place Call",
    "Answer Call",
    "Hang up",
    "Direct Connection",
    NULL
};
static char *chelps[] = {"call","answer","hangup","direct"};
/* ---------- Parameters menu --------------- */
static char *pselcs[] = {
    "Com Port:                       ",
    "Baud Rate:                      ",
    "Data Bits:                      ",
    "Stop Bit(s):                    ",
    "Parity:                         ",
    "Mode of Dialing:                ",
    "Write Parameters",
    NULL
};
static char *phelps[] = {"port","baud","wordlen","stopbits",
                         "parity","dialmode","writecfg"};
/* ---------- menu selection function tables ----------- */
static int (*ffuncs[])() = {loginput,upload,download,quit};
static int (*cfuncs[])() = {call,answer,hangup,directcon};
static int (*pfuncs[])() = {prm,prm,prm,prm,prm,prm,savep};
static int (*efuncs[])() = {comeditor};
static int (*dfuncs[])() = {directory};
/* ------ horizontal prompt messages ---------- */
char fdesc[]="Message File Operations";
char cdesc[]="Connections to Remote Processor";
char pdesc[]="Set Communications Parameters for Program Start";
char edesc[]="Edit a Text File";
char ddesc[]="The SMALLCOM Telephone Directory";
/* ------- horizontal menu bar ----------- */
static MENU cmn [] = {
   {"File",       fdesc, fselcs, fhelps, "ludq",    ffuncs, 0},
   {"Connect",    cdesc, cselcs, chelps, "pahd",    cfuncs, 0},
   {"Parameters", pdesc, pselcs, phelps, "cbdspmw", pfuncs, 0},
   {"Editor",     edesc, NULL,   NULL,   "e",       efuncs, 0},
   {"Directory",  ddesc, NULL,   NULL,   "d",       dfuncs, 0},
   {NULL}
};
/* ------ filename data entry template and buffer ------- */
static char filename[65], filemask[65];
static FIELD fn_template[] = {
    {2,14,1,filename,filemask,NULL},
    {0}
};
/* ------ modem result codes ------- */
static char *results[] = {
    "\r\nOK\r\n",
    "\r\nCONNECT\r\n",
    "\r\nRING\r\n",
    "\r\nNO CARRIER\r\n",
    "\r\nERROR\r\n",
    "\r\nCONNECT 1200\r\n",
    "\r\nNO DIALTONE\r\n",
    "\r\nBUSY\r\n",
    "\r\nNO ANSWER\r\n",
    "\r\n\r\n",
    "\r\nCONNECT 2400\r\n",
    NULL
};
extern int COMPORT,PARITY,STOPBITS,WORDLEN,BAUD;
extern char DIAL[], PHONENO[];
/* ================ MAIN ================== */
void main(void)
{
    int c;
    char *mb;
    inserting = FALSE;
    load_help(HELPFILE);
    loadp();
    savebaud = BAUD;
    set_parameters();
    clear_screen();
    mb = display_menubar(cmn);
    establish_window(1,2,80,24,TEXTFG,TEXTBG,TRUE);
    statusline();
    initcom();
    gotoxy(2,2);
    while (running) {
        set_help("smallcom");
        testcarrier();
        if (keyhit())   {
            switch (c = getkey())  {
                case F10:   smallmenu(0); break;
                case ALT_F: smallmenu(1); break;
                case ALT_C: smallmenu(2); break;
                case ALT_P: smallmenu(3); break;
                case ALT_E: smallmenu(4); break;
                case ALT_D: smallmenu(5); break;
                case CTRL_C:clear_window();
                            wkw.wx = wkw.wy = 0;
                            gotoxy(2,2);
                            break;
                case ESC:   quit(1,1);
                            break;
                default:    if (!(c & 0x80) && connected)    {
                                if (answering
                                        || direct_connection)
                                    logserial(c=='\r'?'\n':c);
                                writecomm(c);
                                if (c == '\r')
                                    writecomm('\n');
                            }
                            break;
            }
        }
        if (input_char_ready()) {
            logserial(c = readcomm());
            if (answering)
                writecomm(c);
        }
    }
    if (connected)
        hangup(1,1);
    release_modem();
    restore_menubar(mb);
    delete_window();
    clear_screen();
}
/* ---------- execute the SMALLCOM menu --------- */
static void smallmenu(int n)
{
    window(1,25,80,25);
    gotoxy(1,1);
    cprintf(spaces);
    putch(' ');
    current_window();
    menu_select(cmn, n);
    set_parameters();
    statusline();
    gotoxy(wkw.wx+2, wkw.wy+2);
}
/* ------ Call menu command ------ */
static int call(hs, vs)
{
    if (!connected) {
        notice("Dialing");
        placecall();
        sleep(4);
        delete_window();
        if ((connected = waitforconnect()) == FALSE)    {
            statusline();
            initmodem();
        }
        else if (script_processor)
            (*script_processor)();
    }
    return TRUE;
}
/* --------- Direct Connection menu command --------- */
static int directcon(hs, vs)
{
    direct_connection ^= 1;
    connected |= direct_connection;
    return TRUE;
}
/* ------- Hangup menu command ------- */
static int hangup(hs, vs)
{
    if (connected)  {
        notice("Hanging up");
        resetline();
        delete_window();
    }
    return TRUE;
}
/* --------- Quit menu command --------- */
static int quit(hs, vs)
{
    int c = 0;
    notice("Exit to DOS?  ");
    c = getkey();
    delete_window();
    running = (tolower(c) != 'y');
    return TRUE;
}
/* -------- Log Input menu command -------- */
static int loginput(hs, vs)
{
    if (logfp == NULL)
        logfp = fopen(LOGFILE, "ab");
    else    {
        fclose(logfp);
        logfp = NULL;
    }
    return TRUE;
}
/* ---------- Upload file menu command ---------- */
static int upload(hs, vs)
{
    int pr = 0;
    if (!connected) {
        error_message("Not connected");
        return FALSE;
    }
    if (uploadfp == NULL)   {
        setmem(filename, sizeof filename - 1, ' ');
        setmem(filemask, sizeof filemask - 1, '_');
        if (get_filename(" Upload what file? ") != ESC) {
            if ((uploadfp = fopen(filename, "rb")) == NULL)
                error_message("Cannot open file");
            else    {
                statusline();
                if (select_transfer_protocol)
                    pr = (*select_transfer_protocol)();
                (*up_protocol[pr])(uploadfp);
                fclose(uploadfp);
                uploadfp = NULL;
            }
        }
    }
    return TRUE;
}
/* ------ upload a file with ASCII transfer protocol ----- */
void upload_ASCII(FILE *fp)
{
    int c;
    while ((c = fgetc(fp)) != EOF)  {
        writecomm(c);
        displaycount();
        if (input_char_ready())
            logserial(readcomm());
        if (keyhit())
            if (getch() == ESC)
                break;
        if (!testcarrier())
            break;
    }
    filecount = 0;
    if (connected)
        writecomm(EOF);
}
/* ---------- Download file menu command ---------- */
static int download(hs, vs)
{
    int pr = 0, save_timeout;
    if (!connected) {
        error_message("Not connected");
        return FALSE;
    }
    setmem(filename, sizeof filename - 1, ' ');
    setmem(filemask, sizeof filemask - 1, '_');
    if (get_filename(" Download what file? ") != ESC) {
        downloadfp = fopen(filename, "wb");
        statusline();
        if (select_transfer_protocol)
            pr = (*select_transfer_protocol)();
        save_timeout = TIMEOUT;
        TIMEOUT = 60;
        (*down_protocol[pr])(downloadfp);
        TIMEOUT = save_timeout;
        fclose(downloadfp);
        downloadfp = NULL;
    }
    return TRUE;
}
/* ----- download a file with ASCII transfer protocol ----- */
void download_ASCII(FILE *fp)
{
    int c = 0;
    while (TRUE)    {
        if (keyhit())   {
            if ((c = getkey()) == ESC)
                break;
            writecomm(c);
            if (!answering)
                logserial(readcomm());
        }
        c = readcomm() & 127;
        if (c == 0 || c == 0x7f)
            break;
        fputc(c, fp);
        displaycount();
        if (!testcarrier())
            break;
    }
}
/* --- echo modem input and write to the log if selected --- */
static void logserial(int c)
{
    putch_window(c);
    if (logfp)
        fputc(c, logfp);
}
/* -------- read a file name ------------- */
static int get_filename(char *ttl)
{
    int rtn;
    establish_window(1,23,80,25,ENTRYFG,ENTRYBG,TRUE);
    window_title(ttl);
    gotoxy(3,2);
    cputs("File name:");
    rtn = data_entry(fn_template, TRUE, 1);
    delete_window();
    return rtn;
}
/* -------- small message ------------ */
static void notice(char *s)
{
    int lf = (80-strlen(s))/2-1;
    int rt = lf+strlen(s)+2;
    establish_window(lf,11,rt,13,HELPFG,HELPBG,TRUE);
    gotoxy(2,2);
    cputs(s);
}
/* ---- comm and modem parameter menu commands ----- */
static int prm(hs, vs)
{
    switch (vs) {
        case 1: COMPORT ^= 3;       /* flip between 1 and 2 */
                break;
        case 2: BAUD *= 2;          /* 110,150,300,  */
                if (BAUD == 220)    /* 600,1200,2400 */
                    BAUD = 150;
                if (BAUD == 4800)
                    BAUD = 110;
                break;
        case 3: WORDLEN ^= 0xf;     /* flip between 7 and 8 */
                break;
        case 4: STOPBITS ^= 3;      /* flip between 1 and 2 */
                break;
        case 5: if (++PARITY == 3)  /* 0, 1, 2              */
                    PARITY = 0;
                break;
        case 6: DIAL[3] = DIAL[3] == 'T' ? 'P' : 'T';
                break;
        default:
                break;
    }
    set_parameters();
    return FALSE;
}
/* ------ post the parameters into the menu display ------- */
static void set_parameters(void)
{
    static char *pars[] = {"None", " Odd", "Even"};
    static char *mode[] = {"Pulse", " Tone"};
    pselcs[0][strlen(pselcs[0])-1] = '0' + COMPORT;
    sprintf(&pselcs[1][strlen(pselcs[1])-4],"%4d",BAUD);
    pselcs[2][strlen(pselcs[2])-1] = '0' + WORDLEN;
    pselcs[3][strlen(pselcs[3])-1] = '0' + STOPBITS;
    sprintf(&pselcs[4][strlen(pselcs[4])-4],"%s",pars[PARITY]);
    sprintf(&pselcs[5][strlen(pselcs[5])-5],"%s",
        mode[DIAL[3]=='T']);
}
/* ------- load the configuration file ---------- */
static void loadp(void)
{
    if ((cfg = fopen(CFGFILE, "r")) != NULL)    {
        fscanf(cfg,"%d %d %d %d %d %c %s",
        &COMPORT,&PARITY,&STOPBITS,&WORDLEN,&BAUD,&DIAL[3],
            &PHONENO[0]);
        fclose(cfg);
    }
}
/* ---------- Write Parameters menu command ---------- */
static int savep(hs, vs)
{
    cfg = fopen(CFGFILE, "w");
    fprintf(cfg, "%d %d %d %d %d %c %s",
        COMPORT,PARITY,STOPBITS,WORDLEN,BAUD,DIAL[3],PHONENO);
    fclose(cfg);
    initcom();
    return FALSE;
}
/* --------- Editor menu command --------------- */
static int comeditor(hs, vs)
{
    extern int MAXLINES, inserting;
    MAXLINES = 800;
    mn = NULL;
    fileedit("");
    inserting = FALSE;
    insert_line();
    return TRUE;
}
/* --------- Directory menu command --------------- */
static int directory(hs, vs)
{
    if (phone_directory)    {
        mn = NULL;
        (*phone_directory)();
    }
    return TRUE;
}
/* ----------- display a status line ----------- */
static void statusline(void)
{
    char stat[81];
    static char *st = NULL;
    sprintf(stat,
        " %s Line %s %s %s %-12.12s       %-14.14s F10:Menu",
        (connected            ? " On"         : "Off"),
        (direct_connection    ? "Direct"      : "      "),
        (logfp                ? "Logging"     : "       "),
        ((answering & !connected)
                              ? "Answering  " :
         uploadfp             ? "Uploading  " :
         downloadfp           ? "Downloading" : "           "),
        (uploadfp||downloadfp ? filename      : " "),
         *PHONENO ? PHONENO : "No Phone #");
    st = prompt_line(stat, 25, st);
}
/* ------- write the file count into the status line ------- */
static void displaycount(void)
{
    filecount++;
    if ((filecount % 10) == 0)  {
        window(1,25,80,25);
        textcolor(MENUFG);
        textbackground(MENUBG);
        gotoxy(50,1);
        cprintf("%5d", filecount);
        current_window();
        gotoxy(wkw.wx+2, wkw.wy+2);
    }
}
/* ----- write a one-liner prompt saving video memory ----- */
char *prompt_line(char *s, int y, char *t)
{
    if (t == NULL)
        if ((t = malloc(160)) != NULL)
            gettext(1,y,80,y,t);
    window(1,y,80,y);
    textcolor(MENUFG);
    textbackground(MENUBG);
    gotoxy(1,1);
    cprintf(spaces);
    putch(' ');
    gotoxy(1,1);
    cprintf(s);
    current_window();
    return t;
}
/* ------- reset the one-liner prompt line --------- */
void reset_prompt(char *s, int y)
{
    puttext(1,y,80,y,s);
    free(s);
}
/* -------- write a character to the user's window -------- */
static void putch_window(int c)
{
    gotoxy(wkw.wx+2, wkw.wy+2);
    switch (c)  {
        case '\t':  while (wkw.wx % 4)
                        putch_window(' ');
                    break;
        case '\b':  if (wkw.wx)
                        --wkw.wx;
                    break;
        default:    putch(c);
                    wkw.wx++;
                    if (wkw.wx < wkw.wd-2)
                        break;
        case '\n':  if (wkw.wy < wkw.ht-1)
                        wkw.wy++;
                    else    {
                        scroll_window(1);
                        writeline(2, wkw.wy+2, spaces+1);
                    }
        case '\r':  wkw.wx = 0;
                    break;
    }
    gotoxy(wkw.wx+2, wkw.wy+2);
}
/* ------------ wait for a call ------------ */
static void waitforcall(void)
{
    answercall();
    if ((connected = answering = waitforconnect()) == FALSE) {
        statusline();
        initmodem();
    }
}
/* ---- wait for a line connection, reset baud rate ---- */
static int waitforconnect(void)
{
    extern int BAUD;
    int baud = 0;
    while (baud == 0)
        switch (waitforresult())    {
            case 1:     baud = 300;  break; /* CONNECT       */
            case 5:     baud = 1200; break; /* CONNECT 1200  */
            case 10:    baud = 2400; break; /* CONNECT 2400  */
            case 0:                         /* OK            */
            case 2:     break;              /* RING          */
            case 3:                         /* NO CARRIER    */
            case 4:                         /* ERROR         */
            case 7:                         /* BUSY          */
            case 8:                         /* NO ANSWER     */
            case -1:    baud = -1;   break; /* time-out      */
            default:    break;              /* anything else */
        }
    if (baud != -1 && baud != BAUD) {
        savebaud = BAUD;
        BAUD = baud;
        initcomport();
    }
    return (baud != -1);
}
/* ---- wait for a modem result (0-10). -1 if timed out ---- */
int waitforresult(void)
{
    return waitforstring(results, ANSWERTIMEOUT, 0);
}
/* --------- wait for a string from the serial port -------- */
int waitforstring(char *tbl[], int wait, int wildcard)
{
    int c, i, done = FALSE;
    char *sr[MAXSTRINGS];
    for (i = 0; tbl[i] != NULL; i++)
        sr[i] = tbl[i];
    while (!done)   {
        set_timer(wait);
        while (!input_char_ready()) {
            if (timed_out())
                return -1;
            if (keyhit())
                if ((c = getkey()) == ESC)
                    return -1;
        }
        logserial(c = readcomm());
        for (i = 0; tbl[i] != NULL; i++)    {
            if (c==*(sr[i]) ||
                   (wildcard && *(sr[i])==wildcard))   {
                if (*(++(sr[i])) == '\0')   {
                    done = TRUE;
                    break;
                }
            }
            else
                sr[i] = tbl[i];
        }
    }
    return i;
}
/* ----- initialize from serial and modem parameters ----- */
static void initcom(void)
{
    notice("Initializing Modem");
    initmodem();
    delete_window();
}
/* ----- test carrier detect -------- */
static int testcarrier(void)
{
    if (!direct_connection && connected && carrier() == FALSE)
        resetline();
    return connected;
}
/* ------ disconnect and reestablish the serial port ------ */
static void resetline(void)
{
    answering = connected = FALSE;
    statusline();
    disconnect();
    BAUD = savebaud;
    initcomport();
}
/* --------- answer a call ----------- */
static int answer(hs, vs)
{
    answering = 1;
    statusline();
    gotoxy(wkw.wx+2, wkw.wy+2);
    waitforcall();
    return TRUE;
}
#if COMPILER==TURBOC
/* --------- use bios to test for a keystroke -------- */
int keyhit(void)
{
    rg.h.ah = 1;
    int86(0x16, &rg, &rg);
    return ((rg.x.flags & 0x40) == 0);
}
#endif
```

### LISTING 2

```c
smallcom (serial.h,modem.h,editor.h,window.h,menu.h,entry.h,help.h)
editshel (editor.h, menu.h, entry.h, help.h, window.h)
editor (editor.h, window.h)
help (help.h, window.h)
modem (serial.h, modem.h)
serial (serial.h)
entry (entry.h, window.h)
menu (menu.h, window.h)
window (window.h)
```

### LISTING 3

```c
#
#  SMALLCOM.MAK: make file for SMALLCOM.EXE with Microsoft C/MASM
#

.c.obj:
    cl /DMSOFT=1 /DTURBOC=2 /DCOMPILER=MSOFT -c -W3 -Gs -AC $*.c

smallcom.obj : smallcom.c serial.h modem.h menu.h entry.h \
                help.h window.h

modem.obj : modem.c serial.h modem.h

serial.obj : serial.c serial.h

entry.obj : entry.c entry.h window.h

menu.obj : menu.c menu.h window.h

help.obj : help.c help.h window.h

editshel.obj : editshel.c editor.h menu.h entry.h help.h \
                window.h

editor.obj : editor.c editor.h window.h

window.obj : window.c window.h

microsft.obj : microsft.c

vpeek.obj : vpeek.asm
    masm /MX vpeek;

keyhit.obj : keyhit.asm
    masm /MX keyhit;

smallcom.exe : smallcom.obj modem.obj serial.obj editor.obj \
              editshel.obj entry.obj menu.obj help.obj \
              window.obj keyhit.obj vpeek.obj microsft.obj
    link @smallcom.lnk
```

### LISTING 4

```c
smallcom+
modem+
serial+
entry+
menu+
editor+
editshel+
help+
window+
vpeek+
keyhit+
microsft
smallcom
nul
\lib\clibce
```
