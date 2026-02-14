
# C programiranje 07

## TINYCOMM: Maleni program serijskih komunikacija

Al Stevens, februar '89

Ovomesečna kolumna govori o asinhronoj serijskoj komunikaciji i kako se ona uklapa u projekat kolone „C programiranje“. Razvićemo prvi u nizu alata dizajniranih za povezivanje jednog računara sa drugim sa modemima i telefonskim sistemom. Da bismo ilustrovali ove alate, napravićemo TINICOMM, mali komunikacioni program sa mogućnošću upućivanja ili odgovaranja na pozive sa drugog računara, interaktivnog razgovora sa tastaturom i ekranom, snimanja poruka u datoteku evidencije na disku i otpremanja datoteka poruka.

Kao što decembarski TWRP, mali procesor reči, nije konkurencija vrhunskim procesorima teksta, TINICOMM nije pretnja za Smartcom, Procomm ili Kmodem. Naš projekat kolumne „C programiranje“ polako prikuplja kolekciju malih, ali korisnih alata koje ćemo na kraju integrisati u program za određenu svrhu. U međuvremenu možemo naučiti iz ovih malih primera, videći kako se jezik C primenjuje u izgradnji takvih aplikacija.

Nije vam potrebno detaljno razumevanje serijskog ulaza/izlaza i modema da biste koristili kod ovog meseca, ali bi vam moglo pomoći. Preporučujem "Mastering Serial Communications" od Petera V. Goftona, 1986, Sibek Inc. Knjiga govori o složenosti serijskih I/O, i to je istinska briga. Ovde ćete, međutim, naučiti da osnovni koraci za uspostavljanje modemske veze sa udaljenim računarom uključuju minimum C koda.

## Komunikacije

Kada ljudi razgovaraju, koristimo kanale komunikacije koji odgovaraju razgovorima i našoj blizini jedni drugima. Najčešći kanal je izgovorena reč koja se izdaje na dohvat ruke učesnika. Manje uobičajeni su kolačići sreće, osuđenici koji zveckaju po vodovodnim cevima i flaširane poruke koje plutaju okeanom, ali bez obzira na medij i poruku, neki su prikladniji od drugih.

Ljudi mogu da izaberu pogodan medij kako vreme i prilike dozvoljavaju, ali kada kompjuteri razgovaraju, medijum mora biti tačno usklađen sa porukom. Učesnik u elektronskom razgovoru mora da koristi medij poznat, prijatan i dostupan ostalim učesnicima.

Stoga, ako želite da pošaljete poruku svojoj majci, a ona ima računar, modem i komunikacioni softver, vaš računar može da pozove njen računar. Ako se njeni mogu javiti na telefon i primiti poruku, na kraju će moći da je pročita. To može, ali ne mora da zahteva učešće jednog ili oboje u vreme prenosa. Verovatno bi više volela da pozove.

### Komunikacioni hardver

Programi koji komuniciraju sa udaljenim računarima moraju da upravljaju sa dva sloja hardvera: modemom i serijskim portom. Lokalni modem prevodi podatke lokalnog programa u tonove koji se šalju preko telefonskih linija do udaljenog modema. Lokalni modem zatim prima tonove udaljenog modema i prevodi ih u podatke. Računar šalje i prima vrednosti podataka do i od modema preko serijskog porta. Upravljanje ovom komunikacijom zahteva funkcije za serijski port i modem. Koji način ćete izabrati da to uradite zavisi od samog hardvera. Koristićemo IBM PC serijske portove i Haies-kompatibilne modeme.

### Asinhroni serijski port

Serijski port je uređaj za protok bitova koji se koristi u aplikacijama gde osam linija podataka nije dostupno i/ili rastojanja između uređaja onemogućavaju korišćenje niže jačine signala računara.

Asinhroni serijski port dodaje početni bit i jedan ili dva stop bita svakom bajtu. Možda postoji paritetni bit koji može koristiti neparan ili parni paritet. Bajt se može sastojati od pet do osam bitova podataka. Brzina prenosa se izražava u bitovima u sekundi i naziva se "brzina prenosa".Pošiljaoci i primaoci asinhronih serijskih poruka moraju se dogovoriti o broju bitova podataka; broj stop bitova; da li postoji neparan, paran ili nikakav paritet; i brzinu prenosa.

IBM PC može imati jedan ili dva serijska porta. Program može komunicirati sa jednim od ovih serijskih portova čitanjem i pisanjem portova direktno, pozivanjem funkcija serijskog I/O ROM BIOS-a ili korišćenjem DOS-a. Svaki pristup ima prednosti i nedostatke. Ranije pomenuta knjiga objašnjava ove kompromise; prostor me sprečava da vam ponudim sveobuhvatan tretman koji subjekt zahteva. Za naše svrhe, možete pretpostaviti da sam odmerio prednosti i napravio najbolji izbor za primere koji su pri ruci. Koristićemo direktno adresiranje portova i rutinu usluge prekida za stranu prijemnika serijskih prenosa.

Ove funkcije koriste XON, XOFF protokol gde računar koji prima efektivno uključuje i isključuje prenose pošiljaoca. Kada se njegov ulazni bafer skoro napuni, primalac šalje KSOFF karakter pošiljaocu. Pošiljalac obustavlja prenos dok primalac ne pošalje KSON karakter. Prijemnik posmatra svoj bafer kako se znakovi uklanjaju i šalje KSOFF kada je nivo ispod sigurnosne granice.

Protokoli za prenos datoteka kao što su KSModem i Kermit koriste sopstvene tehnike upravljanja paketima za rešavanje takvih kašnjenja. KSModem može biti problem u mrežama jer specifikacija KSModem protokola uključuje fiksne periode vremenskog ograničenja koji mogu biti prekoračeni latentnim kašnjenjima koja uvodi mreža. Lokalni komunikacioni softver mora znati kada se koristi jedan od ovih protokola, jer se oni često koriste za prenos binarnih datoteka -- datoteka koje mogu imati KSON ili KSOFF karaktere kao validne bajtove podataka. Tokom ovih prenosa, KSON/KSOFF protokol je onemogućen.

[Listing 1](#listing-1) je "serial.h". Ova datoteka zaglavlja deklarira prototipove i makroe za funkcije serijskog porta. Takođe definiše signale statusnog registra, vektor prekida tajmera i neke kontrolne parametre za naš program. Makro comstat čita status serijskog porta. Makro "input_char_read"  vraća tačnu vrednost ako postoji bajt koji čeka u baferu serijskog ulaza i lažnu vrednost ako ne.

[Listing 2](#listing-2) je "serial.c". Ove funkcije upravljaju inicijalizacijom, ulazom i izlazom serijskog porta. Funkcija "initcomport" inicijalizuje komunikacioni port koristeći strukturu pod nazivom "initcom" koja sadrži parametre inicijalizacije koji su upisani u 8250 univerzalni asinhroni prijemnik/predajnik (UART). Celi brojevi pod nazivom COMPORT, PARITI, STOPBITS, WORDLEN i BAUD se inicijalizuju sa podrazumevanim vrednostima koje preuzimaju funkcije.

Funkcija "initcomport" postavlja proces prekidanja serijskog prijemnika. U ovom procesu, svi serijski ulazni karakteri se čitaju pomoću rutine usluge prekida pod nazivom "newcomint", koja prikuplja znakove dok se čitaju u kružni bafer. Koraci inicijalizacije i operacije serijskog ulaza i izlaza koriste definicije izvedene iz broja porta za COM1 ili COM2. Te definicije su u serijski.h. (Možete modifikovati kod za rad sa drugim mašinama koje koriste 8250 UART i 8259 Programabilni kontroler prekida tako što ćete promeniti adresu baznog porta definisanu u serial.h kao BASEPORT i liniju zahteva za prekid pod nazivom IRK, takođe u serial.h.) Prvo se čita i čuva trenutni sadržaj vektora serijskog prekida. Zatim se vektor inicijalizuje adresom nevcomint-a. 8250 UART ima dva registra koja moraju biti inicijalizovana. Program potvrđuje signale o spremnosti terminala za podatke (DTR), zahtevu za slanje (RTS) i korisnički definisanom izlazu 2 (OUT2) u registru kontrole modema, i upisuje registar za omogućavanje prekida sa signalom dostupnim podacima koji je postavljen da generiše prekide. Programabilni kontroler prekida 8259 je napisan da omogući odgovarajuću IRK liniju da izazove prekid. Zatim se kontroler prekida resetuje i svi UART ulazni registri se čitaju da bi se obrisali svi zalutali prekidi koji bi mogli biti okolo.

Funkciju "restore_serialint" u "serial.c" treba pozvati na kraju programa koji je koristio serijski ulaz/izlaz. Ova funkcija vraća vektor serijskog prekida na njegovu originalnu vrednost.

Funkcija "clear_serial_queue resetuje kružni bafer serijskog prijemnika na prazan uslov. Ovu funkciju koristi menadžer modema da bi se oslobodio svih nepotrebnih tekstualnih odgovora sa modema.

Rutina prekida usluge "newcomint" je za serijski unos. On resetuje kontroler prekida, čita serijski ulazni bajt i stavlja ga u kružni bafer osim ako karakter nije XSON ili XOFF sa omogućenim KSON/KSOFF protokolima. U ovom slučaju funkcija postavlja zastavicu tako da predajnik zna da obustavi ili nastavi prenos. Ako je XSON/XKSOFF omogućen i bafer je na ili iznad svog praga, nevcomint prenosi KSOFF karakter da kaže pošiljaocu da sačeka neko vreme.

"Readcomm" funkcija čeka da bajt bude dostupan u kružnom baferu prijemnika, a zatim ga izdvaja za funkciju koja poziva. Program koji traži serijski ulaz treba da pozove ovu funkciju tek nakon što dobije pravi povratak od makroa input_char_readi jer readcomm čeka da se primi znak. Kada readcomm vidi da je bafer na bezbednom nivou nakon što je "newcomint" preneo XOFF, "readcomm" prenosi XON karakter da kaže pošiljaocu da nastavi prenos.

Programi koji čitaju ASCII tekst sa udaljenog računara sa sedam bitova podataka i parnim ili neparnim paritetom trebalo bi logički I povratak iz "readcomm"-a sa 0x7f da uklone bit parnosti. Ako čitate binarni tok, kao što je arhivirana datoteka, trebalo bi da prihvatite punu osmobitnu vrednost.

Funkcija "writecomm" šalje bajt u svom parametru udaljenom računaru preko serijskog porta. Imajte na umu da "readcomm" i "writecomm" vraćaju lažnu vrednost ako port istekne kao rezultat isteka programirane vrednosti TIMEOUT bez primljenog ili upisanog znaka. TIMEOUT je definisan u "serial.c" i izražava se u sekundama.

Funkcije "timer" u "serial.c" pružaju još jedan primer upotrebe tipa funkcije prekida. Ove funkcije tajmera obrađuju vremenska ograničenja ili obustavljaju obradu na određeni broj sekundi. Svaki program koji će koristiti funkcije u "serial.c" mora prvo pozvati "intercept_timer" da bi se priključio vektoru prekida tajmera. Pre povratka u DOS, program mora pozvati "restore_timer" da resetuje vektor. Ako koristite funkcije modema u "modem.c", ne morate da upućujete ove pozive, jer funkcije modema to rade umesto vas.

"serial.h" ima dva makroa pod nazivom "set_timer" i "timed_out" koji upravljaju tajmerom. Prvi makro postavlja vrednost tajmera u sekundama. Drugi makro vraća tačnu vrednost ako je prošlo poslednje podešavanje. "set_timer" stavlja vrednost u promenljivu "tiker". Vrednost u "tiker"-u je približno 18,2 puta veći od broja sekundi; prekid tajmera se javlja 18,2 puta u sekundi. Prekid novog timera povezuje prekid; onda, ako je "tiker" veći od nule, "newtimer" ga smanjuje. Kada makro "timed_out" otkrije da "tiker" nije veći od nule, vraća tačnu vrednost.

"sleep" koristi tajmer da suspenduje program na određeni broj sekundi. Turbo C već ima upravo takvu funkciju u svojoj biblioteci, ali Microsoft C nema.

Imajte na umu da se Turbo C funkcije `getvect` i `setvect` koriste za čitanje i pisanje vektora prekida serijskih i tajmerskih. Ako koristite Microsoft C, oni se menjaju u `_dos_getvect` i `_dos_setvect`, MSC ekvivalente. Ako prenosite ovaj softver na kompajler koji ne podržava tip funkcije prekida, morate koristiti asemblerski jezik za
ulaz i izlaz u svaku od funkcija prekida. Knjiga koju sam ranije pomenuo ima primer ove tehnike.

Serijski port detektuje stanje prekida, grešku kadriranja, grešku pariteta i grešku prekoračenja, od kojih se bilo šta može dogoditi. Zašto ne proverimo i ne ispravimo ove uslove? Ne radimo iz dva razloga: Prvo, razmena ljudi čitljivih ASCII podataka pretpostavlja da korisnik može da kaže kada su znakovi koji se čitaju netačni. Možete vizuelno da nadoknadite ove takozvane „pogotke u liniji“ ili da prekinete prenos ako greške pređu podnošljiv prag. Drugo, druge razmene – otpremanje i preuzimanje datoteka sa XModemom, Kermitom, i dr. – imaju sopstvene protokole za ispravljanje grešaka koji uključuju kontrolne sume, vremenska ograničenja i ponovne pokušaje. Istražićemo ove tehnike u narednim mesecima.

### Modem

Modemi su poluinteligentni uređaji koji povezuju udaljene računare sa telefonskim linijama i koji se mogu programirati da rade na različite načine. Ovo programiranje podrazumeva jezik za modem - način da se podese režimi modema i pročita status modema. Prihvaćeni standard za ovaj jezik je Haiesov skup komandi.

Pošto modem podržava terminalske veze, on prepoznaje i odgovara jezikom koji ljudi razumeju. Pa, skoro. Haiesov skup komandi je teško ono što biste nazvali prirodnim jezikom, ali ga osoba može naučiti. Čudno, modem govori uverljiviji jezik nego što razume. Kažete mu ATEOM1V1S0 = 0 i on odgovara OK. Kažete ATDT 17033710188 i piše ZAUZET, POVEŽI SE, NEMA ODGOVORA ili NEMA NOSILjAČA. Možete naučiti njegov jezik ako želite, ali svaki komunikacioni program vredan soli će znati jezik i sakriti ga od vas.

[Listing 3](#listing-3) je "modem.h". Ima konfiguracione parametre i prototipove za funkcije modema. Nizovi parametara inicijalizuju i resetuju modem, biraju broj i odgovaraju na dolazni poziv. U nekim komunikacionim programima, ovi parametri se održavaju u konfiguracionoj datoteci koju pravi program za podešavanje. Samo ćemo ih tvrdo kodirati na ovaj način. Kovrdžavi znakovi u parametrima RESETMODEM, INITMODEM i HANG UP nisu deo Haiesovog skupa komandi. Umesto toga, oni govore funkciji modout-a da sačeka jednu sekundu dok šalje komandu modemu.

Prodavci koji prodaju takozvane Haies-kompatibilne modeme ne uspevaju uvek kako treba. Takođe, mnogi modemi imaju konfiguracione mikro prekidače koji postavljaju podrazumevane vrednosti. Vrednost niza parametra INITMODEM je ona koja radi sa internim modemom Toshiba T-1000 i US Robotics Courier 2400. Ovo je crna umetnost. Ako imate problema sa modemom sa ovim programom, možda ćete morati da se petljate sa parametrima, mikro prekidačima ili oboje.

[Listing 4](#listing-4), "modem.c" sadrži funkcije koje kontrolišu modem. Funkcija "initmodem" inicijalizuje serijski port i modem. Poziva "intercept_timer" da dozvoli programu da zakači vektor prekida tajmera. Programi treba da pozovu sledeću funkciju, "release_modem", pre nego što se završe. Vraća tajmer i vektore serijskih prekida i resetuje modem. Ako ne koristite ovu proceduru, vaš računar će otići u trsku i šiblje kada izađete u DOS. Funkcija za pozivanje mesta bira broj u parametru PHONENO. Funkcija odgovora na poziv priprema modem da automatski odgovori na telefon. Funkcija prekida veze isključuje modem sa telefonske linije. Funkciju modout koriste drugi da pošalju komandu modemu. Funkcija testira kovrdžavi karakter u komandnom nizu i modout poziva funkciju "sleep" u "serial.c" da kaže programu da sačeka jednu sekundu za svaki kovrdžavi karakter.

Program koji podržava direktnu vezu serijskih portova dva računara bez modema će postaviti promenljivu "direct_connection" na tačnu vrednost, što potiskuje komande modema. Ovaj režim je jedan od načina da se testira komunikacioni program kada dve telefonske linije nisu dostupne. Morate da povežete serijske portove pomoću "null modem" kabla, koji prelazi linije za slanje i prijem - obično pinove 2 i 3 - u konektorima kablova.

### TINYCOMM

Kod u "serial.h", "serial.c", "modem.h" i "modem.c" predstavlja primitivne funkcije potrebne za komunikaciju modema. Da biste koristili ove funkcije, potreban vam je komunikacioni program višeg nivoa. [Listing 5](#listing-5) je "tinicomm.c", goli počeci takvog programa. Njegova svrha je da demonstrira primenu komunikacionih funkcija, ali ima dobar deo funkcionalnosti upakovane u tako mali paket.

Kada pokrenete TINICOMM, možete odrediti serijski port -- 1 ili 2 -- i telefonski broj na komandnoj liniji kao što je prikazano ovde:

```sh
tinycomm 2 555-1212
```

Nakon ove komande, videćete ovaj meni.

```sh
     ------ TINYCOMM Menu ------
P-lace Call
A-nswer Call
H-ang Up
L-og Input [OFF]
S-end Message File
T-elephone Number (???-????)
E-xit to DOS

Enter Selection >
```

Izbori TINICOMM menija vam omogućavaju da uputite poziv, kažete TINICOMM-u da se pripremi da odgovori na poziv, prekine vezu, uključite i isključite saobraćaj za odjavljivanje diska, pošaljete ASCII datoteku na drugi kraj, reprogramirate telefonski broj i izađete u DOS.

Kada se uspostavi veza, TINICOMM šalje znakove koje unesete i prikazuje znakove koje primi. Ovi prikazi uključuju poruke modema, kao što su CONNECT i NO CARRIER. TINICOMM očekuje da pročitate ove poruke i reagujete na odgovarajući način. Ne postoji automatsko prepoznavanje bilo kakvog unosa teksta u TINICOMM. Ulazite i izlazite iz menija pritiskom na taster Esc.

TINICOMM-ove funkcije za poruke, otpremanje i evidenciju koriste pravi ASCII protokol sa omogućenim XON/XOFF. Ovo je prihvatljivo za saobraćaj poruka, ali uopšte ne bi funkcionisalo za prenos binarnih formata kao što su arhivirane ili izvršne datoteke. Protokoli za prenos datoteka će doći u sledećoj rati.

### Ctrl Break

Kada program preuzme vektor prekida, program se ne sme nenormalno prekinuti. Ako jeste, vektor prekida će i dalje pokazivati na memoriju koju je ranije zauzimao program. Sledeći put kada dođe do prekida, sistem će biti u krivu. TINICOMM spaja vektore tajmera i serijskih prekida, tako da ne smemo dozvoliti da se prekine drugačije osim preko normalne izlazne tačke programa gde se ovi vektori vraćaju.

Kada pritisnete Ctrl C ili Ctrl Break, DOS obično prikazuje ^C token i prekida program. Ovo bi bio jedan od onih neželjenih prekida koje sam upravo pomenuo. Možete preuzeti vektore prekida Ctrl Break i Ctrl C (0k1b i 0k23) i sprečiti završetak, ali glupi ^C token se i dalje prikazuje, kvari vaš dobro planirani prikaz na ekranu i pomera kursor.

Jedan od načina da se pobedi ova sumnjiva karakteristika DOS-a je izbegavanje korišćenja DOS-a za funkcije unosa sa tastature i izlaza na ekranu. TINICOMM koristi Turbo C getch and putch ili sopstvene funkcije tastature i ekrana za Microsoft C, i ove mere efikasno izbegavaju DOS.

TINICOMM ispituje tastaturu pre nego što pozove getcom i ispituje serijski bafer pre nego što pozove getcomm. Ova tehnika omogućava bilo kom uređaju da dobije karakter po ivici, što je neophodno u operaciji punog dupleksa komunikacije. Dva uobičajena načina za ispitivanje tastature su kbhit funkcija i bioskei (TC) ili _bios_kei (MSC) funkcija. Nažalost, ove funkcije uključuju logiku Ctrl Break i Ctrl C, tako da ih ne možemo koristiti. Umesto toga, moramo koristiti BIOS da vidimo da li je taster pritisnut. Oba kompajlera imaju funkciju int86 i ovu funkciju možemo koristiti da pozovemo prekid BIOS-a 0kl6, koji upravlja tastaturom. Funkcija keihit u tinicomm.c koristi int86 i problem je rešen - pa, ne baš. BIOS vraća svoje rezultate u nultom bitu registra zastavica procesora. Turbo C-ov int86 uključuje registar zastavice u REGS uniji koju je napisao int86; Microsoft C verzija ne. Kao posledica toga, Microsoft C ne pruža način na koji mogu da pronađem da ispitam tastaturu, a da se ne pojavi uvredljivi ^C i možda prekine program. Da bismo prevazišli ovu prepreku, koristimo funkciju asemblerskog jezika koja se nalazi na Listingu šest, keihit.asm (strana 140).

Funkcija keihit je jedina odbrambena mera Ctrl Break koja nam je potrebna sa Turbo C-om. Microsoft C nije tako lak. MSC funkcije getch, putch i gets omogućavaju operaciji breaka. Zbog toga se funkcije mscgetch, mscputch i mscgets dodaju verzijama tinicomm.c koje je kompajlirao MSC. Gotoki i clrscr funkcije su klonovi sličnih Turbo C funkcija.

[Listing 7](#listing-7), je "tinicomm.prj", datoteka za izradu Turbo C okruženja. Podesite opciju koju definiše kompajler (Alt-O/C/D) na ove parametre:

```c
MSOFT=1;TURBOC=2;COMPILER=TURBOC
```

[Listing 8](#listing-8) je "tinicomm.mak", make datoteka za Microsoft C. Koristi model male memorije.

Sledećeg meseca ćemo revidirati program TINICOMM da bismo koristili prozore, pomoć, menije i druge alate u našoj kolekciji. Dodaćemo funkcije za preuzimanje datoteka, programirati parametre serijskog porta iz menija, koristiti funkcije direktne veze menadžera modema, pristupiti telefonskom imeniku i održavati podešavanja programa u konfiguracionoj datoteci. Ubacićemo kuke da bismo dodali protokole za prenos datoteka (ali ne još protokole). Dok razvijam ovaj program, testiram ga tako što ga koristim za sve svoje aktivnosti na mreži, pa ako nestanem neke noći usred zagrejane onlajn razmene, znaćete zašto.

## LISTING 1

```c
/* ---------- serial.h ---------------
 * Serial Port Definitions
 */
extern int ticker, COMPORT;
extern char *nextin, *nextout;
/* ----------- serial prototypes ----------- */
void initcomport(void);
int readcomm(void);
int writecomm(int);
void clear_serial_queue(void);
/* -------- timer prototypes --------- */
void sleep(unsigned);
int set_timer(unsigned);
void intercept_timer(void);
void restore_timer(void);
void restore_serialint(void);
/* ----------------- macros ------------------- */
#define comstat() (inp(LINESTATUS))
#define input_char_ready() (nextin!=nextout)
#define timed_out() (ticker==0)
#define set_timer(secs) ticker=secs*182/10+1
#define XON  17
#define XOFF 19
/* ---------------- serial port addresses ----------------- */
/* - 8250 UART base port address:  COM1 = 3f8, COM2 = 2f8 - */
#define BASEPORT    (0x3f8-((COMPORT-1)<<8))
#define TXDATA       BASEPORT      /* transmit data         */
#define RXDATA       BASEPORT      /* receive data          */
#define DIVLSB       BASEPORT      /* baud rate divisor lsb */
#define DIVMSB      (BASEPORT+1)   /* baud rate divisor msb */
#define INTENABLE   (BASEPORT+1)   /* interrupt enable      */
#define INTIDENT    (BASEPORT+2)   /* interrupt ident'n     */
#define LINECTL     (BASEPORT+3)   /* line control          */
#define MODEMCTL    (BASEPORT+4)   /* modem control         */
#define LINESTATUS  (BASEPORT+5)   /* line status           */
#define MODEMSTATUS (BASEPORT+6)   /* modem status          */
/* --------------- serial interrupt stuff ------------------ */
#define IRQ     (4-(COMPORT-1))     /* 0-7 = IRQ0-IRQ7       */
#define COMINT  (12-(COMPORT-1))    /* interrupt vector 12/11*/
#define COMIRQ  (~(1 << IRQ))
#define PIC01   0x21 /*8259 Programmable Interrupt Controller*/
#define PIC00   0x20 /* "      "              "        "     */
#define EOI     0x20 /* End of Interrupt command             */
#define TIMER   0x1c /* PC timer interrupt vector            */
/* --------------- line status register values ------------- */
#define XMIT_DATA_READY    0x20
/* ------------ modem control register values -------------- */
#define DTR   1
#define RTS   2
#define OUT2  8
/* ----------- interrupt enable register signals ------------ */
#define DATAREADY 1
/* ------------- serial input interrupt buffer -------------- */
#define BUFSIZE 1024
#define SAFETYLEVEL (BUFSIZE/4)
#define THRESHOLD (SAFETYLEVEL*3)
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif
```

## LISTING 2

```c
/* ---------- serial.c ---------------
 * Serial Port Communications Functions
 */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include "serial.h"

#if COMPILER == MSOFT
#define getvect _dos_getvect
#define setvect _dos_setvect
#endif

char recvbuff[BUFSIZE];
char *nextin = recvbuff;
char *nextout = recvbuff;
int buffer_count;
int COMPORT  = 1;    /* COM1 or COM2                  */
int PARITY   = 0;    /* 0 = none, 1 = odd, 2 = even   */
int STOPBITS = 1;    /* 1 or 2                        */
int WORDLEN  = 8;    /* 7 or 8                        */
int BAUD     = 1200; /* 110,150,300,600,1200,2400     */
int TIMEOUT  = 10;   /* number of seconds to time out */
int xonxoff_enabled = TRUE;
static int waiting_for_XON;
static int waiting_to_send_XON;
int ticker;

/* ----- the com port initialization parameter byte ------ */
static union    {
    struct {
        unsigned wordlen  : 2;
        unsigned stopbits : 1;
        unsigned parity   : 3;
        unsigned brk      : 1;
        unsigned divlatch : 1;
    } initbits;
    char initchar;
} initcom;
static void (interrupt far *oldtimer)(void);
static void interrupt far newtimer(void);
static void (interrupt far *oldcomint)(void);
static void interrupt far newcomint(void);

/* -------- initialize the com port ----------- */
void initcomport(void)
{
    initcom.initbits.parity   = PARITY == 2 ? 3 : PARITY;
    initcom.initbits.stopbits = STOPBITS-1;
    initcom.initbits.wordlen  = WORDLEN-5;
    initcom.initbits.brk      = 0;
    initcom.initbits.divlatch = 1;
    outp(LINECTL, initcom.initchar);
    outp(DIVLSB, (char) ((115200L/BAUD) & 255));
    outp(DIVMSB, (char) ((115200L/BAUD) >> 8));
    initcom.initbits.divlatch = 0;
    outp(LINECTL, initcom.initchar);
/* ------ hook serial interrupt vector --------- */
    if (oldcomint == NULL)
        oldcomint = getvect(COMINT);
    setvect(COMINT, newcomint);
    outp(MODEMCTL, (inp(MODEMCTL) | DTR | RTS | OUT2));
    outp(PIC01, (inp(PIC01) & COMIRQ));
    outp(INTENABLE, DATAREADY);
    outp(PIC00, EOI);
/* ----- flush any old interrupts ------ */
    inp(RXDATA);
    inp(INTIDENT);
    inp(LINESTATUS);
    inp(MODEMSTATUS);
}

/* ------ restore the serial interrupt vector ---------- */
void restore_serialint(void)
{
    if (oldcomint)
        setvect(COMINT, oldcomint);
}

/* ------- clear the serial input buffer --------- */
void clear_serial_queue(void)
{
    nextin = nextout = recvbuff;
    buffer_count = 0;
}

/* ---- serial input interrupt service routine ------- */
static void interrupt far newcomint(void)
{
    int c;
    outp(PIC00,EOI);
    if (nextin == recvbuff+BUFSIZE)
        nextin = recvbuff;           /* circular buffer */
    c = inp(RXDATA);              /* read the input  */
    if (xonxoff_enabled)
        if (c == XOFF)               /* test XON        */
            waiting_for_XON = 1;
        else if (c == XON)           /* test XOFF       */
            waiting_for_XON = 0;
    if (!xonxoff_enabled || (c != XON && c != XOFF))    {
        *nextin++ = (char) c;        /* put char in buff*/
        buffer_count++;
    }
    if (xonxoff_enabled && !waiting_to_send_XON &&
            buffer_count > THRESHOLD)    {
        while ((inp(LINESTATUS) & XMIT_DATA_READY) == 0)
            ;
        outp(TXDATA, XOFF);          /* send XOFF        */
        waiting_to_send_XON = 1;
    }
}

/* ---- read a character from the input buffer ----- */
int readcomm(void)
{
    set_timer(TIMEOUT);
    while (!input_char_ready())
        if (timed_out())
            return FALSE;
    if (nextout == recvbuff+BUFSIZE)
        nextout = recvbuff;
    --buffer_count;
    if (waiting_to_send_XON && buffer_count < SAFETYLEVEL) {
        waiting_to_send_XON = 0;
        writecomm(XON);
    }
    return *nextout++;
}

/* ---- write a character to the comm port ----- */
int writecomm(int c)
{
    while (waiting_for_XON)
        ;
    set_timer(TIMEOUT);
    while ((inp(LINESTATUS) & XMIT_DATA_READY) == 0)
        if (timed_out())
            return FALSE;
    outp(TXDATA, c);
    return TRUE;
}

/* ---- intercept the timer interrupt vector ----- */
void intercept_timer(void)
{
    if (oldtimer == NULL)    {
        oldtimer = getvect(TIMER);
        setvect(TIMER, newtimer);
    }
}

/* ---------- sleep for n seconds ------------ */
void sleep(unsigned secs)
{
    set_timer(secs);
    while (!timed_out())
        ;
}

/* ---- restore timer interrupt vector ------- */
void restore_timer()
{
    if (oldtimer)
        setvect(TIMER, oldtimer);
}

/* ------ ISR to count timer ticks ------- */
static void interrupt far newtimer()
{
    (*oldtimer)();
    if (ticker)
        --ticker;
}
```

## LISTING 3

```c
/* -------- modem.h ------------
 * Modem Definitions
 */
/* -------- Hayes modem control strings --------- */
#define RESETMODEM "ATZ\r~"
#define INITMODEM  "ATE0M1S7=60S11=55V1X3S0=0\r~"
#define HANGUP     "~+++~ATH0\r~ATS0=0\r~"
#define ANSWER     "ATS0=1\r~"
/* --------- prototypes ---------- */
void initmodem(void);
void placecall(void);
void answercall(void);
void disconnect(void);
void release_modem(void);
```

## LISTING 4

```c
/* ------------ modem.c --------- */

#include <dos.h>
#include <conio.h>
#include "serial.h"
#include "modem.h"

char DIAL[] = "ATDT";
char PHONENO[21];

int direct_connection;    /* true if connected without a modem */

/* ----------- write a command to the modem ------------ */
static void modout(char *s)
{
    while(*s)    {
        if (*s == '~')
            sleep(1);
        else if (!writecomm(*s))
            break;
        s++;
    }
}

/* ----------- initialize the modem ---------- */
void initmodem(void)
{
    intercept_timer();
    initcomport();
    if (!direct_connection)    {
        modout(RESETMODEM);
        modout(INITMODEM);
    }
}

/* -------- release the modem --------- */
void release_modem(void)
{
    if (!direct_connection)
        modout(RESETMODEM);
    restore_timer();
    restore_serialint();
}

/* ----------- place a call -------------- */
void placecall(void)
{
    if (!direct_connection)    {
        modout(DIAL);
        modout(PHONENO);
        modout("\r");
        clear_serial_queue();
    }
}

/* ------------- answer a call ------------ */
void answercall(void)
{
    if (!direct_connection)    {
        modout(ANSWER);
        clear_serial_queue();
    }
}

/* ------------ disconnect the call ----------------- */
void disconnect(void)
{
    if (!direct_connection)    {
        modout(HANGUP);
        clear_serial_queue();
    }
}
```

## LISTING 5

```c
/* ------ tinycomm.c ---------- */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <conio.h>
#include <stdlib.h>
#include <dos.h>
#include "serial.h"
#include "modem.h"

#if COMPILER==MSOFT
#define getch mscgetch
#define putch mscputch
#define gets  mscgets
static void mscgets(char *);
static int mscgetch(void);
static void mscputch(int);
static void gotoxy(int,int);
static void clrscr(void);
#endif
int keyhit(void);

#define TMSG "\r\n\r\nTINYCOMM: << %s >>\r\n"
#define LOGFILE "tinycomm.log"
#define ESC   27
extern char PHONENO[];
extern int COMPORT;
static FILE *logfp;
static FILE *uploadfp;
static int running=1;
static int connected,answering;
static int forcekey, forcecom, forcemenu;
static union REGS rg;
/* ----- prototypes ------ */
static void tinymenu(void);
static void log(int);
static void upload(void);

void main(int argc, char *argv[])
{
    int c;
    if (argc > 1)
        COMPORT = atoi(argv[1]);
    if (argc > 2)
        strcpy(PHONENO, argv[2]);
    initmodem();
    while (running)    {
        if (!connected || forcemenu)    {
            forcemenu = 0;
            tinymenu();    /* display and process the menu */
        }
        /* ------ poll for a keystroke --------- */
        if (keyhit() || forcekey)    {
            c = forcekey ? forcekey : getch();
            forcekey = (answering && c == '\r') ? '\n' : 0;
            if (c == ESC)
                tinymenu();
            else if (connected)    {
                if (answering)
                    log(c);   /* answerer echos his own key */
                writecomm(c); /* transmit the keystroke     */
            }
        }
        /* ------- poll for serial input ---------- */
        if (input_char_ready() || forcecom)    {
            c = forcecom ? forcecom : readcomm();
            forcecom = (answering && c == '\r') ? '\n' : 0;
            log(c);           /* display the serial input    */
            if (answering)
                writecomm(c); /* answerer echos serial input */
        }
    }
    release_modem();
}

/* ------- display and process the TINYCOMM menu --------- */
static void tinymenu(void)
{
    int c;
    clrscr();
    gotoxy(20,5),  cprintf("------ TINYCOMM Menu ------");
    gotoxy(20,7),  cprintf("P-lace Call");
    gotoxy(20,8),  cprintf("A-nswer Call");
    gotoxy(20,9),  cprintf("H-ang Up");
    gotoxy(20,10), cprintf("L-og Input %s",
                    logfp == NULL ? "[OFF]" : "[ON]");
    gotoxy(20,11), cprintf("S-end Message File");
    gotoxy(20,12), cprintf("T-elephone Number (%s)",
                    PHONENO[0] ? PHONENO : "???-????");
    gotoxy(20,13), cprintf("E-xit to DOS");
    gotoxy(20,14), cprintf(connected ?
                        "Esc to return to session" : "");
    gotoxy(20,16), cprintf("Enter Selection > ");
    c = getch();
    putch(toupper(c));
    switch (toupper(c))    {
        case 'P':                /* Place a call */
            if (!connected)    {
                cprintf(TMSG, "Dialing");
                initmodem();  /* initialize the modem  */
                placecall();  /* dial the phone number */
                connected = 1;
                cprintf(TMSG, "Esc for the menu");
            }
            break;
        case 'A':                /* Answer a call */
            if (!connected)    {
                cprintf(TMSG, "Waiting");
                initmodem();  /* initialize the modem      */
                answercall(); /* wait for an incoming call */
                answering = connected = 1;
                cprintf(TMSG, "Esc for the menu");
            }
            break;
        case ESC:                /* Return to the session */
            if (connected)
                cprintf(TMSG, "Esc for the menu");
            break;
        case 'L':                /* Log input on/off*/
            if (logfp == NULL)
                logfp = fopen(LOGFILE, "a");
            else    {
                fclose(logfp);
                logfp = NULL;
            }
            forcemenu++;
            break;
        case 'E':                /* Exit to DOS */
            cprintf(TMSG, "Exiting");
            running = 0;
        case 'H':                /* Hang up */
            if (connected)    {
                cprintf(TMSG, "Hanging up");
                disconnect();
                connected = answering = 0;
            }
            break;
        case 'S':                /* Send a message file */
            upload();
            break;
        case 'T':                /* Change the phone number */
            cprintf(TMSG, "Enter Telephone Number: ");
            gets(PHONENO);
            forcemenu++;
            break;
        default:
            putch(7);
            break;
    }
}

/* --------- upload an ASCII file ---------- */
static void upload(void)
{
    char filename[65];
    int c = 0;
    if (uploadfp == NULL && connected)    {
        cprintf(TMSG, "Enter file drive:path\\name > ");
        gets(filename);
        if ((uploadfp = fopen(filename, "r")) == NULL)
            cprintf(TMSG, "Cannot open file");
        else    {
            cprintf(TMSG, "Press Esc to stop sending file");
            while ((c = fgetc(uploadfp)) != EOF)    {
                if (c == '\n')    {
                    writecomm('\r');
                    log(answering ? '\r' : readcomm());
                }
                writecomm(c);
                log(answering ? c : readcomm());
                if (keyhit())
                    if (getch() == ESC)    {
                        cprintf(TMSG, "Abandoning file");
                        break;
                    }
            }
            fclose(uploadfp);
            uploadfp = NULL;
        }
    }
}

/* ----- echo modem or keyboard input and write to log ----- */
static void log(int c)
{
    putch(c);
    if (logfp)
        fputc(c, logfp);
}

/* --------------------------------------------------------
   Clone functions to keep Ctrl-Break from crashing the
   system by aborting before interrupt vectors get restored
   -------------------------------------------------------- */
#if COMPILER==TURBOC
/* --------- use bios to test for a keystroke -------- */
int keyhit()
{
    rg.h.ah = 1;
    int86(0x16, &rg, &rg);
    return ((rg.x.flags & 0x40) == 0);
}
#else
/* ------- substitute for getch for MSC --------- */
static int mscgetch(void)
{
    rg.h.ah = 0;
    int86(0x16, &rg, &rg);
    return rg.h.al;
}

/* ------- substitute for putch for MSC --------- */
static void mscputch(int c)
{
    rg.x.ax = 0x0e00 | (c & 255);
    rg.x.bx = 0;
    int86(0x10, &rg, &rg);
}

/* -------- gotoxy clone ------------ */
static void gotoxy(int x, int y)
{
    rg.h.ah = 2;
    rg.x.bx = 0;
    rg.h.dh = (char) y-1;
    rg.h.dl = (char) x-1;
    int86(0x10, &rg, &rg);
}

/* -------- clrscr clone ------------- */
static void clrscr(void)
{
    rg.x.ax = 0x0600;
    rg.h.bh = 7;
    rg.x.cx = 0;
    rg.x.dx = (24 << 8) + 79;
    int86(0x10, &rg, &rg);
}

/* ----------- gets clone ------------- */
static void mscgets(char *s)
{
    int c;
    while (1)    {
        if ((c = mscgetch()) == '\r')
            break;
        mscputch(c);
        *s++ = (char) c;
    }
    *s = '\0';
}
#endif
```

## LISTING 6

```c
; ------------- keyhit.asm ---------------
;
; Use this in MSC C programs in place of kbhit
; This function avoids Ctrl-Break aborts
;
_text   segment para public 'code'
assume  cs:_text
public  _keyhit
_keyhit proc    near
        mov     ah,1
        int     16h
        mov     ax,1
        jnz     keyret
        mov     ax,0
keyret: ret
_keyhit endp
_text   ends
        end
```

## LISTING 7

```c
tinycomm (serial.h, modem.h)
modem (serial.h, modem.h)
serial (serial.h)
```

## LISTING 8

```c
#  TINYCOMM.MAK: make file for TINYCOMM.EXE with Microsoft C/MASM
#

.c.obj:
    cl /DMSOFT=1 /DCOMPILER=MSOFT -c -W3 -Gs $*.c

tinycomm.obj : tinycomm.c serial.h modem.h window.h

modem.obj : modem.c serial.h modem.h

serial.obj : serial.c serial.h

keyhit.obj : keyhit.asm
    masm /MX keyhit;

tinycomm.exe : tinycomm.obj modem.obj serial.obj keyhit.obj
    link tinycomm+modem+serial+keyhit,tinycomm,,\lib\slibce
```
