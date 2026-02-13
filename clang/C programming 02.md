
# C programiranje 02

## Jedenje slona: projekat počinje

Al Stevens, septembar '88

Ovog meseca inauguriramo projekat kolumne "C programiranje", funkciju u kojoj ćemo razviti komunikacioni program za koji bi trebalo da bude potrebno nekoliko meseci da se završi. Da bismo započeli projekat, moramo uzeti u obzir zahteve programa. Po mom mišljenju, dve procedure su neophodne tokom razvoja računarskog sistema:

- prva je analiza zahteva,
- druga je koncept inkrementalne implementacije.

Analiza zahteva rezultira izjavom šta program mora da radi i kako mora da radi. Izražava se na jeziku koji razumeju i programeri i korisnici. Bez toga nikad ne znate kada ste gotovi. Takva analiza se bavi dve oblasti: funkcionalnim zahtevima i zahtevima performansi. Odložićemo korak funkcionalnih zahteva za kasnije i za sada ćemo se koncentrisati na zahteve performansi.

Inkrementalna implementacija znači da gradite sistem malo po malo i da ga isporučujete korisniku u malim koracima. Naredne isporuke će imati koristi od iskustava koja su im prethodila, ne samo od vašeg, već i od iskustva korisnika. Većina vladinih projekata ignoriše ovu mudrost, preferirajući da automatizujete celu zapadnu hemisferu, da to uradite u kratkom roku, a zatim da ga uključite odjednom. Postoje izuzeci. Džim Touls, menadžer građevinarstva i inženjera u svemirskom centru Kenedi, zna bolje i tako kaže. "Ako želite da pojedete slona, ne radite to u jednom zalogaju." Neće dozvoliti nikome od onih kompjuterskih ljudi da mu odjednom gurnu veliki sistem. Jim preferira manje zalogaje slona.

Naš projekat će imati inkrementalni pristup. Prvo ćemo razviti alate za podršku zahtevima performansi. Zatim ćemo izgraditi minimalni sistem oko tih alata. Razvoj i upotreba tog prvog zalogaja će nam reći kako da nastavimo sa sledećim.

## Zahtevi za performanse

Učinak programa kolumne "C programiranje" mora da prati ove smernice:

- To mora biti on-line, interaktivni PC program (da li postoji neka druga vrsta?).
- Koristiće padajauće menije.
- Imaće prozore pomoći osetljive na kontekst.
- Unos podataka će biti podržan od strane biblioteke unosa opšte namene orijentisane na prozore.
- Postojaće paket za uređivanje teksta opšte namene za unos svih tekstualnih podataka.
- Komunikacioni deo programa pretpostavlja Hayes-kompatibilan modem.
- Xmodem protokoli za prenos podataka će biti podržani (možda i Kermit u kasnijem razvoju).
- Sve funkcije prozora (unos podataka, uređivač, pomoć, meniji) će koristiti zajedničku biblioteku video prozora.

Za one koji ne znaju šta je video prozor, evo kratkog opisa. Video prozor je pravougaona oblast ekrana koja ima granicu, ponekad naslov i tekst koji se prikazuje unutar granica. Prozori se pojavljuju jedan na drugom. Kada prozor iskoči ili nestane, a video ekran - možda drugi prozori - koji su bili ispod njega ponovo su vidljivi.

## Prozorska biblioteka

Da bismo započeli projekat kolumne "C programiranje", počećemo sa bibliotekom prozora koja podržava ostatak programa. Ovaj paket je na dnu našeg dizajna. U ovom nastojanju kreiramo program odozdo prema gore. Ovo nije uvek u pravom smeru, ali ovde imamo prednost jer su naši zahtevi u skladu sa softverom koji sam objavio na drugim mestima. Oni od vas koji su čitali moje knjige prepoznaće sličnosti. Ovaj projekat će koristiti podskup tog softvera modifikovanog za potrebe ovog programa. Biblioteka prozora će biti dovoljno opšta da je možete koristiti za većinu drugih aplikacija orijentisanih na prozore, ali će joj nedostajati mnoge lepe funkcije koje se nalaze u knjigama i mnogim komercijalnim bibliotekama prozora. Izostavio sam ih jer ovde nisu potrebni. Želimo najefikasniji mogući program.

Funkcije prozora koriste funkcije konzole Turbo C tekstualnog režima za upravljanje postavljanjem prozora i prikazom podataka. Iz tog razloga će vam trebati Turbo C, verzija 1.5 ili novija.

[Listing 1](#listing-1), je "window.h", koji sadrži prototipove globalnih funkcija, neke #define izjave, strukture prozora i informacije o konfiguraciji. Nekoliko reči o konfiguraciji: Mnogi programi dolaze sa instalacionim programom koji vas vodi kroz izbor boja ekrana i slično. Nećemo uključiti takvu funkciju jer je ovo program za programere, koji programeri prave i koji programeri koriste. Programeri mogu da rukuju parametrima konfiguracije promenom izvornog koda i kompajliranjem. Da bih olakšao taj proces, koristiću #define makroe za stavke konfiguracije gde god je to moguće. Videćete blok takvih makroa na dnu liste jedan. Ovi makroi vam omogućavaju da konfigurišete boje ekrana programa. Postoji osam stavki ekrana koje se mogu prilagoditi. Ovo će vam dati nagovještaj o tome šta dolazi u paketu prozora u narednim mesecima. Osam stavki su: ekrani za prikaz podataka, blokovi podataka (na primer, označeni blokovi u uređivaču teksta), prozori pomoći, meniji, trake za biranje menija, prozori za unos podataka, polja u prozorima za unos podataka i poruke o grešci. Kao što je ovde objavljeno, konfiguracija koristi uzorak crno-belih za sve stavke. Po želji možete koristiti i druge boje. Globalni simboli za boje su dati u Turbo C "conio.h". Mogući globalni simboli su BLACK, BLUE, GREEN, CYAN, RED, MAGENTA, BROWN, LIGHT GRAY, DARK GRAY, LIGHT BLUE, LIGHT GREEN, LIGHT CYAN, LIGHT RED, LIGHT MAGENTA, YELLOW, i WHITE.

Obratite pažnju na ezoterične prototipove i strukturu ispod komentara "interne Turbo C stvari". Oni pružaju pristup internoj video logici Turbo C i kompatibilni su sa Turbo C, verzijom 1.5. Ako Borland promeni ove konstrukcije u budućoj verziji, moraćemo da izvršimo prilagođavanja. Koristio sam ove konstrukcije za direktno čitanje i pisanje sa ekrana van domena onoga što dozvoljavaju tekstualne video funkcije u Turbo C-u. One nas oslobađaju od asemblerskog jezika i statusnih registara video retrace. Ovo je veoma upitno, neprenosivo, rizično programiranje hakerskog mentaliteta. Ovim se upozoravate da nikada ne koristite takve prakse u svom kodu.

[Listing 2](#listing-2), je "window.c", biblioteka funkcija prozora. Biblioteka prozora pruža šest osnovnih funkcija za podršku prozorima. Koncept prozora predviđa da se poslednji postavljeni prozor adresira bilo kojim narednim prozorskim operacijama, tako da kada se prozor izbriše, onaj koji je uspostavljen pre nego što postane trenutni prozor. Evo opisa svake od funkcija.

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
Ova funkcija zahteva objašnjenje. Poziva se da pomera trenutni prozor gore ili dole jedan red. On bira jedan od dva načina da to uradi na osnovu promenljive __video.snov. Biblioteka teksta Turbo C je veoma lepo odredila da li će ekran prikazati sneg videa kada se završi direktno čitanje i upisivanje video zapisa. Ovu odluku donosi testiranjem poboljšanog grafičkog adaptera, koji ne pada, adaptera za monohromatski ekran, koji ne, ili grafičkog adaptera u boji, koji radi. Ide dalje i odlučuje da Compak verzija CGA ne pada, što, naravno, ne pada. Rezultat njegove odluke je zabeležen u "__video.snov". To će odlučiti da neke mašine sneže kada ne padnu. Primer je laptop Toshiba T1000. Sve je ovo u redu, ali kada koristimo tekstualne funkcije Turbo C za pomeranje ekrana na snežnoj mašini, pomeranje je presporo. To je zato što svaki znak koji se čita ili upisuje dok se pomera tekst za pomeranje mora da sačeka da se pojavi video retrace. Dakle, ako dozvolimo Turbo C da skroluje umesto nas, performanse trpe. Iz tog razloga koristimo BIOS video usluge za pomeranje snopa ekrana. Možda se pitate zašto to ne radimo uvek. Ah, hirovi nesavršenog sveta! Kada se BIOS pomera, on koristi dosadno zatamnjenje ekrana. Ova smetnja je draža od snega ili sporo, pa se prihvata kao manje od tri zla.

U narednih nekoliko meseci dodaćemo funkcije pomoći, menadžer menija, ekrane za unos podataka i uređivač teksta. Nakon toga ćemo ući u komunikacioni deo programa i njegovu krajnju svrhu.

### C Kroki broj 3: Kako se uči C

(Podsetnik: krokiji su stvari koje iritiraju ljude. Pogledajte prošlomesečno "C programiranje" za objašnjenje i kukičanje broj 1 i broj 2.)

Moja najbolja prijateljica i saputnica, Džudi, upisana je na program informatike na lokalnom koledžu u Virdžiniji. Upravo je završila uvodni čas u C. Instruktor je priznao da je novi u C i dao zadatke za izradu malog programa. Dao je primere kako određene stvari treba da budu kodirane. Evo fragmenta.

```c
while (fgets(line[lineno ++], 100, fp));
```

Odeljenju je rečeno da je ovaj primer kako čitate datoteku teksta u memoriju. Posebno obratite pažnju na tačku i zarez. Većina nas može da shvati šta se ovde dešava. While se koristi za petlju dok se sve linije datoteke ne pročitaju u niz. Pa šta nije u redu sa ovom slikom? Puno.

1. Ne postoji kontrola granica. Ako datoteka ima više redova od niza, program će se verovatno srušiti.

2. Tačka i zarez je na pogrešnoj liniji. Trebalo bi da bude uvučeno ispod iskaza while da bi se čitaocu rekao da je to namerna nulta izjava, a ne nesreća kao što se čini.

3. Instruktor nikada nije objasnio pojam operativnog iskaza kao komponente uslovnog izraza u roku, niti se bavio upotrebom nulte izjave, koja je tu samo da bi dala vhile nešto da uradi sve dok njegov uslov ne bude FALSE.

4. Ideja da izjava koja čita datoteku takođe vraća uslov kraja datoteke i da se može testirati kada se izvrši nije objašnjena.

5. Nivo na kojem se razred nudi sugeriše da učenici nisu spremni za takve koncepte kao što su auto-inkrementi koji se dešavaju u isto vreme kada se pretplatni ceo broj koristi kao indeks.

6. Konstanta 100 je opasna tačka. Ako je linija dvodimenzionalni niz stringova, konstanta se može kodirati veličinom operatora. Barem konstantu treba izjednačiti sa globalnim simbolom radi boljeg čitanja i lakšeg održavanja.

Evo jasnije prezentacije iste logike sa dodatkom provere granica.

```c
while (lineno < MAXLINES) { 
   rtn = fgets(line[lineno], LENGTH, fp); 
   if (rtn = NULL) 
     break; 
   lineno+ +; 
}
```

Naravno, koristi nekoliko redova gde će se raditi, i sigurno, nekoliko iskusnih C programera zaista piše takav kod. Ovaj kroki nije za stručne C programere koji samo programiraju. Oni su zauvek ohrabreni da koriste jezik u potpunosti. Ovaj kroki je za one od vas koji bi predavali. C podstiče čvrste i koncizne izraze i zbog toga ga cenimo. Novi C programer, međutim, mora pažljivo i polako da pređe na tako naprednu upotrebu. Ova sloboda sintakse je glavni predmet kritike C od strane poklonika drugih jezika, ali dukatori ne moraju da ga tako podučavaju. Programeri mogu da nauče lepe, čitljive sekvence iskaza poput onog iznad i da priđu strožoj, elegantnijoj strani jezika sopstvenim tempom. Ono čemu su učenici učili je da će primer instruktora pročitati tekst u niz. Nikada nisu naučili zašto.

Planiram da buduću kolumnu posvetim problemima nastave C.

### C Kroki broj 4: Zamerke zbog naknada za nadogradnju

Sviđa mi se Turbo C. I jednog dana kada dobijem novu ispravljenu verziju, možda će mi se svideti QuickC. Dok sam istraživao knjige o ovim kompajlerima, proveo sam dosta vremena na CompuServe i BIX on-line servisima. Ne postoji bolji način da se reši problem kompajlera ili jezika nego korišćenjem povezanih foruma ovih usluga. U poslednje vreme kroz diskusije se provlači zajednička tema, koja gubi vreme za povezivanje i zaslužuje komentar. Kada Borland ili drugi dobavljač objavi novu verziju C kompajlera, oni obično naplaćuju nominalnu naknadu za nadogradnju registrovanim korisnicima. Takva naknada izgleda razumna kada se uzme u obzir vrednost C kompajlera. Mnogi od vas će se setiti šta smo plaćali za C kompajlere koji su imali mnogo manje funkcija. Uobičajena pritužba je, međutim, da se programeri osećaju pokradenim zbog naknade. Oni tvrde da pošto nadogradnja uključuje ispravku poznatih grešaka, prodavac treba da je obezbedi besplatno. Svi znamo da ti oglasi u boji na više stranica (verujete li Turboman-u?) i te hiljade radnih sati programera nisu besplatni za dobavljače kompajlera. Prihod za nastavak promocije i unapređenja proizvoda mora da dođe odnekud. Ipak, gunđanje i dalje traje. Jedan takav razgovor na forumu trajao je nekoliko dana o naplati nadogradnje od 10 dolara.

Moj drugar Bil Čejni se zapali i kaže da su takvi ljudi "tako stegnuti da ne bi potrošili ni cent da vide kako smrdljiva buba jede balu sena". Polako Bille....

## Programska biblioteka C

Knjiga koju treba pročitati ovog meseca je "Vodič za C programera za serijske komunikacije" Džoa Kembela (Howard V. Sams & Company, 1987). Postoji preko 650 stranica teksta i koda koji se bave temom asinhronih serijskih komunikacija na mikroračunarima. Kembel predstavlja temu jasnim, ali naprednim jezikom. Ovo nije knjiga za početnike. U najmanju ruku treba da budete C programer, a pomaže da već imate prolazno upoznavanje sa mutnim dubinama serijskih komunikacija.

Kembelov stil pisanja je osvežavajući i čini ga dobrim ako ne i laganim čitanjem. On voli da koristi reči koje će mnoge od nas naterati da se pomuče za Websterom, i ne pokušava da sakrije svoj prezir prema onome što smatra inferiornim dizajnom. Njegov tretman Xmodem i Kermit protokola za prenos podataka ostavlja čitaocu utisak da bi po njegovom mišljenju svet bio daleko bolje mesto da je Džou Kembelu bilo dozvoljeno da ih dizajnira. Uglavnom su njegove kritike validne i tačne čak i kada su očigledno rođene iz dragocene perspektive jasnog retrospektiva, ali da sam na mestu Word Kristensen ili nekolicina drugih, uši bi mi gorele. U predgovoru knjizi, Kembel se poziva na svoje "skromno prepoznavanje zahteva [subjekta komunikacije podataka]", i to je poslednji dokaz poniznosti koji ćete naći u ovoj knjizi. Međutim, povremeni propust u poniznosti je u redu kada je očigledna arogancija opravdana ili potkrijepljena podjednako prividnom inteligencijom i stručnim znanjem, a Kembel to iznosi u ovoj knjizi. Njegov opis Kermita je prvi koji sam pročitao, a za razumevanje nije bilo potrebno bar drugo čitanje.

Knjiga govori o teoriji komunikacija, hardveru, softveru i implementaciji ovih koncepata u jeziku C. To je zdrav tretman ovih subjekata i neophodan dodatak biblioteci onih koji su uključeni u C projekte gde računari razgovaraju jedni sa drugima preko serijskih interfejsa. Nedavno sam preuzeo arhivsku datoteku C izvornog koda koja navodno implementira Kermit protokol u Unix okruženju. Želeo sam da prenesem nedokumentovani kod na računar i učim iz njega. Na moju žalost, otkrio sam da je jedan izvorni fajl u arhivi nepotpun, da se naglo završava u sredini funkcije. Uz objašnjenja u Kembelovoj knjizi sada mogu da pokušam da pružim kod koji nedostaje. Kembelova knjiga ima C funkcije koje implementiraju Xmodem protokol, ali nema ekvivalentan kod za Kermit.

Dok nisam pročitao ovu knjigu, nikada nisam u potpunosti razumeo zašto su serijski ulaz-izlaz i RS-232 "standard" bili toliko zbunjeni toliko godina. Kada sam konsultovao 1978. godine, mikroračunar je bio nov u poslovnom svetu, a računarski sistemi sa plug-and-go aparatima su bili retki. Zarađivao sam značajan deo svog života sa kutijom za razbijanje, nešto kablova, DB25 konektorima i lemilom. Svaka instalacija je imala neki novi serijski štampač koji je trebao biti povezan sa nekim hakovanim mikrokompjuterom i nikad ništa nije odgovaralo. U poglavlju o kontroli RS-232, Kembel objašnjava da RS-232 nikada nije bio namenjen da se koristi za rukovanje štampačem. Kako to da to nisam znao? Bez obzira na prvobitne namere RS-232, većina tadašnjih štampača se mogla kupiti sa serijskim portom. Ova opcija je omogućila korisnicima pametnih terminala i modema da podese štampač na pomoćni serijski port na svom terminalu. Kao posledica toga, dizajneri ranih mikroračunara postavili su serijske interfejse štampača u svoje mašine kako bi prihvatili te štampače koji su već u upotrebi.

Centronics paralelni standard za štampače je postojao, ali ga je koristilo nekoliko sistema. Možda su UARTS i čipovi za linijski drajver bili jeftiniji ili dostupniji od paralelnih drajvera. Šta god da je razlog zanemarivanja Centronics interfejsa, kada ga je IBM usvojio za PC, započeo je trend koji me je na kraju srećno poslao iz poslovanja sa povezivanjem od 1200 bauda. Mogao sam da koristim Kembelovu knjigu tada.

Kembel zaslužuje moje poštovanje svojim priznanjem Koulu Porteru i Džoniju Merseru, dvojici kompozitora popularne muzike iz ere pre MTV-a. Oni sigurno nisu bili programeri i verovatno nikada nisu čuli za C, RS-232, pa čak ni za MIDI, ali njihovo nasleđe našoj kulturi se ceni među nama koji predugo i prekasno se zavlačimo u tihe jedinice i nule.

Sledećeg meseca ćemo dodati neke funkcije u biblioteku prozora, pogledati drugu knjigu i pokrenuti još neke kvačice. Do tada, ostanite u formi i nastavite da kodirate.

## LISTING 1

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

## LISTING 2

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
