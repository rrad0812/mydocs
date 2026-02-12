
# C programiranje 04

## Prozorski uređivač teksta

Al Stevens

Ovaj mesec dodajem uređivač teksta u PC C skup alata koji pravimo. Prethodne dve kolumne „C programiranje“ sadržale su biblioteku prozora, prozore za unos podataka i menadžer menija. Buduće kolumne će paketu dodati prozore pomoći, komunikacijske funkcije i protokole za prenos datoteka. Na kraju će sve to biti okupljeno u integrisani komukacioni program. Ali pre nego što dođemo do uređivača teksta ovog meseca, želim da se pozabavim brigom čitalaca koja stalno dobijam.

Nekoliko čitalaca je izrazilo užasnutost što je ovaj projekat zaključan u Turbo C-u, pa ih je stoga zaključao. Prepoznajem ovo kao opravdanu zabrinutost i o tome ću se baviti u kolumni iz januara 1989. godine.

Ispravka će imati oblik biblioteke funkcija i makroa koji prevode Turbo C specifičan kod u kod prihvatljiv za Microsoftov C. U januarskoj koloni ću opisati biblioteku tako da korisnici kompajlera mogu da razviju slične biblioteke. Sada nazad na posao.

Da bismo dizajnirali uređivač teksta, moramo da ispitamo naše zahteve, prvo da vidimo zašto nam je potreban, a zatim da vidimo šta treba da radi. Verovatno ništa u računarstvu nije tako lično kao vaš urednik, bilo da ga koristite za obradu teksta ili za unos izvornog koda. Pošto smo vi i ja korisnici ovog paketa, i pošto svako od nas već ima željeni uređivač i program za obradu teksta, trebalo bi da preispitamo svoje motive za dodavanje još jednog u izbor. Zatim, pod pretpostavkom da nađemo opravdanje za drugog urednika, njegov dizajn mora da zadovolji posebne zahteve svakog od nas. Nije lak cilj, ali ipak plemenit.

Program koji proizilazi iz svega ovoga biće komunikacioni paket za pristup on-line servisu. Usluge na mreži uključuju razmenu tekstualnih poruka – pošte, razgovora na forumu i poruka SISOP-ima – između korisnika. Prema tome, pristupnom programu je potreban unos teksta slobodnog oblika, drugim rečima, uređivač teksta. Mogli bismo da navedemo da korisnik može da koristi sopstveni uređivač, a mi bismo mogli da obezbedimo prolaz do njega u našem kodu. Možda je ta funkcija dodatni zahtev za naš sistem. Čak i tako, bez sumnje će postojati i druga mesta u programu gde je potreban unos teksta, mesta na kojima bi bilo nezgodno izaći u uređivač teksta i priložiti rezultujući unos tekstualnih podataka strukturi podataka koja se gradi. Obrada konferencijskog razgovora u realnom vremenu može biti jedna od takvih aplikacija. Čini se, dakle, da nam je potreban uređivač teksta orijentisan na prozore, onaj koji se može pozvati iz programa da prikupi tekst kroz prozor u bafer.

Pošto uređivači obično rade iz komandnog jezika i pošto većina korisnika ima lične preferencije za uređivanje komandi, naš program takođe mora da dozvoli korisniku da izmeni skup komandi.

Listing 1 i listing 2 su "editor.h" i "editor.c". Oni čine osnovni uređivač teksta prozora. Sledećeg meseca ćemo dodati iskačući meni, funkcije pretraživanja teksta i neke funkcije upravljanja datotekama za kreiranje, uređivanje, čuvanje i spajanje tekstualnih datoteka. Prošireni program za uređivanje će činiti mali procesor teksta, onaj koji se može integrisati u C aplikaciju tačno onako kako nameravamo da je koristimo za poštu i poruke na forumu. Kao i svi naši alati, uređivač je dizajniran tako da se može ponovo koristiti za druge projekte koje vi ili ja možemo preduzeti.

Pre nego što opišem kod, hajde da razgovaramo o filozofiji urednika. Morao sam da razmotrim tri stvari u dizajnu:

- način na koji će urednik raditi interno;
- kako se može prilagoditi; i
- koje funkcije za obradu teksta će uključiti.

### Interni tekstualni formati i rad

Odlučio sam da prilagodim prozorski uređivač teksta iz mojih knjiga o Turbo C i QuickC. Kod radi ispravno, moj i tvoj je za korišćenje i relativno je mali. Ovde je modifikovano da koristi sažetije funkcije prozora iz ove kolumne i da bude proširivo u smislu da je lako dodati komande bez modifikacije izvornog koda samog uređivača. Takođe sada ima potencijal za rekurzivne pozive uredniku. Iako ne postoje neposredni planovi za uređivač sa više prozora ili za uređivač koji se može rekurzivno pozvati, uređivač ne bi trebalo da isključi te funkcije. Prema tome, kod će omogućiti aplikaciji da sačuva trenutno okruženje urednika i ponovo pozove uređivača za drugo okruženje (prozor, bafer i tako dalje).

Da biste koristili uređivač teksta prozora u programu, morate obezbediti prozor i bafer sa, možda, nekim tekstom koji je već u njemu. Veličina bafera je funkcija širine najšire dozvoljene linije teksta i maksimalnog broja redova teksta. Kada ga izvršite, kažete uredniku ove dve dimenzije. Editor pretpostavlja da je prozor dovoljno širok da zadrži redove i da je bafer dovoljno dubok za tekstualnu datoteku sa maksimalnim brojem redova. Ovaj pristup, koji koristi razmake na kraju za popunjavanje svakog reda, koristi više memorije bafera od one koja koristi nove redove u tekstu, ali kod za upravljanje kursorom i blokovima teksta je manje složen. Bafer je u suštini ki pravougaonik od i redova sa k kolona.

Pošto fiksni pravougaoni bafer mora da stane horizontalno unutar prozora, uređivač ne obezbeđuje horizontalno pomeranje. To znači da linije ne mogu da se protežu dalje od desne margine prozora. To takođe znači da tekstualna datoteka ne može imati promenljive margine. Ovo su funkcije koje menjamo da bismo dobili efikasan uređivač unutar aplikacije, a njihov gubitak ne ugrožava naše zahteve za alatom za sastavljanje poruka.

Editor ne snima niti na drugi način koristi tabulator (\t) u tekstualnom baferu. Kada otkucate taster Tab, uređivač ubacuje odgovarajući broj razmaka da bi stavio kursor na sledeće tabulatorsko mesto. Tabulatori su navedeni kao fiksni intervali pozicija znakova. Širina intervala se može konfigurisati kada kompajlirate kod uređivača.

Editor vam omogućava da označite tekstualne blokove za operacije premeštanja, kopiranja, brisanja i paragrafa. Ovi blokovi su onlajn granice, a ne na pozicijama karaktera. I ovaj izbor je napravljen u interesu efikasnog koda.

### Prilagođavanje uređivača

Prilagođavanje uređivača prozora se sastoji od promene nekih podrazumevanih vrednosti i davanja sopstvenih komandnih ključeva umesto onih koji su objavljeni. Ako napravite takve promene, kada se školjka menija i prozori pomoći dodaju kasnije, moraćete da promenite tekst koji vam govori šta su ove komande. Za sada, međutim, potrebno je samo da redefinišete konfigurisane vrednosti komande koje se nalaze u "editor.h". Komande uređivača su vrednosti pritiska jednog tastera koje isporučuje funkcija `getkey` u "window.c" (septembar DDJ). Komande koje se mogu konfigurisati imaju mnemonička globalna imena kao što su END_LINE i DELETE_WORD. Promenite njihove vrednosti definisanjem različitih vrednosti za njih. Imajte na umu da ovaj pristup koristi pojedinačne pritiske na tastere kao uređivačke komande. Ako želite da ovaj uređivač emulira WordStar sekvence dvostrukog klika kao što to rade Borland i QuickC uređivači, morate napisati zamenu `getkey` i koja zna za njih. Postoji presedan za ovo – mnogi ljudi su zadovoljni skupom komandi WordStar – ali urednik kako je ovde objavljen ne podržava sekvence dvostrukog klika.

Tri druge konfiguracione stavke se nalaze u "editor.h". Ovo su:

- širina tabulatora,
- da li se uređivač pojavljuje u režimu umetanja ili prepisivanja i
- da li će uređivač imati omogućeno automatsko reformisanje pasusa.

Da biste promenili boje prozora uređivača, promenite globalne vrednosti TEKSTFG, TEKSTBG, BLOCKFG i BLOCKBG u "window.h" iz kolumne „C programiranje“ za septembar. Boje teksta su normalne boje za prozor uređivača teksta i treba ih navesti kao boje prozora kada uspostavite prozor koji će uređivač koristiti. Boje bloka postavlja urednik kada je blok označen. Uređivač resetuje prozor na boje teksta za neblokirane ekrane. Konfiguracija boja je razmatrana u septembru.

Naravno, dostupan vam je još jedan nivo prilagođavanja. Imate izvorni kod za ovaj uređivač i možete ga naterati da radi šta god želite u granicama svojih mogućnosti sa jezikom C.

### Karakteristike obrade teksta

Niko neće koristiti ovaj uređivač da zameni Word, WordPerfect, XWrite ili WordStar; niti ćete izbaciti Brief, vi, Vedit, EMACS ili Norton Editor. Bez obzira na to, uređivaču je potreban dovoljan skup funkcija za obradu teksta da bi bio upotrebljiv za unos poruka. Iz mog iskustva sa programima za obradu teksta i uslugama na mreži dolazi skup funkcija za koje verujem da su minimum, a one su ugrađene u uređivač. Shvatajući, međutim, da će se potreba za dodatnim funkcijama nesumnjivo pojaviti kasnije, napisao sam kod tako da se ekstenzije - te nove, neotkrivene funkcije - mogu lako postaviti. Više o tome kasnije; prvo minimalne karakteristike.

- **Unos teksta**  
Možete da ukucate reči u bafer. Prelom reči se dešava kada reč koja se kuca dostigne desnu marginu ili kada umetnuti znak gurne krajnju desnu reč na marginu. Kada kucate, režim umetanja ili preklapanja je efikasan. Paragrafi se mogu automatski preformatirati dok kucate ili ne kako je određeno programiranim prekidačem.

- **Pomeranje kursora**  
Kursor možete da pomerate po znakovima, stranicama sa rečima, karticama, desno ili levo od reda, na vrh ili dno prozora, ili na početak ili kraj teksta. Ako otkucate taster Enter, kursor se pomera na sledeći red, prvu kolonu. Ako se to uradi u režimu umetanja, linija se deli.

- **Uređivanje**  
Možete da izbrišete znakove (levo ili desno), reči, linije ili blokove. Možete označiti blok redova, a zatim premestiti, kopirati, izbrisati ili formirati pasus iz bloka. Možete da skinete oznaku sa označenog bloka.

- **Pasusi**  
Pasus počinje uvučenom linijom. Uvlačenje je broj razmaka u intervalu tabulatora. Automatsko reformisanje pasusa se dešava od pozicije kursora do sledećeg pasusa ili praznog reda. Komanda paragrafa radi isto kada nijedan blok nije označen.

Ovo su osnovne funkcije za uređivanje teksta bafera teksta koji se uređuje kroz prozor pomoću našeg uređivača teksta. Aspekti upravljanja datotekama projekta uređivača dolaze sledećeg meseca kada vidimo kako čitati i pisati tekstualne datoteke u i iz bafera. Takođe ćemo dodati algoritme za pretraživanje teksta i školjku menija. Ništa u ovom programu ne gura tehnologiju. Dodali smo alat u našu kolekciju, onaj koji će omogućiti da korisnici naših programa unose i menjaju tekst slobodnog formata u prozoru.

### Kod editora

Listing 1 je "editor.h". Koristi se za definisanje skupa komandi urednika i podešavanja podrazumevanog režima. Naredbe #define za komande dodeljuju vrednosti ključa mnemotehnici komande. Ključne vrednosti su preuzete iz "window.h" ili su ovde definisane. Pritisci na tastere imaju vrednosti koje vraća funkcija getkei u "window.c". Vrednosti za funkcijske tastere se formiraju dodavanjem vrednosti 128 kodu za skeniranje koji vraća BIOS, čime se postavlja najvažniji bit i formira jedinstvena 8-bitna vrednost za pritisak tastera. Pošto "window.h" nema definisane sve moguće vrednosti ključeva, svaki dodatak skupu alata koji treba druge vrednosti ključeva mora sam da ih definiše. Dakle, imamo dve definicije tastera Alt na vrhu "editor.h".

- Mnemonik **TAB** definiše širinu tabulatora. Kao što je ovde objavljeno, TAB je 4, tako da će se tabulatori pojaviti na 5, 9, 13 i tako dalje. Ako promenite TAB, pozicije kartica će se promeniti u skladu sa tim.

- Promenljiva REFORMING je podešena na TRUE ili FALSE da bi se odredilo da li će se automatsko reformisanje pasusa dogoditi dok kucate. Ako ga postavite na TRUE, uređivač testira da vidi da li pasus treba da se reformiše svaki put kada se izbriše znak ili reč i kada dođe do prelamanja reči. Ovaj test upoređuje beli prostor na kraju tekućeg reda sa dužinom prve reči u sledećem redu. Ako reč stane na kraj trenutnog reda, pasus se menja. Ovo je pogodnost kada unosite neobrađeni tekst; to bi bio bol u vratu za šifru ili sto. Stoga će softver menija sledećeg meseca uključivati komandu za uključivanje i isključivanje režima. Na nekim sporim procesorima automatsko reformatiranje postaje sporije kada je reč premotana blizu vrha dugog pasusa, a prikaz tastera koje kucate zaostaje za brzinom srednjeg do brzog daktilografa. Da biste poboljšali ove performanse, uklonite poziv test-para u funkciji carttn u editor.c. Ovo će promeniti pravila reformatiranja tokom prelamanja reči tako da rade samo na trenutnom i sledećem redu teksta, gurajući ostatak pasusa niz ceo red kada je potrebno mesto. Kasnije će pritiskom na komandu PARAGRAPH (F2) završiti operaciju reformatiranja. Ovaj pristup je blizak načinu na koji VordStar funkcioniše. Originalni pristup je sličan, ali ne tako brz kao KsiVrite.

- Promenljiva **INSERT** je postavljena na TRUE ili FALSE da bi se naznačilo da li je kucanje u režimu umetanja ili preklapanja. Vrednost dodeljena globalnom simbolu je podrazumevani režim kada se pokrene uređivač. Nakon toga taster Ins prebacuje režim. #ifndef je za programe koji koriste funkcije unosa podataka od oktobra. Ta biblioteka takođe ima INSERT režim za šablone za unos podataka, a #ifndef sprečava da se dva #define izraza sukobe.

`editor.h` definiše strukturu "edit_env", koja sadrži sve varijable koje se odnose na okruženje određenog poziva urednika. Kasnije, ako odlučimo da koristimo više prozora ili ako treba da pozovemo sekundarni uređivač iz primarnog, složićemo pojavu strukture deklarisane kao "ev" u "editor.c".

"editor.c" sadrži funkciju za uređivanje teksta prozora. Pozivate funkciju pod nazivom "text_editor" i prosledite joj adresu vašeg bafera za uređivanje, maksimalan broj redova u baferu i dužinu najduže linije. Morate da uspostavite prozor sa "estabtish_window u "window.c", i taj prozor mora biti u stanju da sadrži linije širine navedene u pozivu "text_editor". Drugim rečima, prozor mora biti širok najmanje koliko dužina linije plus dva za ivice prozora. Veličina bafera mora biti najmanje dužina reda puta broj redova i treba da sadrži tekstualne podatke ili razmake koji se mogu prikazati. Funkcija "text_editor" vraća pritisak na taster koji je prekinuo sesiju uređivanja. Ova vrednost će biti ili taster "Esc" ili komanda "QUIT" (Alt-Q kako je objavljeno). Program može testirati ovu vrednost da bi znao šta korisnik namerava da uradi sa baferom teksta.

Funkcija "text_editor" prikazuje tekst u prozoru i počinje da prihvata ključeve podataka ili komande od korisnika. Svakim pritiskom na taster poziva se funkcija na koju ukazuje pokazivač funkcije "status_line". Ovo omogućava aplikacijskom programu da prikaže informacije o statusu bafera kao što su brojevi stranica i redova i podešavanja režima. Da bi koristila ovu funkciju, aplikacija mora da inicijalizuje pokazivač na adresu funkcije koja prikazuje status. Koristićemo ovu funkciju sledećeg meseca.

Promenljiva pod nazivom "forcechar" se pojavljuje u redovima 84 i 85. Ako "forcechar" ima vrednost različitu od nule, ta vrednost se zamenjuje za sledeće pritiskanje tastera. Ovaj mehanizam omogućava spoljašnjem kodu da prinudi izvršenje komande. Eksterni kod se može definisati adresom u pokazivaču funkcije pod nazivom "editfunc". Ako inicijalizujete ovaj pokazivač na adresu funkcije, funkcija će biti pozvana kad god uređivač ne može da prepozna pritisak na taster. Vrednost pritiska na taster se prosleđuje u pozivu funkciji na liniji 244.

Funkcija može da vidi spoljnu strukturu pod nazivom "ev" da bi ispitala okruženje uređivača, može da modifikuje to okruženje i može da prisili izvršenje komande kada se vrati postavljanjem vrednosti komande u "forcechar". Ovaj mehanizam će se koristiti sledećeg meseca za dodavanje upravljanja datotekama, menija i pretraživanja teksta uređivaču. To je način na koji uređivač činimo proširivim bez modifikacije koda u samom uređivaču.

Kroki broj 6: Zastareli komentari

Većina koda u "editor.c" objašnjava sam sebe, kao što se C kod može objasniti bez opširnih komentara. Bar ja tako mislim. Meni to radi, a nadam se da će i vama. Moje prakse komentarisanja prate konvenciju koja identifikuje svrhu svake funkcije u komentaru na početku funkcije. Promenljive koje nisu očigledne dobijaju komentare koji opisuju njihovu svrhu. Tamo gde kod postane potpuno zamućen, ubaciću komentare kako bude napredovao, ali uglavnom više volim da pustim da kod sam sebe opisuje. Ova navika i sklonost proizilaze iz godina čitanja koda drugih gde su njihovi opsežni i opsežni komentari prethodili kodu generacijama modifikacija. Bio sam uljuljkan u verovanje u lepo napravljene komentare i samim tim bio sam podsvesno uslovljen da pretpostavim stvari koje nisu istinite. Ovo gubi vreme. Kasnije, kada moja zbunjenost dostigne nepodnošljiv nivo, pribegavam čitanju koda, samo da bih otkrio da se ne slaže sa komentarima. Bez obzira da li su komentari izjave o namerama koje nikada nisu realizovane ili tačni opisi koda više nisu na mestu; komentari govore jedno, a kod drugo.

Zbog ubeđenja i da bih sačuvao ostatke svog razuma, sada odbijam da čitam komentare koji su napisani kao pseudokod osim ako nisam siguran da program nikada nije modifikovan. Čak i tada nerado. Ti komentari se retko (ako ikad) održavaju dok se kod razvija ili kasnije kada se menja. Možda su vaša iskustva drugačija; možda strogo primenjeni standardi i procedure vašeg poslodavca sprečavaju da ovaj kvačicu ne ulazi u vašu radnju; a možda i veruješ u to. Ja ne.

Kod jezika C nije uvek njegova najbolja dokumentacija. To je, međutim, najpouzdanija izjava o tome šta se dešava u programu. Moja praksa mi dobro služi jer sam sklon da zapamtim šta znače ta zagonetna imena promenljivih, i više volim male funkcije sa jednostavnim svrhama. Ako funkcija funkcioniše, njena svrha se razume, a mala je, onda se može smatrati crnom kutijom, može se čitati i može joj se verovati. Nije da uvek praktikujem ono što propovedam; "text_editor" je velika funkcija, iako se uglavnom radi o mnogo malih slučajeva za prekidač pritiska na taster.

Uzgred, ni meni nije stalo da čitam kod koji je komentarisan. Daleka, van vidokruga #if DEBUG izjava ili još neograničeni /* token za početak komentara mogu da vam navedu da čitate gomile koda koji ne postoji.

S druge strane, moj drugar Bil Čejni kaže da bi svako ko ne pruži dovoljno komentara u programu asemblerskog jezika trebalo da za pokoru provede godinu dana održavajući COBOL/CICS programe drajvera ekrana za portorikanski sistem poreza na dohodak.

Primer za korišćenje uređivača teksta

"Testedit.c", Listing 3 je jednostavan primer upotrebe uređivača teksta. Kompilirajte i povežite "testedit.c" sa "editor.c" i "window.c". Pokrećete ga tako što ćete na komandnoj liniji dati ime tekstualnoj datoteci. Ovo nije standardna tekstualna datoteka; to je slika pravougaonog bafera uređivača, tako da kada prvi put pokrenete "testedit", možete dati ime datoteke koje ne postoji i program će je napraviti. "testedit" uspostavlja prozor, čita datoteku – ako postoji – u bafer i poziva funkciju "text_editor". Ako se taster Esc ne vrati, što znači da ste pritisnuli taster QUIT (Alt-Q), bafer se upisuje u datoteku koju ste imenovali u komandnoj liniji. Ako pritisnete Esc, program izlazi bez upisivanja bafera.

Ovaj program nije mnogo pametan. Njegova svrha je da pokaže kako da podesite, koristite i izađete iz uređivača teksta prozora. Ponuda za sledeći mesec uključuje novi uređivač u mali procesor teksta koji sam ranije pomenuo. Preduzeću drastične mere da testiram mali procesor teksta kada budem pisao kolumnu sledećeg meseca. Ja ću, barem privremeno, napustiti svoj voljeni XWrite i koristiti mali procesor teksta za rad tokom meseca, rizikujući plodove svog kreativnog rada novom i neproverenom uređivaču teksta. Ovo je, dragi čitaoci, posvećenost i posvećenost. Ništa manje ne očekujte od DDJ kolumniste.

[LISTING 1]

```c
/* ------------ editor.h ------------- */
#define ALT_Q 144
#define ALT_R 147
/* -------- configured editor commands ---------- */
#define BACKTAB         SHIFT_HT
#define NEXTWORD        CTRL_FWD
#define PREVWORD        CTRL_BS
#define TOPSCREEN       CTRL_T
#define BOTSCREEN       CTRL_B
#define BEGIN_BUFFER    CTRL_HOME
#define END_BUFFER      CTRL_END
#define BEGIN_LINE      HOME
#define END_LINE        END
#define DELETE_LINE     ALT_D
#define DELETE_WORD     CTRL_D
#define INSERT          INS
#define QUIT            ALT_Q
#define PARAGRAPH       F2
#define BEGIN_BLOCK     F5
#define END_BLOCK       F6
#define MOVE_BLOCK      F3
#define COPY_BLOCK      F4
#define DELETE_BLOCK    F8
#define HIDE_BLOCK      F9
#define REPAINT         ALT_R
/* ------- configured default modes ----------- */
#define TAB 4
#define REFORMING TRUE    /* auto paragraph reformat mode */
#ifndef INSERTING
#define INSERTING TRUE    /* insert/overwrite mode        */
#endif
/* ---------- editor prototype ---------------- */
int text_editor(char *, int, int);
/* ------- macros ------------ */
#define curr(x,y) (ev.bfptr+(y)*ev.wwd+(x))
#define lineno(y) ((unsigned)(ev.bfptr-ev.topptr)/ev.wwd+(y))
/* ---------- editor environment ------------- */
struct edit_env    {
    int envinuse;       /* TRUE if the env is in use  */
    struct wn *wdo;     /* the editor window          */
    int wwd;            /* width of edit window       */
    int wsz;            /* size (chars) of window     */
    char *topptr;       /* -> first char in buffer    */
    char *bfptr;        /* -> first char in window    */
    char *nowptr;       /* -> current char in buffer  */
    char *lstptr;       /* -> last nonblank char      */
    char *endptr;       /* -> last char in buffer     */
    int text_changed;   /* TRUE if text has changed   */
    int nolines;        /* no. of lines in buffer     */
    int blkbeg;         /* marked block: 1st line     */
    int blkend;         /* marked block: last line    */
    int curr_x, curr_y; /* current buffer coordinates */
    int edinsert;       /* toggled insert mode        */
    int reforming;      /* toggled para reform mode   */
};
```

[LISTING 2]

```c
/* ----------------------- editor.c ---------------------- */

#include <stdio.h>
#include <ctype.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <mem.h>
#include <ctype.h>
#include "window.h"
#include "editor.h"

#define NEXTTAB (TAB-(ev.curr_x%TAB))
#define LASTTAB ((ev.wwd/TAB)*TAB)
#define PREVTAB (((ev.curr_x-1)%TAB)+1)

struct edit_env ev;         /* the editor environment      */
int do_display_text = TRUE; /* turns display on/off        */
extern struct wn wkw;       /* the current window          */
int forcechar;              /* externally force a kb char  */
void (*status_line)(void);  /* called once each keystroke  */
void (*editfunc)(int);      /* for unknown keystrokes      */

/* ---------- local function prototypes ----------- */
static int lastword(void);
static void last_char(void);
static void test_para(int);
static int trailing_spaces(int);
static int first_wordlen(int);
static int last_wordlen(void);
static void paraform(int);
static int blankline(int);
static void delete_word(void);
static void delete_line(void);
static void delete_block(void);
static void mvblock(int);
static void carrtn(int);
static void backspace(void);
static void fore_word(void);
static int spaceup(void);
static void back_word(void);
static int spacedn(void);
static void forward(void);
static int downward(void);
static void upward(void);
static void display_text(int);
static void disp_line(int y);
static void findlast(void);

/* ----- Process text entry for a window. ---- */
int text_editor(char *bf, int editlines, int editwidth)
{
    int depart, i, c;
    int svx, svlw, tx, tabctr, wraplen;

    current_window();
    depart = FALSE;
    tabctr = 0;
    if (ev.envinuse == FALSE)    {
        ev.wdo = &wkw;
        ev.wwd = editwidth;
        ev.wsz = ev.wwd * ev.wdo->ht;
        ev.topptr = ev.bfptr = bf;
        ev.nolines = editlines;
        ev.endptr = bf + ev.wwd * ev.nolines;
        ev.edinsert  = INSERTING;
        ev.reforming = REFORMING;
        ev.envinuse = TRUE;
    }
    set_cursor_type(ev.edinsert ? 0x0106 : 0x0607);
    display_text(0);
    findlast();
    /* ------- read text/command from the keyboard ------ */
    while (depart == FALSE)    {
        ev.nowptr = curr(ev.curr_x, ev.curr_y);
        if (status_line)
            (*status_line)(); /* external status line func */
        gotoxy(ev.curr_x + 2, ev.curr_y + 2);
        if (tabctr)    {          /* expand typed tabs */
            --tabctr;
            c = ' ';
        }
        else
            c = forcechar ? forcechar : getkey();
        forcechar = 0;
        switch (c)    {
/* ------------ fixed editor commands ----------------- */
            case '\r':
                carrtn(ev.edinsert);
                break;
            case UP:
                upward();
                break;
            case DN:
                downward();
                break;
            case FWD:
                forward();
                break;
            case '\b':
            case BS:
                if (!(ev.curr_x || ev.curr_y))
                    break;
                backspace();
                if (ev.curr_x == ev.wwd - 1)
                    last_char();
                if (c == BS)
                    break;
                ev.nowptr = curr(ev.curr_x, ev.curr_y);
            case DEL:
                movmem(ev.nowptr+1,ev.nowptr,
                    ev.wwd-1-ev.curr_x);
                *(ev.nowptr+ev.wwd-1-ev.curr_x) = ' ';
                disp_line(ev.curr_y);
                test_para(ev.curr_x+1);
                ev.text_changed = TRUE;
                break;
            case PGUP:
                ev.curr_y = 0;
                do_display_text = FALSE;
                for (i = 0; i < ev.wdo->ht; i++)
                    upward();
                do_display_text = TRUE;
                display_text(0);
                break;
            case PGDN:
                ev.curr_y = ev.wdo->ht-1;
                do_display_text = FALSE;
                for (i = 0; i < ev.wdo->ht; i++)
                    downward();
                do_display_text = TRUE;
                display_text(0);
                ev.curr_y = 0;
                break;
            case '\t':
                if (ev.curr_x + NEXTTAB < ev.wwd)    {
                    if (ev.edinsert)
                        tabctr = NEXTTAB;
                    else
                        ev.curr_x += NEXTTAB;
                }
                else
                    carrtn(ev.edinsert);
                break;
/* -------- configured editor commands --------------- */
            case REPAINT:
                display_text(ev.curr_y);
                break;
            case BACKTAB:
                if (ev.curr_x < TAB)    {
                    upward();
                    ev.curr_x = LASTTAB;
                }
                else
                    ev.curr_x -= PREVTAB;
                break;
            case NEXTWORD:
                fore_word();
                break;
            case PREVWORD:
                back_word();
                break;
            case BOTSCREEN:
                ev.curr_y = ev.wdo->ht - 1;
                break;
            case TOPSCREEN:
                ev.curr_y = 0;
                break;
            case BEGIN_BUFFER:
                ev.curr_x = ev.curr_y = 0;
                ev.bfptr = ev.topptr;
                display_text(0);
                break;
            case END_BUFFER:
                do_display_text = FALSE;
                ev.curr_x = 0;
                while (downward())
                    if (curr(0,ev.curr_y) >= ev.lstptr)
                        break;
                do_display_text = TRUE;
                display_text(0);
                break;
            case BEGIN_LINE:
                ev.curr_x = 0;
                break;
            case END_LINE:
                last_char();
                break;
            case DELETE_LINE:
                delete_line();
                ev.text_changed = TRUE;
                break;
            case DELETE_WORD:
                delete_word();
                ev.text_changed = TRUE;
                test_para(ev.curr_x);
                break;
            case INSERT:
                ev.edinsert ^= TRUE;
                set_cursor_type(ev.edinsert ? 0x106 : 0x607);
                break;
            case ESC:
            case QUIT:
                depart = TRUE;
                break;
            case PARAGRAPH:
                paraform(0);
                ev.text_changed = TRUE;
                break;
            case BEGIN_BLOCK:
                ev.blkbeg = lineno(ev.curr_y) + 1;
                if (ev.blkbeg > ev.blkend)
                    ev.blkend = ev.blkbeg;
                display_text(0);
                break;
            case END_BLOCK:
                ev.blkend = lineno(ev.curr_y) + 1;
                if (ev.blkend < ev.blkbeg)
                    ev.blkbeg = ev.blkend;
                display_text(0);
                break;
            case MOVE_BLOCK:
                mvblock(TRUE);
                ev.text_changed = TRUE;
                break;
            case COPY_BLOCK:
                mvblock(FALSE);
                ev.text_changed = TRUE;
                break;
            case DELETE_BLOCK:
                delete_block();
                ev.text_changed = TRUE;
                display_text(0);
                break;
            case HIDE_BLOCK:
                ev.blkbeg = ev.blkend = 0;
                display_text(0);
                break;
            default:
                if (!isprint(c))    {
                    /* ---- not recognized by editor --- */
                    if (editfunc)    {
                        /* --- extended commands --- */
                        (*editfunc)(c);
                        findlast();
                        display_text(0);
                    }
                    else
                        putch(BELL);
                    break;
                }
                /* --- displayable char: put in buffer --- */
                if (ev.nowptr == ev.endptr-1 ||
                   (lineno(ev.curr_y)+1 >=
                       ev.nolines && ev.edinsert &&
                       *curr(ev.wwd-2, ev.curr_y) != ' '))  {
                    error_message("End of buffer...");
                    break;
                }
                if (ev.edinsert) /* --- if insert mode --- */
                    movmem(ev.nowptr,ev.nowptr+1,
                        ev.wwd-1-ev.curr_x);
                if (ev.nowptr < ev.endptr)    {
                    if (ev.nowptr >= ev.lstptr)
                        ev.lstptr = ev.nowptr + 1;
                    *ev.nowptr = (char) c; /* put in buff */
                    disp_line(ev.curr_y);
                }
                if (ev.nowptr == curr(ev.wwd-1, ev.curr_y) &&
                    c == ' ' && ev.edinsert)    {
                    if (strncmp(curr(0,ev.curr_y+1),
                            "        ",TAB) == 0)    {
                        carrtn(TRUE);
                        break;
                    }
                }
                else if (ev.endptr &&
                        *curr(ev.wwd-1, ev.curr_y) != ' ')    {
                    /* ------- word wrap is needed ------- */
                    ev.nowptr = curr(ev.wwd-1, ev.curr_y);
                    svx = ev.curr_x;   /* save x vector */
                    svlw = lastword(); /* last word on line?*/
                    ev.curr_x = ev.wwd-1;
                    if (*(ev.nowptr-1) != ' ')
                        back_word();
                    tx = ev.curr_x;
                    wraplen = last_wordlen();
                    if (trailing_spaces(ev.curr_y+1) <
                            wraplen+2)
                        carrtn(TRUE);
                    else if (strncmp(curr(0,ev.curr_y+1),
                            "        ",TAB) == 0)
                        carrtn(TRUE);
                    else    {
                        ev.nowptr = curr(0, ev.curr_y+1);
                        movmem(ev.nowptr,ev.nowptr+wraplen+1,
                            ev.wwd-wraplen-1);
                        setmem(ev.nowptr, wraplen+1, ' ');
                        movmem(curr(ev.curr_x,ev.curr_y),
                            ev.nowptr,wraplen);
                        setmem(curr(ev.curr_x,ev.curr_y),
                            wraplen, ' ');
                        disp_line(ev.curr_y);
                        downward();
                        disp_line(ev.curr_y);
                    }
                    if (svlw)
                        ev.curr_x = svx-tx;
                    else
                        ev.curr_x = svx, --ev.curr_y;
                }
                forward();
                ev.text_changed = TRUE;
                break;
        }
    }
    return c;
}

/* ----- see if a word is the last word on the line ------ */
static int lastword()
{
    int x = ev.curr_x;
    char *bf = curr(ev.curr_x, ev.curr_y);
    while (x++ < ev.wwd-1)
        if (*bf++ == ' ')
            return 0;
    return 1;
}

/* --- go to last displayable character on the line --- */
static void last_char()
{
    char *bf = curr(0, ev.curr_y);
    ev.curr_x = ev.wwd-1;
    while (ev.curr_x && *(bf + ev.curr_x) == ' ')
        --ev.curr_x;
    if (ev.curr_x && ev.curr_x < ev.wwd - 1)
        ev.curr_x++;
}

/* ----- test to see if paragraph should be reformed ----- */
static void test_para(int x)
{
    int ts, fw;
    int svb, sve;

    if (ev.reforming && ev.curr_y < ev.nolines)    {
        ts = trailing_spaces(ev.curr_y);
        fw = first_wordlen(ev.curr_y+1);
        if (fw && ts > fw)    {
            svb = ev.blkbeg, sve = ev.blkend;
            ev.blkbeg = ev.blkend = 0;
            paraform(x);
            ev.blkbeg = svb, ev.blkend = sve;
            if (svb)
                display_text(0);
        }
    }
}

/* ---- count the trailing spaces on a line ----- */
static int trailing_spaces(int y)
{
    int x = ev.wwd-1, ct = 0;
    char *bf = curr(0, y);
    while (x >= 0)    {
        if (*(bf + x) != ' ')
            break;
        --x;
        ct++;
    }
    return ct;
}

/* ----- count the length of the first word on a line --- */
static int first_wordlen(int y)
{
    int ct = 0, x = 0;
    char *bf = curr(0, y);
    while (x < ev.wwd-1 && *bf == ' ')
        x++, bf++;
    while (x < ev.wwd-1 && *bf != ' ')
        ct++, x++, bf++;
    return ct;
}

/* ----- count the length of the last word on a line --- */
static int last_wordlen()
{
    int ct = 0, x = ev.wwd-1;
    char *bf = curr(x, ev.curr_y);
    while (x && *bf == ' ')
        --x, --bf;
    while (x && *bf != ' ')
        --x, --bf, ct++;
    return ct;
}

/* ------------ form a paragraph -------------- */
static void paraform(int x)
{
    char *cp1, *cp2, *cpend, *svcp;
    int x1, y1, firstline = TRUE;
    int y = ev.curr_y;

    if (!ev.blkbeg)    {    /* ---- if block not marked ---- */
        if (blankline(lineno(y)+1))
            return;        /* next line is blank, no reform */
        ev.blkbeg=ev.blkend=lineno(y)+1; /* pseudoblock */
        ev.blkend++;
        y1 = y+1;
        while (ev.blkend < ev.nolines)    { /* look for para */
            if (strncmp(curr(0, y1++), "        ", TAB) == 0)
                break;
            ev.blkend++;
        }
        --ev.blkend;
    }
    if (lineno(y) != ev.blkbeg-1)
        x = 0;
    x1 = x;
    cp1 = cp2 = ev.topptr + (ev.blkbeg - 1) * ev.wwd + x;
    cpend = ev.topptr + ev.blkend * ev.wwd;
    while (cp2 < cpend)    {
        while (*cp2 == ' ' && cp2 < cpend)    {
            if (firstline)
                *cp1++ = *cp2, x1++;
            cp2++;
        }
        firstline = FALSE;
        if (cp2 == cpend)
            break;
        /* ---- at a word ---- */
        while (*cp2 != ' ' && cp2 < cpend)    {
            if (x1 >= ev.wwd-1)    {
                /* wrap the word */
                svcp = cp1 + (ev.wwd - x1);
                while (*--cp1 != ' ')
                    *cp1 = ' ',    --cp2;
                x1 = 0;
                ev.blkbeg++;
                cp1 = svcp;
                if (y < ev.wdo->ht)
                    disp_line(y++);
            }
            *cp1++ = *cp2++;
            x1++;
        }
        if (cp2 < cpend)
            *cp1++ = ' ', x1++;
    }
    while (cp1 < cpend)
        *cp1++ = ' ';
     ev.blkbeg++;
    if (y < ev.wdo->ht)
        disp_line(y++);
    firstline = ev.blkbeg;
     if (ev.blkbeg <= ev.blkend)    {
        delete_block();
        display_text(y);
    }
    ev.blkbeg = ev.blkend = 0;
    if (firstline)
        display_text(0);
}

/* ------- test for a blank line ---------- */
static int blankline(int line)
{
    char *cp = ev.topptr + (line-1) * ev.wwd;
    int x = ev.wwd;
    while (x--)
        if (*cp++ != ' ')
            break;
    return !(x > -1);
}

/* ------------- delete a word -------------- */
static void delete_word()
{
    int wct = 0;
    char *cp1, *cp2;

    cp1 = cp2 = curr(ev.curr_x, ev.curr_y);
    if (*cp2 == ' ')
        while (*cp2 == ' ' && ev.curr_x + wct < ev.wwd)
            wct++, cp2++;
    else    {
        while (*cp2 != ' ' && ev.curr_x + wct < ev.wwd)
            wct++, cp2++;
        while (*cp2 == ' ' && ev.curr_x + wct < ev.wwd)
            wct++, cp2++;
    }
    movmem(cp2, cp1, ev.wwd - ev.curr_x - wct);
    setmem(cp1 + ev.wwd - ev.curr_x - wct, wct, ' ');
    disp_line(ev.curr_y);
}

/* ----------- delete a line --------------- */
static void delete_line()
{
    char *cp1, *cp2;
    int len;

    cp1 = ev.bfptr + ev.curr_y * ev.wwd;
    cp2 = cp1 + ev.wwd;
    if (cp1 < ev.lstptr)    {
        len = ev.endptr - cp2;
        movmem(cp2, cp1, len);
        ev.lstptr -= ev.wwd;
        setmem(ev.endptr - ev.wwd, ev.wwd, ' ');
        display_text(ev.curr_y);
    }
}

/* ----------- delete a block ------------- */
static void delete_block()
{
    char *cp1, *cp2;
    int len;

    if (!ev.blkbeg || !ev.blkend)    {
        error_message("No block marked ...");
        return;
    }
    cp1 = ev.topptr + ev.blkend * ev.wwd;
    cp2 = ev.topptr + (ev.blkbeg - 1) * ev.wwd;
    len = ev.endptr - cp1;
    movmem(cp1, cp2, len);
    setmem(cp2 + len, ev.endptr - (cp2 + len), ' ');
    ev.blkbeg = ev.blkend = 0;
    ev.lstptr -= cp1 - cp2;
}

/* ------- move and copy text blocks -------- */
static void mvblock(int moving)
{
    char *cp1, *cp2, *hd;
    unsigned len;

    if (!ev.blkbeg || !ev.blkend)    {
        error_message("No block marked ...");
        return;
    }
    if (lineno(ev.curr_y) >= ev.blkbeg-1
            && lineno(ev.curr_y) <= ev.blkend-1)    {
        error_message("Don't move/copy a block into itself");
        return;
    }
    len = (ev.blkend - ev.blkbeg + 1) * ev.wwd;
    if ((hd = malloc(len)) == NULL)
        return;
    cp1 = ev.topptr + (ev.blkbeg-1) * ev.wwd;
    movmem(cp1, hd, len);
    cp2 = ev.topptr + lineno(ev.curr_y) * ev.wwd;
    if (moving)    {
        if (lineno(ev.curr_y) > ev.blkbeg-1)
            cp2 -= len;
        delete_block();
    }
    if (cp2+len <= ev.endptr)    {
        movmem(cp2, cp2 + len, ev.endptr - cp2 - len);
        movmem(hd, cp2, len);
        ev.lstptr += cp1 - cp2;
    }
    else
        error_message("Not enough room...");
    free(hd);
    ev.blkbeg = ev.blkend = 0;
    display_text(0);
}

/* ------- find the last character in the buffer -------- */
static void findlast()
{
    char *lp = ev.endptr - 1, *tp = ev.topptr;
    while (lp > tp && *lp == ' ')
        --lp;
    if (*lp != ' ')
        lp++;
    ev.lstptr = lp;
}

/* -------- carriage return -------- */
static void carrtn(int insert)
{
    int insct;
    char *cp = curr(ev.curr_x, ev.curr_y);
    char *nl = cp+((cp-ev.topptr)%ev.wwd);
    int ctl = 2;
    if (lineno(ev.curr_y) + 2 < ev.nolines)
        if (insert && nl < ev.endptr)    {
            insct = ev.wwd - ev.curr_x;
            while (ctl--)    {
                if (ev.endptr > cp + insct)    {
                    movmem(cp, cp+insct, ev.endptr-insct-cp);
                    setmem(cp, insct, ' ');
                }
                else if (ctl == 1)
                    setmem(cp, ev.endptr - cp, ' ');
                cp += insct * 2;
                insct = ev.curr_x;
            }
        }
    ev.curr_x = 0;
    downward();
    if (insert)    {
        ev.text_changed = TRUE;
        test_para(0);
        display_text(ev.curr_y-1);
        if (lineno(ev.curr_y) + 2 < ev.nolines)
            if ((ev.lstptr + ev.wwd) <= ev.endptr)
                if (ev.lstptr > curr(ev.curr_x, ev.curr_y))
                    ev.lstptr += ev.wwd;
    }
}

/* ------- move the buffer offset back one position ------ */
static void backspace()
{
    if (ev.curr_x == 0)    {
        if (ev.curr_y)
            ev.curr_x = ev.wwd - 1;
        upward();
    }
    else
        --ev.curr_x;
}

/* -------- move the buffer offset forward one word ------ */
static void fore_word()
{
    while (*ev.nowptr != ' ')    {
        if (spaceup() == 0)
            return;
        if (ev.curr_x == 0)
            break;
    }
    while (*ev.nowptr == ' ')
        if (spaceup() == 0)
            return;
}

static int spaceup()
{
    if (ev.nowptr >= ev.lstptr)
        return 0;
    ev.nowptr++;
    forward();
    return 1;
}

/* ------- move the buffer offset backward one word ------ */
static void back_word()
{
    spacedn();
    while (*ev.nowptr == ' ')
        if (spacedn() == 0)
            return;
    while (*ev.nowptr != ' ')    {
        if (ev.curr_x == 0)
            return;
        if (spacedn() == 0)
            return;
    }
    spaceup();
}

static int spacedn()
{
    if (ev.nowptr == ev.topptr)
        return 0;
    --ev.nowptr;
    backspace();
    return 1;
}

/* ----- move the buffer offset forward one position ----- */
static void forward()
{
    int ww = ev.wwd;
    if (++ev.curr_x == ww)    {
        downward();
        ev.curr_x = 0;
    }
}

/* ------- move the buffer offset down one position ------ */
static int downward()
{
    if (ev.curr_y < ev.wdo->ht - 1)    {
        ev.curr_y++;
        return 1;
    }
    else if ((ev.bfptr + ev.wsz) < ev.endptr)    {
        ev.bfptr += ev.wwd;
        if (do_display_text)    {
            scroll_window(1);
            disp_line(ev.wdo->ht-1);
        }
        return 1;
    }
    return 0;
}

/* -------- move the buffer offset up one position ------ */
static void upward()
{
    if (ev.curr_y)
        --ev.curr_y;
    else if ((ev.topptr + ev.wwd) <= ev.bfptr)    {
        ev.bfptr -= ev.wwd;
        if (do_display_text)    {
            scroll_window(0);
            disp_line(0);
        }
    }
}

/* ---- display lines in a window ------ */
static void display_text(y)
{
    while (y < ev.wdo->ht)
        disp_line(y++);
}

/* ---------- Display a line -------- */
static void disp_line(int y)
{
    char ln[81];

    if (lineno(y) >= ev.blkbeg-1)
        if (lineno(y) <= ev.blkend-1)    {
            textcolor(BLOCKFG);
            textbackground(BLOCKBG);
        }
    movmem(ev.bfptr+y*ev.wwd, ln, ev.wwd);
    ln[ev.wwd] = '\0';
    writeline(2, y+2, ln);
    textcolor(TEXTFG);
    textbackground(TEXTBG);
}
```

[LISTING THREE]

```c
/* --------- testedit.c ------------ */
#include <stdio.h>
#include <mem.h>
#include <conio.h>
#include "window.h"
#include "editor.h"

#define LNS 40               /* number of editor lines   */
#define WD  60               /* length of an editor line */
#define LF (1+(80-WD)/2)     /* leftmost column          */
#define TP (1+(25-LNS/2)/2)  /* top row                  */
#define RT LF+WD+1           /* rightmost column         */
#define BT TP+LNS/2+1        /* bottom row               */

char notes[LNS*WD];
void main(int, char **);

void main(int argc, char **argv)
{
    FILE *fd;
    if (argc > 1)    {
        setmem(notes, sizeof notes, ' ');
        if ((fd = fopen(argv[1], "r")) != NULL) {
            fread(notes, WD, LNS, fd);
            fclose(fd);
        }
        clear_screen();
        establish_window(LF,TP,RT,BT,TEXTFG,TEXTBG,FALSE);
        if (text_editor(notes, LNS, WD) != ESC)  {
            fd = fopen(argv[1], "w");
            fwrite(notes, WD, LNS, fd);
            fclose(fd);
        }
        delete_window();
        clear_screen();
        set_cursor_type(0x0607);
    }
}
```
