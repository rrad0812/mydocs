# C Programiranje 15

## Više C++ i korak bliže ANSI C-u

Al Stevens, oktobar '89

Naša kolekcija C++ alata počela je prošlog meseca jednostavnim menadžerom prozora. Prva klasa koju smo napravili bio je prozor koji se pojavljuje kada je deklarisan iz programa i nestaje kada izađe van opsega. Metode prozora uključuju sledeće:

- Preopterećeni operatori << koji dodaju znak, red ili blok teksta u prozor
- Stranica i skrolujte kroz prozor
- Sakrijte prozor i vratite ga na ekran
- Promenite boje prikaza prozora i podrazumevana mesta tabulatora
- Postavite i pročitajte kursor prozora
- Obrišite tekst iz celog prozora, od trenutne pozicije kursora do kraja trenutnog reda i do kraja prozora

Klasa Windov je postala osnovna klasa za tri jednostavne izvedene klase:

- prozor YesNo,
- prozor Message i
- prozor Error.

Te klase su ilustrovale nasleđivanje klasa koje podržava C++. Ovog meseca ćemo detaljnije razmotriti nasleđivanje pomoću klase iskačućeg menija koja je izvedena iz prozora. Zatim ćemo uvesti string tip podataka koji liči na stringove Basic-a.

## Menu Classes

Da bismo nastavili naše istraživanje C++-a, napravićemo klase koje implementiraju dve vrste menija: meni sa kliznom trakom sličan onima koje koriste programi kao što je Lotus 1-2-3 i iskačući meni sličan onima koje koristi SideKick. Klasa menija klizne trake, nazvana "SlideBar", je sopstvena nezavisna klasa. Klasa iskakajućeg menija, nazvana "PopdownMenu", izvedena je iz klase Window. Ovi meniji su slični onima koje smo razvili u tradicionalnom C prošle godine za TWRP i SMALLCOM projekte.

Klasa je novi tip podataka, opisan od strane programera da bi proširio jezik. Kada smo napravili prozor i njegove izvedene klase, efektivno smo dodali tip podataka. sa tim dodatkom, naši C++ programi mogu imati:

- chars, ints, longs, floats, doubles, structures, unions,
- Windows, Messages, Errors i YesNos.

Zatim ćemo dodati SlideBars i PopdownMenus. Izvedena klasa nasleđuje sve karakteristike osnovne klase iz koje je izvedena i dodaje svoje privatne i javne delove.

Naše klase SlideBar i PopdovnMenu izvode operacije slične jedna drugoj, ali sa različitim formatima menija. Kada deklarišete bilo koju klasu, kreira se i prikazuje meni, a od korisnika se traži da izvrši izbor. U zavisnosti od izbora korisnika, funkcija aplikacije se bira između onih koje povezujete sa selekcijama kada deklarišete klasu. Pogledajmo prvo klasu SlideBar.

### Klasa SlideBar

Kada deklarišete promenljivu SlideBar, određujete liniju ekrana na kojoj će se pojaviti meni, tekst izbora, prvi izbor koji će biti označen kursorom za izbor i pokazivače na funkcije povezane sa selekcijama. Čim deklarišete SlideBar, njegova funkcija konstruktora prikazuje meni i traži od korisnika da napravi izbor. Korisnik može da pomera kursor za izbor napred-nazad pritiskom na tastere sa strelicom udesno i nalevo i može da izvrši izbor pritiskom na taster Enter kada je kursor na željenom izboru. Korisnik takođe može da izvrši izbor pritiskom na prvo slovo naziva izbora. Ova konvencija zahteva da dodelite imena izbora sa jedinstvenim prvim slovima kada dizajnirate meni. Kada korisnik napravi izbor, pridružena funkcija se izvršava.

Kada deklarišete jednu od ovih klasa menija, vaš program ostaje u funkcijama članova i u funkcijama aplikacija koje su povezane sa selekcijama sve dok se funkcija konstruktora ne vrati. Funkcija konstruktora poziva funkciju člana privatnog otpremanja da upravlja odabirima korisnika i šalje funkcije vaših aplikacija. Klasa SlideBar ima funkciju člana "terminate" koju aplikativni programi pozivaju da bi rekli dispečerskoj funkciji da prekine obradu menija. Izjava u vašem programu koja prati deklaraciju SlideBar-a će se tada izvršiti, ali meni će ostati vidljiv sve dok SlideBar ne izađe van opsega.

Funkcija otpremljene aplikacije može da koristi funkciju člana "current_selection" da odredi koji je vertikalni izbor na meniju izazvao njeno slanje.

### Klasa PopdownMeni

Zatim hajde da razmotrimo klasu PopdownMenu. Po radu je sličan klasi SlideBar, ali meni ima oblik prozora i stoga je izveden iz klase Window. Kada deklarišete PopdovnMenu, navodite koordinate kolone i reda gde se prikazuje gornji levi ugao prozora PopdovnMenu. Ostali parametri inicijalizacije su isti kao oni u meniju SlideBar, a operacija je slična. Međutim, pomoću iskačućeg menija, korisnik pomera kursor za izbor gore i dole, a ne desno i levo.

Pored funkcija člana "terminate" i "current_selection", koje funkcionišu kao one u klasi SlideBar, klasa PopdownMenu uključuje dodatne funkcije. Iskačući meni može imati izbore koji su selektivno onemogućeni. To znači da su prikazani, ali nisu dostupni za izbor. Imaju jedinstvenu šemu boja, a kursor za izbor prelazi preko njih kada ga korisnik pomera gore-dole. Funkcije javnog člana "disable_selection" i "enable_selection" dozvoljavaju kodu aplikacije da onemogući i omogući određeni izbor menija.

PopdownMenus takođe podržava izbore za prebacivanje, one koji menjaju prekidač binarnog stanja, ali nemaju pridružene funkcije člana aplikacije za slanje. Oni prikazuju kvačicu pored svog imena ako je prekidač uključen i nema oznake ako je prekidač isključen. Funkcija javnog člana "test_toggle" omogućava aplikaciji da testira trenutnu vrednost prekidača izbora.

Definicije klasa SlideBar i PopdovnMenu pojavljuju se u menijima [Listing 1](#listing-1) je "menus.h". Program koji će koristiti bilo koju od ovih klasa uključiće ovu datoteku. Zajedno sa definicijama klasa su definicije šema boja za menije. Klasa Window od prošlog meseca omogućava inicijalizaciju i promenu boja prozora. Klase menija, međutim, pretpostavljaju da program koristi konzistentnu šemu boja za sve menije i ne zahteva od vas da identifikujete boje menija svaki put kada deklarišete meni. [Listing 2](#listing-2) je "menus.c", kod koji implementira klase. Da biste ih koristili, morate povezati svoj program sa objektnim datotekama koje kompajlirate iz ovih datoteka. Da biste koristili klasu PopdownMenu, biće vam potrebne datoteke "window.h" i "window.c" od prošlog meseca.

Svi programi u ovoj i prošlomesečnoj koloni kompajliraju se sa Zortech C++ kompajlerom, verzija 1.07.

[Listing 3](#listing-3) i [Listing 4](#listing-4) su "demoslid.c" i "demopop.c". Ovi programi pokazuju upotrebu dve klase menija. "Demoslid.c" definiše meni SlideBar sa četiri izbora. Svaka od selekcija izvršava funkciju u demoslid.c. Prve tri od ovih simuliranih aplikacionih funkcija deklarišu prozor koji funkcija koristi da bi se identifikovala. Nakon što korisnik pritisne taster, funkcija se vraća. Četvrta funkcija je izbor Quit. Koristi klasu YesNo (definisanu u "window.h" i o kojoj se raspravljalo prošlog meseca) da traži od korisnika da potvrdi komandu Quit. Ako korisnik kaže „da“, funkcija poziva funkciju člana "terminate" da kaže meniju da izađe.

"Demopop.c" je sličan "demoslid.c", ali pokazuje dodatne funkcije dostupne u klasi PopdownMenu. Deklariše PopdownMenu sa pet izbora. Ovi izbori se ponašaju na način koji sugeriše meni File uređivača. Postoje izbori za učitavanje datoteke, čuvanje datoteke i deklarisanje nove datoteke. Postoji preklopni izbor pod nazivom Opcija. Poslednji izbor je Prekini izbor.

Izbor Sačuvaj je prvobitno podešen da bude onemogućen izbor. Znak minus u nazivu teksta „-Save“ ga identifikuje kao takvog. Kad god koristite izbore Učitaj ili Novo, njihove poslate funkcije pozivaju javnu funkciju člana "enable_selection" da bi omogućile izbor Sačuvaj. Kada zatim koristite opciju Sačuvaj, njena poslata funkcija poziva funkciju "disable_selection" da bi se isključila.

Izbor opcija je prekidač. Implicitno je definisan kao takav pomoću pokazivača funkcije NULL koji je program "demopop.c" naveo za pridruženu funkciju aplikacija za izbor opcije. Kada korisnik odabere opciju Option, klasa automatski invertuje preklopnu postavku. Postavka za prebacivanje je predstavljena pojavom ili odsustvom simbola kvačice ('\xfb') kao poslednjeg znaka imena izbora. Koristimo prekidač Option u funkciji Quit da vidimo da li treba da koristimo klasu IesNo da bismo verifikovali zahtev za napuštanje. Ako je prekidač uključen, od korisnika ne tražimo verifikaciju. Ova upotreba služi da ilustruje mehaniku preklopnih izbora.

Kombinovanjem klase SlideBar sa nizom klasa PopdownMenu, mogli biste da napravite sistem menija gde je klizna traka na vrhu ekrana sa iskačućim menijima ispod svakog izbora kliznih traka. Ovo je vrsta sistema menija koji koriste mnogi programi. U svom razvoju dve klase menija, pokušao sam da pređem na sledeći logičan korak i razvijem te tradicionalne klizne trake/skočne menije. Zid na koji sam naleteo bio je ili granica Zortech C++ ili moje sopstveno neiskustvo sa C++ jezikom. Za drajver dvodimenzionalnog menija potrebni su pokazivači na nizove pokazivača funkcija ili nešto slično. Nisam uspeo da nateram ove konstrukcije da rade u domenu novog operatora ili kao parametri za preopterećene funkcije konstruktora. Takođe mi nije jasno kako se funkcija liste promenljivih argumenata u C++ i ANSI C uklapa u konstrukciju preopterećene funkcije. Ovi problemi su tipični za one sa kojima ćemo se vi i ja susresti dok pokušavamo da koristimo C++ na način na koji nas naša mašta pokreće, i, kada i ako ih rešim, podeliću rešenja.

## Klasa String

Kada sam prešao sa Basic-a na C, oplakivao sam gubitak string promenljive. K&R me je uverio da će nizovi znakova i standardne funkcije koje koriste pokazivače znakova, pažljivo, služiti istoj svrsi, ali sam propustio stari način, želeći od tada da mogu da kažem ovo u C programu:

```cpp
if (username == "Wynton")
    username = username + " Marsalis";
```

ili još bolje:

```cpp
username += " Marsalis";
```

Sa C, moramo da koristimo funkciju `strcat` da izvršimo takvu konkatenaciju, a prijemni niz mora biti dovoljno dugačak da primi dodatnu vrednost. Postoje i druge korisne string operacije u Basic-u, a ja ih želim u C-u već duže vreme.

Pa, ne želim više. C++ donosi tu mogućnost u jezik C. Ima, to jest, ako ubacite svoju sopstvenu klasu stringova, a to je upravo ono što ćemo da uradimo.

[Listing 5](#listing-5) je "strings.h", datoteka zaglavlja koja opisuje novu klasu, pod nazivom "string". [Listing 6](#listing-6) je "strings.c", kod koji implementira string klasu.

String ima jedan privatni deo, char pokazivač. Kada deklarišete string, funkcija konstruktora inicijalizuje pokazivač. Sve operacije na stringu koriste ovaj pokazivač. Postoje četiri konstruktorske funkcije za string klasu, koje podržavaju četiri različita načina na koje možete deklarisati string i ilustruju C++ tehniku za preopterećenje konstruktorskih funkcija.

C++ vam omogućava da preopterećujete funkcije. To znači da nekoliko funkcija može imati isto ime, ali različite tipove parametara. Prevodilac jezika odlučuje koju funkciju pozivate na osnovu parametara koje prosleđujete. (Kada bi C imao ovu funkciju, ne bi nam bile potrebne `strcpy` funkcija i `strncpy` funkcija, na primer. Jedna funkcija bi mogla da obradi obe operacije.) Međutim, preopterećenje funkcije zahteva korišćenje prototipova, a to je dobar zahtev.

Ovde su prikazana četiri načina deklarisanja stringa:

```cpp
string name1;            // null string
string name2("George");  // character pointer
string name3(name2);     // another string
string name4(80);        // length
```

Svaka od ovih deklaracija će uspostaviti string sa pokazivačem na odgovarajući niz znakova. Name1 string će sadržati pokazivač na null string. Ostala tri stringa će sadržati pokazivače na nizove znakova. Sva četiri konstruktora koriste prostor stringova preuzet iz besplatne prodavnice (C++ heap) sa C++ operatorom new. Svaki konstruktor pravi novu kopiju vrednosti stringa umesto da jednostavno ukazuje na vrednost inicijalizacije. String name4 ukazuje na niz od 81 karaktera sve sa nultom vrednošću.

**Dodeljivanje stringova**  
Kada deklarišete niz, postoji niz operacija koje možete da izvršite na njemu. Prvo razmotrimo operator dodele. Kada uspostavite promenljivu string, možete joj dodeliti ili niz znakova ili drugi string kao što je prikazano ovde.

```cpp
string oldstring;                   // declare two strings  
string newstring;

oldstring = "Hello, Dolly",         // assign an array  
newstring = oldstring;              // assign a string  
```

Ova dva dodeljivanja se postižu sa `operator=` preopterećenim funkcijama koje imaju pokazivač karaktera i string kao svoje parametre. U C++ možete preopteretiti unarne i binarne C operatore da biste radili sa klasama na način na koji dizajnirate. Ne možete promeniti način na koji operatori rade sa standardnim C tipovima podataka, ne možete kreirati operatore koji ne postoje u C-u, ne možete koristiti unarne operatore kao binarne, i tako dalje. Vaša upotreba preopterećenja operatora mora biti urađena na način koji ima smisla za leksičko skeniranje i parser jezičkog prevodioca. Kasnije ćemo pogledati više preopterećenja operatera.

**Funkcija stradr**  
Funkcija javnog člana "stradr" vraća adresu stringa. Definiše se kao funkcija `const char *`, što znači da vraća pokazivač na nešto što se ne može menjati. Ovde je namera da se spreči aplikacioni program da promeni vrednost stringa preko funkcije stradr. Sve promene stringova bi trebalo da se izvrše sa opisanim operacijama nizova uskoro. Ova namera se ne ostvaruje uvek. Zortech C++ vam ne dozvoljava da dodelite vrednost vraćenu iz funkcije običnom pokazivaču znakova. To vas sprečava da koristite poziv funkcije u izrazu koji bi promenio string. Na primer, sledeći kod je u redu prema Zortech-u:

```cpp
string myname("Joe")
char *cp = myname.stradr();
*cp = 'M';
```

Kompajler bi trebalo da vas upozori da druga izjava dodeljuje pokazivač na-konst običnom pokazivaču, ali to ne čini. Eksperimenti sa Turbo C i Microsoft C otkrivaju da oni izdaju takva upozorenja (naravno, u kontekstu C). Kasnije pogledajte ANSI kutak za više diskusije o ovoj okolnosti jer se odnosi na C, a ne na C++.

Zortech će izdati grešku ako isprobate ovaj kod:

```cpp
string myname("Joe");
*myname.stradr() = 'M';
```

**right, left i mid funkcija**  
Klasa string uključuje tri javne funkcije - člana dizajnirane da emuliraju funkcije podniza RIGHT$, LEFT$ i MID$ Basic-a. Ove funkcije vraćaju delove nizova kao nove nizove. na primer:

```cpp
string name("George Kingfish Stevens");

string firstname;
string middlename;
string lastname;

firstname = name.left(6);
middlename = name.mid(8, 8);
lastname = name.right(7);
```

Ovaj kod dodeljuje tri nulta stringa tri dela prvobitno inicijalizovanog imena.

**Konkatenacija stringova**  
Klasa stringova uključuje nekoliko načina na koje možete spojiti stringove. Pretpostavimo da imate nizove pod nazivom novoime, ime i prezime. Ovo su načini na koje možete spojiti nizove:

```cpp
newname = name + "Smith";
newname = name + lastname;

name = name + "Smith";
name = name + lastname;
name += "Smith";
name += lastname;
```

Povezivanje stringova ne zahteva od vas da obezbedite dovoljno prostora za dodatnu vrednost. Veličine stringova rastu u skladu sa njihovim potrebama.

## Relacioni operateri

Naša nova klasa stringova nam omogućava da pravimo relacione testove između stringova i između nizova i nizova znakova. Možete napraviti sledeće testove:

```cpp
if (name == "Sam") ...
if (name < othername) ...
```

i bilo koju drugu kombinaciju dva stringa ili jednog stringa i niza znakova gde je relacioni operator jedan od ovih:

```cpp
==, !=, <, >, <=, >=
```

Jedino ograničenje je da leva strana testa mora biti string klasa, a ne niz.

**Subscripts stringova**  
Možete da koristite operatore indeksnog pristupa `[]` za čitanje pojedinačnih vrednosti znakova stringa na način sličan onome kako radite sa regularnim nizovima znakova C. na primer:

```cpp
string name("Otis");
char ch = name[2];
```

Promenljiva karaktera "ch" će primiti znak 'i' iz stringa. Međutim, ne možete da uradite ovo:

```cpp
name[2] = 'e'; // invalid statement
```

jer vrednost koju vraća preopterećeni operator `[]` nije C `lvrednost`.

Možete, međutim, da koristite preopterećeni operator `+` da biste formirali lvalue. Sledeće operacije su važeće:

```cpp
ch = *(name+2);
*(name+2) = 'e';
```

String klasa predstavlja ono što verujem da je pravi potencijal za C++, njegovu sposobnost da proširi jezik C pomoću generičkih tipova podataka koji se mogu ponovo koristiti. Možda nemate želju da učinite da C više liči na Basic sa našom string klasom, ali vežba otkriva mogućnosti koje definicija klase dodaje jeziku C.

Jezik C++ je još uvek bez velike korisničke baze koja bi nas vodila ka standardnim konvencijama. ANSI C komitet je odlučio da ne preduzima standardizaciju C++-a. Stoga, sve dok Borland i Microsoft ne uvedu C++ kompajlere, zajedno sa integrisanim razvojnim okruženjima i hot-shot debagerima, PC programeri mogu da se ozbiljno pozabave tim. Ostaje nam da se nadamo da će se to desiti u ne tako dalekoj budućnosti, ali nije bilo najava. Do tada, C++ je još uvek divna studija o tome šta bi programiranje trebalo da bude, i ja vas ohrabrujem da se upustite u to.

## ANSI ugao: konstantno i promenljivo

ANSI Ks3J11 komitet je prošlog proleća podneo nacrt predloženog standarda za jezik C ANSI-ju na odobrenje, ali su se pojavile neke zapreke, uglavnom proceduralne ili birokratske. To bi uskoro trebalo da bude raščišćeno i pravi ANSI C standard bi trebalo da postoji, možda do trenutka kada pročitate ovu kolumnu.

Nacrt standardnog dokumenta nije baš čitanje kraj ognjišta. Na kraju će to biti konačan autoritet o tome kako C treba da radi, ali to nije ni tekst vodiča ni lako razumljiv referentni dokument. Programeri kompajlera će ga detaljno proučiti pokušavajući da se usklade. Mi, potrošači, moramo da verujemo našim piscima kompajlera da su pravilno protumačili i primenili standardni jezik.

Ovaj mali deo kolumne DDJ "C programiranje" je novi mesečni dodatak koji će se baviti nekim karakteristikama koje ANSI C standard dodaje jeziku C. Većina ovih funkcija je već implementirana u C kompajlerima koje sada koristite jer su pisci kompajlera pažljivo pratili razvoj predloženog standarda. Svakog meseca ćemo pogledati još jedan deo ANSI standarda.

### Kvalifikator tipa const

Konstantna promenljiva je ona koju vaš program ne može da modifikuje sa dodelom ili povećanjem ili smanjenjem. Možete da proglasite promenljivu kao `const` na jedan od ovih načina:

```c
const   int             i1;     // (1) Konstantni int
int     const           i2;     // (2) Konstantni int
const   int             *ip1;   // (3) Pokazivač na konstantni int
int     const           *ip2;   // (4) Pokazivač na konstantni int
int *   const           ip3;    // (5) Int pokazivač na konstantni int
const   int     *const  ip4;    // (6) Konstantni pokazivač na konstantni int
int     const   *const  ip5;    // (7) Konstantni pokazivač na konstantni int 
```

- Prva dva oblika deklarišu cele brojeve koji se ne mogu menjati. Jedini ispravan način da se unese vrednost u ovaj ceo broj ili bilo koju drugu promenljivu kvalifikovanu kao `const` je inicijalizacijom kao što je prikazano ovde.

  ```cpp
  const int i = 123;
  ```

- Treći i četvrti oblik su pokazivači na cele brojeve gde se celi brojevi ne mogu menjati.

- Peti oblik je pokazivač na ceo broj koji se može promeniti, ali sam pokazivač ne može da se promeni.
- Šesti i sedmi oblik su pokazivači koji se ne mogu menjati i koji ukazuju na cele brojeve koji se takođe ne mogu menjati.

**Napomena**  
Može se primetiti da su uzastopni `const` `int` i `int` `const` ekvivalentni.

Kvalifikator tipa `const` nije savršen. Postoje izuzeci u zaštiti koju će pružiti, a ANSI dokument odriče odgovornost za većinu njih, govoreći: "Ako se pokuša modifikovati objekat definisan tipom kvalifikovanim za `const` korišćenjem `lvalue` sa tipom koji nije kvalifikovan za `const`, ponašanje je nedefinisano." Mora da je postojao dobar razlog za to. Originalni K&R ne uključuje ključnu reč `const`, tako da razlog ne može biti zaštita postojećeg, pre-ANSI koda.

Šta sve ovo znači? Šta bi, na primer, trebalo da se desi ako biste inicijalizovali nekonstantni pokazivač sa adresom konstantne promenljive kao što je prikazano ovde?

```cpp
const int i;
int *ip = &i;
```

Neki kompajleri hrabro upozoravaju na ovaj kod. Verovatno bi trebalo da bude greška, ali ANSI specifikacija to ne predviđa. Nazovite to greškom i mislim da se ne slažete.

Pretpostavimo da prosledimo pokazivač trećeg oblika iznad funkciji koja očekuje (na osnovu svog prototipa i deklaracije) nekonstantni pokazivač. Razmotrite ovo:

```cpp
const char *ch = "123";
strcpy(ch, "456");
```

Još jednom, neki kompajleri izdaju upozorenja. Ovaj kod bi, međutim, trebalo da bude greška, jer je funkcija `strcpy` definisana da očekuje normalan pokazivač kao prvi parametar i da će se, u stvari, promeniti gde god taj pokazivač pokazuje. Ako ignorišete upozorenje ili koristite kompajler koji ga ne izdaje, ključna reč `const` je bezvredna u ovom kontekstu.

ANSI specificira "nedefinisano" ponašanje kada se `const` koristi zajedno sa deklaracijom funkcije, što znači da sledeći kod može, ali i ne mora dati željene rezultate:

```cpp
const char *myaddr(void);
```

Čini se da ANSI pozicija (ili nepozicija) ostavlja otvorenim šta bi trebalo da se desi kada jedna datoteka izvornog koda proglasi eksternu promenljivu kao `const`, a druga istu promenljivu kao non-const. Turbo C, na primer, ne nudi zaštitu za `const` promenljivu ako druga izvorna datoteka izostavi kvalifikator tipa `const`. Prikladnije ponašanje zavisilo bi od povezivača koji poznaje kvalifikaciju tipa `ekstern` varijabli.

Prema ANSI, ako deklaracija strukture uključuje kvalifikator tipa `const`, članovi strukture su `const`. Microsoft C je u skladu sa ovim pravilom, ali Turbo C ne. ANSI-jev izraz ovog pravila je nejasan i implicitan i verovatno podleže tumačenju.

Ako je niz kvalifikovan `const`, to znači da su njegovi elementi `const`.

**Kvalifikator volatile**  
Promenljiva `volatile` je ona koja se može modifikovati iz eksternog, asinhronog izvora, kao što je rutina usluge prekida. Njegova svrha je da omogući kompajlerima da zaobiđu neke optimizacije prilikom rukovanja promenljivom. Na primer, možda imate globalnu promenljivu sa kojom vaš program radi. Dolazi do hardverskog prekida, a rutina usluge prekida menja tu promenljivu. Ako vaš kompajler nije svestan da bi se takve modifikacije mogle desiti, kompajlirani kod možda čuva promenljivu u registru ili na steku, a ne na svojoj određenoj memorijskoj lokaciji. Ako promenljivu kvalifikujete kao `volatile`, kompajler tada zna da uvek drži vrednost na lokaciji dostupnoj spoljnom uticaju. Ovo bi moglo imati nezgodne posledice. Možda će biti neophodno da kompajlirani kod onemogući prekide kad god radi sa promenljivom, na primer, što može dovesti do problema sa vremenom.

Očigledno, kvalifikator nestalnog tipa (`volatile`) nema značenje kada se primeni na automatsku promenljivu. Statičke promenljive deklarisane unutar funkcija mogu se modifikovati prekidima ako se prekinuta funkcija rekurzivno poziva iz rutine usluge prekida, tako da su podložne prednostima kvalifikatora `volatile`, ali automatske promenljive su obično na steku ili u registrima i svaki rekurzivni poziv funkcije ima sopstvene kopije automatskih promenljivih.

**Konstante, volatile promenljive**
ANSI obezbeđuje promenljivu koja ima kvalifikatore tipa `const` i `volatile`. Ako kodirate ovu izjavu:

```cpp
extern const volatile int x;
```

promenljiva postoji negde drugde na način da se može modifikovati prekidom, ali lokalna funkcija je ne može modifikovati.

## LISTING 1

```cpp
// ------------ menus.h

#ifndef MENUS
#define MENUS

#include "window.h"

#define MAXSELECTIONS 12
#define CHECK '\xfb' // IBM Graphics character set check mark

#define MENUFG CYAN
#define MENUBG BLUE
#define SELECTFG BLACK
#define SELECTBG WHITE
#define DISABLEFG LIGHTGRAY
#define DISABLEBG BLUE

//
//   SLIDING BAR MENUS
//
class SlideBar    {
    int row;                       // menu screen row
    void (**mfunc)(SlideBar&);     // selection functions
    char **mtext;                  // selection titles
    int selections;                // number of selections
    int quitflag;                  // flag for appl to say quit
    unsigned *msave;               // save area for menu bar
    int selection;                 // selection position
    int dispatch(int sel, int titlewidth);
public:
    SlideBar(int line, char **text, int sel,
        void (**func)(SlideBar&));
    ~SlideBar();
    void terminate(void)
        { quitflag = 1; }
    int current_selection(void)
        { return selection; }
};

//
//    POPDOWN MENUS
//
class PopdownMenu : Window    {
    int selections;               // number of selections
    void (**mfunc)(PopdownMenu&); // selection functions
    char **text;                  // address of menu text
    int quitflag;                 // flag for appl to say quit
    int selection;                // current selection position
    // --- private methods
    int get_selection(int sel);
    int dispatch(int sel);
    int menuheight(char **text);
    int menuwidth(char **text);
    void operator<<(char **text);
public:
    PopdownMenu(unsigned left, unsigned top,
            char **text, int sel, void (**func)(PopdownMenu&));
    ~PopdownMenu(){};
    void terminate(void)
        { quitflag = 1; }
    void disable_selection(int sel)
        { *text[sel-1] = '-'; }
    void enable_selection(int sel)
        { *text[sel-1] = ' '; }
    int test_toggle(int selection);
    int current_selection(void)
        { return selection; }
};

#endif
```

## LISTING 2

```cpp
// ----------- menus.c

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <conio.h>
#include "menus.h"
#include "console.h"

static void select(int row,int sel, char *ttl,int set,int wd);
#define ON  1
#define OFF 0

//
//   SLIDING BAR MENUS
//

// ----------- constructor for a sliding menu bar
SlideBar::SlideBar(int line, char **text, int sel,
        void (**func)(SlideBar&))
{
    savecursor();
    hidecursor();
    initconsole();
    // ------ menu variables
    quitflag = 0;
    mfunc = func;
    mtext = text;
    // -------- save video memory
    msave = new unsigned[SCREENWIDTH];
    row = min(line-1, 24);
    savevideo(msave, row, 0, row, SCREENWIDTH-1);
    // --------- display the menu bar
    colors(MENUFG, MENUBG);
    setcursor(0, row);
    int cols = SCREENWIDTH;
    while (cols--)
        window_putc(' ');
    // ---- compute the width of the selection texts
    int titlewidth = 0;
    for (int i = 0; mtext[i] && i < MAXSELECTIONS; i++)
        titlewidth = max(titlewidth, strlen(mtext[i]));
    // ----- save the selection count
    selections = i;
    // ------ display the selection texts
    for (i = 0; i < selections; i++)
        select(row, i+1, mtext[i], OFF, titlewidth);
    // ------- dispatch the menu's selections
    dispatch(sel, titlewidth);
}

// ----------- destructor for a menu bar
SlideBar::~SlideBar(void)
{
    restorevideo(msave, row, 0, row, SCREENWIDTH-1);
    delete msave;
    restorecursor();
    unhidecursor();
}

// ------ navigate the menu and dispatch a chosen function
int SlideBar::dispatch(int sel, int titlewidth)
{
    savecursor();
    int sliding = 1;
    if (sel)
        selection = sel;
    while (sliding)    {
        // ---- highlight the menu bar selection
        select(row, selection, mtext[selection-1],
                                        ON,titlewidth);
        // ----- read a selection key
        int c = getkey();
        switch (c)    {
            case ESC:
                // ----- ESC key quits
                sliding = 0;
                break;
            case FWD:
                // ------ right-arrow cursor key
                select(row, selection, mtext[selection-1],OFF,
                    titlewidth);
                if (selection++ == selections)
                    selection = 1;
                break;
            case BS:
                // ------ left-arrow cursor key
                select(row, selection, mtext[selection-1],OFF,
                    titlewidth);
                if (--selection == 0)
                    selection = selections;
                break;
            default:
                // ---- test for 1st letter match
                for (int i = 0; i < selections; i++)
                    if (tolower(c) == tolower(mtext[i][1]))   {
                        // -- match, turn off current selection
                        select(row, selection,
                            mtext[selection-1],
                            OFF,titlewidth);
                        // --- turn on new selection
                        selection = i+1;
                        select(row, selection,
                            mtext[selection-1],
                            ON,titlewidth);
                        break;
                    }
                if (i == selections)
                    break;
            case '\r':
                // ------- ENTER key = user selection
                if (mfunc[selection-1])
                    (*mfunc[selection-1])(*this);
                    sliding = !(quitflag == 1);
                break;
        }
    }
    restorecursor();
    select(row,selection,mtext[selection-1],OFF,titlewidth);
    return quitflag ? 0 : selection;
}

// --- set or clear the highlight on a menu bar selection
static void select(int row,int sel,char *ttl,int set,int wd)
{
    setcursor(5+(sel-1)*wd, row);
    if (set == OFF)
        colors(MENUFG, MENUBG);
    else
        colors(SELECTFG, SELECTBG);
    window_printf(ttl);
}

//
//    POPDOWN MENUS
//

// -------- constructor for the PopdownMenu
PopdownMenu::PopdownMenu(unsigned left, unsigned top,
            char **mtext, int sel, void (**func)(PopdownMenu&))
                : (left, top, left+1+menuwidth(mtext),
                    top+1+menuheight(mtext), MENUFG, MENUBG)
{
    *this << mtext;
    mfunc = func;
    text = mtext;
    selection = sel;
    selections = menuheight(text);
    // ------ dispatch the menu selection
    dispatch(sel);
}

// ------- write text selections into the popdown menu
void PopdownMenu::operator<<(char **mtext)
{
    hidecursor();
    int y = 0;
    // ----- a NULL-terminated array of character pointers
    text = mtext;
    while (*mtext != NULL)    {
        cursor(0, y++);
        char hold = **mtext;
        if (**mtext == '-')    {
            set_colors(DISABLEFG, DISABLEBG);
            **mtext = ' ';
        }
        *this << *mtext;
        **mtext++ = hold;
        set_colors(MENUFG, MENUBG);
    }
    unhidecursor();
}

// ------------ get a popdown menu selection
int PopdownMenu::get_selection(int sel)
{
    // ---- set the initial selection
    if (sel)
        selection = sel;
    int selecting = 1;
    int c;
    while (selecting)    {
        // ------- display the menu's selection texts
        *this << text;
        // ------ watch for disabled selections
        if (**(text+selection-1)=='-')
            c = DN;        // force a key to
                        // bypass a disabled selection
        else    {
            // ---- highlight the current selection
            cursor(0, selection-1);
            set_colors(SELECTFG, SELECTBG);
            *this << *(text + selection - 1);
            set_colors(MENUFG, MENUBG);
            hidecursor();
            c = getkey();    // --- read the next keystroke
        }
        switch (c)    {
            case ESC:
            case FWD:
            case BS:
                // ---- ESC,FWD, or BS will terminate selection
                selecting = 0;
                break;
            case UP:
                // ------- up-arrow cursor key
                do
                    if (--selection == 0)
                        selection = selections;
                while (**(text+selection-1) == '-');
                break;
            case DN:
                // ------- down-arrow cursor key
                do
                    if (selection++ == selections)
                        selection = 1;
                while (**(text+selection-1) == '-');
                break;
            default:
                // ----- other key, test first letter match
                for (int i = 0; i < selections; i++) {
                    if (tolower(c) == tolower(text[i][1]) &&
                             *(text[i]) != '-')   {
                        selection = i+1;
                        selecting = 0;
                    }
                }
                break;
            case '\r':
                // ---- ENTER key is a selection
                selecting = 0;
                break;
        }
    }
    return c == '\r' ? selection : c;
}

// ------------ get and dispatch a popdown menu selection
int PopdownMenu::dispatch(int sel)
{
    int upanddown = 1;
    while (upanddown)    {
          // ---------- read a user selection
        sel = get_selection(sel);
        switch (sel)    {
               // -------- these keys exit the menu
            case FWD:
            case BS:
            case ESC:
                upanddown = 0;
                break;
            default:
                // -------- user has made a menu selection
                if (mfunc[selection-1]) {
                    // ----- execute a menu selection function
                    hidewindow();
                    (*mfunc[selection-1])(*this);
                    upanddown = !(quitflag == 1);
                    restorewindow();
                }
                else    {
                    // ----- no function, must be a toggle
                    char *cp = text[selection-1];
                    cp += strlen(cp)-1;
                    if (*cp == ' ')
                        *cp = CHECK;
                    else if ((*cp & 255) == CHECK)
                        *cp = ' ';
                }
                break;
        }
    }
    return sel == ESC ? ESC : 0;
}

// --------- compute the height of a popdown menu
int PopdownMenu::menuheight(char **text)
{
    int height = 0;
    while (text[height])
        height++;
    return height;
}

// --------- compute the width of a popdown menu
int PopdownMenu::menuwidth(char **text)
{
    int width = 0;
    while (*text)    {
        width = max(width, strlen(*text));
        text++;
    }
    return width;
}

// ----- test the setting of a toggle selection
int PopdownMenu::test_toggle(int sel)
{
    char *cp = text[sel-1];
    cp += strlen(cp)-1;
    return (*cp & 255) == CHECK;
}
```

## LISTING 3

```cpp
// ---------- demoslid.c

#include <stddef.h>
#include <conio.h>
#include <stdio.h>
#include "menus.h"
#include "console.h"

// ----------- File menu
static char *fmenu[] = {
    " Load ",
    " Save ",
    " New  ",
    " Quit ",
    NULL
};

static void load(SlideBar&);
static void save(SlideBar&);
static void newfile(SlideBar&);
static void quit(SlideBar&);

static void (*ffuncs[])(SlideBar&)={load,save,newfile,quit};

void main(void)
{
    SlideBar menu(1, fmenu, 1, ffuncs);
}

static void load(SlideBar& menu)
{
    Window wnd(20,10,40,20,BLACK,CYAN);
    wnd.title("(Stub Function)");
    wnd << "\n\n\n\n   LOAD A FILE";
    getkey();
}

static void save(SlideBar &menu)
{
    Window wnd(20,10,40,20,YELLOW,RED);
    wnd.title("(Stub Function)");
    wnd << "\n\n\n\n  SAVE A FILE";
    getkey();
}

static void newfile(SlideBar &menu)
{
    Window wnd(20,10,40,20,YELLOW,RED);
    wnd.title("(Stub Function)");
    wnd << "\n\n\n\n   NEW FILE";
    getkey();
}

static void quit(SlideBar& menu)
{
    YesNo yn("Quit");
    if (yn.answer)
        menu.terminate();
}
```

## LISTING 4

```cpp
// ---------- demopop.c

#include <stddef.h>
#include <conio.h>
#include <stdio.h>
#include "menus.h"
#include "console.h"

// ----------- File menu
static char *fmenu[] = {
    " Load    ",
    "-Save    ",
    " New     ",
    " Option  ",
    " Quit    ",
    NULL
};

static void load(PopdownMenu&);
static void save(PopdownMenu&);
static void newfile(PopdownMenu&);
static void quit(PopdownMenu&);

static void (*ffuncs[])(PopdownMenu&) =
    { load, save, newfile, NULL, quit };

void main(void)
{
    PopdownMenu menu(20, 10, fmenu, 1, ffuncs);
}

static void load(PopdownMenu& menu)
{
    Window wnd(20,10,40,20,BLACK,CYAN);
    wnd.title("(Stub Function)");
    wnd << "\n\n\n\n   LOAD A FILE";
    menu.enable_selection(2);    // enable the save command
    getkey();
}

static void save(PopdownMenu &menu)
{
    Window wnd(20,10,40,20,YELLOW,RED);
    wnd.title("(Stub Function)");
    wnd << "\n\n\n\n  SAVE A FILE";
    menu.disable_selection(2);    // disable the save command
    getkey();
}

static void newfile(PopdownMenu &menu)
{
    Window wnd(20,10,40,20,YELLOW,RED);
    wnd.title("(Stub Function)");
    wnd << "\n\n\n\n   NEW FILE";
    menu.enable_selection(2);    // enable the save command
    getkey();
}

static void quit(PopdownMenu& menu)
{
    if (menu.test_toggle(4))
        menu.terminate();
    else    {
        YesNo yn("Quit");
        if (yn.answer)
            menu.terminate();
    }
}
```

## LISTING 5

```cpp
// -------- strings.h

#ifndef STRINGS
#define STRINGS

#include <string.h>

class string    {
    char *sptr;
public:
    //          CONSTRUCTORS
    // -------- construct a null string
    string(void);
    // ------- construct with a char * initializer
    string(char *s);
    // ------- construct with another string as initializer
    string(string& s);
    // -------- construct with just a size
    string(int len);

    //          DESTRUCTOR
    ~string(void) { delete sptr; }

    //          MEMBER FUNCTIONS
    // ------ return the address of the string
    const char *stradr(void) { return sptr; }

    //          SUBSTRINGS
    // ------ substring: right len chars
    string right(int len);
    // ------ substring: left len chars
    string left(int len);
    // ------ substring: middle len chars starting from where
    string mid(int len, int where);

    //          ASSIGNMENTS
    // -------- assign a char array to a string
    void operator=(char *s);
    // ---------- assign a string to a string
    void operator=(string& s) { *this = s.sptr; }

    //          CONCATENATORS
    // ------- 1st concatenation operator (str1 += char *)
    void operator+=(char *s);
    // ------- 2nd concatenation operator (str1 += str2;)
    void operator+=(string& s) { *this += s.sptr; }
    // ------- 3rd concatenation operator (str1 = str2+char*;)
    string operator+(char *s);
    // ------- 4th concatenation operator (str1 = str2 + str3;)
    string operator+(string& s) { return *this + s.sptr; }

    //          RELATIONAL OPERATORS
    int operator==(string& s) { return strcmp(sptr,s.sptr)==0;}
    int operator!=(string& s) { return strcmp(sptr,s.sptr)!=0;}
    int operator<(string& s)  { return strcmp(sptr,s.sptr)< 0;}
    int operator>(string& s)  { return strcmp(sptr,s.sptr)> 0;}
    int operator<=(string& s) { return strcmp(sptr,s.sptr)<=0;}
    int operator>=(string& s) { return strcmp(sptr,s.sptr)>=0;}
    int operator==(char *s)   { return strcmp(sptr,s)==0; }
    int operator!=(char *s)   { return strcmp(sptr,s)!=0; }
    int operator<(char *s)    { return strcmp(sptr,s)< 0; }
    int operator>(char *s)    { return strcmp(sptr,s)> 0; }
    int operator<=(char *s)   { return strcmp(sptr,s)<=0; }
    int operator>=(char *s)   { return strcmp(sptr,s)>=0; }

    //          SUBSCRIPTORS
    char operator[](int n) { return *(sptr + n); }
    char* operator+(int n) { return sptr + n; }
};

#endif
```

## LISTING 6

```cpp
// -------- strings.c

#include <stddef.h>
#include <stream.hpp>
#include "strings.h"

// -------- construct a null string
string::string(void)
{
    sptr = new char;
    *sptr = '\0';
}
// ------- construct with a char * initializer
string::string(char *s)
{
    sptr = new char[strlen(s)+1];
    strcpy(sptr, s);
}
// ------- construct with another string as initializer
string::string(string& s)
{
    sptr = new char[strlen(s.sptr)+1];
    strcpy(sptr, s.sptr);
}
// -------- construct with just a size
string::string(int len)
{
    sptr = new char[len+1];
    memset(sptr, 0, len+1);
}
// -------- assign a char array to a string
void string::operator=(char *s)
{
    delete sptr;
    sptr = new char[strlen(s)+1];
    strcpy(sptr, s);
}
// ------- 1st concatenation operator (str1 += char *;)
void string::operator+=(char *s)
{
    char *sp = new char[strlen(sptr) + strlen(s) + 1];
    strcpy(sp, sptr);
    strcat(sp, s);
    delete sptr;
    sptr = sp;
}
// ------- 3rd concatenation operator (str1 = str2 + char*;)
string string::operator+(char *s)
{
    string tmp(*this);
    tmp += s;
    return tmp;
}
// ------ substring: right len chars
string string::right(int len)
{
    string tmp(sptr + strlen(sptr) - len);
    return tmp;
}
// ------ substring: left len chars
string string::left(int len)
{
    string tmp(len+1);
    strncpy(tmp.stradr(), sptr, len);
    return tmp;
}
// ------ substring: middle len chars starting from where
string string::mid(int len, int where)
{
    string tmp(len+1);
    strncpy(tmp.stradr(),sptr+where-1,len);
    return tmp;
}
```
