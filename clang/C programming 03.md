
# C programiranje 03

## Kontrola ekrana, Programiranje kao umetnost (?) i C ++

Al Stevens, oktobar '88

Prošlog meseca smo započeli programski projekat "C Column" sa bibliotekom video prozora opšte namene za IBM PC i kompatibilne računare. Ovo izdanje nastavlja taj projekat sa setom softverskih alata orijentisanih na prozore koji koriste funkcije prozora od prošlog meseca. Do sada nismo naterali program da uradi bilo šta; to dolazi kasnije. Konačni program će biti drajver komunikacionih usluga koji će koristiti pretplatnici na on-line usluge.

Ovog meseca dodajemo mogućnost menija prozora i drajver ekrana za unos podataka. Ovi alati su dovoljno opšti da ih koristite u drugim projektima dok čekate komunikacionu aplikaciju za koju su namenjeni. Alati su napisani u C i kompajlirani su sa Turbo C, verzija 1.5.

### Prozorski meni

Program koji pravimo koristi meni sa kliznom trakom na vrhu ekrana sa nizom izbora prikazanih horizontalno preko trake. Izbor može imati pridruženi iskačući meni. Da bismo izgradili ovu mogućnost, koristićemo funkciju menija opšte namene koju vode tabele struktura. Strukture su definisane izvan funkcija menija i opisuju prikaze menija i softver koji izvršava svaki izbor menija. Kasniji alati, kao što je uređivač teksta, koristiće ove menije. Tako će biti i sa komunikacionim programom aplikacije.

[Listing 1](#listing-1) je "menu.h". Ova datoteka će biti uključena u programe koji moraju da opisuju i izvršavaju menije koristeći funkcije menija. Meni opisujete tako što deklarišete i inicijalizujete niz struktura koje su imenovane pomoću tipedef "MENU". Svaki element u nizu opisuje izbor na kliznom meniju na vrhu ekrana. Članovi strukture opisuju selekciju. Evo opisa svakog člana:

- **mname**:  
Pokazivač na ime selekcije kako će biti prikazano na kliznoj traci.

- **mhlpmsg**:  
Pokazivač na duži niz koji će biti prikazan u donjem redu ekrana kada je izbor istaknut. Ovaj niz se koristi za pojačavanje značenja izbora tako što se opisuje korisniku.

- **mselcs**: Pokazivač na niz pokazivača znakova, od kojih svaki pokazuje na tekst selekcije u padajućem meniju povezanom sa menijem klizne trake. Ovaj niz mora biti završen NULL pokazivačem. Ako je NULL pokazivač na prvoj poziciji, neće se pojaviti iskačući meni, a sam izbor menija bočne trake će izvršiti jednu komandu.

- **mshelp**: Niz pokazivača karaktera, od kojih svaki ukazuje na mnemoniku prozora pomoći za pridruženi izbor u iskačućem meniju. Ovo će biti objašnjeno kasnije kada instaliramo kontekstno osetljive prozore pomoći.

- **mskeys**: Pokazivač na niz znakova koji sadrži pritiske na tastere koji će izvršiti iskačuće komande. Ova funkcija omogućava da se meni izvrši na jedan od dva načina – pozicioniranje kursora menija na izbor i pritisak na taster Enter, ili pritisak na određeni taster za izbor kao što je navedeno u nizu mskeis.

- **func**: Niz pokazivača funkcija koji upućuju na funkcije koje će biti izvršene pridruženim izborima iz padajućeg menija.

- **lastvsel**: Koristi ga menadžer menija za pamćenje najnovijeg vertikalnog izbora; treba da bude inicijalizovan na nultu vrednost.

Sa nizom tako inicijalizovanih struktura MENU, pozivate funkciju menija __select, prosleđujući joj adresu strukture i ceo broj koji govori koji od horizontalnih izbora treba da bude istaknut kada se meni prvi put prikaže. Obično ceo broj ima vrednost 1.

[Listing 2](#listing-2) je "menu.c", koji sadrži funkcije biblioteke koje prikazuju menije, dobijaju izbor korisnika i izvršavaju odgovarajuće funkcije.

[Listing 3](#listing-3) je "testmenu.c", primer jednostavnog programa menija koji pokazuje kako se softver menija može koristiti. Primer je sažetak menija koji će se koristiti u okviru uređivača teksta dela komunikacionog softverskog paketa. Primer ne radi ništa. To samo ilustruje kako se koriste funkcije menija. Kompilirajte ga i povežite ga sa funkcijama menija i prozora i on će prikazati i kretati se po menijima. Slika 1, strana 111, prikazuje menije onako kako su prvi put prikazani. Tasteri sa strelicama biraju iskačuće menije i pomeraju trake kursora. Taster Enter bira stavku menija. Meni sa opcijama pokazuje primer kako možete da koristite ove menije za uključivanje i isključivanje režima rada.

Kompilirajte i povežite "testmenu.c" sa "menu.c" i "window.c" iz prošlomesečne C kolumne. Biće vam potreban "menu.h" i "window.h" od prošlog meseca.

Možete da napravite ugnežđene slojeve ovih menija tako što ćete pozvati menu_select iz funkcije koja je izvršena iz menija višeg nivoa. U stvari, meni urednika koji smo simulirali u ovom primeru biće pozvan iz uređivačke funkcije koja se poziva iz menija školjke.

Funkcije koje poziva menadžer menija će dobiti dva cela broja kao parametre. Ovi celi brojevi su horizontalni izbor iz kliznog menija

Funkcije koje poziva menadžer menija će dobiti dva cela broja kao parametre. Ovi celi brojevi su horizontalni izbor sa kliznog menija (1, 2,.., n) i vertikalni izbor iz padajućeg menija (1, 2,.., n). Ove vrednosti omogućavaju funkciji da odredi koji izbor menija je izvršio. Svaka funkcija koja se izvršava iz menija mora da vrati celobrojnu vrednost tačno ili netačno. Ako se vrati lažna vrednost, menadžer menija zadržava kontrolu nad izborom gde je funkcija pozvana. Ako se vrati tačna vrednost, menadžer menija se vraća pozivaocu funkcije "menu_select". Iako izbori menija u primeru ne rade ništa, možete videti da ova funkcija funkcioniše u izboru "Prekini" u meniju "Datoteka". njegova funkcija vraća tačnu vrednost, a menadžer menija vraća. Ostale funkcije vraćaju false, a korisnik ostaje u meniju koji je izvršio funkciju.

### Ekrani za unos podataka

[Listing 4](#listing-4) je "input.h", a [Listing 5](#listing-5) je "entry.c". Ove dve datoteke koriste biblioteku prozora za implementaciju ekrana za unos podataka opšte namene. Evo kako to funkcioniše: uspostavljate prozor i gradite niz definicija polja za unos podataka. Upisujete neke informacije koje traže u prozor i pozivate softver za unos podataka, prosleđujući mu adresu niza definicija polja. Softver za unos podataka preuzima i prikuplja korisničke ključne unose u vaše bafere. Sve dobre stvari o skakanju sa polja na polje i prozorima pomoći i slično upravlja biblioteka za unos podataka.

Struktura u "entry.h" pod nazivom "FIELD" se koristi za opisivanje polja za unos podataka. Pravite niz ovih struktura koje završava strukturom nulte vrednosti. Evo objašnjenja svakog od članova u strukturi:

- **frow**:  
Broj reda u kome će polje biti prikazano u trenutnom prozoru.

  Slika 1: Test meni (iz testmenu.c)

- **fcol**:  
Broj kolumne polja. (Napomena: Redovi i kolumne su u odnosu na jedan. Krajnja gornja leva pozicija u prozoru je red 1, kolona 1.)

- **fk**:  
Broj kolumne u toku koji koristi softver za unos podataka. Ova vrednost treba da bude inicijalizovana na 1.

- **fbuff**: Pokazivač na bafer gde će se čuvati vrednost podataka kada je korisnik unese.

- **fmask**:  
Pokazivač na masku polja. Maske su nizovi podvlačenja i drugih znakova. Podvučene linije odgovaraju pozicijama znakova podataka polja. Ostali znakovi su prikazani sa poljem da bi bilo čitljivije. Na primer, maska za telefonski broj može biti sledeća:  

  ```c
  ( ) ______-______
  ```

- **fhelp**:  
Pokazivač na mnemoniku prozora pomoći u polju, koji će biti objašnjen drugi put. Za sada neka bude NULL.

[Listing 6](#listing-6) je program "testentr.c", koji ilustruje upotrebu ekrana za unos podataka. On uspostavlja prozor sa naslovom i upisuje neke poruke u prozoru. Zatim poziva funkciju unosa podataka da prikupi vrednosti podataka u svoj bafer. Parametri su:

- adresa niza FIELD struktura,
- TRUE vrednost koja govori funkciji da inicijalizuje bafere na nizove razmaka koji su završeni nulom,
- i celobrojna vrednost 1 da kaže funkciji da počne sa kursorom polja na prvom polju u nizu.

Slika 2 prikazuje ekran koji je prikazan u primeru nakon unosa nekih vrednosti podataka.

Da biste koristili ekran (koji ne radi ništa osim prikupljanja podataka u bafer, jednostavno ukucate neke vrednosti podataka).

- **Ins** taster prebacuje režim umetanja/prepisivanja.
- **Arrow** tasteri sa strelicama pomeraju kursor.
- **Ctrl-Right** ili **Ctrl-Left** preskaču reči.
- **Home** taster ide na početak polja,
- **End** ide na njegov kraj.
- **Backspace** briše znak sa leve strane,
- **Esc** taster za ukidanje bilo koje funkcije.

Esc će prekinuti unos podataka i vratiti vrednost ključa za završetak pozivaocu funkcije za unos podataka.

Kompilirajte i povežite "testentr.c" sa "entry.c" i "window.c" iz prošlomesečne C kolumne. Biće vam potreban "entri.h" i "windov.h" od prošlog meseca.

Možete da zadržite "testmenu.c" i "testentr.c" kao primere i da testirate druge menije i ekrane za unos podataka koje biste mogli da dizajnirate. Oni, međutim, neće biti deo projekta programiranja C kolumne osim kao primeri. Ostale izvorne datoteke su čuvari.

Sledećeg meseca ćemo dodati uređivač teksta prozora i biblioteku prozora pomoći osetljive na kontekst. Strukture definicija menija i unosa podataka uključuju članove koji ukazuju na pomoć u mnemo tehnici prozora. Do sada ih nismo koristili. Kasnije ćemo u ove članove staviti imena prozora pomoći (pokazivače na nizove) i tekst prozora pomoći u datoteku, a funkcija pomoći će postati automatska. Pritisnite taster za pomoć dok je kursor na izboru menija ili polju za unos podataka i pojaviće se prozor pomoći osetljiv na kontekst.

### Kroki broj 5: Velika rasprava

Sada ću vas podsetiti da ovu kolumnu koristim da izrazim svoje mišljenje o stvarima koje su mučne. Ova pitanja se nazivaju crotchets, i obično se odnose na C, iako se ovomesečni kroki odnosi na praksu programiranja bez obzira na bilo koji određeni jezik.

Da li je programiranje umetnost ili nauka? U "Programming Paradigms," (DDJ, jun 1988) Michael Svaine citira Chucka Moorea koji je rekao: "Programiranje je umetnost; možemo se nadati da će postati zanat; to nikada neće biti nauka." Kasnije Majkl odbacuje programiranje kao umetnost i naziva ga disciplinom. Ova debata se vodi godinama i uvek sam se držao. zatvorio sam usta o tome. Nema više - evo moje vrednosti nikla za ovaj mesec.

Figure 2: Ekran za unos testnih podataka (sa testentr.c)

 Personal Data | ...
 ------------- | ---------------------
 Name: | Cuthbert J. Twilly
 Account #: | 1234-567898
 Password: | intheflesh
 Phone: | (407) 555-1212 ext.0414

Da li je programiranje umetnost? Vebster naglašava ljudsku stranu umetnosti, pridajući njenu prvu definiciju "veštini izvođenja stečenoj iskustvom, proučavanjem ili posmatranjem". Imajte na umu da sama aktivnost nije toliko umetnost koliko je veština potrebna za praktikovanje aktivnosti. Sledeća definicija se bavi granama učenja kao što su slobodne i medicinske umetnosti. Sledi znanje, a zatim kreativna proizvodnja estetskih predmeta. Samo u ovoj poslednjoj definiciji učestvuje i sama stvaralačka aktivnost.

Kompjuterski program je proizvod kreativne aktivnosti, ali da li je estetski objekat? To zavisi, pretpostavljam, od publike. Za mene je umetničko delo nešto od lepote koju je stvorila vešta osoba da bismo u njemu uživali mi ostali, vešti i nevešti. Namenjen je da ga mase cene i nije samo za srodne umetnike. Programi se retko čitaju samo zbog lepote njihovog izraza i to samo od strane drugih veštih programera. Prihvatite ovu ideju i morate zaključiti da je publika za uvažavanje programa premala da bi se većina programa kvalifikovala kao estetski objekti.

Da li je programiranje nauka? Da li su programeri naučnici? Naučnici nose ogrtače, rade u laboratorijama i istražuju. Oni traže odgovore na pitanja i koriste svoje veštine u sistematskoj potrazi za istinom. Dugo idu u školu da nauče kako se to radi. zar ne? Mi programeri to ne radimo.

Da li je programiranje disciplina? Ne na način na koji ja to radim.

Slede mišljenja:

- Otkrića novih formula za beton i boju su nauke. Arhitektura i portret su umetnosti. Stolarija i krečenje kuća su zanati. Hodanje po gredi je disciplina.

- Otkriće novog algoritma sortiranja je nauka. Njegovo objavljivanje je umetnost. Njegova upotreba je zanatska. Njegove metodologije dizajna su discipline, koje se uvek odbacuju kada se približe rokovi.

- Možda mnogi ljudi koji nisu umetnički nadareni to priželjkuju i tako to što rade nazivaju umetnošću. Moj doktor to radi, ali postoji ožiljak na mom stomaku iznad mesta gde mi je nekada bila žučna kesa koji prkosi estetskoj ceni. Isto tako, mi sebe nazivamo naučnicima da podignemo svoj značaj na nekoliko nivoa. Ako, međutim, dozvolimo da nas svrstavaju u red molera i vešalica za gipsane zidove, skloni smo da mislimo da smo izgubili kastu i da smo se odrekli mističnosti koja pripada posebnim ljudima. Nije tako.

Dakle, sada znate. Programeri su zanatlije i dobri stari momci (i devojke takođe). Dodaj mi još jednu Pabst Blue Ribbon.

## C++

Svakih desetak godina dolazi još jedan novi metod programiranja i rečeno nam je da će ovaj novi talas preplaviti stare puteve i bolje je da se popnemo na dasku za surfovanje ili da budemo ostavljeni, jecajući i nezaposleni. Početkom šezdesetih bili smo uvučeni i vrištali u Cobol i Fortran, sve vreme očajnički hvatajući se za poslednju omiljenu listu Autokodera. Kako smo se držali tih znakova reči, adresnih registara i indeksnih registara, verujući da bez njih programiranje ne bi bilo ništa manje nego nemoguće. Baš kada su naši udarci atrofirali i naši vriskovi su izbledeli, a mi smo podlegli, oni su nam oduzeli pokušaj, izmislili vreme i gurnuli nam nešto što se zove strukturirano programiranje u lice. Na kraju smo to zavoleli – ko ne bi – ali te promene nije bilo lako progutati jer su zahtevale od nas da razmišljamo o strukturama koda na načine koji su strani od onoga što smo praktikovali kao drugu prirodu.

Danas nam je rečeno da je sledeći plimni talas "objektno orijentisano programiranje", a ja pokušavam da to shvatim. Problem je u tome da kad god pročitam objašnjenje šta to znači, primeri nisu stvarni. Nigde nisam video konkretne primere šta bi predmeti trebalo da budu. Možda postoje dobri primeri, ali ih još nisam našao. Mogu da razumem date primere kakve jesu, ali još ne mogu da prenesem to razumevanje u ono što dizajniram. Sada, dok razvijam novi program, pitam se koji od agregata podataka treba da budu objekti, a koji ne i zašto. Pitam ali ne odgovaram.

Bolje da ovo razjasnimo jer mi nešto govori da će uskoro svi ostali kodirati objektno orijentisane jezike i da ćemo mi biti izostavljeni. Oklevao sam da priznam ovu slabost karaktera i znanja predanoj čitalačkoj publici sve dok u kolumni Majkla Svejna nisam pročitao da je ni stručnjaci ne razumeju. Dozvolite mi da priznam da je moje istraživanje bilo ograničeno na članke iz časopisa (od nekoliko godina unazad do izdanja časopisa Byte o Smalltalk-u) i neke literature o proizvodima. Ako razradim ovu enigmu, C kolumna će pokušati da objasni nama ostalima ono što se čini da nekolicina već zna. Ako je ova stvar zaista odgovor na devojačku molitvu, sigurno bi joj trebalo jednostavno objašnjenje na engleskom.

Možda se zapitate zašto je ova kolumna forum za tako gaženje u mraku kroz mutnu novu temu. Trebalo bi da pišem o stvarima o kojima već znam, zar ne? Pa, možda se mnogi od vas suočavaju sa istom konfuzijom, a možda je nekolicina od vas daleko naprednija. Možda će zbunjeni imati koristi od ovog probnog ulaska u nepoznato. Možda će svesnost pomoći.

Uskoro ću dobiti kopiju knjige Bjarna Stroustrupa "The C++ Programming Language", definitivnog rada na C++. C++ je nadskup dijalekt C koji uključuje konstrukcije objektno orijentisanog programiranja. Možete definisati nove klase podataka na način sličan davanju typedef strukturi, ali tada možete da povežete funkcije sa novim klasama. Instance klase podataka nazivaju se objekti. Nove klase su izvedene iz i nasleđuju atribute starih klasa kada uključite staru klasu u novu klasu. Ova karakteristika je slična standardnoj sposobnosti C da ima jednu strukturu kao član više strukture.

Zgodna stvar u vezi sa tim je da možete definisati operatore koji rade sa objektima. Na primer, ako treba da dodate i oduzmete objekte, definišete taj operator i obezbedite funkciju koja obavlja operaciju. Nisam siguran koliko je ova funkcija drugačija ili bolja od standardne C mogućnosti za prosleđivanje adresa dve strukture funkciji. Ovo slabo objašnjenje bi bilo jasnije kada bih vam mogao reći šta je očigledan objekat.

Jedna karakteristika C++ će izazvati neke probleme. C++ vam omogućava da date isto ime nekoliko funkcija unutar klase. Kompajler određuje koji želite da pozovete tako što uparuje klase parametara u svakom pozivu zajedničkog imena sa prototipovima funkcije. Na površini se čini da se ovo suočava sa jakim tipiziranjem koji se tvrdi za C++, ali opet to još uvek ne razumem u potpunosti.

Do nedavno je C++ implementiran kao pretprocesor koji je kompajliran u C jezik, koji je potom mogao biti preveden u izvršni kod. Ovakav pristup je način na koji je RATFOR prethodno procesiran u Fortran. Sada je pušten izvorni C++ kompajler za MS-DOS. Zove se Zortech C++ kompajler i izdanak je nedavno nestalog Datalight Optimum C kompajlera. Datalight kompajler je bio respektabilan proizvod koji je nadmašio konkurenciju sa svojim brzim vremenom kompajliranja sve dok ga nije pomeo Turbo C, a kasnije i QuickC.

Sada je talenat koji je ušao u razvoj Datalight-a usmeren na C++ svet. Imam kopiju prvog izdanja Zortech C++ kompajlera i planiram da ga koristim da naučim C++ i objektno orijentisano programiranje ako ikada uspem da shvatim šta je objekat. Zortech priručnik ima primere. Objekti su tosteri u koje možete ubaciti hleb, podesiti temperaturu i testirati da li je iskočio tost. Vidite na šta mislim? Još jedan kroše u izradi. Dok nastavljam sa ovom potragom za istinom -kompjuterstvo? Ja ću objaviti svoj napredak u C kolumni. Napišite da li vidite objekat koji bih mogao prepoznati.

## LISTING 1

```c
/* ----------- menu.h ---------- */

typedef struct w_menu {
    char *mname;            /* menu bar selection names                     */
    char *mhlpmsg;          /* menu bar prompting messages                  */
    char **mselcs;          /* the pop-down menu selections                 */
    char **mshelp;          /* help mnemonics for the pop-down selections   */
    char *mskeys;           /* key strokes that accompany the selections    */
    int (**func)(int,int);  /* the functions to execute for the selections  */
    int lastvsel;           /* most recent vertical selection               */
} MENU;

void menu_select(MENU *, int);
char *display_menubar(MENU *);
void restore_menubar(char *);
```

## LISTING 2

```c
/* ------------ menu.c ------------ */

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "window.h"
#include "menu.h"

#define ON 1
#define OFF 0

static int getvmn(void);
static void haccent(int);
static void dimension(char **,int *,int *);
static void light(int);
static int vlook(int,int);

int hsel = 1;        /* horizontal selection */
MENU *mn;            /* active menu          */

/* ------------- display & process a menu ----------- */
void menu_select(MENU *mnn, int sel)
{
    int hs, sx, sy, vsel, holdhsel, frtn = FALSE;
    char *mb;

    if (sel)
        hsel = sel;
    mn = mnn;
    sx = wherex();
    sy = wherey();
    mb = display_menubar(mn);
    light(ON);
    while (!frtn && ((vsel = getvmn()) != 0))    {
        light(OFF);
        holdhsel = hsel;
        hs = hsel;
        hsel = 1;
        frtn = (*(mn+hs-1)->func [vsel-1]) ?
               (*(mn+hs-1)->func [vsel-1])(hs,vsel) : FALSE;
        hsel = holdhsel;
        mn = mnn;
        light(ON);
    }
    light(OFF);
    gotoxy(sx, sy);
    restore_menubar(mb);
}

/* --- display the menu bar with no selections chosen ---- */
char *display_menubar(MENU *mn)
{
    int i = 0;
    char *mb;

    if ((mb = malloc(160)) != NULL)
        gettext(1,1,80,1,mb);
    window(1,1,80,25);
    gotoxy(1,1);
    textcolor(MENUFG);
    textbackground(MENUBG);
    cprintf("    ");
    while ((mn+i)->mname)
        cprintf(" %-10.10s ", (mn+i++)->mname);
    while (i++ < 6)
        cprintf("            ");
    cprintf("    ");
    hidecursor();
    return mb;
}

/* ------------ restore the menu bar line --------------- */
void restore_menubar(char *mb)
{
    if (mb)    {
        puttext(1,1,80,1,mb);
        free(mb);
    }
}

/* ---------pop down a vertical menu --------- */
static int getvmn()
{
    int ht, wd, vx, sel;

    while (TRUE)    {
        dimension((mn+hsel-1)->mselcs, &ht, &wd);
        if (!(mn+hsel-1)->lastvsel)
            (mn+hsel-1)->lastvsel = 1;
        if (ht > 0)    {
            vx = 5+(hsel-1)*12;
            establish_window(vx, 2, vx+wd+1, ht+3,
                MENUFG, MENUBG,TRUE);
            text_window((mn+hsel-1)->mselcs, 1);
            sel = select_window((mn+hsel-1)->lastvsel,
                SELECTFG, SELECTBG, vlook);
            delete_window();
            if (sel == FWD || sel == BS)
                haccent(sel);
            else
                return ((mn+hsel-1)->lastvsel = sel);
        }
        else    {
            if ((sel = getkey()) == *(mn+hsel-1)->mskeys)
                return 1;
            switch (sel)    {
                case FWD:
                case BS:    haccent(sel);
                            break;
                case '\r':    return 1;
                case ESC:    return 0;
                default:    putch(BELL);
                            break;
            }
        }
    }
}

/* ---- if vertical menu user types FWD, BS,
               or a menu key, return it ---- */
static int vlook(ch, sel)
{
    char *cs, *ks;

    if (ch == FWD || ch == BS)    {
        (mn+hsel-1)->lastvsel = sel;
        return ch;
    }
    ks = (mn+hsel-1)->mskeys;
    if ((cs = memchr(ks, tolower(ch), strlen(ks))) == NULL)
        return ERROR;
    return ((mn+hsel-1)->lastvsel = cs-ks+1);
}

/* ----- manage the horizontal menu selection accent ----- */
static void haccent(int sel)
{
    switch (sel)    {
        case FWD:
            light(OFF);
            if ((mn+hsel)->mname)
                hsel++;
            else
                hsel = 1;
            light(ON);
            break;
        case BS:
            light(OFF);
            if (hsel == 1)
                while ((mn+hsel)->mname)
                    hsel++;
            else
                --hsel;
            light(ON);
            break;
        default:
            break;
    }
}

/* ---------- compute a menu's height & width --------- */
static void dimension(char *sl[], int *ht, int *wd)
{
    *ht = *wd = 0;

    while (sl && sl [*ht])    {
        *wd = max(*wd, (unsigned) strlen(sl [*ht]));
        (*ht)++;
    }
}

/* --------- accent a horizontal menu selection ---------- */
static void light(int onoff)
{
    extern struct wn wkw;
    extern char spaces[];
    int ln;

    window(1,1,80,25);
     textcolor(onoff ? SELECTFG : MENUFG);
     textbackground(onoff ? SELECTBG : MENUBG);
     gotoxy((hsel-1)*12+6, 1);
    cprintf((mn+hsel-1)->mname);
    textcolor(TEXTFG);
    textbackground(TEXTBG);
    if ((mn+hsel-1)->mhlpmsg)    {
        if (onoff)    {
            ln = strlen((mn+hsel-1)->mhlpmsg);
            gotoxy((80-ln)/2,25);
            cprintf((mn+hsel-1)->mhlpmsg);
        }
        else    {
            gotoxy(1,25);
            cprintf(spaces);
        }
    }
    current_window();
    hidecursor();
}
```

## LISTING 3

```c
/* --------- testmenu.c ------------ */
#include <stdio.h>
#include <string.h>
#include "window.h"
#include "menu.h"

/* ---------- menu tables --------- */
static char *fselcs[] = {
    "Load",
    "Save",
    "New",
    "Quit   [Esc]",
    NULL
};

static char *eselcs[] = {
    "Move       [F3]",
    "Copy       [F4]",
    "Delete     [F8]",
    "Find       [F7]",
    NULL
};

static char *oselcs[] = {
    "Auto Paragraph Reformat       ",
    "Insert  [Ins]                 ",
    NULL
};

static int quit(int,int);
static int reform(int, int);
static int insert(int, int);
static void set_toggles(void);

static char forced[] = {F3,F4,F8,F7};

static char options[] = {'a', INS};

static int (*ffuncs[])()={NULL,NULL,NULL,quit};
static int (*efuncs[])()={NULL,NULL,NULL,NULL};
static int (*ofuncs[])()={reform,insert};

static char filehelp[] =
   "Load a file, Save current buffer, Type a new file, Quit";
static char edithelp[] =
   "Move, Copy, Delete blocks, Find a string";
static char optnhelp[] =
   "Toggle editor options";

MENU emn [] = {
    {"File",    filehelp, fselcs, NULL,   "",      ffuncs, 0},
    {"Edit",    edithelp, eselcs, NULL,   forced,  efuncs, 0},
    {"Options", optnhelp, oselcs, NULL,   options, ofuncs, 0},
    {NULL}
};

void main()
{
    set_toggles();
    clear_screen();
    menu_select(emn, 1);
    clear_screen();
}

/* ---- illustrates toggled menu mode selectors -------- */
int reforming, inserting;  /* these are mode variables */

static int reform(hs,vs)
{
    reforming ^= TRUE;
    set_toggles();
    return FALSE;
}

static int insert(hs,vs)
{
    inserting ^= TRUE;
    set_toggles();
    return FALSE;
}

static void set_toggles()
{
    strcpy(&oselcs[0][24], reforming ? "(on) " : "(off)");
    strcpy(&oselcs[1][24], inserting ? "(on) " : "(off)");
}

/* ---- when TRUE is returned, the menu system exits ----- */
static int quit(hs,vs)
{
    return TRUE;
}
```

## LISTING 4

```c
/* --------- entry.h ---------- */

typedef struct field {      /* data entry field description*/
    int frow;               /* field row                   */
    int fcol;               /* field column position       */
    int fx;                 /* running column              */
    char *fbuff;            /* field buffer                */
    char *fmask;            /* field data entry mask       */
    char *fhelp;            /* field help window mnemonic  */
} FIELD;

void field_tally(void);
int data_entry(FIELD *, int, int);
void clear_template(void);
void insert_line(void);

#define INSERTING TRUE      /* initial Insert mode */
```

## LISTING 5

```c
/* --------- entry.c ---------- */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <conio.h>
#include <dos.h>
#include "window.h"
#include "entry.h"

#define FIELDCHAR '_'
int inserting = INSERTING;      /* insert mode, TRUE/FALSE */

extern struct wn wkw;

/* -------- local prototypes -------- */
static void addfield(FIELD *);
static void disp_field(FIELD *, char *, char *);
static int read_field(int);
static void data_value(FIELD *);
static int endstroke(int);
static void home_cursor(void);
static int backspace(void);
static void end_cursor(void);
static void forward(void);
static int fore_word(void);
static int back_word(void);
static void delete_char(void);
static void delete_word(void);

static FIELD *fhead;
static FIELD *ftail;
FIELD *fld;

/* -------- display a data field ------ */
static void disp_field(FIELD *fldv, char *bf, char *msk)
{
    char cl[80], *cp = cl;

    while (*msk)    {
        *cp++ = (*msk != FIELDCHAR ? *msk : *bf++);
        msk++;
    }
    *cp = '\0';
    writeline(fldv->fx + fldv->fcol - 1,
               fldv->frow, cl);
}

/* ------- display the data value in a field ------ */
static void data_value(FIELD *fldv)
{
    gotoxy(fldv->fcol, fldv->frow);
    fldv->fx = 1;
    disp_field(fldv, fldv->fbuff, fldv->fmask);
}

/* ------ display all the fields in a window ------- */
void field_tally()
{
    FIELD *fldt;

    fldt = fhead;
    while (fldt != ftail)    {
        data_value(fldt);
        fldt++;
    }
}

/* ------- clear a template to all blanks ------ */
void clear_template()
{
    FIELD *fldc;
    char *bf, *msk;

    fldc = fhead;
    while (fldc != ftail)    {
        bf = fldc->fbuff;
        msk = fldc->fmask;
        while (*msk)    {
            if (*msk == FIELDCHAR)
                *bf++ = ' ';
            msk++;
        }
        fldc++;
    }
    *bf = '\0';
    field_tally();
}

char *mask, *buff;

/* ------- read a field from the keyboard ------------- */
static int read_field(c)
{
    int done = FALSE, first = TRUE;

    home_cursor();
    while (TRUE)    {
        gotoxy(fld->fx+fld->fcol-1, fld->frow);
        if (!c || !first)
            c = getkey();
        first = FALSE;
        switch (c)    {
            case CTRL_D:
                delete_word();
                break;
            case CTRL_FWD:
                done = !fore_word();
                break;
            case CTRL_BS:
                done = !back_word();
                break;
            case HOME:
                fld->fx = 1;
                home_cursor();
                break;
            case END:
                end_cursor();
                break;
            case FWD:
                forward();
                break;
            case BS:
                backspace();
                break;
            case '\b':
                if (!backspace())
                    break;
                gotoxy(fld->fx+fld->fcol-1, fld->frow);
            case DEL:
                delete_char();
                disp_field(fld, buff, mask);
                break;
            case INS:
                inserting ^= TRUE;
                insert_line();
                break;
            default:
                if (endstroke(c))    {
                    done = TRUE;
                    break;
                }
                if (inserting)    {
                    memmove(buff+1, buff, strlen(buff)-1);
                    disp_field(fld, buff, mask);
                    gotoxy(fld->fcol+fld->fx-1, fld->frow);
                }
                *buff = (char) c;
                putch(c);
                forward();
                if (!*mask)
                    c = FWD;
                break;
        }
        if (done || !*mask)
            break;
    }
    wkw.wx = fld->fx+1;
    return c;
}

/* -------- home the cursor in the field -------- */
static void home_cursor()
{
    buff = fld->fbuff+fld->fx-1;
    mask = fld->fmask+fld->fx-1;
    while (*mask != FIELDCHAR)    {
        fld->fx++;
        mask++;
    }
}

/* ------- move the cursor to the end of the field ------ */
static void end_cursor()
{
    fld->fx += strlen(mask)-1;
    buff += strlen(buff)-1;
    mask += strlen(mask)-1;
    while (*buff == ' ')
        if (!backspace())
            break;
    forward();
}

/* ------ move the cursor forward one character -------- */
static void forward()
{
    do    {
        fld->fx++;
        mask++;
    } while (*mask && *mask != FIELDCHAR);
    buff++;
}

/* ------- move back one character ------------ */
static int backspace()
{
    if (buff != fld->fbuff)    {
        --buff;
        do    {
            --mask;
            --(fld->fx);
        } while (*mask != FIELDCHAR);
        return TRUE;
    }
    return FALSE;
}

/* ------- move forward one word ------- */
static int fore_word()
{
    int ct = 2, test = *buff == ' ';

    while (ct--)    {
        while ((*buff == ' ') == test && *mask)
            forward();
        if (!*mask)
            return FALSE;
        if (test)
            break;
        test = !test;
    }
    return TRUE;
}

/* ------- move backward one word ------- */
static int back_word()
{
    int test;

    if (buff == fld->fbuff)
        return FALSE;
    if (*(buff-1) == ' ')
        backspace();
    test = *buff == ' ';
    while ((*buff == ' ') == test && buff != fld->fbuff)
        backspace();
    if (test)
        while (*buff != ' ' && buff != fld->fbuff)
            backspace();
    if (*buff == ' ')
        forward();
    return TRUE;
}

/* --------- delete a character ----------- */
static void delete_char()
{
    memmove(buff, buff+1, strlen(buff));
    *(buff+strlen(buff)) = ' ';
}

/* ----------- delete a word ---------- */
static void delete_word()
{
    int test = *buff == ' ';
    int ln = strlen(buff);

    while ((*buff == ' ') == test && ln--)
        delete_char();
    if (!test)
        delete_char();
    disp_field(fld, buff, mask);
}

/* ---------- test c for an ending keystroke ----------- */
static int endstroke(int c)
{
    switch (c)    {
        case '\r':
        case '\n':
        case '\t':
        case ESC:
        case F1:
        case F2:
        case F3:
        case F4:
        case F5:
        case F6:
        case F7:
        case F8:
        case F9:
        case F10:
        case PGUP:
        case PGDN:
        case HOME:
        case END:
        case CTRL_FWD:
        case CTRL_BS:
        case CTRL_HOME:
        case CTRL_END:
        case UP:
        case DN:
            return TRUE;
        default:
            return FALSE;
    }
}

/* ----- Process data entry for a screen template. ---- */
int data_entry(FIELD *fldin, int init, int firstfld)
{
    int exitcode = 0, done = FALSE;

    insert_line();
    fhead = ftail = fld = fldin;
    while (ftail->frow)
        ftail++;
    while (firstfld-- && fld != ftail)
        fld++;
    --fld;
    if (init)
        clear_template();
    field_tally();
    /* ---- collect data from keyboard into screen ---- */
    while (done == FALSE)    {
        gotoxy(fld->fcol, fld->frow);
        textcolor(FIELDFG);
        textbackground(FIELDBG);
        data_value(fld);
        gotoxy(fld->fcol, fld->frow);
        fld->fx = 1;
        exitcode = read_field(0);
        textcolor(ENTRYFG);
        textbackground(ENTRYBG);
        data_value(fld);
        switch (exitcode)    {
            case DN:
            case '\r':
            case '\t':
            case CTRL_FWD:
            case FWD:    done = (ftail == fhead+1);
                        fld++;
                        if (fld == ftail)
                            fld = fhead;
                        break;
            case CTRL_BS:
            case UP:    if (fld == fhead)
                            fld = ftail;
                        --fld;
                        break;
            case CTRL_HOME:
                        fld = fhead;
                        break;
            case CTRL_END:
                        fld = ftail-1;
                        break;
            default:    done = endstroke(exitcode);
                        break;
        }
    }
    fld = NULL;
    return (exitcode);
}

/* ---------- set insert/exchange cursor shape ----------- */
void insert_line()
{
    set_cursor_type(inserting ? 0x0106 : 0x0607);
}
```

## LISTING 6

```c
/* ----------- testentr.c --------- */
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include "window.h"
#include "entry.h"

struct config {
    char name[36];
    char acctno[21];
    char password[11];
    char phone[15];
} cfg;

static char mask[] = "___________________________________";
static char phmask[] = "(___) ___-____ ext.____";

FIELD pers_template[] = {
    {3, 14, 1, cfg.name,    mask,    NULL},
    {4, 14, 1, cfg.acctno,  mask+15, NULL},
    {5, 14, 1, cfg.password,mask+25, NULL},
    {6, 14, 1, cfg.phone,   phmask,  NULL},
    {0}
};

void main()
{
    establish_window(15,5,65,12,ENTRYFG,ENTRYBG,TRUE);
    window_title(" Personal Data ");
    gotoxy(3,3);
    cputs("Name:");
    gotoxy(3,4);
    cputs("Account #:");
    gotoxy(3,5);
    cputs("Password:");
    gotoxy(3,6);
    cputs("Phone:");
    data_entry(pers_template, TRUE, 1);
    delete_window();
    return FALSE;
}
```
