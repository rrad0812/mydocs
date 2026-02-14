# C programiranje 14

## C++: O knjigama, kompajlerima i prozorskim objektima

Al Stevens, septembar '89

Prošlog meseca sam pogledao C++ programski jezik Bjornea Stroustrupa i odlučio da pronađem drugu knjigu koja bi bila uvod u C++. Knjiga Stroustrup, koju ću u daljem tekstu s ljubavlju zvati BS (u tradiciji K&R), poslužiće kao referenca kada bolje razumem jezik C++. Ali pošto mi je i dalje bio potreban tekst vodiča za C++, pretražio sam knjižare.

Našao sam knjigu koju mogu da preporučim za učenje C++-a, ali samo uz stroge rezerve. Knjiga je The Whyte Group's "C++ Programming" od Džona Berija i namenjena je C programerima koji žele da nauče C++. Beri je dobar pisac i dobar učitelj. Organizacija i prezentacija materijala je tačna za podučavanje ekstenzija koje C++ donosi u C.

Programski jezik C++, međutim, nije za programere koji još ne znaju C. Dok Beri efikasno piše o C++, on ne pokušava da podučava starinski C, tako da ako ga već ne znate, nećete ga naučiti ovde. Taj propust je razumljiv, pa čak i prikladan -- već ima dosta dobrih tekstova na C. Ali ova knjiga ima jedan neoprostiv problem, sa kojim će se većina programera lako identifikovati, a neki će ga osuditi. Kod je prepun grešaka. Većina grešaka su one koje će programer C odmah uočiti. Ali neko drugi bi mogao pretpostaviti da kod funkcioniše, mogao bi pretpostaviti da C funkcioniše na taj način, i mogao bi zapravo pokušati da natera primere da funkcionišu, što je vežba koju je sam autor trebalo da preduzme pre nego što je otišao da štampa. Neki od programa u ovoj knjizi se ne bi kompajlirali, a još manje izvršili. Druge su izgrađene sa konvencijama koje bi mogle da funkcionišu u ograničenim kontekstima primera, ali ih iskusni C programeri generalno prepoznaju kao nepoželjne prakse kodiranja.

Slede primeri grešaka koje sam pronašao u knjizi. Sada sam otprilike na pola puta i mogu samo da pretpostavim da drugo poluvreme nije ništa bolje od prvog.

Prvo, autor insistira na stavljanju izvršnog koda u datoteke zaglavlja. Tipična `.h` datoteka sadrži definicije klasa i njihove funkcije članova. Programi koji imaju nekoliko datoteka izvornog koda koji koriste iste klase naići će na probleme sa povezivanjem sa višestruko definisanim funkcijama. C programeri su davno uspostavili konvenciju stila gde su datoteke zaglavlja samo za definicije i prototipove, dok sve izjave koje rezervišu ili koriste memoriju idu u `.c` datoteke. Kao novajlija u C++-u, pretpostavljam da će C++ imati istu konvenciju. Ako nije, trebalo bi.

Zatim, postoji nekoliko programa u knjizi koji koriste dužinu stringa, kao što je vraća `strlen`, za dodelu novog bafera. Zatim `strcpy` kopira string u bafer. Svaki C programer zna da `strlen` ne broji nulti karakter terminatora, ali da ga `strcpy` kopira. Primeri bi mogli da rade, ali samo kada ništa kritično nije na heapu odmah iza novog bafera.

Knjiga koristi strukturu podataka steka da bi ilustrovala definicije C++ klasa. Funkcije steka ne koriste prvi element u nizu steka, ali koriste element jedan posle poslednjeg. Još jednom, primer programa verovatno radi OK, ali ako ga koristite u većem programu, verovatno ćete nešto drugo izbaciti. Bar na jednom drugom mestu Beri koristi indeks od 2 da adresira drugi element niza, tehniku koja ne funkcioniše ni u jednom kontekstu. Primer koda uopšte ne bi pružio navedene rezultate. Ove greške u pretplati su tipične za one koje prave novi C programeri, posebno programeri koji dolaze na C sa Basic-a, i one su vrste grešaka koje bi autor pronašao da je odvojio vreme da isproba neki od svog koda.

Funkcija "give_date" dodeljuje novi bafer svaki put kada je pozovete. Ništa nikada ne briše bafer. Ova greška se javlja na nekoliko mesta u knjizi. Obavite dovoljno poziva da biste pozvali "give_date" i heap (koja se u C++ zove "besplatna prodavnica") se iscrpljuje. BS definiše tehniku za hvatanje takvih grešaka, ali knjiga je ignoriše.

Julijanske funkcije datuma zanemaruju prestupne godine. Pretpostavljam da je to oprostivo, ali knjiga bi to trebalo da istakne. Neki programeri koji ništa ne sumnjaju i od poverenja mogu samo pokušati da koriste neke od ovih stvari.

Funkcija "nth_token" vraća znak '\0' kada je funkcija jasno definisana da vraća pokazivač karaktera.

Konačno, diskusija o preopterećenju funkcija zbunjuje long i int. Primeri preopterećenja operatora koriste `strcat` nepravilno.

Ako već niste iskusni C programer, klonite se ove knjige – ona daje loše primere. Ako ste jedan, mogu ga preporučiti zbog njegovog tekstualnog sadržaja. Nakon što sam prepoznao neuredan kod i konvencije koda, napravio sam dodatke i nastavio da uživam u poslu. Radi ono što sam želeo. Jasno objašnjava C++ ekstenzije na način koji ja razumem. Šteta što su primeri koda tako loši. Oni kvare inače odličnu knjigu.

Revnost knjige za C++ zamagljuje njenu objektivnost. Na nekoliko mesta pokušava da opravda jezik i objektno orijentisana proširenja koja donosi u C navodeći prednosti novog i nedostatke starog. I u svakom od ovih slučajeva postoje susedne prednosti u C-u i susedne mane u C++. Možda Beri ne razume C onako dobro kako bi trebalo.

## Vrednost C++ ekstenzija

Kako dalje ulazim u C++ i objektno orijentisanu paradigmu, stvarna prednost koju nude C programerima postaje očigledna. Zagovornici OOP-a se možda neće složiti, ali verujem da ono što C++ nudi nije bolji način da se deklarišu i definišu specifični objekti informacija za aplikaciju, već je odličan metod za proširenje jezika C naprednim i složenim tipovima podataka.

C i C++ su jezička okruženja bez intrinzičnih ulazno/izlaznih operacija. Na samom početku, C programeri su razvili standardnu biblioteku funkcija koje upravljaju I/O datotekama i konzolama. Pojavile su se i druge standardne funkcije -- rukovaoci stringovima, matematika, upravljanje memorijom i tako dalje. C++, kao nadskup ANSI C-a, nasleđuje sve te funkcije, ali ima potencijal za još jedan sloj standardnih proširenja jezika. C++ vam omogućava da dodate tipove podataka -- objekte. To znači da možete proširiti jezik tako da uključuje tipove koji imaju univerzalnu primenu na probleme programiranja. Na primer, možete lako da definišete objekat string koji ima većinu funkcionalnosti string operacija u kojima su Basic programeri oduvek uživali. Druge moguće ekstenzije objekata su polja za monetu, datumi, imena i adrese. Ove ekstenzije nisu složena stabla objekata kao što ih možete izgraditi u prilagođenoj aplikaciji baze podataka, već uobičajeni tipovi podataka koje možete definisati na načine koje svako može da koristi. Pored očiglednih upravo pomenutih, mogu postojati generičke strukture podataka koje većina programera koristi, kao što su povezane liste, stekovi, binarna stabla, AVL stabla, B-stabla i mnoga druga. C++, daleko više od C, je proširiv.

Ali pazi. Programer može dozvoliti svojoj mašti da preoptereti njegove sposobnosti, preopteretiti do vraga operatore i funkcije u C++, a rezultat može biti ista vrsta nerazumljivog koda koji muči sve od Cobola do 4GL-a.

Pošto C++ prevazilazi C kao jezik izbora, biće nam potrebni C++ standardi koji prevazilaze standardne funkcije i proširenja tipa. Potreban nam je skup smernica koje ćemo koristiti u dizajnu preopterećenih operatora i funkcija, da ne bismo svi pisali nečitljiv kod. Možete, na primer, lako da dizajnirate objekat koji preopterećuje plus operator da izvrši oduzimanje. To bi bila glupa stvar i predstavlja preuveličavanje problema, ali C++ nudi potencijal za neke zaista guste operacije, a nama je potreban smer u korišćenju ovog moćnog novog alata.

## Zortech C++ kompajler

Prošle godine sam započeo povremeni prodor u C++ koristeći Zortech C++ kompajler. Zortech je ljubazno obezbedio kompajler, koji je sada do verzije 1.07. To je dobar proizvod, budući da je potomak Datalite C kompajlera. Zortech C++ uključuje kompletan ANSI C kompajler kao i C++ proizvod.

Slabosti u Zortech paketu su na mestima koja mi nisu bitna. ZED editor je vizuelna šala, ali ja koristim Brief. Sistem pomoći koji je rezidentan u memoriji je pogrešan, zamrzaće vaš računar i ne bi trebalo da ga koristite. Uslužni program MAKE eksplodira predvidljivo kada pronađe određene greške kompajliranja. Međutim, kompajler radi odlično. Nema integrisano razvojno okruženje po uzoru na Turbo C i QuickC, i nema program za otklanjanje grešaka. Možete kompajlirati sa određenim prekidačima i povezati se sa Microsoft LINK programom tako da možete koristiti Codeview. Nažalost, uslužni program Turbo Debugger koji omogućava da Codeview programi rade sa TD ne radi sa Zortech C++ programima, tako da mi je uskraćeno korišćenje mog omiljenog debagera.

Pročitao sam da Zortech C++ nije kompatibilan sa drugim jezičkim okruženjima C++, ali još uvek ne znam dovoljno o ​​C++ da bih mogao da procenim, osim da kažem da je sve što sam naučio od BS-a i Berri knjige funkcionisalo sasvim dobro. Sve dok ne budu postojali standardi za C++ i dok veliki momci ne objave C++ kompajlere, Zortech će raditi za mene.

## C++ Windows

Da bih stekao svoje nove C++ veštine, odlučio sam da pokušam sa jednostavnim jezičkim proširenjem i napravim tip podataka video prozora. Većina mojih PC programa uključuje iskačuće prozore i menije, tako da je tip podataka Window prirodno mesto za iskakanje.

[Listing 1](#listing-1) je "windov.h", koji definiše klasu Windov i tri izvedene klase. Pored toga, "windov.h" definiše određene globalne vrednosti koje koriste ove nove klase. C++ program koji koristi nove klase uključio bi ovu datoteku zaglavlja i povezao bi se sa objektnim modulom koji je kompajliran iz [Listing 2](#listing-2).

"Window.h" i "window.c" kreiraju novu klasu pod nazivom "Windov". Programi sada mogu da koriste objekte, koji su instance ove klase, baš kao što mogu da koriste int, float, struct i druge standardne C tipove podataka. Prozor se pojavljuje na ekranu kada uđe u opseg i nestaje kada izađe iz opsega, tako da program koji koristi ne otvara i ne zatvara prozor na način na koji bi to činili sa tradicionalnim C programima. Ova konvencija nameće određenu količinu strukture programima koji koriste tip prozora, što uopšte nije loša ideja.

C++ koristi funkcije konstruktora i destruktora koje prate novu klasu. Konstruktor se automatski izvršava kada klasa uđe u opseg, a funkcija destruktora se izvršava kada klasa izađe van opsega. Funkcija konstruktora prozora uspostavlja i prikazuje prozor, a funkcija destruktora ga briše. Kada deklarišete objekat kao instancu klase, u zagradama uključujete parametre koje funkcija konstruktora koristi za konstruisanje objekta. Da biste proglasili prozor, koristite ovaj format:

```cpp
Window wname(lf,tp,rt,bt,fg,bg);
```

Prva četiri parametra su koordinate znakova četiri ugla prozora, pri čemu je gornji levi ugao ekrana 0,0. Poslednja dva parametra su konstante boja enuma definisane u "window.h" koje određuju boje prednjeg plana i pozadine prozora.

Definicija klase specificira privatne i javne delove klase. Konvencija koja se razvija ima vrednosti podataka klase u privatnom delu i funkcije koje rade na klasi (nazvane "metode" u OOP-govoru, "funkcije članova" u C++ terminologiji) u javnom delu. Samo funkcije članice klase mogu čitati i pisati privatne delove (izvinite, ali tako ih nazivaju osnivači C++) klase.

Privatni delovi prozora uključuju njegove boje, položaj ekrana, pokazivače na bafere za čuvanje video memorije, poziciju kursora i pokazivač na tekst koji se može dodeliti prozoru. Javni delovi uključuju funkcije konstruktora i destruktora, funkciju za dodeljivanje opcionog naslova prozoru, preopterećene operatore << za pisanje znakova, linija i blokova teksta u prozor i funkcije za manipulaciju tekstom u prozoru. Komentari u "window.h" vam govore koje su to funkcije.

## Izvedene klase prozora

"Window.h" i "window.c" sadrže tri klase izvedene iz klase Windov. U C++ izvedena klasa nasleđuje karakteristike klase iz koje je izvedena. Tri izvedene klase obezbeđuju:

- prozore sa obaveštenjima,
- prozore sa greškama i
- prozor YesNo.

Ovi uslužni prozori se pojavljuju, prikazuju poruku, čekaju pritisak na taster i iskaču. U svakom slučaju, prozor prikazuje poruku za inicijalizaciju koja je navedena kada je klasa deklarisana i čeka na pritisak pre nego što nestane. Prozor YesNo zahteva pritisak na taster Y ili N i vraća tačnu vrednost ako je ključ Y. Razlika između druga dva su boje prozora. Kako je objavljeno, prozor sa greškom je crven sa žutim slovima koji trepću, a prozor sa obaveštenjima ima crna slova na cijan pozadini. YesNo prozori su beli na zelenom.

## Funkcije konzole

Kao i sa svim takvim sistemima, moramo se baviti hardverom konzole. Definisao sam grupu funkcija koje upravljaju ekranima, kursorom i tastaturom. Većina njih ima odgovarajuće funkcije u Zortech biblioteci displeja. [Listing 3](#listing-3), "console.h" sadrži makroe za Zortech funkcije i prototipove za ostale. [Listing 4](#listing-4) je "console.c", koji sadrži funkcije koje su nam potrebne, ali koje nisu predstavljene Zortech funkcijama prikaza. Korisnici drugih kompajlera moraju zameniti makroe ili funkcije za one koje podržavaju Zortech funkcije. Slede funkcije upravljanja konzolom koje su potrebne klasi Window i koje se pružaju ili kao C funkcije u "console.c" ili makroi za Zortech funkcije u "console.h":

**hidecursor**  
Ova funkcija sakriva kursor tako da njegova pozicija i dalje utiče na prikaz teksta, ali je sam kursor nevidljiv.

**unhidecursor**
Ova funkcija čini kursor ponovo vidljivim.

**savecursor**
Ova funkcija čuva trenutnu konfiguraciju kursora na steku.

**restorecursor**
Ova funkcija vraća konfiguraciju kursora na onu koja je poslednja gurnuta na stek kursora.

**getkey**
Ova funkcija dobija pritisak na taster izbegavanjem DOS poziva da bi se izbegli prekidi Ctrl-Break na MS-DOS računarima. Takođe prevodi funkcije tastera u jedinstvene 8-bitne vrednosti kao što je definisano u "console.h".

**initconsole** i **closeconsole**
Ove funkcije inicijalizuju i zatvaraju funkcije konzole. Mnogi video paketi zahtevaju takve funkcije za podešavanje sistemskih parametara i ispiranje bafera.

**savevideo** i **restaurvideo**
Ove funkcije prenose znakove video memorije u i iz bafera za čuvanje. Koriste se za čuvanje i vraćanje onoga što prozor pokriva kada dođe u opseg. Parametri funkcije određuju adresu bafera i koordinate ugla karaktera prozora.

**box**
Ova funkcija crta jednolinijski okvir na ekranu. Klasa Window ga koristi da napravi ivicu prozora.

**colors**
Ova funkcija uspostavlja boje prednjeg plana i pozadine za naredne prikaze ekrana.

**setcursor**
Ova funkcija pozicionira kursor na određene koordinate znakova.

**window_printf** i **window_putc**
Ovo su prozorski orijentisane verzije printf i putchar.

**videoscroll**
Ovo je funkcija pomeranja prozora. Njegovi parametri uključuju pravac i broj linija za pomeranje i koordinate ugla karaktera.

## Look.c

Program u [Listing 5](#listing-5) je look.c. On demonstrira klase prozora sa procedurom koja vam omogućava da pregledate tekstualnu datoteku u prozoru preko celog ekrana. Look.c počinje pozivanjem funkcije "set_new_handlerW, BS tehnike koju podržava Zortech i koja poziva vašu funkciju "get" kada novi operater ne može više da dobije memoriju iz besplatne prodavnice. Zatim, program deklariše prozor koji zauzima ceo ekran i koristi metod naslova da napiše naslov u gornjoj ivici prozora. Program postavlja `filebuf` objekat, otvara datoteku koristeći ime u komandnoj liniji i povezuje `instream` objekat sa `filebuf`-om.

Konvencija C++ toka za datoteke je ona za koju smatram da je manje nego intuitivna. Baferi su objekti, a tokovi su objekti. Deklarišete bafer i kažete mu da otvori datoteku. Zatim deklarišete tok i povezujete ga sa baferom. Da biste čitali i pisali datoteku, šaljete poruke metodama strima. Čini mi se da bi sve to moglo da se uradi sa jednim objektom što bi ga učinilo jednostavnijim i lakšim za razumevanje. Ali pošto je konvolucija dva objekta BS tehnika, proizvođači kompajlera će je, bez sumnje, zauvek održavati u ime usaglašenosti.

Program "look.c" čita sve redove teksta iz datoteke, stavlja svaki red u novi bafer i stavlja adrese bafera u niz pokazivača znakova. Zatim tekst ide u prozor preko preopterećenog << operatora.

Da bi demonstrirao upotrebu izvedene klase YesNo, program koristi prozor YesNo da pita da li želite da nastavite. Ako je tako, program poziva funkciju člana stranice da bi vam omogućio skrolovanje i listanje kroz tekst.

Program koristi objekte Error da vam kaže da nije bilo imena datoteke u komandnoj liniji ili da ne može pronaći datoteku koju ste naveli.

## Dug put uz C++ padinu

Kriva učenja na koju sam se popeo da bi ova proširenja prozora funkcionisala bila je zaista strma, a pogoršala ju je nedostatak dobrih OOP-ova i literature za C++ tutoriale. Stvari bi bile lakše da je Berri knjiga bila dostupna za uvod u C++, ali ono što je zaista nedostajalo je pristojno objašnjenje OOP paradigme. Da bih vam popunio tu prazninu, upravo ću prekinuti tradiciju za kolumnu „C programiranje“ i preporučiti vam da molite, pozajmite ili kupite Turbo Pascal 5.5. Nije mi važno ako nikada ne koristite softver; mala knjiga koja objašnjava objektno orijentisana proširenja za TP je ono što želite. To je glava i ramena iznad svakog uvoda u OOP koje sam do sada video.

Sledećeg meseca ćemo koristiti klasu Window kao osnovu iz koje ćemo izvesti neke klase menija, čime ćemo se dublje upoznati sa karakteristikama nasleđivanja C++-a.

## LISTING 1

```cpp
// -------------- window.h

#ifndef WINDOWS
#define WINDOWS

// ---------- screen dimensions
#define SCREENWIDTH 80
#define SCREENHEIGHT 25

// --------- atrribute values for colors
enum color {
    BLACK, BLUE, GREEN, CYAN, RED, MAGENTA, BROWN, LIGHTGRAY,
    GRAY, LIGHTBLUE, LIGHTGREEN, LIGHTCYAN, LIGHTRED,
    LIGHTMAGENTA, YELLOW, WHITE, BLINK = 128
};

// ------------ spaces per tab stop (text displays)
#define TABS 4
// ------------ color assignments for window types
#define YESNOFG  WHITE
#define YESNOBG  GREEN
#define NOTICEFG BLACK
#define NOTICEBG CYAN
#define ERRORFG  (YELLOW | BLINK)
#define ERRORBG  RED

// ------------ a video window
class Window {
    unsigned bg, fg;        // window colors
    unsigned lf,tp,rt,bt;   // window position
    unsigned *wsave;        // video memory save buffer
    unsigned *hsave;        // hide window save buffer
    unsigned row, col;      // current cursor row and column
    int tabs;               // tab stops, this window
    char **text;            // window text content
public:
    Window(unsigned left, unsigned top,
           unsigned right, unsigned bottom,
           color wfg, color wbg);
    ~Window(void);
    void title(char *ttl);
    Window& operator<<(char **btext);
    Window& operator<<(char *ltext);
    Window& operator<<(char ch);
    void cursor(unsigned x, unsigned y);
    void cursor(unsigned *x, unsigned *y)
        { *y = row, *x = col; }
    void clear_window(void);
    void clreos(void);          // clear to end of screen
    void clreol(void);          // clear to end of line
    void hidewindow(void);      // hide an in-scope window
    void restorewindow(void);   // unhide a hidden window
    void page(void);            // page through the text
    void scroll(int d);         // scroll the window up, down
    void set_colors(int cfg, int cbg)   // change the colors
        { fg = cfg, bg = cbg; }
    void set_tabs(int t)        // change the tab stops
        { if (t > 1 && t < 8) tabs = t; }
};

// ---------- utility notice window
class Notice : Window   {
public:
    Notice(char *text);
    ~Notice(){}
};

// ---------- utility yes/no window
class YesNo : Window    {
public:
    YesNo(char *text);
    ~YesNo(){}
    int answer;
};

// ---------- utility error window
class Error : Window    {
public:
    Error(char *text);
    ~Error(){}
};

#define max(x,y) (((x) > (y)) ? (x) : (y))
#define min(x,y) (((x) > (y)) ? (y) : (x))

#endif
```

## LISTING 2

```cpp
// -------------- window.c

// A C++ window library

#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include "window.h"
#include "console.h"

#define HEIGHT (bt - tp + 1)
#define WIDTH  (rt - lf + 1)

// ------- constructor for a Window
Window::Window(unsigned left, unsigned top,     // 0 - 79, 0 - 24
              unsigned right, unsigned bottom,
              color wfg, color wbg)
{
    savecursor();
    initconsole();
    hidecursor();
    // ----- adjust for windows beyond the screen dimensions
    if (right > SCREENWIDTH-1)  {
        left -= right-(SCREENWIDTH-1);
        right = SCREENWIDTH-1;
    }
    if (bottom > SCREENHEIGHT-1)    {
        top -= bottom-(SCREENHEIGHT-1);
        bottom = SCREENHEIGHT-1;
    }
    // ------- initialize window dimensions
    lf = left;
    tp = top;
    rt = right;
    bt = bottom;
    // ------- initialize window colors
    fg = wfg;
    bg = wbg;
    // ------- initialize window cursor and tab stops
    row = col = 0;
    tabs = TABS;
    // ---------- save the video rectangle under the new window
    wsave = new unsigned[HEIGHT * WIDTH];
    hsave = NULL;
    savevideo(wsave, tp, lf, bt, rt);
    // --------- draw the window frame
    box(tp, lf, bt, rt, fg, bg);
    // -------- clear the window text area
    clear_window();
    unhidecursor();
}

// ------- destructor for a Window
Window::~Window(void)
{
    // ----- restore the video RAM covered by the window
    restorevideo(wsave, tp, lf, bt, rt);
    delete wsave;
    if (hsave != NULL)
        delete hsave;
    restorecursor();
}

// ------- hide a window without destroying it
void Window::hidewindow(void)
{
    if (hsave == NULL)  {
        hsave = new unsigned[HEIGHT * WIDTH];
        savevideo(hsave, tp, lf, bt, rt);
        restorevideo(wsave, tp, lf, bt, rt);
    }
}

// --------- restore a hidden window
void Window::restorewindow(void)
{
    if (hsave != NULL)  {
        savevideo(wsave, tp, lf, bt, rt);
        restorevideo(hsave, tp, lf, bt, rt);
        delete hsave;
        hsave = NULL;
        colors(fg,bg);
    }
}

// -------- add a title to a window
void Window::title(char *ttl)
{
    setcursor(lf + (WIDTH - strlen(ttl) - 1) / 2, tp);
    colors(fg, bg);
    window_printf(" %s ", ttl);
    cursor(col, row);
}

// ------- write text body to a window
Window& Window::operator<<(char **btext)
{
    cursor(0, 0);
    text = btext;
    if (*btext != NULL)
        *this << *btext++;
    while (*btext != NULL && row < HEIGHT-3)
        *this << '\n' << *btext++;
}

// -------- write a line of text to a window
Window& Window::operator<<(char *ltext)
{
    while (*ltext && col < WIDTH - 2 && row < HEIGHT - 2)
        *this << *ltext++;
    return *this;
}

// -------- write a character to a window
Window& Window::operator<<(char ch)
{
    cursor(col, row);
    switch (ch) {
        case '\n':
            clreol();
            if (row == HEIGHT-3)
                scroll(1);
            else
                row++;
        case '\r':
            col = 0;
            break;
        case '\b':
            if (col)
                --col;
            break;
        case '\t':
            do
                *this << ' ';
            while (col % tabs);
            break;
        default:
            if (col == WIDTH - 2)
                *this << '\n';
            colors(fg,bg);
            window_putc(ch);
            col++;
            return *this;
    }
    cursor(col, row);
    return *this;
}

// ----- position the window cursor
void Window::cursor(unsigned x, unsigned y)
{
    if (x < WIDTH-2 && y < HEIGHT-2)    {
        setcursor(lf+1+x, tp+1+y);
        row = y;
        col = x;
    }
}

// ------ clear a window to all blamks
void Window::clear_window(void)
{
    cursor(0,0);
    clreos();
}

// --- clear from current cursor position to end of window
void Window::clreos(void)
{
    unsigned rw = row, cl = col;
    clreol();
    col = 0;
    while (++row < HEIGHT-2)
        clreol();
    row = rw;
    col = cl;
}

// --- clear from current cursor position to end of line
void Window::clreol(void)
{
    unsigned cl = col;
    colors(fg,bg);
    while (col < WIDTH-2)
        *this << ' ';
    col = cl;
}

// ----- page and scroll through the text file
void Window::page(void)
{
    int c = 0, lines = 0;
    char **tx = text;

    hidecursor();
    // ------ count the lines of text
    while (*(tx + lines) != NULL)
        lines++;
    while (c != ESC)    {
        c = getkey();
        char **htext = text;
        switch (c)  {
            case UP:
                if (tx != text) {
                    --tx;
                    scroll(-1);
                    unsigned x, y;
                    cursor(&x, &y);
                    cursor(0, 0);
                    *this << *tx;
                    cursor(x, y);
                }
                continue;
            case DN:
                if (tx+HEIGHT-3 < text+lines-1) {
                    tx++;
                    scroll(1);
                    unsigned x, y;
                    cursor(&x, &y);
                    cursor(0, HEIGHT-3);
                    *this << *(tx + HEIGHT - 3);
                    cursor(x, y);
                }
                continue;
            case PGUP:
                tx -= HEIGHT-2;
                if (tx < text)
                    tx = text;
                break;
            case PGDN:
                tx += HEIGHT-2;
                if (tx+HEIGHT-3 < text+lines-1)
                    break;
            case END:
                tx = text+lines-(HEIGHT-2);
                if (tx > text)
                    break;
            case HOME:
                tx = text;
                break;
            default:
                continue;
        }
        *this << tx;
        text = htext;
        clreos();
    }
    unhidecursor();
}

// --------- scroll a window
void Window::scroll(int d)
{
    videoscroll(d, tp+1, lf+1, bt-1, rt-1, fg, bg);
}

// ------ utility notice window
Notice::Notice(char *text)
    : ((SCREENWIDTH-(strlen(text)+2)) / 2, 11,
        ((SCREENWIDTH-(strlen(text)+2)) / 2) + strlen(text)+2,
        14, NOTICEFG, NOTICEBG)
{
    *this << text << "\n Any key ...";
    hidecursor();
    getkey();
    unhidecursor();
    hidewindow();
}

// ------ utility error window
Error::Error(char *text)
    : ( (SCREENWIDTH-(strlen(text)+2)) / 2, 11,
        ((SCREENWIDTH-(strlen(text)+2)) / 2) + strlen(text)+2,
        14, ERRORFG, ERRORBG)
{
    *this << text << "\n Any key ...";
    hidecursor();
    getkey();
    unhidecursor();
    hidewindow();
}

// ------ utility yes/no window
YesNo::YesNo(char *text)
    : ( (SCREENWIDTH-(strlen(text)+10)) / 2, 11,
        ((SCREENWIDTH-(strlen(text)+10)) / 2) + strlen(text)+10,
        13, YESNOFG, YESNOBG)
{
    *this << text << "? (Y/N) ";
    int c = 0;
    hidecursor();
    while (tolower(c) != 'y' && tolower(c) != 'n')
        c = getkey();
    unhidecursor();
    hidewindow();
    answer = tolower(c) == 'y';
}
```

## LISTING 3

```cpp
/* ----------- console.h -------- */

#ifndef CONSOLE
#define CONSOLE

#include <disp.h>

// -------- cursor and keyboard functions (via BIOS)
void hidecursor(void);
void unhidecursor(void);
void savecursor(void);
void restorecursor(void);
int getkey(void);

// -------- key values returned by getkey()
#define BELL      7
#define ESC      27
#define UP      200
#define BS      203
#define FWD     205
#define DN      208
#define HOME    199
#define END     207
#define PGUP    201
#define PGDN    209

#define attr(fg,bg) ((fg)+(((bg)&7)<<4))

// --------- video functions (defined as Zortech C++ equivalents)
#define initconsole()            disp_open()
#define closeconsole()           disp_flush()
#define savevideo(bf,t,l,b,r)    disp_peekbox(bf,t,l,b,r)
#define restorevideo(bf,t,l,b,r) disp_pokebox(bf,t,l,b,r)
#define box(t,l,b,r,fg,bg)       disp_box(1,attr(fg,bg),t,l,b,r)
#define colors(fg,bg)            disp_setattr(attr(fg,bg))
#define setcursor(x,y)           disp_move(y,x)
#define window_printf            disp_printf
#define window_putc              disp_putc
#define videoscroll(d,t,l,b,r,fg,bg) \
        disp_scroll(d,t,l,b,r,attr(fg,bg));


#endif
```

## LISTING 4

```cpp
/* ----------- console.c --------- */

/* PC-specific console functions */

#include <dos.h>
#include <conio.h>
#include "console.h"

/* ------- video BIOS (0x10) functions --------- */
#define VIDEO         0x10
#define SETCURSORTYPE 1
#define SETCURSOR     2
#define READCURSOR    3
#define HIDECURSOR    0x20

#define SAVEDEPTH     20  /* depth to which cursors are saved */

static int cursorpos[SAVEDEPTH];
static int cursorshape[SAVEDEPTH];
static int sd;

union REGS rg;

/* ---- Low-level get cursor shape and position ---- */
static void getcursor(void)
{
    rg.h.ah = READCURSOR;
    rg.h.bh = 0;
    int86(VIDEO,&rg,&rg);
}

/* ---- Save the current cursor configuration ---- */
void savecursor(void)
{
    getcursor();
    if (sd < SAVEDEPTH) {
        cursorshape[sd] = rg.x.cx;
        cursorpos[sd++] = rg.x.dx;
    }
}

/* ---- Restore the saved cursor configuration ---- */
void restorecursor(void)
{
    if (sd) {
        rg.h.ah = SETCURSOR;
        rg.h.bh = 0;
        rg.x.dx = cursorpos[--sd];
        int86(VIDEO,&rg,&rg);
        rg.h.ah = SETCURSORTYPE;
        rg.x.cx = cursorshape[sd];
        int86(VIDEO,&rg,&rg);
    }
}

/* ---- Hide the cursor ---- */
void hidecursor(void)
{
    getcursor();
    rg.h.ch |= HIDECURSOR;
    rg.h.ah = SETCURSORTYPE;
    int86(VIDEO,&rg,&rg);
}

/* ---- Unhide the cursor ---- */
void unhidecursor(void)
{
    getcursor();
    rg.h.ch &= ~HIDECURSOR;
    rg.h.ah = SETCURSORTYPE;
    int86(VIDEO,&rg,&rg);
}

/* ---- Read a keystroke ---- */
int getkey(void)
{
    rg.h.ah = 0;
    int86(0x16,&rg,&rg);
    if (rg.h.al == 0)
        return (rg.h.ah | 0x80) & 255;
    return rg.h.al & 255;
}
```

## LISTING 5

```cpp
// ---------- look.c

// A C++ program to demonstrate the use of the window library.
// This program lets you view a text file

#include <stdio.h>
#include <string.h>
#include <stream.hpp>
#include <stdlib.h>
#include "window.h"

#define MAXLINES 200            // maximum number of text lines

static char *wtext[MAXLINES+1]; // pointers to text lines

// --- taken from BS; handles all free store (heap) exhaustions
void out_of_store(void);
typedef void (*PF)();
extern PF set_new_handler(PF);

main(int argc, char *argv[])
{
    set_new_handler(&out_of_store);
    if (argc > 1)   {
        // ---- open a full-screen window
        Window wnd(0,0,79,24,CYAN,BLUE);
        char ttl[80];
        // ------ put the file name in the title
        sprintf(ttl, "Viewing %s", argv[1]);
        wnd.title(ttl);
        filebuf buf;
        if (buf.open(argv[1], input))   {
            istream infile(&buf);
            int t = 0;
            // --- read the file and load the pointer array
            char bf[120], *cp = bf;
            while (t < MAXLINES && !infile.eof())   {
                infile.get(*cp);
                if (*cp != '\r')    {
                    if (*cp == '\n')    {
                        *cp = '\0';
                        wtext[t] = new char [strlen(bf)+1];
                        strcpy(wtext[t++], bf);
                        cp = bf;
                    }
                    else
                        cp++;
                }
            }
            wtext[t] = NULL;
            // ---- write all the text to the window
            wnd << wtext;
            // ---- a YesNo window
            YesNo yn("Continue");
            if (yn.answer)
                wnd.page();
            // ------ a Notice window
            Notice nt("All done.");
        }
        else
            // ------ error windows
            Error err("No such file");
    }
    else
        Error err("No file name specified");
}

// ----- the BS free-store exhaustion handler
void out_of_store(void)
{
    cerr << "operator new failed: out of store\n";
    exit(1);
}
```
