
# C programiranje 05

## Pomoć osetljiva na kontekst i TWRP, mali procesor teksta

Al Stevens, decembar '88

Tokom poslednjih nekoliko meseci, sastavljali smo biblioteku alata orijentisanih na prozore koji će se koristiti u razvoju komunikacionog programa. Prošlomesečna kolumna predstavljala je mehanizam za uređivanje teksta koji vam omogućava da sakupite tekst slobodnog oblika koji je uneo korisnik u bafer kroz prozor. Ovog meseca ćemo okružiti taj motor strukturom menija i dodati funkcije za upravljanje datotekama i pretraživanje teksta. Međutim, prvo moramo da dovršimo kolekciju alata za prozore dodavanjem funkcije pomoći koja je osetljiva na kontekst.

Većina PC programa nudi korisniku neku vrstu pomoći na mreži. Ova pomoć obično ima oblik tekstualnih prozora koji se pojavljuju i objašnjavaju nešto što korisnik treba da zna. Kada tekst automatski odražava trenutnu poziciju korisnika u programu, za tekst pomoći se kaže da je "osetljiv na kontekst".

Funkcija pomoći koja je osetljiva na kontekst za naš projekat C programiranja je implementirana u dve izvorne datoteke. Ovo su [Listing 1](#listing-1), "help.h", i [Listing 2](#listing-2), "help.c". Kod iz prethodnih meseci je uključivao konstrukcije za podršku funkcijama pomoći. Funkcije unosa podataka i menija u oktobru omogućavaju identifikaciju prozora pomoći za objašnjenje svakog izbora menija i polja za unos podataka. Strukture pronađene u "input.h" i "menu.h" već imaju mesta za mnemoniku prozora i funkciju "getkey" ("window.c", septembar DDJ već traži interventni taster za pomoć i poziva funkciju pomoći.

Prozori pomoći su opisani u ASCII tekstualnoj datoteci. Svaki prozor ima mnemoniku i tekst. Mnemonika ima od 1 do 8 karaktera, a tekst može biti širok do 78 karaktera i dubok 23 reda. Svaki mnemonik je okružen ugaonim zagradama, pojavljuje se na početku reda i jedini je unos. na liniji. Tekst za prozor prati njegovu mnemoniku i završava se pojavom mnemonika sledećeg prozora ili `end` mnemonika koji završava datoteku. [Listing 3](#listing-3), je "twrp.hlp", tekstualna datoteka pomoći koju ćemo koristiti u ovomesečnoj ekspanziji uređivača. Pogledajte ovaj listing sada da biste razumeli ideju. Kasnije ćete možda želeti da promenite datoteku tako da odražava promene koje izvršite u komandama uređivača kada prilagodite uređivač sopstvenim željenim skupom komandi.

Funkcija "getkey" prati dve globalne promenljive - pokazivač funkcije pod nazivom "helpfunc" i ceo broj pod nazivom "helpkey". Ako program želi da presretne pritisak na taster za pomoć, može da dodeli adresu funkcije i vrednost pritiska na ove promenljive. Zatim, ako "getkey" oseti da je korisnik pritisnuo dodeljeni taster za pomoć, poziva dodeljenu funkciju.

Izvorna datoteka pod nazivom "help.c" uključuje funkciju "load_help", koju aplikativni program poziva da učita tekstualnu datoteku pomoći. Funkcija dodeljuje funkcijski taster pomoći (F1 u našoj implementaciji) tako što inicijalizuje ceo broj ključa pomoći i dodeljuje sopstvenu funkciju pomoći ("display_help") inicijalizacijom pokazivača "helpfunc". Fajl "help.h" sadrži makro, skup koji omogućava aplikacijskoj funkciji da imenuje mnemoniku za prozor pomoći. Sa ovim makroom, aplikacija može da održava odgovarajući trenutni prozor za kontekst pomoći programa. Izvorne datoteke "entry.h" i "menu.h" iz oktobra definišu strukture koje aplikativni program inicijalizuje da bi koristio funkcije unosa podataka i menija. Strukture uključuju pokazivače na pokazivače znakova, koji, ako se inicijaliziraju, pružaju mnemoniku prozora za prozore pomoći za izbor menija i polja za unos podataka. Funkcija "display_help" otkriva da je aplikacija koristila set-help za uspostavljanje prozora pomoći, da je kursor menija na izboru menija ili da se polje za unos podataka obrađuje. Funkcija "display_help" tako bira odgovarajući prozor ako korisnik pritisne taster za pomoć. Sa ovom mogućnošću pomoći koja se primenjuje zajedno sa drugim funkcijama prozora, programer aplikacija može da koristi prozore pomoći osetljive na kontekst za menije, ekrane za unos podataka i druge režime programa. Programer sistema treba da uradi samo sledeće stvari da bi primenio funkciju:

1. Napravite tekstualnu datoteku pomoći.
2. Ubacite mnemoniku prozora u meni i strukture za unos podataka.
3. Ubacite makroe "set-help" u program aplikacije za prozore pomoći koji nisu povezani sa menijima ili poljima za unos podataka.

## Mali procesor teksta

Obećao sam vam prošireni uređivač teksta, onaj koji će podržati komunikacioni program. Za ovaj uređivač koristićemo funkciju uređivača prozora od novembra, funkcije unosa podataka i menija od oktobra, i upravo opisane funkcije prozora pomoći osetljive na kontekst. Ovaj prošireni uređivač možemo nazvati iz aplikacijskih programa baš kao što možemo nazvati manje dobro opremljen uređivač prošlog meseca. Da pružim primer proširenog uređivača, napravio sam jednostavan prednji kraj koji koristi uređivač na način na koji će ga koristiti naš uslužni program -- kao mali procesor teksta. Sa prednjim delom, program postaje samostalni uređivački paket sa minimalnim funkcijama i mogućnostima za obradu teksta.

Ovaj novi program označava prekretnicu - koristim ga za pisanje ove kolumne. Ova drastična mera je test za otkrivanje skrivenih grešaka. To je rizičan poduhvat jer ću izgubiti posao ako se ovaj neprovereni urednik sruši ili ne uspe da ispravno sačuva moju besmrtnu prozu. Iz tog razloga ću mnogo odstupiti od kucanja da bih sačuvao tekst i pogledao šta je sačuvano - uključujući i svetost mog razuma. Ovaj novi program je prvi put da koristimo alate u stvarnoj aplikaciji. Kao moj sopstveni beta tester, nalazim stvari koje treba promeniti. Prva modifikacija koju sam napravio je boja urednika, čineći ih prijatnijim od blage monohromatske kombinacije u prozoru.h podrazumevanim. Ovi odabiri su lični; želećete da napravite svoje.

Sledi prijatno otkriće. Veliki deo mog posla obavljam u avionima, a koristim i Toshiba T1000 laptop MS-DOS računar bez upotrebe – niska cena, mala težina, minimalne mogućnosti. Kako se to dešava, ove reči se pišu između Dalasa i Feniksa. Jedan od nedostataka ove najpovoljnije mašine je to što je potrebno večno da se učita bilo koji od potpuno funkcionalnih procesora teksta sa jedne diskete od 3,5 inča. Ali naš novi mali procesor teksta je samo 30K i učitava se za nekoliko sekundi, rešavajući tako problem star mesecima. Sada moram da se pozabavim drugim problemom. Kad god izvučem T1000, postavim ga na sto za poslužavnik i duboko se zaokupim, moj komšija na sledećem sedištu se zainteresuje za slatku malu mašinu. Ovo dovodi do neizbežnog tapkanja po ruci i naknadne neželjene kompjuterske diskusije u kojoj čujem o putnikovim kancelarijskim računarima i kako on ne zna ništa o njima (ali njegov sin je čarolija). Ljudi uvek pitaju gde sam nabavio T1000, koliko košta i za šta ga koristim. Moj drugar Bil Čejni bi nešto gunđao o vođenju evidencije o uzgoju pit bulova, ništa od toga ne bi bilo tačno, ali mu više ne bi smetalo i mogao bi da radi neometano.

Da bismo napravili mali procesor teksta, koji ću sada nazvati TVRP (izgovara se "tverp"), okružujemo prozorski uređivač teksta od prošlog meseca školjkom menija i nekim funkcijama za upravljanje datotekama. Ove funkcije koriste meni i funkcije unosa podataka od oktobra. Želimo da uređivač bude dostupan na dva načina: kao samostalni program i kao funkcija koja se može pozvati iz aplikacijskog programa. TVRP je napravljen od dve izvorne datoteke. [Listing 4](#listing-4), je "editshel.c". Ova datoteka sadrži školjku menija i napredne komande uređivača za upravljanje datotekama i pretraživanje teksta. Sa "editshel.c", aplikacijski program može da se poveže sa naprednim funkcijama uređivača. [Listing 5](#listing-5), je "twrp.c", spoljni sloj koji koristi napredni uređivač za izgradnju TWRP-a kao samostalnog programa.

Primetite da "twrp.c" sadrži poziv "load_help" za učitavanje datoteke pomoći za uređivač. Izolacijom ovog poziva u spoljašnjoj funkciji, zadržavamo mogućnost da drugi programi koji pozivaju uređivač imaju druge, možda složenije, datoteke pomoći.

"editshel.c" sadrži skup proširenih komandi kao iskaze `case` u naredbi `switch`. Funkcija koja sadrži ovaj prekidač se zove meni za uređivanje. Ova funkcija predstavlja tehniku koju ćemo koristiti za dodavanje funkcija uređivaču po potrebi. Kada "editor.c" naiđe na pritisak na taster koji ne prepoznaje, on poziva funkciju adresiranu u eksternom pokazivaču funkcije pod nazivom "edit_func". Ako je pokazivač NULL, urednik zvoni zvonom da bi označio da je komanda nevažeća. Inicijalizacijom tog pokazivača možemo proširiti skup komandi. Stavljanjem sličnog lanca pokazivača funkcija na kraj našeg proširenog skupa komandi, možemo držati otvorena vrata za buduća proširenja. Bilo bi lakše dodati slučajeve originalnom editorovom prekidaču za dispečiranje komandi, ali onda bismo morali da objavimo modifikovani "editor.c" svaki put kada dodamo funkcije uređivaču. Ovim pristupom svaka ekstenzija je sadržana u izvornom modulu koji ima kod za proširenje. Još jedna prednost naše strategije proširenja je ta što uređivaču daje više nivoa funkcionalnosti. Trenutni nivo zavisi od konteksta u kome se uređivač izvršava. Program koji koristi prošireni uređivač takođe može da koristi jednostavniji uređivač prozora za prikupljanje malih količina teksta iz drugih razloga. To pozivanje uređivača ne bi, na primer, imalo komande za upravljanje datotekama jer bi pokazivač funkcije ekstenzije bio NULL za vreme trajanja sesije uređivanja.

Pošto razgovaramo o proširenim komandama uređivača, trebalo bi da razmotrimo šta rade veliki procesori teksta što TWRP ne može. TWRP nema proveru pravopisa ili tezaurus, nema makroe za tastaturu, nema fontove, nema kolone, nema generator indeksa ili tabele sadržaja, nema stilova, nema fusnota. Zvuči mnogo kao WordStar iz prošlosti. Ali sačekajte, ima više -- ili manje, u zavisnosti od toga kako na to gledate. TWRP nema donje crte, podebljano, niti subscriptu i superskriptu; bez paginacije; nema spajanja pošte; nema zaglavlja i podnožja; nema štampanja; nema promenljivih margina.

Pa šta je to dobro i čemu služi? TWRP je prvenstveno alat za sastavljanje teksta, koji podržava najosnovnije aktivnosti unosa teksta koje su potrebne piscima svega, od beleški do rukopisa. Ukucate tekst, a TWRP ga čuva za vas. Možete učitavati i čuvati tekstualne datoteke; spajanje teksta iz drugih datoteka; formirati paragrafe; označite blokove; i pomerajte, kopirajte i brišite blokove.

TWRP je u velikoj meri sličan popularnim programima za beležnice za radnu površinu. Ima, međutim, jedan jedinstven i dopadljiv kvalitet – može se povezati u naše programe kao in-line procesor teksta, potpuno integrisan sa svim ostalim što se dešava. Štaviše, izvorni kod je naš i možemo ga naterati da radi šta god želimo.

Ovo zapažanje nas vraća na temu makroa. TWRP nema makroe na tastaturi u stilu mnogih uređivača i procesora teksta. Brief, urednik programera svetske klase iz Solution Sistems-a, ima makro jezik koji podseća na Lisp. ME editor kompanije Magma Softvare Sistems i njegov derivat, njujorški program za obradu teksta, imaju makro jezike koji liče na C. Novi Microsoft Editor koristi kompajlirane C ekstenzije za napredne makroe. Borlandov Sprint procesor teksta ima proceduralni makro jezik. Kao i XiWrite iz XiQuest-a. Svi ovi urednici i programi za obradu teksta su sastavili ili interpretirali makro jezike, od kojih neki liče na C.

U tom duhu, dakle, TWRP je, po deklaraciji i aklamaciji, blagosloven makro jezikom koji ne samo da liči na C, već je i C. Ako želite da dodate makro, dodajte funkciju proširenja na način koji je prethodno opisan, napišite proceduru kao C funkciju i ponovo kompajlirate i povežete TWRP. Koji je bolji način za C programera da doda složeni kod makro uređivaču?

## MAKE datoteke

[Listing 6](#listing-6) je "twrp.prj", datoteka za izradu Turbo C projekta za kompajler Turbo C okruženja. Koristite model kompaktne memorije sa omogućenom proverom svih grešaka. Biće vam potrebni izvorni fajlovi iz septembra, oktobra, novembra i ovomesečne kolone C programiranje.

## Greška u skupu alata C

Moja upotreba alata je otkrila nekoliko manjih problema koje ću ovde pozabaviti. Kada sam prešao na model `compact` da bih podržao bafer za uređivanje od 800 redova, pojavila se greška u "window.c" (septembar DDJ). Funkcija "Writeline" ima neko neprenosno žongliranje pokazivačima koje treba pooštriti. Zamenite ove linije za poziv funkcije "_vram":

```c
_vram(_vptr(x+wkw.lf-1, y+wkw.tp-1),  (int far *) cl,
   (unsigned) ((int far *) cp - (int far *) cl));
```

Funkcija izbora menija u "menu.c" nije uspela da resetuje globalni pokazivač nazvan na NULL pre povratka. Ubacite ovu izjavu pre nego što se vratite iz menija izaberite na dnu funkcije:

```c
    mn = NULL;
```

Ažuriraću verzije za preuzimanje ovih datoteka na CompuServe-u kako bi odražavale ove ispravke.

## Turbo C2.O

Svi alati za prozore iz kolumne C programiranja su napravljeni sa Turbo C, verzija 2.0. Ovo je razvoj u poslednjem trenutku; 2.0 nije dugo izašao. Velika vest je integrisani debager u Turbo C okruženju; svi su dugo čekali na tu funkciju o kojoj se pričalo, a Turbo C je sada uklonio poslednji razlog zbog kojeg se osećate inferiornim u odnosu na QuickC. Proveo sam dosta vremena koristeći program za otklanjanje grešaka i mogu bez rezerve reći da ćete biti oduševljeni njime. Posebno sam impresioniran njegovom besprekornom integracijom sa okruženjem urednika i kompajlera. Naš mali TVRP projekat ni u kom slučaju nije iscrpan test kompajlera, tako da je ova preporuka preliminarna, ali entuzijastična.

Turbo C ima novi iskačući uslužni program pod nazivom THELP koji koristi Turbo on-line datoteku pomoći. Ovo je značajan dodatak paketu. Znate kako Ctrl-Fl funkcioniše u TC okruženju – pojavljuje se kontekstno osetljiv prozor pomoći o Turbo C-u. Sada možete imati prikaze pomoći za isti jezik i kompajler iz vašeg ličnog uređivača teksta (uključujući TWRP). Interventni taster THELP otvara prozore Turbo pomoći za sve što radite. Ako je kursor na Turbo ključnoj reči - možda nazivu funkcije biblioteke - prikazuje se prozor pomoći koji se odnosi na tu određenu reč.

## TesSeRact

THELP je napravljen od TesSeRact sharevare programske biblioteke rezidentne memorije. Ova biblioteka je dostupna u datotekama koje možete preuzeti sa CompuServe-a u bibliotekama CL i Borland i sa drugih BBS-ova. Izvorni kod je dostupan registrovanim korisnicima.

Za TesSeRact se kaže da je odgovor na molitvu programera za ukidanje i boravak (TSR), u stanju da od bilo kog programa napravi TSR sa dobrim ponašanjem koji može da radi u harmoničnom skladu sa drugim programima, TSR-om i drugim programima. Ne verujem u to u potpunosti, ali ako želite da pišete TSR-ove bez razumevanja kako oni funkcionišu, ovo je dobar način. Dokumentacija objašnjava kako se koriste biblioteke koje su dostupne za C, asemblerski jezik i Pascal. Da biste koristili biblioteke u programima koje distribuirate, morate registrovati svoju upotrebu i zadržati TesSeRact potpis u kodu. Tako sam shvatio da je ThELP TesSeRact program.

Programeri TesSeRact-a su stručnjaci za crnu umetnost TSR programiranja. Razvojni tim je ono što je ostalo od projekta Ringmaster, neuspelog pokušaja industrije da se zadovolji standardom za TSR. S obzirom na nedostatak entuzijazma među privatnim izdavačima softvera da se prijave za takav standard, neki od tima su se odvojili i prihvatili pristup sharevare-a. Biblioteke koje su rezultirale obezbeđuju razumno bezbedan način da se TSR program pokrene uz minimalnu gužvu. Pošto sam istraživao, razvijao i opširno pisao o TSR programima, uveren sam da ne postoji način da se napiše jedan za koji se garantuje da će raditi u svim hardverskim okruženjima računara i u društvu svih drugih programa i sistema. MS-DOS jednostavno nije trebalo da se koristi na taj način, a svi ti uspešni TSR-ovi su nasleđe generacije hakera koji su smislili načine da zaobiđu ograničenja DOS-a - čak i ako mnogi programi ne rade jedni sa drugima. TSR-ovi, radili ili ne, biće sa nama sve dok je MS-DOS sa nama, a to će biti dugo vremena.

## Kroki: Problemi sa programiranjem

Problem sa programiranjem je što ne postaje ništa lakše. Upravo suprotno, programiranje postaje sve teže.

Poslednjih meseci gledam nova ili drugačija okruženja za razvoj softvera. U nedavnoj kolumni ispričao sam o svom prvom upoznavanju sa C++ i objektno orijentisanim programiranjem, veoma ponižavajućem iskustvu. Još jedan poduhvat u nepoznato (za mene, dakle) bio je moj izlet u Macintosh. Neko je primetio da je dobrom programeru potrebno najmanje godinu dana da postane kompetentan Mac programer na C ili bilo kom drugom jeziku.

Sada otvaram OS/2 Softvare Development Kit (SDK) od Microsofta. Sedamdeset pet funti priručnika i disketa i osam video kaseta čine ovu strašnu zver. Na prvi pogled se čini da je otkrivanje onoga što priručnici sadrže koje informacije samo po sebi ogroman napor, a da ne pominjemo proces učenja koji sledi. Tek sam počeo, i pitam se koliko će plavih meseci proći pre nego što vidim OS/2 grupu ekrana koja prikazuje moju sopstvenu poruku "Zdravo, svet".

Ne, programiranje ne postaje lakše. Sećate se obećanja generatora aplikacija i jezika četvrte generacije koji će dizajn sistema i programiranje staviti u domet Everyman-a? Novi talas dBase i NOMAD pseudoprogramera će nas sve izbaciti iz posla. Ne brinite. Složenost ovih novih platformi za razvoj softvera obezbeđuje mesto za talente na nivou hakera u godinama koje dolaze.

Najbolji deo ove postepene evolucije ka komplikovanijem programiranju je to što ne moramo da učimo novi jezik. C je opstao i još uvek je taj preopterećeni kliše, "jezik izbora" – nije da će biti lako zapamtiti 900+ poziva OS/2 funkcija, ali barem je njihova osnova u sintaksi najboljeg programskog jezika ikada osmišljenog.

Međutim, to podiže moje C čistunce kada vidim da su deklaracije funkcija kojima je dodeljen paskal tip u svojim prototipovima. Sreća je što je većina tih mrlja skrivena u datotekama zaglavlja. Zaista!

## Knjiga za sva godišnja doba

Kada se uhvatite u koštac sa OS/2, planirajte da potrošite nešto novca u knjižari. Moraćete da uvećate 75 funti nejasnih Microsoft priručnika sa još nekoliko čitljivijih ponuda drugih autora. Imam većinu knjiga koje su napisane o OS/2, a jedna se ističe kao suštinska prva knjiga za čitanje. Njegov naslov je "Inside OS/2", a autor je Gordon Letvin. Ne mešajte ovu knjigu sa drugom sa istim naslovom. Ovu je objavio Microsoft Press, a zasluga Microsoft uključuje njenu kopiju uz SDK. "Inside OS/2" nije jedina knjiga koja će vam trebati, ali je prva. Pročitajte je čak i pre nego što pogledate video trake. Na taj način, ako prespavate nešto važno, velike su šanse da je Letvin to već objasnio.

"Inside OS/2" je odličan opis osnovnih principa dizajna za OS/2. Pomoći će ako već znate nešto o teoriji operativnih sistema. Razumevanje koncepata objašnjenih u ovom radu je od suštinskog značaja za dizajn programa koji bi efikasno koristili napredne karakteristike OS/2. Sviđa mi se ova knjiga.

(Mala starica na susednom sedištu gleda moj T1000. Uskoro će me tapkati po ruci. Gde si, Bill Čejni, kad mi trebaš?)

## LISTING 1

```c
/*------------ help.h --------------*/

void load_help(char *);
void display_help(void);

extern char *help_window;
#define set_help(s) help_window=s

```

## LISTING 2

```c
/*--------- help.c ----------- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <conio.h>
#include "window.h"
#include "menu.h"
#include "entry.h"
#include "help.h"

#define MAXHELPS 25

static struct helps {
    char hname [9];
    int h, w;
    long hptr;
} hps [MAXHELPS+1];

extern FIELD *fld;
extern MENU *mn;

static int hp = 0;
static FILE *helpfp;
static char hline [80];
char *help_window;

/*----------- load the HELP! definition file ------------ */
void load_help(char *hn)
{
    extern void (*helpfunc)(void);
    extern int helpkey;
    char *cp;

    helpfunc = display_help;
    helpkey = F1;
    hp = 0;
    if ((helpfp = fopen(hn, "r")) == NULL)
        return;
    if ((fgets(hline, 80, helpfp)) == NULL)
        return;
    while (1)   {
        if (hp == MAXHELPS)
            break;
        if (strncmp(hline, "<end>", 5) == 0)
            break;
        if (*hline != '<')
            continue;
        hps[hp].h = 2;
        hps[hp].w = 23;
        strncpy(hps[hp].hname, hline+1, 8);
        hps[hp].hname[8] = '\0';
        cp = strchr(hps[hp].hname, '>');
        if (cp)
            *cp = '\0';
        hps[hp].hptr = ftell(helpfp);
        if (fgets(hline, 80, helpfp) == NULL)
            strcpy(hline, "<end>");
        while (hline[0] != '<') {
            hps[hp].h++;
            hps[hp].w = max(hps[hp].w, strlen(hline)+2);
        if (fgets(hline, 80, helpfp) == NULL)
            strcpy(hline, "<end>");
        }
        hp++;
    }
}

/*---------- display the current help window ----------- */
void display_help()
{
    int hx, hy, i, xx, yy, ch;
    extern int helpkey, hsel;
    char *save_help;
    static int inhelp = 0;
    extern struct wn wkw;

    if (inhelp)
        return;
    inhelp++;
    save_help = help_window;
    if (fld != NULL)
        help_window = fld->fhelp;
    else if (mn != NULL)
        help_window = (mn+hsel-1)->mshelp [wkw.wy-1];
    if (help_window != NULL)    {
        for (ch = 0; ch < hp; ch++)
            if (strcmp(help_window, hps[ch].hname) == 0)
                break;
        if (ch < hp)    {
            xx = wherex();
            yy = wherey();
            hidecursor();
            hx = ((80-hps[ch].w) / 2)+1;
            hy = ((25-hps[ch].h) / 2)+1;
            establish_window(hx, hy,
                    hx+hps[ch].w-1, hy+hps[ch].h,
                    HELPFG, HELPBG, TRUE);
            fseek(helpfp, hps[ch].hptr, 0);
            for (i = 0; i < hps[ch].h-2; i++)   {
                gotoxy(2,2+i);
                fgets(hline, 80, helpfp);
                cprintf(hline);
            }
            gotoxy(2,2+i);
            cprintf(" [Any key to return]");
            hidecursor();
            getkey();
            delete_window();
            if (mn == NULL || fld != NULL)    {
                textcolor(FIELDFG);
                textbackground(FIELDBG);
                gotoxy(xx, yy);
            }
        }
    }
    help_window = save_help;
    --inhelp;
}
```

## LISTING 3

```sh
<editor>
              TINY WORD PROCESSOR (TWP) COMMANDS
-------Cursor Movement------   ---------Page Movement--------
arrows    = move text cursor   Ctrl-Home = Beginning of File
Ctrl-T    = Top of Window      Ctrl-End  = End of File
Ctrl-B    = Bottom of Window   PgUp      = Previous Page
Ctrl ->   = Next Word          PgDn      = Next Page
Ctrl <-   = Previous Word
Home      = Beginning of Line  ---------Editor Control-------
End       = End of Line        Alt-A  = Auto Paragraph Reform
Shift-Tab = Back tab

--------Block Controls------   ---------Edit Commands--------
F2  = Form Paragraph           Alt-Q or Esc = Done
F5  = Mark Block Beginning     Ins          = Insert Mode
F6  = Mark Block End           Del          = Delete Char
F3  = Move Block               <--          = Rubout
F4  = Copy Block               Ctrl-D       = Delete Word
F8  = Delete Block             Alt-D        = Delete Line
F9  = Unmark Block             F7           = Find
                               Alt-F7       = Find again
<load>
Load a new file into TWP, replacing the existing file.

<save>
Save the file from the edit buffer.

<merge>
Merge a file into the edit buffer at the line where the cursor is pointed.

<new>
Clear the edit buffer and create a new file.

<quit>
Exit from TWP, returning to DOS

<move>
Move the block to the line where the cursor points. This is an insert move.

<copy>
Copy the block to the line where the cursor points. This is an insert copy.

<delete>
Delete the block closing the space it occupies.

<hide>
Turn off the block markers.

<formpara>
Form a paragraph. This makes a paragraph from a marked block, or, if no block is marked, to the next
blank or indented line.

<markbeg>
Mark the beginning line of a block for move, copy, delete, or paragraph.

<markend>
Mark the ending line of a block for move, copy, delete, or paragraph.

<find>
Find a specified string in the edit buffer. Move the cursor to the location where the string was found.

<findagn>
Find the next occurrence of the string most recently specified.

<auto>
Turn on/off the automatic paragraph forming feature.

<insert>
Turn on/off the character insert mode. (insert/overstrike toggle)

<filename>
Enter the path and file name. The path is optional but must be fully qualified if entered.

<findstr>
Enter the string to be searched by the Find command.
<end>
```

## LISTING 4

```c
/*--------- editshel.c ------------ */
#include <stdio.h>
#include <string.h>
#include <conio.h>
#include <stdlib.h>
#include <mem.h>
#include <alloc.h>
#include <ctype.h>
#include "window.h"
#include "menu.h"
#include "entry.h"
#include "editor.h"
#include "help.h"

int MAXLINES;           /* maximum number of editor lines */
#define EDITWIDTH  78   /* length of an editor line       */
#define BUFLEN (EDITWIDTH*MAXLINES)

/*--------- alt keys returned by getkey() --------- */
#define ALT_A 158
#define ALT_L 166
#define ALT_M 178
#define ALT_N 177

/*-------- configured advanced editor commands --------- */
#define FIND            F7
#define FIND_AGAIN      ALT_F7
#define LOAD_FILE       ALT_L
#define SAVE_FILE       ALT_S
#define MERGE_FILE      ALT_M
#define NEW_FILE        ALT_N
#define EDITOR_MENU     F10
#define REFORM          ALT_A

/*---------- editor menu tables --------- */
static char *fselcs[] = {
    "Load   [Alt-L]",
    "Save   [Alt-S]",
    "Merge  [Alt-M]",
    "New    [Alt-N]",
    "Quit   [Alt-Q]",
    NULL
};

static char *filehelp[] = {
    "load",
    "save",
    "merge",
    "new",
    "quit"
};

static char *eselcs[] = {
    "Move         [F3]",
    "Copy         [F4]",
    "Delete       [F8]",
    "Hide         [F9]",
    "Paragraph    [F2]",
    "Mark Beg     [F5]",
    "Mark End     [F6]",
    "Find         [F7]",
    "Find Again   [Alt-F7]",
    NULL
};

static char *edithelp[] = {
    "move",
    "copy",
    "delete",
    "hide",
    "formpara",
    "markbeg",
    "markend",
    "find",
    "findagn",
};

static char *oselcs[] = {
    "Auto Paragraph Reformat   [Alt-A]",
    "Insert                    [Ins]",
    NULL
};

static char *opthelp[] = {
    "auto",
    "insert"
};

void fileedit(char *);
static int  edit(int,int);
static void editmenu(int);
static int  get_filename(char *, int);
static int  write_file(void);
static int  load_file(int,int);
static int  save_file(int,int);
static int  merge_file(int,int);
static int  new_file(int,int);
static void editkeys(int);
static void statusline(void);
static int  findstring(void);
static char *find(char *, unsigned);
static int  read_file(char *, char *, int, int, int);
static int  bufferok(char *);
static void notice(char *);
void (*edit_extend)(void);

static char fkeys[] =   {LOAD_FILE,SAVE_FILE,
                         MERGE_FILE,NEW_FILE,QUIT};
static char forced[] =  {MOVE_BLOCK,COPY_BLOCK,
                         DELETE_BLOCK,HIDE_BLOCK,
                         PARAGRAPH,BEGIN_BLOCK,
                         END_BLOCK,FIND,FIND_AGAIN};
static char options[] = {REFORM,INS};

static int (*ffuncs[])() =
    {load_file,save_file,merge_file,new_file,edit};
static int (*efuncs[])() =
    {edit,edit,edit,edit,edit,edit,edit,edit,edit};
static int (*ofuncs[])() =
    {edit,edit,edit};

MENU emn [] = {
    {"File",    NULL, fselcs, filehelp, fkeys,   ffuncs, 0},
    {"Edit",    NULL, eselcs, edithelp, forced,  efuncs, 0},
    {"Options", NULL, oselcs, opthelp,  options, ofuncs, 0},
    {NULL}
};

/*------ filename data entry template and buffer ------- */
static char filename[65];
static char savefn [65];
static char filemask[65];

FIELD fn_template[] = {
    {2,14,1,filename,filemask,"filename"},
    {0}
};

/*------- text find data entry template and buffer ------ */
static char findstr[71];
static char findmask[71];

FIELD find_template[] = {
    { 2,8,1,findstr,findmask,"findstr"},
    {0}
};

extern int forcechar;
extern struct edit_env ev;

static void editkeys(int c)
{
    switch(c)   {
        case REFORM:
            ev.reforming ^= TRUE;
            break;
        case NEW_FILE:
            new_file(1,1);
            break;
        case LOAD_FILE:
            load_file(1,1);
            break;
        case SAVE_FILE:
            save_file(1,1);
            break;
        case MERGE_FILE:
            merge_file(1,1);
            break;
        case FIND:
            if (!findstring())
                break;
        case FIND_AGAIN:
            ev.nowptr++;
            ev.nowptr = find(ev.nowptr, ev.lstptr-ev.nowptr);
            if (ev.nowptr != NULL)  {
                ev.curr_x = (ev.nowptr-ev.topptr) % ev.wwd;
                if (ev.nowptr >= ev.bfptr+ev.wwd*ev.wdo->ht)
                    ev.bfptr = ev.nowptr - ev.curr_x;
                ev.curr_y = (ev.nowptr - ev.bfptr) / ev.wwd;
            }
            else
                error_message("Not found ...");
            break;
        case ALT_F:
            editmenu(1);
            break;
        case ALT_E:
            editmenu(2);
            break;
        case ALT_O:
            editmenu(3);
            break;
        case EDITOR_MENU:
            editmenu(0);
            break;
        default:
            if (edit_extend)
                (*edit_extend)();
            else
                putch(BELL);
            break;
    }
}

static void editmenu(n)
{
    menu_select(emn, n);
}

static int edit(hs,vs)
{
    forcechar = emn[hs-1].mskeys[vs-1] & 255;
    return TRUE;
}

/*---------- edit a file -------------- */
void fileedit(char *file)
{
    char *bf, *mb;
    extern void (*editfunc)();
    extern void (*status_line)();

    setmem(filename, 64, ' ');
    setmem(filemask, 64, '_');
    setmem(findmask, 70, '_');
    setmem(findstr, 70, ' ');
    establish_window(1,2,80,24,TEXTFG,TEXTBG,TRUE);
    editfunc = editkeys;
    status_line = statusline;
    mb = display_menubar(emn);
    statusline();
    if ((bf = malloc(BUFLEN)) != NULL)  {
        setmem(bf, BUFLEN, ' ');
        strcpy(filename, file);
        filename[strlen(filename)] = ' ';
        if (*file)
            read_file(" Loading ... ",bf,0,FALSE,FALSE);
        while (TRUE)    {
            text_editor(bf, MAXLINES, EDITWIDTH);
            if (bufferok("quit"))
                break;
        }
        free(bf);
    }
    restore_menubar(mb);
    delete_window();
}

/*---------- load a file --------------- */
static int load_file(hs,vs)
{
    if (bufferok("reload"))
        strcpy(savefn, filename);
        if (get_filename(" Load what file? ", TRUE) != ESC) {
            setmem(ev.topptr, BUFLEN, ' ');
            read_file(" Loading ... ",ev.topptr,0,FALSE,TRUE);
            forcechar = BEGIN_BUFFER;
            ev.text_changed = FALSE;
        }
        else
            strcpy(filename, savefn);
    return TRUE;
}

/*---------- merge a file into the edit buffer -------- */
static int merge_file(hs,vs)
{
    strcpy(savefn, filename);
    if (get_filename(" Merge what file? ", TRUE) != ESC)    {
        if (read_file(" Merging ... ",
                curr(0, ev.curr_y),
                lineno(ev.curr_y), TRUE, TRUE)) {
            forcechar = REPAINT;
            ev.text_changed = TRUE;
        }
    }
    strcpy(filename, savefn);
    return TRUE;
}

/*--------- save the file -------------- */
static int save_file(hs,vs)
{
    if (get_filename(" Save as what file? ", FALSE) != ESC)
        if (write_file())
            ev.text_changed = FALSE;
    return TRUE;
}

/*---------- start a new file ------------- */
static int new_file(hs,vs)
{
    if (bufferok("erase"))
        if (get_filename(" Build as what file? ",TRUE)!=ESC){
            setmem(ev.topptr, BUFLEN, ' ');
            forcechar = BEGIN_BUFFER;
            ev.text_changed = FALSE;
        }
    return TRUE;
}

/*-------- read a file name ------------- */
static int get_filename(char *ttl, int clear)
{
    int rtn;

    establish_window(1,23,80,25,ENTRYFG,ENTRYBG,TRUE);
    window_title(ttl);
    gotoxy(3,2);
    cputs("File name:");
    rtn = data_entry(fn_template, clear, 1);
    delete_window();
    return rtn;
}

/*--------- write a file ------------ */
static int write_file()
{
    FILE *fp;
    int ln, i, ln1;
    char *cp, buf[EDITWIDTH+1];

    if ((fp = fopen(filename, "w")) == NULL)    {
        error_message(" Can't write that file! ");
        return FALSE;
    }
    notice(" Writing file ... ");
    /*----- find the last significant line ----- */
    for (ln = MAXLINES-1; ln > -1; --ln)    {
        cp = ev.topptr + ln * EDITWIDTH;
        for (i = 0; i < EDITWIDTH; i++)
            if (*(cp + i) != ' ')
                break;
        if (i < EDITWIDTH)
            break;
    }
    for (ln1 = 0; ln1 <= ln; ln1++) {
        movmem(ev.topptr + ln1 * EDITWIDTH, buf, EDITWIDTH);
        i = EDITWIDTH-1;
        cp = buf;
        while (i >= 0 && *(cp + i) == ' ')
            --i;
        if (i == -1 || *(cp + i) != ' ')
            i++;
        *(cp + i) = '\n';
        *(cp + i + 1) = '\0';
        fputs(cp, fp);
    }
    fclose(fp);
    delete_window();
    return TRUE;
}

/*-------------- read (load or merge) a file ----------- */
static int
read_file(char *nt,char *ln,int lines,int merging,int needed)
{
    FILE *fp;
    char ibf[120];
    char *cp;
    int x;

    if ((fp = fopen(filename, "r")) != NULL)    {
        notice(nt);
        while (fgets(ibf, 120, fp) && lines < MAXLINES) {
            lines++;
            if (merging)    {
                movmem(ln,ln+EDITWIDTH,
                    BUFLEN-lines*EDITWIDTH);
                setmem(ln,EDITWIDTH,' ');
            }
            cp = ibf, x = 0;
            while (*cp && *cp != '\n')  {
                if (*cp == '\t')
                    x += TAB-(x%TAB);
                else
                    *(ln+x++) = *cp;
                cp++;
            }
            ln += EDITWIDTH;
        }
        fclose(fp);
        delete_window();
        return TRUE;
    }
    else if (needed)
        error_message("No such file can be found");
    return FALSE;
}

/*----------- display a status line ----------- */
static void statusline()
{
    char stat[81], *st;
    int cl[81], *cp;
    static char msk[] =
"Line:%3d   Column:%2d   %-9.9s   %-21.21s   F1:Help    \
F10:Menu  ";
    unsigned y = 1;
    unsigned x = 1;
    unsigned attr = ((MENUFG | (MENUBG << 4)) << 8);

    if (ev.wwd) {
        y = (unsigned) (ev.nowptr-ev.topptr) / ev.wwd + 1;
        x = (unsigned) (ev.nowptr-ev.topptr) % ev.wwd + 1;
    }
    sprintf(stat,msk,y,x,
        (ev.edinsert ? "Insert" : "Overwrite"),
        (ev.reforming ? "Auto Paragraph Reform" : " "));
    for (st = stat, cp = cl; *st; st++)
        *cp++ = (*st & 255) | attr;
    __vram(__vptr(1,25),cl,80);
    set_help("editor");
}

/*-------- get a string to find --------- */
static int findstring()
{
    char *cp = findstr+60;
    int ans;

    establish_window(1,23,80,25,ENTRYFG,ENTRYBG,TRUE);
    gotoxy(2,2);
    cputs("Find?");
    ans = data_entry(find_template, TRUE, 1);
    delete_window();
    if (ans == ESC)
        return FALSE;
    while (*--cp == ' ')
        ;
    if (*cp)
        *(cp+1) = '\0';
    return TRUE;
}

/*-------- find a string in the buffer -------------- */
static char *find(char *bf, unsigned len)
{
    char *cp;

    for (cp = bf; cp < bf+len-strlen(findstr); cp++)
        if (strncmp(cp, findstr, strlen(findstr)) == 0)
            return cp;
    return NULL;
}

/*---------- test for buffer changed ----------- */
static int bufferok(char *s)
{
    int c = 'Y';
    if (ev.text_changed)    {
        establish_window(23,11,56,13,ERRORFG,ERRORBG,TRUE);
        gotoxy(2,2);
        cprintf("Text has changed, %s? (y/n)", s);
        hidecursor();
        do
            putch(BELL), c = getkey();
        while (toupper(c) != 'Y' && toupper(c) != 'N');
        delete_window();
    }
    return toupper(c) == 'Y';
}

/*-------- small message ------------ */
static void notice(char *s)
{
    int lf = (80-strlen(s))/2-1;
    int rt = lf+strlen(s)+2;
    establish_window(lf,11,rt,13,HELPFG,HELPBG,TRUE);
    gotoxy(2,2);
    cputs(s);
}
```

## LISTING 5

```c
/*----------- twrp.c ------------ */
#include <conio.h>
#include "window.h"
#include "editor.h"
#include "help.h"

void main(int, char **);
void fileedit(char *);

void main(int argc, char **argv)
{
    extern int inserting, MAXLINES;

    MAXLINES = 800;
    load_help("twrp.hlp");
    clear_screen();
    fileedit(argc > 1 ? argv[1] : "");
    clear_screen();
    inserting = FALSE;
    insert_line();
}
```

## LISTING 6

```c
trwp (window.h, editor.h, help.h)
editshel (editor.h, menu.h, entry.h, help.h, window.h)
editor (editor.h, window.h)
entry (entry.h, window.h)
menu (menu.h, window.h)
help (help.h, window.h)
window (window.h)
```
