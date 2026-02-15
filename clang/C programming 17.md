
# C programiranja 17

## Pretraga teksta, C++ i OOPS, i ANSI stringovi

Al Stevens, decembar '89

Ovog meseca ćemo započeti novi projekat rubrike "C programiranje". Ispitaćemo neke od problema povezanih sa održavanjem velikih tekstualnih baza podataka. Možda ste upoznati sa problemom. Imate mnogo referentnog materijala i ne znate uvek kako da pronađete ono što tražite. Ponekad se referentni materijal čuva u tekstualnim datotekama. Ako držite mnogo datoteka za obradu teksta na poslu ili ako preuzmete mnogo softvera sa .DOC ili READ.ME datotekama, vaš materijal je uskladišten negde u ASCII tekstu i bio bi spreman za čitanje samo da ga možete pronaći. Naš novi projekat će se pozabaviti problemima povezanim sa lociranjem tekstualnih datoteka koje sadrže informacije koje su nam potrebne.

Jednog dana će svi imati skenere za optičko prepoznavanje znakova, a mi ćemo sve naše knjige i časopise čuvati na disku. Možda će časopisi početi da prodaju svoj urednički sadržaj i na disketama ili CD-ROM-u. Kada se to desi, onda će svi naši referentni radovi biti spremni za obradu od strane procesora dokumenata sličnog onom koji ćemo izgraditi ovde u ovoj koloni.

Postoji veliki broj softverskih paketa koji rade ono što ćemo napraviti ovde. Većina CD-ROM distribucija velikih tekstualnih baza podataka uključuje takav sistem. Mi ćemo izgraditi svoje. Na taj način svako od nas može da ga prilagodi svojim specifičnim zahtevima. Ovaj softver će biti generički standard C. Ako želite da ga integrišete u određenu korisničku platformu, trebalo bi da se portuje sa malim poteškoćama. Naš projekat će izgraditi sistem koji ćemo nazvati TEKSTSRCH.

Postoji niz disciplina kojima se moramo baviti u razvoju ovakvog projekta. Prvo, moramo se zapitati, s obzirom na veliku bazu tekstualnih podataka i nekoga ko će je pretraživati, kako se pretraga nastavlja? Koji su kriterijumi pretrage i kako ćemo ih izraziti softveru? Ovo su prvi zahtevi koje treba razmotriti, jer će oni odrediti koje tehnike koristimo za skladištenje i pronalaženje stvari. Zatim, kada identifikujemo datoteke teksta koje odgovaraju našim kriterijumima pretrage, kako nam ih sistem mora predstaviti?

## Baza podataka TEXTSRCH

Sistem za pronalaženje dokumenata kao što je TEKSTSRCH najbolje funkcioniše u okruženju gde su datoteke sa podacima statične. Ima puno teksta i ne menja se mnogo. Korisnici kontrolišu promene uz pomoć dodataka ili zamenskih dokumenata u zakazanim intervalima. Tipične primene su inženjerske specifikacije, priručnici za održavanje i referentne biblioteke.

Da biste napravili bazu podataka, prikupljate dokumente na kontrolisanoj lokaciji, možda u sopstvenom poddirektorijumu, a zatim pravite indeks. Indeks podržava pretrage. Neki sistemi zahtevaju da ručno pregledate svaki dokument i označite sve fraze koje će se pojaviti u indeksu. Drugi grade indeks automatski skenirajući tekst i izdvajajući reči. Koristićemo ovaj drugi pristup.

Ovu vrstu sistema želite da koristite na statičkoj bazi podataka jer operacija indeksiranja može potrajati dugo.

Sa bazom podataka na mestu i izgrađenim indeksom, spremni ste za preuzimanje. Hajde da prvo razgovaramo o tome kako će se takve pretrage odvijati. Neko će želeti da pronađe sve reference na reč ili frazu u bazi podataka. Možda ćete želeti svaku referencu na frazu, "programiranje orijentisano na objekte". Morate reći sistemu šta želite i on vam mora reći gde možete pronaći te reference. Ali pretpostavimo da vas zanimaju samo reference na OOP koje se posebno odnose na Smalltalk. Tada biste možda želeli da vaša pretraga isporuči samo one datoteke u kojima se pojavljuju obe ključne fraze. Druga pretraga bi možda želela da posebno isključi reference na C++, tako da morate biti u mogućnosti da obavestite sistem i o tom zahtevu.

## Upiti

Možete videti kuda idemo. Očigledno je na početku da nam je potreban neki oblik strukturiranog jezika upita sa Bulovim operatorima. Moramo biti u mogućnosti da raščlanimo izraz kao što je ovaj:

```sh
"objektno orijentisan" i "smalltalk", ali ne i "C++"
```

Naši izrazi će imati operatore sa prioritetom, tako da ćemo morati da koristimo zagrade da zamenimo podrazumevani prioritet koji je dodelio parser, čineći izraz poput ovog mogućim:

```sh
"object oriented" and ("design" or "programming")
```

## TEXTSRCH analizator izraza

Sve ono što je upravo prethodilo vodi nas do naše prve rate. Prvi skup funkcija koji ćemo razviti će raščlaniti takve izraze upita i pripremiti ih za prolaz u bazi podataka. [Listing 1](#listing-1) je "textsrch.h". Ovaj spisak je onaj koji će rasti tokom meseci. Kako dodajemo funkcije, dodaćemo makroe, prototipove i slično u "textsrch.h". Ovog meseca sadrži ono što nam je potrebno za analizu izraza.

[Listing 2](#listing-2) je "express.c", kod koji analizira naše izraze. Počinje sa modifikovanim BNF-om koji opisuje jezik upita. Kao što vidite, nije mnogo složeno. Program koji želi da raščlani izraz poziva funkciju "lexical_scan" u ovom modulu. Funkcija vraća NULL ako pronađe grešku. Ako je izraz tačan, funkcija vraća pokazivač na raščlanjeni izraz, koji je predstavljen u obliku tokena pogodnih za obradu od strane interpretatora izraza.

Funkcija "lexical_scan" poziva funkciju izraza da proveri sintaksu izraza i da ga pretvori u tokene. Provera sintakse koristi parser rekurzivnog spuštanja. To jest, ako funkcija izraza pronađe levu zagradu u toku, poziva sebe da potvrdi izraz koji je u zagradi, a zatim, kada se vrati, očekuje da pronađe desnu zagradu. Ako pronađe unarni NOT operator, poziva sam sebe da proveri izraz koji sledi iza operatora. Ako pronađe reč ili frazu – fraza u našem kontekstu se sastoji od grupe reči okruženih dvostrukim navodnicima – gleda na sledeći element u izrazu. Ako je taj element binarni AND ili OR operator, funkcija izraza poziva sebe da proveri izraz desno od operatora.

Dok proverava izraz, program ga pretvara u tokene. Svaki element jezika je token od jednog znaka, a tokeni su upisani u niz redosledom kojim se pojavljuju. Kada je token reč ili fraza, kojoj je dodeljen token OPERAND, pridruženi niz sadrži vrednost reči ili fraze.

Nakon što se utvrdi da je izraz tačan i da su tokeni izgrađeni, funkcija "lekical_scan" poziva funkciju postfiks. Njegova svrha je da konvertuje tok tokena iz infiksne notacije u postfiksnu notaciju. Izraz u infiksnom zapisu koristi definisani prioritet operatora sa zagradama da bi zamenio podrazumevane vrednosti. Ovo je notacija koju koristimo kada sastavljamo upit. To je takođe notacija koju koristi jezik C. Postfiksna notacija (koja se naziva i Reverzna poljska notacija) eliminiše zagrade i predstavlja prednost zbog blizine tokena jedan prema drugom. Korisnici naučnih kalkulatora SNOBOL, Forth i TI prepoznaće notaciju.

Infiksni izraz postavlja binarni operator između operanada kao što je prikazano ovde:

```sh
this and that
```

The postfix version of this expression pushes the operands on a stack followed by the operator as shown here:

```sh
and
that
this
```

(U ovim primerima koristio sam ugaone zagrade da odvojim operatore od operanada na steku.)

Infiksni izraz u zagradi kao što je ovaj:

```sh
(ovo i ono) ili šta
```

je predstavljen na postfiks steku na sledeći način:

```sh
or
what
and
that
this
```

Procena postfiks izraza je operacija steka. Gornji element je iskočio. Ako je element operand, koristimo operand da izvedemo vrednost izraza. Ako je element unarni operator, sledeći element se iskače i vrednuje, na njega se primenjuje unarni operator i rezultat se gura. Ako je element binarni operator, sledeća dva operatora se iskaču, binarni operator se primenjuje na njih, a rezultat se gura. Zatim se postupak ponavlja sve dok element ne iskoči kao jedan rezultat. Svaki pop je rekurzivno ponavljanje cele procedure. U našem slučaju gurnuti rezultati su tačne ili netačne vrednosti unutar dokumenta baze podataka u zavisnosti od rezultata pretrage. Svaki put kada ubacimo reč ili frazu, vidimo da li se nalazi u bazi podataka i, ako jeste, u kojim datotekama. Rezultat te pretrage je gurnut. Sledećeg meseca ćemo se detaljnije baviti procenom izraza.

Konverzija iz infiksne notacije u postfiksnu notaciju je jednostavna stvar. Postupak je detaljno opisan u Fundamentals of Data Structures, Horovitz i Sahni, 1976, Computer Science Press, na stranama 91 - 97. Ovu proceduru implementiramo u funkcijama postfik, isp, icp i poststack.

[Listing 3](#listing-3) je "testexpr.c", program za testiranje našeg analizatora izraza i prikazivanje rezultata. To je program za bacanje koji se ne koristi u projektu izvan svoje svrhe kao alat za testiranje i demonstraciju. Ukucate svoj izraz u funkciju gets. Koristite reči i fraze sa isprepletenim operatorima. Važeći operatori su AND, OR i NOT. Svrha upita je da izrazi obrazac reči i fraza koje se pojavljuju ili ne pojavljuju u bazi tekstualnih podataka. Program TEKSTSRCH će pronaći datoteke koje odgovaraju izrazu upita. Tipični izrazi su ovi:

```sh
(fortran or cobol) and not pascal
fortran or (cobol and not pascal)
```

Program testekpr će izvesti sintaksičku grešku tako što će prikazati simbol pokazivača ispod elementa izraza gde je pronašao grešku. Ako je izraz tačan, program će prikazati postfiks stek u ovom formatu:

Unesite izraz za pretragu: fortran ili (kobol a ne paskal)

```sh
Enter the search expression:
fortran or (cobol and not pascal)
```

Ovog meseca ćemo započeti novi projekat rubrike "C programiranje". Ispitaćemo neke od problema povezanih sa održavanjem velikih tekstualnih baza podataka. Možda ste upoznati sa problemom. Imate mnogo referentnog materijala i ne znate uvek kako da pronađete ono što tražite. Ponekad se referentni materijal čuva u tekstualnim datotekama. Ako držite mnogo datoteka za obradu teksta na poslu ili ako preuzmete mnogo softvera sa .DOC ili READ.ME datotekama, vaš materijal je uskladišten negde u ASCII tekstu i bio bi spreman za čitanje samo da ga možete pronaći. Naš novi projekat će se pozabaviti problemima povezanim sa lociranjem tekstualnih datoteka koje sadrže informacije koje su nam potrebne.

Jednog dana će svi imati skenere za optičko prepoznavanje znakova, a mi ćemo sve naše knjige i časopise čuvati na disku. Možda će časopisi početi da prodaju svoj urednički sadržaj i na disketama ili CD-ROM-u. Kada se to desi, onda će svi naši referentni radovi biti spremni za obradu od strane procesora dokumenata sličnog onom koji ćemo izgraditi ovde u ovoj koloni.

Postoji veliki broj softverskih paketa koji rade ono što ćemo napraviti ovde. Većina CD-ROM distribucija velikih tekstualnih baza podataka uključuje takav sistem. Mi ćemo izgraditi svoje. Na taj način svako od nas može da ga prilagodi svojim specifičnim zahtevima. Ovaj softver će biti generički standard C. Ako želite da ga integrišete u određenu korisničku platformu, trebalo bi da se portuje sa malim poteškoćama. Naš projekat će izgraditi sistem koji ćemo nazvati TEKSTSRCH.

Postoji niz disciplina kojima se moramo baviti u razvoju ovakvog projekta. Prvo, moramo se zapitati, s obzirom na veliku bazu tekstualnih podataka i nekoga ko će je pretraživati, kako se pretraga nastavlja? Koji su kriterijumi pretrage i kako ćemo ih izraziti softveru? Ovo su prvi zahtevi koje treba razmotriti, jer će oni odrediti koje tehnike koristimo za skladištenje i pronalaženje stvari. Zatim, kada identifikujemo datoteke teksta koje odgovaraju našim kriterijumima pretrage, kako nam ih sistem mora predstaviti?

## Book Report: Object-Oriented Program Design With Examples in C++

Da li tražite čarape za sebe ove sezone? Postoji nova knjiga Marka Mulinsa pod nazivom "Dizajniranje objekata orijentisanog programa sa primerima u C++". Knjiga je više o objektno orijentisanom programiranju i dizajnu nego o C++, ali koristi C++ za primere i nezaobilazan je dodatak vašoj biblioteci ako pokušavate da shvatite OOP i C++.

Mulin ima jedinstven pristup ovim temama. Umesto da predstavi samo još jedno poboljšanje AT&T C++ referentnih dokumenata i Stroustrupove knjige, on predaje osnove objektno orijentisanog dizajna i koristi C++ da demonstrira lekcije. Pratite ovu knjigu od početka do kraja i imaćete upravljivo razumevanje objektno orijentisanog dizajna i programiranja do trenutka kada završite.

Mullin-ova knjiga takođe uči vredne lekcije o opštem pristupu koji programer koristi u dizajnu softverskih sistema, objektno orijentisanih ili ne, i on to radi sa stilom pisanja koji informiše i često zabavlja čitaoca. Tu i tamo se ubaci u neku suvu šalu koja prođe gotovo neprimećeno.

Da bi vam pokazao kako da dizajnirate objektno orijentisani sistem, Mulin preduzima razvoj integrisanog sistema za upravljanje informacijama za izmišljenu kompaniju. Ovo je prvi opsežni tretman OOP-a koji sam video i koji koristi stvarne, uverljive primere stvari sa kojima se programer može susresti u stvarnom svetu. Većina pisaca gradi izmišljene analogije koje pokušavaju da povežu apstraktne primere tostera, voća ili drugih glupih neautomata sa misterioznim stvarima koje objektno orijentisana paradigma naziva objektima. Mulin koristi evidenciju baze podataka o inventaru, prodaji i osoblju da demonstrira razvoj hijerarhije klasa kako bi podržao svoj hipotetički dizajn. To su stvari sa kojima se svi možemo povezati.

Možda nikada niste radili na sistemu inventara, ali možete lako da vidite šta bi neko trebalo da uradi. Ako postoji skladište puno stvari i morate da locirate određene artikle, ispunite porudžbine za artikle i zamenite ono što prodajete, onda vam je potreban sistem zaliha. Radio sam na nekoliko. Najmanji je bio za prodavnicu video kaseta; najveći je bio za rezervne delove za spejs šatl. Njihovi odgovarajući funkcionalni zahtevi su bili slični. Ljudi koji upravljaju zalihama to zovu „planiranje materijalnih potreba“. Džordž Karlin to naziva "praćenjem vaših stvari". Mulin uzima ovu svetovnu aplikaciju, onu koju će svi prepoznati, i dizajnira objektno orijentisani softverski sistem koji će je podržavati.

Mulin prati dizajn na način na koji bi se zaista mogao pojaviti. Lažni startovi, prefinjenost zahteva, inkrementalni razvoj objektno orijentisanog modela podataka, on ih sve predstavlja onim redosledom u kom biste ih sreli tokom realnog projekta. Dok nastavlja, Mulin vam govori kako treba da razmišljate o objektima i njihovom mestu u hijerarhiji klasa.

Mullin dizajn ponekad teži da izgradi svoju bazu podataka koristeći poglede koji izgledaju – barem meni – neprirodni. Objektno orijentisani dizajn uključuje izgradnju osnovnih klasa i izvedenih klasa u hijerarhiji klasa. Pošto svaki entitet u njegovoj bazi podataka ima ime i adresu, Mulin počinje sa novom klasom pod nazivom "Entiti" koja sadrži članove imena i adrese. Onda sve ostalo izvodi iz te klase. Ovo nije prirodan pogled na stvari. Nismo nužno uvek podređeni našim zajedničkim imeniteljima. To što svi imamo adrese ne znači da smo podređeni tim adresama. Mislim da vidimo tendenciju objektno orijentisane paradigme da savije našu perspektivu u neprikladnim pravcima. Rešenje problema treba da liči na problem, a imena i adrese su komponente njihovih vlasnika, a ne obrnuto. Napravio bih klasu Entiti sa njenim imenom i članovima adrese, a zatim uključio instancu te klase u svaku od klasa koje to zahtevaju. Funkcionalni rezultat bi bio isti, ali ne bismo bili primorani da našu bazu podataka percipiramo iz nelogične perspektive samo da bismo se pridržavali hijerarhijske dogme. Ali onda, ne znam dovoljno o ​​objektno orijentisanom dizajnu da bih napisao knjigu. Bar ne još.

Moja jedina druga kritika ove knjige je da Mulin i njegovi urednici izgleda ne znaju da je reč "podaci" imenica u množini. Ali onda, mnoge moje kolege ovde u DDJ-u ni to ne znaju, a i ja ponekad zaboravim. Osim te kritike, ovo je veoma dobra knjiga. OOP i C++ su molili za ovako dobre literarne tretmane, i srećan sam što vidim da se to konačno dešava.

## ANSI ugao: Preprocesranje i stringovi

Specifikacija ANSI standarda C uvela je nešto što komitet naziva „stringovanje“, što nije prava reč, zapravo odvratna, ali ipak novi koncept u prethodnoj obradi. (Ako nam K&R može dati „inicijalizator“, takođe ni reč, pretpostavljam da je Ks3J11 na sličan način pretpostavio da mogu da prošire engleski dok su proširili C.)

Problem sa ANSI dokumentom je u tome što često ne pruža nikakvu osnovu za određeno proširenje klasičnog C. U previše slučajeva komitet prepušta nama da otkrijemo zašto su uključili određenu funkciju. Oni će opisati karakteristike i možda dati primere kako se mora ponašati usklađeni kompajler, ali ne daju na adekvatan način motiv koji stoji iza svega. Dokument sa obrazloženjem koji prati specifikaciju bi trebalo da popuni ove praznine u informacijama, ali prečesto to ne čini ili pokušava i ne uspe. Takav je slučaj sa # operatorom pretprocesora na #define zamenama makro parametara, takozvanom funkcijom „stringovanja“. Pokušali su da objasne, ali ne razumem objašnjenje. Ako već ne znate kako biste mogli da koristite funkciju #, možda nećete moći da pogodite njenu svrhu iz opisa u specifikaciji čak i kada znate šta ona radi. Ono što možemo da uradimo je da pogledamo šta radi i vidimo da li ovo rešenje rešava neki problem koji bismo mogli da imamo. Operator # pretvara parametar u string. To je sve što radi. Evo primera:

```c
#define str(x) # x
printf(str(HELLO));
```

Ovaj makro proširuje parametar hello u string tako da printf poziv izgleda ovako nakon zamene:

```c
printf("HELLO");
```

Preprocesor obavlja sve druge prevode parametara pre nego što izgradi string kao što je prikazano ovde:

```c
#define HELLO goodbye
#define str(x) # x
printf(str(HELLO));
```

Ovaj niz se pretvara u ovo:

```c
printf("goodbye");
```

Zašto bi želeo da uradiš bilo šta od toga? Ako znate da želite da HELLO bude "ZDRAVO" ili zbogom da bude "zbogom", zašto to jednostavno ne kodirate na taj način? Hajde da pokušamo da zamislimo okolnost u kojoj bi ova funkcija mogla imati koristi. Jedna mogućnost koja vam pada na pamet je u domenu otklanjanja grešaka. Možete koristiti operator # da kreirate TRACE makro koji prati izabrane izraze u vašem programu na konzoli. Možete uključiti i isključiti TRACE pomoću globalne promenljive vremena kompajliranja. Evo makroa:

```c
#ifdef DEBUGGING
    #define TRACE(x)(printf("\n"# x),(x))
#else
    #define TRACE(x) (x)
#endif
```

Pretpostavimo da vaš program ima izraz koji želite da pratite na konzoli. Ako je ovo izraz:

```c
b = strlen ("12345");
```

Da biste pratili ovaj izraz, možete ga prekodovati pomoću TRACE makroa na sledeći način:

```c
b = TRACE(strlen ("12345"));
```

Sada kad god se izraz izvršava sa definisanom globalnom promenljivom DEBUGGING, kod izraza se prikazuje na konzoli, kao i procenjuje se jer je izraz prethodno obrađen u ovoj sekvenci:

b = (printf("\ n" "strlen (\ "12345 \")"), (strlen("12345")));

Obratite pažnju da su dve komponente proširenja makroa, printf i strlen pozivi razdvojeni zarezima. Ovo garantuje da strlen, koji je krajnji desni izraz, vraća potrebnu vrednost kada se izraz procenjuje. Takođe primetite da su ova dva okružena zagradama. Da nisu, dobili bismo pogrešnu vrednost u celom broju "b", jer operator dodele ima veći prioritet od operatora zareza.

Sledećeg meseca ANSI kutak će istražiti ## operator lepljenja tokena. Hoćemo, odnosno ako uspem da smislim korisnu aplikaciju za lepljenje tokena.

## LISTING 1

```c
/* ----------- textsrch.h ---------- */

#define MXTOKS 25       /*  maximum number of tokens */

#define OK    0
#define ERROR !OK

struct postfix  {
    char pfix;      /* tokens in postfix notation */
    char *pfixop;   /* operand strings            */
};

extern struct postfix pftokens[];
extern int xp_offset;

/* --------- expression token values ---------- */
#define TERM     0
#define OPERAND 'O'
#define AND     '&'
#define OR      '|'
#define OPEN    '('
#define CLOSE   ')'
#define NOT     '!'
#define QUOTE   '"'

/* ---------- textsrch prototypes ---------- */
struct postfix *lexical_scan(char *expr);
```

## LISTING 2

```c
/* ---------- express.c ----------- */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "textsrch.h"

/*
 * Parse a search expression into a valid postfix token stream.
 * The input expression has this form:
 *   <expr>   := <ident>
 *               <ident> <op> <expr>
 *               NOT <expr>
 *               (<expr>)
 *   <op>     := AND
 *               OR
 *   <ident>  := <character>
 *               <character> <ident>
 *               "<phrase>"
 *   <phrase> := <ident>
 *               <ident> <space> <phrase>
 */

#define iswhite(c)      (c < 33 && c > 0)
#define isparen(c)      (c == OPEN || c == CLOSE)
#define isop(c)         (c == AND || c == OR)
#define ischaracter(c)  (!isparen(c) && \
                        !isop(c)     && \
                        c != TERM    && \
                        !iswhite(c)  && \
                        c != QUOTE)

/* ----------- prototypes --------------- */
static int getword(char *expr);
static int gettoken(char *expr);
static int expression(char *expr);
static void postfix(void);
static int isp(int tok);
static int icp(int tok);
static void poststack(void);

int xp_offset = 0;              /* offset into the expression */
static char word[50];           /* word from the expression   */

static char tokens[MXTOKS+1];   /* tokens in infix notation   */
static char *operands[MXTOKS];  /* operand strings            */
static int token_ptr = 0;

static char stack[MXTOKS];      /* stack for tokens           */
static char *stopr[MXTOKS];     /* operand strings            */
static int top = 0;

struct postfix pftokens[MXTOKS];
static int pf = 0;

/* ------ analyze the expression for valid syntax ----------*/
struct postfix *lexical_scan(char *expr)
{
    token_ptr = xp_offset = 0;
    if (expression(expr) == ERROR)
        return NULL;
    else if (gettoken(expr) != TERM)
        return NULL;
    postfix();
    return pftokens;
}

/* ---------- analyze an element of the expression ---------- */
static int expression(char *expr)
{
    int tok;

    tok = gettoken(expr);
    switch (tok)    {
        case OPEN:
            if (expression(expr) == ERROR)
                return ERROR;
            if ((tok = gettoken(expr)) != CLOSE)
                return ERROR;
            break;
        case NOT:
            if (expression(expr) == ERROR)
                return ERROR;
            break;
        case OPERAND:
            break;
        default:
            return ERROR;
    }
    tok = gettoken(expr);
    switch (tok)    {
        case TERM:
            return OK;
        case AND:
        case OR:
            return expression(expr);
        case CLOSE:
            --token_ptr;
            --xp_offset;
            return OK;
        default:
            break;
    }
    return ERROR;
}

/* ------- extract the next token from the expression ------- */
static int gettoken(char *expr)
{
    char tok;

    operands[token_ptr] = 0;
    if ((tok = getword(expr))== OPERAND)    {
        operands[token_ptr] = malloc(strlen(word) + 1);
        strcpy(operands[token_ptr], word);
    }
    tokens[token_ptr++] = tok;
    return tok;
}

/* ------- extract a word, operator, parenthesis,
           or terminator from the expression ------------- */
static int getword(char *expr)
{
    int w = 0;

    /* ------- bypass white space -------- */
    while (iswhite(expr[xp_offset]))
        xp_offset++;
    switch (expr[xp_offset])    {
        case OPEN:
        case CLOSE:
            return expr[xp_offset++];
        case TERM:
            return TERM;
        case QUOTE:
            while (expr[++xp_offset] != QUOTE)  {
                if (w == 50)
                    return ERROR;
                word[w++] = tolower(expr[xp_offset]);
            }
            xp_offset++;
            word[w] = '\0';
            return OPERAND;
        default:
            while (ischaracter(expr[xp_offset]))    {
                if (w == 50)
                    return ERROR;
                word[w++] = tolower(expr[xp_offset]);
                xp_offset++;
            }
            word[w] = '\0';
            if (strcmp(word, "and") == 0)
                return AND;
            else if (strcmp(word, "or") == 0)
                return OR;
            else if (strcmp(word, "not") == 0)
                return NOT;
            return OPERAND;
    }
}

/* - convert the expression from infix to postfix notation - */
static void postfix(void)
{
    char tok = '*';

    top = token_ptr = pf = 0;
    stack[top] = '*';
    while (tok != TERM) {
        switch (tok = tokens[token_ptr])    {
            case OPERAND:
                pftokens[pf].pfix = tok;
                pftokens[pf].pfixop = operands[token_ptr];
                pf++;
                break;
            case NOT:
            case OPEN:
            case AND:
            case OR:
                while (isp(stack[top]) >= icp(tok))
                    poststack();
                stack[++top] = tok;
                break;
            case CLOSE:
                while (stack[top] != OPEN)
                    poststack();
                --top;
                break;
            case TERM:
                while (top)
                    poststack();
                pftokens[pf++].pfix = tok;
                break;
        }
        token_ptr++;
    }
}

static int isp(int tok)
{
    return ((tok == OPEN) ?  0 :
            (tok == '*')  ? -1 :
            (tok == NOT)  ?  2 :
                             1  );
}

static int icp(int tok)
{
    return ((tok == OPEN) ?  4 :
            (tok == NOT)  ?  2 :
                             1  );
}

/* --- remove a token from the stack, put it into postfix --- */
static void poststack(void)
{
    pftokens[pf].pfix = stack[top];
    pftokens[pf].pfixop = stopr[top];
    --top;
    pf++;
}
```

## LISTING 3

```c
/* ----------- testexpr.c ------------- */

/*
 * A program to test the TEXTSRCH expression analyzer
 */

#include <stdio.h>
#include <process.h>
#include <string.h>
#include "textsrch.h"

static void disp_token(struct postfix *pf);

void main(void)
{
    char expr[80];

    do  {
        /* ----- read the expression from the console ------ */
        printf("\nEnter the search expression:\n");
        gets(expr);
        if (*expr)  {
            /* --- scan for errors and convert to postfix --- */
            if (lexical_scan(expr) == NULL) {
                while(xp_offset--)
                    putchar(' ');
                putchar('^');
                printf("\nSyntax Error");
                exit(1);
            }

            /* ------ display the postfix tokens ------ */
            printf("\nToken");
            printf("\n--------------");
            disp_token(pftokens);
            printf("\n--------------");
        }
    } while (*expr);
}

static void disp_token(struct postfix *pf)
{
    if (pf->pfix != TERM)   {
        disp_token(pf+1);
        printf("\n %s", pf->pfix == AND  ? "<and>" :
                        pf->pfix == OR   ? "<or>"  :
                        pf->pfix == NOT  ? "<not>" :
                        pf->pfixop);
    }
}
```
