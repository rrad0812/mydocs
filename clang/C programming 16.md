
# C programiranje 16

## C++ i povezane liste

Al Stevens, novembar '89

Ovog meseca se upuštam dalje u C++ sa novom klasom, LinkedList. Programeri već godinama koriste strukturu podataka povezane liste. Omogućava vam da sačuvate nepoznat broj stavki podataka u tabeli gde unosi ne moraju da budu susedni u memoriji. Takođe rešava probleme koji se javljaju kod standardnih nizova kada unosi imaju različite veličine. Evo kako funkcioniše povezana lista.

Struktura podataka povezane liste povezuje listu stavki podataka zajedno sa skupom pokazivača. Sama lista ima pokazivač na prvi i poslednji unos u listi. Ovi pokazivači se nazivaju "glava liste". Povezana lista se sastoji od glave liste i unosa podataka. Svaki unos podataka na listi ima pokazivač na onaj koji je neposredno pre njega i pokazivač na onaj koji je neposredno iza njega. Prvi i poslednji pokazivač u glavi liste su u početku NULL. Oni se inicijalizuju sa vrednostima koje nisu NULL kada se prva stavka doda na listu. Prva stavka na listi ima NULL pokazivač prethodne stavke, a poslednja stavka na listi ima NULL pokazivač sledeće stavke.

Da biste prešli preko povezane liste u nizu unapred, počinjete sa vrednošću u pokazivaču prve stavke iz glave liste. Ako je taj pokazivač NULL, lista je prazna. U suprotnom, ukazuje na prvu stavku podataka. Ta stavka ima pokazivač na sledeću stavku podataka. Krećete se napred koristeći pokazivač sledeće stavke u svakoj sledećoj stavci sve dok ne pronađete stavku sa NULL pokazivačem sledeće stavke, što znači da ste na kraju liste. Ruta obrnutog niza kroz listu funkcioniše na isti način, ali počinje sa pokazivačem poslednje stavke u glavi liste i koristi pokazivač prethodne stavke u stavkama.

Da biste dodali stavku na kraj liste, postavite pokazivač prethodne stavke na mesto gde pokazuje pokazivač poslednje stavke, a pokazivač poslednje stavke postavite na novu stavku. Takođe morate da postavite pokazivač sledeće stavke u ono što je bila poslednja stavka, sa svojim pokazivačem sledeće stavke, na novu stavku.

Brisanje unosa sa povezane liste je pitanje prekida i popravljanja poslednjeg lanca. Prethodna stavka obrisane stavke sada mora da ukazuje na sledeću stavku obrisane stavke i obrnuto. Naravno, morate da testirate da li je izbrisana stavka prva ili poslednja (ili oboje) i da popravite prvi i poslednji pokazivač u glavi liste u skladu sa tim.

To je ono što je povezana lista. Svi rade skoro isto. Ono što se razlikuju je u formatu stavki podataka kojima upravljaju. Kao takva, povezana lista je dobar kandidat da bude generička C++ klasa, i siguran sam da to nije nova ideja.

[Listing 1](#listing-1) i [Listing 2](#listing-2) su "linklist.h" i "linklist.cpp", izvorne datoteke koje implementiraju klasu LinkedList.

Da biste koristili klasu LinkedList, u svoj program uključite "linklist.h". Okruženje strukture podataka svojstvima povezane liste je jednostavno pitanje deklarisanja LinkedList objekta i dodavanja tih struktura kao unosa na listu. Sama struktura podataka može biti jednostavna kao karakter i složena kao nova klasa. Kada prosledite njenu adresu i veličinu funkciji (metodu) člana povezane liste koja dodaje unose, vi kontrolišete podatke u povezanoj listi. Evo kako deklarišete listu:

```cpp
LinkList mylist;
```

To je sve. Sve dok klasa ne izađe iz opsega, lista, isprva prazna, postoji. Evo kako dodajete unose podataka na listu:

```cpp
mylist.addentry(&data, sizeof(data));
```

Ako proglasite listu, a zatim počnete da dodajete unose, oni će se redom dodavati na kraju liste. Svaki novi unos prati onaj koji je neposredno pre njega, i nalazi se na kraju dok ne dođe drugi.

Ako pređete preko liste, a zatim dodate unos dok je pozicioniran u sredini liste, unos će biti dodat odmah iza onog koji ste poslednji preuzeli. Ako se nalazite na početku liste, unos će biti dodat kao prvi.

Da biste preuzeli unos sa povezane liste, vijugate kroz listu unapred ili unazad i pogledate vrednosti podataka unutar svakog unosa. Svaka funkcija člana koja prolazi kroz listu vraća pokazivač na unos liste koji je pronašao. Evo metoda za prelazak liste:

```cpp
void *getfirst(void);
void *getnext(void);
void *getprev(void);
void *getlast(void);
void *getcurr(void);
```

Jednog od njih biste nazvali na ovaj način:

```cpp
char *cp = mylist.getfirst();
```

Svaka od ovih funkcija članica vraća pokazivač na vrednost podataka pronađenog unosa. Ove vraćene vrednosti možete dodeliti pokazivaču na sve što imate na listi. Ako je lista prazna ili skeniranje liste dođe do kraja ili početka, funkcija vraća NULL pokazivač.

Da biste izbrisali unos, idite do njega i pozivate funkciju člana koja briše trenutni unos. Evo poziva za tu operaciju:

```cpp
mylist.delete_entry();
```

[Listing 3](#listing-3) je "demolist.cpp", C++ program koji koristi klasu LinkedList za prikupljanje i upravljanje listom imena. Program koristi string klasu od prošlog meseca i prva stvar koju sam shvatio je da je string klasi potrebna funkcija člana koja vraća veličinu stringa. Sada je ovo vreme kada bi strogo kontrolisan objektno orijentisan projekat verovatno izgradio izvedenu klasu samo da bi dodao tu funkciju. Ali, pošto nismo tako rigorozni i uopšte nismo kontrolisani, samo ćemo dodati funkciju našoj originalnoj string klasi. Pogledajte "strings.h" od prošlog meseca i umetnite ove redove sa funkcijama člana:

```cpp
// -------- return the length of a string
int length(void) { return strlen(sptr)+1; }
```

Program demolist traži od vas da unesete neka imena u listu. Dodaje svako ime koje unesete na LinkedList. Kada završite, otkucate „kraj“ i program će prikazati meni. Možete prikazati imena, umetnuti novo ime na trenutnoj lokaciji, kretati se napred i nazad kroz listu i izbrisati trenutno ime.

## C++ kompajleri

C++ kod za poslednjih nekoliko meseci radi sa Zortech C++. Preveo sam i pokrenuo string kod od prošlog meseca sa još dva kompajlera za računare, Intex C++ i Guidelines C++. Ovi prevodioci su portovi AT&T C++ 1.2 izdanja i, kao takvi, zaista su prevodioci jezika. Potreban vam je kompajler kao i za kompajliranje C koda koji C++ prevodilac gradi iz vašeg C++ izvornog koda.

Intex C++ ima verzije koje rade sa Watcom C, Turbo C, Microsoft C i MetaWare High C. Intex kompajler radi samo na 386 mašini sa proširenom memorijom. Morao sam da onemogućim svoj Expanded Memory Manager da bih koristio Intex C++, i našao sam jednu ozbiljnu grešku: Program drajvera kompajlera radi samo svaki drugi put kada ga pokrenete. Potrebna mu je kontrolna datoteka koju gradi ako je ne pronađe. Nakon što je kompajliranje završeno, ako se kompajliranje završi, proces kompajlera briše datoteku. Problem je u tome što ako datoteka nije tamo, program je pravi, ali prekida. Ako je datoteka tamo, program je koristi i briše. Dakle, program radi samo svaki drugi put. Ljudi iz tehničke podrške u Intexu nisu bili svesni ovog problema i činilo se da su mislili da nešto nije u redu sa mojim podešavanjem. Tek nakon što sam im rekao tačan redosled koraka za reprodukciju problema, oni su priznali i obećali da će ga popraviti. Zaobišao sam to sa batch fajlom koji čuva i vraća kritičnu kontrolnu datoteku. Zbog hardverskih zahteva i cene (495 dolara plus cena vašeg kompajlera), ne vidim mnogo budućnosti za ovaj proizvod u svetu računara.

Guideline C++ je jeftiniji (295 dolara) od Intexa i ne zahteva 386 ili proširenu memoriju. Međutim, morate dodati cenu kompajlera. C++ sa Guideline je dostupan samo za rad sa Microsoft C-om, mada mi kažu da će Turbo C verzija biti uključena kada izdaju svoj port za AT&T C++ 2.0 izdanje, što je trebalo da se desi do trenutka kada ovo pročitate. Nisam našao nikakve probleme sa Guideline C++ osim onih koje nameće sam C++ 1.2. AT&T prevodilac nije svestan ANSI C tretmana pokazivača praznine u odnosu na stvarne pokazivače. ANSI C dozvoljava funkciji da ima prototip koji specificira pokazivač praznine. Pozivajuća funkcija može proslediti bilo koju vrstu pokazivača, a pozvana funkcija se bavi njom kao da je pokazivač karaktera. Slično, funkcija koja je prototipizovana da vrati pokazivač void može biti pozvana da dodeli svoju vrednost bilo kojoj vrsti pokazivača podataka. Ova konvencija vam omogućava da koristite funkcije kao što su `malloc` i `memcpy` bez potrebe za prebacivanjem. Takođe omogućava da se NULL globalno `#definie` kao void pokazivač sa vrednošću null. AT&T C++ 1.2 prevodilac se zamera takvim radnjama, tako da kod u ovomesečnoj koloni ne funkcioniše ni sa Intexom ni sa Guideline C. Začepio sam da ubacim sve te pretvorbe, budući da sam bio razmažen upotrebom kompajlera usklađenih sa ANSI u poslednje vreme, pa sam odlučio da se držim Zortech C++ dok ostali ne izađu.

Zortech C++ kompajler je prava cena u poređenju sa ostalima. Ima cenu od 150 dolara i nije vam potreban drugi kompajler da biste ga koristili. Napravljen je da radi sa ANSI konvencijama novog stila. (Kompajler C dolazi sa paketom.) Njegov nedostatak je što nije čist port AT&T koda; on ne deli iste greške i karakteristike sa ostatkom C++ sveta, tako da vaš kod neće biti toliko prenosiv koliko biste želeli da bude.

## ANSI kutak: protesti, trigrafi, izlazne sekvence i stringovi

Da li se pitate zašto je potrebno toliko vremena da se odobri jezički standard? ANSI X3J11 komitet se približava šest godina definisanja standarda za jezik C. Možda će ova priča pomoći da se objasni zašto.

Pre mnogo godina radio sam za malu konsultantsku firmu. Mi smo licitirali na vladinom ugovoru koji je diskvalifikovao kao ponuđače sve proizvođače računarske opreme. Vlada je posao dodelila velikoj firmi, a moj šef je napisao protestno pismo rekavši da je pobednik trebalo da bude diskvalifikovan zato što su proizvodili teletajp mašine, zatim obične terminalne uređaje za konzolu. Takav protest automatski izaziva birokratsku proceduru reagovanja i obično odlaže rad dok se stvar ne zatvori. Svaki put kada bismo dobili zvaničan odgovor, jednostavno smo napisali još jedno pismo, držeći točkove istrage da se okreću dok su točkovi napretka zaglavljeni. Pitao sam ga zašto se muči. Rekao je da su ga pisma koštala samo vremena, dok su nagrade bile u saznanju da je bacio ključ u radove. I, kako je rekao, "...samo zato što su me odbacili."

ANSI X3J11 komitet je na sličan način iznervirao C programera, gospodina Rasela Hansberija, i on je zauzvrat odgovorio apelom koji osporava validnost X3J11-ovog rada. Poput neprijatnih pisama mog nekadašnjeg hiroviteg šefa, ova žalba je dodatno odložila odobrenje predloženog C standarda.

Prvi problem je bio u tome što je komisija previdela Hansberrijeve originalne komentare, verovatno izgubljena u mešanju. Komisija, pošto je zagubila pismo, nije uspela da pripremi traženi formalni odgovor pre nego što je nacrt standarda dostavila X3, tako da je autor pisma mogao i jeste zahtevao da komitet popravi i odgovori na njegovu zabrinutost.

X3J11 je razmotrio svaku od tačaka u pismu i glasao da ih sve ne odobri. Bilo je mnogo buke i buke, ali je neodobravanje prevladalo demokratskom akcijom. Većina ljudi koji prihvataju koncept samoupravnog slobodnog društva prihvatila bi vladavinu većine i povukla bi se. Hansberi, očigledno odlučivši da demokratija ne ide u njegovu korist, odlučio je da napadne na drugim frontovima. Pravda nikada nije brza tamo gde su ljudi slobodni. Hansberi je iskoristio svoja prava i uložio žalbu koja je pokrenula 40 tehničkih i proceduralnih pitanja u očiglednom pokušaju da poništi sav rad odbora. X3 je glasao u avgustu na ponovnom glasanju, i nije dat nijedan negativan glas, tako da su tehnički problemi Hansberijeve žalbe efektivno poraženi. Međutim, proceduralna pitanja i dalje ostaju i moraju se rešiti pre nego što odobrenje bude konačno. Ako X3 odbije ova pitanja, Hansberri se i dalje može obratiti ANSI-ju. Neke od njegovih briga su opravdane, ali druge su usmerene na promenu jezika na način koji bi ugrozio postojeći kod. U međuvremenu, mi ostali čekamo odobreni standard za jezik C.

Možda sledeće godine....

## ANSI Dodaci

Iako je prvobitna povelja za standardnu specifikaciju ANSI C bila da dokumentuje standard za postojeću praksu jezika C, predloženi nacrt dodaje neke karakteristike jeziku C. Najznačajnije od njih su definicije i deklaracije funkcija novog stila, nazvane „prototipovi“, koje su usvojene iz C++. Druge nove funkcije su takođe korisne, ali bi mogle ostati neprimećene osim ako ne odvojite vreme da ih naučite. Među ovim dodacima su trigrafi, heksadecimalne izlazne sekvence, nekoliko novih karakternih konstanti i susedni literali stringova.

## Trigrafi

Trigrafi nisu toliko korisni kao drugi dodaci, ali ćete morati da znate o njima jer ćete se jednog dana možda spotaknuti o njih. Trigraf je način da se izraze oni znakovi koje C u velikoj meri koristi, ali koji ne postoje u nekim skupovima znakova koji nisu ASCII, pre svega skupu koji je definisala Međunarodna organizacija za standarde kao ISO 646. Postoji devet takvih znakova koje ISO 646 ne uključuje. To su #, [, ], {, }, \, |, ~ i ^. Pokušajte da napišete C program bez njih. Da bi omogućio korisnicima ISO 646 da programiraju u C-u, ANSI je uveo upotrebu trigrafa kao načina za izražavanje ovih znakova. Trigraf su dva znaka pitanja iza kojih sledi znak koji postoji u ISO 646 i koji podseća na znak koji nedostaje. Ovo su trigrafi za znakove koji nedostaju:

Trigraph           Replaces

```cpp
??=               #
??(               [
??/               \
??)               ]
??'               ^
??<               {
??!               |
??>               }
??-               ~
```

Napišite program sa ovim trigrafima i verovatno bi pobedio u jednom od onih glupih zamagljenih C takmičenja (ili prelepom APL takmičenju). Bez obzira na to, korisnicima tih drugih skupova znakova ne bi trebalo uskratiti jezik, pa je bio potreban određeni smeštaj. Trigrafi su neelegantno, ali izvodljivo rešenje. Većina nas nikada neće morati da zna za trigrafe osim da se bavi retkim prilikama kada želimo da stavimo "??" u string dok prevodilac želi da ga pretvori u trigraf. (Koristite "? \?".)

## Escape sekvence

U C leksikonu, izlazna sekvenca je vrednost u konstantnom ili string literalu koja počinje znakom obrnute kose crte i koja se prevodi u vrednost karaktera. Već znate za oktalne izlazne sekvence. Oni su u tradicionalnom K&R C od početka. Možete kodirati karakternu konstantu sa oktalnom vrednošću ovako:

```cpp
char c = '\ 101';
```

Znak obrnute kose crte iza kojeg sledi oktalna cifra (O do 7) govori kompajleru da cifre koje slede obrnutu kosu crtu čine vrednost znaka iz oktalnog izraza. Sa kompajlerima koji su usklađeni sa ANSI, sada takođe možete izraziti konstante znakova koristeći heksadecimalne cifre kao što je prikazano ovde:

```cpp
char c = '\ x 41';
```

Povratna kosa crta-x escape sekvenca govori kompajleru da će cifre koje slede biti heksadecimalna karakterna konstanta. Oba primera koja su upravo data formiraju vrednost karaktera za ASCII 'A' i bolje bi se izrazili na ovaj način:

```cpp
char c = 'A';
```

Sa oktalnom izlaznom sekvencom, dozvoljeno vam je od jedne do tri oktalne cifre (O do 7) da formirate vrednost. Ovo vam daje teoretsku maksimalnu vrednost '\777' ili 511. ANSI navodi, međutim, da konstanta karaktera ne može da pređe opseg nepotpisanog znaka, koji je na računaru širok 8 bita ili maksimalno '\377' ili 255. Mašine koje imaju široke znakove mogu imati karakterne konstante koje se protežu na to ograničenje, ali maksimalna konstanta od tri cifre i dalje je ograničena za aplikaciju oc. je 511 bez obzira na širinu širokog karaktera.

Dok oktalna izlazna sekvenca ne može imati više od tri cifre, heksadecimalna karakterna konstanta može imati bilo koji broj heksadecimalnih cifara iza izlazne sekvence obrnute kose crte-x. I on je ograničen na maksimalnu vrednost nepotpisanog znaka, tako da je njegovo ograničenje na računaru sa 8-bitnim karakterom '\ xf f' ili šire ako se primenjuju široki znakovi. Drugi skupovi znakova mogu imati mnogo šire veličine znakova, tako da ANSI ne definiše teoretsko ograničenje za heksadecimalnu karakternu konstantu, čime se dozvoljava izražavanje svih vrednosti znakovnih konstanti u onim kulturama (Japan, na primer) gde u skupu znakova ima daleko više od 256 znakova.

Nekoliko uobičajenih vrednosti znakova ne može se izraziti jednim alfabetskim, numeričkim ili grafičkim znakovima. Znakovi Escape i Carriage Return su dva primera. Možete izraziti ove znakove sa oktalnim ili heksadecimalnim konstantama znakova, ali takvi izrazi ne bi bili prenosivi na mašine sa nekompatibilnim skupovima znakova. ANSI obezbeđuje izlazne sekvence za neke, ali ne sve znakove koji se ne mogu prikazati. Ovde prikazane vrednosti su u standardu za neke od karakternih konstanti koje imaju, u ANSI prikazu, univerzalnu primenu:

```cpp
\a audible alarm
\b backspace
\f form feed
\n newline
\r carriage return
\t horizontal tab
\v vertical tab
```

'\ a' i '\ v' su dodati od X3J11.

Oni nisu bili deo K&R definicije, ali ih je komisija dodala standardu jer imaju univerzalnu primenu. Većina konzolnih uređaja ima znak zvona (BEL u ASCII) koji oglašava zvučni alarm, a mnogi štampači će reagovati na znak vertikalnog taba. ANSI je odlučio da ne uključi '\ e' za Escape karakter jer neki skupovi znakova - EBCDIC, na primer - nemaju ekvivalentan karakter.

Neki ASCII znakovi koji se mogu prikazati imaju značenje u kontekstu karakterne konstante ili string literala. Da bi vam omogućio da predstavite ove znakove, ANSI obezbeđuje sledeće izlazne sekvence da bi obezbedio prenosive izraze njihovih vrednosti:

```cpp
\    single quote
\"   double quote
\?   question mark
\\   backslash
```

Jednostruki navodnik treba obrnutu kosu crtu u karakternoj konstanti, ali će funkcionisati na bilo koji način u literalu stringa. Suprotno tome, dvostrukom navodniku je potrebna obrnuta kosa crta u literalu stringa, ali radi u oba smera u karakternoj konstanti.

"\?" je specificirano u ANSI nacrtu da bi se prilagodio njegov izraz u skupovima znakova koji zahtevaju trigrafove o kojima smo ranije govorili. Da biste dobili dvostruki znak pitanja u nizu, kodirate "?\?".

Iako nacrt nije specifičan u ovom trenutku, ako iza obrnute kose crte sledi bilo šta drugo osim oktalne cifre ili jednog od znakova ?, '<", a, b, f, n, r, t, v, k, ' ili \, usamljena obrnuta kosa crta se zanemaruje (osim ako ništa ne sledi na liniji, u kom slučaju se pretpostavi da je prvi red koji se nastavlja na prvi red na At). konvenciju koju prati većina takozvanih kompajlera usklađenih sa ANSI.

## Strings

Takođe možete staviti oktalne i heksadecimalne izlazne sekvence u literale niza znakova, čineći ove upotrebe mogućim, od kojih sve isporučuju istu vrednost stringa.

```cpp
char *cp = "\ 101phids";
char *cp = "\ x41phids";
char *cp = "Aphids";
```

Oktalna izlazna sekvenca u stringu se nastavlja sve dok kompajler ne pronađe ne-oktalnu cifru ili ne pogodi treću oktalnu cifru. Heksadecimalna izlazna sekvenca se nastavlja sve dok prevodilac pronađe heksadecimalne cifre. Kompajlatori bi trebalo da vas upozore ako formirane vrednosti znakova prelaze gornju granicu znaka na računaru.

Očigledno, treći format je ono što biste koristili za vrednost datu u gornjem primeru, ali postoje prilike u kojima ćete želeti vrednosti stringa koje sadrže znakove koji nisu predstavljeni znakovima koji se mogu prikazati. Kontrolni nizovi koji komanduju ANSI video terminalu koriste Escape sekvencu da bi terminalu dali do znanja da komanda dolazi. Da biste obrisali ekran na ANSI terminalu (ili računaru sa instaliranim ANSI.SIS), možete koristiti ovu izjavu:

```cpp
printf(" \ 33[2J");
```

"\ 33" je oktalna izlazna sekvenca koja formira vrednost jednog znaka za Escape karakter. Pretpostavimo da je vaš komandni niz trebao da ima Escape znak praćen "345". Ovaj niz ne bi funkcionisao:

```cpp
printf(" \ 333 45");
```

Kompajler bi tretirao treću '3' kao deo oktalne konstante karaktera i napravio bi string koji bi počeo sa znakom '\ 333' nakon čega bi sledio "45". Pošto su oktalne konstante znakova ograničene na tri cifre, string možete kodirati na sledeći način:

```cpp
printf(" \ 0 333 45");
```

Vodeća nula radi trik tako što ubacuje \ 033 u maksimalne tri cifre. (Korisnici Turbo C 2.0 treba da budu svesni da greška u kompajleru čini da misli da su sve cifre u stringu deo oktalne konstante.)

Evo izjave sa jasnog ekrana sa heksadecimalnom izlaznom sekvencom:

```cpp
printf(" \ x1b [2J");
```

" \ k1b" je heksadecimalna izlazna sekvenca za Escape karakter. Pretpostavimo sada da želite da se gore koristi string Escape, "345". Ovo ne bi funkcionisalo:

```cpp
printf(" \ x 1b 3 45");
```

Prevodilac bi pretpostavio da su sve cifre koje slede iza "\x" deo karakterne konstante jer heksadecimalne konstante znakova mogu biti šire od dve cifre. Kompajler za mašinu sa 8-bitnim karakterima treba da izda upozorenje za ovaj primer, jer vrednost prelazi opseg nepotpisanog karaktera. Međutim, ne bi trebalo pretpostaviti da ste mislili da bude Escape, „345“ samo zato što imate 8-bitne karaktere. Takva konvencija bi promovisala razvoj neprenosivog koda. Pa, kako to naterati da radi? Možete to napisati ovako:

```cpp
printf(" % d 3 4 5", '\ x1b');
```

To bi funkcionisalo, ali samo tamo gde koristite formatiran izlaz u stilu printf. Pretpostavimo da želite da napišete string sa funkcijom puts umesto printf. U tu svrhu ANSI je uveo susedni string literal.

## Literali susednih stringova

Ako kodirate dva stringova literala jedan pored drugog, oni se spajaju kao jedan string sa nultom završetkom. Evo poznatog primera:

```cpp
printf("Hello," " world");
```

Ova karakteristika ima nekoliko prednosti. Literale dugih stringova možete izraziti čitljivije tako što ćete ih razbiti na nekoliko susednih nizova i svaki deo staviti na svoju liniju. Ali, što je još bolje, problem heksadecimalne sekvence izlaza karaktera je rešen. Sada možemo da uradimo ovo:

```cpp
printf(" \ x1b" "345");
```

I, konačno, opisniji način kodiranja stringa je ovaj:

```cpp
#define ESCAPE " \ x1b"

printf(ESCAPE "345");
```

## Niko ne govori S reč

Još jedna stvar koju je X3J11 dodao našoj kulturi bila je reč „stringing“, koju su skovali da znači nešto novo za C preprocesor. To je užasna reč, ali korisna karakteristika. Koristiću ovu funkciju, ali više nikada neću koristiti reč osim da vam možda kažem koliko je užasna. Sledećeg meseca ću objasniti tu funkciju strašnom S rečju.

## LISTING 1

```cpp
// -------- linklist.h

#ifndef LINKLIST
#define LINKLIST

#include <stdio.h>

class LinkedList {
   typedef struct list_entry   {
      struct list_entry *NextEntry;
      struct list_entry *PrevEntry;
      void *entrydata;
   } ListEntry;
   ListEntry *FirstEntry;
   ListEntry *LastEntry;
   ListEntry *CurrEntry;
public:
   // ---- constructor
   LinkedList(void)
      { FirstEntry = LastEntry = CurrEntry = NULL; }
   // ---- destructor
   ~LinkedList(void);
   // ---- add an entry
   void addentry(void *newentry, int size);
   // ---- delete the current entry
   void delete_entry(void);
   // ---- get the first entry in the list
   void *getfirst(void);
   // ---- get the next entry in the list
   void *getnext(void);
   // ---- get the previous entry in the list
   void *getprev(void);
   // ---- get the last entry in the list
   void *getlast(void);
   // ---- get the current entry in the list
   void *getcurr(void)
      {return CurrEntry==NULL ? NULL : CurrEntry->entrydata;}
};

#endif
```

## LISTING 2

```cpp
// -------------- linklist.cpp

#include <string.h>
#include "linklist.h"

// ------- linked list destructor
LinkedList::~LinkedList(void)
{
   ListEntry *thisentry = FirstEntry;

   while (thisentry != NULL)   {
      delete thisentry->entrydata;
      ListEntry *hold = thisentry;
      thisentry = thisentry->NextEntry;
      delete hold;
   }
}

// --------- add an entry to the list
void LinkedList::addentry(void *newentry, int size)
{
   /* ------- build the new entry ------- */
   ListEntry *thisentry = new ListEntry;
   thisentry->entrydata = new char[size];
   memcpy(thisentry->entrydata, newentry, size);

   if (CurrEntry == NULL)   {
      thisentry->PrevEntry = NULL;
      // ---- adding to the beginning of the list
      if (FirstEntry != NULL)   {
         /* ---- already entries in this list ---- */
         thisentry->NextEntry = FirstEntry;
         FirstEntry->PrevEntry = thisentry;
      }
      else   {
         // ----- adding to an empty list
         thisentry->NextEntry = NULL;
         LastEntry = thisentry;
      }
      FirstEntry = thisentry;
   }
   else   {
      // ------- inserting into the list
      thisentry->NextEntry = CurrEntry->NextEntry;
      thisentry->PrevEntry = CurrEntry;
      if (CurrEntry == LastEntry)
         // ---- adding to the end of the list
         LastEntry = thisentry;
      else
         // ---- inserting between existing entries
         CurrEntry->NextEntry->PrevEntry = thisentry;
      CurrEntry->NextEntry = thisentry;
   }
   CurrEntry = thisentry;
}

// ---------- delete the current entry from the list
void LinkedList::delete_entry(void)
{
   if (CurrEntry != NULL)   {
      if (CurrEntry->NextEntry != NULL)
         CurrEntry->NextEntry->PrevEntry = CurrEntry->PrevEntry;
      else
         LastEntry = CurrEntry->PrevEntry;
      if (CurrEntry->PrevEntry != NULL)
         CurrEntry->PrevEntry->NextEntry = CurrEntry->NextEntry;
      else
         FirstEntry = CurrEntry->NextEntry;
      delete CurrEntry->entrydata;
      ListEntry *hold = CurrEntry->NextEntry;
      delete CurrEntry;
      CurrEntry = hold;
   }
}

// ---- get the first entry in the list
void *LinkedList::getfirst(void)
{
   CurrEntry = FirstEntry;
   return CurrEntry == NULL ? NULL : CurrEntry->entrydata;
}

// ---- get the next entry in the list
void *LinkedList::getnext(void)
{
   if (CurrEntry == NULL)
      CurrEntry = FirstEntry;
   else
      CurrEntry = CurrEntry->NextEntry;
   return CurrEntry == NULL ? NULL : CurrEntry->entrydata;
}

// ---- get the previous entry in the list
void *LinkedList::getprev(void)
{
   if (CurrEntry == NULL)
      CurrEntry = LastEntry;
   else
      CurrEntry = CurrEntry->PrevEntry;
   return CurrEntry == NULL ? NULL : CurrEntry->entrydata;
}

// ---- get the last entry in the list
void *LinkedList::getlast(void)
{
   CurrEntry = LastEntry;
   return CurrEntry == NULL ? NULL : CurrEntry->entrydata;
}
```

## LISTING 3

```cpp
// -------- demolist.cpp

#include <stream.hpp>
#include "strings.h"
#include "linklist.h"

void collectnames(LinkedList& namelist);
int menu(void);
void displaynames(LinkedList& namelist);
void stepforward(LinkedList& namelist);
void stepbackward(LinkedList& namelist);
string insertname(LinkedList& namelist);

void main(void)
{
   cout << "Enter some names followed by \"end\"\n";
   // ------ a linked list of names
   LinkedList namelist;
   collectnames(namelist);
   int key = 0;
   while (key != 6)   {
      switch (key = menu())   {
         case 1:
            displaynames(namelist);
            break;
         case 2:
            stepforward(namelist);
            break;
         case 3:
            stepbackward(namelist);
            break;
         case 4:
            insertname(namelist);
            break;
         case 5:
            namelist.delete_entry();
            break;
         case 6:
            cout << "Quitting...";
            break;
         default:
            break;
      }
   }
}

void collectnames(LinkedList& namelist)
{
   // ------- until the user types "end"
   while (insertname(namelist) != "end")
      ;
}

int menu(void)
{
   cout << "\n1 = display the names";
   cout << "\n2 = step forward through the names";
   cout << "\n3 = step backward through the names";
   cout << "\n4 = insert a name";
   cout << "\n5 = delete the current name";
   cout << "\n6 = quit";
   cout << "\nEnter selection: ";
   int key;
   cin >> key;
   return key;
}

// ------ read the names in a list and display them
void displaynames(LinkedList& namelist)
{
   cout << "------ NAME LIST ------\n";
   char *name = namelist.getfirst();
   while (name != NULL)   {
      cout << name << "\n";
      name = namelist.getnext();
   }
   cout << "-----------------------\n";
}

// ------- step forward through the list of names
void stepforward(LinkedList& namelist)
{
   char *name = namelist.getnext();
   cout << (name ? name : "-- End of list --") << "\n";
}

// ------- step backwardward through the list of names
void stepbackward(LinkedList& namelist)
{
   char *name = namelist.getprev();
   cout << (name ? name : "-- Beginning of list --") << "\n";
}

// ------- insert a name into the list
string insertname(LinkedList& namelist)
{
   cout << "Enter a name: ";
   // ----- a string to hold one name
   string name(80);
   // ------- read a name
   cin >> name.stradr();
   // ------- add it to the list
   if (name != "end")
      namelist.addentry(name.stradr(), name.length());
   return name;
}
