
# C Programiranje 01

## Da krenemo

Al Stevens, avgust '88

Kada su me ljudi iz DDJ pozvali da radim ovu kolumnu, ponudili su mi šansu da vratim dugove koji su dugo dospevali. Kao lojalni čitač DDS-a od njegovih najranijih dana, dobio sam mnogo znanja, saveta i dobrog koda sa njegovih stranica. Ova kolumna je sada prva rata u onome što bi moglo da postane dug period otplate. Molim vas, sada mi ugodite malo uvoda.

Počeo sam da programiram pre 30 godina kada su pravi piloti leteli taildragger-ima, prave limenke piva su bile napravljene od čelika, a pravi kompjuteri su imali memoriju bubnja i logička kola vakuumske cevi. Ova izjava verovatno govori više o mojim godinama nego o mojim kvalifikacijama jer se malo od onoga što sam naučio tokom tih ranih godina primenjuje na današnju profesiju razvoja softvera. Programiranje se tada nije smatralo profesijom, pa čak ni karijerom, i niko nije mnogo brinuo o tome da li je programiranje umetnost ili nauka. Bio je to posao. Nije bilo univerzitetskih kurseva, strukture ili discipline, niti časopisa. Niko nije objavio niti delio izvorni kod. Časopis MAD je imao šest godina, Plejboj pet, a DDS još nije bio sjajan. Kao što moj stari prijatelj Bill Chanei kaže, "Vremena su bila teška, Buddi." "Sada su bolja vremena, Bill".

Moj brat Fred me je upoznao sa DDS-om kada je njegov naslov bio mnogo duži i hirovitiji. Fred je napravio jedan od ranih računara Altair 8080 oko 1976. Napravio ga je za upotrebu u razvoju sistema zasnovanih na mikroprocesoru. Fred i ja nismo bili hobisti ili hakeri u uobičajenom smislu – naše potrebe su bile svakodnevnije, i iako nismo imali veze sa hakerima, nije nam nedostajalo hakerskog entuzijazma. Naše kombinovane konsultantske aktivnosti – njegove u dizajnu hardvera, a moje u razvoju softvera – zahtevale su testiranje i razvojni sistem. Altair je bio samo karta. Bilo je zabavno graditi i trčati. DDS je u to vreme bio neophodan dodatak laboratoriji. Nije bilo mnogo softvera i uvek smo mogli da računamo na dobrog doktora za izdašnu dozu korisnog koda. Fred ima svako izdanje DDS-a.

Iako je Fred sada praktikant Forth-a (radim na tome da ga preobratim) on me je uveo u C. Mnogo sam putovao, i jednog dana mi je gurnuo paket na izlazu - nešto za čitanje u avionu, rekao je. Bio je to BDS C kompajler za CP/M sa kopijom Kernigana i Ričijevog Programskog jezika C. Sećam se da sam pomislio da jezik koji ima samo jedno slovo za ime ne sme biti mnogo jezik. Nazivi pravih jezika su zdravi, tehnički akronimi kao što su ALGOL, MACRO-11 i MUMPS. ("MUMPS" je zdrav?) Bio sam skeptičan prema jeziku sa slabim imenom kao što je "C". Bez obzira na to, čitao sam K&R tokom leta, i do trenutka kada sam "deplanirao", bio sam konvertit u C. Nikada nije postojala takva elegancija u programskom jeziku. Činilo se da su sve stvari koje programeri ne vole u ​​sintaksi koda majstorski ispravljene u jednostavnoj i moćnoj strukturi C-a. Njegovo labavo tipiziranje stvoreno je za savršeno podudaranje između C i sistemskog programiranja. C je izgledao kao idealan jezik. Čitaocima DDS-a ne treba da im to govorim, ali ja to ipak volim da kažem.

Prvom prilikom sam krenuo da napravim svoj prvi C program, verujući Kerniganu i Ričiju da se programski jezik uči pisanjem programa. Prvo, trebao mi je program za pisanje. Kako se ispostavilo, već mi je trebao lični program za jedno od mojih glupih diverzija, oni kriptogrami koje novine vode ispod ukrštenice. Videli ste ih. Oni šifruju pametnu frazu jednostavnim zamenama slova dajući vam jedno od slova kao trag. Morate da dešifrujete frazu, obično otkrivajući stenjanje igre reči. Najteži deo kod kriptograma je prikupljanje činovničkih napora da se prati koja slova ste koristili i gde se nalaze u poruci. Moja ćerka Šeron ima gumicu koja je iseckala više papira nego Faun Hol dok je cepala margine lokalnih dnevnih vesti dešifrujući nešto poput "hzsdmpdi nsvigotrf pm ijr hzsddmpdrf tiddosm npcrt".

Dakle, prvi C program koji sam napisao imao je kao svoju plemenitu svrhu održavanje zamena slova i prikaz delimično dekodiranih kriptograma. iznenađujuće je koliko brzo možete dekodirati kriptogram kada se eliminiše dosada. Primer 1, na strani 101, je PC verzija programa. Originalna verzija je radila sa bilo kojim terminalom koji sam imao u to vreme. Za pokretanje ove verzije trebaće vam instaliran drajver ANSI.SYS. Većina C kompajlera i interpretera će se nositi sa tim.

Završimo ovo putovanje kroz traku sećanja priznanjem. Uprkos tome što sam dobio ovu kolumnu kao zadatak, nisam baš stručnjak za jezik C. Za trenutak ću opisati problem za koji je trebalo nekoliko dana da se reši. Pretpostavljam da napišem ovu kolumnu jer sam verovatno poput vašeg programera na C-u koji može da pokrene programe i koji ima opšte razumevanje šta predstavlja koristan, pouzdan, C program koji se može održavati. Ako tražite naučnu obradu jezika kroz akademski mikroskop, potražite na drugom mestu. Ti forumi postoje i imaju vrednost. Ova kolumna govori o korišćenju jezika C kao alata za pravljenje programa koje ljudi mogu da koriste.

Toliko o upoznavanju i dobrim starim danima. U narednim mesecima ćemo se prepustiti omiljenoj zabavi, objavljivanju i objašnjenju alata C jezika za višekratnu upotrebu. Napisao sam četiri knjige i nekoliko članaka na tu temu, a povratne informacije čitalaca govore da je kraj itekako vredan traganja. Ova kolumna je dobar medij za takav materijal; Objavljivanje besplatnih softverskih alata je cenjena tradicija DDS-a.

Mali program za dešifrovanje u ovom izdanju ne mora nužno da bude primer onoga što možete očekivati od ove kolone. Kao i kod prethodnih kolumna "C sanduk", većina ovih kolumna će imati značajne primere C koda. Međutim, neke kolumne će odstupiti da bi raspravljale o drugim stvarima. Mogao bih da recenziram knjigu ili proizvod ili intervjuišem C-svetilo. Povremeno ću posvetiti kolumnu pismima od vas dragi čitaoci. Međutim, uvek će postojati bar vinjeta koda. Tako je mala igra u Listingu 1.

Jedna od prednosti pisanja kolumne je da možete da uključite neke svoje stvari. U prošlosti sam vodio ne baš uspešan posao sa prodajom softverskih alata poštom. Neki od proizvoda su i dalje dostupni, ali više nemam nikakav poslovni interes za njih. Povremeno se u ovoj kolumni može raspravljati o jednom od njih ako je relevantno za predmet. Ako se to dogodi, reći ću vam o posebnom odnosu koji imam sa predmetom kako biste razumeli moj neobuzdani entuzijazam i prihvatili ga kako god želite. Sve to na stranu, ne ustručavam se da vas ohrabrim da čitate moje knjige.

## Krokiji

Džejms J. Kilpatrik je nacionalni politički komentator sa širokom čitalačkom publikom na uredničkim stranicama mnogih novina. Pored pisanja i pojavljivanja u televizijskim programima, gospodin Kilpatrik je sebe postavio za jednog od čuvara dobrog stila i pravilne upotrebe engleskog jezika. (Samoimenovani čuvari stila i upotrebe su jedina vrsta koju dobijamo – niko drugi ne brine dovoljno da ih imenuje.) Napisao je divnu knjigu pod nazivom "Umetnost pisca" u kojoj navodi ono što on naziva svojim uputstvima o praksi pisanja. Kilpatrikovi krokiji su kućni ljubimci kao što su zloupotrebe reči nadamo se i jedino, a njegova lista je pametno predstavljena i ima mnogo smisla. Ako volite da čitate ili pišete, preporučujem vam da pročitate "Umetnost pisca". Sam predgovor Vilijama F. Baklija vredi svoje cene. Nemojte se odlagati ako se politika bilo kog autora razlikuje od vaše. U "Umetnosti pisca" nema političkih izjava, samo dobar smisao za pisanu reč. Pozajmivši Kilpatrikovu ideju i prenevši njenu primenu sa engleskog na C, svaka kolumna "Programiranje C" će ponuditi jedan ili više mojih sopstvenih C programskih krokija kada mi padnu na pamet i kada naiđem na njih. Evo nekoliko primera za početak liste.

Primer 1: PC verzija crypto.c

```c
#include <stdio.h>
#include <ctype.h>
#include <string.h>
char crypto [80];       /* encrypted message */
char decoded [80];      /* decrypted message */
char alphabet [];       /*"abcdefghijklmnopqrstuvwxyz"*/
char subs [];           /*"                          "*/

/* -----ANSI.SYS screen driver-----------*/
#define cursor (x,y) printf("\033[%02dH",y,x)
#define clear_screen() puts("\033[2]")

main ()
{
    int i, cp, a, b;
    char sb [3];
    clear_screen ();
    cursor (1, 6);
    puts("Enter the encrpted quote:\n");
    fget(crypto, 80, stdin);
    for (i = 0; i < strlen(crypto); i++)
        decoded[i] = ' ';       /* clear the decrypted msg */
    decoded[i] = '\0';
    while (1)  {
        cursor(1,9);
        puts(decoded);
        puts(alphabet);         /* display the alphabet */
        puts("\nEnter 2 letter substitution: (xy):");
        if(fgets(sb, "99", 2) == 0)
            break;
        a = *sb, b = *(sb+1);
        if (isalpha(a) && isalpha(b)) {
            for (cp = 0; cp < 26; cp++)
                if (subs[cp] == a)
                subs[cp] = ' ';
            subs[b - 'a'] = a;
            for (cp = 0; cp < strlen(crypto); cp++) {
                if (decoded[cp] == b)
                    decoded[cp] = ' ';
                if (tolower(crypto[cp]) == a)
                    decoded[cp] = b:
            }
        }
    }
}
```

Primer 2: Nekoliko načina za postavljanje zagrada i uvlačenje linija.

```c
/* --- then ... endif style (K&R, too) --- */
if (a == b)    {
    foo();
    bar();
}
/* --- begin ... end style --- */
if (a == b)
{
    foo();
    bar();
}
/* --- do ... doend style --- */
if (a == b)
    {
    foo();
    bar();
    }
/* --- poised roadrunner style (ugh!) --- */
if (a == b)
{   foo();
    bar();    }
```

### C kroki broj 1

Uznemiren sam programima predprocesora jezika C ili datotekama makroa koji vas podstiču da kodirate u nekoj nestandardnoj sintaksi, koja se zatim prevodi u C. Praksa sugeriše da je jeziku C potrebno poboljšanje (izvan ANSI i C + +, naravno). Na primer, neki predprocesori ili makroi žele da koristite početak i kraj umesto otvorenih i zatvorenih zagrada, navodno da bi vaš kod izgledao više kao Pascal ili na drugi način čitljiviji. po mom mišljenju, početnicima C programerima bolje je bez ovih takozvanih C pojačivača. Bolje je naučiti pravu stvar i zavoleti je mnogo ranije.

### C kroki broj 2

Ne volim da čitam C programe koji su nedosledni u svom pozicioniraju zagrada i kodu za uvlačeje. Primer 2, ova stranica, predstavja nekoliko načina na koje možete postaviti zagrade i uvući linije.

Koji god od ovih ili drugih stilova da preferirate, budite dosledni ili rizikujte da izazovete probleme sledećem čitaču vašeg koda. Problem nedoslednosti obično se pojavjuje kada neko modifikuje kod drugog i dva programera preferiraju različite stilove. Program sa takvim mešanim konvencijama je haos.

Dva krokija su dovojna da vam daju ideju i trebalo bi da budu dovojna za mesečnu kolumnu. Moji krokiji su zasnovani na mojim mišjejima i ne odražavaju ničiju zvaničnu sankciju. Nisu svi krokiji o upotrebi C. Neki bi mogli biti o kompajlerima ili bibliotekama ili piscima ili bilo čemu što se mene tiče. Ako želite da dodate ili oduzmete spisak – možda se zamarate piscima koji uvek koriste foo i bar u svojim primerima – molimo vas da svoje priloge pošajete na DDS ili na BIX (alstevens) na CompuServe (71101,1262). Ukjučiću zanimjive, smešne ili na neki drugi način relevantne krokije u ovu kolonu zajedno sa imenima saradnika za C krokije.

## Odeljak za knjige

Knjiga meseca je Macintosh "Programming Secrets" od Skota Knastera. Nema C kod, ali knjiga je neophodna za čitanje za svakog programera koji će se pozabaviti Macintosh-om. Ne dozvolite da vas doza Pascala uplaši; ti Pascal momci će se uskoro pojaviti, a trenutno postoji nekoliko dobrih C kompajlera za Macintosh.

Osim što je dobar uvod u unutrašnjost ove fascinantne mašine, knjiga je smešna baš kada treba, i nikada nije smešna kada ne. Obično ne volim kompjuterske knjige koje pokušavaju da budu smešne. Često autor ne može da to izvede, koristeći zamornu parodiju ili umornu satiru. Knaster je uspešno izbalansirao zabavni humor i vredne tehničke informacije. Većina pisaca želi da to mogu da urade i da se izvuku. Obično mudri stari urednik sa zelenim nijansama za oči, zlobnom crvenom olovkom i bledim smislom za humor precrta sve te duhovite dragulje. (ako ne čitate ove reči, to se ponovo dogodilo). I pored toga, pozivamo vas da pročitate Knasterovo delo. On je imao vlast nad tim urednikom i uspeo je da na tržište izbaci knjigu koja vas obaveštava, zabavlja i mestimično nasmeje.

## Dete K&R

Deset godina nakon prvog izdanja, Programski jezik C je objavljen u drugom izdanju. Dešava se da može biti mnogo C programera koji nikada nisu pročitali prvo izdanje. Sramota. Mnoštvo drugih knjiga o C-u ima za cilj da ponovo uradi ono što "bela knjiga” najbolje radi: opiše C programerima. Za moj novac, K&R je i dalje najbolji uvod u C ako već znate šta je programiranje. Drugo izdanje dostiže ANSI standard u nastajanju (oh, toliko dugo je "nastajao") iako je Prentice Hol možda bio mudar da sačeka zvanični standard, a ne da promoviše knjigu kao "zasnovanu na nacrtu predloženog ANSI C" u baneru na koricama. Pretpostavljam da je nacrt prilično blizu konačnog standarda, ali ova rana publikacija verovatno znači da ću morati da platim još 21 dolar kada se ukloni oznaka "nacrt" za sledeće štampanje drugog izdanja.

**PS.**:  
Viđen ovog leta u Parizu: francuski automobil sa hromiranim amblemom koji identifikuje njegov model: "Turbo D." Novi jezik i kompajler od Filipa, možda?

## Zbunjujuće ali neophodne C konstrukcije

Pretpostavimo da imate niz struktura i da svaka struktura ima niz pokazivača funkcija. Sada, s obzirom na pokazivač na niz struktura, ceo broj koji ide u indeks na željeni element strukture u nizu, i ceo broj koji je u indeksu na funkciju koju želite da pozovete, kako napisati izraz koji poziva funkciju?

Dok razmišljate o tome, evo neke pozadine. Ova vežba nije još jedna C zagonetka, već pravi problem na koji se susreće pisac menija opšte namene. Upravljački program koristi nizove struktura i stringova da opiše hijerarhiju menija. Ovaj koncept alatke menija za višekratnu upotrebu je centralni za većinu C sistema koje sam razvijao, a njegov format se povremeno menjao da bi podržao različite stilove prezentacije menija. Nećete biti iznenađeni da ista tehnika značajno figurira u novom projektu "C programiranje" koji će uskoro biti otkriven.

Upravo pomenuta struktura opisuje meni. Niz tih struktura predstavlja sve menije u programu. Niz pokazivača funkcija predstavlja funkcije koje treba pozvati kada korisnik odabere odgovarajuće komande menija. Primer 3, ova stranica, prikazuje relevantne strukture podataka i stavke. U problemu, pokazivač "mnn" pokazuje na niz menija, "curr_menu" ceo broj predstavlja trenutni meni, a "selection" predstavlja trenutni ceo broj za izbor. Zamislite da želite da pozovete odgovarajuću funkciju i prosledite joj broj menija i broj za izbor koji su doveli do njenog poziva.

Primer 3: Struktura koja ukazuje na niz menija i funkcija za njihovu kontrolu.

```c
struct menus {
    char *menu_selections[MAX_SELECTIONS];
    int (*func[MAX_SELECTIONS])(int, int);
} mn{MAX_MENUS];

struct menus *mnn = mn;
int curr_menu;
int selection;
```

Iskusni C programer može intuitivno smisliti tačan izraz bez razmišljanja o tome. Novi C programer će, međutim, morati da radi na tome neko vreme. Možete tvrditi da novi C programer nikada ne bi imao ovu dilemu jer konstrukcija ne bi bila očigledna – programer bi dizajnirao jednostavniju strukturu podataka – možda različite nizove za svaki meni. Ipak, ovi problemi su stvarni i bolje ćete razumeti jezik C nakon što ih rešite.

Da li se odgovor čini očiglednim? Ako jeste, čestitam. Ako ne, razjasniće se kada to shvatite. Ranije sam, kao mladi C programer, dizajnirao sebe u ovom uglu ne shvatajući koliko će to biti zbunjujuće. Tačna sintaksa pozivajuće izjave je ono što mi je izmicalo. Dakle, uzimajući komponente iskaza, isprobao sam svaku varijaciju zagrada, indeksa i operatora pokazivača dok izjava nije proradila. Rešenje koje je radilo sa jednim kompajlerom nije uspelo sa nekoliko drugih. Onaj koji je kasnije prikazan bio je onaj koji su svi prevodioci prihvatili.

Da biste ga izvukli, možete razložiti različite delove problema, što je bolji pristup od moje metode kamen spoticanja. Počinjemo tako što znamo da poziv funkcije preko pokazivača izgleda ovako:

```c
(*func)(curr_menu, selection);
```

Func pokazivač sadrži adresu neke funkcije. U ANSI standardu, možete izostaviti zvezdicu i zagrade za vezivanje tako da poziv funkcije izgleda kao običan poziv funkcije bez pokazivača. Nisam baš spreman za ovu novu konvenciju. Starija konvencija – koja je još uvek podržana, hvala Bogu – govori vam dok čitate izjavu da se poziv vrši preko pokazivača. Ova konvencija vam daje naznaku gde da tražite funkciju koja se poziva. Takvi tragovi su vredni kada pretražujete neki stari, možda nedovoljno komentarisani kod. Više volim kada jezik pomaže da se objasni sam.

Evo načina na koji se adresa funkcije dodeljuje pokazivaču funkcije:

```c
func = funcname;
```

Jednostavno, zar ne? To ste već znali. Pretpostavimo sada da je "func" pokazivač na niz funkcija. Ako "selection" korisničkog menija odgovara stavki u nizu, dodeljivanje izgleda ovako:

```c
func[selection] = funcname;
```

I ovaj zadatak je očigledan. Ali da biste pozvali ovaj niz pokazivača, potreban vam je pretplatni ceo broj da biste odredili koji od pokazivača u nizu sadrži adresu funkcije koju treba pozvati. Pretplaćeni poziv izgleda ovako:

```c
(*func[selection]) (curr_menu,selection);
```

Ovaj poziv nije tako očigledan. Trebalo mi je malo eksperimentisanja da dođem do toga. Sada na idemo strukturu: ako je pokazivač funkcije u strukturi bio u nizu, a "mn" je ukazivao na konkretnu strukturu, dodeljivanje bi bilo ovako:

```c
mn->func = funcname;
```

Tada bi poziv izgledao ovako:

```c
(*mn->func)(curr_menu, selection);
```

Napredak do ove tačke takođe nije tako očigledan, ali je razuman. Trebalo mi je vremena da shvatim blizinu operatora pokazivača - zvezdice i zagrada. Sada dodajte indeks selekcije u izjavu i to izgleda ovako:

```c
(*mn->func[selection]) (curr_menu, selection);
```

Ali u problemu, pokazivač "mn" pokazuje na niz struktura – prvi element strukture u nizu – umesto na željeni element strukture, a ceo broj curr_menu pokazuje koji element u nizu predstavlja trenutni meni. Dakle, konačni odgovor je ovaj:

```c
(*(mm + curr_menu)->func[selection])(curr_menu, selection);
```

The new ANSI rule would simplify it only a little as shown here.

```c
(mn + curr_menu)->func[selection](curr_menu, selection); 

// ili 
mn[cur_menu]->func[selection](curr_menu, selection);
```

Zbog bliskog odnosa između pokazivača i nizova, postoje varijacije ovog izraza koje će takođe funkcionisati. Svi su podjednako zagonetni.

Ovakvi problemi će obeshrabriti sve osim najjačih srca. Kada se C programeri okupe oko hladnjaka i razgovaraju o takvim stvarima, oni imaju tendenciju da takva rešenja odbace kao trivijalna i očigledna. Izluđujuća je karakteristika C-a da su odgovori na zagonetke kodiranja neuhvatljivi kada ih ne znate i očigledni kada ih znate. Ova karakteristika vas tera da se zapitate da li ste jedini koji ne razumete. Ohrabri se. Osim ako niste kompjuterski naučnik i stručnjak za jezik sa prirodnom sklonošću refleksivnom raščlanjivanju s leva na desno i evaluaciji prioriteta, moraćete s vremena na vreme da rešite problem poput ovog. Koristeći postepeni pristup otkrivanju složenog C idioma, kao što je ovaj ilustrovan ovde, možete izbeći metod pokušaja i greške koji sam nevino koristio. Evo saveta. Prijavite se na BIX ili CompuServe i pitajte nekoga. Tamo ima nekih pametnih ljudi i skoro sigurno će neko od njih rešiti sličan problem. Ako nije, neko je obično voljan da ode van mreže, smisli rešenje i ponovo se prijavi da objavi odgovor. Nikada me nisu razočarali ovi velikodušni ljudi.

## C programski projekat

Tokom narednih nekoliko meseci, koristiću forum u rubrici "Programiranje na C" za razvoj pomoćnog programa na jeziku C. Puna svrha programa biće otkrivena u kasnijoj kolumni, ali za sada ću reći da će se program koristiti za modemsku komunikaciju i da će raditi na IBM PC-u i kompatibilnim računarima. Nemojte ovo odbaciti kao samo još jedan modemski program; Reći ću vam potpuni razlog zašto je uskoro i biće od interesa za većinu vas. Većina izvornog koda će biti objavljena u ovoj koloni; sve će to biti lako dostupno svakom DDS čitaču u štampanim i magnetnim medijima.

Projekat će se sastojati od brojnih C alata koji su prilagođeni iz C biblioteka, a postojaće i neki prilagođeni kod aplikacija. Počeću tako što ću definisati i razvijati alate – među njima biblioteku prozora, menadžer menija (sa tim neuhvatljivim pokazivačem funkcije), biblioteku pomoći, ekrane za unos podataka, funkcije komunikacije i uređivač teksta. Svakog meseca ću dodavati nove alate kolekciji. Moći ćete da koristite alate u drugim aplikacijama, ali njihova primarna svrha je podrška projektu. Skup alata će se razvijati odozdo prema gore. Dok budem gradio nove mogućnosti, oni će koristiti funkcije u bibliotekama koje su im prethodile. Oni od vas koji čitaju knjige koje pišem prepoznaće stil (i neke od funkcija).

Program će, naravno, biti napisan na C-u, da bi se kompajlirao sa Borlandovim Turbo C-om, koji sam izabrao zbog njegove popularnosti i jake biblioteke funkcija teksta na ekranu. Turbo C pruža funkcije za upravljanje ekranom računara koje oslobađaju teret brige oko adaptera za ekran i signala za praćenje video zapisa. Pošto će samo alatke za upravljanje prozorima koristiti ove Turbo biblioteke, ostatak koda možete preneti na druge C kompajlere uz malu modifikaciju.

Ovaj projekat će biti interaktivan. Vaši komentari i sugestije će doprineti njegovom napretku. Možda ćemo između nas napisati sledeći veliki američki program.

## C Odjava za programiranje

Želeo bih da završim ovo, svoje prvo putovanje u rubrici "Programiranje C", vrhom kape mom prethodniku, Alenu Holubu, čiji je doprinos unapređenju jezika C značajan. Nismo se sreli, ali sam stekao mnogo od njegovog rada u njegovoj kolumni i njegovim knjigama. Alen je dobio glas zahvalnosti u predgovoru za drugo izdanje Kernigana i Ričijevog Programskog jezika C. U ovom poslu, takvo priznanje je ekvivalent Oskaru. Zbogom, Alen, u tvojim novim poduhvatima.
