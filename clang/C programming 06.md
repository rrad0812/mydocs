
# C programiranje 06

## Surogate Library

Al Stevens, januar '89

Nekoliko čitalaca je pitalo zašto sam izabrao Turbo C za projekat kolumne „Programiranje C“. Mnogi od vas koriste druge kompajlere, a neki se osećaju izostavljenim. Jedan takav čitalac ide toliko daleko da kaže da će mu sledećih nekoliko brojeva DDJ-a biti beskorisno i da će verovatno dozvoliti da mu istekne pretplata. To mi je privuklo pažnju.

Sva ova nevolja je zato što su programi o kojima se raspravlja napisani sa specifičnim funkcijama upravljanja ekranom Turbo C. Ovaj čitalac je smatrao da bi se kolumna trebalo ograničiti na generički ANSI C kad god je to moguće kako bi doprla do najšire moguće publike. Taj argument je opravdan i ne želim da izgubim čitaoce, posebno one kojima je dovoljno stalo da mi kažu kada stvari nisu u redu. Moram, dakle, da se pozabavim ovom zabrinutošću.

Prvo, hajde da razmotrimo zašto generički C nije praktičan za ovaj projekat. Program koristi naše sopstvene video prozore na IBM PC-u i kompatibilne. Takav program mora, ako se uzme u obzir performanse, direktno adresirati video memoriju. Program će takođe koristiti serijski port računara. Ovi zahtevi vezuju program za hardversku arhitekturu računara. Naravno, većina mnogih C kompajlera za PC bi se mogla koristiti za razvoj takvog programa, a svaki takav program bi se donekle razlikovao od ostalih. Metode koje koristite za pristup hardveru niskog nivoa su dovoljno različite sa svakim kompajlerom da će rezultirati različiti programi. Možda postoji potreba za standardnom bibliotekom za MS-DOS PC programe, ali takav standard nije prihvaćen. Neki prevodioci pokušavaju da budu kompatibilni sa drugima, ali ovi pokušaji propadaju kada programeri kompajlera rade na sličnim ekstenzijama u isto vreme. Pogledajte različite pristupe grafičkim bibliotekama u Turbo C i Microsoft C.

Vest, međutim, nije sve loša. Relativno mala količina koda u našem projektu je specifična za Turbo C i zavisi od njega. Sav takav kod je u oblasti upravljanja ekranom, i predstavljen je skupom Turbo C funkcija i nekoliko internih Turbo C konstrukcija. Većina ostatka koda je generička, barem na nivou računara. Microsoft C i Turbo C rade mnoge stvari na isti način. Zapamtite, međutim, da je osnovna teorija koja se primenjuje na naše video prozore zasnovana na video arhitekturi računara; funkcije koje razvijamo su za PC sa MS-DOS-om. To nisu CP/M, Unix ili VAX/VMS programi, na primer. Ova činjenica bi mogla da izazove novu galamu, a ja nerado čekam tu vrućinu i pokušaću da hrabro izdržim pod njom.

Zašto sam izabrao Turbo C? Odgovor je jednostavan: Turbo C je kompajler koji koristim za skoro sve ostalo u MS-DOS-u. To nije toliko potvrda koliko puka činjenica. Koristim Turbo C jer ima odličnu podršku za vrste programa koje pišem: TSR-ove i programe sa video prozorima. Microsoft C ima određenu podršku za te oblasti, ali ne toliko kao Turbo C.

Možda sam izabrao Microsoft C za projekat, u kom slučaju bih se ovim istim govorom obratio korisnicima svih ostalih kompajlera. Sviđa mi se Microsoft C. Nedavno se moj rad proširio na OS/2 arenu, a taj poduhvat me je naterao da ponovo koristim Microsoft C. Sva ta sjajna podrška za Turbo C ne važi za OS/2. Prvo, Turbo C ne dolazi u OS/2 ukusu. Drugo, ne piše se ista vrsta TSR-a za OS/2 kao za MS-DOS. Treće, ne radi se Windows u OS/2; to će biti domen grupa ekrana i Presentation Manager-a, sviđalo se to vama ili ne.

## Surogat biblioteka za MSC

Kakvu korist ima sva ova racionalizacija za one od vas dragi čitaoci koji ne koriste Turbo C i koji žele da koriste kod u kolumni „C programiranje“? Nažalost, nijedan, i to je nešto što nameravam da delimično ispravim u ovoj kolumni. Ovog meseca ću obezbediti biblioteku Microsoft C funkcija i makroa koji pretvaraju projekat „Programiranje na C“ u program kompatibilan sa Microsoft C-om. Biblioteka će ih preslikati. Funkcije specifične za Turbo C koje sam koristio da biste mogli da kompajlirate funkcije projekta sa Microsoft C-om. U narednim mesecima dodaću ovu biblioteku (ako bude potrebno).

Šta je sa ostalim kompajlerima? Na kraju krajeva, bilo je bezbroj drugih MS-DOS C kompajlera. Lattice, Aztec, Vatcom, Zortech, MIKS, High C, Vhitesmith's, Mark Villiams, De Smet, KC88, Small C, Eco-C, sve su to imena koja mi padaju na pamet. U jednom ili drugom trenutku, koristio sam svaki od njih. Međutim, nemam nameru da ovde dam program nezavisan od kompajlera. Opisaću Turbo C funkcije koje se moraju emulirati za Microsoft C, i daću izvorni kod u tu svrhu. Korisnici tih drugih finih kompajlera mogu koristiti moj primer da naprave sopstvene biblioteke. Radim to za Microsoft C jer će to zadovoljiti najveću bazu čitalaca, a taj točak sada prilično upadljivo škripi i tako dobija mast. Da bih krenuo na ovo isto sporedno putovanje za sve te kompajlere, koristio bih moj dodeljeni prostor u DDJ-u za otprilike sledeću godinu, i ne bismo ništa drugo uradili.

Ovaj razvoj biblioteke nije veliki napor kao što ćete videti, ali ohrabrujem svakoga ko napravi jednu za drugi kompajler da pošalje kod; mi ćemo ga objaviti na CompuServe-u na DDJ forumu. Ali dozvolite mi da vas upozorim. U prošlosti sam pisao i prodavao biblioteku C funkcija i distribuirao verzije koje su radile sa većinom tih kompajlera. Bilo je to frustrirajuće iskustvo. Napor potreban da se ide u korak sa najnovijim verzijama kompajlera bio je značajan. Mete nikada ne bi mirovale. Na kraju sam se predao i distribuirao samo izvorni kod. Izvorni kod je bio mešavina uslova kompajliranja koji su upravljali razlikom između kompajlera i, čineći to, zamaglili prvobitno značenje koda. Takav nered je marginalno prihvatljiv u komercijalnom proizvodu, ali ga nikada ne bi trebalo videti u objavljenom delu gde je svrha isto toliko informisanja koliko i izvođenja.

Napravio sam ove surogat funkcije i testirao ih sa Microsoft C, verzijama 5.0 i 5.1. Pokušao sam da ih pokrenem i sa QuickC-om, ali datoteka "window.c" izaziva internu grešku kompajlera QuickC-a. Ponovo sam kompajlirao samo taj modul sa Microsoft C-om, povezao ga sa ostatkom programa kao što je kompajliran sa QuickC-om, i sve je funkcionisalo. Novija verzija QuickC-a možda neće imati ovaj problem, ali ja imam samo prvu verziju.

Biblioteka se sastoji od tri izvorne datoteke i make datoteke. Izvorni fajlovi su "microsft.h" (pogledajte [Listing 1](#listing-1)), "microsft.c" ([Listing 2](#listing-2)) i "vpeek.asm" ([Listing 3](#listing-3)). Make datoteka je "twrp.mak" ([Listing 4](#listing-4)). Datoteka make koristi ove nove funkcije da napravi prošlomesečni TWRP mali procesor teksta sa Microsoft C-om. Datoteka "microsft.h" treba da bude dodata ili uključena u datoteku "window.h" iz moje kolumne za septembar. Datoteka "microsoft.c" obezbeđuje surogat funkcije koje oponašaju one karakteristike projekta koje zavise od stvari jedinstvenih za Turbo C.

Ove emulacije nisu sveobuhvatne – neke od njih neće raditi kao Turbo C zamena za sve što možete da uradite. Umesto toga, oni pružaju dovoljnu podršku za načine na koje smo do sada koristili te funkcije u projektu "C programiranje". Iz tog razloga ih nazivam surogatima, a ne klonovima. Naravno, dok budem dodavao projekat, proveriću da li sav novi kod radi sa oba kompajlera i ažuriraću ovu biblioteku ako bude potrebno. Ako želite sveobuhvatnu biblioteku za unakrsne kompajlere, možete pokušati da kupite izvorni kod Turbo C runtime biblioteke i kompajlirate odgovarajuće funkcije sa Microsoft C-om. Takav ambiciozan poduhvat je van okvira ove kolumne. Ne treba da kažem ništa više od toga da bi to bio značajan napor, pun opasnosti.

Sledi diskusija o svakoj komponenti biblioteke.

- **"microsft.h"** -- treba da bude dodata ili uključena u "window.h" od septembra. Morate napraviti jednu promenu u "window.h" da biste prilagodili razlike između tretmana prototipova od strane dva kompajlera. U "window.h" otprilike u redu 9 videćete ovaj prototip:

```c
int select_window(int,int,int,int (*func)(int,int));
```

Morate ga promeniti na sledeće:

```c
int select_window(int,int,int,int (*)(int,int));
```

Microsoft C vam ne dozvoljava da mešate imenovane i neimenovane parametre u prototipu, dok Turbo C izgleda ne smeta.

Datoteka "microsft.h" počinje testom uslovne zastavice COMPILER-a u vremenu kompajliranja. Ova zastavica je definisana naredbom kompajliranja u make datoteci i govori kompajleru da koristi kod umetnut iz "microsft.h". Moj cilj je bio da minimiziram promene u već objavljenom kodu.

Izrazi #define za "movmem" i "setmem" zamenjuju imena odgovarajućih MSC funkcija.

- Funkcija "setmem" postavlja sve bajtove u baferu na određenu vrednost karaktera. Prvi parametar je pokazivač karaktera koji ukazuje na bafer. Drugi parametar je veličina bafera u bajtovima. Treći parametar je vrednost karaktera koja treba da se popuni u bafer. Odgovarajuća Microsoft C funkcija je `memset`. Njegovi parametri su u drugačijem redosledu od onih kod Turbo C `setmem`-a.

Funkcija "movmem" uzima tri parametra: pokazivač izvornog karaktera, pokazivač odredišnog karaktera i broj bajtova. Funkcija pomera blok memorije i uzima u obzir preklapanje izvornih i odredišnih blokova, pomerajući se sa ispravnog kraja bafera da bi sprečila replikaciju bajtova. Odgovarajuća Microsoft C funkcija je `memmove`. Njegovi parametri su u drugačijem redosledu od onih kod "movmem"-a. Imajte na umu da Microsoft C ima funkciju movmem koja podseća na movmem Turbo C-a, ali ne ispravlja preklapanje bafera i ne može se koristiti ovde.

Izrazi `#define` za `cprintf`, `cputs`, `getch` i `putch` zamenjuju imena funkcija u "microsft.c" za ova imena. I Turbo C i MSC imaju funkcije sa ovim imenima, ali njihova ponašanja su dovoljno različita da moramo da izvršimo zamene.

Slede prototipovi za funkcije TC-a koje MSC nema. Nakon toga su `#define` izjave za boje ekrana.

**"microsft.c"** -- Ova datoteka ima funkcije koje oponašaju Turbo C funkcije -- Turbo surogat. Počinje sa nekim `#includes` i prototipovima. "Vpeek" i "vpoke" prototipovi su za funkcije u "vpeek.asm", koje čitaju i pišu video memoriju kompenzujući sneg video zapisa. Ako vaš sistem ne koristi grafički adapter u boji (CGA), ne trebaju vam funkcije asemblerskog jezika.

Globalni simbol za vreme kompajliranja, ADAPTOR, određuje video adapter koji vaš program koristi, a VSEG simbol se automatski izjednačava sa adresom segmenta memorije za osvežavanje ekrana, što je 0kb000 za monohromatski adapter za ekran i 0kb800 za ostale. Simbol SNEG je postavljen na 0 ako adapter ne generiše video sneg kada se pristupi video memoriji. U suprotnom je podešen na 1. Ako koristite adapter koji nije CGA, možete ukloniti reference na "vpeek.asm" i "vpeek.obj" sa [Listinga 4](#listing-4), twrp.mak.

Sa Turbo C testovi za video segment i sneg su napravljeni u toku rada kompajliranim kodom i nisu vidljivi vama. Za ovu emulaciju podskupa, međutim, morate navesti video adapter kada kompajlirate program. Ovo je u skladu sa filozofijom ovog programa gde se konfiguracione stavke kontrolišu kompajliranim `#define` kontrolnim izjavama.

Video struktura je duplikat spoljne strukture koja je interna za Turbo C i koja se pominje u "window.c`. Ovde ga izjavljujemo jer Microsoft C nema takvu strukturu. Održavanjem vrednosti koje naš softver koristi, možemo naterati program da reaguje baš kao što to radi kada Turbo C run-time biblioteka (RTL) pokreće stvari.

Funkcija prozora se koristi za uspostavljanje pravougaonika memorije kao trenutnog video prozora. Njegovi celobrojni parametri su leve, gornje, desne i donje koordinate ekrana gde je gornja leva koordinata ekrana red 1, kolona 1, a donja desna koordinata red 25, kolona 80.

Funkcija "_vptr" vraća udaljeni pokazivač na video memoriju na osnovu k i i koordinata koje su joj prosleđene. Ovo je emulacija eksterne Turbo C funkcije koja se obično poziva samo iz Turbo C runtime biblioteke, ali koju smo koristili u "window.c". Sećate se da sam se kaznio što sam ga koristio. Sada plaćam.

Funkcija "_vram" upisuje linearni blok programske memorije u video memoriju. Njeni parametri su udaljeni pokazivač na početak lokacije video memorije, udaljeni pokazivač na bafer programske memorije i broj 2-bajtnih celih brojeva za pisanje. Video memorija se sastoji od 2 bajta -- video atributa i ASCII znaka -- za svaku poziciju znaka na ekranu. Ova funkcija je takođe emulacija one koja se obično poziva samo iz TC RTL-a. Koristili smo ga u "window.c" da postignemo glatko upisivanje na ekran bez smetnji zbog treperenja kursora preko pisanja, kao što vidite kada koristite "cprintf" i "cputs".

Funkcija "_getvram" je suprotna od "_vram". Čita, a ne upisuje video memoriju. Ova funkcija nije emulacija, već nam je potrebna za gettekt funkciju opisanu u nastavku. U Turbo C, funkcija "_vram" upravlja kretanjem video memorije u oba smera koristeći adrese segmenta da bi se utvrdilo da li je izvor ili odredište video RAM. Umesto da se mučim, kodirao sam jednostavan "_vram", a zatim dodao "_getvram" kao recipročan "_vram".

Funkcije "gettext" i "puttext" su funkcije video memorije za čitanje i pisanje višeg nivoa. One se bave linearnim baferima programske memorije i pravougaonim prozorima video memorije. Koriste se za čuvanje i vraćanje video prostora koji naši prozori zauzimaju. Koordinate (levo, gore, desno, dole) su u odnosu na ceo ekran i počinju sa 1,1 u gornjem levom uglu. Funkcija movetekt pomera video prozor. Koristimo ga za pomeranje prozora, tako da ova emulirana funkcija radi samo sa vertikalnim pokretima. Funkcija Turbo C pokreta teksta je mnogo pametnija, jer može da pomera prozor sa i na bilo koju poziciju na ekranu. Argumenti za pokretni tekst su koordinate četiri ugla originalnog prozora (u odnosu na ceo ekran) i gornje leve koordinate mesta na koje treba da se pomeri. Ako koristite CGA, možete eliminisati "movetext" jer smo koristili BIOS za pomeranje CGA ekrana iz razloga performansi. Da biste izbegli promenu funkcije pomeranja u "window.c", stavite nultu funkciju pod nazivom movetext u "microsft.c" umesto ove date.

Goto funkcija pozicionira kursor na lokaciju u trenutnom prozoru (onoj koja je poslednja definisana funkcijom prozora) kao što je navedeno parametrima x i y. Ovi parametri se odnose na prozor, pri čemu je 1,1 gornji levi ugao prozora.

Funkcije "wherex" i "wherey vraćaju trenutne x i y pozicije kursora u odnosu na trenutni prozor.

Funkcije boje teksta i pozadine teksta postavljaju boje koje će se koristiti sledeći put kada bude napisan tekst. Njihovi parametri su celobrojne vrednosti kao što je definisano u "microsft.h".

Funkcija "vprintf" je zamena za "cprintf". Oba kompajlera imaju slične funkcije `cprintf`, ali su oba povezana sa sopstvenim internim procesima prikaza teksta. Makro `cprintf` u "microsft.h" zamenjuje pozive `cprintf` sa onima za "vprintf". Ova nova funkcija čini jednostavan prevod koristeći `vsprintf` funkciju Microsoft C. Dobijeni string se zatim kopira u video memoriju na osnovu trenutne pozicije kursora. Imajte na umu da ova surogat funkcija ne pokušava da se bavi kontrolnim znakovima kao što su \n i \r. Ovaj propust je zato što nikada ne koristimo takve kontrole u ​​našim pozivima `cprintf`-u. Nadam se da se kasnije neću pokajati zbog ove prečice.

Zamenili smo "vgetch" i "vputch" za "getch" i "putch" jer postojeće funkcije Microsoft C ne rade u kontekstu u kojem ih koristimo.

"vpeek.asm" -- Funkcija "vpeek" ima dva nepotpisana parametra. Prva je adresa segmenta video memorije (uvek vrednost #define kao VSEG), a druga je adresa video offseta. Funkcija vraća vrednost od 2 bajta u adresi video memorije. Pre nego što pristupi video RAM-u, "vpeek" čeka ciklus ponovnog praćenja video zapisa da bi izbegao konflikt pristupa memoriji koji izaziva sneg između CPU-a i video kontrolera.

Funkcija "vpoke" ubacuje vrednost od 2 bajta u lokaciju video memorije sa istom rutinom uklanjanja snega kao i "vpeek". Ima ista dva parametra adrese video memorije kao vpeek. Njegov treći parametar je reč od dva bajta koja se ubacuje u video memoriju.

## Datoteke zaglavlja

Turbo C i Microsoft C definišu svoje prototipove funkcija u datotekama zaglavlja uglavnom na isti način. Iste datoteke zaglavlja definišu iste ANSI standardne funkcije. Međutim, postoje dve manje razlike. Turbo C definiše sve funkcije dodele memorije u "alloc.h", a Microsoft C koristi "malloc.h". Turbo C koristi "mem.h" za prototipove funkcija sličnih onima koje Microsoft C stavlja u "memory.h". Da biste zaobišli ovaj problem, možete staviti surogat "alloc.h" i "mem.h" datoteke u svoje Microsoft C datoteke zaglavlja. Datoteka "alloc.h treba" da jednostavno `#include malloc.h`. "Mem.h" će `#include memory.h`. Nisam uključio liste ovih jednostrukih.

## C Kroki: ANSI Gotcha

U svom cilju da specificira standardni jezik C koji je sve za sve računare, ANSI X3J11 komitet se suočio sa nekim teškim problemima. Njihova rešenja ne daju uvek najbolji odgovor za sve. Nekada davno, ako ste kodirali ovu izjavu:

```c
char cp [] = "\00123";
```

imate niz znakova od četiri karaktera inicijalizovan sa tri znaka i nultim terminatorom. Prvi znak je bila binarna vrednost 001 kao što je navedeno oktalnom sekvencom obrnute kose crte. Sledeća dva znaka su ASCII vrednosti '2' i '3'.

Nacrt ANSI specifikacije kaže da pošto neki računari imaju veličinu znakova veću od osam bita, redosled obrnute kose crte mora da dozvoljava više od tri cifre. Stoga će izjava sada deklarisati niz od dva znaka sa oktalnom vrednošću 123 (hek 53) praćen nultim terminatorom. Prevoditeljovo skeniranje cifara da bi se formirala integralna vrednost nastavlja se sve dok prevodilac vidi oktalne cifre.

Postojeći kod mogu da pokvare kompajleri koji se pridržavaju, posebno ako se niz kompajlira bez poruka upozorenja. ANSI ne daje odredbe za zaštitu postojećeg koda sa promenama koje donosi ovo novo pravilo. Ova diskusija je, stoga, namenjena onima koji bi mogli da prelaze na kompajler koji je kompatibilan sa ANSI. Na kraju će to uključiti sve nas. Ovo će biti deo budućeg ANSI nasleđa, ali hajde da vidimo kako to utiče na neke od nas trenutno.

Borland je odlučio da ispoštuje ovo novo pravilo standarda u Turbo C 2.0. Rečeno mi je da je ova odluka zasnovana na Borlandovoj posvećenosti potpunoj usklađenosti sa ANSI standardom. Sa tim stavom je teško raspravljati čak i kada posledice izgledaju strašne. Dozvolite mi da probam.

Kada su se programeri žalili da ovo novo pravilo krši postojeći kod, ponuđeno im je rešenje: Koristite funkciju ANSI konkatenacije stringova, koja funkcioniše ovako:

```c
char *cp = "\001" "123";
```

Ovo je, naravno, rešenje za programere novog koda i ne rešava problem sa kojim se suočavaju oni koji kompajliraju velike sisteme postojećeg koda. ANSI se pozabavio ovim pravilom da bi rešio probleme prenosivosti. Neke mašine imaju 6-bitne karaktere, neke 8, neke 12. Tradicionalno, kada je programer preneo C kod na novu mašinu, ovo je bilo jedno od razmatranja prenosivosti. Sada, međutim, novo ANSI pravilo - ili tačnije, usklađenost sa njim Borland - daje nam neočekivani problem prenosivosti i neželjeno iznenađenje. Programi koji su godinama kompajlirani ispravno na jedan način – uključujući i Turbo C 1.5 – sada nisu prenosivi na Turbo C 2.0, a kompajler ne izdaje nikakvo upozorenje i ne pruža način da isključi novo pravilo.

Jedina poruka o grešci povezana sa originalnim formatom javlja se kada slučajne oktalne cifre koje prate oktalnu konstantu formiraju integralnu vrednost veću od 255. Dakle, ako inicijalizator sadrži „\00377“, kompajler kaže da je vaš kod ispravan, ali kompajlira nešto drugo od onoga što nas je deset godina tradicije uslovilo da očekujemo. Međutim, ako je string „\00400“, dobijate grešku kompajliranja. U poslednjem slučaju možete učiniti nešto po tom pitanju. U prvom, nemate pojma da nešto nije u redu dok program ne prestane da radi.

Po mom mišljenju, kompajler treba da izda upozorenje kada odstupi od tradicije u ime ANSI. Upozorenje bi mogli da isključe oni koji ne trebaju ili žele da ga vide.

Možda bi prevodioci za računare sa 8-bitnim znakovima trebalo da ignorišu novo pravilo. Sumnjam da bi se mnogi korisnici žalili. Tako je, vaš kolumnista DDJ C ponekad zagovara nenasilnu građansku neposlušnost. Kao što je Pol Njumen rekao u filmu Hud: „Uvek sam verovao u blag odnos prema zakonu. Ponekad se oslanjam na jednu stranu, ponekad na drugu“.

Većina PC programa je, međutim, napisana tako da se izvršava unutar korisničkog interfejsa koji je čvrsto vezan za arhitekturu računara. Naš projekat kolumne „C programiranje“ je primer takvog programa. Ogromna većina programa napisanih u Turbo C-u nikada neće biti portovana nigde osim u sledeće poboljšano izdanje Turbo C-a. Pretpostavljam da ako Borland ponudi samo ANSI verziju kompajlera, bilo bi malo korisnika.

Borlandov zvanični stav po ovom pitanju je da kompajler pokazuje ispravno ANSI ponašanje i da je, prema tome, ispravan. S druge strane, Borland je kompanija koja sluša svoje korisnike. Ako dovoljno vas kaže da vam treba nešto da se promeni, oni će to promeniti. Kažem da nam treba poruka upozorenja.

Od verzije 1.0, preporučio sam Turbo C bez oklevanja. Međutim, dok se ovaj problem ne reši, predlažem da budete oprezni ako se Turbo C 2.0 koristi u projektima koji uključuju veliku pomoć postojećeg koda. Na sreću, Turbo C 1.5, Microsoft C 5.1 i drugi kompajleri nisu usvojili ovo pravilo, tako da imate razumne alternative Turbo C 2.0 ako ovo novo pravilo predstavlja problem.

## Druge Turbo C 2.0 ponude

Turbo C 2.0 ima obilje novih funkcija. Najistaknutiji je dugo očekivani integrisani debager u okruženju. Borland sada prodaje i samostalni Turbo Debugger, a Turbo C programi takođe mogu biti otklonjeni sa njim. Ako dobijete Turbo C Professional paket, dobijate Turbo C 2.0, Turbo Debugger i novi Turbo Assembler. Takođe su uključeni linker, bibliotekar, make utility, grep i mnoštvo drugih uslužnih programerskih programa. Nedostaje samo programski uređivač sa više prozora u klasi Brief ili Microsoft Editor.

Jedna nova karakteristika Turbo C 2.0 je mešoviti blagoslov. Svaka objektna datoteka je kodirana putanjama i imenima #include datoteka koje su ušle u njegovu kompilaciju. Mogućnost izrade projekta ima opciju koja se zove „Automatske zavisnosti“. Kada je ova opcija uključena, proces izrade proverava datume datoteka koje su uključene u odnosu na datum objektne datoteke. Ovo je korisno u projektima gde datoteke izrade projekta možda nisu aktuelne. Sa ovom funkcijom možete biti prilično aljkavi u održavanju datoteke izrade projekta ažurnom.

Zašto mešoviti blagoslov? Imena ugrađenih datoteka mogu značajno povećati veličinu datoteke objekta. Jedan programer je otkrio da njegova komercijalna biblioteka sada zahteva dodatne diskete. Postoji nedokumentovan TLIB prekidač (/0) za eliminisanje ovih nizova. Moji izvori kažu da će Borland objaviti uslužni program za uklanjanje imena putanja iz objektnih datoteka i da će sledeće izdanje Turbo C-a uključivati prekidač za njihovo suzbijanje. Međutim, sada kada ih imam, ne mogu bez njih. Želeo bih da ih koristi uslužni program MAKE komandne linije.

Greške u verziji 1.5 `cprintf` i `cputs` funkcijama su ispravljene u verziji 2.0. Šta je to, nisi znao da su te funkcije pokvarene? Onda vi, kao i ja, niste pročitali dokumentaciju, u kojoj je uvek pisalo da `cprintf` i `cputs` ne proširuju novi red u povratni red, prelazak na red. Tako je pisalo u dokumentaciji, ali u TC 1.5 nova linija je proširena. U 2.0 morate kodirati \r\n da biste dobili isti efekat koji ste imali sa \n ranije. Ovo održava kompajler u sinhronizaciji sa njegovom dokumentacijom, a takođe i sa Microsoft C. TC će morati da ostane blizu MSC-a sa mnogim takvim funkcijama ako namerava da uđe u OS/2 igru. Mnogi programeri su koristili cprintf na način na koji je funkcionisao, a ne na način na koji je opisan i nisu zadovoljni promenom.

Stiže...

Sledećeg meseca nastavljamo sa projektom. Razgovaraćemo o ozbiljnoj temi asinhrone komunikacije i dodati funkcije serijskog porta i modema u našu biblioteku.

## LISTING 1

```c
/*-------------- microsft.h ---------------- */
/* #include this file at the end of window.h */

#if COMPILER == MSOFT

/* this line replaces the select_window prototype in window.h */
int select_window(int, int, int, int (*)(int,int));

#define setmem(bf,sz,c) memset(bf,c,sz)
#define movmem(fr,to,ln) memmove(to,fr,ln)
#define cprintf wprintf
#define cputs(s) wprintf(s)
#define putch(c) wputch(c)
#define getch() wgetch()

void window(int lf,int tp,int rt,int bt);
void puttext(int lf,int tp,int rt,int bt,char *sv);
void gettext(int lf,int tp,int rt,int bt,char *sv);
void movetext(int lf, int tp, int rt, int bt, int lf1, int tp1);
void gotoxy(int x,int y);
void textcolor(int cl);
void textbackground(int cl);
int  wherex(void);
int  wherey(void);
void wprintf(char *, ...);
void wputch(int c);
int wgetch(void);

#define BLACK         0
#define BLUE          1
#define GREEN         2
#define CYAN          3
#define RED           4
#define MAGENTA       5
#define BROWN         6
#define LIGHTGRAY     7
#define DARKGRAY      8
#define LIGHTBLUE     9
#define LIGHTGREEN   10
#define LIGHTCYAN    11
#define LIGHTRED     12
#define LIGHTMAGENTA 13
#define YELLOW       14
#define WHITE        15

#endif
```

## LISTING 2

```c
/*----------- microsft.c ------------ */

/*
 *  Surrogate Turbo C functions
 *  for Microsoft C users.
 */

#include <dos.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <conio.h>
#include <bios.h>

/*-------- One of these is your Display Adapter -------- */
#define MDA 1           /* Monochrome Display Adapter */
#define CGA 2           /* Color Graphics Adapter     */
#define EGA 3           /* Enhanced Graphics Adapter  */
#define VGA 4           /* Video Graphics Array       */

#define ADAPTER EGA     /* Specifies the Display Adapter */

#if ADAPTER==MDA
#define VSEG 0xb000     /* VSEG is the video memory segment */
#else
#define VSEG 0xb800
#endif

#if ADAPTER==CGA
#define SNOW 1
/*--- assembly language vpeek.asm: manages CGA flicker --- */
void vpoke(unsigned adr, unsigned off, int ch);
int vpeek(unsigned adr, unsigned off);
#else
#define SNOW 0
/*---- macros for vpeek and vpoke for non-CGA systems ---- */
#define MKFP(s,o)    (((long)s<<16)|o)
#define vpoke(a,b,c) (*((int  far*)MKFP(a,b))=c)
#define vpeek(a,b)   (*((int  far*)MKFP(a,b)))
#endif

static union REGS rg;

/*--- a structure defined within Turbo C and used by us --- */
struct {
    char filler1[4];
    char attribute;     /* saves the current video attribute */
    char filler2[5];
    char snow;          /* says if the adapter snows */
} _video;

static int wlf,wtp,wrt,wbt; /* current window corners */
static int wx,wy;           /* current window cursor  */

/*------- define a video window ---------- */
void window(int lf,int tp,int rt,int bt)
{
    wlf = lf;
    wtp = tp;
    wrt = rt;
    wbt = bt;
    _video.snow = (char ) SNOW;
}

/*------ makes a video offset from x,y coordinates ----- */
#define vaddr(x,y) (((y)-1)*160+((x)-1)*2)

/*-- makes far pointer to video RAM from x,y coordinates -- */
void far * pascal __vptr(int x, int y)
{
    void far *vp;

    FP_SEG(vp) = VSEG;
    FP_OFF(vp) = vaddr(x,y);
    return vp;
}

/*---- writes a block of memory to video ram ----- */
void pascal __vram(int far *vp, int far *bf, int len)
{
    while(len--)    {
        vpoke(VSEG, FP_OFF(vp), *bf++);
        vp++;
    }
}

/*---- gets a block of memory from video ram ----- */
void pascal __getvram(int far *vp, int far *bf, int len)
{
    while(len--)    {
        *bf++ = vpeek(VSEG, FP_OFF(vp));
        vp++;
    }
}

/*----- writes a memory block to a video window ----- */
void puttext(int lf,int tp,int rt,int bt,char *sv)
{
    while (tp < bt+1)   {
        __vram(__vptr(lf, tp), (int far *) sv, rt+1-lf);
        tp++;
        sv += (rt+1-lf)*2;
    }
}

/*----- reads a memory block from a video window ------ */
void gettext(int lf,int tp,int rt,int bt,char *sv)
{
    while (tp < bt+1)   {
        __getvram(__vptr(lf, tp), (int far *) sv, rt+1-lf);
        tp++;
        sv += (rt+1-lf)*2;
    }
}

/*------ moves a video window (used for scrolling) ------ */
void movetext(int lf, int tp, int rt, int bt, int lf1, int tp1)
{
    int nolines = bt - tp + 1;
    int incr = tp - tp1;
    int len, i;
    unsigned src, dst;

    if (tp > tp1)   {
        src = tp;
        dst = tp1;
    }
    else    {
        src = bt;
        dst = tp1+nolines-1;
    }
    while (nolines--)   {
        len = rt - lf + 1;
        for (i = 0; i < len; i++)
            vpoke(VSEG, vaddr(lf1+i, dst),
                vpeek(VSEG,vaddr(lf+i, src)));
        src += incr;
        dst += incr;
    }
}

/*----- position the window cursor ------ */
void gotoxy(int x,int y)
{
    wx = x;
    wy = y;
    rg.h.ah = 15;
    int86(16, &rg, &rg);
    rg.x.ax = 0x0200;
    rg.h.dh = wtp + y - 2;
    rg.h.dl = wlf + x - 2;
    int86(16, &rg, &rg);
}

/*----- return the window cursor x coordinate ----- */
int wherex(void)
{
    return wx;
}

/*----- return the window cursor y coordinate ----- */
int wherey(void)
{
    return wy;
}

/*----- sets the window foreground (text) color ------- */
void textcolor(int cl)
{
    _video.attribute = (_video.attribute & 0xf0) | (cl&0xf);
}

/*----- sets the window background color ------- */
void textbackground(int cl)
{
    _video.attribute = (_video.attribute & 0x8f) | ((cl&7)<<4);
}

void writeline(int, int, char *);

/*------ our substitution for MSC cprintf -------- */
void wprintf(char *ln, ...)
{
    char dlin [81], *dl = dlin, ch;
    int cl[81], *cp = cl;
    va_list ap;

    va_start(ap, ln);
    vsprintf(dlin, ln, ap);
    va_end(ap);

    while (*dl) {
        ch = (*dl++ & 255);
        if (!isprint(ch))
            ch = ' ';
        *cp++ = ch | (_video.attribute << 8);
    }
    __vram(__vptr(wx+wlf-1,wy+wtp-1),
            (int far *) cl, strlen(dlin));
    wx += strlen(dlin);
}

/*------ our substitution for MSC putch -------- */
void wputch(c)
{
    if (!isprint(c))
        putch(c);
    wprintf("%c", c);
}

/*------ our substitution for MSC getch -------- */
int wgetch(void)
{
    static unsigned ch = 0xffff;

    if ((ch & 0xff) == 0)   {
        ch++;
        return (ch >> 8) & 0x7f;
    }
    ch = _bios_keybrd(_KEYBRD_READ);
    return ch & 0x7f;
}
```

## LISTING 3

```c
;---------------------------  vpeek.asm  ----------------------------
        dosseg
        .model compact
        .code
        public  _vpoke
; -------- insert a word into video memory
;   vpoke(vseg, adr, ch);
;   unsigned vseg;    /* the video segment address     */
;   unsigned adr;     /* the video offset address      */
;   unsigned ch;      /* display byte & attribute byte */
; ------------------------------------------
_vpoke  proc
        push    bp
        mov     bp,sp
        push    di
        push    es
        mov     cx,4[bp]    ; video board base address
        mov     es,cx
        mov     di,6[bp]    ; offset address from caller
        mov     dx,986      ; video status port address
loop1:  in      al,dx       ; wait for retrace to quit
        test    al,1
        jnz     loop1
loop2:  in      al,dx       ; wait for retrace to start
        test    al,1
        jz      loop2
        mov     ax,8[bp]    ; word to insert
        stosw               ; insert it
        pop     es
        pop     di
        pop     bp
        ret
_vpoke  endp

        public  _vpeek
; -------- retrieve a word from video memory
;   vpeek(vseg, adr);
;   unsigned vseg;    /* the video segment address */
;   unsigned adr;     /* the video offset address  */
; ------------------------------------------
_vpeek  proc
        push    bp
        mov     bp,sp
        push    si
        push    ds
        mov     si,6[bp]    ; offset address
        mov     cx,4[bp]    ; video board base address
        mov     ds,cx
        mov     dx,986      ; video status port address
loop3:  in      al,dx       ; wait for retrace to stop
        test    al,1
        jnz     loop3
loop4:  in      al,dx       ; wait for retrace to start
        test    al,1
        jz      loop4
        lodsw               ; get the word
        pop     ds
        pop     si
        pop     bp
        ret
_vpeek  endp
        end
```

## LISTING 4

```sh
/*TWRP.MAK -- make file for TWRP.EXE with Microsoft C/MASM*/

.c.obj:
    cl /DCOMPILER=MSOFT -c -W3 -Gs -AC $*.c

twrp.obj : twrp.c editor.h help.h window.h

editshel.obj : editshel.c editor.h menu.h entry.h help.h \
                window.h microsft.h

editor.obj : editor.c editor.h window.h microsft.h

entry.obj : entry.c entry.h window.h microsft.h

menu.obj : menu.c menu.h window.h microsft.h

help.obj : help.c help.h window.h microsft.h

window.obj : window.c window.h microsft.h

microsft.obj : microsft.c

vpeek.obj : vpeek.asm
    masm /MX vpeek;

twrp.exe : twrp.obj editshel.obj editor.obj entry.obj menu.obj help.obj \
        window.obj microsft.obj vpeek.obj
    cl twrp editshel editor entry menu help window microsft vpeek
```
