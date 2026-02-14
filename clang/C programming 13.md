
# C programiranje 13

## OPP levo, FOP desno i pogled iz centra

Al Stevens, avgust '89

Ispostavilo se da sam zloupotrebio funkciju jezika C, čiju snagu sam tek sada shvatio. Karakteristika je `typedef` izjava. Hajde da ispitamo kako je došlo do mog manjeg prestupa gledajući kako se tipedef može koristiti i kako ga treba koristiti.

Pretpostavimo da imate strukturu poput ove:

```c
struct empl_rcd {
   int emplno;
   char emplname[25];
   unsigned date_hired;
   int empl_category;
   long salary;
};
```

Možete definisati potpuno novi tip podataka za ovu strukturu pomoću naredbe `typedef`. Takva je proširiva priroda C. Funkcioniše ovako:

```c
typedef struct empl_rcd EMPLOYEE;
```

Sve deklaracije strukture sada mogu da koriste novi tip podataka EMPLOYEE umesto tipa "struct empl_rcd". Ovo su primeri kako možete deklarisati instance ovog tipa podataka.

```c
EMPLOYEE newhire;
EMPLOYEE *retired;
EMPLOYEE chiefs[5];
```

Zašto ovo radiš? Naknadno upućivanje na strukture ne može ništa dobiti od upotrebe `typedef`-a. U praksi, gledajući kod koji je pristupio jednoj od ovih struktura, ne biste mogli reći da je tip podataka EMPLOYEE uopšte definisan. Pogledajte ove izraze.

```c
printf(newhire.emplname);
total_payroll += chiefs[i].salary;
return retired->empl_category;
```

Ni u jednom od ovih slučajeva kod nije izolovan od formata strukture podataka. U svim slučajevima, kod je potpuno isti kao da tipedef nije korišćen. Najjača prednost `typedef`-a, njegova sposobnost da sakrije informacije, nije ostvarena ovom njegovom upotrebom. Ako nijedan drugi softver u vašem sistemu ne mora da zna da postoje EMPLOYEE, `typedef` se gubi. Šta onda dobijate od EMPLOYEE `typedef`-a koji je dat u ovim primerima? Ništa, nažalost, osim ako ne uzmete u obzir smanjene pritiske na tastere uključene u kodiranje imena.

Sada razmislite o C standardnoj ulazno/izlaznoj biblioteci tipa `FILE`. Ovo je `typedef` definisan u `stdio.h` koji identifikuje definiciju datoteke toka. Sama struktura, ako takva postoji, definisana je u `stdio.h`, a njeno ime i format zavise od implementacije. Dizajner standardne C ulazno/izlazne biblioteke može da definiše `FILE` kao bilo šta, od jednostavnog celog broja do same strukture koja definiše fajl. Čini se da je ovo ispravnija upotreba `typedef`-a. Postoji nešto što se zove FILE, a njegovi implementatori znaju njegove unutrašnje stvari. Vi znate samo ono što treba da znate da biste koristili FILE, i nije vas briga da li je to struktura, unija, ceo broj, pokazivač ili šta. Sve što znate je da ga možete koristiti.

U aplikacijskom programu obično radite tri stvari sa tipom podataka FILE. Deklarišete pokazivače na tip, dodeljujete vrednosti pokazivačima pozivanjem bibliotečkih funkcija koje vraćaju adresu FILE-a i prosleđujete `FILE` pokazivače drugim funkcijama. Funkcije biblioteke znaju šta da rade sa pokazivačima jer znaju format tipa podataka FILE. Program koji poziva to ne zna (ili ne mora). Informacije su na taj način skrivene. Možete ga zaviriti i naučiti ako želite, i mogli biste kodirati specifične reference na strukture podataka koje su u osnovi, ali biste sigurno žrtvovali prenosivost, ne samo među kompajlerima, već i između verzija istog kompajlera.

Kako bismo onda mogli da koristimo EMPLOYEE `typedef` na načine koji bi nam bili od koristi? Jedan od načina bi bio da ga definišete u zaglavlju i sklonite sve funkcije vezane za zaposlene u njihovu sopstvenu biblioteku funkcija, slično kao što se koriste `stdio.h` datoteka i standardna biblioteka. Zatim, aplikativni programi koji treba da rade stvari sa zapisima zaposlenih mogu pozvati ove funkcije pomoću pokazivača EMPLOYEE na isti način na koji koristimo pokazivač `FILE` za stream ulaz/izlaz. Funkcije u biblioteci bi morale da budu svesne formata struktura podataka, ali funkcije korišćenja ne bi. Na taj način bi sistemi platnog spiska, osoblja i upravljanja projektima mogli da koriste istu biblioteku funkcija zaposlenih bez potrebe da znaju interne formate i metode kako se čuvaju evidencija zaposlenih. Skoro objektno orijentisan, zar ne?

Kako sam zloupotrebio `typedef`? Prošle jeseni, u biblioteci funkcija prozora koja podržava projekat SMALLCOM, definisao sam tipove podataka MENU i FIELD, a zatim zahtevao od aplikativnih programa, programa koje biste mogli da pišete, da inicijalizuju strukture pod definicijama `typedef`. Ništa nije skriveno. Da biste koristili meni i biblioteku funkcija za unos podataka, morate znati format tih struktura. Promenite format i morate promeniti svoje programe. Bolji pristup bi bio da se obezbede inicijalizacione funkcije ili makroi sa vrednostima podataka za inicijalizaciju kao parametrima. Zatim, kada se osnovne strukture i funkcije promene da bi se prilagodile novim zahtevima, treba pogledati samo aplikativne programe koji se bave promenama.

Zašto moram da krijem informacije od sebe? Znam formate tih struktura, razumem verovatnoću da će se promeniti i implikacije takve promene, a osim toga, inicijalizacije su ionako efikasnije u vreme kompajliranja nego u vreme izvršavanja. (Ovde unesite svoj omiljeni argument da nastavite da radite stvari na način na koji ste ih uvek radili.)

Kompajler mora sakriti detalje tipa podataka `FILE` da bi sačuvao standardni metod za njegovu implementaciju. Ali koliko je dobro skriveno? Pogledajte u `stdio.h` i videćete celu enchiladu. Format `FILE` strukture je tu. Neke od standardnih funkcija biblioteke se proširuju u makroe koji se direktno obraćaju članovima u `FILE` strukturi, i makroi su tu. Koliko su dobro skrivene informacije koje mogu da pronađem u dobro komentarisanom izvornom kodu koristeći svoj uređivač teksta?

Odgovor je, naravno, da je standardna ulazno/izlazna informacija skrovište benigno skladište, benigno po tome što radoznali tragalac može da ga pronađe i pregleda njegov sadržaj bez poteškoća, benigno u tome što samo usputne mere štite njegove tajne od voajera. Ne biste želeli da svakodnevno vodite računa o detaljima odlaganja smeća u vašoj kuhinji ili destinaciji njegove cene, ali znate gde se informacije mogu naći ako su vam zaista potrebne. Isto važi i za informacije skrivene u lako dostupnim spremištima kao što je `stdio.h`.

To nisu nove ideje. O skrivanju informacija se priča i koristi godinama. Ali poslednjih godina se na to gleda u novom svetlu, u onom koji se zove "programiranje orijentisano na objekte", i počeo sam da ponovo razmatram svoju praksu sa stvarima kao što je typedef kada sam počeo da proučavam OOP. Novo svetlo je još uvek prigušeno i moj vid kratkovidan, ali uskoro ćemo se izvući iz oblaka, mislim. Jedno je jasno: OOP vas podstiče da bolje sakrijete informacije koje se mogu sakriti.

## OOPs

Mislim da je Robert Benčli primetio da je svet podeljen na dve vrste ljudi - na one koji dele svet na dve vrste ljudi i na one koji ne dele. Svet programera se deli. Postoje oni koji prihvataju objektno orijentisano programiranje i oni koji ne prihvataju. Pitajmo se zašto.

Rečeno nam je da da bismo naučili OOP moramo da napustimo ono što znamo o konvencionalnom programiranju, da je najveća prepreka učenju neučenje. Mogućnost povratka u osnovnu školu će obeshrabriti mnoge konvencionalne programere koji smatraju da znaju kako da pišu programe sasvim dobro, hvala.

Moj drug u DDJ-u, Kent Porter, predložio je dvosložnu reč za pojam da je OOP potpuno nov i drugačiji, reč koja znači ono što dobijate kada svoj omiljeni slamnati šešir prođete kroz bika. Kent se poigravao sa objektno orijentisanim ekstenzijama koje su dolazile do Turbo Paskala i prešao je ogradu ka zelenijim pašnjacima OOP-a. Kent je bio prestar da bi počeo da uči iznova. I bio je premlad da bi otišao kada je to uradio. nedostajaće mi.

Za mene, sada proučavam C++ i sugerisano je da su ekstenzije Turbo Pascal glatki most od C do C++. Nameravam da istražim tu mogućnost. Moje proročanstvo je da će preovlađujući OOP jezik biti C++, i da će do njegovog potpunog prihvatanja doći kada i ako Borland i Microsoft uvedu C++ kompajlere. Ako je bilo šta ikada vrištilo za OOP podršku, interfejsi programa za DOS Windows i OS/2 Presentation Manager imaju. Njihovim naizgled objektno orijentisanim arhitekturama svakako je potreban C++ sa srodnim bibliotekama objekata iza sebe.

Težim C++ jer je on nadskup C-a i uvek se može povući na C kada se OOP pogled ne uklapa. Neki OOP puristi sugerišu da C++ zapravo nije OOP. Možda, možda ne, i možda se paradigma nije ustalila, ali ipak se kladim da je C++ talas sutrašnjice ako će doći do novog talasa.

Da li će C++ zameniti C kao primarni razvojni jezik ili ne zavisi od toga koliko je podstaknut pokretnim silama u razvoju jezika i šta radi za programera. Naravno, ako Borland i Microsoft stanu iza toga, mnogi programeri će pokušati. Ali programeri se neće držati C++ osim ako nema razloga za to.

Vrednost jezika merimo njegovom sposobnošću da izrazi složeni algoritam sa nekoliko linija koda na način koji se može pročitati i jasno razumeti. Većina programera je prihvatila C jer je mogao da radi te stvari a da se ne udalji previše od hardvera i bez strogog sprovođenja bilo čega. Da bi osvojio C programere, C++ mora da pokrene stvari sa manje i lakših linija koda, da napravi programe koje je lakše menjati ili oboje. Sve dok je kod kasnije čitljiv, čemu kod dva kada će jedan? C++ mora da poboljša našu sposobnost pisanja programa, ili nije neophodan.

Dakle, šta je drugačije u vezi sa OOP-om? Možda je to što OOP pogled na programski problem ima drugačiju perspektivu od one u kojoj sada uživamo. Ali da li je to zaista tako?

OOP pogled gleda na strukture podataka i funkcije specifične za te strukture. Opisujete klase podataka, objekte koji su instance tih klasa i metode za obradu objekata. Zatim, da bi se stvari desile, šaljete poruku objektu i stvari se dešavaju. Uzmite u obzir da stari kesten, "hello.c", u konvencionalnom C.

```c
/* ------ hello.c in C ------ */
#include <stdio.h>
main( )
{
     printf("hello, world\n") ;
}
```

Ovaj program je orijentisan na funkciju koju obavlja. To je program orijentisan na funkciju (FOP). FOP programer razmišlja o rešenjima u smislu procedure, funkcija, koraka koji se preduzimaju da bi se nešto rešilo.

Sada razmotrite isti program u C++

```cpp
\\ ------ hello.c in C++
#include <stream.h >>
main( )
{
cout << "hello, world \ n";
}
```

Rečeno nam je da je ovaj program orijentisan na svoje objekte, strukture podataka koje podržava i metode koje koristi da ih podrži.

Izgledaju slično, zar ne? U konvencionalnom C-u ste pozvali funkciju `printf`, prosleđujući joj adresu stringa za prikaz. Datoteka zaglavlja `stdio.h` je obezbedila prototip za funkciju `printf`. U C++ šaljete poruku `cout`, što je objekat klase `ostream` kako je definisano u `stream.h`. Poruka se sastoji od adrese stringa. Operator `<<`, takođe definisan u `stream.h` kao deo klase, je metod koji će objekat koristiti.

Operator `<<` nije sam po sebi sastavni deo C++, kao što je funkcija printf sastavni deo C jezika. Operator `<<` je povezan sa klasom `ostream` unutar `stream.h`. Možete da povežete prilagođene operatore sa klasama u C++. Operator veće od, na primer, može značiti različite stvari različitim klasama. Štaviše, može značiti različite stvari u istoj klasi kada se uzme u kontekstu tipova onoga na čemu radi.

Hajde sada da napišemo "zdravo, svet" poruku u datoteku. Prvo ćemo koristiti konvencionalni C:

```c
#include <stdio.h>
main()
{
    FILE *fp;
    fp = fopen("hello.dat", "w");
    fputs("hello, world\n", fp);
    fclose(fp);
}
```

U standardnom FOP C pozivate tri funkcije. Prvi otvara datoteku i vraća pokazivač na `FILE`. Druga funkcija prihvata adresu podataka i `FILE` pokazivač i šalje niz podataka u datoteku. Treća funkcija zatvara datoteku.

Sada pogledajmo istu operaciju sa C++:

```cpp
#include <stream.h>
main()
{
    filebuf bf;
    bf.open("hello.dat", output);
    ostream op(&bf);
    op.put("hello, world\n");
}
```

U C++ OOP prikazu, deklarišete objekat klase `filebuf` pod nazivom "bf". Ovaj objekat je samo bafer podataka. Zatim povezujete bafer sa `stream` datotekom sa funkcijom `filebuf::open`. Drugi način da se to kaže je da šaljete adresu stringa i izlaznu zastavicu kao poruke "bf" objektu, govoreći mu da koristi svoj `open` metod. Zatim deklarišete objekat tipa `ostream` pod nazivom `op` sa `filebuf`-om kao inicijalizujućim argumentom. Konačno šaljete string poruku "op" objektu govoreći mu da koristi svoj `put` metod.

Na ovom nivou, ova dva pogleda su i dalje slična. C programer bi mogao da pređe na C++ sa malim problemom ako je ovo bilo do kraja. U stvari, mogli biste se zapitati, zašto se mučiti? Čini se da je u pitanju promena sintakse, ništa više. Velika razlika je, međutim, u onome što je skriveno. Definicije klasa `filebuf` i `ostream` značajno se razlikuju od njihovih `FILE` parnjaka u standardu C.

"Pa šta?" možete pitati. "Razlike su skrivene. Koga briga koliko su različite?"

Vratite se na defekciju tipa EMPLOYEE. To je deo aplikacije, vaša je odgovornost, i, ako želite da bude objekat, potrebna mu je klasa, skrivene strukture podataka članova i funkcije metoda javnih članova. Da biste dizajnirali ove stvari, morate biti objektno orijentisani kao i jezik. Ako OOP ima šta da ponudi, očigledno drugačija sintaksa iskaza nije to. Mora postojati zasluga u zauzimanju te drugačije perspektive.

Jedan od problema sa kojima ćemo se suočiti pri pisanju naših prvih C++ programa je odlučivanje šta bi trebalo da budu klase i objekti, a šta ne. Kada šaljemo poruke, a kada pozivamo funkcije? Koji je najprikladniji način slanja poruka? Pretpostavimo da vaš program postavlja uslove greške tako što otvara prozor i prikazuje poruku. Da li procesor grešaka treba da bude staromodna C funkcija poput ove?

```c
error("Too many Chiefs");
```

Ili bi procesor grešaka trebao biti objekat klase izlaza poput ove?

```cpp
class error: public window {
 . . .
};
error errs;
errs.post("Too many Chiefs");
```

Da li procesor grešaka treba da koristi funkciju člana kao što je upravo prikazano, ili da koristi sopstveni operator za objavljivanje ovakvih grešaka?

```cpp
errs << "Too many Chiefs";
```

Možete objaviti greške sa funkcijama konstruktora i destruktora klase grešaka. Funkcija konstruktora se poziva kada je objekat deklarisan, a funkcija destruktora se poziva kada objekat izađe van opsega.

```c
if (chiefs > MAXCHIEFS) {
   error errs("Too many chiefs");
}
```

Ove OOP stvari počinju da imaju smisla. Kod u ovoj koloni možda ima nekoliko grešaka, ali ja upravo učim C++. U narednih nekoliko meseci istražiću ga detaljnije i zajedno ćemo učiti.

Da li su ljudi objektno orijentisani?

Da bi programska paradigma zaživela, ona mora da funkcioniše kako ljudi misle. Rešenje problema treba da liči na problem. Da li smo objektno orijentisana stvorenja? Mislim da bi mogli biti. Ljudi znaju da rade mnoge složene stvari, ali obično samo u kontekstu uključenih objekata. Vodoinstalaterske metode imaju značenje samo kada se posmatraju u kontekstu lavaboa, kada i slično. Oduzeti razumevanje objekata i metoda bi bila besmislica.

Dakle, šta je važnije i zaslužuje više pažnje, predmet ili metode, kada ili vodoinstalaterski alati i procedure? Gde bi trebalo da bude perspektiva? A šta treba sačuvati?

Sa tradicionalnim FOP-om, čuvate funkcije opšte namene koje se mogu ponovo koristiti u bibliotekama, a strukture podataka aplikacija čuvate u datotekama zaglavlja. Njih dvoje nisu povezani osim u kontekstu programa koji ih povezuju. Funkcije specifične za aplikacije se uglavnom ne zadržavaju za ponovnu upotrebu. U OOP-u možete katalogizirati stvari za višekratnu upotrebu kao i kod FOP-a, ali takođe blisko povezujete funkcije orijentisane na podatke sa strukturama podataka kao objektima, i možete sačuvati ove objekte za ponovnu upotrebu ako je potrebno.

Dakle, šta bi trebalo da bude u biblioteci, funkcije ili objekti? Šta bi trebalo da bude dizajnirano za višekratnu upotrebu? Očigledno, samo one stvari za koje postoji verovatnoća da će se ponovo koristiti treba da budu tako dizajnirane, a ponekad to ide jednom, a ponekad drugom. Bez znanja boljeg, C++ me privlači zbog njegovog podskupa ANSI C. Možete ići na bilo koji način čak i unutar istog programa. Samo će vreme pokazati da li je to dobra ideja.

Moj drug Bill Chanei mi kaže da ponovo razmislim; FOP je dendi, a OOP znači da je napravljena greška.

## Izveštaj o knjizi: Programski jezik C++

Programski jezik C++ Bjarnea Stroustrupa mnogo se trudi da bude K&R knjiga C++-a. Podseća na „belu knjigu“. Ima istu veličinu, isti stil i prezentaciju i (skoro) isti naslov. Ali K&R je dobro napravljen opis C-a namenjen programerima koji još ne znaju jezik. Dok čitate K&R, pažljivo se vodite kroz karakteristike jezika sa dobro organizovanim primerima i dobro osmišljenom sekvencom za prezentaciju informacija. Knjiga C++ ne odgovara ovim visokim standardima.

Ako će nešto značiti propast C++-a ili OOP-a uopšte, to je da je jezik i paradigmu teško opisati ili se čini da je teško opisati. Programeri ne prihvataju lako nešto što stručnjaci ne mogu da objasne. Možda i nije tako teško; možda ih još niko nije tačno opisao. Čućete da mnogi programeri imaju poteškoća u prelasku sa C na C++. Možda to pokušavaju da urade čitajući ovu knjigu. Verujte mi, Stroustrupova knjiga nije mesto za početak.

Kada prođe kratak opis podskupa C, knjiga zaroni u detalje koji su previše duboki za pridošlicu. Na primer, uvodi klase sa diskusijom o tome kako se `ostream` implementira bez objašnjenja šta je `ostream`. Zatim tone u tajanstveni opis kako se preopterećenje operatora koristi za implementaciju izraza kao što je `A << B << C` mnogo pre nego što u potpunosti objasni kako možete proširiti jezik pomoću operatora za objekte. Ova knjiga pati od bolesti koju ima većina tretmana OOP - nejasnih primera. Većina programera neće se baviti implementacijom strim ulaznih/izlaznih klasa i objekata jer ne rade takvu vrstu programiranja. Većina programera se neće povezivati sa primerima koji koriste kompleksne brojeve, jer ih koristi relativno mali broj programskih problema u stvarnom svetu. Knjiga preimenuje C-ove nizove, nazivajući ih vektorima, a to nikada ne objašnjava. Zatim, da vas dodatno zbuni, diskusija o vektorima čini da mislite da postoji vektorska klasa stvaranjem jedne. Tek kasnije saznate da vam je rečeno kako da koristite karakteristike C++-a za proveru sopstvenih vektorskih granica i da uopšte ne koristite C++-ove neograničene vektore. Cela tema operatora koristi implementaciju poboljšanih vektora kao primer. Većina programera ne bi prihvatila ove primere čak i da su primeri dobro organizovani i predstavljeni jer većina programera ne piše sistemske programe u kojima se takve tehnike primenjuju. Pristup koji koristi ova knjiga podseća na tipičan univerzitetski nastavni plan i program računarskih nauka, gde je naglasak na dizajnu jezika i razvoju kompajlera, a ne na tome kako i zašto se koriste.

To je otprilike onoliko koliko sam stigao sa programskim jezikom C++. Tražim drugu knjigu.

Moj prvi utisak o C preuzet je iz K&R-a. Organizacija i jasnoća tog rada izazvali su poverenje u jezik. Bilo je lako poverovati da će jezik ispuniti svoje obećanje; Denis Riči, jedan od koautora knjige, dizajnirao je jezik.

Ako je ista konfuzija i nedostatak organizacije koja se nalazi u C++ "definitivnoj referenci i vodiču" u jeziku (C++ je dizajnirao Stroustrup), mi smo u dubokom sušiju. Prvi utisci teško umiru, ali nadam se da grešim. I dalje verujem da će C++ biti sledeći glavni programski jezik.

Kako se rad ANSI X3J11 komiteta završava, oni razmatraju standardizaciju C++-a. Ako ovaj poduhvat krene, to je siguran znak da će C++ uspostaviti i održati snažno prisustvo dugo vremena.
