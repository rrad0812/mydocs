
# Jam.py veb aplikacioni radni okvir

Po definiciji, veb okvir je biblioteka koda koja čini razvoj veb aplikacija bržim i lakšim, pružajući zajedničke pristupe za izgradnju dostupnih, skalabilnih i jednostavnih za održavanje aplikacija. Više od 15 godina, profesionalni razvoj veba uvek je uz upotrebu veb okvira, osim u izuzetno neobičnim situacijama.

## O Jam.py veb radnom okviru

Pravo osveženje dolazi sa severoistoka evropskog kontinenta. Novi radni okvir, nove ideje.

Jam.py je klijent-server, dogadjajima vodjen, monolitni okvir koji je dizajniran za kreiranje veb poslovnih aplikacija.

To je prelepa kombinacija HTML-a, Java Script, JQuery, CSS i Python koda.I to je sve.

Stvaranje aplikacija započinje i završava u Graditelju Aplikacije, koji nije ništa drugo do veb aplikacije napisano u Jam.py radnom okviru i koja radi na istom serveru kao željena aplikacija.

Ovde programer kreira novu šemu baze podataka ili koristi postojeću, stvara forme, izveštaje, menije i druge detalje u aplikaciji.

Jam.py podržava sve dobro poznate baze podataka otvorenih izvora: SQLite, MySQL, PostgreSQL, Firebird i vlasničke MS SQL Server i Oracle RDBMS-ove.

Jam.py ima ugrađenu autentifikaciju i autorizaciju kao i upravljanje podacima u sesiji.

Jam.py podržava višejezičnost i ima mogućnost proširenja na nove jezike.

Jam.py ima organizaciju koda u skladu sa principom vođenim događajima.Postoji snažna literatura koja objašnjava prednosti MVC načina organizovanja koda. Autor ovog teksta ne deli takvo mišljenje, ako znate bilo šta o tome kako funkcioniše sistem vodjen događajima, lako ćete upravljati i odmah započeti avanturu sa Jam.py.

Apsolutna novost je organizovati koda u hijerarhijskom obliku, gde se može definisati funkcionalnost na različitim, unapred definisanim nivoima. Na ovaj način autor okvira uspešno definiše unapred definisanu funkcionalnost na osnovnom nivou - nivoa taska a na korisniku je da promeni predloženu funkcionalnost na nivou grupe objekata ili na nivou objekta.

Grupa objekata prikuplja predmete slične po nekim od osnovnih karakteristika, tako da postoje journals, catalogs, reports i details. Postojanje grupe pruža programeru mogućnost da definiše zajednički kod na nivou grupe.

Journals okupljaju objekte koji sadrže transakcione podatke, catalogs su objekti koji sadrže retko promenljive podatke i definišu osnovne sistemske činioce, reports objekti su predefinisani spredsheet šabloni u koje Jam.py mapira podatke i konvertuje u željeni korisnički format.

Da bi sve ispravno uradilo, potrebno je instalirati spreadsheet softver na serveru sa odgovarajućim mogućnostima konverzije.

Grupa detalja izgleda malo misteriozno - to su objekti koji dolaze sa unapred određenim svojstvima potrebnim za pridruživanje glavnim objektima iz journals i catalogs grupa.

Još jedna retko vidjena funkcija je mogućnost pridruživanja jednog objekta detalja na nekoliko glavnih objekata i obrnuto.

Objekti nisu ništa drugo nego korisnički interfejs i prezentacija podataka, najbliže odgovaraju akcionim upitima nekih drugih poznatih sistema.

Glavni nosač objekta, u zavisnosti od njegovih osobina, može biti tabela u bazi podataka. Najvažniji delovi objekta, naravno, polja sa svim potrebnim vrstama podataka i standardnim alatom za programiranje - lookup polja.

Takođe postoje lookup-ovi preko SQL JOIN-a, lookup looku-a, itd. Jam.py gradi kompletnu aplikaciju koja pokreće i upravlja podacima bez potrebe za pisanjem i jedne jedine linije koda.

I šta programer radi ovde? Kod je potreban za složenije validacije, za različite upravljanje podacima tokom ulaska ili pisanja podataka u bazu podataka i na mnogim drugim mestima.

Kod je napisan u editoru koji je deo graditelja. Editor radi kroz odvojene module za deo klijenta i servera.

Jam.py nije generator koda. Programer gradi aplikaciju, a okvir unosi izbor programera i podešavanja u SQLite bazu podataka projekta. Kada korisnik pristupi Jam.py aplikaciji, Jam.py učitava podešavanja iz SQLite baze podataka projekta i aplikacija ulazi u svoj radni ciklus.

## Dobre strane Jam.py Okvir

Standardni `pip install jam.py` i `python setup.py install` instalacioni modovi.

Krivulja učenja je veoma blaga zbog prisustva onlajn Graditelja Aplikacije i odlične, sveobuhvatne dokumentacije sa mnoštvom grafike i studija slučaja.

Programer je posvećen samo radu sa poslovnim pravilima. Nema detalja vezanih za HTTP protokol, usmjeravanje i slične stvari koje se ne treba očekivati od veb biznis programera.

Iz HTML šablona, ovo je jedna od najčešćih HTML datoteka, čiju struktura možete upoznati za 15 minuta.

Odsustvo potrebe za konfiguracijom sistema.

Prisustvo svih klasičnih elemenata veb aplikacija, poput šifrovanja, sesija, autentifikacije, autorizacije, menadžmenta statičkog sadržaja, WSGI dev servera sa ugrađenim debagerom.

Sposobnost pokretanja na Pithon2.7 i Pithon 3.5, Pithon 3.6 i Pithon 3.7 WSGI na veb servere.

Import i eksport aplikacije na/sa druge servere i druge baze podataka.

Onlajn održavanje sa ugrađenim Graditeljem i editorima.

## Loše strane Jam.py Okvir

Iako Jam.py ima Python u imenu, to je pre svega veb okvir Java skripta koji koristi PYthon za pozadinu. Korisnici se mogu iznenaditi činjenicom da se puno posla završi na strani Java Skripta klijenta.

Ne postoji dodatni kod u obliku dodataka ili umetak, zbog jednostavne činjenice da je okvir i dalje vrlo mlad i nepoznat i sa malim brojem sledbenika. Na ovaj način razvoj je potpuno zavisan od autora.

Kompletan unapred definisan deo koda i objekata koji se zahtevaju tokom razvoja i u proizvodnji je definisan u obliku SQLite baze podataka. Stoga je standardni postupak za verzionisaje koda gotovo nemoguć, jer poznati SVC sistemi slabo upravljaju binarnim formatima.

## Zaključak

Autor ovog teksta ima dugogodišnje iskustvo u pokušaju da pronađe dobar veb okvir. Shodno tome, ideja sa grafičkim putem onlajn graditelja i editora nije bila strana, kao i osnovna teorija relacionih sistema baze podataka.

Stoga je početak sa okvirom bio veoma lagan, okvir je omogućio izgradnju kompletne aplikacije sa svim pratećim elementima u stvarno kratkom roku.

Ali čim je postojala potreba za pisanjem koda, stvari su se promenile. Pre svega, potreba za Java skriptom je neizbežna.

Pored toga, hijerarhijski način obrade događaja u stablu aplikacije je nešto zaista neobično i treba da bude prihvaćeno.

Slično je za kod servera, iako ga je znatno manje nego za klijent i njegovo pisanje je usmereno na hijerarhijsku strukturu okvira.

Međutim, količina koda potrebnog za dobijanje rezultata mnogo je manja u poređenju sa MVC radnim okvirima.

Poslovica kaže da je se na dobre stvari lako naviknuti.To je slučaj sa Jam.py. Na tržištu ne postoji nijedan proizvod koji omogućava bržu i lakšu izgradnju veb aplikacije. Zbog njegovog kapaciteta i fokusa, okvir ima za cilj razvije aplikacija preduzeća.

Naravno, potrebno je vreme i posvećenost autora i zajednice stvorena oko ovog projekta da promoviše i širi ideje Jam.py koje nam on donosi.
