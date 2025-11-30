
# Kako instalirati i koristiti PostgreSQL na Ubuntu

## Uvod

Sistemi za upravljanje bazama podataka su ključna komponenta mnogih veb lokacija i aplikacija, pružajući strukturiran način SQLadištenja, organizovanja i pristupa informacijama. PostgreSQL, poznatiji kao Postgres, je vodeći sistem za upravljanje relacionim bazama podataka otvorenog koda. Pruža potpunu implementaciju SQL jezika za upite i dobro je cijenjen zbog uSQLađenosti sa standardima i naprednih funkcija, kao što su pouzdane transakcije i konkurentnost bez zaključavanja čitanja.

Ovaj vodič demonstrira kompletan proces podešavanja PostgreSQL-a na Ubuntu serveru, počevši od instalacije potrebnih softverskih paketa. Zatim ćete naučiti kako da stupite u interakciju sa Postgres sistemom za autentifikaciju, kojim se upravlja preko "role“, uključujući kreiranje novih korisnika i baza podataka. Nakon toga ćemo pokriti osnove upravljanja bazom podataka: povezivanje sa vašom novom bazom podataka, definisanje strukture podataka kreiranjem tabela i manipulisanje podacima pomoću osnovnih SQL komandi. Takođe ćemo istražiti ključne administrativne teme, uključujući suštinsku konfiguraciju za performanse, strategije za pravljenje rezervnih kopija i vraćanje, poređenje PostgreSQL-a sa drugim mašinama baze podataka i vodič za rešavanje uobičajenih problema.

Ključni za poneti:

- PostgreSQL se može instalirati na Ubuntu koristeći podrazumevana
spremišta i menadžer paketa `apt`. Ovo obezbeđuje stabilnu i bezbednu verziju servera baze podataka i njegovih klijentskih alata.

- PostgreSQL upravlja dozvolama koristeći koncept `role`, koje mogu
predstavljati korisnike ili grupe. Ovaj sistem kontroliše ko može da se prijavi, kojim bazama može da pristupi i koje operacije sme da obavlja.

- Podrazumevana rola `postgres` se kreira tokom instalacije sa punim
`superuser` privilegijama i obično se koristi za početnu administraciju i podešavanje drugih rola i baza podataka.

- Podrazumevano, Postgres koristi `peer` autentifikaciju za lokalne
veze, koja daje pristup roli baze ako ima isto ime kao aktivni korisnik Linux sistema.

- Nove role i baze možete kreirati iz komandne linije pomoću alatki `createuser` i `createdb`, koje se obično pokreću pod Linux korisnikom `postgres` radi potrebnih dozvola.

- `psql` je primarni terminal za rad sa PostgreSQL-om; koristi se za povezivanje na određenu bazu kao određeni korisnik i izvršavanje SQL upita.

- Iako je `peer` autentifikacija podrazumevana, PostgreSQL možete podesiti da zahteva lozinke za lokalne ili udaljene veze — kreiranjem role sa `password` i ažuriranjem `pg_hba.conf`.

- U članku je detaljno opisano upravljanje strukturom podataka kroz kreiranje i izmenu tabela, kao i osnovne SQL komande za manipulaciju (INSERT za dodavanje podataka i SELECT za preuzimanje).

- Osim osnova, vodič obuhvata važne administrativne teme: podešavanje performansi servera, strategije bekapa i vraćanja, kao i metode za rešavanje uobičajenih problema.

## Preduslovi

**Kompatibilnost verzija** : Verifikovano je da ovaj vodič radi na Ubuntu 20.04, 22.04, 24.04 i 25.04. Ako koristite novije izdanje, ovi koraci ostaju važeći za većinu verzija Ubuntua. Ako se verzije paketa ili konfiguracione putanje razlikuju na vašem sistemu, pratite isti tok posla i konsultujte napomene o izdanju za vaše specifične verzije Ubuntu-a i PostgreSQL-a.

Da biste pratili ovaj vodič, biće vam potreban jedan Ubuntu server koji je konfigurisan prateći naš vodič "Početno podešavanje servera za Ubuntu“. Nakon što završite ovaj preduslovni vodič, vaš server bi trebalo da ima `no-root` korisnika sa `sudo` dozvolama i osnovnim `firevall-om`.

## Instaliranje i korišćenje PostgreSQL-a

Da biste instalirali PostgreSQL, prvo osvežite lokalni indeks paketa vašeg servera:

```sh
sudo apt update
```

Zatim instalirajte postgres paket zajedno sa `-contrib` paketom koji dodaje neke dodatne uslužne programe i funkcionalnost:

```sh
sudo apt install postgresql postgresql-contrib
```

Možete proveriti verziju tako što ćete pokrenuti sledeću komandu:

```sh
psql --version
```

Uverite se da je usluga pokrenuta:

```sh
sudo systemctl start postgresql.service
```

### Korišćenje PostgreSQL uloga i baza podataka

Postgres podrazumevano koristi koncept koji se zove `role` za rukovanje autentifikacijom i autorizacijom. Oni su, na neki način, slični korisnicima i grupama u stilu Unix-a.

Nakon instalacije, Postgres je podešen da koristi `peer` autentifikaciju, što znači da povezuje `postgres` ulogu sa odgovarajućim Unix/Linux sistemskim nalogom. Ako uloga postoji u Postgresu, Unix/Linux korisničko ime sa istim imenom može da se prijavi kao ta uloga.

Procedura instalacije kreirala je korisnički nalog pod nazivom `postgres` koji je povezan sa podrazumevanom ulogom Postgres. Postoji nekoliko načina da koristite ovaj nalog za pristup Postgresu:

- Jedan od načina je da pređete na `postgres` nalog na vašem serveru
tako što ćete pokrenuti sledeće komanda:

  ```sh
  sudo -i -u postgres
  ```
  
  Zatim možete pristupiti Postgres promptu pokretanjem:
  
  ```sh
  psql
  ```
  
  Ovo će vas prijaviti u PostgreSQL prompt i od tog trenutka možete odmah da radite sa sistemom za upravljanje bazom.
  
  Da izađete iz PostgreSQL prompta, pokrenite sledeće:
  
  ```sh
  \q
  ```

  Ovo će vas vratiti na postgres Linux komandni prompt. Da se vratite
na vaš uobičajeni korisnički nalog sistema, pokrenite komandu exit:
  
  ```sh
  exit
  ```

- Drugi način da se povežete na Postgres prompt je da pokrenete `psql`
komandu kao `postgres` nalog direktno pomoću sudo:

  ```sh
  sudo -u postgres psql
  ```
  
  Ovo će vas prijaviti direktno u Postgres bez međufaze bash školjke.
  
  Ponovo, možete izaći iz interaktivne Postgres sesije pokretanjem:
  
  ```sh
  \q
  ```

### Kreiranje nove role

- Ako ste prijavljeni kao nalog `postgres`, novu rolu možete kreirati
pokretanjem sledeće komande:

  ```sql
  createuser --interactive
  ```

- Ako umesto toga želite da koristite `sudo` za svaku komandu bez
prebacivanja sa svog uobičajenog naloga, pokrenite:

  ```sh
  sudo -u postgres createuser --interactive
  ```

- U oba slučaja skripta će vam postaviti nekoliko izbora i na osnovu
vaših odgovora izvršiće odgovarajuće Postgres komande da kreira korisnika po vašim specifikacijama.

  ```sh
  Enter name of role to add: sammy
  Shall the new role be a superuser? (y/n) y
  ```

### Kreiranje nove baze podataka

Još jedna podrazumevana pretpostavka u Postgres autentifikaciji je da svaka rola kojom se prijavljujete ima bazu podataka sa istim imenom kojoj može da pristupi.

To znači da ako je korisnik koga ste kreirali u prethodnom odeljku nazvan "sammy", ta rola će podrazumevano pokušati da se poveže na bazu koja se takođe zove "sammy". Odgovarajuću bazu možete napraviti komandom `createdb`.

- Ako ste prijavljeni kao `postgres` nalog, ukucaćete nešto ovako:

  ```sh
  createdb sammy
  ```

- Ako više volite da koristite `sudo` za svaku komandu bez prebacivanja
sa svog uobičajenog naloga, pokrenućete:

  ```SH
  sudo -u postgres createdb sammy
  ```

### SQL metoda za kreiranje novog korisnika i baze (opciono)

`createuser` i `createdb` komandne alatke su zgodni pomoćnici. Međutim, za više kontrole možete iste radnje izvršiti direktno u PostgreSQL-u pomoću SQL komandi. Ovaj pristup je često jasniji kada odmah postavljate lozinke ili dodeljujete specifične privilegije.

- **Povežite se** kao administratorski **postgres** korisnik:

  ```sh
  sudo -u postgres psql
  ```

  Kada ste u PostgreSQL promptu možete koristiti CREATE ROLE i CREATE DATABASE da podesite novog korisnika i bazu.

- **Kreirajte novu rolu (korisnika)**: Dok je `createuser` interaktivna
komandna alatka, CREATE ROLE vam omogućava da sve definišete jednom naredbom. Da kreirate korisnika po imenu "sammy" koji može da se prijavi (LOGIN) i ima lozinku, pokrenite:

  ```sql
  CREATE ROLE sammy WITH LOGIN PASSWORD 'your_strong_password';
  ```

  Ako ovom korisniku treba i pravo da kreira baze, možete ga dodeliti
istovremeno:
  
  ```sql
  CREATE ROLE sammy WITH LOGIN PASSWORD 'your_strong_password' CREATEDB;
  ```

  Ovo daje podskup superuser privilegija, konkretno omogućava korisniku da kreira nove baze. Dok odgovor "yes" na pitanje o superuser statusu u interaktivnom pomoćniku takođe daje ovu dozvolu, SUPERUSER je mnogo šira i rizičnija rola.

- **Kreirajte novu bazu**: Zatim napravite bazu. Dobra praksa je da
vlasništvo nove baze dodelite ulozi koju ste upravo kreirali.

  ```sql
  CREATE DATABASE sammydb OWNER sammy;
  ```

  Da dodelite korisniku sammy dozvolu da se poveže na novu bazu (čest
sledeći korak), možete pokrenuti:

  ```sql
  GRANT ALL PRIVILEGES ON DATABASE sammydb TO sammy;
  ```

  Ovo dodeljuje privilegije na nivou baze kao CONNECT, ali ne i dozvole na specifičnim tabelama.

  > [!Napomena]  
  Ovo automatski ne dodeljuje dozvole za pregled ili izmenu tabela u bazi; potrebne su dodatne GRANT komande nad šemom (npr. public) i samim tabelama.

- **Izlaz iz postgres sesije**: Sada možete napustiti psql prompt za postgres korisnika.

  ```sh
  \q
  ```

  Pošto ste postavili lozinku, ovaj novi korisnik može da se
autentifikuje metodama koje nisu `ident`, što je neophodno za udaljene konekcije.

Kada ste u `postgres` promptu možete koristiti CREATE ROLE i CREATE DATABASE da podesite novog korisnika i bazu.

- **Kreirajte novu rolu (korisnika)**: Dok je `createuser` interaktivna
  alatka, CREATE ROLE vam dozvoljava da sve definišete u jednoj naredbi. Da kreirate korisnika "sammy" koji može da se prijavi (LOGIN) i ima lozinku, pokrenite:

  ```sql
  CREATE ROLE sammy WITH LOGIN PASSWORD 'your_strong_password';
  ```
  
  Ako korisniku treba i dozvola za kreiranje baza, možete je dodeliti u istoj naredbi:
  
  ```sql
  CREATE ROLE sammy WITH LOGIN PASSWORD 'your_strong_password' CREATEDB;
  ```

  Ovo daje podskup superuser privilegija dozvoljavajući kreiranje novih baza. SUPERUSER je ipak šira i rizičnija rola.

- **Kreirajte novu bazu**: Napravite bazu i dodelite vlasništvo novoj
  roli koju ste kreirali.

  ```sql
  CREATE DATABASE sammydb OWNER sammy;
  ```

  Da dodelite korisniku sammy dozvolu da se poveže na novu bazu,
pokrenite:
  
  ```sql
  GRANT ALL PRIVILEGES ON DATABASE sammydb TO sammy;
  ```

Ovo dodeljuje privilegije na nivou baze kao CONNECT, ali ne i dozvole nad konkretnim tabelama. Ne daje automatski dozvole za pregled ili izmenu tabela; potrebne su dodatne GRANT komande nad šemom (npr. public) i tabelama.

- **Izlaz iz postgres sesije**: Možete izaći iz `psql` prompta za postgres korisnika.

```sh
\q
```

### Otvaranje postgres prompta sa novom rolom

Da biste se prijavili koristeći ident (peer) autentifikaciju, potreban vam je Linux korisnik sa istim imenom kao Postgres rola i baza.

Ako nemate odgovarajući Linux korisnički nalog, možete ga kreirati komandom `adduser`. Ovo radite sa svog `non-root` naloga koji ima `sudo` privilegije (dakle ne kao postgres korisnik):

```sh
sudo adduser sammy
```

Kada je novi nalog spreman, možete se prebaciti na njega i povezati na bazu pokretanjem:

```sh
sudo -i -u sammy
psql
```

Ili možete to uraditi inline:

```sh
sudo -u sammy psql
```

Ova komanda će vas automatski prijaviti pod pretpostavkom da je sve ispravno konfigurisano.

Ako želite da se korisnik poveže na drugu bazu, možete to uraditi navođenjem baze ovako:

```sh
psql -d postgres
```

Kada ste prijavljeni, trenutne informacije o konekciji možete proveriti komandom:

```sql
\conninfo
```

```sql
You are connected to database "sammy" as user "sammy" via socket in "/var/run/postgresql" at port "5432".
```

Ovo je korisno ako se povezujete na ne-podrazumevane baze ili sa ne-podrazumevanim korisnicima.

### Kreiranje i brisanje tabela

Sada kada znate kako da se povežete na PostgreSQL sistem, možete naučiti neke osnovne zadatke upravljanja Postgres-om.

Osnovna sintaksa za kreiranje tabela izgleda ovako:

```sql
CREATE TABLE table_name (
    column_name1 col_type (field_length) column_constraints,
    column_name2 col_type (field_length),
    column_name3 col_type (field_length)
);
```

Kao što vidite, ove komande daju `tabeli` `naziv`, a zatim definišu `kolone` kao i `tip kolone` i `maksimalnu dužinu` podataka polja. Po želji možete dodati i `ograničenja` (constraints) za svaku kolonu.

Za potrebe demonstracije, kreirajte sledeću tabelu:

```sql
CREATE TABLE playground (
    equip_id serial PRIMARY KEY,
    type varchar (50) NOT NULL,
    color varchar (25) NOT NULL,
    location varchar(25) check (location in ('north', 'south', 'west', 'east', 'northeast', 'southeast', 'southwest', 'northwest')),
    install_date date
);
```

Ova komanda kreira tabelu koja vodi evidenciju opreme na igralištu. Prva kolona u tabeli sadržaće ID brojeve opreme tipa `serial`, što je `auto-increment` celobrojna vrednost. Ova kolona ima ograničenje `PRIMARY KEY`, što znači da vrednosti moraju biti `jedinstvene` i `ne mogu biti null`.

Naredne dve linije kreiraju kolone za tip opreme i boju, od kojih nijedna ne može biti prazna (`NOT NULL`). Sledeća linija kreira kolonu za lokaciju sa `ograničenjem` da vrednost mora biti jedna od osam mogućih. Poslednja linija kreira kolonu datuma koja beleži datum instalacije opreme.

Za dve kolone (equip_id i install_date) komanda ne navodi dužinu polja. Razlog je što neki tipovi podataka ne zahtevaju fiksnu dužinu, jer je dužina ili format implicitno definisan.

Novu tabelu možete videti komandom:

```sql
\d
```

List of relations:

Schema | Name                    | Type     | Owner
-------|-------------------------|----------|-------
public | playground              | table    | sammy
public | playground_equip_id_seq | sequence | sammy

Vaša `playground` tabela je tu, ali postoji i objekat `playground_equip_id_seq` tipa `sequence`. To je reprezentacija `serial` tipa koji ste dali koloni equip_id. Ona vodi evidenciju o sledećem broju u sekvenci i automatski se kreira za kolone ovog tipa.

Ako želite da vidite samo tabelu bez sekvence, upišite:

```sql
\dt
```

List of relations:

 Schema |    Name    | Type  | Owner
--------|------------|-------|-------
 public | playground | table | sammy

Sa spremnom tabelom, iskoristimo je za vežbu upravljanja podacima.

### Dodavanje, upit i brisanje podataka u tabeli

Sada kada imate tabelu, možete u nju ubaciti podatke. Na primer, dodajte tobogan i ljuljašku tako što ćete navesti tabelu, imenovati kolone i obezbediti podatke za svaku kolonu ovako:

```sql
INSERT INTO playground (type, color, location, install_date) 
    VALUES ('slide', 'blue', 'south', '2017-04-28');
INSERT INTO playground (type, color, location, install_date) 
    VALUES ('swing', 'yellow', 'northwest', '2018-08-16');
```

Obratite pažnju prilikom unosa podataka da izbegnete nekoliko uobičajenih zamki.

- Prvo, ne stavljajte imena kolona u navodnike, ali vrednosti koje unosite treba da budu u navodnicima.

- Još jedna stvar: ne unosite vrednost za kolonu "equip_id". Ona se automatski generiše svaki put kada dodate novi red u tabelu.

Preuzmite informacije koje ste dodali komandom:

```sql
SELECT * FROM playground;
```

equip_id | type  | color  | location  | install_date
---------|-------|--------|-----------|--------------
 1       | slide | blue   | south     | 2017-04-28
 2       | swing | yellow | northwest | 2018-08-16

Ovde vidite da je `equip_id` uspešno popunjen i da su ostali podaci pravilno organizovani.

Ako se tobogan pokvari i morate ga ukloniti, red možete obrisati iz tabele komandom:

```sql
DELETE FROM playground WHERE type = 'slide';
```

Ponovo izvršite upit nad tabelom:

```sql
SELECT * FROM playground;
```

```sql
 equip_id | type  | color  | location  | install_date 
----------+-------+--------+-----------+--------------
        2 | swing | yellow | northwest | 2018-08-16
(1 row)
```

Primetićete da red za tobogan više nije deo tabele.

### Ažuriranje podataka u tabeli

Do sada ste naučili kako da dodate zapise u tabelu i kako da ih obrišete, ali ovaj vodič još nije pokrio kako da izmenite postojeće unose.

Možete ažurirati vrednosti postojećeg unosa tako što ćete pronaći željeni zapis i postaviti kolonu na novu vrednost. Na primer, možete pronaći zapis za ljuljašku (poklopiće svaku ljuljašku u tabeli) i promeniti joj boju u crvenu. Ovo je korisno ako ste je ofarbali:

```sql
UPDATE playground SET color = 'red' WHERE type = 'swing';
```

Možete proveriti uspešnost operacije ponovnim upitom nad podacima:

```sql
SELECT * FROM playground;
```

 equip_id | type  | color | location  | install_date
----------|-------|-------|-----------|--------------
 2        | swing | red   | northwest | 2018-08-16

Kao što vidite, ljuljaška je sada zabeležena kao crvena.

## PostgreSQL konfiguracija i saveti za performanse

Ispravna konfiguracija PostgreSQL-a je ključna za stabilnu i brzu bazu. Podrazumevana podešavanja su dizajnirana da budu bezbedna za bilo koji hardver, što znači da gotovo nikada nisu optimalna za produkciona opterećenja. Tjuniranje performansi podrazumeva prilagođavanje raspodele resursa—pre svega memorije, I/O-a i rukovanja konekcijama—u skladu sa hardverom vašeg servera i specifičnim obrascima upita vaše aplikacije.

Hajde da prođemo kroz nekoliko ključnih parametara konfiguracije i opštih strategija za poboljšanje performansi baze.
Ključne konfiguracione datoteke

Pre podešavanja, morate znati gde se nalaze konfiguracione datoteke. Na Ubuntu-u, nalaze se u "/etc/postgresql/[version]/main/":

- **postgresql.conf** : Glavna konfiguraciona datoteka servera. Ovde podešavate sve parametre koji utiču na performanse.

- **pg_hba.conf** : Datoteka koja kontroliše autentifikaciju klijenata (koji korisnici mogu da se povežu sa kojih adresa). Ne utiče direktno na performanse upita, ali je ključna za bezbednost.

Nakon uređivanja "postgresql.conf" datoteke ili izmene primenite `reload`-ovanje PostgreSQL servisa:

```sh
sudo systemctl reload postgresql
```

Međutim, neki parametri (označeni kao `postmaster` kontekst u dokumentaciji) zahtevaju puni `restart`:

```sh
sudo systemctl restart postgresql
```

Možete pokrenuti sledeću komandu da proverite da li je posle izmene potreban restart ili reload:

```sql
SELECT context FROM pg_settings WHERE name = 'parameter_name';
```

Za razliku od toga, izmene u "pg_hba.conf" zahtevaju samo `reload`.

### Konfiguracija memorije

Memorija je najznačajniji faktor performansi baze. PostgreSQL koristi deljenu memoriju za keširanje podataka i zasebnu memoriju po konekciji za operacije poput sortiranja.

Parametar | Preporučeno | Podrazumevano | Svrha
----------|-------------|---------------|----------
**shared_buffers** | 25% of total RAM | 128 MB | Ovo je najvažnije memorijsko podešavanje. Definiše količinu memorije koju PostgreSQL posvećuje svom `data cache`-u. Veći keš znači da se veći deo radnog skupa podataka može čitati iz brze memorije umesto sa diska.
**work_mem** | Krenite od podrazumevane vrednosti (`4MB`). Analizirajte spore upite pomoću EXPLAIN ANALYZE. Ako vidite *“Sort Method: external merge Disk”* ili *“HashAgg Disk”*, to znači da je **work_mem** premali. Povećavajte ga postepeno (npr. 16MB, 32MB) za test u sesiji i postavite globalno tek kada ste sigurni. | 4MB |  Ovo podešavanje definiše količinu memorije koju svaka pojedinačna operacija baze (poput sortiranja, hash, join-a ili bitmapa) može da koristi pre nego što se prelije u privremene fajlove na disku. Složeni upiti mogu imati više operacija, od kojih svaka koristi ovu količinu memorije.Ako je vrednost previsoka, može doći do iscrpljivanja memorije kada se paralelno izvršava mnogo složenih upita. Ako je preniska, često dolazi do sporog sortiranja na disku.
**maintenance_work_mem** | Postavite vrednost veću od **work_mem** (npr. 128MB ili 256MB). Pošto se ove operacije ne pokreću tako često kao korisnički upiti, možete biti izdašniji kako biste ubrzali održavanje.| | Ovo podešavanje rezerviše memoriju za interne operacije održavanja, kao što su VACUUM, CREATE INDEX i ALTER TABLE ADD FOREIGN KEY.

#### Podešavanje Checkpoint-a i WAL-a

Checkpoint je proces u kome PostgreSQL upisuje sav „prljav“ (izmenjeni) podatak iz memorije u trajne podatke na disku. Ovo je vrlo intenzivna I/O operacija. Write-Ahead Log (WAL) beleži svaku transakciju. Podešavanje ovih parametara pomaže u balansu I/O opterećenja i vremena oporavka.

Parametar   | Preporučeno  | Početna vrednost | Svrha
------------|--------------|------------------|------------
**wal_buffers** | 16MB | | Postavlja količinu deljene memorije za WAL podatke pre upisa na disk. Podrazumevana vrednost je mala; 16MB je bezbedno povećanje.
**checkpoint_timeout** | 15min | podrazumevano 5min | Maksimalno vreme između automatskih checkpoint-a. Veći razmaci smanjuju I/O pikove, ali povećavaju vreme oporavka posle pada.
**max_wal_size** | 2GB | podrazumevano 1GB | Meka granica ukupne veličine WAL fajlova koja pokreće checkpoint. Povećanje pomaže da se checkpoint-i rasporede.
**min_wal_size** | 512MB | podrazumevano 80MB | Minimalna veličina koju treba zadržati za WAL fajlove kako se stari fajlovi ne bi prebrzo reciklirali.
**checkpoint_completion_target** | 0.9 | podrazumevano 0.5 | Razvlači checkpoint I/O preko dužeg perioda (90% vremena između checkpoint-a) kako bi se ublažili I/O pikovi.

#### Rukovanje konekcijama

Svaka konekcija ka PostgreSQL-u troši memoriju. Veliki broj konekcija može iscrpeti resurse servera.

Parametar   | Preporučeno  | Početna vrednost | Svrha
------------|--------------|------------------|------------
**max_connections** | | Podrazumevano 100 | Maksimalan broj istovremenih konekcija. Iako je primamljivo povećavati, svaka konekcija troši memoriju.
**Pooler konekcija** | | | Za aplikacije koje često otvaraju/zatvaraju konekcije (kao većina veb aplikacija), nemojte povećavati **max_connections**. Umesto toga koristite spoljašnji pooler poput **PgBouncer**-a ili **Pgpool-II**. Pooler održava mali skup stalnih konekcija ka bazi i opslužuje mnogo istovremenih aplikacionih konekcija efikasnije.

#### Planiranje i analiza upita

Ništa od podešavanja servera ne može popraviti neefikasan upit.

- **EXPLAIN ANALYZE** : Primarni alat za tjuniranje upita. Pokrenite ga nad sporim upitima da vidite stvarni plan izvršavanja i vreme.

  ```sql
  EXPLAIN ANALYZE SELECT * FROM users WHERE last_name = 'Smith';
  ```

- **Tražite “Sequential Scan” ( `Seq Scan` ) na velikim, indeksiranim tabelama** : Ovo često znači da vam nedostaje indeks ili upit nije dovoljno selektivan.

- **Kreirajte indekse** : Ako često filtrirate ili spajate po određenim kolonama (npr. user_id, email, created_at), one su dobri kandidati za indeks.

  ```sql
  CREATE INDEX idx_users_on_email ON users (email);
  ```

- **Pokrenite VACUUM ANALYZE** : PostgreSQL koristi planer upita koji se oslanja na statistiku o podacima u tabelama. Ove statistike ažurira ANALYZE. Iako `autovacuum` to radi automatski, dobra praksa je pokrenuti ručni VACUUM ANALYZE posle velikih učitavanja podataka ili kada planovi upita nisu dobri.

  **Napomena** : Pokretanje VACUUM ANALYZE; bez imena tabele radi nad celom bazom i može biti veoma sporo. Bolja praksa je pokretati **VACUUM ANALYZE table_name**; za konkretne tabele posle masovnih učitavanja.

## Bekap i vraćanje PostgreSQL baza

Zaštita podataka je primarna odgovornost administracije baze. Redovni, verifikovani bekapi su najefikasnija odbrana od oštećenja podataka, kvara hardvera ili ljudske greške. PostgreSQL obezbeđuje skup robusnih alatki komandne linije za pravljenje logičkih bekapa i povraćaj.

**Napomena o prostoru na disku** : Pre početka bekapa, uverite se da imate dovoljno prostora. Za obične tekstualne (.sql) dump-ove, planirajte slobodan prostor približno 100% veličine baze. Za kompresovane custom (.dump) fajlove, planirajte 30–50% veličine baze, u zavisnosti od kompresibilnosti podataka.

Postoje dve glavne kategorije bekapa:

- **Logički bekapi (SQL dump-ovi)** : Sastoje se od SQL komandi koje, kada se izvrše, ponovo kreiraju objekte baze (kao tabele i indekse) i podatke. Prave se pomoću `pg_dump` i `pg_dumpall` alatki. Metoda je fleksibilna i prenosiva između različitih verzija PostgreSQL-a.

- **Fizički bekapi (na nivou fajl sistema)** : Podrazumeva kopiranje fizičkih fajlova podataka iz direktorijuma podataka PostgreSQL-a. Tipično se koristi za Point-in-Time Recovery (PITR), naprednu postavku obrađenu kasnije u ovom poglavlju.

### Pravljenje logičkih bekapa pomoću pg_dump

Alatka `pg_dump` pravi bekap jedne baze. Ne bekapuje globalne objekte poput rola ili tablespace-ova. Veoma je fleksibilna i može se pokretati nad aktivnom bazom bez blokiranja čitača ili pisača.

**Uobičajene opcije za pg_dump** :

Izlaz `pg_dump` možete kontrolisati pomoću više opcija komandne linije:

- **-F c (format: custom)** : Preporučen format za većinu bekapa. Proizvodi kompresovanu binarnu arhivu (nečitljivu za čoveka) koja omogućava paralelno vraćanje i ređanje/izuzimanje objekata tokom vraćanja.
- **-F p (format: plain)** : Podrazumevani format. Izbacuje veliki ".sql" fajl. Glavni nedostatak je da se mora vratiti odjednom i da ne podržava paralelno vraćanje.
- **-s (samo šema)** : Bekapuje samo strukturu baze (tabele, poglede, indekse) bez podataka.
- **-a (samo podaci)** : Bekapuje samo podatke, pod pretpostavkom da šema već postoji na odredištu.
- **--exclude-table=TABLE_NAME** : Isključuje konkretnu tabelu iz bekapa. Korisno za preskakanje velikih, privremenih ili manje bitnih tabela.
- **-f FILENAME**: Navodi izlazni fajl.
- **-U USER** : Navodi PostgreSQL korisnika pod kojim se konektuje.

Ovo je preporučena komanda za većinu zadataka bekapa. Povezuje se na bazu mydatabase kao korisnik `postgres` i kreira kompresovani fajl "mydatabase.dump".

```sh
pg_dump -U postgres -F c -f mydatabase.dump mydatabase
```

### Vraćanje logičkih bekapa

#### Vraćanje iz custom arhive (.dump)

Koristite alatku `pg_restore`. Obezbeđuje značajnu fleksibilnost, uključujući paralelno vraćanje i provere pre izvršavanja.

Umesto ručnog kreiranja baze, najbolja praksa je koristiti flag `--create`. On govori `pg_restore` da iz arhive izda CREATE DATABASE komandu. Za to se morate povezati na neku postojeću bazu, poput podrazumevane `postgres` baze.

Sledeća komanda se povezuje na postgres bazu, briše i ponovo kreira mydatabase ( zbog `--clean` i `--create` ) i koristi 4 paralelna procesa za vraćanje.

```sh
pg_restore -U postgres -d postgres --create --clean -j 4 mydatabase.dump
```

Ovde je:

- **-d postgres** : Povezuje se na postgres bazu radi izdavanja create komande.

- **--create** : Kaže `pg_restore` da kreira ciljnu bazu (npr. mydatabase) pre vraćanja u nju.

- **--clean** : Uz `--create` briše i ponovo kreira celu ciljnu bazu. Ako se koristi samostalno (vraćanje u postojeću bazu), briše postojeće objekte pre njihovog ponovnog kreiranja.

- **-j 4**: Koristi 4 paralelna posla za vraćanje.

Povezujemo se na postgres bazu jer `pg_restore` treba vezu ka postojećoj bazi na serveru da bi izdao CREATE DATABASE komandu za naš novi cilj.

**Napomena o paralelnom vraćanju** : Flag `-j` primarno paralelizuje učitavanje podataka i izgradnju indeksa i dostupan je samo za `custom` format arhive. Neke operacije, poput kreiranja baze ili vraćanja objekata sa složenim zavisnostima, ostaju serijske. Ne može se koristiti sa opcijom `--single-transaction`.
  
#### Vraćanje iz običnog tekstualnog fajla (.sql)

Koristite standardni psql klijent. Ova metoda zahteva da ciljna baza već postoji.

- Kreirajte bazu:

  ```sh
  createdb -U postgres mydatabase_new
  ```

- Prosledite ".sql" fajl u psql:

  ```sh
  psql -U postgres -d mydatabase_new < mydatabase.sql
  ```

### Bekap svih baza (pg_dumpall)

Za bekap svih baza na serveru, plus svih globalnih objekata (role, korisnici, tablespace-ovi), koristite alatku `pg_dumpall`. Ona proizvodi samo običan tekstualni ".sql" fajl.

```sh
# Run as the postgres superuser
pg_dumpall -U postgres > all_databases.sql
```

Za vraćanje iz `pg_dumpall` fajla, prosledite ga u `psql`. Morate se povezati kao superuser (poput postgres) na neku podrazumevanu bazu (poput postgres). Skripta sadrži sve potrebne CREATE ROLE i CREATE DATABASE komande i izvršiće ih odatle.

```sh
# Run as the postgres superuser
psql -U postgres -d postgres -f all_databases.sql
```

### Bekap samo rola i globalnih objekata

Uobičajen i koristan obrazac je bekap samo globalnih objekata. Radi se flagom `--globals-only` i predstavlja efikasan način replikacije korisničkih dozvola između servera.

```sh
pg_dumpall -U postgres --globals-only > roles.sql
```

### Napomena o fizičkim bekapima i Point-in-Time Recovery (PITR)

Iako se ovaj vodič fokusira na logičke bekape, PostgreSQL podržava i fizičke bekape. Ovaj napredni pristup, poznat kao Point-in-Time Recovery (PITR), kombinuje bazni bekap na nivou fajl sistema (kreiran pomoću `pg_basebackup`) sa kontinuiranom arhivom WAL (Write-Ahead Log) fajlova.

Ova tehnika omogućava vraćanje baze na bilo koji tačno definisan trenutak (npr. „na 15:05, neposredno pre slučajnog brisanja podataka“), a ne samo na vreme bekapa. Najrobustnija je strategija bekapa za kritične produkcione sisteme, ali zahteva dodatnu konfiguraciju, uključujući postavljanje `wal_level` na replica ili više (ne minimal) u "postgresql.conf" i podešavanje "archive_command" za arhiviranje WAL-a.

### Rezime strategije bekapa i vraćanja

Sledeća tabela sumira strategiju bekapa i vraćanja:

Alat              |    Akcija   |      Izlaz     |        Slučaj upotrebe
------------------|-------------|----------------|--------------------------
**pg_dump**       |    Bekap    | .sql ili .dump | Bekap jedne baze. Veoma fleksibilan.
**pg_restore**    |    Vraćanje |       N/A      | Vraćanje .dump arhive.
**psql**          |    Vraćanje |       N/A      | Vraćanje običnog .sql fajla.
**pg_dumpall**    |    Bekap    |      .sql      | Bekap svih baza i globalnih rola.
**pg_basebackup** |    Bekap    |  Data fajlovi  | Kreira bazni bekap za fizičku/PITR strategiju.

### Automatizacija i najbolje prakse za bekap

Plan bekapa nije potpun dok nije automatizovan, verifikovan i bezbedan.

- **Automatizujte bekape** : Koristite standardne Linux alatke poput `cron`-a da zakazujete `pg_dump` ili `pg_dumpall` komande za noćno pokretanje.

- **Bezbedno rukujte lozinkama** : Za pokretanje bekapa u neinteraktivnim skriptama, ne hardkodirajte lozinke. Koristite fajl ".pgpass". Kreirajte ga na `~/.pgpass` u home direktorijumu korisnika (npr. "/var/lib/postgresql/.pgpass"). Fajl mora imati stroge `0600` dozvole, inače će ga PostgreSQL ignorisati. Format je `hostname:port:database:user:password`. Možete koristiti `*` kao džoker za bilo koje polje, npr. `localhost:5432:*:postgres:your_password`.

- **Testirajte vraćanja** : Bekap je koristan samo ako može da se uspešno vrati. Redovno vežbajte vraćanje bekapa na zasebnom, ne-produkcionom serveru radi verifikacije integriteta.

- **Verifikujte bekape** : Nakon kreiranja bekapa, dodajte korak u skripti koji proverava da veličina fajla nije nula. Za custom formate, možete pokrenuti `pg_restore -l mydatabase.dump &> /dev/null` (što ispisuje sadržaj u "/dev/null") i preko exit koda potvrditi da arhiva nije korumpirana.

- **Čuvajte bekape udaljeno** : Ne skladištite bekape na istom serveru kao bazu. Potpuni pad servera bi izgubio oba. Kopirajte bekape na udaljenu lokaciju poput DigitalOcean Spaces ili na drugi server kao završni korak skripte.

## Poređenje PostgreSQL-a sa drugim sistemima baza na Ubuntu-u

Ubuntu repozitorijumi paketa obezbeđuju pristup širokom spektru sistema baza. Najbolji izbor u potpunosti zavisi od zahteva vaše aplikacije, kao što su model podataka, potrebe skaliranja i garancije konzistentnosti.

- **PostgreSQL**: Objektno-relaciona baza poznata po visokoj usklađenosti sa standardima, integritetu podataka i bogatom skupu funkcija. Potpuno je ACID kompatibilna i dobro podržava napredne tipove podataka i složene upite, pa je čest izbor za primarnu „system of record“ bazu.

- **MySQL** : Kao deo originalnog LAMP stack-a, jedan je od najpopularnijih open-source sistema. Poznat po jednostavnosti, istorijski snažnim performansama čitanja i jakoj zajednici. Takođe je ACID kompatibilan (sa podrazumevanim InnoDB engine-om) i često se bira za jednostavne veb aplikacije.

- **MariaDB** : Fork MySQL-a vođen zajednicom. Iako zadržava visoku kompatibilnost (posebno sa starijim verzijama), vremenom se udaljio i uvodi sopstvene funkcije, pa možda nije potpuna zamena za aplikacije zasnovane na modernim funkcijama MySQL 8.0+.

- **SQLite** : Nije klijent-server baza. To je C biblioteka koja ugrađuje ceo DB engine u aplikaciju, čuvajući celu bazu u jednom fajlu. Dobro podnosi više istovremenih čitača, ali nije idealna za aplikacije sa čestim istovremenim upisima ili mrežnim multi-user scenarijima.

- **MongoDB** : Vodeća NoSQL baza koja skladišti podatke u fleksibilnim, JSON-sličnim dokumentima. Omogućava fleksibilnu šemu i napravljena je za visoku dostupnost i horizontalno skaliranje. Od verzije 4.0 podržava ACID transakcije kroz više dokumenata, pa je održiva za aplikacije sa zahtevom za snažnu konzistentnost.

- **Redis** : In-memory skladište struktura podataka, što znači da drži sve podatke u RAM-u radi izuzetne brzine. Nije zamena opšte namene za disk-bazu, ali se gotovo uvek koristi uz neku (poput PostgreSQL-a) kao keš ili skladište sesija velike brzine.

Evo poređenja PostgreSQL-a sa drugim popularnim sistemima baza dostupnim na Ubuntu-u.

### Poređenje sistema baza

Baza       | Tip    | Primarni model | Najjača osobina | Tipičan slučaj upotrebe
-----------|--------|---------------|-------------------|-------------------------------------
PostgreSQL | ORDBMS | Relacioni    | Integritet podataka, usklađenost sa standardima, napredne funkcije (JSONB, PostGIS) | Složene aplikacije, system of record, finansijski sistemi
MySQL      | RDBMS  | Relacioni    | Jednostavnost, istorijski snažno čitanje, jaka zajednica | Veb aplikacije, CMS sistemi
MariaDB    | RDBMS  | Relacioni    | Kompatibilan sa MySQL-om, open-source fork sa dodatnim funkcijama i optimizacijama | Često zamena za MySQL, ali možda nije 1-na-1 za novije MySQL (8.0+) funkcije.
SQLite     | Embedded | Relacioni (fajl) | Lagan, bez servera, bez konfiguracije | Mobilne aplikacije, ugrađeni uređaji, lokalni razvoj
MongoDB    | NoSQL  | Dokumentni   | Fleksibilna šema, horizontalno skaliranje, ACID transakcije | Nestrukturirani podaci, big data, upravljanje sadržajem
Redis      | In-Memory | Key-Value | Izuzetna brzina (svi podaci u RAM-u) | Keširanje, upravljanje sesijama, message broker

## Uobičajeni problemi i rešavanje sa PostgreSQL-om na Ubuntu-u

Pokretanje PostgreSQL-a na Ubuntu-u može doneti specifične izazove, naročito korisnicima koji su novi u njegovoj arhitekturi na Debian-baziranim sistemima. Većina grešaka spada u predvidljive kategorije:

- konflikti pri instalaciji,
- pravila autentifikacije,
- dozvole fajlova i
- upravljanje servisom.

Pre rešavanja problema, vaš najvažniji alat je log fajl PostgreSQL-a. Na standardnim Ubuntu instalacijama nalazi se na "/var/log/postgresql/postgresql-[version]-main.log". Uvek prvo proverite ovaj log, jer daje konkretne poruke o greškama koje usmeravaju dijagnostiku.

### Problemi pri instalaciji

Ovi problemi se obično javljaju tokom početnog procesa `apt` instalacije ili kada `initdb` prvi put pokušava da kreira novi klaster baze.

#### Konflikti paketa ili neusaglašene verzije

Ovaj problem nastaje kada sistem pokušava da instalira PostgreSQL pakete iz dva izvora, kao što su podrazumevani Ubuntu repozitorijum i zvanični PGDG repozitorijum. Ubuntu repozitorijumi često sadrže starije, stabilne verzije, dok PGDG obezbeđuje najnovija izdanja. Mešanje može dovesti do pokvarenih zavisnosti.

**Rešenje** :

- **Proverite izvore** : Koristite `apt-cache policy postgresql` da vidite koje su verzije dostupne i iz kog repozitorijuma.
- **Izaberite jedan izvor** : Preporučuje se korišćenje PGDG repozitorijuma
  za najaktuelnije verzije. Ako dodate PGDG, možda ćete morati da koristite `apt pinning` da biste mu dali prioritet nad podrazumevanim Ubuntu paketima.
- Popravite pokvarene instalacije: Ako ste već u konfliktu, `sudo apt-get -f install` može pomoći. Ako ne, možda treba potpuno ukloniti sve PostgreSQL pakete ( `sudo apt-get purge "postgresql-*"` ) i ponovo instalirati iz jednog, čistog izvora.

#### Port 5432 je već u upotrebi

Ova greška znači da neki drugi program već sluša na podrazumevanom PostgreSQL portu (5432). Često se dešava ako se stara, deinstalirana instanca PostgreSQL-a nije pravilno ugasila ili ako imate više klastera koji rade (funkcionalnost Ubuntu-ovog `pg_ctlcluster` sistema).

**Rešenje** :

- **Identifikujte proces** : Koristite netstat da otkrijete šta koristi port.

  ```sh
  sudo netstat -tulpn | grep 5432
  ```

- **Proverite druge klastere** : Ako je proces postgres, koristite komandu `pg_lsclusters` da vidite sve konfigurisane klastere i njihova stanja.

  ```sh
  pg_lsclusters
  ```

  Izlaz može pokazati jedan "online" i jedan "down".  
  
- **Zaustavite neželjeni klaster** : Ako pronađete stari ili neplanirani klaster koji radi, zaustavite ga koristeći njegovu verziju i ime.

  ```sh
  sudo pg_ctlcluster 13 main stop
  ```

- **Promenite port**: Ako drugi servis legitimno koristi port 5432, morate konfigurisati novi PostgreSQL klaster da koristi drugi port uređivanjem "postgresql.conf" fajla.

#### Greške lokalizacije i enkodiranja tokom initdb

Greške poput `invalid locale name` ili `could not determine default locale` tokom inicijalne postavke klastera ( `initdb` ) znače da podešavanja lokalizacije na vašem Ubuntu sistemu nedostaju ili su pogrešno podešena. PostgreSQL nasledjuje ova podešavanja iz OS-a da bi odredio redosled sortiranja (collation), klasifikaciju karaktera i formatiranje.

**Rešenje** :

- **Proverite sistemske lokalizacije**: Pokrenite locale -a da vidite sve dostupne lokalizacije.

- **Generišite nedostajuće lokalizacije**: Ako željena lokalizacija (npr. `en_US.UTF-8`) nije navedena, morate je generisati.

  ```sh
  sudo locale-gen en_US.UTF-8
  sudo update-locale
  ```
  
  Ponovno inicijalizujte klaster: Nakon podešavanja sistemskih lokalizacija, možda ćete morati da oborite i ponovo kreirate PostgreSQL klaster. Komanda pg_createcluster je standardan Ubuntu metod za to.

  **Upozorenje**: Ovo uništava sve podatke u klasteru

  ```sh
  sudo pg_dropcluster [version] main
  sudo pg_createcluster [version] main
  ```

### Problemi sa autentifikacijom

Ovo su najčešći problemi sa kojima se korisnici suočavaju nakon uspešne instalacije.

#### Neuspeh peer autentifikacije

Ovo je najčešća greška. Znači da pokušavate lokalno povezivanje, a vaše trenutno Ubuntu korisničko ime se ne poklapa sa nazivom PostgreSQL role koju pokušavate da koristite.

Podrazumevano, PostgreSQL na Ubuntu-u koristi `peer` autentifikaciju za lokalne veze. Ova metoda pita operativni sistem: "Koje je korisničko ime procesa koji se povezuje?" i dozvoljava vezu ako se to ime poklapa sa traženom rolom baze.

Na primer, ako ste prijavljeni kao "myuser" i pokrenete `psql -U postgres`, veza će pasti jer OS prijavljuje vaše korisničko ime kao "myuser", a ne postgres.

**Rešenje** :

Ispravan način povezivanja kao postgres superuser je da prvo preuzmete identitet postgres sistemskog korisnika koristeći sudo.

```sh
sudo -u postgres psql
```

Ako želite da promenite ovo ponašanje, morate postaviti lozinku za `postgres` rolu i zatim urediti "pg_hba.conf" da umesto `peer` koristi `md5` ili `scram-sha-256`.

#### Pogrešna konfiguracija "pg_hba.conf"

Greške u fajlu "pg_hba.conf" (Host-Based Authentication) su glavni uzrok neuspešnih konekcija. Ovaj fajl, na putanji "/etc/postgresql/[version]/main/pg_hba.conf", predstavlja skup pravila koja kontrolišu ko se može povezati, odakle, na koju bazu i kojim metodom. Jedna sintaksna greška ili pogrešan redosled pravila može vas zaključati.

**Rešavanje** :

- **Redosled pravila je bitan** : PostgreSQL čita fajl odozgo nadole i koristi prvo poklapajuće pravilo. Specifičnija pravila uvek moraju biti iznad opštih.

- **Reload nakon izmena**: Nakon uređivanja "pg_hba.conf", morate izvršiti `reload` PostgreSQL servisa da bi izmene stupile na snagu:

  ```sh
  sudo systemctl reload postgresql
  ```

Evo uobičajenih grešaka u konfiguraciji:

Problem | Primer linije | Objašnjenje
--------|---------------|------------
Udaljene konekcije padaju | host all all 127.0.0.1/32 scram-sha-256 | Pravilo dozvoljava konekcije samo sa `localhost`. Dodajte posebnu liniju za vaš udaljeni IP opseg.
Pogrešan redosled pravila | host all all 0.0.0.0/0 reject, host all all 192.168.1.100/32 md5 | `reject all` je iznad specifičnog IP pravila, pa se do njega nikada ne dolazi. Specifična IP pravila moraju biti iznad opštih.
`Peer` neuspeh posle izmene | local all postgres md5 | Metod je promenjen sa `peer` na `md5`, ali lozinka nije postavljena ili se i dalje povezujete preko `sudo -u postgres psql`.

### Problemi sa dozvolama

Ove greške se često pojavljuju kao `Permission denied` u logovima ili pri pokušaju povezivanja.
Nije moguće pristupiti socket fajlu.

Greške poput `Could not connect to server: No such file or directory` ili `Permission denied` pri referenciranju "/var/run/postgresql/.s.PGSQL.5432" obično znače jedno od sledećeg:

- **Servis ne radi** : Socket fajl se uklanja kada se servis zaustavi. Proverite status servisa komandom sudo `systemctl status postgresql`.

- **Korisničke dozvole**: Direktorijum socket-a je u vlasništvu `postgres` korisnika i dostupan `postgres` grupi. Ako vaš sistemski korisnik nije u `postgres` grupi, ne možete koristiti socket.

**Rešenje** :

Dodajte svog korisnika u `postgres` grupu. Morate se odjaviti i ponovo prijaviti da bi izmena stupila na snagu.

  ```sh
  sudo usermod -a -G postgres $USER
  ```

#### Dozvole direktorijuma podataka

Ako se PostgreSQL servis ne pokreće, a logovi pominju `Permission denied` vezano za direktorijum podataka (npr. "/var/lib/postgresql/[version]/main"), vlasništvo nad direktorijumom ili dozvole fajlova su pogrešni.

PostgreSQL zahteva da direktorijum podataka bude u isključivom vlasništvu sistemskog korisnika `postgres` i da ima dozvole `0700` (samo vlasnik može da čita, piše i izvršava). Ovo je bezbednosna mera. Problem se često javlja nakon vraćanja bekapa ili ručnog pomeranja direktorijuma podataka.

**Rešenje** :

Rekurzivno resetujte vlasništvo i dozvole.

```sh
# Ispravite vlasništvo (zamenite 18 vašom verzijom)
sudo chown -R postgres:postgres /var/lib/postgresql/18/main

# Ispravite dozvole
sudo chmod 0700 /var/lib/postgresql/18/main
```

### Upravljanje servisom

Ovi problemi se odnose na pokretanje, zaustavljanje i upravljanje samim postgresql servisom.

#### Servis ne uspeva da se pokrene nakon izmena konfiguracije

Ako se servis ne pokrene odmah nakon uređivanja "postgresql.conf" ili "pg_hba.conf", verovatno ste uveli sintaksnu grešku.

**Rešavanje** :

- **Proverite logove** : Najbrži način da nađete grešku. `sudo journalctl -u postgresql` ili konkretan log na "/var/log/postgresql/postgresql-[version]-main.log" često će navesti tačan broj linije sa nevažećim podešavanjem.

  Uobičajene greške u postgresql.conf:

  - Neobuhvaćene vrednosti stringa (npr. `log_destination = stderr` je tačno; `log_destination = /var/log/my.log` je pogrešno, treba `var/log/my.log`).
  
  - Nevažeće vrednosti podešavanja (npr. `shared_buffers = 10GB` kada imate samo `8GB` RAM-a).

### Zbrka oko više klastera

Paket `postgresql-common` na Ubuntu-u uključuje skup wrapper skripti (`pg_lsclusters`, `pg_ctlcluster`, `pg_createcluster`) koje omogućavaju pokretanje više, odvojenih PostgreSQL instanci (klastera) na istom računaru, često različitih verzija.

Ovo je moćna funkcija, ali može biti zbunjujuća. Možda uređujete konfiguracioni fajl za verziju 18, a zapravo se povezujete na verziju 13, pa pomislite da se "promene ne primenjuju".

**Rešavanje** :

- Ispišite sve klastere: Uvek počnite odavde da vidite šta radi, na kom portu i gde su konfiguracioni fajlovi.

```sh
pg_lsclusters

# Ver Cluster Port Status Owner    Data directory                       Log file
# 13  main    5432 down   postgres /var/lib/postgresql/13/main         ...
# 18  main    5433 online postgres /var/lib/postgresql/18/main         ...
```

- Upravljajte konkretnim klasterom: Pri pokretanju, zaustavljanju ili reload-u, uvek navedite klaster koji želite da menjate:

```sh
sudo pg_ctlcluster 18 main reload
```

### Ograničenja resursa

Ove greške se javljaju kada PostgreSQL traži više resursa (poput memorije) nego što je operativni sistem podešen da obezbedi.

#### Greške deljene memorije

Greške pri pokretanju poput "could not create shared memory segment: Invalid argument" ili "No space left on device" često znače da je podešavanje `shared_buffers` veće od kernel limit-a OS-a za deljenu memoriju (`SHMMAX`).

PostgreSQL koristi veliki blok deljene memorije za keširanje i međuprocesnu komunikaciju. Ako zahteva više nego što OS dozvoljava, OS odbija zahtev i PostgreSQL ne uspeva da se pokrene. Ovo je često u okruženjima sa ograničenom memorijom ili u kontejnerima.

**Rešavanje**:

- Proverite OS limite:

```sh
cat /proc/sys/kernel/shmmax
```

- Proverite "postgresql.conf": Pogledajte podešavanje `shared_buffers`.

  - **Rešenje 1 (preporučeno)**: Smanjite `shared_buffers` u "postgresql.conf" da bude bezbedno ispod `shmmax` limita.

  - **Rešenje 2 (napredno)**: Povećajte kernel shmmax limit. Uredite "/etc/sysctl.conf" i dodajte liniju poput `kernel.shmmax = 134217728` (za 128MB). Primeni komandom `sudo sysctl -p`. Ovo je uglavnom potrebno samo za visokoperformansno tjuniranje.

## Česta pitanja (FAQ)

### Šta je PostgreSQL i zašto da ga koristim na Ubuntu-u

PostgreSQL, često nazivan Postgres, je moćan, open-source objektno-relacioni sistem baza podataka. Ima snažnu reputaciju po pouzdanosti, robusnosti funkcija i integritetu podataka, uz preko 30 godina aktivnog razvoja.

Treba da ga razmotrite za svoje aplikacije jer u potpunosti podržava ACID (Atomicity, Consistency, Isolation, Durability), dobro podnosi visoku istovremenost i podržava širok spektar naprednih tipova podataka i ekstenzija, poput PostGIS-a za geoprostorne podatke.

Korišćenje PostgreSQL-a na Ubuntu-u je čest izbor jer je Ubuntu popularna i stabilna Linux distribucija za servere. PostgreSQL je uključen u podrazumevane Ubuntu repozitorijume, što čini instalaciju i blagovremeno primanje bezbednosnih ažuriranja jednostavnim procesom koji upravlja apt menadžer paketa.

### Kako da instaliram PostgreSQL na Ubuntu-u

PostgreSQL možete instalirati direktno iz podrazumevanih Ubuntu repozitorijuma koristeći komandu `apt`.

Prvo osvežite lokalni indeks paketa na serveru kako biste imali najnovije liste paketa:

```sh
sudo apt update
```

Zatim instalirajte glavni PostgreSQL paket i contrib paket, koji uključuje dodatne alatke i ekstenzije:

```sh
sudo apt install postgresql postgresql-contrib
```

**Napomena** : Ova komanda instalira podrazumevanu PostgreSQL verziju dostupnu u glavnom Ubuntu repozitorijumu (npr. postgresql-16). Ako vam treba druga verzija, možete je navesti punim nazivom paketa, kao:

```sh
sudo apt install postgresql-18 postgresql-contrib-18.
```

### Kako da pokrenem i omogućim PostgreSQL servis?

Nakon uspešne instalacije, PostgreSQL servis bi trebalo da se automatski pokrene. Status možete proveriti komandom `systemctl`.

```sh
sudo systemctl status postgresql
```

Možete videti izlaz sličan ovome:

```sh
● postgresql.service - PostgreSQL RDBMS
     Loaded: loaded (/lib/systemd/system/postgresql.service; enabled; vendor preset: enabled)
     Active: active (exited) since Wed 2025-10-29 10:30:01 IST; 1min 12s ago
   Main PID: 1234 (code=exited, status=0/SUCCESS)
      Tasks: 0 (limit: 4662)
...
```

**Napomena** : Normalno je da glavni `postgresql.service` prikazuje status active (exited). Ovaj servis je wrapper koji pokreće klaster-specifičan servis (npr. <postgresql@18-main.service> ), koji obavlja stvarni rad. Sve dok je servis "enabled“ i "active“, vaš DB server radi.

Ako servis ne radi, možete ga pokrenuti ručno:

```sh
sudo systemctl start postgresql
```

Da bi se baza automatski pokretala pri svakom boot-u servera, morate omogućiti servis:

```sh
sudo systemctl enable postgresql
```

### Kako da kreiram novu bazu u PostgreSQL-u

Najjednostavniji metod je alatka komandne linije `createdb`. Ovu komandu mora pokrenuti rola u PostgreSQL-u koja ima dozvolu za kreiranje baza.

Podrazumevano, Linux nalog `postgres` je podešen da se povezuje kao `postgres` i to je `superuser` baze. Možete se prebaciti na ovaj nalog i pokrenuti komandu:

```sh
sudo -i -u postgres
createdb my_database
```

Alternativno, možete se prijaviti u interaktivni PostgreSQL prompt (psql) i koristiti SQL komandu CREATE DATABASE.

Prvo pristupite promptu kao korisnik postgres:

```sh
sudo -i -u postgres
psql
```

Zatim, iz `psql` prompta (izgleda kao postgres=# jer je postgres superuser), pokrenite SQL komandu. Ne zaboravite da SQL naredbe završite tačkom-zarezom.

```sql
CREATE DATABASE my_database;
```

Iz `psql` prompta izađite kucanjem `\q`, a zatim `exit` da se vratite na vaš regularni korisnički nalog.

**Napomena o vlasništvu** : Korisnik koji izvrši CREATE DATABASE komandu postaje vlasnik nove baze po podrazumevanom pravilu. Ovo vlasništvo mu daje sve privilegije nad tom bazom. Ako je kreira `postgres` korisnik, vlasnik će biti `postgres`.

### Kako da kreiram i upravljam PostgreSQL korisnicima i rolama

U PostgreSQL-u upravljanje korisnicima se vrši preko rola. Rola može biti korisnik baze ili grupa korisnika, ali rola sa privilegijom LOGIN se smatra korisnikom.

#### Kreiranje korisnika

Možete koristiti alatku komandne linije `createuser`. Najbolje je pokretati je kao korisnik `postgres`.

```sh
sudo -i -u postgres
```

Preporučeni pristup je korišćenje `--interactive` flag-a za interaktivno postavljanje i `--pwprompt` flag-a za trenutno postavljanje lozinke.

```sh
createuser --interactive --pwprompt
```

Bićete upitani za `naziv role`, `lozinku` i da li rola treba da bude `superuser` ili da ima druge visoke privilegije.

```sh
Enter name of role to add: sammy
Enter password for new role: 
Enter it again: 
Shall the new role be a superuser? (y/n) n
Shall the new role be allowed to create databases? (y/n) n
Shall the new role be allowed to create more new roles? (y/n) n
```

Korisnika možete kreirati i jednom SQL naredbom iz `psql` prompta. Ovo kreira minimalnog korisnika koji može samo da se prijavi.

```sql
CREATE ROLE sammy WITH LOGIN PASSWORD 'secure_password';
```

#### Dodela privilegija

Kreiranje korisnika mu ne daje automatski pristup bazi. Privilegije morate eksplicitno dodeliti. Proces ima nekoliko koraka:

- **Dodelite CONNECT pristup** : Prvo dodelite novoj roli dozvolu da se poveže na vašu bazu.

  ```sql
  GRANT CONNECT ON DATABASE my_database TO sammy;
  ```

- **Česta zamka** : Korišćenje

  ```sql
  GRANT ALL PRIVILEGES ON DATABASE my_database TO sammy; 
  ```

  može biti zavaravajuće.

  Ova komanda dodeljuje samo
  
  - CONNECT,
  - CREATE, and
  - TEMP  
  
  dozvole na samoj bazi, ne na tabelama u njoj.

- **Dodelite pristup šemi**: Zatim morate dodeliti dozvolu za pristup šemi unutar baze. Bez ovoga, korisnik može da se poveže, ali ne može da vidi nijednu tabelu.

  ```sql
  GRANT USAGE ON SCHEMA public TO sammy;
  ```

- **Dodela dozvola nad tabelama** : Sada možete dodeliti specifične dozvole koje korisniku trebaju nad objektima u šemi. Povežite se na bazu kao superuser da biste ovo uradili

  ```sql
  \c my_database
  ```

  Sada ste povezani na "my_database".

- **Dodela dozvola nad postojećim objektima** : Da biste roli sammy dali read/write pristup svim postojećim tabelama, pokrenite:

  ```sql
  GRANT ALL ON ALL TABLES IN SCHEMA public TO sammy;
  GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO sammy;
  ```

  Ako želite samo `read-only` pristup, koristite GRANT SELECT umesto GRANT ALL.

- **(Opcionalno) Dodela dozvola za buduće objekte**: Gornje komande utiču samo na postojeće tabele. Da automatski dodelite dozvole za nove tabele koje će se tek kreirati (npr. od strane admina), morate izmeniti podrazumevane privilegije:

  ```sql
  ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO sammy;
  ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO sammy;
  ```

### Koje su osnovne PostgreSQL komande za početnike

Najvažniji alat za učenje je interaktivni terminal `psql`. Kada ste prijavljeni (npr. pokretanjem `sudo -i -u postgres` pa `psql`), možete koristiti meta-komande (koje počinju obrnutom kosom crtom \) da se krećete i pregledate bazu.

Evo nekih najčešćih psql komandi:

Komanda          | Svrha
-----------------|-------------------------------------
\l ili \list     | Ispisuje sve baze podataka
\c database_name | Povezuje se na zadatu bazu podataka
\conninfo        | Prikazuje informacije o trenutnoj konekciji
\dt              | Ispisuje tabele u trenutno aktivnoj bazi
\du              | Ispisuje role/korisnike
\d table_name    | Prikazuje šemu tabele, uključujući indekse i kolone
\?               | Prikazuje pomoć u okviru `psql` prompta
\q               | Napušta `psql` prompt

**Napomena** : Znak u psql promptu ukazuje na nivo privilegija.

- Znak heš (#) znači da ste povezani kao superuser (npr. postgres).
- Znak veće (=>) znači da ste regularan, ne-superuser.

### Kako da se povežem na PostgreSQL bazu iz terminala?

Način povezivanja zavisi od metode autentifikacije podešene u fajlu "pg_hba.conf" (obično na "/etc/postgresql/version/main/pg_hba.conf"). Podrazumevano, PostgreSQL na Ubuntu-u koristi `peer` autentifikaciju za lokalne veze.

`peer` autentifikacija radi tako što dobija korisničko ime klijenta iz kernela i koristi ga kao dozvoljeno korisničko ime baze. Ovo radi samo za `lokalne veze` preko `Unix socket`-a.

#### Povezivanje kao postgres admin (lokalna peer autentifikacija)

Ovo je najjednostavniji način lokalnog povezivanja. Koristite `sudo` da pokrećete komande kao Linux korisnik `postgres`, što `peer` autentifikacija automatski mapira na `postgres` `superuser`-a PostgreSQL-a.

```sh
sudo -i -u postgres
psql
```

Ovo vas povezuje preko lokalnog `Unix socket`-a na podrazumevanu bazu `postgres`.

#### Povezivanje kao konkretan korisnik (lokalna peer vs. TCP/IP lozinka)

Recimo da ste kreirali PostgreSQL rolu po imenu "sammy", ali ste prijavljeni na server kao drugi Linux korisnik (npr. "ubuntu").

- **Pokušaj 1: Neuspeh (nepoklapanje `peer` autentifikacije)** : Ako pokrenete `psql -U sammy -d my_database`, verovatno će pasti. Komanda pokušava da koristi lokalni Unix socket, pa `peer` autentifikacija pokušava da poklopi vašeg OS korisnika ("ubuntu") sa korisnikom baze ("sammy"). Pošto se ne poklapaju, veza se odbija.

- **Pokušaj 2: Uspeh (autentifikacija lozinkom)** : Da zaobiđete `peer`, možete naterati `TCP/IP` (mrežnu) vezu koristeći `-h localhost`. Podrazumevano, Ubuntu-ov "pg_hba.conf" je podešen da koristi `md5` ili `scram-sha-256` (lozinku) za lokalne `TCP/IP` veze.

  ```sh
  psql -U sammy -d my_database -h localhost
  ```

  Terminal će zatim tražiti lozinku koju ste postavili za rolu "sammy".

  **Savet** : Da izbegnete unos lozinke pri svakoj vezi, napravite fajl ".pgpass" u svom home direktorijumu. Ovaj fajl bezbedno čuva lozinke vaših konekcija. Obavezno mu postavite dozvole `0600` (čitanje/pisanje samo za vaš korisnički nalog).

- **Pokušaj 3: Uspeh (poklapanje peer autentifikacije)** : Ako imate i Linux sistemskog korisnika "sammy", možete koristiti `sudo` da pokrenete `psql` kao taj korisnik. `peer` autentifikacija bi tada uspela.

  ```sh
  sudo -i -u sammy
  psql -d my_database
  ```

## Zaključak

Ovaj vodič obezbeđuje kompletan prolazak kroz instalaciju i upravljanje PostgreSQL-om na Ubuntu serveru. Počeli ste od početne instalacije softvera, konfigurisali autentifikaciju korisnika kreiranjem rola i postavili nove baze. Zatim smo pokrili osnovne SQL komande za manipulaciju podacima i obradili ključne administrativne zadatke, uključujući konfiguraciju performansi, bekap i vraćanje, kao i rešavanje uobičajenih problema.

Sada imate funkcionalan DB server i osnovne veštine potrebne da ga održavate bezbedno i efikasno. Sposobnost upravljanja podacima, pristupom korisnika i obezbeđivanjem integriteta baze priprema vas za podršku složenim aplikacijama.

Ako želite da naučite više o Postgresu i njegovoj upotrebi, preporučujemo sledeće vodiče:

- SQLite vs MySQL vs PostgreSQL: Poređenje sistema za upravljanje
  relacionim bazama podataka
- Vežbajte pokretanje upita sa SQL-om
