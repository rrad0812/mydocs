# Jampy, Postgres, Gunicorn i Nginx na Ubuntu

- Ažurirano 3. decembra 2025. god,
- Radosav

## Instalacija produkcionog Ubuntu servera

- Instaliraj `Ubuntu server` na virtuelnu ili fizičku mašinu Ubuntu 64 OS, sa najmanje 2GB Ram-a i 1 procesor-om.

- Pri instalaciji obavezno čekiraj `OpenSSH` server.

- Ažuriraj izvore paketa i podigni na poslednje verzije:
  
  ```sh
  sudo apt update && sudo apt upgrade -y
  ```

- Aktiviraj `firewall` i dozvoli `OpenSSH`
  
  ```sh
  sudo ufw enable
  sudo ufw allow OpenSSH
  ```

- Ako je NAT-ovana mreža (VM) obavezno NAT-uj `guest port:22` na, recimo
  `localhost:2022`.

- Iako ne traži reboot

  ```sh
  sudo shutdown -r now
  ```

- Proveri da li je git instaliran sa git --version, ako nije instaliraj sa:
  
  ```sh
  sudo apt install git -y
  ```

- Instaliraj MidnightCommander:

  ```sh
  sudo apt install mc -y
  ```

- Promeni vremensku zonu, uključi sinhronizaciju vremena

  ```sh
  sudo timedatectl set-timezone Europe/Belgrade
  sudo timedatectl set-ntp on
  ```

- Promeni locale

  ```sh
  sudo dpkg-reconfigure locales
  ```

- Dodaj nove `locale sr_RS@latin UTF-8` i postavi ih za default.

- Reboot

  ```sh
  sudo shutdown -r now
  ```

- Generiši par SSH ključeva za pristup serveru

  ```sh
  ssh-keygen -t rsa -b 4096 -C "rrad0812@gmail.com"
  ```

  Pri generisanju obavezno unesi `passpharse`. Za pristup serveru iskopiraj javni ključ. Za GitHub iskopiraj javni ključ.

## Instaliranje paketa iz Ubuntu repozitorijuma

Da biste započeli proces, preuzećete i instalirati sve potrebne stavke iz Ubuntu repozitorijuma. Kasnije ćete koristiti Python menadžer paketa `pip` da biste instalirali dodatne komponente.

```sh
sudo apt update
sudo apt install python3-venv python3-pip python3-dev postgresql postgresql-contrib nginx curl
```

## PostgreSQL

### Uvod u PostgreSQL

- PostgreSQL se može instalirati na Ubuntu koristeći podrazumevana spremišta i menadžer paketa `apt`.
- PostgreSQL upravlja dozvolama koristeći koncept `role`, koje mogu predstavljati korisnike ili grupe.
- Podrazumevana rola `postgres` se kreira tokom instalacije sa punim `superuser` privilegijama i obično se koristi za početnu administraciju i podešavanje drugih rola i baza podataka.
- Podrazumevano, Postgres koristi `peer` autentifikaciju za lokalne veze, koja daje pristup roli baze ako ima isto ime kao aktivni korisnik Linux sistema.
- Nove role i baze možete kreirati iz komandne linije pomoću alatki `createuser` i `createdb`, koje se obično pokreću pod Linux korisnikom `postgres`.
- `psql` je primarni terminal za rad sa PostgreSQL-om; koristi se za povezivanje na određenu bazu kao određeni korisnik i izvršavanje SQL upita.
- Iako je `peer` autentifikacija podrazumevana, PostgreSQL možete podesiti da zahteva lozinke za lokalne ili udaljene veze — kreiranjem role sa `password` i ažuriranjem `pg_hba.conf`.
- Vodič obuhvata i važne administrativne teme:
  - podešavanje performansi servera,
  - strategije bekapa i vraćanja,
  - metode za rešavanje uobičajenih problema.

### Instaliranje i korišćenje PostgreSQL-a

Da biste instalirali PostgreSQL, prvo osvežite lokalni indeks paketa vašeg Ubuntu servera:

```sh
sudo apt update
```

Zatim instalirajte PostgresSQL paket zajedno sa `-contrib` paketom koji dodaje neke dodatne uslužne programe i funkcionalnost:

```sh
sudo apt install postgresql postgresql-contrib
```

Možete proveriti verziju tako što ćete pokrenuti sledeću komandu:

```sh
psql --version
```

Uverite se da je usluga pokrenuta:

```sh
sudo systemctl status postgresql.service
```

Ako nije onda iskoristite komandu:

```sh
sudo systemctl start postgresql.service
```

Ako servis nije omogućen na pokretanju OS-a:

```sh
sudo systemctl enable postgresql.service
```

#### Osnovno podešavanje PostgreSQL uloga i baza podataka

Postgres podrazumevano koristi koncept koji se zove `role` za rukovanje autentifikacijom i autorizacijom. One su, na neki način, slične korisnicima i grupama u stilu Unix-a.

> [!Note]  
> Nakon instalacije, Postgres je podešen da koristi `peer` autentifikaciju, što znači da povezuje
`postgres` rolu sa odgovarajućim Unix/Linux sistemskim nalogom. Ako rola postoji u Postgresu, Unix/Linux korisničko ime sa istim imenom može da se prijavi kao ta uloga.

Procedura instalacije kreirala je korisnički nalog pod nazivom `postgres` koji je povezan sa podrazumevanom rolom PostgreSQL-a. Postoji nekoliko načina da koristite ovaj nalog za pristup PostgreSQL-u:

- Jedan od načina je da pređete na `postgres` nalog na vašem serveru tako što ćete pokrenuti sledeću komanda:

  ```sh
  sudo -i -u postgres 
  ```
  
  Ovim prelazite/logujete se na postgres nalog na računaru. Zatim možete pristupiti PostgreSQL alatu komandne linije `psql` pokretanjem:
  
  ```sh
  psql 
  ```

  Ovo će vas prijaviti u PostgreSQL `psql` prompt i od tog trenutka možete odmah da radite sa
sistemom za upravljanje bazom.
  
  Da izađete iz PostgreSQL `psql` prompta, pokrenite sledeće:
  
  ```sh
  \q
  ```

  Ovo će vas vratiti na postgres Linux komandni prompt. Da se vratite na vaš uobičajeni korisnički
nalog sistema, pokrenite komandu exit:
  
  ```sh
  exit
  ```

- Drugi način da se povežete na Postgres `psql` prompt je da pokrenete komandu kao `postgres` nalog direktno pomoću `sudo`:

  ```sh
  sudo -u postgres psql
  ```
  
  Ovo će vas prijaviti direktno na PostgreSQL `psql` prompt bez međufaze bash školjke.
  
  Ponovo, možete izaći iz interaktivne Postgres sesije pokretanjem:
  
  ```sh
  \q
  ```

#### Kreiranje nove role

- Ako ste prijavljeni kao `postgres`, novu rolu možete kreirati pokretanjem sledeće komande:

  ```sql
  createuser --interactive
  ```

- Ako umesto toga želite da koristite `sudo` za svaku komandu bez prebacivanja sa svog uobičajenog naloga, pokrenite:

  ```sh
  sudo -u postgres createuser --interactive
  ```

- U oba slučaja skripta će vam postaviti nekoliko izbora i na osnovu vaših odgovora izvršiće odgovarajuće Postgres komande da kreira korisnika po vašim specifikacijama.

  ```sh
  Enter name of role to add: sammy
  Shall the new role be a superuser? (y/n) y
  ```

#### Kreiranje nove baze podataka

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

#### SQL metoda za kreiranje novog korisnika i baze

`createuser` i `createdb` komandne alatke su zgodni pomoćnici. Međutim, za više kontrole možete iste radnje izvršiti direktno u PostgreSQL-u pomoću SQL komandi. Ovaj pristup je često jasniji kada odmah postavljate lozinke ili dodeljujete specifične privilegije.

- **Povežite se** kao administratorski **postgres** korisnik:

  ```sh
  sudo -u postgres psql
  ```

  Kada ste u PostgreSQL promptu možete koristiti CREATE ROLE i CREATE DATABASE da podesite novog korisnika i bazu.

- **Kreirajte novu rolu (korisnika)**: Dok je `createuser` interaktivna komandna alatka, CREATE ROLE vam omogućava da sve definišete jednom naredbom. Da kreirate korisnika po imenu "sammy" koji može da se prijavi (LOGIN) i ima lozinku, pokrenite:

  ```sql
  CREATE ROLE sammy WITH LOGIN PASSWORD 'your_strong_password';
  ```

  Ako ovom korisniku treba i pravo da kreira baze, možete ga dodeliti istovremeno:
  
  ```sql
  CREATE ROLE sammy WITH LOGIN PASSWORD 'your_strong_password' CREATEDB;
  ```

  Ovo daje podskup superuser privilegija, konkretno omogućava korisniku da kreira nove baze. Dok odgovor "yes" na pitanje o superuser statusu u interaktivnom pomoćniku
takođe daje ovu dozvolu, SUPERUSER je mnogo šira i rizičnija rola.

  Nakon toga, izmenićete nekoliko parametara veze za korisnika koga ste upravo kreirali. Ovo će ubrzati rad baze podataka tako da nećete morati da tražite i podešavate 
ispravne vrednosti svaki put kada se veza uspostavi:

  - Postavićete podrazumevano kodiranje znakova na `UTF8`, što "Jampy" očekuje.
  - Takođe podešavate podrazumevanu šemu izolacije transakcija na `read committed`, što blokira čitanja iz nepotvrđenih transakcija.
  - Na kraju, podešavate `vremensku zonu`. Podrazumevano, "Jampy" projekti će biti podešeni da koriste `UTC+1`.
  
  ```sql
  ALTER ROLE sammy SET client_encoding TO 'utf8';
  ALTER ROLE sammy SET default_transaction_isolation TO 'read committed';
  ALTER ROLE sammy SET timezone TO 'UTC+1';
  ```

- **Kreirajte novu bazu**: Zatim napravite bazu. Dobra praksa je da vlasništvo nove baze dodelite ulozi koju ste upravo kreirali.

  ```sql
  CREATE DATABASE sammydb OWNER sammy;
  ```

  Da dodelite korisniku sammy dozvolu da se poveže na novu bazu, možete pokrenuti:

  ```sql
  GRANT ALL PRIVILEGES ON DATABASE sammydb TO sammy;
  ```

  Ovo dodeljuje privilegije na nivou baze kao CONNECT, ali ne i dozvole na specifičnim tabelama.

  > [!Napomena]  
  > Ovo automatski ne dodeljuje dozvole za pregled ili izmenu tabela u bazi; potrebne su dodatne GRANT komande nad šemom ( public ) i samim tabelama.

- **Izlaz iz postgres sesije**: Sada možete napustiti `psql` prompt za `postgres` korisnika.

  ```sh
  \q
  ```

Pošto ste postavili lozinku, ovaj novi korisnik može da se autentifikuje metodama koje nisu `ident`, što je neophodno za udaljene konekcije.

#### Otvaranje postgres prompta sa novom rolom

Da biste se prijavili koristeći `ident` ( `peer` ) autentifikaciju, potreban vam je Linux korisnik sa istim imenom kao Postgres rola i baza.

Ako nemate odgovarajući Linux korisnički nalog, možete ga kreirati komandom `adduser`. Ovo radite sa svog `non-root` naloga koji ima `sudo` privilegije (dakle ne kao `postgres` korisnik):

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

Na ovaj način korisnik (sammy) se prijavu na postges bazu podataka. Kada ste prijavljeni, trenutne informacije o konekciji možete proveriti komandom:

```sql
\conninfo
```

```sql
You are connected to database "sammy" as user "sammy" via socket in "/var/run/postgresql" at port "5432".
```

Ovo je korisno ako se povezujete na ne-podrazumevane baze ili sa ne-podrazumevanim korisnicima.

### Bekap i vraćanje PostgreSQL baze podataka

Zaštita podataka je primarna odgovornost administracije baze. Redovni, verifikovani bekapi su najefikasnija odbrana od oštećenja podataka, kvara hardvera ili ljudske greške. PostgreSQL obezbeđuje skup robusnih alatki komandne linije za pravljenje logičkih bekapa i povraćaj.

**Napomena o prostoru na disku** : Pre početka bekapa, uverite se da imate dovoljno prostora. Za obične tekstualne (.sql) dump-ove, planirajte slobodan prostor približno 100% veličine baze. Za kompresovane custom (.dump) fajlove, planirajte 30–50% veličine baze, u zavisnosti od kompresibilnosti podataka.

Postoje dve glavne kategorije bekapa:

- **Logički bekapi (SQL dump-ovi)** : Sastoje se od SQL komandi koje, kada se izvrše, ponovo kreiraju objekte baze (kao tabele i indekse) i podatke. Prave se pomoću `pg_dump` i `pg_dumpall` alatki. Metoda je fleksibilna i prenosiva između različitih verzija PostgreSQL-a.

- **Fizički bekapi (na nivou fajl sistema)** : Podrazumeva kopiranje fizičkih fajlova podataka iz direktorijuma podataka PostgreSQL-a. Tipično se koristi za Point-in-Time Recovery (PITR), naprednu postavku obrađenu kasnije u ovom poglavlju.

#### Pravljenje logičkih bekapa pomoću pg_dump

Alatka `pg_dump` pravi bekap jedne baze. Ne bekapuje globalne objekte poput rola ili tablespace-ova. Veoma je fleksibilna i može se pokretati nad aktivnom bazom bez blokiranja čitača ili pisača.

**Uobičajene opcije za pg_dump** :

Izlaz `pg_dump` možete kontrolisati pomoću više opcija komandne linije:

- **-F c** (format: custom) : Preporučen format za većinu bekapa. Proizvodi kompresovanu binarnu arhivu (nečitljivu za čoveka) koja omogućava paralelno vraćanje i ređanje/izuzimanje objekata tokom vraćanja.
- **-F p** (format: plain) : Podrazumevani format. Izbacuje veliki ".sql" fajl. Glavni nedostatak je da se mora vratiti ođednom i da ne podržava paralelno vraćanje.
- **-s** (samo šema) : Bekapuje samo strukturu baze (tabele, poglede, indekse) bez podataka.
- **-a** (samo podaci) : Bekapuje samo podatke, pod pretpostavkom da šema već postoji na odredištu.
- **--exclude-table=TABLE_NAME** : Isključuje konkretnu tabelu iz bekapa. Korisno za preskakanje velikih, privremenih ili manje bitnih tabela.
- **-f FILENAME**: Navodi izlazni fajl.
- **-U USER** : Navodi PostgreSQL korisnika pod kojim se konektuje.

Ovo je preporučena komanda za većinu zadataka bekapa. Povezuje se na bazu mydatabase kao korisnik `postgres` i kreira kompresovani fajl "mydatabase.dump".

```sh
pg_dump -U postgres -F c -f mydatabase.dump mydatabase
```

#### Vraćanje logičkih bekapa

**Vraćanje iz custom arhive (.dump)**:

Koristite alatku `pg_restore`. Obezbeđuje značajnu fleksibilnost, uključujući paralelno vraćanje i provere pre izvršavanja.

Umesto ručnog kreiranja baze, najbolja praksa je koristiti flag `--create`. On govori `pg_restore` da iz arhive izda CREATE DATABASE komandu. Za to se morate povezati na neku postojeću bazu, poput podrazumevane `postgres` baze.

Sledeća komanda se povezuje na postgres bazu, briše i ponovo kreira mydatabase ( zbog `--clean` i `--create` ) i koristi 4 paralelna procesa za vraćanje.

```sh
pg_restore -U postgres -d postgres --create --clean -j 4 mydatabase.dump
```

Ovde je:

- **-d postgres** : Povezuje se na "postgres" bazu radi izdavanja create komande.

- **--create** : Kaže `pg_restore` da kreira ciljnu bazu (npr. mydatabase) pre vraćanja u nju.

- **--clean** : Uz `--create` briše i ponovo kreira celu ciljnu bazu. Ako se koristi samostalno (vraćanje u postojeću bazu), briše postojeće objekte pre njihovog ponovnog kreiranja.

- **-j 4**: Koristi 4 paralelna posla za vraćanje.

Povezujemo se na postgres bazu jer `pg_restore` treba vezu ka postojećoj bazi na serveru da bi izdao CREATE DATABASE komandu za naš novi cilj.

**Napomena o paralelnom vraćanju** : Flag `-j` primarno paralelizuje učitavanje podataka i izgradnju indeksa i dostupan je samo za `custom` format arhive. Neke operacije, poput kreiranja baze ili vraćanja objekata sa složenim zavisnostima, ostaju serijske. Ne može se koristiti sa opcijom `--single-transaction`.
  
**Vraćanje iz običnog tekstualnog fajla (.sql)**:

Koristite standardni psql klijent. Ova metoda zahteva da ciljna baza već postoji.

- Kreirajte bazu:

  ```sh
  createdb -U postgres mydatabase_new
  ```

- Prosledite ".sql" fajl u psql:

  ```sh
  psql -U postgres -d mydatabase_new < mydatabase.sql
  ```

#### Bekap svih baza (pg_dumpall)

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

#### Bekap samo rola i globalnih objekata

Uobičajen i koristan obrazac je bekap samo globalnih objekata. Radi se flagom `--globals-only` i predstavlja efikasan način replikacije korisničkih dozvola između servera.

```sh
pg_dumpall -U postgres --globals-only > roles.sql
```

#### Napomena o fizičkim bekapima i Point-in-Time Recovery (PITR)

Iako se ovaj vodič fokusira na logičke bekape, PostgreSQL podržava i fizičke bekape. Ovaj napredni pristup, poznat kao Point-in-Time Recovery (PITR), kombinuje bazni bekap na nivou fajl sistema (kreiran pomoću `pg_basebackup`) sa kontinuiranom arhivom WAL (Write-Ahead Log) fajlova.

Ova tehnika omogućava vraćanje baze na bilo koji tačno definisan trenutak (npr. „na 15:05, neposredno pre slučajnog brisanja podataka“), a ne samo na vreme bekapa. Najrobustnija je strategija bekapa za kritične produkcione sisteme, ali zahteva dodatnu konfiguraciju, uključujući postavljanje `wal_level` na replica ili više (ne minimal) u "postgresql.conf" i podešavanje "archive_command" za arhiviranje WAL-a.

#### Rezime strategije bekapa i vraćanja

Sledeća tabela sumira strategiju bekapa i vraćanja:

Alat              |    Akcija   |      Izlaz     |        Slučaj upotrebe
------------------|-------------|----------------|--------------------------
**pg_dump**       |    Bekap    | .sql ili .dump | Bekap jedne baze. Veoma fleksibilan.
**pg_restore**    |    Vraćanje |       N/A      | Vraćanje .dump arhive.
**psql**          |    Vraćanje |       N/A      | Vraćanje običnog .sql fajla.
**pg_dumpall**    |    Bekap    |      .sql      | Bekap svih baza i globalnih rola.
**pg_basebackup** |    Bekap    |  Data fajlovi  | Kreira bazni bekap za fizičku/PITR strategiju.

#### Automatizacija i najbolje prakse za bekap

Plan bekapa nije potpun dok nije automatizovan, verifikovan i bezbedan.

- **Automatizujte bekape** : Koristite standardne Linux alatke poput `cron`-a da zakazujete `pg_dump` ili `pg_dumpall` komande za noćno pokretanje.

- **Bezbedno rukujte lozinkama** : Za pokretanje bekapa u neinteraktivnim skriptama, ne hardkodirajte lozinke. Koristite fajl ".pgpass". Kreirajte ga na `~/.pgpass` u home direktorijumu korisnika (npr. "/var/lib/postgresql/.pgpass"). Fajl mora imati stroge `0600` dozvole, inače će ga PostgreSQL ignorisati. Format je `hostname:port:database:user:password`. Možete koristiti `*` kao džoker za bilo koje polje, npr. `localhost:5432:*:postgres:your_password`.

- **Testirajte vraćanja** : Bekap je koristan samo ako može da se uspešno vrati. Redovno vežbajte vraćanje bekapa na zasebnom, ne-produkcionom serveru radi verifikacije integriteta.

- **Verifikujte bekape** : Nakon kreiranja bekapa, dodajte korak u skripti koji proverava da veličina fajla nije nula. Za custom formate, možete pokrenuti `pg_restore -l mydatabase.dump &> /dev/null` (što ispisuje sadržaj u "/dev/null") i preko exit koda potvrditi da arhiva nije korumpirana.

- **Čuvajte bekape udaljeno** : Ne skladištite bekape na istom serveru kao bazu. Potpuni pad servera bi izgubio oba. Kopirajte bekape na udaljenu lokaciju poput DigitalOcean Spaces ili na drugi server kao završni korak skripte.

### Uobičajeni problemi i rešavanje sa PostgreSQL-om na Ubuntu-u

Pokretanje PostgreSQL-a na Ubuntu-u može doneti specifične izazove, naročito korisnicima koji su novi u njegovoj arhitekturi na Debian-baziranim sistemima. Većina grešaka spada u predvidljive kategorije:

- konflikti pri instalaciji,
- pravila autentifikacije,
- dozvole fajlova i
- upravljanje servisom.

Pre rešavanja problema, vaš najvažniji alat je log fajl PostgreSQL-a. Na standardnim Ubuntu instalacijama nalazi se na "/var/log/postgresql/postgresql-[version]-main.log". Uvek prvo proverite ovaj log, jer daje konkretne poruke o greškama koje usmeravaju dijagnostiku.

#### Problemi pri instalaciji

Ovi problemi se obično javljaju tokom početnog procesa `apt` instalacije ili kada `initdb` prvi put pokušava da kreira novi klaster baze.

**Konflikti paketa ili neusaglašene verzije** :

Ovaj problem nastaje kada sistem pokušava da instalira PostgreSQL pakete iz dva izvora, kao što su podrazumevani Ubuntu repozitorijum i zvanični PGDG repozitorijum. Ubuntu repozitorijumi često sadrže starije, stabilne verzije, dok PGDG obezbeđuje najnovija izdanja. Mešanje može dovesti do pokvarenih zavisnosti.

**Rešenje** :

- **Proverite izvore** : Koristite `apt-cache policy postgresql` da vidite koje su verzije dostupne i iz kog repozitorijuma.
- **Izaberite jedan izvor** : Preporučuje se korišćenje PGDG repozitorijuma
  za najaktuelnije verzije. Ako dodate PGDG, možda ćete morati da koristite `apt pinning` da biste mu dali prioritet nad podrazumevanim Ubuntu paketima.
- Popravite pokvarene instalacije: Ako ste već u konfliktu, `sudo apt-get -f install` može pomoći. Ako ne, možda treba potpuno ukloniti sve PostgreSQL pakete ( `sudo apt-get purge "postgresql-*"` ) i ponovo instalirati iz jednog, čistog izvora.

**Port 5432 je već u upotrebi** :

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

Greške poput `invalid locale name` ili `could not determine default locale` tokom inicijalne postavke klastera ( `initdb` ) znače da podešavanja lokalizacije na vašem Ubuntu sistemu nedostaju ili su pogrešno podešena. PostgreSQL nasleđuje ova podešavanja iz OS-a da bi odredio redosled sortiranja (collation), klasifikaciju karaktera i formatiranje.

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

#### Problemi sa autentifikacijom

Ovo su najčešći problemi sa kojima se korisnici suočavaju nakon uspešne instalacije.

**Neuspeh peer autentifikacije** :

Ovo je najčešća greška. Znači da pokušavate lokalno povezivanje, a vaše trenutno Ubuntu korisničko ime se ne poklapa sa nazivom PostgreSQL role koju pokušavate da koristite.

Podrazumevano, PostgreSQL na Ubuntu-u koristi `peer` autentifikaciju za lokalne veze. Ova metoda pita operativni sistem: "Koje je korisničko ime procesa koji se povezuje?" i dozvoljava vezu ako se to ime poklapa sa traženom rolom baze.

Na primer, ako ste prijavljeni kao "myuser" i pokrenete `psql -U postgres`, veza će pasti jer OS prijavljuje vaše korisničko ime kao "myuser", a ne postgres.

**Rešenje** :

Ispravan način povezivanja kao postgres superuser je da prvo preuzmete identitet postgres sistemskog korisnika koristeći sudo.

```sh
sudo -u postgres psql
```

Ako želite da promenite ovo ponašanje, morate postaviti lozinku za `postgres` rolu i zatim urediti "pg_hba.conf" da umesto `peer` koristi `md5` ili `scram-sha-256`.

**Pogrešna konfiguracija "pg_hba.conf"** :

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

#### Problemi sa dozvolama

Ove greške se često pojavljuju kao `Permission denied` u logovima ili pri pokušaju povezivanja.
Nije moguće pristupiti socket fajlu.

Greške poput `Could not connect to server: No such file or directory` ili `Permission denied` pri referenciranju "/var/run/postgresql/.s.PGSQL.5432" obično znače jedno od sledećeg:

- **Servis ne radi** : Socket fajl se uklanja kada se servis zaustavi. Proverite status servisa komandom sudo `systemctl status postgresql`.

- **Korisničke dozvole**: Direktorijum socket-a je u vlasništvu `postgres` korisnika i dostupan `postgres` grupi. Ako vaš sistemski korisnik nije u `postgres` grupi, ne možete koristiti socket.

**Rešenje** :

Dodajte svog korisnika u `postgres` grupu. Morate se ođaviti i ponovo prijaviti da bi izmena stupila na snagu.

  ```sh
  sudo usermod -a -G postgres $USER
  ```

**Dozvole direktorijuma podataka** :

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

#### Upravljanje servisom

Ovi problemi se odnose na pokretanje, zaustavljanje i upravljanje samim postgresql servisom.

**Servis ne uspeva da se pokrene nakon izmena konfiguracije** :

Ako se servis ne pokrene odmah nakon uređivanja "postgresql.conf" ili "pg_hba.conf", verovatno ste uveli sintaksnu grešku.

**Rešavanje** :

- **Proverite logove** : Najbrži način da nađete grešku. `sudo journalctl -u postgresql` ili konkretan log na "/var/log/postgresql/postgresql-[version]-main.log" često će navesti tačan broj linije sa nevažećim podešavanjem.

  Uobičajene greške u postgresql.conf:

  - Neobuhvaćene vrednosti stringa (npr. `log_destination = stderr` je tačno; `log_destination = /var/log/my.log` je pogrešno, treba `var/log/my.log`).
  
  - Nevažeće vrednosti podešavanja (npr. `shared_buffers = 10GB` kada imate samo `8GB` RAM-a).

**Zbrka oko više klastera** :

Paket `postgresql-common` na Ubuntu-u uključuje skup wrapper skripti (`pg_lsclusters`, `pg_ctlcluster`, `pg_createcluster`) koje omogućavaju pokretanje više, odvojenih PostgreSQL instanci (klastera) na istom računaru, često različitih verzija.

Ovo je moćna funkcija, ali može biti zbunjujuća. Možda uređujete konfiguracioni fajl za verziju 18, a zapravo se povezujete na verziju 13, pa pomislite da se "promene ne primenjuju".

**Rešavanje** :

Ispišite sve klastere: Uvek počnite odavde da vidite šta radi, na kom portu i gde su konfiguracioni fajlovi.

```sh
pg_lsclusters

# Ver Cluster Port Status Owner    Data directory                       Log file
# 13  main    5432 down   postgres /var/lib/postgresql/13/main         ...
# 18  main    5433 online postgres /var/lib/postgresql/18/main         ...
```

Upravljajte konkretnim klasterom: Pri pokretanju, zaustavljanju ili reload-u, uvek navedite klaster koji želite da menjate:

```sh
sudo pg_ctlcluster 18 main reload
```

#### Ograničenja resursa

Ove greške se javljaju kada PostgreSQL traži više resursa (poput memorije) nego što je operativni sistem podešen da obezbedi.

**Greške deljene memorije** :

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

### Česta postavljana pitanja o PostgreSQL

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

- **(Opciono) Dodela dozvola za buduće objekte**: Gornje komande utiču samo na postojeće tabele. Da automatski dodelite dozvole za nove tabele koje će se tek kreirati (npr. od strane admina), morate izmeniti podrazumevane privilegije:

  ```sql
  ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO sammy;
  ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO sammy;
  ```

#### Osnovne PostgreSQL psql komande

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

#### Povezivanje korisnika (lokalna peer vs. TCP/IP lozinka)

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

## Virtuelno okruženje i novi Jampy projekat

Sada kada imate spremnu bazu podataka, možete početi sa dobijanjem ostalih zahteva za projekat. Instaliraćete Pajton zahteve u virtuelnom okruženju radi lakšeg upravljanja.

### Kreiranje virtualnog okruženja

Prvo, kreirajte i promenite direktorijum u kome možete čuvati datoteke projekta:

```sh
mkdir ~/project
cd ~/project
```

Unutar direktorijuma projekta, kreirajte virtuelno okruženje u Pajtonu tako što ćete otkucati:

```sh
python3 -m venv .venv
```

Ovo će kreirati direktorijum pod nazivom ".venv" unutar vašeg "project" direktorijuma. Unutar njega će biti instalirana lokalna verzija Pajtona i lokalna verzija `pip` za upravljanje paketima.
Pre instaliranja Pajton zahteva vašeg projekta, potrebno je da aktivirate virtuelno okruženje:

```sh
source .venv/bin/activate
```

Vaš prompt bi trebalo da se promeni kako bi naznačio da sada radite u virtuelnom okruženju Pajtona.

```sh
(.venv)user@host:~/project$
```

Sa aktivnim virtuelnim okruženjem, instalirajte `jampy`, `gunicorn` i `psycopg2` - PostgreSQL adapter sa lokalnom instancom `pip`:

**Napomena**: Kada je virtuelno okruženje aktivirano, kada vaš prompt ima (.venv) koristite `pip` umesto `pip3`, čak i ako koristite Python 3.

```sh
pip install jam-py-v5 gunicorn psycopg2-binary
```

Sada bi trebalo da imate sav softver potreban za pokretanje Jampy projekta.

### Kreiranje i konfigurisanje novog Jampy projekta

Pokrenite komandu

./jam-project.py

da bi ste kreirali novi Jampy projekat.

Ako ste pratili "Početni vodič za podešavanje servera", trebalo bi da imate `ufw` zaštitni zid koji štiti vaš server. Da biste testirali razvojni server, potrebno je da omogućite pristup portu koji ćete koristiti.

Napravite izuzetak za port 8080:

```sh
sudo ufw allow 8080
```

Konačno, možete testirati svoj projekat pokretanjem Jampy razvojnog servera pomoću:

```sh
./server.py
```

U vašem veb pregledaču posetite ime domena ili IP adresu vašeg servera, a zatim :8080: <http://server_domain_or_IP:8080>

Sada je potrebno da završite početnu konfiguraciju projekta na: <http://server_domain_or_IP:8080/builder.html>

### Migracija razvojnog projekta na produkciju

- Na dev. serveru iz Jampy buildera izvezite meta podatke u zip fajl.

- Prenesite zip fajl sa dev. na produkcioni server pomoću `scp` komande:
  
  SCP je terminalski alat za kopiranje fajlova i direktorijuma između lokalnog i udaljenog hosta, ili između dva udaljena hosta, korišćenjem SSH za transfer fajlova i autentifikaciju.

  - Za kopiranje fajla sa lokalne mašine na udaljeni server:

    ```sh
    scp /path_to_local_file.txt username@remote_host:/path_to_remote_destination/
    ```

  - Za kopiranje fajla sa sa udaljenog servera na lokalnu mašinu:

    ```sh
    scp username@remote_host:/path_to_remote_file.txt /path_to_local_destination/
    ```

  Ako želite da kopirate kompletan direktorijum samo dodajte jedno `-r` posle `scp` komande.

- Uvezite zip fajl sa metapodacima razvoja u proizvodni projekat.

### Prijava administratora

Nakon završetka konfiguracije projekta možete pristupiti i ulogovati se kao admin na podrazumevanom Jampy admin interfejsu na <http://server_domain_or_IP:8080>.

## Gunicorn

### Testiranje gunicorna

Poslednja stvar koju treba da uradite pre nego što napustite svoje virtuelno okruženje jeste da testirate Gunicorn kako biste bili sigurni da može da služi aplikaciji.

To možete učiniti tako što ćete:

- prvo zaustaviti razvojni web server koji dolazi kao deo Jampy paketa sa `Ctrl+C`
- ući iz direktorijuma projekta i koristiti komandu `gunicorn` za učitavanje `wsgi` modula projekta:

```sh
cd ~/project
gunicorn --bind 0.0.0.0:8080 wsgi.py
```

Ovo će pokrenuti Gunicorn na istom interfejsu na kojem je radio Jampy razvojni server. Možete se vratiti i ponovo testirati aplikaciju u svom pregledaču.

**Napomena** : Interfejs neće imati primenjen nijedan od stilova jer Gunicorn ne zna kako da pronađe statički `css` sadržaj odgovoran za ovo.

Prosledili ste `wsgi` modul Gunicorn-u tako što ste naveli relativnu putanju fajla do Jampy `wsgi.py` datoteke, koja je ulazna tačka za vašu aplikaciju, koristeći Python-ovu sintaksu modula. Unutar ove datoteke, definisana je funkcija `application`, koja se koristi za komunikaciju sa aplikacijom.

Kada završite sa testiranjem, pritisnite `CTRL-C` u prozoru terminala da biste zaustavili Gunicorn.

Sada ste završili sa konfigurisanjem vaše Jampy aplikacije. Možete se vratiti iz virtuelnog okruženja:

```sh
deactivate
```

Indikator virtuelnog okruženja u vašem promptu će biti uklonjen.

### Gunicorn sistemski socketi i servisne datoteke

Testirali ste da Gunicorn može da komunicira sa Jampy aplikacijom, ali sada bi trebalo da implementirate robusniji način pokretanja i zaustavljanja aplikacijskog servera. Da biste to postigli, napravićete `systemd` servis i `socket` datoteku.

Gunicorn `socket` će biti kreiran pri pokretanju sistema i slušaće veze. Kada se veza uspostavi, `systemd` će automatski pokrenuti Gunicorn proces za obradu veze.

Počnite tako što ćete kreirati i otvoriti sistemsku `socket` datoteku za Gunicorn sa `sudo` privilegijama:

```sh
sudo nano /etc/systemd/system/gunicorn.socket
```

Unutra ćete kreirati `[Unit]` odeljak za opisivanje socketa, `[Socket]` odeljak za definisanje lokacije socketa i `[Install]` odeljak da biste bili sigurni da je socket kreiran u pravo vreme:

"/etc/systemd/system/gunicorn.socket"

```sh
[Unit]
Description=gunicorn socket

[Socket]
ListenStream=/run/gunicorn.sock

[Install]
WantedBy=sockets.target
```

Sačuvajte i zatvorite datoteku kada završite.

Zatim, kreirajte i otvorite sistemsku servisnu datoteku za Gunicorn sa `sudo` privilegijama u vašem uređivaču teksta. Ime servisne datoteke treba da se podudara sa imenom `socket` datoteke, sa izuzetkom ekstenzije:

```sh
sudo nano /etc/systemd/system/gunicorn.service
```

Počnite sa `[Unit]` odeljkom, koji se koristi za određivanje metapodataka i zavisnosti. Ovde stavite opis servisa i recite `init` sistemu da ga pokrene tek nakon `network.target`. Pošto se vaš servis oslanja na socket iz datoteke "gunicorn.socket", potrebno je da uključite direktivu `Requires` koja označava tu vezu:

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target
```

Zatim ćete otvoriti odeljak [Service]. Navedite korisnika i grupu pod kojima želite da se proces pokrene. Daćete svom redovnom korisničkom nalogu vlasništvo nad procesom, jer on poseduje sve relevantne datoteke. Daćete grupno vlasništvo nad grupom "www-data" kako bi Nginx mogao lako da komunicira sa Gunicorn-om.

Zatim ćete mapirati radni direktorijum i navesti komandu koja će se koristiti za pokretanje servisa. U ovom slučaju, morate navesti punu putanju do izvršne datoteke `gunicorn`, koja je instalirana u našem virtuelnom okruženju. Zatim ćete povezati proces sa Unix socketom koji ste kreirali u direktorijumu `/run` kako bi proces mogao da komunicira sa Nginx-om. Sve podatke ćete evidentirati na standardnom izlazu kako bi proces `journald` mogao da prikuplja Gunicorn logove. Ovde možete navesti i bilo koja opcionalna podešavanja za Gunicorn. Na primer, u ovom slučaju ste naveli 3 radna procesa:

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target

[Service]
User=sammy
Group=www-data
WorkingDirectory=/home/sammy/project
ExecStart=/home/sammy/project/.venv/bin/gunicorn \
          --access-logfile - \
          --workers 3 \
          --bind unix:/run/gunicorn.sock \
          wsgi.py:application
```

Konačno, dodaćete `[Install]` odeljak. Ovo će reći `systemd`-u sa čim da poveže ovu uslugu ako je omogućite da se pokreće pri pokretanju sistema. Želite da se ova usluga pokrene kada je pokrenut i radi običan višekorisnički sistem:

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target

[Service]
User=sammy
Group=www-data
WorkingDirectory=/home/sammy/project
ExecStart=/home/sammy/project/.venv/bin/gunicorn \
          --access-logfile - \
          --workers 3 \
          --bind unix:/run/gunicorn.sock \
          wsgi.py:application

[Install]
WantedBy=multi-user.target
```

Time je vaša sistemska servisna datoteka "gunicorn.service" završena. Sačuvajte je i zatvorite sada.

Sada možete pokrenuti i omogućiti "gunicorn.socket". Ovo će kreirati datoteku socketa "/run/gunicorn.sock" u sadašnjem trenutku i pri pokretanju sistema. Kada se uspostavi veza sa tim socket-om, `systemd` će automatski pokrenuti `gunicorn.service` da bi je obradio:

```sh
sudo systemctl start gunicorn.socket
sudo systemctl enable gunicorn.socket
```

Možete potvrditi da je operacija bila uspešna proverom datoteke socketa.

### Provera datoteke gunicorn.socket

Proverite status procesa da biste saznali da li je mogao da se pokrene:

```sh
sudo systemctl status gunicorn.socket
```

Trebalo bi da dobijete izlaz poput ovog:

```sh
● gunicorn.socket - gunicorn socket
     Loaded: loaded (/etc/systemd/system/gunicorn.socket; enabled; vendor preset: enabled)
     Active: active (listening) since Mon 2022-04-18 17:53:25 UTC; 5s ago
   Triggers: ● gunicorn.service
     Listen: /run/gunicorn.sock (Stream)
     CGroup: /system.slice/gunicorn.socket

Apr 18 17:53:25 Jampy systemd[1]: Listening on gunicorn socket.
```

Zatim, proverite postojanje datoteke "gunicorn.sock" u "/run" direktorijumu:

```sh
file /run/gunicorn.sock
```

```sh
/run/gunicorn.sock: socket
```

Ako `systemctl status` komanda je ukazala na grešku ili ako ne pronađete "gunicorn.sock" datoteku u direktorijumu, to je indikacija da "gunicorn.sock" nije mogao biti pravilno kreiran. Proverite logove Gunicorn socketa tako što ćete otkucati:

```sh
sudo journalctl -u gunicorn.socket
```

Ponovo pogledajte "/etc/systemd/system/gunicorn.sock" datoteku da biste rešili eventualne probleme pre nego što nastavite.

### Testiranje aktivacije socket-a

Trenutno, ako ste tek pokrenuli gunicorn.socket jedinicu, `gunicorn.service` još uvek neće biti aktivna jer socket još nije primio nikakve veze. Ovo možete proveriti tako što ćete otkucati:

```sh
sudo systemctl status gunicorn
```

```sh
○ gunicorn.service - gunicorn daemon
     Loaded: loaded (/etc/systemd/system/gunicorn.service; disabled; vendor preset: enabled)
     Active: inactive (dead)
TriggeredBy: ● gunicorn.socket
```

Da biste testirali mehanizam aktivacije socketa, možete poslati vezu sa socketom tako što ćete otkucati:

```sh
curl --unix-socket /run/gunicorn.sock localhost
```

Trebalo bi da dobijete HTML izlaz iz vaše aplikacije u terminalu. Ovo ukazuje da je Gunicorn pokrenut i da je mogao da opslužuje vašu Jam.py aplikaciju. Možete proveriti da li Gunicorn servis radi tako što ćete otkucati:

```sh
sudo systemctl status gunicorn
```

```sh
● gunicorn.service - gunicorn daemon
     Loaded: loaded (/etc/systemd/system/gunicorn.service; disabled; vendor preset: enabled)
     Active: active (running) since Mon 2022-04-18 17:54:49 UTC; 5s ago
TriggeredBy: ● gunicorn.socket
   Main PID: 102674 (gunicorn)
      Tasks: 4 (limit: 4665)
     Memory: 94.2M
        CPU: 885ms
     CGroup: /system.slice/gunicorn.service
             ├─102674 /home/sammy/project/.venv/bin/python3 /home/sammy/project/.venv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock wsgi.py:application
             ├─102675 /home/sammy/project/.venv/bin/python3 /home/sammy/project/.venv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock wsgi.py:application
             ├─102676 /home/sammy/project/.venv/bin/python3 /home/sammy/project/.venv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock wsgi.py:application
             └─102677 /home/sammy/project/.venv/bin/python3 /home/sammy/project/.venv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock wsgi.py:application
```

Ako izlaz iz `curl` ili `systemctl status` ukazuje na to da je došlo do problema, proverite dnevnike za dodatne detalje:

```sh
sudo journalctl -u gunicorn
```

Proverite da li u "/etc/systemd/system/gunicorn.service" datoteci ima problema. Ako napravite izmene u "/etc/systemd/system/gunicorn.service" datoteci, ponovo učitajte demon da biste ponovo pročitali definiciju servisa i ponovo pokrenite proces Gunicorn kucanjem:

```sh
sudo systemctl daemon-reload
sudo systemctl restart gunicorn
```

Uverite se da ste rešili gore navedene probleme pre nego što nastavite.

## Nginx web server

### Konfigurisanje Nginxa

Sada kada je Gunicorn podešen, potrebno je da konfigurišete Nginx da prosledi saobraćaj Gunicorn procesu.

Počnite tako što ćete kreirati i otvoriti novi blok servera u Nginx-ovom "sites-available" direktorijumu:

```sh
sudo nano /etc/nginx/sites-available/project
```

Unutra, otvorite novi `server {}` blok. Počećete tako što ćete navesti da ovaj blok treba da sluša na normalnom portu `80` i  da treba da odgovara na ime domena ili IP adresu vašeg servera:

"/etc/nginx/sites-available/project"

```sh
server {
    listen 80;                          # Port na kome server sluša
    server_name server_domain_or_IP;    # Domensko ime servera ili IP adresa
}
```

Zatim ćete reći Nginx-u da ignoriše sve probleme sa pronalaženjem "fav" ikona. Takođe ćete mu reći gde da pronađe statičke elemente koje ste sakupili u svom direktorijumu. Sve ove datoteke imaju standardni URI prefiks "/static", tako da možete kreirati `location` blok koji odgovara tim zahtevima: `~/project/static`

"/etc/nginx/sites-available/project"

```sh
server {
    listen 80;
    server_name server_domain_or_IP;

    location = /favicon.ico { access_log off; log_not_found off; }
    location /static/ {
        root /home/sammy/project;
    }
}
```

Konačno, kreirajte `location /` blok koji će odgovarati svim ostalim zahtevima. Unutar ove lokacije, uključićete standardnu `proxy_params` datoteku koja je uključena u Nginx instalaciju, a zatim ćete proslediti saobraćaj direktno do Gunicorn socket-a:

"/etc/nginx/sites-available/project"

```sh
server {
    listen 80;
    server_name server_domain_or_IP;

    location = /favicon.ico { access_log off; log_not_found off; }
    location /static/ {
        root /home/sammy/project;
    }

    location / {
        include proxy_params;
        proxy_pass http://unix:/run/gunicorn.sock;
    }
}
```

Sačuvajte i zatvorite datoteku kada završite. Sada možete omogućiti datoteku tako što ćete je povezati sa direktorijumom "sites-enabled":

```sh
sudo ln -s /etc/nginx/sites-available/project /etc/nginx/sites-enabled
```

Testirajte svoju Nginx konfiguraciju za sintaksičke greške tako što ćete otkucati:

```sh
sudo nginx -t
```

Ako se ne prijave greške, ponovo pokrenite Nginx tako što ćete otkucati:

```sh
sudo systemctl restart nginx
```

Konačno, potrebno je da otvorite svoj zaštitni zid ( `ufw` ) za normalan saobraćaj na portu 80. Pošto vam više nije potreban pristup razvojnom serveru, možete ukloniti pravilo za otvaranje i porta 8080:

```sh
sudo ufw delete allow 8000
sudo ufw allow 'Nginx Full'
```

Sada bi trebalo da odete na domen ili IP adresu vašeg servera da biste videli svoju aplikaciju.

**Napomena** : Nakon konfigurisanja Nginx-a, sledeći korak bi trebalo da bude obezbeđivanje saobraćaja ka serveru pomoću SSL/TLS-a. Ovo je važno jer se bez njega sve informacije, uključujući lozinke, šalju preko mreže u običnom tekstu.

Ako imate ime domena, najlakši način da dobijete SSL sertifikat za zaštitu vašeg saobraćaja je korišćenje "Let's Encrypt"-a. Pratite ovaj vodič za Ubuntu 22.04 / Ubuntu 20.04 / Ubuntu 18.04 da biste podesili "Let's Encrypt" sa Nginx-om na Ubuntu 22.04. Pratite proceduru koristeći Nginx serverski blok koji ste kreirali u ovom vodiču.

### Rešavanje problema sa Nginx-om i Gunicorn-om

Ako ovaj poslednji korak ne prikazuje vašu aplikaciju, moraćete da rešite problem sa instalacijom.

#### Nginx prikazuje podrazumevanu stranicu umesto Jampy aplikacije

Ako Nginx prikazuje podrazumevanu stranicu umesto da se povezuje sa vašom aplikacijom, to obično znači da treba da podesite `server_name` unutar datoteke "/etc/nginx/sites-available/project" da bi ukazivalo na IP adresu ili ime domena vašeg servera.

Nginx koristi `server_name` da bi odredio koji blok servera da koristi za odgovor na zahteve. Ako dobijete podrazumevanu Nginx stranicu, to je znak da Nginx nije bio u stanju da eksplicitno upari zahtev sa blokom servera, pa se vraća na podrazumevani blok definisan u "/etc/nginx/sites-available/default".

Blok `server_name` servera u vašem projektu mora biti specifičniji od onog u podrazumevanom bloku servera da bi bio izabran.

#### Nginx:: greška 502 - Bad Gateway umesto Jampy aplikacije

Greška 502 ukazuje da Nginx ne može uspešno da proksira zahtev. Širok spektar problema sa konfiguracijom se izražava greškom 502, tako da je potrebno više informacija za pravilno rešavanje problema.

Glavno mesto za traženje više informacija je u Nginx-ovim evidencijama grešaka. Generalno, ovo će vam reći koji su uslovi izazvali probleme tokom proksiranja. Pratite Nginx evidencije grešaka tako što ćete otkucati:

```sh
sudo tail -F /var/log/nginx/error.log
```

Sada, napravite još jedan zahtev u pregledaču da biste generisali novu grešku (pokušajte da osvežite stranicu). Trebalo bi da dobijete novu poruku o grešci koja će biti zapisana u dnevnik. Ako pogledate poruku, trebalo bi da vam pomogne da suzite problem.

- Možda ćete dobiti sledeću poruku:
  
  ```sh
  connect() to unix:/run/gunicorn.sock failed (2: No such file or directory)
  ```
  
  Ovo ukazuje da Nginx nije mogao da pronađe gunicorn.sock datoteku na datoj lokaciji. Trebalo
bi da uporedite `proxy_pass` lokaciju definisanu u okviru /etc/nginx/sites-available/project datoteke sa stvarnom lokacijom datoteke gunicorn.sock koju je generisala gunicorn.socket   sistemska jedinica.

  Ako ne možete da pronađete gunicorn.sock datoteku u /run direktorijumu, to generalno znači da
`systemd` socket datoteka nije mogla da je kreira. Vratite se na odeljak o proveri Gunicorn socket datoteke da biste prošli kroz korake za rešavanje problema za Gunicorn.
  
- Poruka može biti i:

  ```sh
  connect() to unix:/run/gunicorn.sock failed (13: Permission denied)
  ```
  
  Ovo ukazuje da Nginx nije mogao da se poveže sa Gunicorn socketom zbog problema sa dozvolama.
Ovo se može desiti kada se postupak sprovede koristeći `root` korisnika umesto `sudo` korisnika. Dok `systemd` može da kreira Gunicorn socket datoteku, Nginx ne može da joj pristupi.
  
  Ovo se može desiti i ako postoje ograničene dozvole u ​​bilo kojoj tački između korenskog
direktorijuma ( / ) i gunicorn.sock datoteke. Možete pregledati dozvole i vrednosti vlasništva za socket datoteke i svakog od njenih roditeljskih direktorijuma tako što ćete komandi proslediti apsolutnu putanju do vaše socket datoteke namei:
  
  ```sh
  namei -l /run/gunicorn.sock
  ```
  
  ```sh
  f: /run/gunicorn.sock
  drwxr-xr-x root root /
  drwxr-xr-x root root run
  srw-rw-rw- root root gunicorn.sock
  ```
  
  Izlaz prikazuje dozvole svake od komponenti direktorijuma. Posmatranjem dozvola (prva kolona),
vlasnika (druga kolona) i vlasnika grupe (treća kolona), možete utvrditi koja vrsta pristupa je dozvoljena datoteci socketa.
  
  U gornjem primeru, socket datoteka i svaki od direktorijuma koji vode do socket datoteke imaju  
dozvole za čitanje i izvršavanje (kolona dozvola za direktorijume se završava sa `r-x` umesto `---`). Nginx proces bi trebalo da bude u mogućnosti da uspešno pristupi socketu.
  
  Ako bilo koji od direktorijuma koji vode do socketa nema dozvolu za čitanje i izvršavanje,
Nginx neće moći da pristupi socketu bez dozvoljavanja dozvola za čitanje i izvršavanje ili bez osiguravanja da je vlasništvo nad grupom dato grupi čiji je Nginx deo.
  
#### Jampy:: nije moguće povezati se sa serverom: Veza odbijena“

Jedna poruka koju možete dobiti od Jampy-a kada pokušavate da pristupite delovima aplikacije u veb pregledaču je:

```sh
OperationalError at /admin/login/
could not connect to server: Connection refused
    Is the server running on host localhost (127.0.0.1) and accepting
    TCP/IP connections on port 5432?
```

Ovo ukazuje da Jampy ne može da se poveže sa Postgres bazom podataka. Uverite se da je Postgres instanca pokrenuta tako što ćete otkucati:

```sh
sudo systemctl status postgresql
```

Ako nije, možete je pokrenuti i omogućiti da se automatski pokrene pri pokretanju sistema (ako već nije konfigurisana za to) tako što ćete otkucati:

```sh
sudo systemctl start postgresql
sudo systemctl enable postgresql
```

Ako i dalje imate problema, proverite da li su podešavanja baze podataka definisana u tabeli "parameters" ispravna.

#### Dalje rešavanje problema

Za dodatno rešavanje problema, evidencije mogu pomoći u sužavanju uzroka. Proverite svaku od njih redom i potražite poruke koje ukazuju na problematična područja.

Sledeći dnevnici mogu biti korisni:

- Proverite logove Nginx procesa tako što ćete otkucati:  
  `sudo journalctl -u nginx`
- Proverite logove pristupa Nginx-u tako što ćete otkucati:  
  `sudo less /var/log/nginx/access.log`
- Proverite evidenciju grešaka Nginx-a tako što ćete otkucati:  
  `sudo less /var/log/nginx/error.log`
- Proverite logove aplikacije Gunicorn tako što ćete otkucati:  
  `sudo journalctl -u gunicorn`
- Proverite logove Gunicorn socketa tako što ćete otkucati:  
  `sudo journalctl -u gunicorn.socket`

Kako ažurirate konfiguraciju ili aplikaciju, verovatno ćete morati ponovo pokrenuti procese da biste se prilagodili promenama.

Ako ažurirate svoju Jampy aplikaciju, možete ponovo pokrenuti Gunicorn proces da biste primenili promene tako što ćete otkucati:

```sh
sudo systemctl restart gunicorn
```

Ako promenite Gunicorn socket ili servisne datoteke, ponovo učitajte demon i restartujte proces kucanjem:

```sh
sudo systemctl daemon-reload
sudo systemctl restart gunicorn.socket gunicorn.service
```

Ako promenite konfiguraciju bloka Nginx servera, testirajte konfiguraciju, a zatim restARTUJTE Nginx tako što ćete otkucati:  
`sudo nginx -t && sudo systemctl restart nginx`

Ove komande su korisne za prihvatanje promena dok podešavate konfiguraciju.

### Često postavljana pitanja o Nginx

#### Statičko rukovanje datotekama

```sh
# static managed files
location /static/ {
    root /path/to/your/project;
    expires 1y;
    add_header Cache-Control public, immutable;
    access_log off;
}

#### Gzip kompresija
gzip on;
gzip_vary on;
gzip_min_length 1024;
gzip_comp_level 6;
gzip_types text/plain text/css application/json application/javascript;

#### Konfiguracija proksija
location / {
    proxy_pass http://unix:/run/gunicorn.sock;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_connect_timeout 30s;
    proxy_send_timeout 30s;
    proxy_read_timeout 30s;
}

#### Bezbednosni hederi
add_header X-Frame-Options SAMEORIGIN always;
add_header X-Content-Type-Options nosniff always;
add_header X-XSS-Protection 1; mode=block always;
```

#### Kako da obezbedim svoju Jampy aplikaciju pomoću SSL-a na Ubuntu-u?

Obezbeđivanje vaše Jampy aplikacije pomoću SSL-a uključuje nekoliko koraka:

- **Instalirajte Certbot** :

  ```sh
  sudo apt install certbot python3-certbot-nginx
  ```

- **Nabavite SSL sertifikat** :

  ```sh
  sudo certbot --nginx -d yourdomain.com
  ```

- **Ažurirajte konfiguraciju Nginx-a** :

```sh
server {
    listen 443 ssl http2;
    server_name yourdomain.com;
    
    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;
    
    # SSL configuration
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512;
    ssl_prefer_server_ciphers off;
}
```

- **Podesite automatsko obnavljanje** :

```sh
sudo crontab -e # Add: 0 12 * * * /usr/bin/certbot renew --quiet
```

  Ovo osigurava da je vaša Jampy aplikacija pravilno zaštićena SSL/TLS enkripcijom za svu komunikaciju.

## Napredne optimizacije performansi

### Podešavanje performansi Gunicorn servera

Podrazumevana konfiguracija Gunicorn-a funkcioniše za osnovne instalacije, ali proizvodne aplikacije koje obrađuju hiljade istovremenih korisnika zahtevaju pažljivo podešavanje. Evo zašto je svaki parametar važan i kako ga optimizovati za vaše specifično radno opterećenje:

Razumevanje Gunicorn radnih procesa: Gunicorn koristi model glavnog radnog procesa gde glavni proces upravlja radnim procesima koji obrađuju stvarne zahteve. Broj radnih procesa direktno utiče na sposobnost vaše aplikacije da obrađuje istovremene zahteve, ali više nije uvek bolje.

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target

[Service]
User=sammy
Group=www-data
WorkingDirectory=/home/sammy/project
ExecStart=/home/sammy/project/.venv/bin/gunicorn \
          --access-logfile - \
          --error-logfile - \
          --workers 4 \
          --worker-class gevent \
          --worker-connections 1000 \
          --max-requests 1000 \
          --max-requests-jitter 100 \
          --timeout 30 \
          --keep-alive 2 \
          --bind unix:/run/gunicorn.sock \
          wsgi.py:application

[Install]
WantedBy=multi-user.target
```

**Objašnjenje ključnih parametara performansi Gunicorna** :

- **--workers 4** : Formula (2 × CPU jezgra) + 1 dobro funkcioniše za aplikacije vezane za CPU, ali
Jampy aplikacije su često vezane za I/O operacije (čekaju upite u bazu podataka, eksterne API-je). Za aplikacije koje zahtevaju mnogo I/O operacija, možete bezbedno koristiti 2-4 puta veći broj CPU jezgara. Pratite korišćenje CPU-a da biste pronašli idealnu vrednost.

- **--worker-class gevent** : Ovo koristi zelene niti (kooperativni multitasking) umesto OS niti.
Gevent je savršen za Jampy aplikacije koje prave mnogo upita u bazi podataka ili API poziva jer omogućava jednom radniku da istovremeno obrađuje više zahteva dok čeka na I/O operacije. Ovo može povećati propusnost za 3-5 puta u poređenju sa sinhronim radnicima.

- **--worker-connections 1000** : Ovo ograničava koliko istovremenih veza svaki gevent vorker može  da obradi. Sa 4 vorkera i 1000 veza po njemu, teoretski možete da obradite 4000 istovremenih veza. Podesite ovo na osnovu očekivanog saobraćaja i raspoložive memorije.

- **--max-requests 1000** Curenje memorije u dugotrajnim Pajton procesima je uobičajeno. Ovaj
  parametar ponovo pokreće procese nakon 1000 zahteva kako bi se sprečilo akumuliranje memorije. Parametar podrhtavanja (100) dodaje slučajnost tako da se svi procesi ne ponovo pokreću istovremeno, što bi izazvalo kratak prekid usluge.

- **--timeout 30** :Ovo ukida radnike kojima je potrebno više od 30 sekundi da odgovore. Podesite ovo na osnovu najsporije očekivane operacije. Za većinu Jampy aplikacija, 30 sekundi je razumno, ali prilagodite na osnovu vašeg specifičnog slučaja upotrebe.

- **--keep-alive 2** : Ovo održava HTTP veze aktivnim 2 sekunde nakon slanja odgovora, smanjujući
  opterećenje veze za klijente koji podnose više zahteva.

### Optimizacija performansi Nginx-a

Nginx deluje kao obrnuti proksi i statički fajl server, obrađujući klijentske veze i direktno poslužujući statičke resurse. Ova konfiguracija optimizuje Nginx za visoko-performansne Jampy aplikacije:

Zašto su ove optimizacije važne:

- **Statičko serviranje datoteka** : Nginx servira statičke datoteke (CSS, JS, slike) mnogo brže nego Jampy

- **Rukovanje konekcijama** : Nginx može efikasno da obrađuje hiljade istovremenih konekcija

- **Keširanje** : Smanjuje opterećenje vaše Jampy aplikacije prikazivanjem keširanog sadržaja

- **Kompresija** : Smanjuje upotrebu propusnog opsega i poboljšava vreme učitavanja stranice

"/etc/nginx/sites-available/project"

```sh
server {
    listen 80;
    server_name server_domain_or_IP;

    # Security headers
    add_header X-Frame-Options SAMEORIGIN always;
    add_header X-Content-Type-Options nosniff always;
    add_header X-XSS-Protection 1; mode=block always;
    add_header Referrer-Policy no-referrer-when-downgrade always;

    # Gzip compression
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_proxied any;
    gzip_comp_level 6;
    gzip_types
        text/plain
        text/css
        text/xml
        text/javascript
        application/json
        application/javascript
        application/xml+rss
        application/atom+xml
        image/svg+xml;

    # Static files with caching
    location = /favicon.ico { 
        access_log off; 
        log_not_found off; 
        expires 1y;
        add_header Cache-Control public, immutable;
    }
    
    location /static/ {
        root /home/sammy/project;
        expires 1y;
        add_header Cache-Control public, immutable;
        access_log off;
    }

    # Media files
    location /media/ {
        root /home/sammy/project;
        expires 1M;
        add_header Cache-Control public;
    }

    # Main application
    location / {
        include proxy_params;
        proxy_pass http://unix:/run/gunicorn.sock;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_connect_timeout 30s;
        proxy_send_timeout 30s;
        proxy_read_timeout 30s;
    }
}
```

**Objašnjenje parametara Nginx konfiguracije** :

- **Bezbednosni zaglavlja** :

  - **X-Frame-Options SAMEORIGIN** : Sprečava klikdžeking napade kontrolišući da li se vaš sajt može ugraditi u okvire.
  
  - **X-Content-Type-Options nosniff** : Sprečava pregledače da odgovore MIME-sniffing-om, smanjujući vektore XSS napada
  
  - **X-XSS-Protection 1; mode=block** : Omogućava ugrađenu XSS zaštitu pregledača

  - **Referrer-Policy no-referrer-when-downgrade** : Kontroliše koliko informacija o referentu se šalje sa zahtevima

- **Gzip kompresija** :

  - **gzip on** : Omogućava kompresiju tekstualnih datoteka

  - **gzip_comp_level 6** Nivo kompresije (1-9, gde 6 pruža dobru ravnotežu između korišćenja procesora i odnosa kompresije)
  - **gzip_types** : Određuje koje tipove datoteka treba kompresovati. Tekstualne datoteke se dobro kompresuju (smanjenje od 60-80%), dok su slike i video zapisi već kompresovani.

- **Statičko keširanje datoteka**:

  - **expires 1y** : Govori pregledačima da keširaju statičke datoteke godinu dana
  - **Cache-Control public, immutable** : Označava da se datoteke neće menjati, što omogućava agresivno keširanje
  - **access_log off** : Onemogućava evidentiranje statičkih datoteka radi smanjenja I/O opterećenja

- **Konfiguracija proksija**:

  - **proxy_set_header** : Prosleđuje informacije o klijentu kompaniji Jampy radi pravilnog obrađivanja zahteva
  - **proxy_connect_timeout 30s** : Maksimalno vreme za uspostavljanje veze sa Gunicorn-om
  - **proxy_send_timeout 30s** : Maksimalno vreme za slanje zahteva kompaniji Gunicorn
  - **proxy_read_timeout 30s** : Maksimalno vreme čekanja na odgovor od Gunicorn-a

### Podešavanje performansi PostgreSQL-a

Podrazumevana konfiguracija PostgreSQL-a je konzervativna i dizajnirana je da radi na bilo kom hardveru. Za proizvodne Jampy aplikacije, ove optimizacije mogu poboljšati performanse baze podataka za 2-5 puta:

Zašto je podešavanje baze podataka važno:

- **Alokacija memorije** : Pravilna podešavanja memorije smanjuju ulazno-izlazne operacije diska tako što često pristupane podatke čuvaju u RAM-u.
- **Upravljanje vezama** : Optimizacija ograničenja veza sprečava iscrpljivanje resursa
- **Performanse upita** : Bolje evidentiranje pomaže u identifikaciji sporih upita kojima je potrebna optimizacija

"/etc/postgresql/14/main/postgresql.conf"

```sh
# Memory settings
shared_buffers = 256MB
effective_cache_size = 1GB
work_mem = 4MB
maintenance_work_mem = 64MB

# Connection settings
max_connections = 100
listen_addresses = 'localhost'

# Logging
log_statement = 'mod'
log_min_duration_statement = 1000
log_line_prefix = '%t [%p]: [%l-1] user=%u,db=%d,app=%a,client=%h '

# Checkpoint settings
checkpoint_completion_target = 0.9
wal_buffers = 16MB
```

**Objašnjenje konfiguracije PostgreSQL** :

- **Podešavanja memorije** :

  - **shared_buffers = 256MB** Ovo je glavna keš memorija PostgreSQL-a. Podesite je na 25% ukupne RAM memorije za namenske servere baza podataka ili 15% za deljene servere. Ova keš memorija čuva često pristupane stranice podataka, smanjujući ulazno-izlazne operacije diska.
  
  - **effective_cache_size = 1GB** : Govori PostgreSQL-u koliko memorije je dostupno za keširanje (uključujući keš memoriju operativnog sistema). Postavlja se na 75% ukupne RAM memorije. Ovo pomaže planeru upita da donosi bolje odluke o korišćenju indeksa.
  
  - **work_mem = 4MB** : Memorija koja se koristi za sortiranje, heš, spajanje i druge operacije. Svaka veza može da koristi ovu količinu. Za Jampy aplikacije sa složenim upitima, povećajte na 8-16MB, ali pratite ukupnu upotrebu (**work_mem × max_connections**).
  
  - **maintenance_work_mem = 64MB** : Memorija za operacije održavanja kao što su VACUUM, CREATE INDEX i ALTER TABLE. Može se podesiti na mnogo veću vrednost od **work_mem** (do **2GB**) jer je koristi samo jedna operacija istovremeno.
  
- **Podešavanja konekcija**:

  - **max_connections = 100** : Maksimalan broj istovremenih veza. Svaka veza koristi memoriju, zato uravnotežite ovo sa potrebama vaše aplikacije. Za Jampy aplikacije, obično je dovoljno 50-100 veza.

  - **listen_addresses = 'localhost'** Slušajte samo na lokalnom hostu radi bezbednosti. Promenite na '*' samo ako su vam potrebne udaljene veze (ne preporučuje se za produkciju).

- **Podešavanja evidentiranja**

  - **log_statement = 'mod'** : Zabeležava sve naredbe koje menjaju podatke (INSERT, UPDATE, DELETE). Pomaže pri otklanjanju grešaka i analizi performansi.

  - **log_min_duration_statement = 1000** : Beleži upite koji traju duže od 1 sekunde. Neophodno za identifikaciju sporih upita kojima je potrebna optimizacija.

  - **log_line_prefix** : Prilagođava format dnevnika kako bi uključio vremensku oznaku, ID procesa, korisnika, bazu podataka i IP adresu klijenta radi boljeg otklanjanja grešaka.

- **Podešavanja kontrolne tačke**

  - **checkpoint_completion_target = 0.9** : Raspodeljuje U/I kontrolne tačke na preko 90% intervala kontrolne tačke, smanjujući skokove U/I.

  - **wal_buffers = 16MB** : Baferi dnevnika za unapred pisanje. Podesite na 16MB za bolje performanse sa velikim opterećenjem pisanja.

## Skaliranje i visoka dostupnost

### Horizontalno skaliranje sa balansiranjem opterećenja

Kako vaša Jampy aplikacija raste, moraćete da se skalirate van jednog servera kako biste podneli povećani saobraćaj. Horizontalno skaliranje podrazumeva dodavanje više aplikacijskih servera i raspodelu opterećenja među njima:

- **Kreirajte više Gunicorn servisa na različitim portovima** :

  "/etc/systemd/system/gunicorn-1.service"

  ```sh
  [Unit]
  Description=gunicorn daemon instance 1
  After=network.target
  
  [Service]
  User=sammy
  Group=www-data
  WorkingDirectory=/home/sammy/project
  ExecStart=/home/sammy/project/.venv/bin/gunicorn \
            --workers 2 \
            --bind 127.0.0.1:8001 \
            wsgi.py:application
  
  [Install]
  WantedBy=multi-user.target
  ```

- **Konfiguracija Nginx Load Balancer-a**

  /etc/nginx/sites-available/project

  ```sh
  upstream Jampy_backend {
      least_conn;
      server 127.0.0.1:8000 weight=3;
      server 127.0.0.1:8001 weight=3;
      server 127.0.0.1:8002 weight=2;
      keepalive 32;
  }
  
  server {
      listen 80;
      server_name server_domain_or_IP;
  
      location / {
          proxy_pass http://Jampy_backend;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
      }
  }
  ```

### Strategije skaliranja baze podataka

- **Pročitajte replike**

  Podesite PostgreSQL replike za čitanje za opterećenja sa velikim brojem čitanja:

  ```sh
  # On replica server
  sudo -u Postgres pg_basebackup -h primary_server -D /var/lib/postgresql/14/main -U replicator -v -P -W
  ```

- **Grupisanje veza**

  Implementirajte PgBouncer za objedinjavanje konekcija:

  ```sh
  sudo apt install pgbouncer
  ```

  Konfiguriši "/etc/pgbouncer/pgbouncer.ini":
  
  ```sh
  [databases]
  project = host=localhost port=5432 dbname=project
  
  [pgbouncer]
  listen_port = 6432
  listen_addr = 127.0.0.1
  auth_type = md5
  auth_file = /etc/pgbouncer/userlist.txt
  pool_mode = transaction
  max_client_conn = 100
  default_pool_size = 20
  ```
  
### Strategije keširanja

Keširanje je ključno za visoko-performansne Jampy aplikacije. Smanjuje opterećenje baze podataka, poboljšava vreme odziva i omogućava vašoj aplikaciji da obradi više istovremenih korisnika:

#### Redis keširanje

Redis je skladište podataka u memoriji koje je savršeno za keširanje u Jampy jer je brzo, podržava složene strukture podataka i može da podnese visoku konkurentnost:

```sh
sudo apt install redis-server
```

Ažuriranje Jampy podešavanja:

> [!Note]  
> Potrebno je proučiti i napisati ovaj deo.

#### Nginx keširanje

  Implementirajte Nginx keširanje za statički i dinamički sadržaj:

  ```sh
  # Cache zone definition
  proxy_cache_path /var/cache/nginx levels=1:2 keys_zone=my_cache:10m max_size=10g inactive=60m use_temp_path=off;
  
  server {
      #... existing configuration
      
      location / {
          proxy_cache my_cache;
          proxy_cache_valid 200 302 10m;
          proxy_cache_valid 404 1m;
          proxy_cache_use_stale error timeout updating http_500 http_502 http_503 http_504;
          proxy_cache_lock on;
          proxy_pass http://Jampy_backend;
          #... other proxy settings
      }
  }
  ```
  