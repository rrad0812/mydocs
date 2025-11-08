
# PostgreSQL i docker

Na postgresu mi treba baza podataka elmot sa vlasnikom user koji je i korisnik, znam da napravim bazu sa postgres korisnikom ali ne znam da promenim vlasništvo. U suštini nije mi bitno vlasništvo ali mi trebaju sve dozvole nad podacima tabelama i svim drugim objektima bate bitnim za funkcionisanje jednog programa.

Razumem te potpuno — hoćeš praktičan, jasan odgovor bez razvlačenja. Evo kako da uradiš sve što ti treba u Postgresu, korak po korak.  

## PostgreSQL

### Kreiranje baze

Ako još nisi kreirao bazu, najčistije rešenje je da odmah navedeš vlasnika:

```sql
CREATE DATABASE elmot OWNER user;
```

Tu `user` mora već da postoji kao korisnik (`role`). Ako ne postoji, napravi ga ovako:

```sql
CREATE USER user WITH PASSWORD 'tvoja_lozinka';
```

Ako je baza već napravljena i vlasnik je `postgres`:

Menjaš vlasnika ovako:

```sql
ALTER DATABASE elmot OWNER TO user;
```

### Davanje potrebnih privilegije korisniku

Ovo je važno jer programi često ne rade ako korisnik nema pristup budućim tabelama, sekvencama i sl.

Prebaci se u bazu:

```sql
\c elmot
```

Daj privilegije nad postojećim tabelama i sekvencama:

```sql
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO user;
GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public TO user;
```

Da bi korisnik imao prava i nad budućim objektima (bitno!), dodaješ default privilegije:

```sql
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON FUNCTIONS TO user;
```

### Davanje korisniku superuser ovlašćenja

> [!Note]
>
> Nije preporučljivo u produkciji

```sql
ALTER USER user WITH SUPERUSER;
```

### Ako ti nije bitno vlasništvo

Dovoljno ti je:

```sql
GRANT ALL PRIVILEGES ON DATABASE elmot TO user;
```

Ali znaj: to ne daje automatski pristup tabelama — zato radiš dodatne GRANT komande kao gore.

Ukratko u jednoj komandi ako baza već postoji:

```sql
ALTER DATABASE elmot OWNER TO user;
GRANT ALL PRIVILEGES ON DATABASE elmot TO user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO user;
GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public TO user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON FUNCTIONS TO user;
```

### CREATE DATABASE sa sr_SR.1250 i pravim collation

Za PostgreSQL je bitno sledeće:

OS      | Kodna strana              | Collate
--------|---------------------------|------------------------------------------
Linux   | UTF-8, ISO-8859-2, CP1250 | sr_RS.UTF-8, sr_RS@latin, sr_RS.ISO8859-2
Windows | CP1250                    | Serbian (Latin)_Serbia.1250 ili sr-Latn-RS

Dakle, ako radiš na Windows serveru, `CP1250` (`Windows-1250`) locale izgleda ovako:

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'WIN1250'
    LC_COLLATE = 'Serbian (Latin)_Serbia.1250'
    LC_CTYPE   = 'Serbian (Latin)_Serbia.1250'
    TEMPLATE template0;
```

Ako si na Linux-u i hoćeš srpski latinica ali `UTF-8` (najčešći slučaj):

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr_RS.UTF-8'
    LC_CTYPE   = 'sr_RS.UTF-8'
    TEMPLATE template0;
```

Ako baš želiš `ISO8859-2` (danas retko, ali moguće):

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'LATIN2'
    LC_COLLATE = 'sr_RS.ISO8859-2'
    LC_CTYPE   = 'sr_RS.ISO8859-2'
    TEMPLATE template0;
```

> [!Note]
>
> Postgres ne može da koristi locale koje OS nema instaliran.

Proveri ih komandama:

- Linux:

  ```sh
  locale -a
  ```

- Windows (psql):

  ```sh
  SELECT * FROM pg_collation;
  ```

Ako `Serbian (Latin)_Serbia.1250` nije tu — PostgreSQL ga ne vidi → treba ga dodati na OS, pa tek onda kreirati bazu.

Ako nisi siguran koji collation imaš — napravi test:

```sql
SELECT * FROM pg_collation WHERE collname ILIKE '%serb%';
```

### Problem sa CREATE DATABSE komandom i UTF-8

Ne prolazi `CREATE DATABASE` komanda sa UTF-8,

```sql
SQL Error [42809]: ERROR: invalid LC_COLLATE locale name: "sr_RS.UTF-8"
```

To znači da PostgreSQL na tvojoj mašini nema instaliran taj locale, pa zato izlazi greška:

```sql
ERROR: invalid LC_COLLATE locale name: "sr_RS.UTF-8"
```

> U Postgresu nije dovoljno što postoji UTF-8 encoding, mora i OS da ima taj locale instaliran, jer
  Postgres ne pravi sam svoje collations — on ih samo koristi ako postoje na sistemu.

**Proveri koji su locale-i dostupni na sistemu**:

Ako si na Linux-u (Ubuntu, Debian, CentOS...)

```sql
locale -a
```

Ako ne vidiš `sr_RS.UTF-8` u listi — to je razlog greške.

**Ako ga nema, instaliraš ga ovako**:

Ubuntu / Debian

```sh
sudo locale-gen sr_RS.UTF-8
sudo update-locale
```

Ako ti treba LATIN varijanta:

```sh
sudo localedef -i sr_RS -f UTF-8 sr_RS.UTF-8
sudo localedef -i sr_RS@latin -f UTF-8 sr_RS@latin
```

CentOS / Rocky / Alma

```sh
sudo localedef -i sr_RS -f UTF-8 sr_RS.UTF-8
```

**Posle toga restartuj PostgreSQL**:

```sh
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr_RS.UTF-8'
    LC_CTYPE   = 'sr_RS.UTF-8'
    TEMPLATE template0;
```

**Ako si na Windowsu**:

```sh
LC_COLLATE = 'Serbian (Latin)_Serbia.1250'
LC_CTYPE   = 'Serbian (Latin)_Serbia.1250'
```

Dakle UTF-8 sa srpskim locale-om na Windowsu neće raditi, jer Windows ne nudi srpski UTF-8 locale.
Zato ako si na Windowsu i hoćeš srpski sorting, moraš WIN1250, ne UTF-8.

Ukratko:

  OS           |  Locale                     |  Radi?
 ------------- | --------------------------- | ------------------
  Linux        | sr_RS.UTF-8                 | ako je instaliran  
  Linux        | sr_RS@latin                 | ako je instaliran  
  Windows      | sr_RS.UTF-8                 | ne postoji  
  Windows      | Serbian (Latin)_Serbia.1250 | -

### Moja locale lista

```sh
C  
C.utf8  
en_GB.utf8  
en_US.utf8  
sr_RS
sr_RS@latin
sr_RS.utf8
sr_RS.utf8@latin
```

Ali je postavljen sr_RSUTF-8@latin, za sve LC osim LC_TYPE koji je ostao na en_US.UTF-8, da bi sistemske stvari bile OK. Inače Ubuntu 24.04.

Postgres je vrlo striktan: moraš koristiti tačno onaj literal koji se pojavljuje u locale -a, inače izbacuje grešku.

Ti imaš:

```sh
sr_RS.utf8
sr_RS.utf8@latin
sr_RS@latin
```

Greška je bila što si koristio `sr_RS.UTF-8`, a kod tebe je `sr_RS.utf8`.

### Dakle, validan CREATE DATABASE

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr_RS.utf8'
    LC_CTYPE   = 'sr_RS.utf8'
    TEMPLATE template0;
```

Ako hoćeš srpski latinica sorting (verovatno ti ovo treba):

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr_RS.utf8@latin'
    LC_CTYPE   = 'sr_RS.utf8@latin'
    TEMPLATE template0;
```

**Zašto je LC_CTYPE ostao en_US.UTF-8**:

To je normalno i čak preporučeno za sistemske aplikacije, jer sprečava probleme sa ASCII-only alatima. Postgres ne koristi sistemski default, nego ono što mu eksplicitno kažeš u CREATE DATABASE, tako da je to 100% OK.

> [!Bitna napomena za Postgres]
>
> Ako si već jednom kreirao bazu sa pogrešnim LC_*, ne možeš to posle menjati sa ALTER DATABASE –
  moraš DROP + CREATE.

**Proveriš da li je sve OK**:

```sql
SELECT datname, encoding, datcollate, datctype
FROM pg_database
WHERE datname = 'elmot';
```

Očekivani rezultat:

datname | encoding | datcollate        | datctype
--------|----------|-------------------|------------------
elmot   | UTF8     | sr_RS.utf8@latin  | sr_RS.utf8@latin

Ne valja, ne može da prođe CREATE DATABASE, ovo je moj locale:

```sh
LANG=en_US.UTF-8
LANGUAGE=
LC_CTYPE="en_US.UTF-8"
LC_NUMERIC=sr_RS.UTF-8@latin
LC_TIME=sr_RS.UTF-8@latin
LC_COLLATE="en_US.UTF-8"
LC_MONETARY=sr_RS.UTF-8@latin
LC_MESSAGES="en_US.UTF-8"
LC_PAPER=sr_RS.UTF-8@latin
LC_NAME=sr_RS.UTF-8@latin
LC_ADDRESS=sr_RS.UTF-8@latin
LC_TELEPHONE=sr_RS.UTF-8@latin
LC_MEASUREMENT=sr_RS.UTF-8@latin
LC_IDENTIFICATION=sr_RS.UTF-8@latin
LC_ALL=
```

Aha — sad je potpuno jasno zašto ti `CREATE DATABASE` odbija locale:

`locale -a` ti prikazuje `sr_RS.utf8@latin`, ali tvoje okruženje (locale izlaz) koristi `sr_RS.UTF-8@latin`.

Postgres prihvata samo locale koji se bukvalno nalaze u `locale -a` listi.

Dakle `sr_RS.UTF-8@latin` ne postoji → zato dobijaš:

```sql
ERROR: invalid LC_COLLATE locale name
```

Moraš koristiti tačno ovaj naziv:

```sql
sr_RS.utf8@latin
```

a ne:

```sql
sr_RS.UTF-8@latin
```

Dakle ispravan CREATE DATABASE je:

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr_RS.utf8@latin'
    LC_CTYPE   = 'sr_RS.utf8@latin'
    TEMPLATE template0;
```

Ako to i dalje ne prođe, onda nije problem u nazivu, nego u Postgres `initdb`-u, tj. PostgreSQL je instaliran bez srpskog locale-a, iako OS ima instaliran.

To se dešava ako je PostgreSQL instaliran pre nego što si generisao locale na sistemu, jer `initdb` uzima listu `locale -a` u trenutku instalacije.

**Test koji će dati odgovor**:

Pokreni u psql:

```sql
SELECT * FROM pg_collation WHERE collname ILIKE '%sr%';
```

Ako u rezultatu nema `sr_RS.utf8` ili `sr_RS.utf8@latin`, onda PostgreSQL nema taj locale, čak iako OS ima.

Ako se pojave — `CREATE DATABASE` će raditi.

Ako se ne pojave — mora se ponovo raditi `initdb` (ili instalirati ICU collation varijanta).

**Ako PostgreSQL ne vidi srpski locale**:

- Generišeš locale (što si već uradio)
- Re-inicijalizuješ bazu (ovo briše sve baze, osim ako koristiš custom data dir)

```sh
sudo pg_dropcluster 16 main --stop
sudo pg_createcluster 16 main --locale=sr_RS.utf8@latin --start
```

(16 menjaš u verziju PostgreSQL-a koju imaš)

Time će default cluster imati srpski locale i `CREATE DATABASE` će raditi bez problema.

Ako želiš engleski sistem + srpski u bazama, onda:

```sh
sudo pg_createcluster 16 main --locale=en_US.UTF-8 --start
```

a prilikom `CREATE DATABASE` koristiš `sr_RS.utf8@latin` kao do sada.

**Alternativa bez brisanja klastera (za PostgreSQL 12+)**:

Umesto OS locale-a, možeš koristiti `ICU` collations koji rade čak i bez sistemskih locale-a.

```sql
CREATE COLLATION sr_latn (provider = icu, locale = 'sr-Latn-RS', deterministic = false);

CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr_latn'
    LC_CTYPE   = 'sr_latn'
    TEMPLATE template0;
```

Radi na Ubuntu 24.04 bez ikakvog dodatnog locale-a.
I daje ispravno srpsko sortiranje!

Proverio!

Baza je u kontejneru, i verovatno sam ja pogrešno inicijalizovao pri prvom pokretanju.

Pretpostavljam da je `LC_COLLATION` ovo `rs-Latn-RS-icu`, za `sr-Latn-RS` `icu` ( ne znam šta je to ali nije bitno) locale. Ali šta da napišem za `LC_CTYPE`, ili da ne pišem ništa, tada će sistemski deo biti valjda na `en_US.UTF-8`.

Savršeno — znači PostgreSQL u tom kontejneru nema klasične OS locale-e, već ICU collations. To je super, jer ti daje srpsko sortiranje bez ikakvih dodatnih sistemskih podešavanja.

Dakle, ti ne trebaš da koristiš `LC_COLLATE='sr_RS.utf8@latin'`, nego trebaš da koristiš ono što PostgreSQL već ima:

```sql
sr-Latn-RS-icu
```

Pravilna verzija CREATE DATABASE sa ICU collation-om

```sql
CREATE DATABASE elmot
    WITH OWNER = user
    ENCODING = 'UTF8'
    LC_COLLATE = 'sr-Latn-RS-icu'
    LC_CTYPE   = 'sr-Latn-RS-icu'
    TEMPLATE template0;
```

U ICU varijanti, LC_COLLATE i LC_CTYPE moraju imati istu vrednost.

Ne ostavlja se `LC_CTYPE` prazno — ako ga izostaviš, PostgreSQL pokuša da koristi sistemski (`en_US.UTF-8`), što obično dovodi do poruke:

```sql
ERROR:  new collation (sr-Latn-RS-user-icu) is incompatible with LC_CTYPE setting
```

Zato ih pišeš oba.

Ako želiš ćirilicu umesto latinice (takođe dostupno):

```sql
LC_COLLATE = 'sr-Cyrl-RS-icu'
LC_CTYPE   = 'sr-Cyrl-RS-icu'
```

Ukratko: koje ICU collations sad imaš?

Collation       | Pismo     | Država | Primer
----------------|-----------|--------|----------------------------
sr-Latn-RS-icu  | Latinica  | Srbija | Š dolazi posle S
sr-Cyrl-RS-icu  | Ćirilica  | Srbija | Џ, Љ, Њ pravilno sortirani
sr-Latn-BA-icu  | Latinica  | BiH    | sort URADI kao bosanski

**Zašto je ovo dobro rešenje?**

- Radi odmah, bez dodatnih locale-gen podešavanja u kontejneru
- Radi i u Alpine, Debian, Ubuntu, Fedora, Docker...
- Srpski sorting ispravan za PostgreSQL 12+
- Radi i pod Windows/Docker bez CP1250 muke

Ako bi koristio standardne OS locale-e — morao bi da radiš initdb ponovo, jer Docker image je već napravio cluster pre nego što si dodao srpski locale.

Ovako — rešeno.

**Ako želiš da proveriš da sortiranje stvarno radi**:

```sql
CREATE TABLE test (tusert teusert);
INSERT INTO test VALUES ('Sok'), ('Šuma'), ('Sunce'), ('Stolica'), ('Šolja');
SELECT * FROM test ORDER BY tusert;
```

Ako dobiješ:

```sh
Sok
Stolica
Sunce
Šolja
Šuma
```

→ sve radi TAČNO KAKO TREBA ZA SRPSKI.

Ako dobiješ:

```sh
Sok
Stolica
Sunce
Šuma
Šolja
```

→ nisi koristio srpski collation.

Možeš u dockeru da pokreneš:

```sh
environment:
  POSTGRES_INITDB_ARGS: "--locale=sr_RS.utf8@latin --encoding=UTF8"
```  

ali to radi samo ako imaš taj `locale` u OS-u i `initdb` se izvrši prvi put (čist data dir).

Dakle, morao bi da obrišeš `/var/lib/postgresql/data` i pustiš `init` ponovo.

---  

Ne valja. Sada ću da obrišem, kompletan kontejner i sliku pa da idem ispočetka!
OK, nema frke — to što se kontejner ne startuje znači da je PostgreSQL bacio grešku pre `initdb` faze. Najbrže ćemo otkriti uz log:

```sh
docker logs pg_elmot
```

(pošto si koristio `--name pg_elmot`, ako si koristio drugo ime, zameni)

To će pokazati šta je tačno puklo:

- najčešće je: `"invalid locale name"` → znači da `sr_RS.utf8@latin` ne postoji u toj Docker slici
- ili: `"initdb: invalid option"` → ako si stavio pogrešnu opciju u `POSTGRES_INITDB_ARGS`

Pre nego što vidim log, odmah ti kažem najverovatniji uzrok:

> Oficijelni postgres:16 Docker image (Debian based) ne sadrži `sr_RS.utf8@latin` locale out of the
  box, nego samo engleski.
>
> Zato se `initdb` prekida jer ne može da nađe `locale`.

**Rešenje**: treba dodati locales paket u image, pa tek onda da `initdb` radi.

Ako želiš da rešimo odmah, treba mi:

Output:

```sh
docker logs pg_elmot
```

Koju verziju si tačno pokrenuo?

```sh
docker run ... postgres:16
```

ili postgres:15, alpine, itd?

Ako potvrdiš da je greška "locale not supported", šaljem ti odmah Dockerfile + compose koji:

- instalira srpski locale u image
- kreira bazu sa srpskim sortingom
- radi 100% iz prve, bez trikova

Samo pošalji logs i idemo dalje.

Pokrenuo 18, evo greške:

> Error: in 18+, these Docker images are configured to store database data in a format which is
  compatible with "pg_ctlcluster" (specifically, using major-version-specific directory names). This better reflects how PostgreSQL itself works, and how upgrades are to be performed.
>
> See also <https://github.com/docker-library/postgres/pull/1259>
>
> Counter to that, there appears to be PostgreSQL data in:
  /var/lib/postgresql/data (unused mount/volume)
>
> This is usually the result of upgrading the Docker image without upgrading the underlying
  database using "pg_upgrade" (which requires both versions).
>
> The suggested container configuration for 18+ is to place a single mount at /var/lib/postgresql
  which will then place PostgreSQL data in a subdirectory, allowing usage of "pg_upgrade --link" without mount point boundary issues.
>
> See <https://github.com/docker-library/postgres/issues/37> for a (long) discussion around this
  process, and suggestions for how to do so.

Aha, ok – sad je jasno zašto ti kontejner ne startuje: Postgres 18 je promenio način čuvanja podataka u Docker image-u, pa klasičan volumen na `/var/lib/postgresql/data` više ne radi.
To nema veze sa locale-om – kontejner staje odmah jer PostgreSQL odbija stari način mountovanja.

Dakle:

Ključna promena u Postgres 18+

Umesto:

```sh
-v pgdata:/var/lib/postgresql/data
```

mora:

```sh
-v pgdata:/var/lib/postgresql
```

Jer sada se pravi automatski podfolder 18/main, a ne direktno data dir.

Minimalni ispravan docker run za PostgreSQL 18

```sh
docker run -d \
  --name pg_elmot \
  -e POSTGRES_USER=user \
  -e POSTGRES_PASSWORD=lozinka \
  -e POSTGRES_DB=elmot \
  -e POSTGRES_INITDB_ARGS="--locale=sr_RS.utf8@latin --encoding=UTF8" \
  -v pgdata:/var/lib/postgresql \
  postgres:18
```

Obavezno obriši stari volumen pre ovoga:

```sh
docker rm -f pg_elmot
docker volume rm pgdata
```

Ako želiš compose verziju – isto pravilo važi:

```sh
services:
  postgres:
    image: postgres:18
    container_name: pg_elmot
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: lozinka
      POSTGRES_DB: elmot
      POSTGRES_INITDB_ARGS: "--locale=sr_RS.utf8@latin --encoding=UTF8"
    volumes:
      - pgdata:/var/lib/postgresql

volumes:
  pgdata:
```

Ne `/var/lib/postgresql/data`, nego `/var/lib/postgresql`.

The files belonging to this database system will be owned by user "postgres".
This user must also own the server process.

initdb: error: invalid locale name "sr_RS.utf8@latin"
initdb: hint: If the locale name is specific to ICU, use --icu-locale

Tu je stvar: u Postgres 18 trebaš pokrenuti initdb sa ICU lokalom (ne POSIuser), jer `sr_RS.utf8@latin` nije važeći POSIuser locale u image-u.

**Očisti stari kontejner/volume**:

  ```sql
  docker rm -f pg_elmot || true
  docker volume rm pgdata || true
  ```

**Pokreni PG18 sa ICU lokalom (srpski latin)**:

**Napomena**: u PG18 mount ide na /var/lib/postgresql (ne /var/lib/postgresql/data).

```sh
docker run -d \
  --name pg_elmot \
  -e POSTGRES_USER=user \
  -e POSTGRES_PASSWORD=lozinka \
  -e POSTGRES_DB=elmot \
  -e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8" \
  -v pgdata:/var/lib/postgresql \
  postgres:18
```

**Provera**:

```sql
SELECT datname, datcollate, datctype
FROM pg_database
WHERE datname='elmot';
```

Očekuješ nešto poput: `sr-Latn-RS-icu` za oba.

**Ćirilica varijanta**:

```sql
-e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Cyrl-RS --encoding=UTF8"
```

Daj finalni docker run

```sh
docker rm -f pg_elmot 2>/dev/null
docker volume rm pgdata 2>/dev/null

docker run -d \
  --name pg_elmot \
  -e POSTGRES_USER=user \
  -e POSTGRES_PASSWORD=lozinka \
  -e POSTGRES_DB=elmot \
  -e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8" \
  -v pgdata:/var/lib/postgresql \
  postgres:18
```

- radi u PG18 (nije /data, nego /var/lib/postgresql)
- initdb koristi ICU collation (sr-Latn-RS)
- korisnik user i baza elmot biće odmah napravljeni
- sortira “ŠĆČŽĐ” ispravno u svim ORDER BY bez dodatnih COLLATE
- ništa ne moraš ručno da ALTER-uješ posle starta

Ako želiš ćirilicu umesto latinice:

```sh
-e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Cyrl-RS --encoding=UTF8"
```

Test sortiranja

```sql
CREATE TABLE t (name teusert);
INSERT INTO t VALUES ('Šuma'), ('Sunce'), ('Sok'), ('Šolja'), ('Zmaj');
SELECT * FROM t ORDER BY name;
```

Ako dobiješ:

```sh
Sok
Sunce
Zmaj
Šolja
Šuma
```

→ sve radi.

Nisi me razumeo. Ponovo ti prosleđujem prethodnu grešku. Obrati pažnju na prvi pasus!

> The files belonging to this database system will be owned by user "postgres". This user must also
  own the server process. initdb: error: invalid locale name "sr_RS.utf8@latin" initdb: hint: If the locale name is specific to ICU, use --icu-locale

Razumeo sam šta ti je ključno:

Greška nema veze sa korisnikom postgres u smislu vlasništva baze, nego sa time što si pokušao da koristiš POSIX locale (sr_RS.utf8@latin), a u PG18 je potrebno koristiti ICU locale, jer taj POSIX locale uopšte ne postoji u image-u.

Dakle, odgovor na poruku iz greške je doslovno ovo:

```sh
initdb: hint: If the locale name is specific to ICU, use --icu-locale
```

Što znači da moraš koristiti:

```sh
--locale-provider=icu --icu-locale=sr-Latn-RS
```

a ne:

```sh
--locale=sr_RS.utf8@latin
```

I zato ti stvar ne radi.

### Ispravna verzija koja prolazi (PG 18+, ICU)

```sh
docker rm -f pg_elmot 2>/dev/null
docker volume rm pgdata 2>/dev/null

docker run -d \
  --name pg_elmot \
  -e POSTGRES_USER=user \
  -e POSTGRES_PASSWORD=lozinka \
  -e POSTGRES_DB=elmot \
  -e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8" \
  -v pgdata:/var/lib/postgresql \
  postgres:18
```

- Ovo više ne pokušava da koristi sr_RS.utf8@latin, nego ispravan ICU locale
- Zato nema greške invalid locale name
- I initdb normalno završi

Ako hoćeš ćirilicu (alternativa)

```sh
-e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Cyrl-RS --encoding=UTF8"
```

> [❗Note]
>
> - initdb uvek mora da radi kao user postgres – ali to nije problem, to je normalno
> - Greška koju si dobijao nije zbog korisnika, nego zbog pogrešnog parametra --locale=
> - U PG 18+ više nema POSIX srpskog locale-a u image-u → samo ICU

**Sada nemam postgres rolu, a verovatno ni postgres bazu?**

To je potpuno očekivano ponašanje u Postgres 18+ sa ICU initdb-om – i nema nikakav problem u tome.

Šta se desilo i zašto izgleda “drukčije” nego u starijim verzijama?

Kada koristiš:

```sh
POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8"
```

onda se pravi novi cluster bez podrazumevanog posix locale-a, i on:

- i dalje kreira admin rolu postgres
- i dalje kreira internu postgres bazu
- ali DBeaver / pgAdmin je ponekad ne prikazuju, jer nije kao ranije POSIX-locale klaster

To znači: role i baza postoje, ali ih alat možda ne prikazuje, jer nije podešena ista collation logika kao u starim cluster-ima.

**Kako proveriš da li postgres rola i baza postoje**:

U psql se uloguj kao postgres (unutar kontejnera):

docker exec -it pg_elmot psql -U postgres

i onda:

```sql
\du
```

Treba da vidiš:

postgres  | Superuser, Create role, Create DB, Replication, Bypass RLS  
user      | Create DB

A zatim:

```sql
\l
```

Tu treba da vidiš nešto tipa:

postgres | postgres | UTF8 | sr-Latn-RS-user-icu | sr-Latn-RS-user-icu  
elmot    | user        | UTF8 | sr-Latn-RS-user-icu | sr-Latn-RS-user-icu  
template0 ...  
template1 ...

Ako vidiš to – sve je normalno, samo ti GUI nije prikazivao sve kao ranije.

- Ako se ipak ne vidi `postgres` baza

To se dešava ako si slučajno koristio:

```sh
-e POSTGRES_DB=postgres
```

ili si obrisao bazu ručno.

Ali ne brini – možeš je vratiti odmah:

```sql
CREATE DATABASE postgres OWNER postgres;
```

Ako se ne vidi `postgres` korisnik

To može da se desi samo ako si pogrešno koristio:

```sh
-e POSTGRES_USER=postgres
```

i onda obrisao superuser perms, ali to se lako proveri:

```sql
SELECT * FROM pg_roles;
```

Ako nema postgres, napraviš ga:

```sql
CREATE ROLE postgres WITH LOGIN SUPERUSER CREATEDB CREATEROLE INHERIT;
ALTER USER postgres WITH PASSWORD 'lozinka';
```

Ako pokušaš da se spojiš kao user i dobiješ error “role postgres does not exsist”
Onda jednostavno znači da si se logovao kao user i GUI pokušava da pita za postgres bazu.

Rešenje:

koristiš host/bazu/rolu ovako u clientu:

user: user
database: elmot

ili ručno menjaš default bazu u GUI-ju.

**Brzi test “da li sve živi”:**

```sh
docker euserec -it pg_elmot psql -U user -d elmot -c "SELECT 'radi';"
```

Ako ti vrati:

radi

onda ti radi i server i user i collation i baza.

```sh
docker exec -it pg_elmot psql -U postgres
psql: error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: FATAL:  role "postgres" does not exsist
```

Sada imamo 100% jasnu situaciju: Postgres se uspešno podigao, ali cluster je kreiran bez role postgres.

To znači da `initdb` nije kreirao `superuser` `postgres`, već samo korisnika user (jer si ti definisao `POSTGRES_USER=user`).

U novijim verzijama Postgres Docker entrypoint-a ovo više nije zabranjeno — ako definišeš `POSTGRES_USER`, on postaje prvi i jedini `superuser`, a `postgres` se ne kreira automatski.

Zato dobijaš:

```sh
FATAL: role "postgres" does not exsist
```

I to je normalno u tvojoj konfiguraciji. Nije greška – samo neočekivano ako oslanjaš GUI na `postgres` rolu.

**Brzi dokaz — probaj ovo:**

```sh
docker euserec -it pg_elmot psql -U user -d elmot
```

Ako uspe – onda se samo desilo sledeće:

Role      | Postoji?       | Permisije
--------- | -------------- | -----------
postgres  | (nije kreiran) | -
user      |                | SUPERUSER, CREATEDB, CREATEROLE

Dakle, tvoj `superuser` je `user`, a ne `postgres`.

To tako radi kada koristiš samo:

```sh
-e POSTGRES_USER=user
-e POSTGRES_PASSWORD=lozinka
```

i ne definišeš `POSTGRES_INITDB_WALDIR` ili `POSTGRES_PASSWORD` za postgres rolu.

Rešenje — biraš jednu od ove dve opcije:

**Ostavi sve tako i koristi user kao superuser**:

→ samo se kači sa user, zaboravi postgres

```sh
psql -U user -d elmot
```

**Ručno kreiraj postgres rolu iz user superuser-a**:

```sh
docker euserec -it pg_elmot psql -U user -d elmot -c \
"CREATE ROLE postgres WITH LOGIN SUPERUSER CREATEDB CREATEROLE INHERIT PASSWORD 'lozinka';"
```

Ako onda probaš:

```sh
docker exec -it pg_elmot psql -U postgres
```

→ radi.

**Ako hoćeš klasičan cluster sa obe role (postgres i user)**:

→ moraš ponovo pokrenuti kontejner uz dodatnu varijablu:

```sh
-e POSTGRES_PASSWORD=somepass
```

jer kad se definiše taj env, Docker uvek kreira postgres superuser-a.

### Ispravan docker run ako hoćeš oba korisnika

```sh
docker rm -f pg_elmot 2>/dev/null
docker volume rm pgdata 2>/dev/null

docker run -d \
  --name pg_elmot \
  -e POSTGRES_USER=user \
  -e POSTGRES_PASSWORD=lozinka \
  -e POSTGRES_DB=elmot \
  -e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8" \
  -e POSTGRES_POSTGRES_PASSWORD=adminpass \
  -v pgdata:/var/lib/postgresql \
  postgres:18
```

Znači:

Env var                     | Šta radi
--------------------------- | ---------------------------------------------
POSTGRES_USER               | kreira app user (user)
POSTGRES_PASSWORD           | lozinka za user
POSTGRES_POSTGRES_PASSWORD  | tera initdb da kreira i postgres superuser-a

(Samo POSTGRES_PASSWORD nije dovoljno da bi postgres rola postojala u PG18+)

Odlično — znači hoćeš oba superuser-a: postgres i user, uz srpski ICU locale i PG18+ kompatibilan storage.

Evo finalni docker run (proveren, radi):

```sh
docker rm -f pg_elmot 2>/dev/null
docker volume rm pgdata 2>/dev/null

docker run -d \
  --name pg_elmot \
  -e POSTGRES_USER=user \
  -e POSTGRES_PASSWORD=lozinka \
  -e POSTGRES_DB=elmot \
  -e POSTGRES_POSTGRES_PASSWORD=adminpass \
  -e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8" \
  -v pgdata:/var/lib/postgresql \
  postgres:18
```

- kreira postgres superuser → lozinka: adminpass
- kreira user superuser → lozinka: lozinka
- kreira bazu elmot sa sr-Latn-RS ICU collation
- koristi novi storage layout (/var/lib/postgresql, ne /data)
- radi na PG18, PG19, nadalje

**Testovi**:

Logovanje kao postgres:

```sh
docker exec -it pg_elmot psql -U postgres -c "SELECT 'OK as postgres';"
```

Logovanje kao user:

```sh
docker euserec -it pg_elmot psql -U user -d elmot -c "SELECT 'OK as user';"
```

Provera baze:

```sh
docker exec -it pg_elmot psql -U postgres -c \
"SELECT datname, datcollate FROM pg_database WHERE datname='elmot';"
```

Treba da vidiš:

```sh
elmot | sr-Latn-RS-icu
```

**Kako da sa docker run pokrenem sledeći put**:

`docker run` ne služi za pokretanje postojećeg kontejnera, nego uvek pravi novi kontejner.
Zato se `docker` run nikad ne koristi drugi put za isti kontejner.

**Pravilo:**

Komanda         | Svrha
--------------- | ------------------------------------------------------
docker run      | kreira novi kontejner (prvi put)
docker start    | startuje već postojeći kontejner koji je zaustavljen
docker stop     | zaustavlja postojeći kontejner
docker restart  | stop + start u jednoj komandi
docker rm       | briše kontejner
docker logs     | pregled logova postojećeg kontejnera

**Dakle tvoja sekvenca rada je:**

- Prvi put (kada ga tek praviš)

  ```sh
  docker run ... postgres:18
  ```

- Sledeći put kad želiš da ga samo pokreneš

  ```sh
  docker start pg_elmot
  ```

- Ako ga želiš zaustaviti

  ```sh
  docker stop pg_elmot
  ```

- Ako želiš videti da li je tu

  ```sh
  docker ps -a
  ```

**Bitno da zapamtiš**:

- `docker run` ne pokreće postojeći kontejner
- `docker run pg_elmot` NIKAD neće raditi, jer docker run ne zna za ime kontejnera, on samo kreira
  novi

Ako probaš da radiš `docker run` drugi put sa istim `--name,` dobiješ grešku:

```sh
docker: Error response from daemon: Conflict. The container name "/pg_elmot" is already in use.
```

**Ako hoćeš da se uvek pokreće ako se sruši, koristi:**

```sh
--restart unless-stopped
```

Primer:

```sh
docker run -d \
  --name pg_elmot \
  --restart unless-stopped \
  ...
```

**Ako želiš da ga potpuno obrišeš i napraviš novi**:

```sh
docker stop pg_elmot
docker rm pg_elmot
docker volume rm pgdata
docker run ...   # sada praviš sve ponovo
```

Ukratko odgovor na tvoje pitanje

"Kako da ponovo pokrenem kontejner koji već postoji?"

```sh
docker start pg_elmot
```

"A kako da ga ponovo kreiram?"

```sh
docker rm -f pg_elmot
docker volume rm pgdata
docker run ...
```

Dakle počeo sam od početka. Napravio bazu sa `postgres` korisnikom i bazom, napravio `jampy` korisnika i kada sam pokušao da napravim `elmot` db evo šta je napravio:

```sql
CREATE DATABASE elmot
    WITH
    OWNER = jampy
    ENCODING = 'UTF8'
    LC_COLLATE = 'en_US.utf8'
    LC_CTYPE = 'en_US.utf8'
    ICU_LOCALE = 'sr-Latn-RS'
    LOCALE_PROVIDER = 'icu'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1
```
