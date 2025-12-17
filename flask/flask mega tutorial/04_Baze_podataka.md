
# Poglavlje 4 - Baze podataka

Tema ovog poglavlja je izuzetno važna. Za većinu aplikacija, postojaće potreba za održavanjem trajnih podataka koji se mogu efikasno preuzeti, a upravo za to su baze podataka i napravljene.

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.4), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.4.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.3...v0.4).

## Baze podataka u Flasku

Kao što ste sigurno već čuli, Flask ne podržava baze podataka izvorno. Ovo je jedno od mnogih područja u kojima Flask namerno nema mišljenje, što je odlično, jer imate slobodu da izaberete bazu podataka koja najbolje odgovara vašoj aplikaciji umesto da budete primorani da se prilagođavate jednoj.

Postoje odlični izbori za baze podataka u Pajtonu, mnogi od njih sa Flask ekstenzijama koje omogućavaju bolju integraciju sa aplikacijom. Baze podataka se mogu podeliti u dve velike grupe, one koje prate relacioni model i one koje ga ne prate. Potonja grupa se često naziva NoSQL, što ukazuje na to da ne implementiraju popularni relacioni jezik za upite SQL. Iako postoje odlični proizvodi za baze podataka u obe grupe, moje mišljenje je da su relacione baze podataka bolje rešenje za aplikacije koje imaju strukturirane podatke kao što su liste korisnika, objave na blogovima itd., dok su NoSQL baze podataka obično bolje za podatke koji imaju manje definisanu strukturu. Ova aplikacija, kao i većina drugih, može se implementirati korišćenjem bilo koje vrste baze podataka, ali iz gore navedenih razloga, ja ću se odlučiti za relacionu bazu podataka.

U 3. poglavlju sam vam pokazao prvo Flask proširenje. U ovom poglavlju ću koristiti još dva. Prvo je `Flask-SQLAlchemy`, proširenje koje pruža Flask - prijateljski omotač za popularni SQLAlchemy paket, koji je maper relacionih objekata ili ORM. ORM-ovi omogućavaju aplikacijama da upravljaju bazom podataka koristeći entitete visokog nivoa kao što su klase, objekti i metode umesto tabela i SQL-a. Zadatak ORM-a je da prevede operacije visokog nivoa u komande baze podataka.

Lepa stvar kod SQLAlchemy-ja je to što je ORM ne za jednu, već za mnoge relacione baze podataka. SQLAlchemy podržava dugačak spisak motora baza podataka, uključujući popularne MySQL, PostgreSQL i SQLite. Ovo je izuzetno moćno, jer možete da radite svoj razvoj koristeći jednostavnu SQLite bazu podataka koja ne zahteva server, a zatim kada dođe vreme za implementaciju aplikacije na produkcijskom serveru, možete da izaberete robusniji MySQL ili PostgreSQL server, bez potrebe da menjate svoju aplikaciju.

Da biste instalirali `Flask-SQLAlchemy` u svoje virtuelno okruženje, prvo se uverite da ste ga aktivirali, a zatim pokrenite:

```sh
(venv) $ pip install flask-sqlalchemy
```

## Migracije baza podataka

Većina tutorijala o bazama podataka koje sam video pokrivaju kreiranje i korišćenje baze podataka, ali ne obrađuju adekvatno problem ažuriranja postojeće baze podataka kako aplikacija treba da se menja ili raste. Ovo je teško jer su relacione baze podataka usredsređene na strukturirane podatke, tako da kada se struktura promeni, podaci koji su već u bazi podataka moraju se migrirati u izmenjenu strukturu.

Drugo proširenje koje ću predstaviti u ovom poglavlju je `Flask-Migrate`, koje sam zapravo ja kreirao. Ovo proširenje je Flask omotač za Alembic, okvir za migraciju baze podataka za SQLAlchemy. Rad sa migracijama baze podataka dodaje malo posla za pokretanje baze podataka, ali to je mala cena koju treba platiti za robustan način za pravljenje promena u vašoj bazi podataka u budućnosti.

Proces instalacije za `Flask-Migrate` je sličan drugim ekstenzijama koje ste videli:

```sh
(venv) $ pip install flask-migrate
```

### Konfiguracija Flask-SQLAlchemy-ja

Tokom razvoja, koristiću SQLite bazu podataka. SQLite baze podataka su najpogodniji izbor za razvoj malih aplikacija, ponekad čak i ne tako malih, jer se svaka baza podataka čuva u jednoj datoteci na disku i nema potrebe za pokretanjem servera baza podataka kao što su MySQL i PostgreSQL.

`Flask-SQLAlchemy` zahteva novu stavku konfiguracije dodatu u konfiguracionu datoteku:

> `config.py` : Konfiguracija `Flask-SQLAlchemy`-ja

```py
import os
basedir = os.path.abspath(os.path.dirname(__file__))

class Config:
    #...
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL') or \
        'sqlite:///' + os.path.join(basedir, 'app.db')
```

Ekstenzija `Flask-SQLAlchemy` uzima lokaciju baze podataka aplikacije iz `SQLALCHEMY_DATABASE_URI` konfiguracione promenljive. Kao što se sećate iz 3. poglavlja, generalno je dobra praksa da se konfiguracija podesi iz promenljivih okruženja i obezbedi rezervna vrednost kada okruženje ne definiše promenljivu. U ovom slučaju uzimam URL baze podataka iz `DATABASE_URL` promenljive okruženja, a ako to nije definisano, konfigurišem bazu podataka pod nazivom `app.db` koja se nalazi u glavnom direktorijumu aplikacije, a koja je sačuvana u `basedir` promenljivoj.

Baza podataka će u aplikaciji biti predstavljena instancom baze podataka. Mehanizam za migraciju baze podataka će takođe imati instancu. Ovo su objekti koje je potrebno kreirati nakon aplikacije, u datoteci `app/__init__.py` :

> `app/__init__.py` : Inicijalizacija `Flask-SQLAlchemy` i `Flask-Migrate`

```py
from flask import Flask
from config import Config
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate

app = Flask(__name__)
app.config.from_object(Config)
db = SQLAlchemy(app)
migrate = Migrate(app, db)

from app import routes, models
```

Napravio sam tri izmene u datoteci `__init__.py`. Prvo sam dodao `db` objekat koji predstavlja bazu podataka. Zatim sam dodao `migrate`, koji predstavlja mehanizam za migraciju baze podataka. Nadam se da vidite obrazac u radu sa Flask ekstenzijama. Većina ekstenzija je inicijalizovana kao ova dva. U poslednjoj promeni, uvozim novi modul koji se zove `models`, na dnu. Ovaj modul će definisati strukturu baze podataka.

### Modeli baza podataka

Podaci koji će biti sačuvani u bazi podataka biće predstavljeni kolekcijom klasa, obično nazvanih modeli baza podataka. ORM sloj unutar SQLAlchemy-ja će izvršiti prevode potrebne za mapiranje objekata kreiranih iz ovih klasa u redove u odgovarajućim tabelama baze podataka.

Počnimo kreiranjem modela koji predstavlja korisnike. Koristeći alat `WWW SQL Designer`, napravio sam dijagram za predstavljanje podataka koje želimo da koristimo u tabeli korisnici.

#### Tabela Users

- Polje `id` je obično u svim modelima i koristi se kao primarni ključ. Svakom korisniku u bazi podataka biće dodeljena jedinstvena vrednost ID-a, sačuvana u ovom polju. Primarni ključevi se, u većini slučajeva, automatski dodeljuju od strane baze podataka, tako da samo treba da navedem `id` polje označeno kao primarni ključ.

- Polja `username`, `email` i `password_hash` su definisana kao stringovi (ili VARCHAR u žargonu baza podataka), a njihove maksimalne dužine su navedene tako da baza podataka može da optimizuje korišćenje prostora. Iako su polja `username` i `email` sama po sebi razumljiva, `password_hash` zaslužuju određenu pažnju. Želim da budem siguran da aplikacija koju pravim usvaja najbolje bezbednosne prakse i iz tog razloga neću čuvati korisničke lozinke u običnom tekstu. Problem sa čuvanjem lozinki je taj što ako baza podataka ikada bude ugrožena, napadači će imati pristup lozinkama, a to bi moglo biti pogubno za korisnike. Umesto da direktno pišem lozinke, pisaću heševe lozinki, što značajno poboljšava bezbednost. Ovo će biti tema drugog poglavlja, zato se za sada ne brinite previše o tome.

Dakle, sada kada znam šta želim za svoju tabelu korisnika, mogu to pretvoriti u kod u novom modulu `app/models.py` :

> `app/models.py` : Model korisnika

```py
from typing import Optional
import sqlalchemy as sa
import sqlalchemy.orm as so
from app import db

class User(db.Model):
    id: so.Mapped[int] = so.mapped_column(primary_key=True)
    username: so.Mapped[str] = so.mapped_column(sa.String(64), index=True, unique=True)
    email: so.Mapped[str] = so.mapped_column(sa.String(120), index=True, unique=True)
    password_hash: so.Mapped[Optional[str]] = so.mapped_column(sa.String(256))

    def __repr__(self):
        return '<User {}>'.format(self.username)
```

Počinjem uvozom modula `sqlalchemy` i `sqlalchemy.orm` iz SQLAlchemy paketa, koji pružaju većinu elemenata potrebnih za rad sa bazom podataka. Modul `sqlalchemy` uključuje opšte funkcije baze podataka i klase kao što su tipovi i pomoćnici za izgradnju upita, dok `sqlalchemy.orm` pruža podršku za korišćenje modela. S obzirom da su imena ova dva modula dugačka i da će ih trebati često pozivati, alijasi sai sosu definisani direktno u naredbama za import. Instanca `db` iz `Flask-SQLAlchemy` i `Optional` - savet za tipove iz Pajtona su takođe uvezeni.

Klasa `User` kreirana iznad će predstavljati korisnike sačuvane u bazi podataka. Klasa nasleđuje `db.Model`, osnovnu klasu za sve modele iz `Flask-SQLAlchemy`. Model `User` definiše nekoliko polja kao promenljive klase. To su kolone koje će biti kreirane u odgovarajućoj tabeli baze podataka.

Poljima se dodeljuje tip pomoću Pajtonovih naznaka tipova, obmotanih `so.Mapped` generičkim tipom SQLAlchemy-ja. Deklaracija tipa kao što je `so.Mapped[int]` ili `so.Mapped[str]` definiše tip kolone, a takođe čini vrednosti obaveznim ili nultabilnim u terminima baze podataka. Da bi se definisala kolona kojoj je dozvoljeno da bude prazna ili nultabilna, `Optional`  se dodaje i pomoćnik iz Pajtona, kao što je u `password_hash` prikazano.

U većini slučajeva, definisanje kolone tabele zahteva više od samog tipa kolone. SQLAlchemy koristi `so.mapped_column()` poziv funkcije dodeljen svakoj koloni da bi obezbedio ovu dodatnu konfiguraciju. U gore navedenom slučaju, `id` kolona je konfigurisana kao primarni ključ. Za kolone tipa string, mnoge baze podataka zahtevaju da se navede dužina, tako da je i ovo uključeno. Uključio sam i druge opcione argumente koji mi omogućavaju da naznačim koja su polja jedinstvena i indeksirana, što je važno kako bi baza podataka bila konzistentna, a pretrage efikasne.

Metoda `__repr__` govori Pajtonu kako da ispiše objekte ove klase, što će biti korisno za debagovanje. Možete videti metodu `__repr__()` u akciji u sesiji Pajton interpretera ispod:

```py
>>> from app.models import User
>>> u = User(username='susan', email='susan@example.com')
>>> u
<User susan>
```

### Kreiranje migracionog repozitorijuma

Klasa modela kreirana u prethodnom odeljku definiše početnu strukturu baze podataka (ili šemu ) za ovu aplikaciju. Ali kako aplikacija nastavlja da raste, verovatno je da ću morati da napravim izmene u toj strukturi, kao što je dodavanje novih stvari, a ponekad i modifikovanje ili uklanjanje stavki. Alembic (frejmvork za migraciju koji koristi `Flask-Migrate`) će napraviti ove izmene šeme na način koji ne zahteva da se baza podataka ponovo kreira od nule svaki put kada se napravi promena.

Da bi obavio ovaj naizgled težak zadatak, Alembic održava repozitorijum migracija, što je direktorijum u kojem čuva svoje skripte za migraciju. Svaki put kada se izvrši promena u šemi baze podataka, skripta za migraciju se dodaje u repozitorijum sa detaljima promene. Da bi se migracije primenile na bazu podataka, ove skripte za migraciju se izvršavaju redosledom kojim su kreirane.

`Flask-Migrate` otkriva svoje komande putem `flask` komande. Već ste videli `flask run`, što je podkomanda koja je izvorna u Flask-u. `flask db` dodaje podkomandu `Flask-Migrate` da bi se upravljalo svim što je vezano za migracije baze podataka. Dakle, hajde da kreiramo spremište migracija za mikroblog pokretanjem `flask db init`:

```sh
(venv) $ flask db init
  Creating directory /home/miguel/microblog/migrations... done
  Creating directory /home/miguel/microblog/migrations/versions... done
  Generating /home/miguel/microblog/migrations/alembic.ini... done
  Generating /home/miguel/microblog/migrations/env.py... done
  Generating /home/miguel/microblog/migrations/README... done
  Generating /home/miguel/microblog/migrations/script.py.mako... done
  Please edit configuration/connection/logging settings in
  '/home/miguel/microblog/migrations/alembic.ini' before proceeding.
```

Zapamtite da `flask` komanda zavisi od `FLASK_APP` promenljive okruženja da bi znala gde se nalazi Flask aplikacija. Za ovu aplikaciju, želite da podesite `FLASK_APP` vrednost `microblog.py`, kao što je objašnjeno u Poglavlju 1. Ako ste uključili `.flaskenv` datoteku u svoj projekat, onda će sve podkomande komande `flask` automatski imati pristup aplikaciji.

Nakon što pokrenete `flask db init` komandu, pronaći ćete novi direktorijum za migracije, sa nekoliko datoteka i poddirektorijumom za verzije unutra. Sve ove datoteke treba od sada tretirati kao deo vašeg projekta, a posebno ih treba dodati u kontrolu izvornog koda zajedno sa kodom vaše aplikacije.

#### Prva migracija baze podataka

Kada je spremište migracija na mestu, vreme je da se kreira prva migracija baze podataka, koja će uključivati tabelu `users` koja se mapira na `User` model baze podataka. Postoje dva načina za kreiranje migracije baze podataka:

- ručno ili
- automatski.

Da bi se migracija automatski generisala, Alembic upoređuje šemu baze podataka kako je definisana modelima baze podataka sa stvarnom šemom baze podataka koja se trenutno koristi u bazi podataka. Zatim popunjava skriptu za migraciju potrebnim promenama da bi šema baze podataka odgovarala modelima aplikacije. U ovom slučaju, pošto ne postoji prethodna baza podataka, automatska migracija će dodati ceo `User` model u skriptu za migraciju. `flask db migrate` podkomanda generiše ove automatske migracije:

```sh
(venv) $ flask db migrate -m "users table"
INFO  [alembic.runtime.migration] Context impl SQLiteImpl.
INFO  [alembic.runtime.migration] Will assume non-transactional DDL.
INFO  [alembic.autogenerate.compare] Detected added table 'user'
INFO  [alembic.autogenerate.compare] Detected added index 'ix_user_email' on '['email']'
INFO  [alembic.autogenerate.compare] Detected added index 'ix_user_username' on '['username']'
  Generating /home/miguel/microblog/migrations/versions/e517276bb1c2_users_table.py... done
```

Izlaz komande vam daje predstavu o tome šta je Alembic uključio u migraciju. Prva dva reda su informativna i obično se mogu ignorisati. Zatim se kaže da je pronašao korisničku tabelu i dva indeksa. Zatim vam se govori gde je napisao skriptu za migraciju. Vrednost e517276bb1c2je automatski generisan i jedinstven kod za migraciju (za vas će biti drugačiji). Komentar dat uz `-m` opciju je opcionalan, on samo dodaje kratak opisni tekst migraciji.

Generisani skript za migraciju je sada deo vašeg projekta i, ako koristite git ili drugi alat za kontrolu izvornog koda, potrebno ga je uključiti kao dodatnu izvornu datoteku, zajedno sa svim ostalim datotekama sačuvanim u direktorijumu za migracije. Slobodno možete da pregledate skriptu ako ste radoznali da vidite kako izgleda. Videćete da ima dve funkcije koje se zovu `upgrade()` i `downgrade()`. Funkcija `upgrade()` primenjuje migraciju, a funkcija `downgrade()` je uklanja. Ovo omogućava Alembic-u da migrira bazu podataka na bilo koju tačku u istoriji, čak i na starije verzije, koristeći putanju za vraćanje na stariju verziju.

Komanda `flask db migrate` ne pravi nikakve izmene u bazi podataka, već samo generiše skriptu za migraciju. Da biste primenili izmene na bazu podataka, mora se koristiti `flask db upgrade` komanda.

```sh
(venv) $ flask db upgrade
INFO  [alembic.runtime.migration] Context impl SQLiteImpl.
INFO  [alembic.runtime.migration] Will assume non-transactional DDL.
INFO  [alembic.runtime.migration] Running upgrade  -> e517276bb1c2, users table
```

Pošto ova aplikacija koristi SQLite, `upgrade` komanda će otkriti da baza podataka ne postoji i kreiraće je (primetićete da se datoteka pod nazivom app.db dodaje nakon što se ova komanda završi, to je SQLite baza podataka). Kada radite sa serverima baza podataka kao što su MySQL i PostgreSQL, morate da kreirate bazu podataka na serveru baze podataka pre pokretanja `upgrade`.

Imajte na umu da `Flask-SQLAlchemy` podrazumevano koristi konvenciju imenovanja "zmijsko-veliko slovo" za tabele baze podataka. Za gore navedeni `User` model, odgovarajuća tabela u bazi podataka će biti nazvana `user`. Za `AddressAndPhone` klasu modela, tabela bi se zvala `address_and_phone`. Ako više volite da sami birate imena tabela, možete dodati atribut imenovan `__tablename__` klasi modela, postavljen na željeno ime kao string.

#### Tok rada za nadogradnju i vraćanje baze podataka

Aplikacija je trenutno u povojima, ali ne škodi razgovarati o tome kakva će biti strategija migracije baze podataka u budućnosti. Zamislite da imate aplikaciju na razvojnoj mašini, a takođe imate kopiju raspoređenu na proizvodnom serveru koji je onlajn i u upotrebi.

Recimo da za sledeće izdanje vaše aplikacije morate da uvedete promenu u svoje modele, na primer, potrebno je dodati novu tabelu. Bez migracija, morali biste da smislite kako da promenite šemu vaše baze podataka, kako na vašoj razvojnoj mašini, tako i na vašem serveru, a to bi moglo biti mnogo posla.

Ali uz podršku za migraciju baze podataka, nakon što izmenite modele u vašoj aplikaciji, generišete novi skript za migraciju ( `flask db migrate` ), pregledate ga da biste se uverili da je automatsko generisanje uradilo ispravnu stvar, a zatim primenite promene na vašu razvojnu bazu podataka ( `flask db upgrade` ). Dodaćete skriptu za migraciju u kontrolu izvornog koda i potvrditi je.

Kada budete spremni da objavite novu verziju aplikacije na vašem produkcijskom serveru, sve što treba da uradite je da preuzmete ažuriranu verziju vaše aplikacije, koja će sadržati novi skript za migraciju, i pokrenete `flask db upgrade`. Alembic će otkriti da produkcijska baza podataka nije ažurirana na najnoviju reviziju šeme i pokrenuti sve nove skripte za migraciju koje su kreirane nakon prethodnog izdanja.

Kao što sam ranije pomenuo, imate i `flask db downgrade` komandu koja poništava poslednju migraciju. Iako vam verovatno neće biti potrebna ova opcija na produkcijskom sistemu, može vam biti veoma korisna tokom razvoja. Možda ste generisali skriptu za migraciju i primenili je, samo da biste otkrili da promene koje ste napravili nisu baš ono što vam je potrebno. U tom slučaju, možete vratiti bazu podataka na stariju verziju, obrisati skriptu za migraciju, a zatim generisati novu koja će je zameniti.

## Relacije na bazama podataka

Relacione baze podataka su dobre za čuvanje relacija između stavki podataka. Razmotrimo slučaj korisnika koji piše blog post. Korisnik će imati zapis u tabeli `users`, a `post` će imati zapis u tabeli `posts`. Najefikasniji način da se zabeleži ko je napisao dati post jeste povezivanje dva povezana zapisa.

Kada se uspostavi veza između korisnika i objave, baza podataka može da odgovori na upite o ovoj vezi. Najtrivijalniji upit je kada imate blog objavu i potrebno je da znate koji je korisnik napisao. Složeniji upit je suprotan ovom. Ako imate korisnika, možda ćete želeti da znate sve objave koje je taj korisnik napisao. SQLAlchemy pomaže kod obe vrste upita.

Hajde da proširimo bazu podataka da bismo čuvali blog postove i videli relacije u akciji. Evo šeme za novu tabelu postova:

### Tabela Posts

Tabela objava će imati obavezna polja:

- `id`,
- `body` i
- `timestamp`.
- Ali pored ovih očekivanih polja, dodajem `user_id` polje koje povezuje objavu sa njenim autorom.

Videli ste da svi korisnici imaju `id` primarni ključ, koji je jedinstven. Način da povežete objavu na blogu sa korisnikom koji ju je napisao jeste da dodate referencu na korisnikov `id`, i to je upravo ono čemu služi `user_id` polje. Ovo `user_id` polje se naziva strani ključ, jer se odnosi na primarni ključ druge tabele. Dijagram baze podataka prikazuje strane ključeve kao vezu između polja i polja `id` tabele na koju se odnosi. Ova vrsta odnosa se naziva `jedan-prema-više`, jer "jedan" korisnik piše "više" objava.

Izmenjena datoteka `app/models.py` je prikazana ispod:

> `app/models.py` : Tabela baze podataka Post, User i relacija

```py
from datetime import datetime, timezone
from typing import Optional
import sqlalchemy as sa
import sqlalchemy.orm as so
from app import db

class User(db.Model):
    id: so.Mapped[int] = so.mapped_column(primary_key=True)
    username: so.Mapped[str] = so.mapped_column(sa.String(64), index=True,
                                                unique=True)
    email: so.Mapped[str] = so.mapped_column(sa.String(120), index=True,
                                             unique=True)
    password_hash: so.Mapped[Optional[str]] = so.mapped_column(sa.String(256))

    posts: so.WriteOnlyMapped['Post'] = so.relationship(
        back_populates='author')

    def __repr__(self):
        return '<User {}>'.format(self.username)

class Post(db.Model):
    id: so.Mapped[int] = so.mapped_column(primary_key=True)
    body: so.Mapped[str] = so.mapped_column(sa.String(140))
    timestamp: so.Mapped[datetime] = so.mapped_column(
        index=True, default=lambda: datetime.now(timezone.utc))
    user_id: so.Mapped[int] = so.mapped_column(sa.ForeignKey(User.id),
                                               index=True)

    author: so.Mapped[User] = so.relationship(back_populates='posts')

    def __repr__(self):
        return '<Post {}>'.format(self.body)
```

Nova `Post` klasa će predstavljati blog postove koje su napisali korisnici.

- Polje `timestamp` je definisano pomoću `datetime` naznake tipa i konfigurisano je da bude indeksirano, što je korisno ako želite da efikasno preuzimate postove hronološkim redom. Takođe sam dodao `default` argument i prosledio lambda funkciju koja vraća trenutno vreme u UTC vremenskoj zoni. Kada prosledite funkciju kao podrazumevanu, SQLAlchemy će postaviti polje na vrednost koju vraća funkcija. Generalno, želećete da radite sa UTC datumima i vremenima u serverskoj aplikaciji umesto sa lokalnim vremenom gde se nalazite. Ovo osigurava da koristite jedinstvene vremenske oznake bez obzira gde se nalaze korisnici i server. Ove vremenske oznake će biti konvertovane u lokalno vreme korisnika kada se prikažu.

- Polje `user_id` je inicijalizovano kao strani ključ za `User.id`, što znači da referencira vrednosti iz `id` kolone u tabeli `User`. Pošto ne kreiraju sve baze podataka automatski indeks za strane ključeve, opcija `index=True` je eksplicitno dodata, tako da su pretrage zasnovane na ovoj koloni optimizovane.

- Klasa `User` ima novo `posts` polje koje je inicijalizovano sa `so.relationship()`. Ovo nije stvarno polje baze podataka, već prikaz visokog nivoa odnosa između korisnika i objava, i zbog toga se ne nalazi u dijagramu baze podataka.

- Slično tome, klasa `Post` ima `author` polje koje je takođe inicijalizovano kao odnos. Ova dva atributa omogućavaju aplikaciji da pristupi povezanim `user` i `post` unosima.

- Prvi argument `so.relationship()` je klasa modela koja predstavlja drugu stranu odnosa. Ovaj argument može biti naveden kao string, što je neophodno kada se klasa definiše kasnije u modulu. Argumenti `back_populates` se odnose na ime atributa odnosa na drugoj strani, tako da SQLAlchemy zna da se ovi atributi odnose na dve strane istog odnosa.

Atribut relacije  `posts` koristi drugačiju definiciju tipa. Umesto `so.Mapped`, koristi `so.WriteOnlyMapped`, što definiše `posts` kao tip kolekcije sa `Post` objektima unutra. Ne brinite ako vam ovi detalji još uvek nemaju mnogo smisla, pokazaću vam primere na kraju ovog članka.

Pošto imam ažuriranja modela aplikacija, potrebno je generisati novu migraciju baze podataka:

```sh
(venv) $ flask db migrate -m "posts table"
INFO  [alembic.runtime.migration] Context impl SQLiteImpl.
INFO  [alembic.runtime.migration] Will assume non-transactional DDL.
INFO  [alembic.autogenerate.compare] Detected added table 'post'
INFO  [alembic.autogenerate.compare] Detected added index 'ix_post_timestamp' on '['timestamp']'
  Generating /home/miguel/microblog/migrations/versions/780739b227a7_posts_table.py... done
```

I migracija treba da se primeni na bazu podataka:

```sh
(venv) $ flask db upgrade
INFO  [alembic.runtime.migration] Context impl SQLiteImpl.
INFO  [alembic.runtime.migration] Will assume non-transactional DDL.
INFO  [alembic.runtime.migration] Running upgrade e517276bb1c2 -> 780739b227a7, posts table
```

Ako čuvate svoj projekat u kontroli izvornog koda, ne zaboravite da mu dodate i novi skript za migraciju.

## Rad sa bazom podataka

Naterao sam vas da patite kroz dug proces definisanja baze podataka, ali vam još nisam pokazao kako sve funkcioniše. Pošto aplikacija još uvek nema nikakvu logiku baze podataka, hajde da se poigramo sa bazom podataka u Pajton interpreteru da bismo se upoznali sa njom. Pokrenite Pajton pokretanjem pythona vašem terminalu. Uverite se da je vaše virtuelno okruženje aktivirano pre nego što pokrenete interpreter.

Kada se nađemo u Pajton promptu, uvezimo aplikaciju, instancu baze podataka, modele i SQLAlchemy ulaznu tačku:

```py
>>> from app import app, db
>>> from app.models import User, Post
>>> import sqlalchemy as sa
```

Sledeći korak je pomalo čudan. Da bi Flask i njegova proširenja imali pristup Flask aplikaciji bez potrebe da se prosleđujs `app` kao argument u svaku funkciju, mora se kreirati i pokrenuti kontekst aplikacije. Konteksti aplikacije će biti detaljnije obrađeni kasnije u tutorijalu, pa za sada, ukucajte sledeći kod u vašoj Python shell sesiji:

```py
>>> app.app_context().push()
```

Zatim, kreirajte novog korisnika:

```py
>>> u = User(username='john', email='john@example.com')
>>> db.session.add(u)
>>> db.session.commit()
```

Promene u bazi podataka se vrše u kontekstu sesije baze podataka, kojoj se može pristupiti kao `db.session`. Više promena može se akumulirati u sesiji i kada se sve promene registruju, možete izdati jednu `db.session.commit()`, koja atomski zapisuje sve promene. Ako u bilo kom trenutku tokom rada na sesiji dođe do greške, poziv na `db.session.rollback()` će prekinuti sesiju i ukloniti sve promene sačuvane u njoj. Važno je zapamtiti da se promene zapisuju u bazu podataka samo kada se izda `commit` sa `db.session.commit()`. Sesije garantuju da baza podataka nikada neće biti ostavljena u nekonzistentnom stanju.

Da li se pitate kako sve ove operacije sa bazom podataka znaju koju bazu podataka da koriste? Kontekst aplikacije koji je potisnut gore omogućava `Flask-SQLAlchemy`-ju da pristupi instanci Flask aplikacije `app` bez potrebe da je primi kao argument. Ekstenzija traži u rečniku `app.config` unos `SQLALCHEMY_DATABASE_URI` koji sadrži URL adresu baze podataka.

Dodajmo još jednog korisnika:

```py
>>> u = User(username='susan', email='susan@example.com')
>>> db.session.add(u)
>>> db.session.commit()
```

Baza podataka može da odgovori na upit koji vraća sve korisnike:

```py
>>> query = sa.select(User)
>>> users = db.session.scalars(query).all()
>>> users
[<User john>, <User susan>]
```

Promenljivoj `query` u ovom primeru je dodeljen osnovni upit koji bira sve korisnike. To se postiže prosleđivanjem klase modela `sa.select()` pomoćnoj funkciji za upite SQLAlchemy. Videćete da većina upita baze podataka počinje pozivom `sa.select()`.

Sesija baze podataka, koja je gore korišćena za definisanje i potvrđivanje promena, takođe se koristi za izvršavanje upita. Metoda `db.session.scalars()` izvršava upit baze podataka i vraća iterator rezultata. Pozivanje metode `all()` objekta rezultata konvertuje rezultate u običnu listu.

U mnogim situacijama je najefikasnije koristiti iterator rezultata u `for` - petlji umesto da ga konvertujete u listu:

```py
>>> users = db.session.scalars(query)
>>> for u in users:
...     print(u.id, u.username)
...
1 john
2 susan
```

Imajte na umu da su polja `id` automatski podešena na 1 i 2 kada su ti korisnici dodati. To se dešava zato što SQLAlchemy konfiguriše kolone primarnog ključa sa celim brojem da se automatski povećavaju.

Evo još jednog načina za upite. Ako znate vrednost `id` korisnika, možete ga pronaći na sledeći način:

```py
>>> u = db.session.get(User, 1)
>>> u
<User john>
```

Sada dodajmo objavu na blogu:

```py
>>> u = db.session.get(User, 1)
>>> p = Post(body='my first post!', author=u)
>>> db.session.add(p)
>>> db.session.commit()
```

Nisam morao da podešavam vrednost za `timestamp` polje, jer ovo polje ima podrazumevanu vrednost, koju možete videti u definiciji modela. A šta je sa `user_id` poljem? Podsetimo se da `so.relationship` koje sam kreirao u `Post` klasi dodaje `author` atribut objavama. Dodeljujem autora objavi koristeći ovo authorpolje umesto da se bavim korisničkim ID-ovima. SQLAlchemy je odličan u tom pogledu, jer pruža apstrakciju visokog nivoa nad odnosima i stranim ključevima.

Da bismo završili ovu sesiju, pogledajmo još nekoliko upita baze podataka:

```py
>>> # get all posts written by a user
>>> u = db.session.get(User, 1)
>>> u
<User john>
>>> query = u.posts.select()
>>> posts = db.session.scalars(query).all()
>>> posts
[<Post my first post!>]

>>> # same, but with a user that has no posts
>>> u = db.session.get(User, 2)
>>> u
<User susan>
>>> query = u.posts.select()
>>> posts = db.session.scalars(query).all()
>>> posts
[]

>>> # print post author and body for all posts
>>> query = sa.select(Post)
>>> posts = db.session.scalars(query)
>>> for p in posts:
...     print(p.id, p.author.username, p.body)
...
1 john my first post!

# get all users in reverse alphabetical order
>>> query = sa.select(User).order_by(User.username.desc())
>>> db.session.scalars(query).all()
[<User susan>, <User john>]

# get all users that have usernames starting with "s"
>>> query = sa.select(User).where(User.username.like('s%'))
>>> db.session.scalars(query).all()
[<User susan>]
```

Obratite pažnju na to kako se u prva dva primera iznad koristi odnos između korisnika i objava. Podsetimo se da model `User` ima `posts` atribut relacije koji je konfigurisan sa `WriteOnlyMapped` generičkim tipom. Ovo je poseban tip odnosa koji dodaje `select()` metodu koja vraća upit baze podataka za povezane stavke. `posts.select()` izraz se brine o generisanju upita koji povezuje korisnika sa njegovim objavama na blogu.

Poslednji upit pokazuje kako filtrirati sadržaj tabele koristeći uslov. Klauzula `where()` se koristi za kreiranje filtera koji biraju samo podskup redova iz izabranog entiteta. U ovom primeru koristim `like()` operator za izbor korisnika na osnovu patterna.

Dokumentacija SQLAlchemy-ja je najbolje mesto za učenje o mnogim opcijama koje su dostupne za upite na bazi podataka.

Za kraj, izađite iz Pajton ljuske i koristite sledeće komande da obrišete test korisnike i objave kreirane gore, tako da baza podataka bude čista i spremna za sledeće poglavlje:

```sh
(venv) flask db downgrade base
(venv) flask db upgrade
```

Prva komanda govori `Flask-Migrate`-u da primeni migracije baze podataka obrnutim redosledom. Kada `downgrade` komandi nije dodeljen cilj, ona vraća jednu reviziju na stariju verziju. Cilj `base` uzrokuje da se sve migracije vraćaju na stariju verziju, sve dok baza podataka ne ostane u početnom stanju, bez tabela.

Komanda `upgrade` ponovo primenjuje sve migracije redosledom unapred. Podrazumevani cilj za nadogradnje je `head`, što je prečica za najnoviju migraciju. Ova komanda efikasno vraća tabele koje su iznad vraćene na stariju verziju. Pošto migracije baze podataka ne čuvaju podatke sačuvane u bazi podataka, vraćanje na stariju verziju, a zatim nadogradnja, ima efekat brzog pražnjenja svih tabela.

## Kontekst školjke

Sećate se šta ste uradili na početku prethodnog odeljka, odmah nakon pokretanja Pajton interpretera? Na početku ste uneli neke importe, a zatim ste pokrenuli kontekst aplikacije:

```py
>>> from app import app, db
>>> from app.models import User, Post
>>> import sqlalchemy as sa
>>> app.app_context().push()
```

Dok radite na svojoj aplikaciji, moraćete veoma često da testirate stvari u Pajton ljusci, tako da će ponavljanje gore navedenih izjava svaki put postati dosadno. Ovo je dobar trenutak da se pozabavite ovim problemom.

Podkomanda `flask shell` je još jedan veoma koristan alat u `flask` grupi komandi. Komanda `shell` je druga "osnovna" komanda koju implementira Flask, posle `run`. Svrha ove komande je pokretanje Pajton interpretera u kontekstu aplikacije. Šta to znači? Pogledajte sledeći primer:

```py
(venv) python
>>> app
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: name 'app' is not defined
>>>

(venv) flask shell
>>> app
<Flask 'app'>
```

Kod regularne sesije interpretera, `app` simbol nije poznat osim ako nije eksplicitno uvezen, ali kada se koristi `flask shell`, komanda prethodno uvozi instancu aplikacije i prosleđuje njen kontekst aplikacije umesto vas. Lepota `flask shell` je što ne samo da prethodno uvozi `app`, već i što možete konfigurisati "kontekst školjke", što je lista drugih simbola za prethodno uvoz.

Sledeća funkcija u `microblog.py` kreira kontekst `shell`-a koji dodaje instancu baze podataka i modele u sesiju `shell`-a:

```py
import sqlalchemy as sa
import sqlalchemy.orm as so
from app import app, db
from app.models import User, Post

@app.shell_context_processor
def make_shell_context():
    return {'sa': sa, 'so': so, 'db': db, 'User': User, 'Post': Post}
```

Dekorator `app.shell_context_processor` registruje funkciju kao funkciju konteksta shell-a. Kada `flask shell` se pokrene, ona će pozvati ovu funkciju i registrovati stavke koje je vratila u sesiji šella. Razlog zašto funkcija vraća rečnik, a ne listu, je taj što za svaku stavku morate dati i ime pod kojim će se na nju pozivati u šelu, koje je dato ključevima rečnika.

Nakon što dodate funkciju obrade konteksta školjke, možete raditi sa entitetima baze podataka bez potrebe da ih uvozite:

```py
(venv) $ flask shell
>>> db
<SQLAlchemy sqlite:////home/miguel/microblog/app.db>
>>> User
<class 'app.models.User'>
>>> Post
<class 'app.models.Post'>
```

Ako pokušate gore navedeno i dobijete `NameError` izuzetke kada pokušate da pristupite `sa`, `so`, `db`, `User` i `Post`, onda `make_shell_context()` funkcija nije registrovana sa Flask-om. Najverovatniji uzrok ovoga je da niste podesili `FLASK_APP=microblog.py` u okruženju. U tom slučaju, vratite se na Poglavlje 1 i pregledajte kako da podesite `FLASK_APP` promenljivu okruženja. Ako često zaboravljate da podesite ovu promenljivu kada otvarate nove prozore terminala, možete razmotriti dodavanje `.flaskenv` datoteke u vaš projekat, kao što je opisano na kraju tog poglavlja.
