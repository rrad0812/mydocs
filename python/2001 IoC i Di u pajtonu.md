
# IoCc i DI u Pajtonu

Inverzija kontrole (IoC) i „dependency injection“ (DI) su koncepti koji se u Pythonu ne koriste baš onako strogo kao u Javi ili C#, ali i te kako imaju smisla i mogu da učine tvoj kod fleksibilnijim i lakšim za testiranje.

## Šta je IoC?

Inverzija kontrole znači da ne pravi tvoja klasa sama zavisnosti koje joj trebaju, nego ih dobija „spolja“. Time klasa postaje manje zavisna od konkretnih implementacija, a više od apstrakcija.

Na primer, umesto da klasa sama pravi konekciju ka bazi, ti joj proslediš objekat koji već zna da radi sa bazom. Ako kasnije hoćeš da zameniš bazu fajlom ili mock-om za testiranje, to uradiš bez menjanja koda same klase.

### Najjednostavniji primer bez IoC**

```py
class Database:
    def query(self):
        return "Podaci iz baze"

class Service:
    def __init__(self):
        self.db = Database()  # Service sam pravi Database

    def get_data(self):
        return self.db.query()

s = Service()
print(s.get_data())
```

Ovde je `Service` tesno povezan sa Database. Ako hoćeš drugu implementaciju (npr. `MockDatabase` za testiranje), moraš da menjaš `Service`.

### IoC (Dependency Injection)

```py
class Database:
    def query(self):
        return "Podaci iz baze"

class MockDatabase:
    def query(self):
        return "Test podaci"

class Service:
    def __init__(self, db):  # Zavisnost ulazi spolja
        self.db = db

    def get_data(self):
        return self.db.query()

s1 = Service(Database())
print(s1.get_data())  # "Podaci iz baze"

# U testu
s2 = Service(MockDatabase())
print(s2.get_data())  # "Test podaci"
```

Sad klasa `Service` ne zna ništa o konkretnom tipu baze – samo zna da postoji objekat sa metodom `query`.

### IoC container (malo naprednije)

U Pythonu se retko koriste formalni IoC container-i (kao u Spring-u), jer dinamička priroda jezika omogućava da DI radimo vrlo jednostavno. Ali postoje biblioteke poput `dependency-injector` koje ti daju kontejnere i konfiguraciju zavisnosti.

Mali primer sa tom bibliotekom:

```py
from dependency_injector import containers, providers

class Database:
    def query(self):
        return "Podaci iz baze"

class Service:
    def __init__(self, db):
        self.db = db

    def get_data(self):
        return self.db.query()

# Definišemo container
class Container(containers.DeclarativeContainer):
    database = providers.Factory(Database)
    service = providers.Factory(Service, db=database)

# Koristimo container
container = Container()
service = container.service()

print(service.get_data())  # "Podaci iz baze"
```

Predlog za učenje:

- Eksperimentiši sa običnim DI bez biblioteka (konstruktorska injekcija).
- Probaj da pišeš testove gde ubacuješ mock objekte.
- Ako ti zatreba ozbiljnija aplikacija (npr. API sa FastAPI ili Flask), pogledaj `dependency-injector`.

## Mini FastAPI projekat koji koristi IoC (dependency injection)

Cilj: imamo `UserService` koji radi sa korisnicima. On ne zna da li podaci dolaze iz prave baze ili iz mock-a. FastAPI će da ubaci zavisnost preko DI mehanizma.

### Struktura projekta

```sh
project/
│── main.py
│── services.py
│── repositories.py
```

`repositories.py`

```py
from typing import List, Dict

# "Interface" - Python nema interfejse, ali koristimo dogovor
class UserRepository:
    def get_users(self) -> List[Dict]:
        raise NotImplementedError


class InMemoryUserRepository(UserRepository):
    def __init__(self):
        self._users = [
            {"id": 1, "name": "Pera"},
            {"id": 2, "name": "Mika"},
        ]

    def get_users(self) -> List[Dict]:
        return self._users


class MockUserRepository(UserRepository):
    def get_users(self) -> List[Dict]:
        return [{"id": 999, "name": "Test User"}]
```

`services.py`

```py
from typing import List, Dict
from .repositories import UserRepository

class UserService:
    def __init__(self, repo: UserRepository):
        self._repo = repo

    def list_users(self) -> List[Dict]:
        return self._repo.get_users()
```

`main.py`

```py
from fastapi import FastAPI, Depends
from .services import UserService
from .repositories import InMemoryUserRepository, MockUserRepository

app = FastAPI()

# --- IoC: biramo implementaciju ---
def get_user_service() -> UserService:
    repo = InMemoryUserRepository()  # 👉 ovde menjaš repo
    return UserService(repo)

@app.get("/users")
def list_users(service: UserService = Depends(get_user_service)):
    return service.list_users()
```

**Pokretanje**:

```sh
uvicorn main:app --reload
```

**Otvori u browseru**:

```sh
http://127.0.0.1:8000/users
```

Dobićeš JSON:

```json
[
  {"id": 1, "name": "Pera"},
  {"id": 2, "name": "Mika"}
]
```

Ako u `get_user_service` zameniš `InMemoryUserRepository()` sa `MockUserRepository()`, API će vraćati samo test korisnika.

Ovo je baš jednostavan primer, ali lepo pokazuje kako IoC radi u Python + FastAPI kontekstu.

### Zašto uopšte pričamo o IoC i DI, kad u Pythonu i ovako mogu da prosledim objekat konstruktoru?

**"Ručno prosleđivanje" vs. IoC**:

Tačno je – u Pythonu (ili bilo kom jeziku) uvek možeš ručno da proslediš zavisnost u konstruktor.

**Primer (ručno)**:

```py
repo = InMemoryUserRepository()
service = UserService(repo)
```

Ovo radi i to je zapravo `dependency injection` – injektuješ zavisnost spolja. Ali to nije još uvek IoC kontejner, nego samo DI "na mišiće".

**Gde nastaje problem**:

Zamisli da tvoja aplikacija raste:

- imaš 20 različitih servisa
- svaki koristi različite repozitorijume ili druge zavisnosti
- repozitorijumi koriste konekcije, fajl sisteme, API klijente, itd.

Ako sve to radiš ručno, dobijaš puno koda za „vezivanje“ objekata:

```py
db = DatabaseConnection("localhost")
repo = UserRepository(db)
service = UserService(repo)
controller = UserController(service)
```

I sad svuda u kodu moraš ponavljati istu stvar, ili ručno menjati kada hoćeš npr. mock za testiranje.

**Prednosti IoC kontejnera**:

IoC kontejner (ili DI framework) rešava ovo na centralizovan način:

- `Centralizacija konfiguracije` – jednom definišeš:
  "Za UserRepository koristi InMemoryUserRepository", i kontejner to svuda po app-u sam ubacuje.

- `Laka zamena implementacije` – u testovima možeš da prebaciš na
  "MockRepository" bez da menjaš kod servisa.

- `Lazy instanciranje` i `scope` – npr. pravi novi objekat za svaki request
  (scoped), ili koristi isti objekat kroz ceo program (`singleton`).

- `Manje "žičenja"` – ne moraš ti ručno da praviš lance zavisnosti,
  kontejner to sam uradi.

### FastAPI primer

FastAPI u stvari koristi IoC light.

Kad kažeš:

```py
@app.get("/users")
def list_users(service: UserService = Depends(get_user_service)):
    return service.list_users()
```

FastAPI preuzima kontrolu nad kreiranjem objekata.

Ti si samo rekao "daj mi UserService na ovaj način", a framework ga ubacuje.

To je IoC – tvoja funkcija više ne kontroliše kako dobija service, nego framework.

### Ukratko

- `Bez IoC`: ti praviš sve objekte, prosleđuješ ih i menjaš ručno.
- `Sa IoC`: ti samo definišeš pravila (koji servis koristi koju zavisnost), a framework vodi računa o
  instanciranju i ubacivanju.
- `Drugim rečima`: DI možeš raditi i sam, IoC framework to radi umesto tebe i olakšava ti posao u 
  većim aplikacijama.

### Razlika ručno vs. IoC (FastAPI stil)

**Primer: Controller → Service → Repository → Database**:

Imamo četiri sloja:

- Database – npr. SQLite konekcija (ili mock).
- Repository – radi sa bazom.
- Service – poslovna logika.
- Controller (API) – izlaže endpoint.

**Ručno povezivanje (bez IoC)**:

`database.py`

```py
class Database:
    def query(self, sql: str):
        return f"Rezultat iz baze za upit: {sql}"
```

`repositories.py`

```py
class UserRepository:
    def __init__(self, db: Database):
        self.db = db

    def get_users(self):
        return self.db.query("SELECT * FROM users")
```

`services.py`

```py
class UserService:
    def __init__(self, repo: UserRepository):
        self.repo = repo

    def list_users(self):
        return self.repo.get_users()
```

`main_manual.py`

```py
from fastapi import FastAPI
from database import Database
from repositories import UserRepository
from services import UserService

app = FastAPI()

# ručno povezivanje
db = Database()
repo = UserRepository(db)
service = UserService(repo)

@app.get("/users")
def list_users():
    return service.list_users()
```

Ovde mi pravimo sve objekte (db → repo → service) i prosleđujemo dalje.
Ako hoću MockDatabase, moram da menjam kod u main_manual.py.

**IoC stil (FastAPI zavisnosti)**:

`database.py`

```py
class Database:
    def query(self, sql: str):
        return f"Rezultat iz baze za upit: {sql}"

class MockDatabase(Database):
    def query(self, sql: str):
        return f"MOCK podaci za upit: {sql}"
```

`repositories.py`

```py
class UserRepository:
    def __init__(self, db: Database):
        self.db = db

    def get_users(self):
        return self.db.query("SELECT * FROM users")
```

`services.py`

```
class UserService:
    def __init__(self, repo: UserRepository):
        self.repo = repo

    def list_users(self):
        return self.repo.get_users()
```

`main_ioc.py`

```py
from fastapi import FastAPI, Depends
from database import Database, MockDatabase
from repositories import UserRepository
from services import UserService

app = FastAPI()

# --- definicija zavisnosti ---
def get_db():
    return Database()  # 👉 ovde možeš lako promeniti u MockDatabase()

def get_user_repo(db: Database = Depends(get_db)):
    return UserRepository(db)

def get_user_service(repo: UserRepository = Depends(get_user_repo)):
    return UserService(repo)

@app.get("/users")
def list_users(service: UserService = Depends(get_user_service)):
    return service.list_users()
```

Ovde:

- Mi samo kažemo pravila: UserService zavisi od UserRepository, a on od Database.
- FastAPI sam rešava lance zavisnosti i ubacuje gde treba.
- Ako želim mock bazu u testu, promenim samo get_db() i ništa drugo.

**Ključna razlika**:

- Ručno: ti vodiš računa o tome kako se objekti kreiraju i povezuju.
- IoC (FastAPI): ti samo definišeš "šta od čega zavisi", framework sam pravi objekte i ubacuje ih.

To postaje ogromna ušteda kad app naraste (npr. 50 servisa i repozitorijuma).

## Masonite IoC

Evo kako Masonite koristi IoC (servisni kontejner / Service Container) i automatsko rešavanje zavisnosti — objasniću koncept, plus konkretne stvari kako se to vidi u kodu, da bude jasnije.

### Osnovni pojmovi u Masonite

- `Service Container` (IoC Container) — mesto gde se "vezuju" (bind) klase ili instance i odakle se 
  te zavisnosti mogu injektovati automatski.

- `Service Providers` — klase koje služe da registruju zavisnosti u container, da konfigurišu 
  binding, da dodaju "features" u aplikaciju (npr. baza, mejl, drajveri, itd).

- `Auto-resolving dependency injection` — Masonite može da pogleda potpis (annotations, tipove 
   parametara) i da automatski ubaci odgovarajuće instance iz container-a, bez da ti ručno prolaziš ceo lanac zavisnosti.

### Kako konkretno radi IoC i DI u Masonite

Evo ključnih detalja:

**Registracija zavisnosti**:

U Service Provider-ima definišeš “bind” (ili “singleton”, zavisno od potrebe) u container. Na primer, možeš reći da se određena klasa veže za interfejs (ili alias) i da kada god neko traži taj interfejs/alias, Masonite da instancu klase koju si registrovao.

**Korišćenje tipova u konstruktoru ili metodu**:

U kontrolerima, middleware-u, drugim delovima code-a, kada definišeš konstruktor ili metodu, možeš navesti tipove parametara (tip anotacije) koji su klase/servisi vezani u container-u. Masonite će prepoznati taj tip, i ubaciti instancu.

Na primer, kontroler klasa može imati:

```py
class SomeController:
    def __init__(self, Request):
        self.request = Request
    
    def show(self, SomeService):
        return SomeService.do_something()
```

Tu `Request` i `SomeService` automatski dobijaju instance iz container-a jer Masonite zna da su registrovani.

**Service Providers & "bootstrapping"**:

Service Provider se koriste da Bootstrap-uju stvari pre nego što se aplikacija pokrene. Oni u svojim register i boot metodama vezuju klase i instance u container. Kad se request pokrene, svi binding-ovi su već spremni i container zna šta treba da ubaci kad neko zatraži.

**Resolvers**:

Masonite ima mehanizam koji „resolves” zavisnosti prema potrebama — kad vidi funkciju/konstruktor/method koja ima parametre sa tipom koji je registrovan u container, on automatski kreira zahtevanu instancu i ubacuje je. Ovo smanjuje boilerplate kod, jer ne moraš svaki put ručno da kreiraš instance i prosleđuješ ih.

Svi delovi aplikacije mogu da koriste injektovane zavisnosti.

To uključuje:

kontrolere (Controllers)
middleware
drajvere,
servise koje koristiš za mejl,
datoteke,
baze

### Primer kako bi to izgledalo

Da ilustrujem sa primerom (pseudokod) na osnovu Masonite-stila:

Pretpostavimo da želiš servis koji šalje mejlove, i kontroler koji ga koristi.

`app/providers/MailProvider.py`

```py
from masonite.providers import ServiceProvider
from app.services.MailService import MailService
from app.mailers.SMTPMailer import SMTPMailer

class MailProvider(ServiceProvider):
    def register(self):
        # vežemo SMTPMailer u container
        self.app.bind('mailer', SMTPMailer)
        # ili možemo da vežemo MailService koji koristi mailer
        self.app.bind(MailService, lambda: MailService(self.app.make('mailer')))

    def boot(self):
        pass

# app/services/MailService.py
class MailService:
    def __init__(self, mailer):
        self.mailer = mailer

    def send(self, to, subject, body):
        return self.mailer.send_email(to, subject, body)

# app/http/controllers/UserController.py
from masonite.controllers import Controller

class UserController(Controller):
    def show(self, MailService):
        # Masonite će automatski kreirati instancu MailService iz container-a
        MailService.send('user@example.com', 'Hello', 'Tekst mejla')
        return "Sent"
```

U ovom primeru:

`MailProvider` je taj koji registruje (`bind`) konkretne `mailer` klase i `servis` u container.

`UserController` u svojoj `show` metodi samo traži `MailService` kao parametar — Masonite prepoznaje tip i ubacuje instancu.

**Prednosti Masonite-eg pristupa**:

- Smanjuje boilerplate — ne moraš svuda uvezivati i praviti objekte, samo ih navedeš kao tipove, 
  framework ih ubacuje.

- Laka zamena implementacije — ako hoćeš da promeniš SMTPMailer u npr. SendGridMailer, menjaš u 
  provider-u, ostalo ostaje isto.

- Jedinstven način da se prošire funkcije (drajveri, servisi) preko provider-a, što održava 
  aplikaciju modularnom.

- Testiranje je lakše, jer možeš bindovati mock servise u testovima.

Ako krećeš novi projekat u Pythonu i hoćeš IoC / dependency injection da ti bude čist i održiv, ima par opcija koje su daleko modernije i održavanije od Spring Python-a.

Moje preporuke:

- FastAPI built-in Dependencies
  
  - Ako radiš web API → ovo je top opcija.
  - FastAPI ima ugrađen dependency injection sistem, vrlo lagan i moćan.
  - Samo dodaš parametre sa Depends(...), a framework rešava lanac zavisnosti.
  - Bonus: automatski se integriše sa validacijom i dokumentacijom (OpenAPI).
  - Koristi ako ti treba moderni REST/GraphQL API.

- Masonite Service Container

  - Ako želiš nešto što liči na Laravel/Spring stil, Masonite ima ozbiljan IoC container i service
    provider pattern.
  - Ima smisla ako ti prija ta arhitektura i želiš da pišeš u MVC stilu sa mnogo auto-resolving 
    zavisnosti.
  - Koristi ako ti je blizak Laravel način rada, ili želiš „full-stack“ Python framework sa IoC.

- dependency-injector (biblioteka)

  - General-purpose DI library za Python.
  - Možeš da je koristiš u bilo kom projektu (CLI, microservice, worker, web).
  - Ima IoC container, providers (Factory, Singleton, etc.), lako menjaš konfiguracije (npr. u testu 
    vs produkciji).
  - Aktivno održavana i dobro dokumentovana.
  - Koristi ako praviš ne-web aplikaciju (npr. CLI tool, mikroservise, machine learning pipeline), 
    ili hoćeš jasan i striktan IoC container.

- Ručni DI (Pythonic way)

  - U mnogim projektima dovoljno je da DI radiš ručno (prosleđivanjem kroz konstruktor).
  - Python je fleksibilan → framework ti često i ne treba osim ako projekat baš raste.
  - Možeš kasnije uvesti container ako stvari postanu kompleksne.
  - Koristi ako ti je projekat mali do srednji i ne želiš dodatne slojeve apstrakcije.

### Ukratko o korišćenju

- FastAPI ako praviš API (ima ugrađen DI).
- Masonite ako hoćeš Laravel-like framework sa IoC.
- dependency-injector ako hoćeš DI biblioteku nezavisnu od frameworka.
- Ručno DI ako radiš nešto manje i hoćeš jednostavnost.

Hajde da napravimo isti primer u tri varijante:

Imamo `UserService` koji zavisi od `UserRepository`. `UserRepository` koristi neku bazu (`Database`).
API endpoint `/users` vraća listu korisnika.

**FastAPI (ugrađeni DI)**:

```py
from fastapi import FastAPI, Depends

# --- Core classes ---
class Database:
    def query(self, sql: str):
        return [{"id": 1, "name": "Pera"}, {"id": 2, "name": "Mika"}]

class UserRepository:
    def __init__(self, db: Database):
        self.db = db

    def get_users(self):
        return self.db.query("SELECT * FROM users")

class UserService:
    def __init__(self, repo: UserRepository):
        self.repo = repo

    def list_users(self):
        return self.repo.get_users()

# --- Dependency definitions ---
def get_db():
    return Database()

def get_repo(db: Database = Depends(get_db)):
    return UserRepository(db)

def get_service(repo: UserRepository = Depends(get_repo)):
    return UserService(repo)

# --- FastAPI app ---
app = FastAPI()

@app.get("/users")
def list_users(service: UserService = Depends(get_service)):
    return service.list_users()
```

FastAPI sam rešava lanac zavisnosti Controller → Service → Repo → DB.

**Masonite (IoC container + auto-resolve)**:

`app/providers/UserProvider.py`

```py
from masonite.providers import ServiceProvider
from app.repositories import UserRepository
from app.services import UserService
from app.database import Database

class UserProvider(ServiceProvider):
    def register(self):
        self.app.bind("db", Database)
        self.app.bind(UserRepository, lambda: UserRepository(self.app.make("db")))
        self.app.bind(UserService, lambda: UserService(self.app.make(UserRepository)))

    def boot(self):
        pass


# app/controllers/UserController.py
from masonite.controllers import Controller
from app.services import UserService

class UserController(Controller):
    def show(self, service: UserService):
        # Masonite auto ubaci UserService iz container-a
        return service.list_users()
```

Ovde sve zavisnosti definišeš u provideru. Kad Masonite vidi UserService u parametru kontrolera, on ga ubaci automatski.

**dependency-injector (general IoC lib)**:

```py
from dependency_injector import containers, providers

# --- Core classes ---
class Database:
    def query(self, sql: str):
        return [{"id": 1, "name": "Pera"}, {"id": 2, "name": "Mika"}]

class UserRepository:
    def __init__(self, db: Database):
        self.db = db

    def get_users(self):
        return self.db.query("SELECT * FROM users")

class UserService:
    def __init__(self, repo: UserRepository):
        self.repo = repo

    def list_users(self):
        return self.repo.get_users()

# --- IoC Container ---
class Container(containers.DeclarativeContainer):
    db = providers.Singleton(Database)
    repo = providers.Factory(UserRepository, db=db)
    service = providers.Factory(UserService, repo=repo)

# --- Korišćenje ---
container = Container()
service = container.service()
print(service.list_users())
```

Ovde imaš čisti IoC container. Možeš ga koristiti bilo gde, ne mora da bude web app.

### Poređenje

 Rešenje             | Kada koristiti
---------------------|-------------------------------------------------------------------------------
 FastAPI DI          | Ako praviš API i hoćeš DI odmah integrisan sa web framework-om.
 Masonite IoC        | Ako ti prija Laravel/Spring stil sa providerima i centralnim containerom.
 dependency-injector | Ako hoćeš framework-agnostic rešenje, npr. CLI, mikroservisi, ML projekti.
