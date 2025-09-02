
# Migriranje na prilagođeni korisnički model u sredini projekta

Ovaj članak gleda kako da se prebaci na prilagođeni korisnički model u sredini
projekta u Django-u.

Django-ov podrazumevani korisnički model dolazi sa relativno malim brojem polja.
Ova polja nisu dovoljna za sve slučajeve korišćenja, ima puno prebacivanja Django
projekta na prilagođene korisničke modele.

Prelazak na prilagođeni korisnički model je lak pre nego što migrirate bazu podataka,
ali postaje značajno teže nakon toga jer utiče na `foreign keys`, `više-na-više`
relacoje, poglede, i migracije, da imenujemo nekoliko.

Da izbegnete prolazak kroz ovaj glomazan proces migracije, zvanična Django-a
dokumentacija veoma preporučuje da postavite prilagođeni korisnički model na
početku projekta čak i ako je podrazumevani dovoljan.

Do danas, još uvek nema zvaničnog načina prelaska na prilagođeni korisnički model
u sredini projekta. Zajednica Django i dalje raspravlja o tome koji je najbolji
način da se selidbe na prilagodjeni korisnički model?

U ovom članku ćemo pogledati relativno jednostavan pristup migriranju na prilagodjeni
korisnički model u sredini projekta.Proces migracije koji ćemo koristiti nije tako
destruktivan.  

## Sadržaj

- Prilagođeni korisnički model
- Dummy project
- Proces migracije

Migiranje na prilagođeni korisnički model u sredini projekta je potencijalno
destruktivna akcija. Zbog toga, pripremio sam dummy projekat koji možete da
koristite za testiranje.

Migrirajte Django admina posle prelaska na vaš prilagodjeni model.

## Preimenujte korisnički model/tabelu

Dummy projekat koji ćemo raditi naziva se `django-custom-user`.To je
jednostavna `todo` aplikacija koja koristi korisnički model.

Klonirajte ga:

```sh
git clone --single-branch --branch base git@github.com:duplxey/django-custom-user.git
cd django-custom-user
```

Kreirajte novo virtuelno okruženje i aktivirajte ga:

```sh
python3 -m venv venv && source venv/bin/activate
```

Instalirajte zahteve:

```sh
(venv)$ pip install -r requirements.txt
```

Pokrenite postgres kontejner za docker:

```sh
docker run --name django-todo-postgres -p 5432:5432 \
    -e POSTGRES_USER=django-todo -e POSTGRES_PASSWORD=complexpassword123 \
    -e POSTGRES_DB=django-todo -d postgres
```

Alternativno, možete da instalirate i pokrenete PostgreSQL izvan Docker-a ako je
to za vas prednost. Samo budite sigurni da idite na core/settings.py i
promenite je u skladu sa tim.

Migrirajte bazu podataka:

```sh
(venv)$ python manage.py migrate
```

Učitajte fixtures:

```sh
(venv)$ python manage.py loaddata fixtures/auth.json --app auth
(venv)$ python manage.py loaddata fixtures/todo.json --app todo
```

Ove dve fixtures dodala su nekoliko korisnika, grupa i zadataka u bazu podataka i
stvorila superusera sa sledećim akreditivima:

```sh
username: admin
password: password
```

Dalje, pokrenite server:

```sh
(venv)$ python manage.py runserver
```

I na kraju, idite do Admin panela na <http://localhost:8000/admin>, Prijavite se
kao superuser i proverite da li su podaci uspešno učitani.

## Proces migracije

Proces migracije koji ćemo koristiti pretpostavlja da:

1. Vaš projekat još nema prilagođeni korisnički model.
2. Već ste stvorili svoju bazu podataka i migrirali ga.
3. Ne postoje migracije u toku i sve postojeće migracije su primenjene.
4. Ne želite da izgubite nikakve podatke.

Ako ste još uvek u fazi razvoja i podaci u vašoj bazi podataka nisu važni, ne morate
da sledite ove korake, da se ​​prebacite u prilagođeni korisnik model. Jednostavno
možete obrisati bazu podataka, obrišite sve datoteke migracije i zatim sledite
korake ovde.

Pre nego što sledite, u potpunosti napravite rezervnu kopiju baze podataka (i koda).
Takođe bi trebalo da isprobate korake na filijali/okruženju za inscenciju pre nego
što pređete na proizvodno okruženje.

### Koraci migracije

1. Postavite `auth_user_model` na default Django korisnika u `settings.py`.
2. Zamenite sve korisničke reference sa `auth_user_model` ili `get_user_model()` u skladu s tim.
3. Pokrenite novu aplikaciju Django i registrujte je u `settings.py`.
4. Stvorite praznu migraciju u novostvorenoj aplikaciji.
5. Migrirajte bazu podataka, tako da se primenjuje ovu praznu migraciju.
6. Izbrišite praznu datoteku migracije.
7. Kreirajte prilagođeni korisnički model u novokreiranoj aplikaciji.
8. Postavite `django_user_model` na prilagođeni.
9. Pkrenite migracije.

Hajde da počnemo!

#### Korak 1

Da se ​​prebacimo na prilagođeni korisnički model, prvo se moramo osloboditi svih direktnih korisničkih referenci.Da biste to učinili, počnite dodavanjem novog podešavanja po imenu `auth_user_model` u `settings.py` kao:

`core/settings.py`

```py
AUTH_USER_MODEL = 'auth.User'
```

Ovo podešavanje govori Django sa kojim modelom korisnika radi. Pošto još nemamo prilagodjeni korisnički model, ukazajemo na podrazumevani korisnički model.

#### Korak 2

Dalje, prođite kroz celu svoju kodnu bazu i obavezno zamenite sve reference sa `auth_user_model` ili `get_user_model()` u skladu sa tim:

`todo/models.py`

```py
class UserTask(GenericTask):
  user = models.ForeignKey(
    to=AUTH_USER_MODEL,
    on_delete=models.CASCADE
  )

  def __str__(self):
    return f'UserTask {self.id}'

class GroupTask(GenericTask):
  users = models.ManyToManyField(
    to=AUTH_USER_MODEL
  )
  
  def __str__(self):
    return f'GroupTask {self.id}'
```

Ne zaboravite da uvezete AUTH_USER_MODEL na vrhu datoteke:

```py
from core.settings import AUTH_USER_MODEL
```

Takođe proverite da li su sve aplikacije/paketi treće strane koje koristite koriste isto.
Ako bilo koji od njih referencira direktno korisnički model, stvari bi se mogle slomiti.
Ne moram da se brinemo o tome jer od većine popularnih paketa koji koriste korisnički
model ne referencirajte ga direktno.

#### Korak 3

Pokretanje, moramo da pokrenemo novu aplikaciju Django, koja će ugostiti prilagođeni
korisnički model. Imenovaću je `Users`, ali možete odabrati drugačije ime:

```sh
(venv) $ python manage.py startapp users
```

Ako želite, možete ponovo koristiti već postojeću aplikaciju, ali morate da budete
sigurni da još nema migracija u toj aplikaciji; Inače, proces migracija neće raditi
zbog ograničenja Django-a.

Registrujte aplikaciju u `settings.py`:

`core/settings.py`

```py
INSTALLED_APPS = [
  'django.contrib.admin',
  'django.contrib.auth',
  'django.contrib.contenttypes',
  'django.contrib.sessions',
  'django.contrib.messages',
  'django.contrib.staticfiles',
  'todo.apps.TodoConfig',
  'users.apps.UsersConfig', # new
]
```

#### Korak 4

Zatim moramo da prevarimo Django da misli da je korisnička aplikacija zadužena za
auth_user tabrlu. To se obično može učiniti sa komandom migracije i lažnom zastavom,
ali ne u ovom slučaju, jer ćemo naići na nedoslednost migracije.
Pošto većina migracija zavisi od migracija AUTH. U svakom slučaju, zaobići to,
možemo da koristimo hakiranje rešetka.

Prvo, stvorićemo praznu migraciju, primenite je tako da se sačuva u `django_migrations`
tabeli, a zatim zamenite je stvarnim `auth_user` migracijama.

Stvorite praznu migraciju u aplikaciji users:

```sh
(venv)$ python manage.py makemigrations --empty users

Migrations for 'users':
  users\migrations\0001_initial.py

This should create an empty migration named users/migrations/0001_initial.py.
```

Korak 5
Migrirajte bazu podataka tako da se prazna migracija dodaje u django_migracije
tabelu:

```sh
(venv)$ python manage.py migrate

Operations to perform:
  Apply all migrations: admin, auth, contenttypes, sessions, todo, users
Running migrations:
  Applying users.0001_initial... OK
```

Korak 6
Sada izbrišite praznu datoteku migracije:

```sh
(venv)$ rm users/migrations/0001_initial.py
```

#### Korak 7

Idite na `users/models.py` i definišite prilagođeni korisnički model kao:

`users/models.py`

```py
from django.contrib.auth.models import AbstractUser

class User(AbstractUser):
  
  class Meta:
  db_table = 'auth_user'
```

Još ne dodajte nijedno prilagođeno polje.Ovaj model mora biti direktna replika
Django-ov podrazumevanog korisničkog model jer ćemo ga koristiti za kreiranje
početne `auth_user` tabele.

Takođe, obavezno imenovati korisnika, u suprotnom ćete možda naići na probleme jer
tipovi sadržaja.

Kasnije ćete moći da promenite ime modela.

Korak 8

Dođite do svojih settings.py i `auth_user_model` na tek stvoreni prilagodjeni
korisnički model:

`core/settings.py`

```py
AUTH_USER_MODEL = 'users.User'
```

Ako se vaša aplikacija ne zove `users` promenite kod shodno tome.

Korak 9

Pokrenite `makemigrations` da generišete inicijalnu `auth_user`  migraciju:

```sh
(venv)$ python manage.py makemigrations

Migrations for 'users':
  users\migrations\0001_initial.py
    - Create model User
```

I to je to! Generisane migracije su već primenjene kada ste prvi put pokretali
Django's Auth App, tako da ponovo pokretanje `migrate` neće ništa učiniti.

### Dodajte nova polja

Jednom kada dobijete prilagođeni korisnički model, lako je dodavanje novih polja.

Da dodate telefon i adresu, na primer, dodajte sledeće na prilagodjeni korisnički
model:

`users/models.py`

```py
class User(AbstractUser):
phone = models.CharField(max_length=32, blank=True, null=True) # new
address = models.CharField(max_length=64, blank=True, null=True) # new

class Meta:
db_table = 'auth_user'
```

Don't forget to import models  at the top of the file:

```py
from django.db import models
```

Next, make migrations and migrate:

```sh
(venv)$ python manage.py makemigrations
(venv)$ python manage.py migrate
```

To make sure the fields have been reflected in the database bash into the Docker
container:

```sh
docker exec -it django-todo-postgres bash
```

Connect to the database via psql :

```sh
root@967e9158a787:/# psql -U django-todo

psql (14.5 (Debian 14.5-1.pgdg110+1))
Type "help" for help.
```

And inspect the auth_user  table:

```sh
django-todo=# \d+ auth_user

                    Table "public.auth_user"
    Column    |           Type           | Collation | Nullable | Default                         | Storage  | Compression | Stats target | Description
--------------+--------------------------+-----------+----------+---------------------------------+----------+-------------+--------------+------------
 id           | integer                  |           | not null | generated by default as identity| plain    |             |              |
 password     | character varying(128)   |           | not null |                                 | extended |             |              |
 last_login   | timestamp with time zone |           |          |                                 | plain    |             |              |
 is_superuser | boolean                  |           | not null |                                 | plain    |             |              |
 username     | character varying(150)   |           | not null |                                 | extended |             |              |
 first_name   | character varying(150)   |           | not null |                                 | extended |             |              |
 last_name    | character varying(150)   |           | not null |                                 | extended |             |              |
 email        | character varying(254)   |           | not null |                                 | extended |             |              |
 is_staff     | boolean                  |           | not null |                                 | plain    |             |              |
 is_active    | boolean                  |           | not null |                                 | plain    |             |              |
 date_joined  | timestamp with time zone |           | not null |                                 | plain    |             |              |
 phone        | character varying(32)    |           |          |                                 | extended |             |              |
 address      | character varying(64)    |           |          |                                 | extended |             |              |
```

Možete videti da su dodana nova polja nazvana telefon i adresa.

### Django Admin

Da biste prikazali prilagođeni korisnički model u admin Django, prvo morate
stvorite novu klasu koja nasleđuje user ugradjenu i zatim je registrujte. Zatim
uključuje telefon i adresa u polju.

Finalno admin.py treba da izgleda ovako:

`users/admin.py`

```py
from django.contrib import admin
from django.contrib.auth.admin import UserAdmin
from users.models import User

class CustomUserAdmin(UserAdmin):
  fieldsets = UserAdmin.fieldsets + (
    ('Additional info', {'fields': ('phone', 'address')}),
  )

admin.site.register(User, CustomUserAdmin)
```

Ponovo pokrenite server, prijavite se kao običan korisnik.Pomerite se prema dole
na dno i trebalo bi da vidite novi odeljak sa novim poljima.

Ako želite da prilagodite admin Django još više, pogledajte Django Admin sajt sa
zvaničnim dokumenatima.

### Preimenovanje korisničke tabele/modela

U ovom trenutku, možete preimenovati korisnički model i tabelu kao i obično.

Da biste preimenovali korisnički model, jednostavno promenite ime klase.
Promenite svojstvo `db_table`:

`users/models.py`

```py
class User(AbstractUser): # <-- you can change me
  phone = models.CharField(max_length=32, blank=True, null=True)
  address = models.CharField(max_length=64, blank=True, null=True)

  class Meta:
    db_table = 'auth_user' # <-- you can change me
```

Ako uklonite `db_table`, ime tabele će se vratiti nazad na <app_name>_<model_name>.

Nakon što završite sa svojim promenama, pokrenite:

```sh
(venv)$ python manage.py makemigrations
(venv)$ python manage.py migrate
```

Generalno ne bih preporučio da bilo šta preimenujete, jer će struktura vaše baze
podataka postati nedosledna. Neke od tabela će imati prefix users_, dok će neke
od njih imati prefiks auth_. Ali sa druge strane, mogli biste tvrditi da je korisnički
model sada deo aplikacije korisnika, tako da to ne bi trebalo imati auth_prefix.

U slučaju da odlučite da preimenujete tabelu, konačna struktura baze podataka
će izgledati slično kao:

```sh
django-todo=# \dt
                  List of relations
------------------------------------------------------------
 Schema |            Name             | Type  |    Owner
--------|-----------------------------|-------|-------------
 public | auth_group                  | table | django-todo
 public | auth_group_permissions      | table | django-todo
 public | auth_permission             | table | django-todo
 public | django_admin_log            | table | django-todo
 public | django_content_type         | table | django-todo
 public | django_migrations           | table | django-todo
 public | django_session              | table | django-todo
 public | todo_task                   | table | django-todo
 public | todo_task_categories        | table | django-todo
 public | todo_taskcategory           | table | django-todo
 public | users_user                  | table | django-todo
 public | users_user_groups           | table | django-todo
 public | users_user_user_permissions | table | django-todo
```

## Zaključak

Iako je ovaj problem prelaska na prilagođeni korisnički model u sredini projekta bio sve vreme još uvek nema zvaničnog rešenja.

Nažalost, puno Django programera mora da prođe kroz ovaj postupak migracije,
jer dokumentacija Django ne naglašava dovoljno da bi trebala kreirate prilagođeni korisnički model na početku projekta. Možda su to mogli da uključe u vodič?

Nadamo se da proces migracije predstavljen u članku radi za vas. U slučaju da nešto nije uspelo za vas ili mislite da bi nešto moglo biti bolje, voleo bih da čujem vaše povratne informacije.
