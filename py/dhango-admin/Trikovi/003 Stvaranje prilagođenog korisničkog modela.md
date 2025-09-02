
# Stvaranje prilagođenog korisničkog modela u Django-u

Kako mogu u potpunosti zameniti polje `username` sa poljem `email` za Django autentifikaciju?

Ovaj članak objašnjava korak po korak kako da kreirate prilagođeni korisnički model u Django-u tako da se adresa e-pošte može koristiti kao primarni identifikator korisnika umesto korisničkog imena za autentifikaciju.

Imajte na umu da je proces koji je navedeni u ovom članku zahtevao značajne
promene u šemi baze podataka. Zbog toga se preporučuje samo za novi projekt. Ako radite na postojećim projektom, moraćete da sledite drugačiji skup koraka. Za više o tome pregledajte migraciju na prilagođeni korisnički model u MID-projektu,

## Ciljevi

Do kraja ovog članka trebali biste biti u mogućnosti da:

1. Opišete razliku između `AbstractUser` i `AbstractBaseUser`
2. Objasnite zašto biste trebali postaviti prilagođeni korisnički model
   prilikom pokretanja novog projekta Django.
3. Pokrenete novi projekat Django sa prilagođenim korisničkim modelom
4. Koristite adresu e-pošte kao primarni identifikator korisnika umesto
   korisničkog imena za autentifikaciju
5. Radite probno testiranje tokom primene prilagođenog modela korisnika

## AbstractUser vs AbstractBaseUser

Podrazumevani korisnički model u Django-u koristi korisničko ime kao jedinstveno identifikovanje korisnika tokom autentifikacije. Ako biste radije koristili adresu e-pošte, moraćete da kreirate prilagođeni korisnički model nasledjivanjem `AbstractUser`-a ili `AbstractBaseUser`-a.

Opcije:

1. `AbstractUser`: Koristite ovu opciju ako ste zadovoljni postojećim
   poljima na korisničkom modelu i samo želite da uklonite polje za korisničko ime.
2. `AbstractBaseUser`: Koristite ovu opciju ako želite da počnete od nule
   kreiranje sopstvenog, potpuno novog korisničkog modela.

Pogledaćemo obe opcije, u ovome članaku.

Koraci su isti za svaki:

1. Napravite prilagođeni korisnički model i menadžer
2. Ažurirajte `settings.py`
3. Prilagodite korisničku forme i šablone za korisnike
4. Ažurirajte admin

Toplo se preporučuje postavljanje prilagođenog korisničkog modela prilikom pokretanja novog Django projekta. Bez toga, moraćete da kreirate drugi model (kao `UserProfile`) i povežete ga na Đango korisnički model sa `onetoonefield` ako Želite da dodate nova polja u korisnički model.

## Podešavanje projekta

Započnite stvaranjem novog projekta Django zajedno sa aplikacijom `User`:

```sh
$ mkdir django-custom-user-model && cd django-custom-user-model
$ python3 -m venv env
$ source env/bin/activate

(env)$ pip install Django==4.1.5
(env)$ django-admin startproject hello_django .
(env)$ python manage.py startapp users
```

Slobodno zamenite "virtealenv" i "pip" za "poetry" ili "pipenv". Za više,
"Pregledajte moderna python okruženja".

> [!Note]
>
> Ne primenjujte migracije.
>
> **Zapamtite**: Morate da kreirate prilagođeni korisnički model pre nego
  što primenite svoju prvu migraciju.

Dodajte novu aplikaciju na `INSTALLED_APPS` listu u `settings.py`:

```py
INSTALLED_APPS = [
  "django.contrib.admin",
  "django.contrib.auth",
  "django.contrib.contenttypes",
  "django.contrib.sessions",
  "django.contrib.messages",
  "django.contrib.staticfiles",
  "users", # new
]
```

## Testiranje

```py
from django.contrib.auth import get_user_model
from django.test import TestCase

class UsersManagersTests(TestCase):
  
  def test_create_user(self):
    User = get_user_model()
    user = User.objects.create_user(email="normal@user.com",
      password="foo")

    self.assertEqual(user.email, "normal@user.com")
    self.assertTrue(user.is_active)
    self.assertFalse(user.is_staff)
    self.assertFalse(user.is_superuser)

    try:
      # username is None for the AbstractUser option
      # username does not exist for the AbstractBaseUser option
      self.assertIsNone(user.username)
    except AttributeError:
      pass

    with self.assertRaises(TypeError):
      User.objects.create_user()
    
    with self.assertRaises(TypeError):
      User.objects.create_user(email="")
    
    with self.assertRaises(ValueError):
      User.objects.create_user(email="", password="foo")

  def test_create_superuser(self):
    User = get_user_model()
    admin_user = User.objects.create_superuser(email="super@user.com", 
      password="foo")

    self.assertEqual(admin_user.email, "super@user.com")
    self.assertTrue(admin_user.is_active)
    self.assertTrue(admin_user.is_staff)
    self.assertTrue(admin_user.is_superuser)
    
    try:
      # username is None for the AbstractUser option
      # username does not exist for the AbstractBaseUser option
      self.assertIsNone(admin_user.username)
    except AttributeError:
      pass

    with self.assertRaises(ValueError):
      User.objects.create_superuser(
        email="super@user.com", 
        password="foo", 
        is_superuser=False
      )
```

Dodajte specifikacije na `users/tests.py`, a onda se uverite da li testovi uspevaju.

## Model Manager

Prvo, moramo dodati prilagođeni menadžera, nasledjivanjem `BaseUserManager`, koji koristi e-poštu kao jedinstveni identifikator umesto korisničkog imena.

Kreirajte datoteku `menager.py` u direktorijumu "Users":

```py
from django.contrib.auth.base_user import BaseUserManager
from django.utils.translation import gettext_lazy as _

class CustomUserManager(BaseUserManager):
  """
  Custom user model manager where email is the unique identifiers
  for authentication instead of usernames.
  """

  def create_user(self, email, password, **extra_fields):
    """
    Create and save a user with the given email and password.
    """

    if not email:
      raise ValueError(_("The Email must be set"))
    
    email = self.normalize_email(email)
    user = self.model(email=email, **extra_fields)
    user.set_password(password)
    user.save()
    return user

  def create_superuser(self, email, password, **extra_fields):
    """
    Create and save a SuperUser with the given email and password.
    """
    extra_fields.setdefault("is_staff", True)
    extra_fields.setdefault("is_superuser", True)
    extra_fields.setdefault("is_active", True)
    
    if extra_fields.get("is_staff") is not True:
      raise ValueError(_("Superuser must have is_staff=True."))

    if extra_fields.get("is_superuser") is not True:
      raise ValueError(_("Superuser must have is_superuser=True."))

    return self.create_user(email, password, **extra_fields)
```

## User Model

Odlučite koju opciju želite da koristite: nasledjivanje `AbstractUser` or
`AbstractBaseUser`.

### AbstractUser

Ažurirajmo `users/models.py`:

```py
from django.contrib.auth.models import AbstractUser
from django.db import models
from django.utils.translation import gettext_lazy as _
from .managers import CustomUserManager

class CustomUser(AbstractUser):
  username = None
  email = models.EmailField(_("email address"), unique=True)
  USERNAME_FIELD = "email"
  REQUIRED_FIELDS = []
  objects = CustomUserManager()

  def __str__(self):
    return self.email
```

Ovde,:

1. Kreiramo novu klasu "CustomUser" nasledjivanjem `AbstractUser`
2. Ukklanjamo "username" polje
3. Napravimo "email" polje zahtevanim i jedinstvenim
4. Postavljamo `USERNAME_FIELD` za `User` model na `email`
5. Specificiramo da svi objekti klase dolaze iz "CustomUserManager".

### AbstractBaseUser

Ažurirajmo `users/models.py`:

```py
from django.contrib.auth.models import AbstractBaseUser, PermissionsMixin
from django.db import models
from django.utils import timezone
from django.utils.translation import gettext_lazy as _
from .managers import CustomUserManager

class CustomUser(AbstractBaseUser, PermissionsMixin):
  email = models.EmailField(_("email address"), unique=True)
  is_staff = models.BooleanField(default=False)
  is_active = models.BooleanField(default=True)
  date_joined = models.DateTimeField(default=timezone.now)
  USERNAME_FIELD = "email"
  REQUIRED_FIELDS = []
  objects = CustomUserManager()

  def __str__(self):
    return self.email
```

Ovde,:

1. Kreiramo novu klasu "CustomUser" nasledjivanjem `AbstractBaseUser`
2. Dodajemo polja `email`, `is_staff`, `is_active`, i `date_joined`
3. Napravimo "email" polje zahtevanim i jedinstvenim
4. Postavljamo `USERNAME_FIELD` za `User` model na `email`
5. Precizirano da svi obnjekti za klasu dolaze od "CustomUserManager"

## Settings

Dodajte sledeću liniju na datoteku `settings.py` tako da Django zna da koristi novu prilagođenu klasu korisnika:

```py
`AUTH_USER_MODEL` = "users.CustomUser"
```

Sada možete da kreirate i primenite migracije, koje će stvoriti novu bazu podataka koja koristi prilagođeni korisnički model. Pre nego što to učinimo, pogledajmo kako će migracije zapravo izgledati bez stvaranja datoteke migracije, sa `--dry-run` flag:

```sh
(env)$ python manage.py makemigrations --dry-run --verbosity 3
```

Trebali biste videti nešto slično:

```sh
# Generated by Django 4.1.5 on 2023-01-21 20:36

from django.db import migrations, models
import django.utils.timezone

class Migration(migrations.Migration):
  initial = True
  dependencies = [
    ('auth', '0012_alter_user_first_name_max_length'),
  ]

  operations = [
    migrations.CreateModel(
      name='CustomUser',
      fields = [
        ('id', models.BigAutoField(auto_created=True, primary_key=True,serialize=False, verbose_name='ID')),
        ('password', models.CharField(max_length=128,verbose_name='password')),
        ('last_login', models.DateTimeField(blank=True, null=True,verbose_name='last login')),
        ('is_superuser', models.BooleanField(default=False,help_text='Designates that this user has all permissions without explicitly assigning them.', verbose_name='superuser status')),
        ('first_name', models.CharField(blank=True, max_length=150,verbose_name='first name')),
        ('last_name', models.CharField(blank=True, max_length=150,verbose_name='last name')),
        ('is_staff', models.BooleanField(default=False,help_text='Designates whether the user can log into this admin site.',verbose_name='staff status')),
        ('is_active', models.BooleanField(default=True,help_text='Designates whether this user should be treated as  active. Unselect this instead of deleting accounts.', verbose_name='active')),
        ('date_joined', models.DateTimeField(default=django.utils.timezone.now, verbose_name='date joined')),
        ('email', models.EmailField(max_length=254, unique=True,verbose_name='email address')),
        ('groups', models.ManyToManyField(blank=True,help_text='The groups this user belongs to. A user will get all permissions granted to each of their groups.', related_name='user_set', related_query_name='user', to='auth.group', verbose_name='groups')),
        ('user_permissions', models.ManyToManyField(blank=True,help_text='Specific permissions for this user.', related_name='user_set',related_query_name='user', to='auth.permission', verbose_name='user permissions')),
      ],
      options={
        'verbose_name': 'user',
        'verbose_name_plural': 'users',
        'abstract': False,
      },
    ),
  ]
```

Ako ste išli ​​sa rutom `AbstractBaseUser`, nećete imati polja za `first_name` ili `last_name`. Zašto?

Proverite da li migracija ne uključuje polje `username`. Zatim, kreirajte i primenite migraciju:

```sh
(env)$ python manage.py makemigrations
(env)$ python manage.py migrate
```

## View the schema

```sh
$ sqlite3 db.sqlite3
SQLite version 3.28.0 2019-04-15 14:49:49
Enter ".help" for usage hints.

sqlite> .tables
  auth_group                         django_migrations
  auth_group_permissions             django_session
  auth_permission                    users_customuser
  django_admin_log                   users_customuser_groups
  django_content_type                users_customuser_user_permissions

sqlite> .schema users_customuser
CREATE TABLE IF NOT EXISTS "users_customuser" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "password" varchar(128) NOT NULL,
  "last_login" datetime NULL,
  "is_superuser" bool NOT NULL,
  "first_name" varchar(150) NOT NULL,
  "last_name" varchar(150) NOT NULL,
  "is_staff" bool NOT NULL,
  "is_active" bool NOT NULL,
  "date_joined" datetime NOT NULL,
  "email" varchar(254) NOT NULL UNIQUE
);
```

Ako ste išli ​​preko `AbstractBaseUser`, zašto je `last_logina` deo
modela?

Sada možete da referencate korisnički model sa `auth_user_model` ili `get_user_model()`. Pogledajte "Referencu korisničkog modela" zvaničnih dokumenata za više informacija.

Takođe, kada kreirate `superuser`-a, od vas će se trebati zatražiti da unesete e-poštu a ne korisničko ime:

```sh
(env)$ python manage.py createsuperuser
Email address: test@test.com
Password:
Password (again):
Superuser created successfully.
Make sure the tests pass:

(env)$ python manage.py test
Creating test database for alias 'default'...
System check identified no issues (0 silenced).
...
---
Ran 2 tests in 0.282s
OK

Destroying test database for alias 'default'...
```

## Forme

Dalje, hajde da nasledimo `UserCreationForm` i `UserChangeForm`  forme tako da one koriste novi `CustomUser`  model.

Kreirajmo novu datoteku u "Users" direktorijumu pod nazivom `forms.py`:

```py
from django.contrib.auth.forms import UserCreationForm, UserChangeForm
from .models import CustomUser

class CustomUserCreationForm(UserCreationForm):

  class Meta:
    model = CustomUser
    fields = ("email",)

class CustomUserChangeForm(UserChangeForm):
  
  class Meta:
    model = CustomUser
    fields = ("email",)
```

## Admin

Recimo adminu da koristi ove forme naseldjivanjem `UserAdmin`  u `users/admin.py`:

```py
from django.contrib import admin
from django.contrib.auth.admin import UserAdmin
from .forms import CustomUserCreationForm, CustomUserChangeForm
from .models import CustomUser

class CustomUserAdmin(UserAdmin):
  add_form = CustomUserCreationForm
  form = CustomUserChangeForm
  model = CustomUser
  list_display = ("email", "is_staff", "is_active",)
  list_filter = ("email", "is_staff", "is_active",)
  fieldsets = (
    (None, {"fields": ("email", "password")}),
    ("Permissions", {"fields": ("is_staff", "is_active", "groups",
      "user_permissions")}),
  )
  add_fieldsets = (
    (None, {
      "classes": ("wide",),
      "fields": (
        "email", "password1", "password2", "is_staff",
        "is_active", "groups", "user_permissions"
      )}
    ),
  )

  search_fields = ("email",)
  ordering = ("email",)

admin.site.register(CustomUser, CustomUserAdmin)
```

To je to. Pokrenite server i prijavite se na veb lokaciju Admin. Trebali biste moći da dodate i promenite korisnike kao i normalno.

## Zaključak

U ovom članku smo pogledali kako da kreiramo prilagođeni korisnički model tako da se adresa e-pošte može koristiti kao primarni identifikator korisnika umesto korisničkog imena za autentifikaciju.

Konačni kod možete pronaći za obe opcije, AbstractUser i AbstractBaseUser, u `django-custom-user-model` github repou. Konačni primeri koda uključuju šablone, preglede i URL adrese za autentifikaciju korisnika.

Želite da saznate više o prilagođavanju korisničkog modela Django?Pogledajte sledeće resurse:

1. Prilagođavanje korisničkog modela Django.
2. Kako produžiti Django korisnički model?
3. Dobijanje maksimalno iz Django-ovog korisničkog modela (video).
4. Prilagođavanje autentifikacije u Django-u.
