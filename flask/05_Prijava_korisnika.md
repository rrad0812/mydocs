
# Poglavlje 5 - Prijava korisnika

U 3. poglavlju ste naučili kako da kreirate obrazac za prijavu korisnika, a u 4. poglavlju ste naučili kako da radite sa bazom podataka. Ovo poglavlje će vas naučiti kako da kombinujete teme iz ta dva poglavlja da biste kreirali jednostavan sistem za prijavu korisnika.

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.5), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.5.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.4...v0.5).

## Heširanje lozinki

U 4. poglavlju, modelu korisnika je dodeljeno `password_hash` polje koje je do sada bilo nekorišćeno. Svrha ovog polja je da čuva heš korisničke lozinke, koji će se koristiti za verifikaciju lozinke koju je korisnik uneo tokom procesa prijavljivanja. Heširanje lozinki je komplikovana tema koju bi trebalo prepustiti stručnjacima za bezbednost, ali postoji nekoliko jednostavnih biblioteka koje implementiraju svu tu logiku na način koji je jednostavan za pozivanje iz aplikacije.

Jedan od paketa koji implementira heširanje lozinki je `Werkzeug`, na koji ste možda videli referencu u izlazu `pip`-a kada ste instalirali Flask, jer je to jedna od njegovih osnovnih zavisnosti. Pošto je to zavisnost, Werkzeug je već instaliran u vašem virtuelnom okruženju. Sledeća sesija Python shell-a pokazuje kako heširati lozinku pomoću ovog paketa:

```py
>>> from werkzeug.security import generate_password_hash
>>> hash = generate_password_hash('foobar')
>>> hash
'scrypt:32768:8:1$DdbIPADqKg2nniws$4ab051ebb6767a...'
```

U ovom primeru, lozinka foobar se transformiše u dugačak kodirani string kroz niz kriptografskih operacija koje nemaju poznatu inverznu operaciju, što znači da osoba koja dobije heširanu lozinku neće moći da je koristi za oporavak originalne lozinke. Kao dodatna mera, ako heširate istu lozinku više puta, dobićete različite rezultate, jer sve heširane lozinke dobijaju različitu kriptografsku so, tako da je nemoguće identifikovati da li dva korisnika imaju istu lozinku gledajući njihove hešove.

Proces verifikacije se vrši pomoću druge funkcije iz `Werkzeug`-a, na sledeći način:

```py
>>> from werkzeug.security import check_password_hash
>>> check_password_hash(hash, 'foobar')
True
>>> check_password_hash(hash, 'barfoo')
False
```

Funkcija verifikacije uzima heš lozinke koji je prethodno generisan i lozinku koju je korisnik uneo prilikom prijavljivanja. Funkcija vraća rezultat `True` ako se lozinka koju je korisnik uneo podudara sa hešem ili `False` ako ne.

Čitava logika heširanja lozinki može se implementirati kao dve nove metode u korisničkom modelu:

> `app/models.py` : Heširanje i verifikacija lozinki

```py
from werkzeug.security import generate_password_hash, check_password_hash
#...

class User(db.Model):
    #...
    def set_password(self, password):
        self.password_hash = generate_password_hash(password)

    def check_password(self, password):
        return check_password_hash(self.password_hash, password)
```

Sa ove dve metode, korisnički objekat sada može da izvrši bezbednu verifikaciju lozinke, bez potrebe da ikada čuva originalne lozinke. Evo primera korišćenja ovih novih metoda:

```py
>>> u = User(username='susan', email='susan@example.com')
>>> u.set_password('mypassword')
>>> u.check_password('anotherpassword')
False
>>> u.check_password('mypassword')
True
```

## Uvod u Flask-Login

U ovom poglavlju ću vam predstaviti veoma popularno Flask proširenje pod nazivom `Flask-Login`. Ovo proširenje upravlja stanjem prijavljenog korisnika, tako da se, na primer, korisnici mogu prijaviti u aplikaciju, a zatim navigirati na različite stranice dok aplikacija "pamti" da je korisnik prijavljen. Takođe pruža funkcionalnost "Zapamti me" koja omogućava korisnicima da ostanu prijavljeni čak i nakon zatvaranja prozora pregledača. Da biste bili spremni za ovo poglavlje, možete početi instaliranjem `Flask-Login`-a u vašem virtuelnom okruženju:

```sh
(venv) $ pip install flask-login
```

Kao i kod drugih ekstenzija, `Flask-Login` treba kreirati i inicijalizovati odmah nakon instance aplikacije u `app/__init__.py`. Ovako se inicijalizuje ova ekstenzija:

> `app/__init__.py` : Inicijalizacija `Flask-login`-a

```py
#...
from flask_login import LoginManager

app = Flask(__name__)
#...
login = LoginManager(app)
#...
```

## Priprema korisničkog modela za Flask-login

Ekstenzija `Flask-Login` radi sa korisničkim modelom aplikacije i očekuje da određena svojstva i metode budu implementirane u njoj. Ovaj pristup je dobar, jer sve dok se ove potrebne stavke dodaju modelu, Flask-Login nema nikakve druge zahteve, pa na primer, može da radi sa korisničkim modelima koji su zasnovani na bilo kom sistemu baza podataka.

Četiri potrebne stavke su navedene u nastavku:

- `is_authenticated` : svojstvo je `True` ako korisnik ima važeće akreditive ili `False` ako ne.
- `is_active` : svojstvo je `True` ako je korisnički nalog aktivan ili `False` ne.
- `is_anonymous` : svojstvo je `False` za redovne korisnike i `True` ostalim.
- `get_id()` : metoda koja vraća jedinstveni identifikator za korisnika kao string.

Mogu lako da implementiram ove četiri, ali pošto su implementacije prilično generičke, `Flask-Login` pruža klasu mixin koja se zove `UserMixin` koja uključuje bezbedne implementacije koje su prikladne za većinu klasa korisničkih modela. Evo kako se klasa mixin dodaje modelu:

> `app/models.py` : Klasa za mešanje korisnika u `Flask-Login`

```py
#...
from flask_login import UserMixin

class User(UserMixin, db.Model):
    #...
```

## Funkcija učitavanja korisnika

`Flask-Login` prati prijavljenog korisnika tako što čuva njegov jedinstveni identifikator u Flask-ovoj korisničkoj sesiji, prostoru za skladištenje dodeljenom svakom korisniku koji se poveže sa aplikacijom. Svaki put kada prijavljeni korisnik pređe na novu stranicu, `Flask-Login` preuzima `ID` korisnika iz sesije, a zatim učitava tog korisnika u memoriju.

Pošto `Flask-Login` ne zna ništa o bazama podataka, potrebna mu je pomoć aplikacije pri učitavanju korisnika. Iz tog razloga, ekstenzija očekuje da će aplikacija konfigurisati funkciju za učitavanje korisnika, koja se može pozvati da bi se učitao korisnik na osnovu datog `ID`-a. Ova funkcija se može dodati u modulu `app/models.py` :

> `app/models.py` : Funkcija za učitavanje korisnika u `Flask-Login`

```py
from app import login
#...

@login.user_loader
def load_user(id):
    return db.session.get(User, int(id))
```

Korisnički učitavač je registrovan sa `Flask-Login`-om sa dekoratorom `@login.user_loader`. Ono `id` što `Flask-Login` prosleđuje funkciji kao argument biće string, tako da baze podataka koje koriste numeričke `ID`-ove moraju da konvertuju string u ceo broj kao što vidite gore.

## Prijavljivanje korisnika

Hajde da ponovo pogledamo funkciju pogleda prijave, koja je, kao što se sećate, implementirala lažno prijavljivanje koje je samo izdalo `flash()` poruku. Sada kada aplikacija ima pristup bazi podataka korisnika i zna kako da generiše i verifikuje heševe lozinki, ova funkcija pogleda može biti završena.

> `app/routes.py` : Logika funkcije pogleda prijavljivanja

```py
#...
from flask_login import current_user, login_user
import sqlalchemy as sa
from app import db
from app.models import User
#...
@app.route('/login', methods=['GET', 'POST'])
def login():
    if current_user.is_authenticated:
        return redirect(url_for('index'))
    form = LoginForm()
    if form.validate_on_submit():
        user = db.session.scalar(
            sa.select(User).where(User.username == form.username.data))
        if user is None or not user.check_password(form.password.data):
            flash('Invalid username or password')
            return redirect(url_for('login'))
        login_user(user, remember=form.remember_me.data)
        return redirect(url_for('index'))
    return render_template('login.html', title='Sign In', form=form)
```

Prva dva reda u `login()` funkciji se bave čudnom situacijom. Zamislite da imate korisnika koji je prijavljen i da korisnik ode na `/login` URL adresu vaše aplikacije. Očigledno je da je to greška, tako da ne želim da to dozvolim. Promenljiva `current_user` dolazi iz `Flask-Login`-a i može se koristiti u bilo kom trenutku tokom obrade zahteva za dobijanje objekta korisnika koji predstavlja klijenta tog zahteva. Vrednost ove promenljive može biti objekat korisnika iz baze podataka (koji Flask-Login čita kroz povratni poziv učitavača korisnika koji sam naveo gore) ili poseban anonimni objekat korisnika ako se korisnik još nije prijavio. Sećate se onih svojstava koja je `Flask-Login` zahtevao u objektu korisnika? Jedno od njih je bilo `is_authenticated`, što je korisno da se proveri da li je korisnik prijavljen ili ne. Kada je korisnik već prijavljen, ja samo preusmerim na indeksnu stranicu.

Umesto poziva flash()koji sam ranije koristio, sada mogu stvarno da prijavim korisnika. Prvi korak je učitavanje korisnika iz baze podataka. Korisničko ime je došlo sa slanjem obrasca, tako da mogu da upitam bazu podataka sa njim da bih pronašao korisnika. U tu svrhu koristim klauzulu where(), da bih pronašao korisnike sa datim korisničkim imenom. Pošto znam da će biti samo jedan ili nula rezultata, izvršavam upit pozivanjem db.session.scalar(), koja će vratiti objekat korisnika ako postoji, ili Noneako ne postoji. U poglavlju 4 videli ste da kada pozovete all()metodu, upit se izvršava i dobijate listu svih rezultata koji odgovaraju tom upitu. Metoda first()je još jedan često korišćen način za izvršavanje upita, kada vam je potreban samo jedan rezultat.

Ako dobijem podudaranje za korisničko ime koje je navedeno, mogu sledeće da proverim da li je lozinka koja je takođe došla uz obrazac važeća. To se radi pozivanjem metode koju `check_password()` sam gore definisao. Ovo će uzeti heš lozinke sačuvan kod korisnika i utvrditi da li se lozinka uneta u obrazac podudara sa hešem ili ne. Dakle, sada imam dva moguća uslova greške: korisničko ime može biti nevažeće ili lozinka može biti netačna za korisnika. U oba ova slučaja, prikazujem poruku i preusmeravam nazad na prompt za prijavu kako bi korisnik mogao ponovo da pokuša.

Ako su i korisničko ime i lozinka ispravni, onda pozivam funkciju login_user(), koja dolazi iz Flask-Login-a. Ova funkcija će registrovati korisnika kao prijavljenog, što znači da će sve buduće stranice na koje korisnik kreće imati current_userpromenljivu podešenu na tog korisnika.

Da bih završio proces prijave, samo preusmerim novoprijavljenog korisnika na indeksnu stranicu.

## Odjavljivanje korisnika

Znam da ću takođe morati da ponudim korisnicima opciju da se odjave iz aplikacije. To se može uraditi pomoću logout_user()funkcije Flask-Login. Evo funkcije za prikaz odjave:

> `app/routes.py` : Funkcija pogleda za odjavu

```py
#...
from flask_login import logout_user

#...
@app.route('/logout')
def logout():
    logout_user()
    return redirect(url_for('index'))
```

Da bih ovaj link predstavio korisnicima, mogu da podesim da se link za prijavu u navigacionoj traci automatski prebaci na link za odjavu nakon što se korisnik prijavi. To se može uraditi uslovnim izrazom u šablonu base.html :

> `app/templates/base.html` : Linkovi za uslovno prijavljivanje i odjavljivanje

```html
<div>
    Microblog:
    <a href="{{ url_for('index') }}">Home</a>
    {% if current_user.is_anonymous %}
    <a href="{{ url_for('login') }}">Login</a>
    {% else %}
    <a href="{{ url_for('logout') }}">Logout</a>
    {% endif %}
</div>
```

Svojstvo is_anonymousje jedan od atributa koje Flask-Login dodaje korisničkim objektima kroz UserMixinklasu. current_user.is_anonymousIzraz će biti aktivan Truesamo kada korisnik nije prijavljen.

## Zahtevanje od korisnika da se prijave

Flask-Login pruža veoma korisnu funkciju koja primorava korisnike da se prijave pre nego što mogu da vide određene stranice aplikacije. Ako korisnik koji nije prijavljen pokuša da vidi zaštićenu stranicu, Flask-Login će automatski preusmeriti korisnika na obrazac za prijavu i vratiti ga na stranicu koju je želeo da vidi tek nakon što je proces prijave završen.

Da bi ova funkcija bila implementirana, Flask-Login mora da zna koja je funkcija pregleda koja obrađuje prijave. Ovo se može dodati u app/__init__.py :

```py
#...
login = LoginManager(app)
login.login_view = 'login'
```

Gore navedena vrednost 'login'je naziv funkcije (ili krajnje tačke) za prikaz za prijavu. Drugim rečima, naziv koji biste koristili u pozivu url_for()da biste dobili URL adresu.

Flask-Login štiti funkciju pregleda od anonimnih korisnika pomoću dekoratora pod nazivom @login_required. Kada dodate ovaj dekorator funkciji pregleda ispod @app.routedekoratora iz Flask-a, funkcija postaje zaštićena i neće dozvoliti pristup korisnicima koji nisu autentifikovani. Evo kako se dekorator može primeniti na funkciju pregleda indeksa aplikacije:

> `app/routes.py` : `@login_required dekorator`

```py
from flask_login import login_required

@app.route('/')
@app.route('/index')
@login_required
def index():
    #...
```

Preostaje da se implementira preusmeravanje nazad sa uspešne prijave na stranicu kojoj je korisnik želeo da pristupi. Kada korisnik koji nije prijavljen pristupi funkciji pregleda zaštićenoj dekoratorom @login_required, dekorator će preusmeriti na stranicu za prijavu, ali će u ovo preusmeravanje uključiti neke dodatne informacije kako bi se aplikacija zatim mogla vratiti na originalnu stranicu. Ako korisnik ode na /index, na primer, @login_requireddekorator će presresti zahtev i odgovoriti preusmeravanjem na /login, ali će dodati argument stringa upita ovom URL-u, čineći kompletan URL preusmeravanja /login?next=/index. nextArgument stringa upita je podešen na originalni URL, tako da aplikacija može to koristiti za preusmeravanje nazad nakon prijave.

Evo isečka koda koji pokazuje kako se čita i obrađuje nextargument upitnog niza. Promene su u četiri reda ispod login_user()poziva.

> `app/routes.py` : Preusmeri na sledeću stranicu

```py
from flask import request
from urllib.parse import urlsplit

@app.route('/login', methods=['GET', 'POST'])
def login():
    #...
    if form.validate_on_submit():
        user = db.session.scalar(
            sa.select(User).where(User.username == form.username.data))
        if user is None or not user.check_password(form.password.data):
            flash('Invalid username or password')
            return redirect(url_for('login'))
        login_user(user, remember=form.remember_me.data)
        next_page = request.args.get('next')
        if not next_page or urlsplit(next_page).netloc != '':
            next_page = url_for('index')
        return redirect(next_page)
    #...
```

Odmah nakon što se korisnik prijavi pozivanjem funkcije Flask-Login, dobija se login_user()vrednost argumenta upitnog stringa. Flask pruža promenljivu koja sadrži sve informacije koje je klijent poslao sa zahtevom. Konkretno, atribut prikazuje sadržaj upitnog stringa u prijateljskom rečničkom formatu. Zapravo postoje tri moguća slučaja koja treba uzeti u obzir da bi se utvrdilo gde preusmeriti korisnika nakon uspešnog prijavljivanja:nextrequestrequest.args

- Ako URL za prijavu nema nextargument, korisnik se preusmerava na indeksnu stranicu.
- Ako URL za prijavu sadrži nextargument koji je podešen na relativnu putanju (ili drugim rečima, URL bez dela domena), korisnik se preusmerava na taj URL.
- Ako URL za prijavu sadrži nextargument koji je podešen na punu URL adresu koja uključuje ime domena, onda se ta URL adresa ignoriše i korisnik se preusmerava na indeksnu stranicu.

Prvi i drugi slučaj se sami po sebi razumeju. Treći slučaj je tu da bi aplikacija bila bezbednija. Napadač bi mogao da ubaci URL adresu zlonamerne veb stranice u argument next, tako da aplikacija preusmerava samo kada je URL adresa relativna, što osigurava da preusmeravanje ostaje unutar iste veb stranice kao i aplikacija. Da bih utvrdio da li je URL adresa apsolutna ili relativna, analiziram je pomoću Pajtonove urlsplit()funkcije, a zatim proveravam da li netlocje komponenta podešena ili ne.

## Prikaz prijavljenog korisnika u šablonima

Sećate li se da sam još u drugom poglavlju kreirao lažnog korisnika da mi pomogne da dizajniram početnu stranicu aplikacije pre nego što je korisnički podsistem bio postavljen? Pa, aplikacija sada ima prave korisnike, tako da sada mogu da uklonim lažnog korisnika i počnem da radim sa pravim korisnicima. Umesto lažnog korisnika mogu da koristim Flask-Login-ove `current_user` u šablonu `index.html` :

> `app/templates/index.html` : Prosledi trenutnog korisnika šablonu

```html
{% extends "base.html" %}

{% block content %}
    <h1>Hi, {{ current_user.username }}!</h1>
    {% for post in posts %}
    <div><p>{{ post.author.username }} says: <b>{{ post.body }}</b></p></div>
    {% endfor %}
{% endblock %}
```

I mogu ukloniti userargument šablona u funkciji pregleda:

> `app/routes.py` : Više ne prosleđuj korisnika šablonu

```py
@app.route('/')
@app.route('/index')
@login_required
def index():
    #...
    return render_template("index.html", title='Home Page', posts=posts)
```

Ovo je dobar trenutak da testirate kako funkcioniše funkcionalnost prijavljivanja i odjavljivanja. Pošto još uvek nema registracije korisnika, jedini način da se korisnik doda u bazu podataka jeste da se to uradi preko Pajton ljuske, pa pokrenite flask shelli unesite sledeće komande da biste registrovali korisnika:

```py
>>> u = User(username='susan', email='susan@example.com')
>>> u.set_password('cat')
>>> db.session.add(u)
>>> db.session.commit()
```

Ako sada pokrenete aplikaciju i odete na URL-ove aplikacije / ili /index, bićete odmah preusmereni na stranicu za prijavu, a nakon što se prijavite koristeći akreditive korisnika koga ste dodali u svoju bazu podataka, bićete vraćeni na originalnu stranicu, na kojoj ćete videti personalizovani pozdrav i lažne objave na blogu. Ako zatim kliknete na vezu za odjavu u gornjoj navigacionoj traci, bićete vraćeni na indeksnu stranicu kao anonimni korisnik i odmah ćete biti ponovo preusmereni na stranicu za prijavu pomoću Flask-Login-a.

### Registracija korisnika

Poslednji deo funkcionalnosti koji ću napraviti u ovom poglavlju je obrazac za registraciju, tako da se korisnici mogu registrovati putem veb obrasca. Počnimo kreiranjem klase veb obrasca u app/forms.py :

> `app/forms.py` : Forma za registraciju korisnika

```py
from flask_wtf import FlaskForm
from wtforms import StringField, PasswordField, BooleanField, SubmitField
from wtforms.validators import ValidationError, DataRequired, Email, EqualTo
import sqlalchemy as sa
from app import db
from app.models import User
#...
class RegistrationForm(FlaskForm):
    username = StringField('Username', validators=[DataRequired()])
    email = StringField('Email', validators=[DataRequired(), Email()])
    password = PasswordField('Password', validators=[DataRequired()])
    password2 = PasswordField(
        'Repeat Password', validators=[DataRequired(), EqualTo('password')])
    submit = SubmitField('Register')

    def validate_username(self, username):
        user = db.session.scalar(sa.select(User).where(
            User.username == username.data))
        if user is not None:
            raise ValidationError('Please use a different username.')

    def validate_email(self, email):
        user = db.session.scalar(sa.select(User).where(
            User.email == email.data))
        if user is not None:
            raise ValidationError('Please use a different email address.')
```

Postoji nekoliko zanimljivih stvari u ovom novom obrascu vezanih za validaciju. Prvo, za emailpolje sam dodao drugi validator posle DataRequired, nazvan Email. Ovo je još jedan validator zaliha koji dolazi sa WTForms-om i koji će osigurati da ono što korisnik unese u ovo polje odgovara strukturi imejl adrese.

Validator `Email()` iz WTForms zahteva instaliranje eksterne zavisnosti:

```sh
(venv) $ pip install email-validator
```

Pošto je ovo formular za registraciju, uobičajeno je da se od korisnika traži da dva puta unese lozinku kako bi se smanjio rizik od greške u kucanju. Iz tog razloga, ja imam polja passwordi password2. Drugo polje za lozinku koristi još jedan validator akcija pod nazivom EqualTo, koji će se uveriti da je njegova vrednost identična onoj za prvo polje za lozinku.

Kada dodate bilo koje metode koje odgovaraju obrascu validate_<field_name>, WTForms ih uzima kao prilagođene validatore i poziva ih pored standardnih validatora. Dodao sam dve takve metode ovoj klasi za polja usernamei email. U ovom slučaju želim da budem siguran da korisničko ime i adresa e-pošte koje je korisnik uneo već nisu u bazi podataka, pa ove dve metode izdaju upite u bazu podataka očekujući da neće biti rezultata. U slučaju da rezultat postoji, greška validacije se pokreće pokretanjem izuzetka tipa ValidationError. Poruka uključena kao argument u izuzetku biće poruka koja će biti prikazana pored polja da bi je korisnik video.

Obratite pažnju na to kako se izdaju dva upita za validaciju. Ovi upiti nikada neće pronaći više od jednog rezultata, pa umesto da ih pokrećem sa db.session.scalars()koristim db.session.scalar()u jednini, koja vraća Noneako nema rezultata, ili u suprotnom prvi rezultat.

Da bih prikazao ovaj obrazac na veb stranici, potreban mi je HTML šablon, koji ću sačuvati u datoteci app/templates/register.html. Ovaj šablon je konstruisan slično kao onaj za obrazac za prijavu:

> `app/templates/register.html` : Šablon za registraciju

```html
{% extends "base.html" %}

{% block content %}
    <h1>Register</h1>
    <form action="" method="post">
        {{ form.hidden_tag() }}
        <p>
            {{ form.username.label }}<br>
            {{ form.username(size=32) }}<br>
            {% for error in form.username.errors %}
            <span style="color: red;">[{{ error }}]</span>
            {% endfor %}
        </p>
        <p>
            {{ form.email.label }}<br>
            {{ form.email(size=64) }}<br>
            {% for error in form.email.errors %}
            <span style="color: red;">[{{ error }}]</span>
            {% endfor %}
        </p>
        <p>
            {{ form.password.label }}<br>
            {{ form.password(size=32) }}<br>
            {% for error in form.password.errors %}
            <span style="color: red;">[{{ error }}]</span>
            {% endfor %}
        </p>
        <p>
            {{ form.password2.label }}<br>
            {{ form.password2(size=32) }}<br>
            {% for error in form.password2.errors %}
            <span style="color: red;">[{{ error }}]</span>
            {% endfor %}
        </p>
        <p>{{ form.submit() }}</p>
    </form>
{% endblock %}
```

Šablonu formulara za prijavu potreban je link koji šalje nove korisnike na formular za registraciju, odmah ispod formulara:

> `app/templates/login.html` : Link do stranice za registraciju

```html
<p>New User? <a href="{{ url_for('register') }}">Click to Register!</a></p>
```

I na kraju, treba da napišem funkciju pregleda koja će obrađivati registracije korisnika u `app/routes.py` :

> `app/routes.py` : Funkcija pregleda registracije korisnika

```py
from app import db
from app.forms import RegistrationForm
#...
@app.route('/register', methods=['GET', 'POST'])
def register():
    if current_user.is_authenticated:
        return redirect(url_for('index'))
    form = RegistrationForm()
    if form.validate_on_submit():
        user = User(username=form.username.data, email=form.email.data)
        user.set_password(form.password.data)
        db.session.add(user)
        db.session.commit()
        flash('Congratulations, you are now a registered user!')
        return redirect(url_for('login'))
    return render_template('register.html', title='Register', form=form)
```

I ova funkcija pogleda bi trebalo da bude uglavnom sama po sebi razumljiva. Prvo proveravam da li je korisnik koji poziva ovu rutu prijavljen. Forma se obrađuje na isti način kao i ona za prijavu. Logika koja se obavlja unutar if `validate_on_submit()` uslovnog izraza kreira novog korisnika sa korisničkim imenom, imejlom i lozinkom koji su navedeni, upisuje ga u bazu podataka, a zatim preusmerava na prompt za prijavu kako bi se korisnik mogao prijaviti.

### Forma za registraciju

Sa ovim promenama, korisnici bi trebalo da budu u mogućnosti da kreiraju naloge na ovoj aplikaciji, kao i da se prijavljuju i odjavljuju. Obavezno isprobajte sve funkcije validacije koje sam dodao u formular za registraciju kako biste bolje razumeli kako funkcionišu. Ponovo ću se osvrnuti na podsistem za autentifikaciju korisnika u budućem poglavlju kako bih dodao dodatne funkcionalnosti, kao što je omogućavanje korisniku da resetuje lozinku ako je zaboravi. Ali za sada je ovo dovoljno da se nastavi sa izgradnjom drugih oblasti aplikacije.
