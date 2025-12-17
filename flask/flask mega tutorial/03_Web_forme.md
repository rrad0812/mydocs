# Veb forme

U drugom poglavlju sam napravio jednostavan šablon za početnu stranicu aplikacije i koristio lažne objekte kao rezervisane prostore za stvari koje još nemam, kao što su korisnici i objave na blogu. U ovom poglavlju ću se pozabaviti jednom od mnogih rupa koje još uvek imam u ovoj aplikaciji, tačnije kako da prihvatim unos od korisnika putem veb forme.

Veb forme su jedan od najosnovnijih gradivnih blokova u svakoj veb aplikaciji. Koristiću forme da bih omogućio korisnicima da šalju objave na blogu, a takođe i za prijavljivanje u aplikaciju.

Pre nego što nastavite sa ovim poglavljem, uverite se da imate instaliranu aplikaciju za mikroblog onako kako sam je ostavio u prethodnom poglavlju i da je možete pokrenuti bez ikakvih grešaka.

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.3), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.3.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.2...v0.3).

## Uvod u Flask-WTF

Za rukovanje veb formama u ovoj aplikaciji koristiću ekstenziju Flask-WTF, koja je tanki omotač oko WTForms paketa i lepo ga integriše sa Flask-om. Ovo je prva Flask ekstenzija koju vam predstavljam, ali neće biti poslednja. Ekstenzije su veoma važan deo Flask ekosistema, jer pružaju rešenja za probleme o kojima Flask namerno ne iznosi svoje mišljenje.

Flask ekstenzije su regularni Pajton paketi koji se instaliraju sa `pip`. Možete da nastavite i instalirate `Flask-VTF` u svoje virtuelno okruženje:

```sh
(venv) pip install flask-wtf
```

Za sada je aplikacija veoma jednostavna i zbog toga nisam morao da brinem o njenoj konfiguraciji. Ali za bilo koju aplikaciju osim najjednostavnijih, otkrićete da Flask (a moguće i Flask ekstenzije koje koristite) nude izvesnu količinu slobode u načinu rada, i potrebno je da donesete neke odluke, koje prosleđujete frejmvorku kao listu konfiguracionih promenljivih.

Postoji nekoliko formata u kojima aplikacija može da odredi opcije konfiguracije. Najosnovnije rešenje je da definišete svoje promenljive kao ključeve u `app.config`, koji koriste stil rečnika za rad sa promenljivim. Na primer, mogli biste da uradite nešto ovako:

```py
app = Flask(__name__)
app.config['SECRET_KEY'] = 'you-will-never-guess'
#... add more variables here as needed
```

Iako je gornja sintaksa dovoljna za kreiranje opcija konfiguracije za Flask, volim da primenjujem princip razdvajanja brige, pa umesto da stavljam svoju konfiguraciju na isto mesto gde kreiram svoju aplikaciju, koristiću malo složeniju strukturu koja mi omogućava da čuvam svoju konfiguraciju u posebnoj datoteci.

Rešenje koje mi se jako sviđa jer je veoma proširivo jeste korišćenje Pajton klase za čuvanje konfiguracionih promenljivih. Da bih stvari lepo organizovao, kreiraću konfiguracionu klasu u posebnom Pajton modulu. Ispod možete videti novu konfiguracionu klasu za ovu aplikaciju, sačuvanu u modulu `config.py` u direktorijumu najvišeg nivoa.

> `config.py` : Konfiguracija tajnog ključa

```py
import os

class Config:
    SECRET_KEY = os.environ.get('SECRET_KEY') or 'you-will-never-guess'
```

Prilično jednostavno, zar ne? Podešavanja konfiguracije su definisana kao promenljive klase unutar `Config` klase. Kako aplikaciji treba više stavki konfiguracije, one se mogu dodati ovoj klasi, a kasnije, ako otkrijem da mi je potrebno više od jednog skupa konfiguracije, mogu kreirati podklase. Ali ne brinite o tome još uvek.

Promenljiva konfiguracije `SECRET_KEY` koju sam dodao kao jedinu stavku konfiguracije je važan deo u većini Flask aplikacija. Flask i neka od njegovih proširenja koriste vrednost tajnog ključa kao kriptografski ključ, koristan za generisanje potpisa ili tokena. Proširenje Flask-WTF ga koristi za zaštitu veb forme od opasnog napada koji se zove `Cross-Site Request Forgery` ili `CSRF` (izgovara se "sisurf"). Kao što mu ime govori, tajni ključ bi trebalo da bude tajan, jer snaga tokena i potpisa generisanih pomoću njega zavisi od toga da ga nijedna osoba izvan pouzdanih održavalaca aplikacije ne zna.

Vrednost tajnog ključa se postavlja kao izraz sa dva člana, spojena operatorom `or`. Prvi član traži vrednost promenljive okruženja, koja se naziva `SECRET_KEY`. Drugi član je samo string u kodu. Ovo je forma koji ćete često videti da ponavljam za konfiguracione promenljive. Ideja je da je vrednost dobijena iz promenljive okruženja poželjnija, ali ako okruženje ne definiše promenljivu, onda se umesto toga koristi string u kodu kao podrazumevana vrednost. Kada razvijate ovu aplikaciju, bezbednosni zahtevi su niski, tako da možete jednostavno ignorisati ovo podešavanje i dozvoliti da se koristi string u kodu. Ali kada se ova aplikacija rasporedi na produkcijskom serveru, postaviću jedinstvenu i teško pogodljivu vrednost u okruženju, tako da server ima bezbedan ključ koji niko drugi ne zna.

Sada kada imam konfiguracioni fajl, treba da kažem Flasku da ga pročita i primeni. To se može uraditi odmah nakon što se kreira instanca Flask aplikacije pomoću `app.config.from_object()` metode:

> `app/__init__.py` : Konfiguracija Flaska

```py
from flask import Flask
from config import Config

app = Flask(__name__)
app.config.from_object(Config)

from app import routes
```

Način na koji uvozim `Config` klasu može u početku delovati zbunjujuće, ali ako pogledate kako se Flask klasa (veliko slovo "F") uvozi iz flas kpaketa (malo slovo "f") primetićete da radim isto sa konfiguracijom. Malo slovo "config" je naziv Pajton modula `config.py`, a očigledno je da je onaj sa velikim slovom "C" stvarna klasa.

Kao što sam gore pomenuo, stavkama konfiguracije se može pristupiti pomoću sintakse rečnika iz Flask `app.config`. Ovde možete videti kratku sesiju sa Pajton interpreterom gde proveravam koja je vrednost tajnog ključa:

```py
>>> from microblog import app
>>> app.config['SECRET_KEY']
'you-will-never-guess'
```

## Forma za prijavu korisnika

Ekstenzija Flask-WTF koristi Pajton klase za predstavljanje veb forme. Klasa forme jednostavno definiše polja forme kao promenljive klase.

Još jednom imajući u vidu razdvajanje briga, koristiću novi modul `app/forms.py` za čuvanje klasa mojih veb formi. Za početak, definišimo formu za prijavu korisnika, koji traži od korisnika da unese korisničko ime i lozinku. Forma će takođe sadržati polje za potvrdu "zapamti me" i dugme za slanje:

> `app/forms.py` : Formular za prijavu

```py
from flask_wtf import FlaskForm
from wtforms import StringField, PasswordField, BooleanField, SubmitField
from wtforms.validators import DataRequired

class LoginForm(FlaskForm):
    username = StringField('Username', validators=[DataRequired()])
    password = PasswordField('Password', validators=[DataRequired()])
    remember_me = BooleanField('Remember Me')
    submit = SubmitField('Sign In')
```

Većina Flask ekstenzija koristi `flask_name` konvenciju imenovanja za svoj simbol uvoza najvišeg nivoa. U ovom slučaju, Flask-WTF ima sve svoje simbole pod `flask_wtf`. Ovo je mesto odakle se osnovna klasa `FlaskForm` uvozi, na vrhu `app/forms.py`.

Četiri klase koje predstavljaju tipove polja koje koristim za ovu formu su direktno uvezene iz WTForms paketa, pošto Flask-WTF ekstenzija ne pruža prilagođene verzije. Za svako polje, objekat se kreira kao promenljiva klase u klasi `LoginForm`. Svakom polju se daje opis ili oznaka kao prvi argument.

Opcioni `validators` argument koji vidite u nekim poljima koristi se za dodavanje validacije poljima. Validator `DataRequired` jednostavno proverava da li je polje poslato prazno. Dostupno je mnogo više validatora, od kojih će se neki koristiti u drugim formama.

### Šablon forme

Sledeći korak je dodavanje forme u HTML šablon kako bi se mogao prikazati na veb stranici. Dobra vest je da polja koja su definisana u `LoginForm` klasi znaju kako da se prikažu kao HTML, tako da je ovaj zadatak prilično jednostavan. Ispod možete videti šablon za prijavu, koji ću sačuvati u datoteci `app/templates/login.html` :

> `app/teplates/login.html` : Šablon formulara za prijavu

```html
{% extends "base.html" %}

{% block content %}
    <h1>Sign In</h1>
    <form action="" method="post" novalidate>
        {{ form.hidden_tag() }}
        <p>
            {{ form.username.label }}<br>
            {{ form.username(size=32) }}
        </p>
        <p>
            {{ form.password.label }}<br>
            {{ form.password(size=32) }}
        </p>
        <p>{{ form.remember_me() }} {{ form.remember_me.label }}</p>
        <p>{{ form.submit() }}</p>
    </form>
{% endblock %}
```

Za ovaj šablon ponovo koristim `base.html` šablon kao što je prikazano u 2. poglavlju, kroz `extends` naredbu o nasleđivanju šablona. Zapravo ću ovo uraditi sa svim šablonima, kako bih osigurao dosledan raspored koji uključuje gornju navigacionu traku na svim stranicama aplikacije.

Ovaj šablon očekuje da se objekat forme instanciran iz `LoginForm` klase da kao argument, na koji možete videti referencu kao form. Ovaj argument će se poslati funkciji pogleda prijave, koju još uvek nisam napisao.

HTML `<form>` element se koristi kao kontejner za veb formu. Atribut `form action` se koristi da obavesti pregledač o URL-u koji treba koristiti prilikom slanja informacija koje je korisnik uneo u formu. Kada je akcija podešena na prazan string, forma se šalje na URL koji se trenutno nalazi u adresnoj traci, a to je URL koji je prikazao formu na stranici. Atribut `method` određuje HTTP metod zahteva koji treba koristiti prilikom slanja forme na server. Podrazumevano je slanje sa zahtevom `GET`, ali u skoro svim slučajevima, korišćenje `POST` zahteva omogućava bolje korisničko iskustvo jer zahtevi ovog tipa mogu poslati podatke forme u telu zahteva, dok `GET` zahtevi dodaju polja forme u URL, zatrpavajući adresnu traku pregledača. Atribut `novalidate` se koristi da obavesti veb pregledač da ne primenjuje validaciju na polja u ovoj formi, što efikasno prepušta ovaj zadatak Flask aplikaciji koja se pokreće na serveru. Korišćenje `novalidate` je potpuno opciono, ali za ovu prvu formu je važno da ga podesite jer će vam to omogućiti da testirate validaciju na strani servera kasnije u ovom poglavlju.

Argument šablona `form.hidden_tag()` generiše skriveno polje koje sadrži token koji se koristi za zaštitu forme od CSRF napada. Sve što treba da uradite da biste zaštitili formu jeste:

- da uključite ovo skriveno polje i
- da definišete `SECRET_KEY` promenljivu u konfiguraciji Flask-a.

Ako se pobrinete za ove dve stvari, Flask-WTF će uraditi ostalo za vas.

Ako ste ranije pisali HTML veb forme, možda vam je bilo čudno što u ovom šablonu nema HTML polja. To je zato što polja iz objekta forme znaju kako da se prikazuju kao HTML. Sve što je trebalo da uradim jeste da uključim {{ `form.<field_name>.label` }} gde želim oznaku polja i {{ `form.<field_name>()` }} gde želim polje. Za polja koja zahtevaju dodatne HTML atribute, oni se mogu proslediti kao argumenti. Polja za korisničko ime i lozinku u ovom šablonu uzimaju `size` kao argument koji će biti dodat `<input>` HTML elementu kao atribut. Na ovaj način možete dodati i CSS klase ili ID-ove poljima forme.

### Funkcija pogleda forme

Poslednji korak pre nego što možete videti ovu formu u pregledaču jeste kodiranje nove funkcije pogleda u aplikaciji koja prikazuje šablon iz prethodnog odeljka.

Dakle, hajde da napišemo novu funkciju pogleda mapiranu na URL adresu `/login` koja kreira formu i prosleđuje je šablonu za renderovanje. Ova funkcija pogleda takođe može da ide u modul `app/routes.py` sa prethodnom:

> `app/routes.py` : Funkcija pogleda pri prijavljivanju

```py
from flask import render_template
from app import app
from app.forms import LoginForm

#...

@app.route('/login')
def login():
    form = LoginForm()
    return render_template('login.html', title='Sign In', form=form)
```

Ono što sam ovde uradio jeste da sam uvezao `LoginForm` klasu iz `forms.py`, napravio instancu objekta iz nje i poslao je šablonu. Sintaksa `form=form` može izgledati čudno, ali jednostavno se radi o prosleđivanju `form` objekta kreiranog u gornjem redu (i prikazanog sa desne strane) šablonu sa imenom `form` (prikazano sa leve strane). Ovo je sve što je potrebno da bi se polja forme prikazala.

Da bi se olakšao pristup formi za prijavu, osnovni šablon može proširiti `<div>` element u datoteci `base.html` da bi uključio vezu do njega u navigacionoj traci:

> `app/templates/base.html` : Link za prijavu u navigacionoj traci

```html
<div>
    Microblog:
    <a href="/index">Home</a>
    <a href="/login">Login</a>
</div>
```

U ovom trenutku možete pokrenuti aplikaciju i videti formu u vašem veb pregledaču. Dok je aplikacija pokrenuta, unesite tekst <http://localhost:5000/> u adresnu traku pregledača, a zatim kliknite na vezu "Login" u gornjoj navigacionoj traci da biste videli novu formu za prijavu. Prilično kul, zar ne?

### Primanje podataka forme

Ako pokušate da pritisnete dugme za slanje, pregledač će prikazati grešku `"Metod nije dozvoljen"`. To je zato što funkcija pogleda za prijavu iz prethodnog odeljka do sada obavlja samo polovinu posla. Ona može da prikaže formu na veb stranici, ali još uvek nema logiku za obradu podataka koje je korisnik poslao. Ovo je još jedno područje gde Flask-WTF čini posao zaista lakšim. Evo ažurirane verzije funkcije pogleda koja prihvata i validira podatke koje je korisnik poslao:

> `app/routes.py` : Primanje podataka za prijavu

```py
from flask import render_template, flash, redirect

@app.route('/login', methods=['GET', 'POST'])
def login():
    form = LoginForm()
    if form.validate_on_submit():
        flash('Login requested for user {}, remember_me={}'.format(
            form.username.data, form.remember_me.data))
        return redirect('/index')
    return render_template('login.html', title='Sign In', form=form)
```

Prva nova stvar u ovoj verziji je `methods` argument u dekoratoru rute. Ovo govori Flasku da ova funkcija prikaza prihvata `GET` i `POST` zahteve, zamenjujući podrazumevanu vrednost, koja je da prihvata samo GET zahteve. HTTP protokol navodi da su `GET` zahtevi oni koji vraćaju informacije klijentu (veb pregledaču u ovom slučaju). Svi zahtevi u aplikaciji do sada su ovog tipa. `POST` zahtevi se obično koriste kada pregledač šalje podatke forme serveru (u stvarnosti, GET zahtevi se takođe mogu koristiti u ovu svrhu, ali to nije preporučena praksa). Greška `"Metod nije dozvoljen"` koju vam je pregledač ranije pokazao, pojavljuje se zato što je pregledač pokušao da pošalje zahtev `POST`, a aplikacija nije bila konfigurisana da ga prihvati. Navođenjem argumenta `methods`, govorite Flasku koje metode zahteva treba prihvatiti.

Metoda `form.validate_on_submit()` obavlja sav posao obrade forme. Kada pregledač pošalje zahtev `GET` za prijem veb stranice sa formom, ova metoda će vratiti `False`, tako da u tom slučaju funkcija preskače `if` naredbu i direktno prelazi na prikazivanje šablona u poslednjem redu funkcije.

Kada pregledač pošalje POST zahtev kao rezultat toga što korisnik klikne na dugme za slanje, `form.validate_on_submit()` on će prikupiti sve podatke, pokrenuti sve validatore povezane sa poljima i, ako je sve u redu, vratiće `True`, što ukazuje da su podaci validni i da ih aplikacija može obraditi. Ali ako bar jedno polje ne prođe validaciju, funkcija će vratiti `False`, i to će prouzrokovati da se forma vrati korisniku, kao u GET slučaju zahteva. Kasnije ću dodati poruku o grešci kada validacija ne uspe.

Kada `form.validate_on_submit()` vrati `True`, funkcija `login` poziva dve nove funkcije, uvezene iz Flask-a. Funkcija `flash()` je koristan način da se korisniku prikaže poruka. Mnoge aplikacije koriste ovu tehniku da obaveste korisnika da li je neka radnja bila uspešna ili ne. U ovom slučaju, koristiću ovaj mehanizam kao privremeno rešenje, jer još uvek nemam svu potrebnu infrastrukturu za stvarno prijavljivanje korisnika. Najbolje što mogu da uradim za sada je da prikažem poruku koja potvrđuje da je aplikacija primila akreditive.

Druga nova funkcija koja se koristi u funkciji prikaza pri prijavi je `redirect()`. Ova funkcija nalaže klijentskom veb pregledaču da automatski pređe na drugu stranicu, datu kao argument. Ova funkcija pogleda je koristi da preusmeri korisnika na indeksnu stranicu aplikacije.

Kada pozovete `flash()` funkciju, Flask čuva poruku, ali fleš poruke se neće magično pojaviti na veb stranicama. Šabloni aplikacije moraju da prikažu ove fleš poruke na način koji funkcioniše za raspored sajta. Dodaću ove poruke osnovnom šablonu, tako da svi šabloni naslede ovu funkcionalnost. Ovo je ažurirani osnovni šablon:

> `app/templates/base.html` : Fleš poruke u osnovnom šablonu

```html
<html>
    <head>
        {% if title %}
        <title>{{ title }} - microblog</title>
        {% else %}
        <title>microblog</title>
        {% endif %}
    </head>
    <body>
        <div>
            Microblog:
            <a href="/index">Home</a>
            <a href="/login">Login</a>
        </div>
        <hr>
        {% with messages = get_flashed_messages() %}
        {% if messages %}
        <ul>
            {% for message in messages %}
            <li>{{ message }}</li>
            {% endfor %}
        </ul>
        {% endif %}
        {% endwith %}
        {% block content %}{% endblock %}
    </body>
</html>
```

Ovde koristim `with` konstrukciju da dodelim rezultat poziva `get_flashed_messages()` promenljivoj messages, sve u kontekstu šablona. Funkcija `get_flashed_messages()` dolazi iz Flaska i vraća listu svih poruka koje su `flash()` prethodno registrovane. Uslov koji sledi proverava da li `messages` ima neki sadržaj i u tom slučaju, `<ul>` element se prikazuje sa svakom porukom kao `<li>` stavkom liste. Ovaj stil prikazivanja ne izgleda dobro za statusne poruke, ali tema stilizovanja veb aplikacije biće kasnije.

Zanimljivo svojstvo ovih flešovanih poruka je da kada se jednom zatraže kroz `get_flashed_messages()` funkciju, one se uklanjaju sa liste poruka, tako da se pojavljuju samo jednom nakon što se `flash()` funkcija pozove.

Ovo je odlično vreme da još jednom isprobate aplikaciju i testirate kako forma funkcioniše. Obavezno pokušajte da pošaljete formu sa praznim poljima za korisničko ime ili lozinku, kako biste videli kako validator `DataRequired` zaustavlja proces slanja.

### Poboljšanje validacije

Validatori koji su priloženi poljima forme sprečavaju prihvatanje nevažećih podataka u aplikaciju. Aplikacija postupa sa nevažećim unosom u formu tako što ponovo prikazuje forma, kako bi korisnik mogao da izvrši potrebne ispravke.

Ako ste pokušali da pošaljete nevažeće podatke, siguran sam da ste primetili da, iako mehanizmi validacije dobro funkcionišu, korisniku se ne daje nikakva indikacija da nešto nije u redu sa formom, već korisnik jednostavno dobija formular nazad. Sledeći zadatak je poboljšanje korisničkog iskustva dodavanjem smislene poruke o grešci pored svakog polja koje nije prošlo validaciju.

U stvari, validatori forme već generišu ove opisne poruke o greškama, tako da sve što nedostaje jeste dodatna logika u šablonu za njihovo prikazivanje.

Evo šablona za prijavu sa dodatnim porukama za validaciju polja za korisničko ime i lozinku:

> `app/templates/login.html` : Greške u validaciji u šablonu forme za prijavu

```html
{% extends "base.html" %}

{% block content %}
    <h1>Sign In</h1>
    <form action="" method="post" novalidate>
        {{ form.hidden_tag() }}
        <p>
            {{ form.username.label }}<br>
            {{ form.username(size=32) }}<br>
            {% for error in form.username.errors %}
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
        <p>{{ form.remember_me() }} {{ form.remember_me.label }}</p>
        <p>{{ form.submit() }}</p>
    </form>
{% endblock %}
```

Jedina promena koju sam napravio jeste dodavanje for petlji odmah nakon polja za korisničko ime i lozinku koje prikazuju poruke o greškama koje su dodali validatori crvenom bojom. Kao opšte pravilo, sva polja koja imaju priložene validatore imaće sve poruke o greškama koje su rezultat validacije dodate pod `form.<field_name>.errors`. Ovo će biti lista, jer polja mogu imati priloženih više validatora i više od jednog može prikazivati poruke o greškama korisniku.

Ako pokušate da pošaljete `formu` sa praznim korisničkim imenom ili lozinkom, dobićete lepu poruku o grešci crvenom bojom.

### Generisanje linkova

Forma za prijavu je sada prilično kompletna, ali pre nego što zatvorim ovo poglavlje, želim da razgovaram o pravilnom načinu uključivanja linkova u šablone i preusmeravanja. Do sada ste videli nekoliko slučajeva u kojima su linkovi definisani. Na primer, ovo je trenutna navigaciona traka u osnovnom šablonu:

```html
<div>
    Microblog:
    <a href="/index">Home</a>
    <a href="/login">Login</a>
</div>
```

Funkcija pogleda za prijavu takođe definiše vezu koja se prosleđuje funkciji `redirect()`:

```py
@app.route('/login', methods=['GET', 'POST'])
def login():
    form = LoginForm()
    if form.validate_on_submit():
        #...
        return redirect('/index')
    #...
```

Jedan problem sa pisanjem linkova direktno u šablonima i izvornim datotekama je taj što ako jednog dana odlučite da reorganizujete svoje linkove, moraćete da ih pretražite i zamenite u celoj aplikaciji.

Da bi imao bolju kontrolu nad ovim linkovima, Flask pruža funkciju pod nazivom `url_for()`, koja generiše URL-ove koristeći svoje interno mapiranje URL-ova na funkcije pogleda. Na primer, izraz `url_for('login')` vraća `/login`, i `url_for('index')` vraća `/index`. Argument `url_for()` je naziv krajnje tačke, što je naziv funkcije pogleda.

Možda se pitate zašto je bolje koristiti imena funkcija umesto URL-ova. Činjenica je da je mnogo verovatnije da će se URL-ovi promeniti nego imena funkcija pregleda, koja su potpuno interna. Sekundarni razlog je taj što, kao što ćete kasnije saznati, neki URL-ovi imaju dinamičke komponente u sebi, tako da bi ručno generisanje tih URL-ova zahtevalo spajanje više elemenata, što je zamorno i podložno greškama. Funkcija `url_for()` je takođe u stanju da generiše ove složene URL-ove sa mnogo elegantnijom sintaksom.

Dakle, od sada ću koristiti `url_for()` svaki put kada mi bude potrebno da generišem URL adresu aplikacije. Navigaciona traka u osnovnom šablonu tada postaje:

> `app/templates/base.html` : Koristite funkciju `url_for()` za linkove

```html
<div>
    Microblog:
    <a href="{{ url_for('index') }}">Home</a>
    <a href="{{ url_for('login') }}">Login</a>
</div>
```

A evo i ažurirane `login()` funkcije pogleda:

> `app/routes.py` : Koristite funkciju `url_for()` za linkove

```py
from flask import render_template, flash, redirect, url_for

#...

@app.route('/login', methods=['GET', 'POST'])
def login():
    form = LoginForm()
    if form.validate_on_submit():
        #...
        return redirect(url_for('index'))
    #...
```
