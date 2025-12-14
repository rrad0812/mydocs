
# Poglavlje 7 - Obrada grešaka

U ovom poglavlju pravim pauzu od kodiranja novih funkcija u mojoj aplikaciji za mikroblog i umesto toga ću razmotriti nekoliko strategija za rešavanje grešaka, koje se neizbežno pojavljuju u svakom softverskom projektu. Da bih ilustrovao ovu temu, namerno sam dozvolio da se greška uvuče u kod koji sam dodao u 6. poglavlju. Pre nego što nastavite sa čitanjem, proverite da li možete da je pronađete!

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.7), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.7.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.6...v0.7).

## Obrada grešaka u Flasku

Šta se dešava kada dođe do greške u Flask aplikaciji? Najbolji način da saznate je da to iskusite iz prve ruke. Slobodno pokrenite aplikaciju i uverite se da imate registrovana najmanje dva korisnika. Prijavite se kao jedan od korisnika, otvorite stranicu profila i kliknite na vezu "Izmeni". U uređivaču profila pokušajte da promenite korisničko ime u korisničko ime drugog korisnika koji je već registrovan i bum! Ovo će dovesti do zastrašujuće stranice "Interna greška servera" :

Ako pogledate u terminalnu sesiju gde se aplikacija pokreće, videćete trag steka greške. Tragovi steka su izuzetno korisni u otklanjanju grešaka, jer prikazuju redosled poziva u tom steku, sve do linije koja je proizvela grešku:

```sh
([2023-04-28 23:59:42,300] ERROR in app: Exception on /edit_profile [POST]
Traceback (most recent call last):
  File "venv/lib/python3.11/site-packages/sqlalchemy/engine/base.py", line 1963, in _exec_single_context
    self.dialect.do_execute(
  File "venv/lib/python3.11/site-packages/sqlalchemy/engine/default.py", line 918, in do_execute
    cursor.execute(statement, parameters)
sqlite3.IntegrityError: UNIQUE constraint failed: user.username
```

Trag steka vam pomaže da utvrdite u čemu je greška. Aplikacija dozvoljava korisniku da promeni korisničko ime, ali ne proverava da li se novo izabrano korisničko ime ne kolidira sa drugim korisnikom koji je već u sistemu. Greška dolazi od SQLAlchemy, koji pokušava da upiše novo korisničko ime u bazu podataka, ali baza podataka ga odbija jer `username` je kolona definisana sa `unique=True` opcijom.

Važno je napomenuti da stranica sa greškom koja se prikazuje korisniku ne pruža mnogo informacija o grešci, i to je dobro. Definitivno ne želim da korisnici saznaju da je pad sistema izazvan greškom u bazi podataka, ili koju bazu podataka koristim, ili koja su neka od imena tabela i polja u mojoj bazi podataka. Sve te informacije treba čuvati interno.

Ali postoji nekoliko stvari koje su daleko od idealnih. Imam stranicu sa greškom koja je veoma ružna i ne odgovara rasporedu aplikacije. Takođe imam važne tragove steka aplikacije koji se prikazuju na terminalu i koje moram stalno da pratim kako bih bio siguran da neću propustiti nijednu grešku. I naravno, imam grešku koju treba da ispravim. Pozabaviću se svim ovim problemima, ali prvo, hajde da pričamo o Flask-ovom režimu za otklanjanje grešaka.

## Režim otklanjanja grešaka

Način na koji ste videli da se greške obrađuju iznad je odličan za sistem koji radi na produkcijskom serveru. Ako dođe do greške, korisnik dobija nejasnu stranicu sa greškom (mada ću ovu stranicu sa greškom učiniti lepšom), a važni detalji greške nalaze se u izlazu procesa servera ili u datoteci dnevnika.

Ali kada razvijate svoju aplikaciju, možete omogućiti režim debagovanja, režim u kojem Flask direktno prikazuje odličan program za debagovanje u vašem pregledaču. Da biste aktivirali režim debagovanja, zaustavite aplikaciju, a zatim podesite sledeću promenljivu okruženja:

```sh
(venv) $ export FLASK_DEBUG=1
```

Ako koristite Microsoft Windows, ne zaboravite da koristite `set` umesto `export`.

Nakon što podesite `FLASK_DEBUG`, ponovo pokrenite server. Izlaz na vašem terminalu će biti malo drugačiji od onoga što ste navikli da vidite:

```sh
(venv) $ flask run
 * Serving Flask app 'microblog.py' (lazy loading)
 * Environment: development
 * Debug mode: on
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
 * Restarting with stat
 * Debugger is active!
 * Debugger PIN: 118-204-854
```

Sada ponovo srušite aplikaciju da biste videli interaktivni debager u vašem pregledaču.

Debager vam omogućava da proširite svaki stek frejm i vidite odgovarajući izvorni kod. Takođe možete otvoriti Pajton prompt na bilo kom od frejmova i izvršiti bilo koje validne Pajton izraze, na primer da biste proverili vrednosti promenljivih.

Izuzetno je važno da nikada ne pokrećete Flask aplikaciju u režimu za otklanjanje grešaka na produkcijskom serveru. Debager omogućava korisniku da daljinski izvršava kod na serveru, tako da može biti neočekivani poklon zlonamernom korisniku koji želi da se infiltrira u vašu aplikaciju ili vaš server. Kao dodatna mera bezbednosti, debager koji se pokreće u pregledaču počinje zaključan, a pri prvoj upotrebi će tražiti PIN, koji možete videti u izlazu komande `flask run`.

Pošto sam već kod teme režima za otklanjanje grešaka, trebalo bi da pomenem drugu važnu funkciju koja je omogućena u režimu za otklanjanje grešaka, a to je ponovno pokretanje. Ovo je veoma korisna razvojna funkcija koja automatski ponovo pokreće aplikaciju kada se izvorni fajl izmeni. Ako pokrenete aplikaciju `flask run` dok ste u režimu za otklanjanje grešaka, možete je ponovo pokrenuti i svaki put kada sačuvate fajl, aplikacija će se ponovo pokrenuti da bi preuzela novi kod.

## Prilagođene stranice sa greškama

Flask pruža mehanizam kojim aplikacija može da instalira sopstvene stranice sa greškama, tako da vaši korisnici ne moraju da vide obične i dosadne podrazumevane stranice. Na primer, definišimo prilagođene stranice sa greškama za HTTP greške `404` i `500`, dve najčešće. Definisanje stranica za ostale greške funkcioniše na isti način.

Da bi se deklarisao prilagođeni obrađivač grešaka, koristi se dekorator `@errorhandler`. Smeštaću svoje obrađivače grešaka u novi modul app/errors.py.

> `app/errors.py` : Prilagođeni obrađivači grešaka

```py
from flask import render_template
from app import app, db

@app.errorhandler(404)
def not_found_error(error):
    return render_template('404.html'), 404

@app.errorhandler(500)
def internal_error(error):
    db.session.rollback()
    return render_template('500.html'), 500
```

Funkcije za greške rade veoma slično funkcijama pogleda. Za ove dve greške, vraćam sadržaj njihovih odgovarajućih šablona. Imajte na umu da obe funkcije vraćaju drugu vrednost nakon šablona, što je broj koda greške. Za sve funkcije za pogleda koje sam do sada kreirao, nisam morao da dodam drugu povratnu vrednost jer je podrazumevana vrednost `200` (kod statusa za uspešan odgovor) ono što sam želeo. U ovom slučaju, ovo su stranice sa greškama, pa želim da kod statusa odgovora to odražava.

Rukovalac grešaka za grešku `500` mogao je biti pozvan nakon greške u bazi podataka, što je zapravo bio slučaj sa duplikatom korisničkog imena iznad. Da bih se uverio da neuspele sesije baze podataka ne ometaju pristupe bazi podataka koje je pokrenuo šablon, izdajem vraćanje sesije u prethodno stanje. Ovo resetuje sesiju u čisto stanje.

Evo šablona za grešku 404:

> `app/templates/404.html` : Šablon greške "Nije pronađen“

```html
{% extends "base.html" %}

{% block content %}
    <h1>File Not Found</h1>
    <p><a href="{{ url_for('index') }}">Back</a></p>
{% endblock %}
```

A evo i onog za grešku 500:

> `app/templates/500.html` : Šablon interne greške servera

```html
{% extends "base.html" %}

{% block content %}
    <h1>An unexpected error has occurred</h1>
    <p>The administrator has been notified. Sorry for the inconvenience!</p>
    <p><a href="{{ url_for('index') }}">Back</a></p>
{% endblock %}
```

Oba šablona nasleđuju šablon `base.html`, tako da stranica sa greškom ima isti izgled i osećaj kao i normalne stranice aplikacije.

Da bih registrovao ove obrađivače grešaka u Flask-u, potrebno je da uvezem novi modul `app/errors.py` nakon što je instanca aplikacije kreirana:

> `app/__init__.py` : Rukovaoci grešaka pri uvozu

```py
#...
from app import routes, models, errors
```

Ako podesite `FLASK_DEBUG=0` u vašoj terminalnoj sesiji (ili obrišete `FLASK_DEBUG` promenljivu), a zatim ponovo pokrenete grešku dupliranog korisničkog imena, videćete malo prijateljskiju stranicu sa greškom.

## Slanje grešaka putem e-pošte

Drugi problem sa podrazumevanom obradom grešaka koju pruža Flask jeste to što nema obaveštenja. Tragovi steka za sve greške se ispisuju na terminal, što znači da je potrebno pratiti izlaz servera da bi se otkrile greške. Kada pokrećete aplikaciju tokom razvoja, ovo je sasvim u redu, ali kada se aplikacija instalira na produkcijskom serveru, niko neće gledati izlaz servera, pa je potrebno primeniti robusnije rešenje.

Mislim da je veoma važno zauzeti proaktivan pristup u vezi sa greškama. Ako se greška dogodi u produkcijskoj verziji aplikacije, želim odmah da znam. Dakle, moje prvo rešenje će biti da konfigurišem Flask da mi odmah pošalje imejl nakon greške, sa tragom steka greške u telu imejla.

Prvi korak je dodavanje detalja o serveru e-pošte u konfiguracionu datoteku:

> `config.py` : Konfiguracija imejla

```py
class Config:
    #...
    MAIL_SERVER = os.environ.get('MAIL_SERVER')
    MAIL_PORT = int(os.environ.get('MAIL_PORT') or 25)
    MAIL_USE_TLS = os.environ.get('MAIL_USE_TLS') is not None
    MAIL_USERNAME = os.environ.get('MAIL_USERNAME')
    MAIL_PASSWORD = os.environ.get('MAIL_PASSWORD')
    ADMINS = ['your-email@example.com']
```

Konfiguracione promenljive za e-poštu uključuju server i port, bulovu zastavicu za omogućavanje šifrovanih veza i opciono korisničko ime i lozinku. Pet konfiguracionih promenljivih potiču iz njihovih ekvivalenata u promenljivim okruženja. Ako server e-pošte nije podešen u okruženju, onda ću to koristiti kao znak da greške pri slanju e-pošte treba onemogućiti. Port servera e-pošte takođe može biti dat u promenljivoj okruženja, ali ako nije podešen, koristi se standardni port 25. Akreditivi servera e-pošte se podrazumevano ne koriste, ali se mogu navesti po potrebi. Konfiguraciona `ADMINS` promenljiva je lista adresa e-pošte koje će primati izveštaje o greškama, tako da bi vaša adresa e-pošte trebalo da bude na toj listi.

Flask koristi Pajtonov `logging` paket za pisanje svojih logova, a ovaj paket već ima mogućnost slanja logova putem imejla. Sve što treba da uradim da bi se imejlovi slali o greškama je da dodam instancu `SMTPHandler` objektu `loger`-a Flask, koja je `app.logger`:

> `app/__init__.py` : Zabeleži greške putem imejla

```py
import logging
from logging.handlers import SMTPHandler
#...
if not app.debug:
    if app.config['MAIL_SERVER']:
        auth = None
        if app.config['MAIL_USERNAME'] or app.config['MAIL_PASSWORD']:
            auth = (app.config['MAIL_USERNAME'], app.config['MAIL_PASSWORD'])
        secure = None
        if app.config['MAIL_USE_TLS']:
            secure = ()
        mail_handler = SMTPHandler(
            mailhost=(app.config['MAIL_SERVER'], app.config['MAIL_PORT']),
            fromaddr='no-reply@' + app.config['MAIL_SERVER'],
            toaddrs=app.config['ADMINS'], subject='Microblog Failure',
            credentials=auth, secure=secure)
        mail_handler.setLevel(logging.ERROR)
        app.logger.addHandler(mail_handler)

from app import routes, models, errors
```

Kao što vidite, omogućiću evidenciju imejlova samo kada aplikacija radi bez režima za otklanjanje grešaka, što je naznačeno sa `app.debug` `True`, a takođe i kada imejl server postoji u konfiguraciji.

Podešavanje evidencije imejlova je pomalo zamorno zbog potrebe za rukovanjem opcionim bezbednosnim opcijama koje su prisutne na mnogim imejl serverima. Ali u suštini, gornji kod kreira instancu `SMTPHandler`, podešava njen nivo tako da prijavljuje samo greške, a ne upozorenja, informativne ili poruke o otklanjanju grešaka, i na kraju je prikačuje objektu `app.logger` iz Flaska.

Postoje dva pristupa za testiranje ove funkcije. Najlakši je korišćenje `SMTP` servera za otklanjanje grešaka. Ovo je lažni imejl server koji prihvata imejlove, ali umesto da ih šalje, štampa ih u konzolu. Da biste pokrenuli ovaj server, otvorite drugu terminalnu sesiju, aktivirajte virtuelno okruženje i instalirajte paket `aiosmtpd`:

```sh
(venv) $ pip install aiosmtpd
```

Zatim pokrenite sledeću komandu da biste pokrenuli server za otklanjanje grešaka na e-pošti:

```sh
(venv) $ aiosmtpd -n -c aiosmtpd.handlers.Debugging -l localhost:8025
```

Ova komanda još uvek neće ništa ispisati, već će čekati da se klijenti povežu. Ostavite SMTP server za otklanjanje grešaka pokrenut i vratite se na svoj prvi terminal i konfigurišite svoj imejl server na sledeći način:

```sh
export MAIL_SERVER=localhost
export MAIL_PORT=8025
```

Kao i uvek, koristite `set` umesto `export` ako koristite Microsoft Windows. Uverite se da je promenljiva `FLASK_DEBUG` podešena na 0 ili da uopšte nije podešena, jer aplikacija neće slati imejlove u režimu za otklanjanje grešaka. Pokrenite aplikaciju i još jednom pokrenite grešku SQLAlchemy da biste videli kako terminalna sesija koja pokreće lažni imejl server prikazuje imejl sa kompletnim stekom greške.

Drugi pristup testiranju ove funkcije je konfigurisanje pravog imejl servera. Ispod je konfiguracija za korišćenje imejl servera vašeg Gmail naloga:

```sh
export MAIL_SERVER=smtp.googlemail.com
export MAIL_PORT=587
export MAIL_USE_TLS=1
export MAIL_USERNAME=<your-gmail-username>
export MAIL_PASSWORD=<your-gmail-password>
```

Ako koristite Microsoft Windows, ne zaboravite da koristite `set` umesto `export` u svakoj od gore navedenih izjava.

Bezbednosne funkcije u vašem Gmail nalogu mogu sprečiti aplikaciju da šalje imejlove preko njega, osim ako eksplicitno ne dozvolite "manje bezbednim aplikacijama" pristup vašem Gmail nalogu. O tome možete pročitati ovde, a ako ste zabrinuti za bezbednost svog naloga, možete kreirati sekundarni nalog koji konfigurišete samo za testiranje imejlova ili možete omogućiti manje bezbednim aplikacijama samo privremeno da pokreću ovaj test, a zatim se vratiti na podrazumevane vrednosti.

Još jedna alternativa je korišćenje namenske usluge e-pošte kao što je `SendGrid`, koja vam omogućava da šaljete do 100 e-poruka dnevno sa besplatnim nalogom.

## Prijavljivanje grešaka u datoteku

Primanje grešaka putem imejla je lepo, ali ponekad to nije dovoljno. Postoje neki uslovi greške koji se ne završavaju izuzetkom u Pajtonu i nisu veliki problem, ali i dalje mogu biti dovoljno zanimljivi da se sačuvaju za potrebe debagovanja. Iz tog razloga, takođe ću održavati datoteku dnevnika za aplikaciju.

Da bi se omogućilo evidentiranje zasnovano na datotekama, drugi rukovalac, ovog puta tipa `RotatingFileHandler`, mora biti povezan sa evidentiranjem aplikacije, slično kao i rukovalac imejlom.

> `app/__init__.py` : Prijavljivanje u datoteku

```py
#...
from logging.handlers import RotatingFileHandler
import os
#...
if not app.debug:
    #...

    if not os.path.exists('logs'):
        os.mkdir('logs')
    file_handler = RotatingFileHandler('logs/microblog.log', maxBytes=10240,
                                       backupCount=10)
    file_handler.setFormatter(logging.Formatter(
        '%(asctime)s %(levelname)s: %(message)s [in %(pathname)s:%(lineno)d]'))
    file_handler.setLevel(logging.INFO)
    app.logger.addHandler(file_handler)

    app.logger.setLevel(logging.INFO)
    app.logger.info('Microblog startup')
```

Pišem log datoteku sa imenom `microblog.log` u direktorijumu `logs`, koji kreiram ako već ne postoji.

Klasa `RotatingFileHandler` je dobra jer rotira logove, osiguravajući da log datoteke ne postanu prevelike kada aplikacija radi duže vreme. U ovom slučaju ograničavam veličinu log datoteke na 10 KB i čuvam poslednjih deset log datoteka kao rezervnu kopiju.

Klasa `logging.Formatter` pruža prilagođeno formatiranje za poruke dnevnika. Pošto se ove poruke čuvaju u datoteci, želim da sadrže što više informacija. Zato koristim format koji uključuje vremensku oznaku, nivo evidentiranja, poruku i izvornu datoteku i broj linije odakle potiče unos u dnevnik.

Da bi evidentiranje bilo korisnije, takođe snižavam nivo evidentiranja na `INFO` kategoriju, kako u programu za evidentiranje aplikacija, tako i u programu za upravljanje evidentiranjem datoteka. U slučaju da niste upoznati sa kategorijama evidentiranja, to su `DEBUG`, `INFO`, `WARNING`, `ERROR` i `CRITICAL`, u rastućem redosledu ozbiljnosti.

Kao prva zanimljiva upotreba log datoteke, server upisuje red u logove svaki put kada se pokrene. Kada se ova aplikacija pokrene na produkcijskom serveru, ovi unosi u log će vam reći kada je server ponovo pokrenut.

## Ispravljanje greške dupliranog korisničkog imena

Predugo sam koristio grešku sa dupliranjem korisničkih imena. Sada kada sam vam pokazao kako da pripremite aplikaciju za rukovanje ovim vrstama grešaka, mogu da je popravim.

Ako se sećate, `RegistrationForm` već implementira validaciju korisničkih imena, ali zahtevi forme za uređivanje su malo drugačiji. Tokom registracije, moram da se uverim da korisničko ime uneto u formu ne postoji u bazi podataka. Na formi za uređivanje profila moram da uradim istu proveru, ali sa jednim izuzetkom. Ako korisnik ostavi originalno korisničko ime netaknuto, onda bi validacija trebalo da to dozvoli, jer je to korisničko ime već dodeljeno tom korisniku. Ispod možete videti kako sam implementirao validaciju korisničkog imena za ovu formu:

> `app/forms.py` : Potvrdite korisničko ime u formi za uređivanje profila.

```py
class EditProfileForm(FlaskForm):
    username = StringField('Username', validators=[DataRequired()])
    about_me = TextAreaField('About me', validators=[Length(min=0, max=140)])
    submit = SubmitField('Submit')

    def __init__(self, original_username, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.original_username = original_username

    def validate_username(self, username):
        if username.data != self.original_username:
            user = db.session.scalar(sa.select(User).where(
                User.username == username.data))
            if user is not None:
                raise ValidationError('Please use a different username.')
```

Implementacija je u prilagođenoj metodi validacije, ali postoji preopterećeni konstruktor koji prihvata originalno korisničko ime kao argument. Ovo korisničko ime se čuva kao promenljiva instance i proverava u metodi `validate_username()`. Ako je korisničko ime uneto u formu isto kao i originalno korisničko ime, onda nema razloga da se proverava baza podataka za duplikate.

Da bih koristio ovu novu metodu validacije, potrebno je da dodam originalni argument korisničkog imena u funkciju ogleda, gde se kreira objekat forme:

> `app/routes.py` : Potvrdite korisničko ime u formi za uređivanje profila.

```py
@app.route('/edit_profile', methods=['GET', 'POST'])
@login_required
def edit_profile():
    form = EditProfileForm(current_user.username)
    #...
```

Sada je greška ispravljena i duplikati u formi za uređivanje profila će biti sprečeni u većini slučajeva. Ovo nije savršeno rešenje, jer možda neće funkcionisati kada dva ili više procesa istovremeno pristupaju bazi podataka. U toj situaciji, uslov trke može prouzrokovati da validacija prođe, ali trenutak kasnije, kada se pokuša preimenovanje, baza podataka je već bila izmenjena od strane drugog procesa i ne može se preimenovati korisnik. Ovo je donekle malo verovatno, osim za veoma zauzete aplikacije koje imaju mnogo serverskih procesa, tako da se za sada neću brinuti o tome.

U ovom trenutku možete pokušati da reprodukujete grešku još jednom da biste videli kako je novi metod validacije obrasca sprečava.

## Trajno omogućavanje režima za otklanjanje grešaka

Flaskov režim za otklanjanje grešaka je toliko koristan da ćete možda želeti da ga podrazumevano uključite. To se može uraditi dodavanjem FLASK_DEBUG promenljive okruženja u `.flaskenv` datoteku.

`.flaskenv` : Promenljive okruženja za komandu flask

```sh
FLASK_APP=microblog.py
FLASK_DEBUG=1
```

Ovom promenom, režim za otklanjanje grešaka će biti omogućen kada pokrenete server pomoću `flask run` komande.
