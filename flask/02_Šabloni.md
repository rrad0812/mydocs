# Poglavlje 2 - Šabloni

Nakon završetka Poglavlja 1, trebalo bi da imate jednostavnu, ali funkcionalnu veb aplikaciju koja ima sledeću strukturu datoteka:

```sh
microblog\
  venv\
  app\
    __init__.py
    routes.py
  microblog.py
```

Da biste pokrenuli aplikaciju, podesite `FLASK_APP=microblog.py` u vašoj terminalnoj sesiji (ili još bolje, dodajte `.flaskenv` datoteku sa ovom promenljivom), a zatim izvršite `flask run`. Ovo pokreće veb server sa aplikacijom, koju možete otvoriti tako što ćete uneti URL adresu <http://localhost:5000/> u adresnu traku vašeg veb pregledača.

U ovom poglavlju ćete nastaviti rad na istoj aplikaciji, a posebno ćete naučiti kako da generišete složenije veb stranice koje imaju kompleksnu strukturu i mnogo dinamičkih komponenti. Ako vam nešto u vezi sa aplikacijom ili tokom razvoja do sada nije jasno, ponovo pregledajte 1. poglavlje pre nego što nastavite.

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.2), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.2.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.1...v0.2).

## Šta su šabloni?

Želim da početna stranica moje aplikacije za mikroblog-ovanje ima naslov koji pozdravlja korisnika. Za sada ću ignorisati činjenicu da aplikacija još uvek nema koncept korisnika, jer će to doći kasnije. Umesto toga, koristiću lažnog korisnika, koga ću implementirati kao Pajton rečnik, na sledeći način:

```py
user = {'username': 'Miguel'}
```

Kreiranje lažnih objekata je korisna tehnika koja vam omogućava da se koncentrišete na jedan deo aplikacije, a da ne morate da brinete o drugim delovima sistema koji još ne postoje. Želim da dizajniram početnu stranicu svoje aplikacije i ne želim da me činjenica da nemam uspostavljen korisnički sistem ometa, pa jednostavno pravim korisnički objekat kako bih mogao da nastavim.

Funkcija pogleda u aplikaciji vraća jednostavan string. Ono što sada želim da uradim jeste da proširim taj vraćeni string u kompletnu HTML stranicu, možda nešto poput ovoga:

> `app/routes.py` : Vraća kompletnu HTML stranicu iz funkcije view

```py
from app import app

@app.route('/')
@app.route('/index')
def index():
    user = {'username': 'Miguel'}
    return '''
        <html>
            <head>
                <title>Home Page - Microblog</title>
            </head>
            <body>
                <h1>Hello, ''' + user['username'] + '''!</h1>
            </body>
        </html>'''
```

Ako niste upoznati sa HTML-om, preporučujem vam da pročitate HTML Markup na Vikipediji za kratak uvod.

Ažurirajte funkciju pogleda kao što je prikazano gore i ponovo pokrenite aplikaciju da biste videli kako izgleda u vašem pregledaču.

Nadam se da se slažete sa mnom da rešenje koje je gore korišćeno za isporuku HTML-a pregledaču nije dobro. Razmislite koliko će složen kod u ovoj funkciji prikaza postati kada dodate blog postove od korisnika, koji će se stalno menjati. Aplikacija će takođe imati više funkcija prikaza koje će biti povezane sa drugim URL-ovima, pa zamislite da jednog dana odlučim da promenim raspored ove aplikacije i da moram da ažuriram HTML u svakoj funkciji prikaza. Ovo očigledno nije opcija koja će se skalirati kako aplikacija raste.

Kada biste mogli da odvojite logiku vaše aplikacije od rasporeda ili prezentacije vaših veb stranica, onda bi stvari bile mnogo bolje organizovane, zar ne? Čak biste mogli da angažujete veb dizajnera da kreira odličan veb sajt dok vi kodirate logiku aplikacije u Pajtonu.

Šabloni pomažu u postizanju ovog razdvajanja između prezentacije i poslovne logike. U Flasku, šabloni se pišu kao odvojene datoteke, koje se čuvaju u fascikli šablona koja se nalazi unutar paketa aplikacije. Nakon što se uverite da ste u direktorijumu mikroblog-a, kreirajte direktorijum u kome će se šabloni čuvati:

```sh
(venv) $ mkdir app/templates
```

Ispod možete videti vaš prvi šablon, koji je po funkcionalnosti sličan HTML stranici koju vraća gornja funkcija pogleda `index()`. Napišite ovu datoteku u `app/templates/index.html` :

> `app/templates/index.html` : Šablon glavne stranice

```html
<!doctype html>
<html>
    <head>
        <title>{{ title }} - Microblog</title>
    </head>
    <body>
        <h1>Hello, {{ user.username }}!</h1>
    </body>
</html>
```

Ovo je standardna, kratka HTML stranica. Jedina zanimljiva stvar na ovoj stranici je to što postoji nekoliko rezervisanih mesta za dinamički sadržaj, zatvorenih u `{{...}}` rezervisana mesta. Ova rezervisana mesta predstavljaju delove stranice koji su promenljivi i biće poznati tek tokom izvršavanja.

Sada kada je prezentacija stranice prebačena na HTML šablon, funkcija pogleda može se pojednostaviti:

> `app/routes.py` : Koristite funkciju `render_template()`

```py
from flask import render_template
from app import app

@app.route('/')
@app.route('/index')
def index():
    user = {'username': 'Miguel'}
    return render_template('index.html', title='Home', user=user)
```

Ovo izgleda mnogo bolje, zar ne? Isprobajte ovu novu verziju aplikacije da vidite kako šablon funkcioniše. Kada se stranica učita u pregledač, možda ćete želeti da pogledate izvorni HTML i uporedite ga sa originalnim šablonom.

Operacija koja konvertuje šablon u kompletnu HTML stranicu naziva se renderovanje. Da bih renderovao šablon, morao sam da uvezem funkciju koja dolazi sa Flask frejmvorkom pod nazivom `render_template()`. Ova funkcija uzima naziv datoteke šablona i promenljivu listu argumenata šablona i vraća isti šablon, ali sa svim rezervisanim mestima u njemu zamenjenim stvarnim vrednostima.

Funkcija `render_template()` poziva Ninja mehanizam šablona koji dolazi u paketu sa Flask frejmvorkom. Nindža zamenjuje `{{...}}` blokove odgovarajućim vrednostima, datim argumentima navedenim u `render_template()` pozivu.

### Uslovni iskazi

Videli ste kako Nindža zamenjuje rezervisana mesta stvarnim vrednostima tokom renderovanja, ali ovo je samo jedna od mnogih moćnih operacija koje Nindža podržava u šablonskim datotekama. Na primer, šabloni takođe podržavaju kontrolne iskaze, date unutar `{%... %}` blokova. Sledeća verzija šablona `index.html` dodaje uslovni iskaz:

> `app/templates/index.html` : Uslovna izjava u šablonu

```html
<!doctype html>
<html>
    <head>
        {% if title %}
        <title>{{ title }} - Microblog</title>
        {% else %}
        <title>Welcome to Microblog!</title>
        {% endif %}
    </head>
    <body>
        <h1>Hello, {{ user.username }}!</h1>
    </body>
</html>
```

Sada je šablon malo pametniji. Ako funkcija pogleda zaboravi da prosledi vrednost za `title` promenljivu čuvara mesta, onda će umesto prikazivanja praznog naslova šablon dati podrazumevani. Možete isprobati kako ovaj uslov funkcioniše uklanjanjem argumenta `title` u `render_template()` pozivu funkcije pogleda.

### Petlje

Prijavljeni korisnik će verovatno želeti da vidi nedavne objave povezanih korisnika na početnoj stranici, pa ću sada proširiti aplikaciju da to podrži.

Još jednom, oslanjaću se na praktičan trik sa lažnim objektom da bih kreirao neke korisnike i neke objave za prikaz:

> `app/routes.py` : Funkcija prikazivanja lažnih objava

```py
from flask import render_template
from app import app

@app.route('/')
@app.route('/index')
def index():
    user = {'username': 'Miguel'}
    posts = [
        {
            'author': {'username': 'John'},
            'body': 'Beautiful day in Portland!'
        },
        {
            'author': {'username': 'Susan'},
            'body': 'The Avengers movie was so cool!'
        }
    ]
    return render_template('index.html', title='Home', user=user, posts=posts)
```

Da bih predstavio korisničke objave, koristim listu, gde je svaki element rečnik koji ima `author` i `body` polja. Kada budem mogao da implementiram korisnike i blog objave zapravo, pokušaću da sačuvam ova imena polja koliko god je to moguće, kako bi sav rad koji obavljam na dizajniranju i testiranju šablona početne stranice koristeći ove lažne objekte i dalje bio validan kada uvedem stvarne korisnike i objave.

Na strani šablona moram da rešim novi problem. Lista objava može imati bilo koji broj elemenata, na funkciji prikaza je da odluči koliko će objava biti prikazano na stranici. Šablon ne može da pravi nikakve pretpostavke o tome koliko objava postoji, tako da mora biti spreman da prikaže onoliko objava koliko prikaz pošalje na generički način.

Za ovu vrstu problema, Nindža nudi `for` kontrolnu strukturu:

> `app/templates/index.html` : ciklus `for` u šablonu

```html
<!doctype html>
<html>
    <head>
        {% if title %}
        <title>{{ title }} - Microblog</title>
        {% else %}
        <title>Welcome to Microblog</title>
        {% endif %}
    </head>
    <body>
        <h1>Hi, {{ user.username }}!</h1>
        {% for post in posts %}
        <div><p>{{ post.author.username }} says: <b>{{ post.body }}</b></p></div>
        {% endfor %}
    </body>
</html>
```

Jednostavno, zar ne? Isprobajte ovu novu verziju aplikacije i obavezno se poigrajte sa dodavanjem više sadržaja na listu objava da biste videli kako se šablon prilagođava i uvek prikazuje sve objave koje funkcija pogleda šalje.

## Nasleđivanje šablona

Većina veb aplikacija danas ima navigacionu traku na vrhu stranice sa nekoliko često korišćenih linkova, kao što je link za uređivanje profila, za prijavu, odjavu itd. Mogu lako dodati navigacionu traku šablonu `index.html` sa još malo HTML-a, ali kako aplikacija raste, biće mi potrebna ista navigaciona traka i na drugim stranicama. Zaista ne želim da moram da održavam nekoliko kopija navigacione trake u mnogim HTML šablonima, dobra je praksa da se ne ponavljate ako je to moguće.

Nindža ima funkciju nasleđivanja šablona koja se posebno bavi ovim problemom. U suštini, ono što možete da uradite jeste da premestite delove rasporeda stranice koji su zajednički za sve šablone u osnovni šablon, iz kojeg su izvedeni svi ostali šabloni.

Dakle, ono što ću sada uraditi jeste da definišem osnovni šablon pod nazivom `base.html` koji uključuje jednostavnu navigacionu traku i logiku naslova koju sam ranije implementirao. Potrebno je da napišete sledeći šablon u datoteci `app/templates/base.html` :

> `app/templates/base.html` : Osnovni šablon sa navigacionom trakom

```html
<!doctype html>
<html>
    <head>
      {% if title %}
      <title>{{ title }} - Microblog</title>
      {% else %}
      <title>Welcome to Microblog</title>
      {% endif %}
    </head>
    <body>
        <div>Microblog: <a href="/index">Home</a></div>
        <hr>
        {% block content %}{% endblock %}
    </body>
</html>
```

U ovom šablonu sam koristio `block` kontrolnu naredbu da definišem mesto gde izvedeni šabloni mogu sami sebe da ubace. Blokovima se daje jedinstveno ime, na koje izvedeni šabloni mogu da se pozivaju kada pružaju svoj sadržaj.

Sa osnovnim šablonom, sada mogu da pojednostavim `index.html` tako što ću ga nasleđivati od `base.html` :

> `app/templates/index.html` : Nasledidjivanje od osnovnog šablona

```html
{% extends "base.html" %}

{% block content %}
    <h1>Hi, {{ user.username }}!</h1>
    {% for post in posts %}
    <div><p>{{ post.author.username }} says: <b>{{ post.body }}</b></p></div>
    {% endfor %}
{% endblock %}
```

Pošto će šablon `base.html` sada voditi računa o opštoj strukturi stranice, uklonio sam sve te elemente iz `index.html` i ostavio samo deo sadržaja. `extends` izjava uspostavlja vezu nasleđivanja između dva šablona, tako da Nindža zna da kada se zatraži prikazivanje `index.html`, treba da ga ugradi unutar `base.html`. Dva šablona imaju odgovarajuće `block` izjave sa imenom `content`, i tako Nindža zna kako da kombinuje dva šablona u jedan. Sada, ako treba da napravim dodatne stranice za aplikaciju, mogu ih kreirati kao izvedene šablone iz istog šablona `base.html`, i tako mogu imati sve stranice aplikacije koje dele isti izgled i osećaj bez dupliranja.
