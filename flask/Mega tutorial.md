
# Mega vodič za Flask

## Poglavlje 1 - Zdravo svete

U ovom prvom poglavlju, naučićete kako da podesite Flask projekat. Do kraja ovog poglavlja imaćete jednostavnu Flask veb aplikaciju koja radi na vašem računaru!
Svi primeri koda predstavljeni u ovoj knjizi nalaze se na GitHub repozitorijumu. Preuzimanje koda sa GitHub-a može vam uštedeti mnogo kucanja, ali vam toplo preporučujem da sami otkucate kod, barem za prvih nekoliko poglavlja. Kada se bolje upoznate sa Flask-om i primerom aplikacije, možete pristupiti kodu direktno sa GitHub-a ako kucanje postane previše zamorno.

Na početku svakog poglavlja, daću vam tri GitHub linka koji vam mogu biti korisni dok radite kroz poglavlje. Link `Browse` će otvoriti GitHub repozitorijum za Microblog na mestu gde su dodate izmene za poglavlje koje čitate, bez uključivanja bilo kakvih izmena uvedenih u budućim poglavljima. `Zip` link je link za preuzimanje zip datoteke koja uključuje celu aplikaciju, uključujući i izmene u poglavlju. `Diff` link će otvoriti grafički prikaz svih izmena koje su napravljene u poglavlju koje ćete uskoro pročitati.

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.1), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.1.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.0...v0.1).

### Instaliranje Pajtona

Ako nemate instaliran Pajton na računaru, instalirajte ga sada. Ako vaš operativni sistem ne pruža Pajton paket, možete preuzeti instalater sa zvanične veb stranice Pajtona. Ako koristite Majkrosoft Vindous zajedno sa WSL-om ili Cygwin-om, imajte na umu da nećete koristiti nativnu verziju Pajtona za Vindous, već verziju prilagođenu Juniksu koju treba da preuzmete iz Ubuntua (ako koristite WSL) ili iz Cygwin-a.
Da biste bili sigurni da je vaša instalacija Pajtona funkcionalna, možete otvoriti prozor terminala i otkucati `python3`, ili ako to ne funkcioniše, samo `python`. Evo šta biste trebali očekivati:

```sh
$ python3
Python 3.12.0 (main, Oct  5 2023, 10:46:39) [GCC 11.4.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> _
```

Pajton interpreter sada čeka na interaktivnom promptu, gde možete unositi Pajton izraze. U budućim poglavljima ćete saznati za šta je ovaj interaktivni prompt koristan. Ali za sada, potvrdili ste da je Pajton instaliran na vašem sistemu. Da biste izašli iz interaktivnog prompta, možete otkucati `exit()` i pritisnuti `Enter`. Na verzijama Pajtona za Linuks i Mak OS X, interpreter možete napustiti i pritiskom na `Ctrl-D`. Na Vindousu, prečica za izlaz je `Ctrl-Z` praćena `Enterom`.

### Instaliranje Flaska

Sledeći korak je instaliranje Flask-a, ali pre nego što pređem na to, želim da vam kažem o najboljim praksama povezanim sa instaliranjem Python paketa.

U Pajtonu, paketi kao što je Flask dostupni su u javnom repozitorijumu, odakle ih svako može preuzeti i instalirati. Zvanični repozitorijum Pajton paketa se zove `PyPI`, što je skraćenica od Python Package Index (neki ljudi ovaj repozitorijum nazivaju i "prodavnica sira"). Instaliranje paketa iz `PyPI` je veoma jednostavno, jer Pajton dolazi sa alatom pod nazivom `pip` koji obavlja ovaj posao.
Da biste instalirali paket na vaš računar, koristite:

```sh
pip install <package-name>
```

Zanimljivo je da ovaj metod instaliranja paketa neće funkcionisati u većini slučajeva. Ako je vaš Pajton interpreter instaliran globalno za sve korisnike vašeg računara, verovatno je da vaš redovni korisnički nalog neće imati dozvolu da ga modifikuje, tako da je jedini način da gornja komanda funkcioniše da je pokrenete sa administratorskog naloga. Ali čak i bez te komplikacije, razmislite šta se dešava kada instalirate paket na ovaj način. Alat `pip` će preuzeti paket sa `PyPI`-ja, a zatim ga dodati vašoj Pajton instalaciji. Od tog trenutka, svaka Pajton skripta koju imate na svom sistemu imaće pristup ovom paketu. Zamislite situaciju u kojoj ste završili veb aplikaciju koristeći verziju 2 Flaska, koja je bila najnovija verzija Flaska kada ste počeli, ali je sada zamenjena verzijom 3. Sada želite da pokrenete drugu aplikaciju, za koju biste želeli da koristite verziju 3, ali ako nadogradite verziju 1 koju ste instalirali, rizikujete da pokvarite svoju stariju aplikaciju. Da li vidite problem? Bilo bi idealno kada bi bilo moguće imati instaliranu i dostupnu Flask verziju 2 za vašu staru aplikaciju, a istovremeno instalirati Flask verziju 3 za vašu novu.

Da bi se rešio problem održavanja različitih verzija paketa za različite aplikacije, Pajton koristi koncept virtuelnih okruženja. Virtuelno okruženje je potpuna kopija Pajton interpretera. Kada instalirate pakete u virtuelnom okruženju, sistemski Pajton interpreter nije pogođen, već samo kopija. Dakle, rešenje za potpunu slobodu instaliranja bilo koje verzije vaših paketa za svaku aplikaciju jeste korišćenje različitog virtuelnog okruženja za svaku aplikaciju. Virtuelna okruženja imaju dodatnu prednost što su u vlasništvu korisnika koji ih kreira, tako da im nije potreban administratorski nalog.

Počnimo tako što ćemo kreirati direktorijum u kome će se projekat nalaziti. Nazvaću ovaj direktorijum mikroblog, pošto je to naziv aplikacije:

```sh
mkdir microblog
cd microblog
```

Podrška za virtuelna okruženja je uključena u sve novije verzije Pajtona, tako da je sve što treba da uradite da biste ga kreirali sledeće:

```sh
python3 -m venv .venv
```

Ovom komandom tražim od Pajtona da pokrene `venv` paket, koji kreira virtuelno okruženje pod nazivom `.venv`. Prvi `venv` u komandi je argument opcije `-m`, što je naziv paketa virtuelnog okruženja Pajtona, a drugi je naziv virtuelnog okruženja koji ću koristiti za ovo konkretno okruženje. Ako vam je ovo zbunjujuće, možete zameniti drugi argument `.venv` drugim imenom koje želite da dodelite svom virtuelnom okruženju. Generalno, ja kreiram svoja virtuelna okruženja sa imenom `.venv` u direktorijumu projekta, tako da kad god uđem u projekat pronađem njegovo odgovarajuće virtuelno okruženje.

Imajte na umu da ćete u nekim operativnim sistemima možda morati da koristite `python` umesto `python3` u gornjoj komandi. Neke instalacije koriste `python` za izdanja Pajtona 2.x i `python3` za izdanja 3.x, dok se druge mapiraju na izdanja 3.x i uopšte nemaju `python` komandu.

Nakon što se komanda završi, imaćete direktorijum pod nazivom `.venv` gde se čuvaju datoteke virtuelnog okruženja.

Sada morate da kažete sistemu da želite da koristite ovo virtuelno okruženje, a to radite tako što ga aktivirate. Da biste aktivirali svoje novo virtuelno okruženje, koristite sledeću komandu:

```sh
source.venv/bin/activate
(.venv) $ _
```

Ako koristite komandnu liniju operativnog sistema Microsoft Windows, komanda za aktivaciju je malo drugačija:

```sh
.venv\Scripts\activate
(.venv) $ _
```

Ako koristite Windows, ali koristite PowerShell umesto komandne linije, onda postoji još jedna komanda za aktivaciju koju bi trebalo da koristite:

```sh
$.venv\Scripts\Activate.ps1
(.venv) $ _
```

Kada aktivirate virtuelno okruženje, konfiguracija vaše terminalne sesije se menja tako da se Pajton interpreter koji se čuva u njemu poziva kada kucate `python`. Takođe, terminalski prompt se menja tako da uključuje ime aktiviranog virtuelnog okruženja. Promene napravljene u vašoj terminalnoj sesiji su sve privremene i privatne za tu sesiju, tako da se neće sačuvati kada zatvorite prozor terminala. Ako radite sa više otvorenih prozora terminala istovremeno, sasvim je u redu da imate različita virtuelna okruženja aktivirana na svakom od njih.

Sada kada ste kreirali i aktivirali virtuelno okruženje, konačno možete instalirati Flask u njega:

```sh
(.venv) $ pip install flask
```

Ako želite da potvrdite da je u vašem virtuelnom okruženju sada instaliran Flask, možete pokrenuti Python interpreter i uvesti Flask u njega:

```sh
>>> import flask
>>> _
```

Ako vam ova izjava ne da nikakve greške, možete sebi čestitati, jer je Flask instaliran i spreman za upotrebu.

Imajte na umu da gore navedene komande za instalaciju ne određuju koju verziju Flaska želite da instalirate. Podrazumevano, kada nije navedena verzija, instalira se najnovija verzija dostupna u repozitorijumu paketa. Ovaj tutorijal je dizajniran za verziju 3 Flaska, ali bi trebalo da radi i sa verzijom 2. Gore navedena komanda će instalirati najnoviju verziju 3.x, koja bi trebalo da bude odgovarajuća za većinu korisnika. Ako iz bilo kog razloga više volite da pratite ovaj tutorijal na 2.x izdanju Flaska, možete koristiti sledeću komandu da biste instalirali najnoviju verziju 2.x:

```sh
(venv) $ pip install "flask<3" "werkzeug<3"
```

### Aplikacija u flasku "Zdravo svete"

Ako odete na stranicu za brzi početak Flaska, dočekaće vas veoma jednostavan primer aplikacije koji ima samo pet linija koda. Umesto da ponavljam taj trivijalni primer, pokazaću vam malo razrađeniji koji će vam dati dobru osnovnu strukturu za pisanje većih aplikacija.

Aplikacija će postojati u paketu. U Pajtonu, poddirektorijum koji sadrži datoteku `__init__.py` smatra se paketom i može se uvesti. Kada uvezete paket, `__init__.py` se izvršava i definiše koje simbole paket izlaže spoljnom svetu.

Hajde da napravimo paket pod nazivom `app`, koji će hostovati aplikaciju. Uverite se da ste u direktorijumu `mikrobloga`, a zatim pokrenite sledeću komandu:

```sh
(venv) $ mkdir app
```

Datoteka `__init__.py` za app paket će sadržati sledeći kod:

> `app/__init__.py` : Instanca Flask aplikacije

```py
from flask import Flask

app = Flask(__name__)

from app import routes
```

Gore navedeni skript kreira objekat aplikacije kao instancu klase `Flask` uvezene iz flask paketa. Promenljiva `__name__` koja se prosleđuje klasi `Flask` je unapred definisana promenljiva u Pajtonu, koja je podešena na ime modula u kojem se koristi.

Flask koristi lokaciju modula koji je ovde prosleđen kao početnu tačku kada treba da učita povezane resurse kao što su datoteke šablona, o čemu ću govoriti u 2. poglavlju. U sve praktične svrhe, prosleđivanje `__name__` će skoro uvek konfigurisati Flask na ispravan način. Aplikacija zatim uvozi `routes` modul, koji još ne postoji.

Jedan aspekt koji u početku može delovati zbunjujuće jeste da postoje dva entiteta sa imenom `app`. Paket `app` je definisan direktorijumom aplikacije i skriptom `__init__.py` i na njega se referencira u `from app import routes` izrazu. Promenljiva app je definisana kao instanca klase `Flask` u skripti `__init__.py`, što je čini članom paketa `app`.

Još jedna osobenost je to što `routes` se modul uvozi na dnu, a ne na vrhu skripte, kao što se uvek radi. Donji uvoz je dobro poznato rešenje koje izbegava kružni uvoz, čest problem sa Flask aplikacijama. Videćete da `routes` modul treba da uveze `app` promenljivu definisanu u ovoj skripti, tako da stavljanje jednog od recipročnih uvoza na dno izbegava grešku koja nastaje usled međusobnih referenci između ove dve datoteke.

Šta se onda nalazi u `routes` modulu? Rute obrađuju različite URL-ove koje aplikacija podržava. U Flasku, obrađivači ruta aplikacije su napisani kao Pajton funkcije, koje se nazivaju funkcije `pogleda`. Funkcije `pogleda` su mapirane na jedan ili više URL-ova ruta tako da Flask zna koju logiku da izvrši kada klijent zahteva dati URL.

Evo prve funkcije pogleda za ovu aplikaciju, koju treba da napišete u novom modulu pod nazivom `app/routes.py` :

> `app/routes.py` : Ruta početne stranice

```py
from app import app

@app.route('/')
@app.route('/index')
def index():
    return "Hello, World!"
```

Ova funkcija pogleda je zapravo prilično kratka, ona samo vraća pozdrav kao string.

Dva čudna `@app.route` reda iznad funkcije su dekoratori, jedinstvena karakteristika Pajton jezika. Dekorator modifikuje funkciju koja sledi posle njega. Uobičajeni obrazac sa dekoratorima je da se koriste za registrovanje funkcija kao povratnih poziva za određene događaje. U ovom slučaju, dekorator `@app.route` kreira vezu između URL-a datog kao argument i funkcije. U ovom primeru postoje dva dekoratora, koji povezuju URL-ove `/` i `/index` sa ovom funkcijom. To znači da kada veb pregledač zahteva bilo koji od ova dva URL-a, Flask će pozvati ovu funkciju i proslediti njenu povratnu vrednost nazad pregledaču kao odgovor. Ako vam ovo još uvek nema potpunog smisla, uskoro će imati, kada pokrenete ovu aplikaciju.

Da biste završili aplikaciju, potreban vam je Pajton skript na najvišem nivou koji definiše instancu Flask aplikacije. Nazovimo ovu skriptu `microblog.py` i definišimo je kao jednu liniju koja uvozi instancu aplikacije:

> `microblog.py` : Glavni modul aplikacije

```py
from app import app
```

Sećate se dva app entiteta? Ovde možete videti oba zajedno u istoj rečenici. Instanca Flask aplikacije se zove `app` i član je paketa `app`. `from app import app` izjava uvozi `app` promenljivu koja je član paketa `app`. Ako vam je ovo zbunjujuće, možete preimenovati paket ili promenljivu u nešto drugo.

Samo da biste bili sigurni da sve radite ispravno, ispod možete videti dijagram strukture projekta do sada:

```sh
microblog/
 .venv/
  app/
    __init__.py
    routes.py
  microblog.py
```

Verovali ili ne, ova prva verzija aplikacije je sada završena! Međutim, pre pokretanja, Flasku je potrebno reći kako da je uveze, podešavanjem `FLASK_APP` promenljive okruženja:

```sh
(.venv) $ export FLASK_APP=microblog.py
```

Ako koristite komandnu liniju operativnog sistema Microsoft Windows, koristite je `set` umesto `export` u gornjoj komandi.

Da li ste spremni da budete oduševljeni? Možete pokrenuti svoju prvu veb aplikaciju tako što ćete otkucati komandu `flask run`, kao što je prikazano ispod:

```sh
(.venv) $ flask run
 * Serving Flask app 'microblog.py' (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Šta se ovde desilo? `flask run` komanda će tražiti instancu Flask aplikacije u modulu na koji se poziva `FLASK_APP` promenljiva okruženja, što je u ovom slučaju `microblog.py`. Komanda podešava veb server koji je konfigurisan da prosleđuje zahteve ovoj aplikaciji.

Nakon što se server inicijalizuje, čekaće klijentske veze. Izlaz iz `flask run` pokazuje da server radi na IP adresi `127.0.0.1`, što je uvek adresa vašeg računara. Ova adresa je toliko česta da ima i jednostavnije ime koje ste možda već videli: `localhost`. Mrežni serveri osluškuju veze na određenom broju porta. Aplikacije raspoređene na produkcionim veb serverima obično osluškuju port `443`, ili ponekad `80` ako ne implementiraju šifrovanje, ali pristup ovim portovima zahteva administratorska prava. Pošto se ova aplikacija pokreće u razvojnom okruženju, Flask koristi port `5000`. Sada otvorite veb pregledač i unesite sledeći URL u polje za adresu:

<http://localhost:5000/>

Alternativno, možete koristiti ovu drugu URL adresu:

<http://localhost:5000/index>

Da li vidite mapiranja ruta aplikacije u akciji? Prvi URL se mapira na `/`, dok se drugi mapira na `/index`. Obe rute su povezane sa jedinom funkcijom pogleda u aplikaciji, tako da proizvode isti izlaz, a to je string koji funkcija vraća. Ako unesete bilo koji drugi URL, dobićete grešku, jer aplikacija prepoznaje samo ova dva URL-a.

Kada završite sa igranjem na serveru, možete jednostavno pritisnuti `Ctrl-C` da biste ga zaustavili.

Čestitamo, završili ste prvi veliki korak da postanete veb programer!

Da li ste imali problema sa pokretanjem aplikacije Flask? Na većini računara port `5000` je dostupan, ali postoji mogućnost da vaš računar već pokreće aplikaciju koja koristi ovaj port, u kom slučaju će komanda `flask run` proizvesti grešku `"adresa je već u upotrebi"` ili sličnu. Ako koristite Macintosh računar, neke verzije macOS-a pokreću uslugu pod nazivom "Airplay Receiver" na ovom portu. Ako ne možete da shvatite kako da uklonite softver koji koristi port `5000`, možete pokušati da pokrenete Flask na drugom portu. Na primer, evo kako da pokrenete server na portu `5001`:

```sh
(.venv) $ flask run --port 5001
```

Pre nego što završim ovo poglavlje, pokazaću vam još jednu stvar. Pošto se promenljive okruženja ne pamte u svim terminalnim sesijama, može vam biti dosadno da uvek morate da podešavate `FLASK_APP` promenljivu okruženja kada otvorite novi prozor terminala da biste radili na svojoj Flask aplikaciji. Ali srećom, Flask vam omogućava da registrujete promenljive okruženja koje želite da se automatski koriste kada pokrenete komandu `flask`. Da biste koristili ovu opciju, morate da instalirate paket `python-dotenv` :

```sh
(.venv) $ pip install python-dotenv
```

Sada možete samo da napišete ime i vrednost promenljive okruženja u datoteku pod nazivom `.flaskenv` koja se nalazi u direktorijumu najvišeg nivoa projekta:

> `.flaskenv` : Promenljive okruženja za komandu flask

```sh
FLASK_APP=microblog.py
```

Komanda `flask` će prvo potražiti `.flaskenv` datoteku i uvesti sve promenljive definisane u njoj tačno kao da su definisane u okruženju.

## Poglavlje 2 - Šabloni

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

### Šta su šabloni?

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

> app/routes.py : Koristite funkciju render_template()

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

#### Uslovni iskazi

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

#### Petlje

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

### Nasleđivanje šablona

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
