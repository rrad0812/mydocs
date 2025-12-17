
# Poglavlje 6 - Stranica profila i avatari

Ovo poglavlje će biti posvećeno dodavanju stranica korisničkog profila u aplikaciju. Stranica korisničkog profila je stranica na kojoj su prikazane informacije o korisniku, često sa informacijama koje su sami korisnici uneli. Pokazaću vam kako da dinamički generišete stranice profila za sve korisnike, a zatim ću dodati mali uređivač profila koji korisnici mogu koristiti za unos svojih informacija.

Linkovi ka GitHubu za ovo poglavlje su: [Browse](https://github.com/miguelgrinberg/microblog/tree/v0.6), [Zip](https://github.com/miguelgrinberg/microblog/archive/v0.6.zip), [Diff](https://github.com/miguelgrinberg/microblog/compare/v0.5...v0.6).

## Stranica korisničkog profila

Da bismo kreirali stranicu korisničkog profila, dodajmo rutu /user/<korisničko_ime > aplikaciji.

> `app/routes.py` : Funkcija pogleda korisničkog profila

```py
@app.route('/user/<username>')
@login_required
def user(username):
    user = db.first_or_404(sa.select(User).where(User.username == username))
    posts = [
        {'author': user, 'body': 'Test post #1'},
        {'author': user, 'body': 'Test post #2'}
    ]
    return render_template('user.html', user=user, posts=posts)
```

Dekorator `@app.route` koji sam koristio za deklarisanje ove funkcije pogleda izgleda malo drugačije od prethodnih. U ovom slučaju imam dinamičku komponentu u njoj, koja je označena kao `<username>` URL komponenta okružena sa `<` i `>`. Kada ruta ima dinamičku komponentu, Flask će prihvatiti bilo koji tekst u tom delu URL-a i pozvati funkciju prikaza sa stvarnim tekstom kao argumentom. Na primer, ako klijentski pregledač zahteva URL `/user/susan`, funkcija pogleda će biti pozvana sa argumentom `username` postavljenim na 'susan'. Ova funkcija pogleda će biti dostupna samo prijavljenim korisnicima, pa sam dodao `@login_required` dekorator iz `Flask-Login`.

Implementacija ove funkcije pogleda je prilično jednostavna. Prvo pokušavam da učitam korisnika iz baze podataka koristeći upit po korisničkom imenu. Videli ste ranije da se upit baze podataka može izvršiti sa `db.session.scalars()` ako želite da dobijete sve rezultate, ili `db.session.scalar()` ako želite da dobijete samo prvi rezultat ili `None` ako nema rezultata. U ovoj funkciji pogleda koristim varijantu koju pruža Flask-SQLAlchemy pod nazivom `db.first_or_404()`, koja radi kao `scalar()` kada postoje rezultati, ali u slučaju da nema rezultata, automatski šalje grešku 404 nazad klijentu. Izvršavanjem upita na ovaj način štedim sebe od provere da li je upit vratio korisnika, jer kada korisničko ime ne postoji u bazi podataka, funkcija neće vratiti i umesto toga će se izazvati izuzetak 404.

Ako upit u bazu podataka ne pokrene grešku 404, to znači da je pronađen korisnik sa datim korisničkim imenom. Zatim inicijalizujem lažnu listu objava za ovog korisnika i prikazujem novi šablon `user.html` kojem prosleđujem objekat korisnika i listu objava.

Šablon user.html je prikazan ispod:

> `app/templates/user.html` : Šablon korisničkog profila

```html
{% extends "base.html" %}

{% block content %}
    <h1>User: {{ user.username }}</h1>
    <hr>
    {% for post in posts %}
    <p>
    {{ post.author.username }} says: <b>{{ post.body }}</b>
    </p>
    {% endfor %}
{% endblock %}
```

Stranica profila je sada završena, ali link do nje ne postoji nigde na veb-sajtu. Da bih korisnicima malo olakšao proveru sopstvenog profila, dodaću link do nje u navigacionu traku na vrhu:

> `app/templates/base.html` : Šablon korisničkog profila

```html
<div>
    Microblog:
    <a href="{{ url_for('index') }}">Home</a>
    {% if current_user.is_anonymous %}
    <a href="{{ url_for('login') }}">Login</a>
    {% else %}
    <a href="{{ url_for('user', username=current_user.username) }}">Profile</a>
    <a href="{{ url_for('logout') }}">Logout</a>
    {% endif %}
</div>
```

Jedina zanimljiva promena ovde je `url_for()` poziv koji se koristi za generisanje veze do stranice profila. Pošto funkcija pogleda korisničkog profila prima dinamički argument, funkcija `url_for()` dobija vrednost za ovaj deo URL-a kao ključni argument. Pošto je ovo veza koja ukazuje na profil prijavljenog korisnika, mogu da koristim `Flask-Login`-ov `current_user` da generišem ispravnu URL adresu.

Isprobajte aplikaciju sada. Klik na `Profile` link na vrhu trebalo bi da vas odvede na vašu korisničku stranicu. Trenutno nema linkova koji će vas odvesti na stranicu profila drugih korisnika, ali ako želite da pristupite tim stranicama, možete ručno uneti URL adresu u adresnu traku pregledača. Na primer, ako imate korisnika po imenu "jovan" registrovanog u vašoj aplikaciji, možete pogledati odgovarajući korisnički profil tako što ćete uneti <http://localhost:5000/user/jovan> u adresnu traku.

## Avatari

Siguran sam da se slažete da su stranice profila koje sam upravo napravio prilično dosadne. Da bih ih učinio malo zanimljivijim, dodaću korisničke avatare, ali umesto da se bavim potencijalno velikom kolekcijom otpremljenih slika na serveru, koristiću `Gravatar` servis da obezbedim slike za sve korisnike.

`Gravatar` servis je veoma jednostavan za korišćenje. Da biste zatražili sliku za datog korisnika, mora se koristiti URL adresa u formatu `https://www.gravatar.com/avatar/<hash>`, gde je MD5 heš korisničke imejl adrese. Ispod možete videti kako da dobijete Gravatar URL za korisnika sa imejlom `john@example.com`:

```py
>>> from hashlib import md5
>>> 'https://www.gravatar.com/avatar/' + md5(b'john@example.com').hexdigest()
'https://www.gravatar.com/avatar/d4c74594d841139328695756648b6bd6'
```

Ako želite da vidite stvarni primer, moj Gravatar URL je:

```sh
https://www.gravatar.com/avatar/729e26a2a2c7ff24a71958d4aa4e5f35
```

Podrazumevano, vraćena veličina slike je `80x80` piksela, ali se može zahtevati i druga veličina dodavanjem `s` argumenta u string upita URL-a. Na primer, da bih dobio svoj avatar kao sliku veličine 128x128 piksela, URL je <https://www.gravatar.com/avatar/729e26a2a2c7ff24a71958d4aa4e5f35?s=128>.

Još jedan zanimljiv argument koji se može proslediti Gravataru kao argument upitnog stringa je `d`, koji određuje koju sliku Gravatar pruža korisnicima koji nemaju registrovan avatar na servisu. Moj omiljeni se zove "identicon", koji vraća lep geometrijski dizajn koji je drugačiji za svaku e-poštu.

Imajte na umu da neka proširenja za privatnost veb pregledača, kao što je `Ghostery`, blokiraju slike `Gravatar`-a, jer smatraju da Automattic (vlasnici Gravatar servisa) mogu da utvrde koje sajtove posećujete na osnovu zahteva koje dobijaju za vaš avatar. Ako ne vidite avatare u svom pregledaču, uzmite u obzir da problem može biti posledica proširenja koje ste instalirali u svom pregledaču.

Pošto su avatari povezani sa korisnicima, ima smisla dodati logiku koja generiše URL-ove avatara u model korisnika.

> `app/models.py` : URL-ovi korisničkih avatara

```py
from hashlib import md5
#...

class User(UserMixin, db.Model):
    #...
    def avatar(self, size):
        digest = md5(self.email.lower().encode('utf-8')).hexdigest()
        return f'https://www.gravatar.com/avatar/{digest}?d=identicon&s={size}'
```

Nova `avatar()` metoda klase `User` vraća URL adresu slike avatara korisnika, skaliranu na traženu veličinu u pikselima. Za korisnike koji nemaju registrovan avatar, generisaće se slika "identikon". Da bih generisao MD5 heš, prvo konvertujem imejl adresu u mala slova, jer to zahteva Gravatar servis. Zatim, pošto MD5 podrška u Pajtonu radi na bajtovima, a ne na stringovima, kodiram string kao bajtove pre nego što ga prosledim heš funkciji.

Ako ste zainteresovani da saznate više o drugim opcijama koje nudi Gravatar servis, posetite njihovu veb stranicu sa dokumentacijom.

Sledeći korak je umetanje slika avatara u šablon korisničkog profila:

> `app/templates/user.html` : Avatar korisnika u šablonu

```html
{% extends "base.html" %}

{% block content %}
    <table>
        <tr valign="top">
            <td><img src="{{ user.avatar(128) }}"></td>
            <td><h1>User: {{ user.username }}</h1></td>
        </tr>
    </table>
    <hr>
    {% for post in posts %}
    <p>
    {{ post.author.username }} says: <b>{{ post.body }}</b>
    </p>
    {% endfor %}
{% endblock %}
```

Lepa stvar kod toga što je `User` klasa odgovorna za vraćanje URL-ova avatara je to što ako jednog dana odlučim da Gravatar avatari nisu ono što želim, mogu jednostavno prepisati metodu `avatar()` da vraća različite URL-ove, i svi šabloni će automatski početi da prikazuju nove avatare.

Sada imam lep veliki avatar na vrhu stranice korisničkog profila, ali zaista nema razloga da se tu zaustavim. Imam nekoliko objava od korisnika na dnu koje bi takođe mogle imati mali avatar. Naravno, za stranicu korisničkog profila sve objave će imati isti avatar, ali onda mogu implementirati istu funkcionalnost na glavnoj stranici, a onda će svaka objava biti ukrašena autorovim avatarom, i to će izgledati zaista lepo.

Da bih prikazao avatare za pojedinačne objave, potrebno je da napravim još jednu malu promenu u šablonu:

> `app/templates/user.html` : Avatari korisnika u objavama

```html
{% extends "base.html" %}

{% block content %}
    <table>
        <tr valign="top">
            <td><img src="{{ user.avatar(128) }}"></td>
            <td><h1>User: {{ user.username }}</h1></td>
        </tr>
    </table>
    <hr>
    {% for post in posts %}
    <table>
        <tr valign="top">
            <td><img src="{{ post.author.avatar(36) }}"></td>
            <td>{{ post.author.username }} says:<br>{{ post.body }}</td>
        </tr>
    </table>
    {% endfor %}
{% endblock %}
```

## Korišćenje podšablona Nindže

Dizajnirao sam stranicu korisničkog profila tako da prikazuje objave koje je napisao korisnik, zajedno sa njihovim avatarima. Sada želim da i indeksna stranica prikazuje objave sa sličnim rasporedom. Mogao bih jednostavno da kopiram/nalepim deo šablona koji se bavi prikazivanjem objave, ali to zaista nije idealno jer kasnije, ako odlučim da napravim izmene u ovom rasporedu, moraću da zapamtim da ažuriram oba šablona.

Umesto toga, napraviću podšablon koji prikazuje samo jednu objavu, a zatim ću ga referencirati iz šablona `user.html` i `index.html`. Za početak, mogu da napravim podšablon, samo sa HTML oznakama za jednu objavu. Nazvaću ovaj šablon `app/templates/_post.html`. `_` prefiks je samo konvencija imenovanja koja mi pomaže da prepoznam koje datoteke šablona su podšabloni.

> `app/templates/_post.html` : Podšablon objave

```html
<table>
    <tr valign="top">
        <td><img src="{{ post.author.avatar(36) }}"></td>
        <td>{{ post.author.username }} says:<br>{{ post.body }}</td>
    </tr>
</table>
```

Da bih pozvao ovaj podšablon iz šablona `user.html`, koristim Nindžinu `include` izjavu:

> `app/templates/user.html` : Avatari korisnika u objavama

```html
{% extends "base.html" %}

{% block content %}
    <table>
        <tr valign="top">
            <td><img src="{{ user.avatar(128) }}"></td>
            <td><h1>User: {{ user.username }}</h1></td>
        </tr>
    </table>
    <hr>
    {% for post in posts %}
        {% include '_post.html' %}
    {% endfor %}
{% endblock %}
```

Indeksna stranica aplikacije još nije zaista razrađena, tako da još neću tamo dodavati ovu funkcionalnost.

### Još zanimljivih profila

Jedan problem koji imaju nove stranice korisničkih profila je taj što se na njima zapravo ne prikazuje mnogo toga. Korisnici vole da ispričaju malo o sebi na ovim stranicama, pa ću im dozvoliti da napišu nešto o sebi da ovde pokažu. Takođe ću pratiti kada je svaki korisnik poslednji put pristupio sajtu i prikazaću to na svojoj stranici profila.

Prva stvar koju treba da uradim da bih podržao sve ove dodatne informacije je da proširim `users` tabelu u bazi podataka sa dva nova polja:

> `app/models.py` : Nova polja u korisničkom modelu

```py
class User(UserMixin, db.Model):
    #...
    about_me: so.Mapped[Optional[str]] = so.mapped_column(sa.String(140))
    last_seen: so.Mapped[Optional[datetime]] = so.mapped_column(
        default=lambda: datetime.now(timezone.utc))
```

Svaki put kada se baza podataka izmeni, potrebno je generisati migraciju baze podataka. U poglavlju 4 sam vam pokazao kako da podesite aplikaciju da prati promene u bazi podataka putem skripti za migraciju. Sada imam dva nova polja koja želim da dodam u bazu podataka, tako da je prvi korak generisanje skripte za migraciju:

```sh
(venv) $ flask db migrate -m "new fields in user model"
INFO  [alembic.runtime.migration] Context impl SQLiteImpl.
INFO  [alembic.runtime.migration] Will assume non-transactional DDL.
INFO  [alembic.autogenerate.compare] Detected added column 'user.about_me'
INFO  [alembic.autogenerate.compare] Detected added column 'user.last_seen'
  Generating migrations/versions/37f06a334dbf_new_fields_in_user_model.py... done
```

Izlaz komande `migrate` izgleda dobro, jer pokazuje da su dva nova polja u klasi `User` otkrivena. Sada mogu da primenim ovu promenu na bazu podataka:

```sh
(venv) $ flask db upgrade
INFO  [alembic.runtime.migration] Context impl SQLiteImpl.
INFO  [alembic.runtime.migration] Will assume non-transactional DDL.
INFO  [alembic.runtime.migration] Running upgrade 780739b227a7 -> 37f06a334dbf, new fields in user model
```

Nadam se da shvatate koliko je korisno raditi sa migracionim okvirom. Svi korisnici koji su bili u bazi podataka su i dalje tamo, migracioni okvir hirurški primenjuje promene u skripti za migraciju bez uništavanja bilo kakvih podataka.

Za sledeći korak, dodaću ova dva nova polja u šablon korisničkog profila:

> `app/templates/user.html` : Prikaži informacije o korisniku u šablonu korisničkog profila

```html
{% extends "base.html" %}

{% block content %}
    <table>
        <tr valign="top">
            <td><img src="{{ user.avatar(128) }}"></td>
            <td>
                <h1>User: {{ user.username }}</h1>
                {% if user.about_me %}<p>{{ user.about_me }}</p>{% endif %}
                {% if user.last_seen %}<p>Last seen on: {{ user.last_seen }}</p>{% endif %}
            </td>
        </tr>
    </table>
   ...
{% endblock %}
```

Imajte na umu da ova dva polja obuhvatam Nindžinim uslovnim izrazima, jer želim da budu vidljiva samo ako su podešena. U ovom trenutku ova dva nova polja su prazna za sve korisnike, tako da ih još nećete videti.

### Beleženje vremena poslednje posete korisnika

Počnimo sa `last_seen` poljem, koje je lakše od ta dva. Ono što želim da uradim jeste da upišem trenutno vreme u ovo polje za datog korisnika kad god taj korisnik pošalje zahtev serveru.

Dodavanje prijave za podešavanje ovog polja na svakoj mogućoj funkciji prikaza koja se može zahtevati od pregledača je očigledno nepraktično, ali izvršavanje dela generičke logike pre nego što se zahtev pošalje funkciji prikaza je toliko čest zadatak u veb aplikacijama da ga Flask nudi kao nativnu funkciju. Pogledajte rešenje:

> `app/routes.py` : Zabeležite vreme poslednje posete

```py
from datetime import datetime, timezone

@app.before_request
def before_request():
    if current_user.is_authenticated:
        current_user.last_seen = datetime.now(timezone.utc)
        db.session.commit()
```

Dekorator `@before_request` iz Flaska registruje dekorisanu funkciju da se izvrši neposredno pre funkcije pogleda. Ovo je izuzetno korisno jer sada mogu da ubacim kod koji želim da se izvrši pre bilo koje funkcije pogleda u aplikaciji, i mogu ga imati na jednom mestu. Implementacija jednostavno proverava da li je `current_user` korisnik prijavljen i u tom slučaju postavlja `last_seen` polje na trenutno vreme. Već sam pomenuo ovo, serverska aplikacija mora da radi u konzistentnim vremenskim jedinicama, a standardna praksa je da se koristi UTC vremenska zona. Korišćenje lokalnog vremena sistema nije dobra ideja, jer onda ono što ide u bazu podataka zavisi od vaše lokacije.

Poslednji korak je potvrda sesije baze podataka, tako da se gore navedena promena zapiše u bazu podataka. Ako se pitate zašto nema `db.session.add()` pre potvrde, uzmite u obzir da kada referencirate `current_user`, Flask-Login će pozvati funkciju povratnog poziva za učitavanje korisnika, koja će pokrenuti upit u bazi podataka koji će staviti ciljnog korisnika u sesiju baze podataka. Dakle, možete ponovo dodati korisnika u ovoj funkciji, ali to nije neophodno jer je već tamo.

Ako pogledate stranicu svog profila nakon što napravite ovu promenu, videćete red „Poslednji put viđen/a“ sa vremenom koje je veoma blizu trenutnom vremenu. A ako napustite stranicu profila, a zatim se vratite, videćete da se vreme stalno ažurira.

Činjenica da čuvam ove vremenske oznake u UTC vremenskoj zoni čini da vreme prikazano na stranici profila takođe bude u UTC-u. Pored toga, format vremena nije onakav kakav biste očekivali, jer je ono zapravo interna reprezentacija Pajton `datetime` objekta. Za sada se neću brinuti o ova dva problema, jer ću se temom rukovanja datumima i vremenima u veb aplikaciji baviti u kasnijem poglavlju.

## Uređivač profila

Takođe treba da korisnicima pružim formu u koji mogu da unesu neke informacije o sebi. Forma će omogućiti korisnicima da promene svoje korisničko ime, a takođe i da napišu nešto o sebi, što će biti sačuvano u novom `about_me` polju. Hajde da počnemo da pišemo klasu forme za to:

> `app/forms.py` : Forma za uređivanje profila

```py
from wtforms import TextAreaField
from wtforms.validators import Length
#...
class EditProfileForm(FlaskForm):
    username = StringField('Username', validators=[DataRequired()])
    about_me = TextAreaField('About me', validators=[Length(min=0, max=140)])
    submit = SubmitField('Submit')
```

Koristim novi tip polja i novi validator u ovoj formi. Za polje "About me" koristim `TextAreaField`, što je višeredno polje u koje korisnik može da unese tekst. Za validaciju ovog polja koristim `Length`, koji će osigurati da je uneti tekst između 0 i 140 znakova, što je prostor koji sam dodelio za odgovarajuće polje u bazi podataka.

Šablon koji prikazuje ovu formu je prikazan ispod:

> `app/templates/edit_profile.html` : Forma za uređivanje profila

```html
{% extends "base.html" %}

{% block content %}
    <h1>Edit Profile</h1>
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
            {{ form.about_me.label }}<br>
            {{ form.about_me(cols=50, rows=4) }}<br>
            {% for error in form.about_me.errors %}
            <span style="color: red;">[{{ error }}]</span>
            {% endfor %}
        </p>
        <p>{{ form.submit() }}</p>
    </form>
{% endblock %}
```

I konačno, evo funkcije pogleda koja sve povezuje:

> `app/routes.py` : Funkcija za uređivanje pogleda profila

```py
from app.forms import EditProfileForm

@app.route('/edit_profile', methods=['GET', 'POST'])
@login_required
def edit_profile():
    form = EditProfileForm()
    if form.validate_on_submit():
        current_user.username = form.username.data
        current_user.about_me = form.about_me.data
        db.session.commit()
        flash('Your changes have been saved.')
        return redirect(url_for('edit_profile'))
    elif request.method == 'GET':
        form.username.data = current_user.username
        form.about_me.data = current_user.about_me
    return render_template('edit_profile.html', title='Edit Profile', form=form)
```

Ova funkcija pogleda obrađuje formu na malo drugačiji način. Ako `validate_on_submit()` vrati `True` kopiram podatke iz forme u korisnički objekat, a zatim upisujem objekat u bazu podataka. Ali kada `validate_on_submit()` vrati `False` to može biti zbog dva različita razloga. Prvo, može biti zato što je pregledač upravo poslao `GET` zahtev, na koji treba da odgovorim pružanjem početne verzije šablona forme. Takođe može biti kada pregledač pošalje zahtev `POST` sa podacima forme, ali nešto u tim podacima je nevažeće. Za ovaj obrazac, moram da tretiram ova dva slučaja odvojeno. Kada se forma prvi put zahteva zahtevom GET, želim da unapred popunim polja podacima koji su sačuvani u bazi podataka, tako da moram da uradim obrnuto od onoga što sam uradio u slučaju slanja i da premestim podatke sačuvane u korisničkim poljima u formu, jer će to osigurati da ta polja forme imaju trenutne podatke sačuvane za korisnika. Ali u slučaju greške u validaciji ne želim da pišem ništa u polja forme, jer su ona već popunjena pomoću `WTForms`-a. Da bih razlikovao ova dva slučaja, proveravam `request.method`, što će biti `GET` za početni zahtev, i `POST` za zahtev koji nije prošao validaciju.

Da bih korisnicima olakšao pristup stranici za uređivanje profila, mogu dodati vezu na njihovu stranicu profila:

> `app/templates/user.html` : Link za uređivanje profila

```html
{% if user == current_user %}
<p><a href="{{ url_for('edit_profile') }}">Edit your profile</a></p>
{% endif %}
```

Obratite pažnju na pametan uslov koji koristim da bih se uverio da se link "Uredi" pojavljuje kada gledate svoj profil, ali ne i kada gledate profil nekog drugog.
