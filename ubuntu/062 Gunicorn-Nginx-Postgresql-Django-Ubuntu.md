# Podešavanje "Django" sa "Postgres"-om, "Nginx"-om i "Gunicorn"-om na "Ubuntu"-u

- Ažurirano 3. oktobra 2025.

## Uvod

"Django" je moćan veb frejmvork koji vam može pomoći da pokrenete svoju "Python" aplikaciju ili veb lokaciju. "Django" uključuje pojednostavljeni razvojni server za lokalno testiranje vašeg koda, ali za sve što je čak i malo povezano sa produkcijom, potreban je bezbedniji i moćniji veb server.

U ovom tutorijalu, instaliraćete i konfigurisati neke komponente na "Ubuntu"-u da bi podržale i služile "Django" aplikacije. Podesićete "PostgreSQL" bazu podataka umesto korišćenja podrazumevane SQLite baze podataka. Konfigurisaćete "Gunicorn" aplikacijski server da se poveže sa vašim aplikacijama. Zatim ćete podesiti "Nginx" da koristi obrnuti proksi za "Gunicorn", dajući vam pristup njegovim bezbednosnim i performansnim funkcijama za služinje vaših aplikacija.

Za sveobuhvatno poređenje opcija baza podataka, pogledajte naš vodič o "SQLite" vs "MySQL" vs "PostgreSQL": "Poređenje sistema za upravljanje relacionim bazama podataka."

Rasporedite svoje aplikacije sa GitHub-a koristeći DigitalOcean App Platform. Dozvolite DigitalOcean-u da se fokusira na skaliranje vaše aplikacije.

Instaliraćete "Django" u virtuelnom okruženju. Instaliranje "Django"-a u okruženje specifično za vaš projekat omogućiće da se vaši projekti i njihovi zahtevi obrađuju odvojeno. Za detaljnije informacije o radu sa "PostgreSQL"-om u "Django"-u, pogledajte naš sveobuhvatni vodič o tome kako koristiti "PostgreSQL" sa vašom "Django" aplikacijom na "Ubuntu 22.04".

Kada pokrenete bazu podataka i aplikaciju, instaliraćete i konfigurisati "Gunicorn" aplikacijski server. Ovo će služiti kao interfejs za našu aplikaciju, prevodeći zahteve klijenata iz HTTP u "Python" pozive koje naša aplikacija može da obradi. Zatim ćete podesiti "Nginx" ispred "Gunicorn"-a kako biste iskoristili njegove visoko efikasne mehanizme za rukovanje vezama i njegove lako implementirane bezbednosne funkcije.

Hajde da počnemo.

## Šta ćemo naučiti

Do kraja ovog sveobuhvatnog tutorijala, imaćete:

- **Implementacija "Django"-a spremna za produkciju** : Podesite robusnu "Django" aplikaciju sa
  "PostgreSQL"-om, "Nginx"-om i "Gunicorn"-om na "Ubuntu"-u.

- **Napredna optimizacija performansi** : Konfigurišite "Gunicorn" sa optimalnim radnim procesima,
  implementirajte "Nginx" strategije keširanja i podesite "PostgreSQL" za visokoperformansne operacije baze podataka.

- **Najbolje bezbednosne prakse** : Implementirajte SSL/TLS enkripciju, konfigurišite pravila
  zaštitnog zida i primenite "Django" bezbednosna podešavanja za proizvodna okruženja

- **Mogućnosti integracije veštačke inteligencije** : Naučite kako da integrišete modele mašinskog
  učenja, veštačke inteligencione API-je i moderne strategije primene za inteligentne aplikacije

- **Praćenje i rešavanje problema** : Podesite sveobuhvatne tehnike evidentiranja, praćenja
  performansi i otklanjanja grešaka za proizvodna okruženja

- **Arhitektura skalabilnosti** : Razumeti kako da skalirate svoju "Django" aplikaciju
  horizontalno i vertikalno za scenarije sa velikim prometom

## Preduslovi

Ako koristite "Ubuntu" verziju 16.04 ili stariju, preporučujemo vam da nadogradite na noviju verziju jer "Ubuntu" više ne podržava ove verzije. Ova kolekcija vodiča će vam pomoći u nadogradnji vaše "Ubuntu" verzije.

Da biste završili ovaj vodič, potreban vam je server koji pokreće "Ubuntu", zajedno sa korisnikom koji nije `root` sa `sudo` privilegijama i aktivnim zaštitnim zidom. Za smernice o tome kako da ih podesite, izaberite svoju distribuciju sa ove liste i pratite naš Vodič za početno podešavanje servera.

## "Django", "Nginx" i "Gunicorn" podešavanje na "Ubuntu"

- **Instaliranje pakete iz "Ubuntu" repozitorijuma**
- **Kreiranje "PostgreSQL" baze podataka i korisnika**
- **Kreiranje virtuelnog okruženja u Pajtonu za projekat**
- **Kreiranje i konfigurisanje novog "Django" projekat**
- **Završetak podešavanje "Django" projekta**
- **Testiranje "Gunicorn"-ove sposobnosti da služi projektu**
- **Kreiranje "Gunicorn" "systemd" socketa i servisnih datoteka**
- **Proverite datoteku socketa "Gunicorn"**
- **Testiranje aktivaciju socketa**
- **Konfigurišite "Nginx" da proksira ka "Gunicorn"-u**
- **Rešavanje problema sa "Nginx"-om i "Gunicorn"-om**

### 1. Instaliranje paketa iz "Ubuntu" repozitorijuma

Da biste započeli proces, preuzećete i instalirati sve potrebne stavke iz "Ubuntu" repozitorijuma. Kasnije ćete koristiti "Python" menadžer paketa `pip` da biste instalirali dodatne komponente.

Prvo treba da ažurirate lokalni `apt` indeks paketa, a zatim preuzmete i instalirate pakete. Paketi koje instalirate zavise od toga koju verziju Pajtona će vaš projekat koristiti.

Ako koristite "Django" sa "Python" 3, otkucajte:

```sh
sudo apt update
sudo apt install python3-venv python3-dev libpq-dev postgresql postgresql-contrib nginx curl
```

Ova komanda će instalirati alat za kreiranje virtuelnih okruženja za vaše Pajton projekte, Pajton razvojne datoteke potrebne za kasnije izgradnju Gunikorna, "Postgres" sistem baze podataka i biblioteke potrebne za interakciju sa njim, i Nginx veb server.

Da biste započeli proces, preuzećete i instalirati sve potrebne stavke iz "Ubuntu" repozitorijuma. Kasnije ćete koristiti "Python" menadžer paketa `pip` da biste instalirali dodatne komponente.

### 2. Kreiranje "PostgreSQL" baze podataka i korisnika

Sada možete odmah da krenete i kreirate bazu podataka i korisnika baze podataka za našu "Django" aplikaciju.

Podrazumevano, "Postgres" koristi šemu autentifikacije pod nazivom `peer` autentifikacija za lokalne veze. U osnovi, to znači da ako se korisničko ime korisnika operativnog sistema podudara sa važećim korisničkim imenom "Postgres"-a, taj korisnik može da se prijavi bez dalje autentifikacije.

Tokom instalacije "Postgres"-a, kreiran je `postgres` korisnik operativnog sistema pod nazivom koji odgovara `postgres` administratorskom korisniku "PostgreSQL"-a. Potrebno je da koristite ovog korisnika za obavljanje administrativnih zadataka. Možete koristiti `sudo` i proslediti korisničko ime sa `-u` opcijom.

Prijavite se na interaktivnu "Postgres" sesiju tako što ćete otkucati:

```sh
sudo -u "postgres" psql
```

Dobićete "psql" prompt gde možete podesiti naše zahteve.

Prvo, kreirajte bazu podataka za svoj projekat:

```sql
CREATE DATABASE myproject;
```

**Napomena**: Svaka "psql" naredba mora da se završi tačkom-zarezom, zato se uverite da se vaša komanda završava sa njim ako imate problema.

Zatim, kreirajte korisnika baze podataka za naš projekat. Obavezno izaberite bezbednu lozinku:

```sql
CREATE USER myprojectuser WITH PASSWORD 'password';
```

Nakon toga, izmenićete nekoliko parametara veze za korisnika koga ste upravo kreirali. Ovo će ubrzati rad baze podataka tako da nećete morati da tražite i podešavate ispravne vrednosti svaki put kada se veza uspostavi.

Postavićete podrazumevano kodiranje znakova na `UTF-8`, što "Django" očekuje. Takođe podešavate podrazumevanu šemu izolacije transakcija na `read committed`, što blokira čitanja iz nepotvrđenih transakcija. Na kraju, podešavate `vremensku zonu`. Podrazumevano, "Django" projekti će biti podešeni da koriste `UTC`. Ovo su sve preporuke iz samog "Django" projekta :

```sql
ALTER ROLE myprojectuser SET client_encoding TO 'utf8';
ALTER ROLE myprojectuser SET default_transaction_isolation TO 'read committed';
ALTER ROLE myprojectuser SET timezone TO 'UTC';
```

Sada možete dati novom korisniku pristup za administriranje nove baze podataka:

```sql
GRANT ALL PRIVILEGES ON DATABASE myproject TO myprojectuser;
```

Kada završite, izađite iz "PostgreSQL" prompta tako što ćete otkucati:

```sql
\q
```

"Postgres" je sada podešen tako da se "Django" može povezati sa svojom bazom podataka i upravljati njome.

### 3. Kreiranje virtuelnog okruženja u Pajtonu za vaš projekat

Sada kada imate spremnu bazu podataka, možete početi sa dobijanjem ostalih zahteva za projekat. Instaliraćete Pajton zahteve u virtuelnom okruženju radi lakšeg upravljanja.

Prvo, kreirajte i promenite direktorijum u kome možete čuvati datoteke projekta:

```sh
mkdir ~/myprojectdir
cd ~/myprojectdir
```

Unutar direktorijuma projekta, kreirajte virtuelno okruženje u Pajtonu tako što ćete otkucati:

```sh
python3 -m venv myprojectenv
```

Ovo će kreirati direktorijum pod nazivom "myprojectenv" unutar vašeg "myprojectdir" direktorijuma. Unutar njega će biti instalirana lokalna verzija Pajtona i lokalna verzija `pip` za upravljanje paketima. Ovu strukturu virtuelnog okruženja možete koristiti za instaliranje i konfigurisanje izolovanog Pajton okruženja za bilo koji projekat koji želite da kreirate.

Pre instaliranja Pajton zahteva vašeg projekta, potrebno je da aktivirate virtuelno okruženje. To možete učiniti tako što ćete otkucati:

```sh
source myprojectenv/bin/activate
```

Vaš prompt bi trebalo da se promeni kako bi naznačio da sada radite u virtuelnom okruženju Pajtona. Izgledaće otprilike ovako:

```sh
(myprojectenv)user@host:~/myprojectdir$
```

Sa aktivnim virtuelnim okruženjem, instalirajte "Django", "Gunicorn" i `psycopg2`, "PostgreSQL" adapter sa lokalnom instancom `pip`:

**Napomena**: Kada je virtuelno okruženje aktivirano (kada vaš prompt ima "(myprojectenv)" koristite `pip` umesto `pip3`, čak i ako koristite "Python 3". Kopija alata u virtuelnom okruženju se uvek zove `pip`, bez obzira na verziju Python-a.

```sh
pip install "Django" gunicorn psycopg2-binary
```

Sada bi trebalo da imate sav softver potreban za pokretanje "Django" projekta.

### 4. Kreiranje i konfigurisanje novog "Django" projekta

Kada su vaše "Python" komponente instalirane, sada možete da kreirate stvarne "Django" projektne datoteke.

Pošto već imate direktorijum projekta, reći ćete "Django"-u da instalira datoteke ovde. On će kreirati direktorijum drugog nivoa sa stvarnim kodom, što je normalno, i postaviti skriptu za upravljanje u ovaj direktorijum. Ključ za ovo je da eksplicitno definišete direktorijum umesto da dozvolite "Django"-u da donosi odluke u odnosu na naš trenutni direktorijum:

```sh
django-admin startproject myproject ~/myprojectdir
```

U ovom trenutku, vaš direktorijum projekta ( u ovom primeru ) treba da ima sledeći sadržaj:

```sh
~/myprojectdir
    /manage.py                      // "Django" skripta za upravljanje projektom.
    /myproject/                     // Paket projekta. 
        __init__.py, 
        settings.py, 
        urls.py, 
        asgi.py, i 
        wsgi.py.
    /myprojectenv/    // Dir. virtuelnog okruženja koji ste ranije kreirali.
```

Prvo što treba da uradite sa novokreiranim datotekama projekta jeste da podesite podešavanja. Otvorite datoteku podešavanja u uređivaču teksta:

```sh
nano ~/myprojectdir/myproject/settings.py
```

Počnite tako što ćete pronaći `ALLOWED_HOSTS` direktivu. Ona definiše listu adresa servera ili imena domena koji se mogu koristiti za povezivanje sa "Django" instancom. Svi dolazni zahtevi sa `host` zaglavljem koje nije na ovoj listi izazvaće izuzetak. "Django" zahteva da ovo podesite kako biste sprečili određenu klasu bezbednosne ranjivosti.

U uglastim zagradama navedite IP adrese ili imena domena koji su povezani sa vašim "Django" serverom. Svaka stavka treba da bude navedena pod navodnicima, a unosi treba da budu odvojeni zarezom. Ako želite zahteve za ceo domen i sve poddomene, dodajte tačku na početak unosa. U donjem isečku koda, postoji nekoliko primera bez komentara koji se koriste za demonstraciju:

**Napomena** : Obavezno uključite `localhost` kao jednu od opcija jer ćete proksirati veze preko lokalne "Nginx" instance.

"~/myprojectdir/myproject/settings.py"

```py
...
# The simplest case: just add the domain name(s) and IP addresses of your "Django" server
# ALLOWED_HOSTS = [ 'example.com', '203.0.113.5']
# To respond to 'example.com' and any subdomains, start the domain with a dot
# ALLOWED_HOSTS = ['.example.com', '203.0.113.5']
ALLOWED_HOSTS = ['your_server_domain_or_IP', 'second_domain_or_IP',..., 'localhost']
...
```

Zatim, pronađite odeljak koji konfiguriše pristup bazi podataka. Počinjaće sa `DATABASES`. Konfiguracija u datoteci je za SQLite bazu podataka. Već ste kreirali "PostgreSQL" bazu podataka za naš projekat, tako da je potrebno da prilagodite podešavanja.

Promenite podešavanja sa informacijama o vašoj "PostgreSQL" bazi podataka. Recite "Django"-u da koristi `psycopg2` adapter koji ste instalirali sa `pip`. Potrebno je da navedete ime baze podataka, korisničko ime baze podataka, lozinku korisnika baze podataka, a zatim navedete da se baza podataka nalazi na lokalnom računaru. Možete ostaviti podešavanje `PORT` kao prazan string:

"~/myprojectdir/myproject/settings.py"

```py
...
DATABASES = {
    'default': {
        'ENGINE': '"Django".db.backends.postgresql_psycopg2',
        'NAME': 'myproject',
        'USER': 'myprojectuser',
        'PASSWORD': 'password',
        'HOST': 'localhost',
        'PORT': '',
    }
}
...
```

Zatim, idite na dno datoteke i dodajte podešavanje koje pokazuje gde treba da se smeste statičke datoteke. Ovo je neophodno kako bi "Nginx" mogao da obrađuje zahteve za ove stavke. Sledeći red govori "Django"-u da ih smesti u direktorijum koji se zove "static" u osnovnom direktorijumu projekta:

"~/myprojectdir/myproject/settings.py"

```py
...
STATIC_URL = 'static/'

# Default primary key field type
# https://docs."Django"project.com/en/4.0/ref/settings/#default-auto-field

DEFAULT_AUTO_FIELD = '"Django".db.models.BigAutoField'

import os
STATIC_ROOT = os.path.join(BASE_DIR, 'static/')
```

Sačuvajte i zatvorite datoteku kada završite.

### 5. Završetak podešavanja projekta

Sada možete migrirati početnu šemu baze podataka u našu "PostgreSQL" bazu podataka koristeći skriptu za upravljanje:

```sh
~/myprojectdir/manage.py makemigrations
~/myprojectdir/manage.py migrate
```

Kreirajte administratorskog korisnika za projekat tako što ćete otkucati:

```sh
~/myprojectdir/manage.py createsuperuser
```

Moraćete da izaberete korisničko ime, unesete adresu e-pošte i izaberete i potvrdite lozinku.

Možete sakupiti sav statički sadržaj u direktorijum koji ste konfigurisali tako što ćete otkucati:

```sh
~/myprojectdir/manage.py collectstatic
```

Moraćete da potvrdite operaciju. Statičke datoteke će zatim biti smeštene u direktorijum koji se zove "static" unutar direktorijuma vašeg projekta.

Ako ste pratili početni vodič za podešavanje servera, trebalo bi da imate `UFW` zaštitni zid koji štiti vaš server. Da biste testirali razvojni server, potrebno je da omogućite pristup portu koji ćete koristiti.

Napravite izuzetak za port 8000 tako što ćete otkucati:

```sh
sudo ufw allow 8000
```

Konačno, možete testirati svoj projekat pokretanjem "Django" razvojnog servera pomoću ove komande:

```sh
~/myprojectdir/manage.py runserver 0.0.0.0:8000
```

U vašem veb pregledaču posetite ime domena ili IP adresu vašeg servera, a zatim :8000:  <http://server_domain_or_IP:8000>

Trebalo bi da dobijete podrazumevanu "Django" indeksnu stranicu.

Ako dodate "/admin" na kraj URL adrese u adresnoj traci, biće vam zatraženo da unesete administrativno korisničko ime i lozinku koje ste kreirali pomoću `createsuperuser` komande:

#### Prijava administratora na "Django"

Nakon autentifikacije, možete pristupiti podrazumevanom "Django" administratorskom interfejsu.

#### Administrativni interfejs "Django"-a

Kada završite sa istraživanjem, pritisnite CTRL-C u prozoru terminala da biste isključili razvojni server.

### 6. Testiranje "Gunicorn" sposobnosti da služi projektu

Poslednja stvar koju treba da uradite pre nego što napustite svoje virtuelno okruženje jeste da testirate "Gunicorn" kako biste bili sigurni da može da služi aplikaciji. To možete učiniti tako što ćete ući u direktorijum projekta i koristiti `gunicorn` komandu za učitavanje `WSGI` modula projekta:

```sh
cd ~/myprojectdir
gunicorn --bind 0.0.0.0:8000 myproject.wsgi
```

Ovo će pokrenuti "Gunicorn" na istom interfejsu na kojem je radio "Django" razvojni server. Možete se vratiti i ponovo testirati aplikaciju u svom pregledaču.

**Napomena** : Administratorski interfejs neće imati primenjen nijedan od stilova jer "Gunicorn" ne zna kako da pronađe statički `CSS` sadržaj odgovoran za ovo.

Prosledili ste `wsgi` modul "Gunicorn"-u tako što ste naveli relativnu putanju direktorijuma do "Django" `wsgi.py` datoteke, koja je ulazna tačka za vašu aplikaciju, koristeći Python-ovu sintaksu modula. Unutar ove datoteke, definisana je funkcija `application`, koja se koristi za komunikaciju sa aplikacijom.

Kada završite sa testiranjem, pritisnite `CTRL-C` u prozoru terminala da biste zaustavili "Gunicorn".

Sada ste završili sa konfigurisanjem vaše "Django" aplikacije. Možete se vratiti iz našeg virtuelnog okruženja tako što ćete otkucati:

```sh
deactivate
```

Indikator virtuelnog okruženja u vašem promptu će biti uklonjen.

### 7. Kreiranje sistemskih socketa i servisnih datoteka za "Gunicorn"

Testirali ste da "Gunicorn" može da komunicira sa našom "Django" aplikacijom, ali sada bi trebalo da implementirate robusniji način pokretanja i zaustavljanja aplikacijskog servera. Da biste to postigli, napravićete `systemd` servis i `socket` datoteku.

"Gunicorn" `socket` će biti kreiran pri pokretanju sistema i slušaće veze. Kada se veza uspostavi, `systemd` će automatski pokrenuti "Gunicorn" proces za obradu veze.

Počnite tako što ćete kreirati i otvoriti sistemsku `socket` datoteku za "Gunicorn" sa `sudo` privilegijama:

```sh
sudo nano /etc/systemd/system/gunicorn.socket
```

Unutra ćete kreirati `[Unit]` odeljak za opisivanje socketa, `[Socket]` odeljak za definisanje lokacije socketa i `[Install]` odeljak da biste bili sigurni da je socket kreiran u pravo vreme:

"/etc/systemd/system/gunicorn.socket"

```sh
[Unit]
Description=gunicorn socket

[Socket]
ListenStream=/run/gunicorn.sock

[Install]
WantedBy=sockets.target
```

Sačuvajte i zatvorite datoteku kada završite.

Zatim, kreirajte i otvorite sistemsku servisnu datoteku za "Gunicorn" sa `sudo` privilegijama u vašem uređivaču teksta. Ime servisne datoteke treba da se podudara sa imenom `socket` datoteke, sa izuzetkom ekstenzije:

```sh
sudo nano /etc/systemd/system/gunicorn.service
```

Počnite sa `[Unit]` odeljkom, koji se koristi za određivanje metapodataka i zavisnosti. Ovde stavite opis servisa i recite inicijalnom sistemu da ga pokrene tek nakon što se dostigne cilj mreže. Pošto se vaš servis oslanja na socket iz datoteke socketa, potrebno je da uključite direktivu `Requires` koja označava tu vezu:

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target
```

Zatim ćete otvoriti odeljak [Service]. Navedite korisnika i grupu pod kojima želite da se proces pokrene. Daćete svom redovnom korisničkom nalogu vlasništvo nad procesom, jer on poseduje sve relevantne datoteke. Daćete grupno vlasništvo nad grupom www-datakako bi "Nginx" mogao lako da komunicira sa "Gunicorn"-om.

Zatim ćete mapirati radni direktorijum i navesti komandu koja će se koristiti za pokretanje servisa. U ovom slučaju, morate navesti punu putanju do izvršne datoteke "Gunicorn", koja je instalirana u našem virtuelnom okruženju. Zatim ćete povezati proces sa Unix socketom koji ste kreirali u direktorijumu "/run" kako bi proces mogao da komunicira sa "Nginx"-om. Sve podatke ćete evidentirati na standardnom izlazu kako bi proces `journald` mogao da prikuplja "Gunicorn" logove. Ovde možete navesti i bilo koja opcionalna podešavanja za "Gunicorn". Na primer, u ovom slučaju ste naveli 3 radna procesa:

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target

[Service]
User=sammy
Group=www-data
WorkingDirectory=/home/sammy/myprojectdir
ExecStart=/home/sammy/myprojectdir/myprojectenv/bin/gunicorn \
          --access-logfile - \
          --workers 3 \
          --bind unix:/run/gunicorn.sock \
          myproject.wsgi:application
```

Konačno, dodaćete `[Install]` odeljak. Ovo će reći `systemd`-u sa čim da poveže ovu uslugu ako je omogućite da se pokreće pri pokretanju sistema. Želite da se ova usluga pokrene kada je pokrenut i radi običan višekorisnički sistem:

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target

[Service]
User=sammy
Group=www-data
WorkingDirectory=/home/sammy/myprojectdir
ExecStart=/home/sammy/myprojectdir/myprojectenv/bin/gunicorn \
          --access-logfile - \
          --workers 3 \
          --bind unix:/run/gunicorn.sock \
          myproject.wsgi:application

[Install]
WantedBy=multi-user.target
```

Time je vaša sistemska servisna datoteka "gunicorn.service" završena. Sačuvajte je i zatvorite sada.

Sada možete pokrenuti i omogućiti "Gunicorn" socket. Ovo će kreirati datoteku socketa "/run/gunicorn.sock" u sadašnjem trenutku i pri pokretanju sistema. Kada se uspostavi veza sa tim socket-om, `systemd` će automatski pokrenuti `gunicorn.service` da bi je obradio:

```sh
sudo systemctl start gunicorn.socket
sudo systemctl enable gunicorn.socket
```

Možete potvrditi da je operacija bila uspešna proverom datoteke socketa.

### 8. Provera datoteke "gunicorn.socket"

Proverite status procesa da biste saznali da li je mogao da se pokrene:

```sh
sudo systemctl status gunicorn.socket
```

Trebalo bi da dobijete izlaz poput ovog:

```sh
● gunicorn.socket - gunicorn socket
     Loaded: loaded (/etc/systemd/system/gunicorn.socket; enabled; vendor preset: enabled)
     Active: active (listening) since Mon 2022-04-18 17:53:25 UTC; 5s ago
   Triggers: ● gunicorn.service
     Listen: /run/gunicorn.sock (Stream)
     CGroup: /system.slice/gunicorn.socket

Apr 18 17:53:25 "Django" systemd[1]: Listening on gunicorn socket.
```

Zatim, proverite postojanje datoteke "gunicorn.sock" u "/run" direktorijumu:

```sh
file /run/gunicorn.sock
```

```sh
/run/gunicorn.sock: socket
```

Ako `systemctl status` komanda je ukazala na grešku ili ako ne pronađete "gunicorn.sock" datoteku u direktorijumu, to je indikacija da "gunicorn.sock" nije mogao biti pravilno kreiran. Proverite logove "Gunicorn" socketa tako što ćete otkucati:

```sh
sudo journalctl -u gunicorn.socket
```

Ponovo pogledajte "/etc/systemd/system/gunicorn.sock" datoteku da biste rešili eventualne probleme pre nego što nastavite.

### 9. Testiranje aktivacije socket-a

Trenutno, ako ste tek pokrenuli "gunicorn.socket" jedinicu, `gunicorn.service` još uvek neće biti aktivna jer socket još nije primio nikakve veze. Ovo možete proveriti tako što ćete otkucati:

```sh
sudo systemctl status gunicorn
```

```sh
○ gunicorn.service - gunicorn daemon
     Loaded: loaded (/etc/systemd/system/gunicorn.service; disabled; vendor preset: enabled)
     Active: inactive (dead)
TriggeredBy: ● gunicorn.socket
```

Da biste testirali mehanizam aktivacije socketa, možete poslati vezu sa socketom tako curlšto ćete otkucati:

```sh
curl --unix-socket /run/gunicorn.sock localhost
```

Trebalo bi da dobijete HTML izlaz iz vaše aplikacije u terminalu. Ovo ukazuje da je "Gunicorn" pokrenut i da je mogao da opslužuje vašu "Django" aplikaciju. Možete proveriti da li "Gunicorn" servis radi tako što ćete otkucati:

```sh
sudo systemctl status gunicorn
```

```sh
● gunicorn.service - gunicorn daemon
     Loaded: loaded (/etc/systemd/system/gunicorn.service; disabled; vendor preset: enabled)
     Active: active (running) since Mon 2022-04-18 17:54:49 UTC; 5s ago
TriggeredBy: ● gunicorn.socket
   Main PID: 102674 (gunicorn)
      Tasks: 4 (limit: 4665)
     Memory: 94.2M
        CPU: 885ms
     CGroup: /system.slice/gunicorn.service
             ├─102674 /home/sammy/myprojectdir/myprojectenv/bin/python3 /home/sammy/myprojectdir/myprojectenv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock myproject.wsgi:application
             ├─102675 /home/sammy/myprojectdir/myprojectenv/bin/python3 /home/sammy/myprojectdir/myprojectenv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock myproject.wsgi:application
             ├─102676 /home/sammy/myprojectdir/myprojectenv/bin/python3 /home/sammy/myprojectdir/myprojectenv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock myproject.wsgi:application
             └─102677 /home/sammy/myprojectdir/myprojectenv/bin/python3 /home/sammy/myprojectdir/myprojectenv/bin/gunicorn --access-logfile - --workers 3 --bind unix:/run/gunicorn.sock myproject.wsgi:application

Apr 18 17:54:49 "Django" systemd[1]: Started gunicorn daemon.
Apr 18 17:54:49 "Django" gunicorn[102674]: [2022-04-18 17:54:49 +0000] [102674] [INFO] Starting gunicorn 20.1.0
Apr 18 17:54:49 "Django" gunicorn[102674]: [2022-04-18 17:54:49 +0000] [102674] [INFO] Listening at: unix:/run/gunicorn.sock (102674)
Apr 18 17:54:49 "Django" gunicorn[102674]: [2022-04-18 17:54:49 +0000] [102674] [INFO] Using worker: sync
Apr 18 17:54:49 "Django" gunicorn[102675]: [2022-04-18 17:54:49 +0000] [102675] [INFO] Booting worker with pid: 102675
Apr 18 17:54:49 "Django" gunicorn[102676]: [2022-04-18 17:54:49 +0000] [102676] [INFO] Booting worker with pid: 102676
Apr 18 17:54:50 "Django" gunicorn[102677]: [2022-04-18 17:54:50 +0000] [102677] [INFO] Booting worker with pid: 102677
Apr 18 17:54:50 "Django" gunicorn[102675]:  - - [18/Apr/2022:17:54:50 +0000] "GET / HTTP/1.1" 200 10697 "-" "curl/7.81.0"
```

Ako izlaz iz `curl` ili `systemctl status` ukazuje na to da je došlo do problema, proverite dnevnike za dodatne detalje:

```sh
sudo journalctl -u gunicorn
```

Proverite da li u "/etc/systemd/system/gunicorn.service" datoteci ima problema. Ako napravite izmene u "/etc/systemd/system/gunicorn.service" datoteci, ponovo učitajte demon da biste ponovo pročitali definiciju servisa i ponovo pokrenite proces "Gunicorn" kucanjem:

```sh
sudo systemctl daemon-reload
sudo systemctl restart gunicorn
```

Uverite se da ste rešili gore navedene probleme pre nego što nastavite.

### 10. Konfigurišite "Nginx" da proksira podatke na "Gunicorn"

Sada kada je "Gunicorn" podešen, potrebno je da konfigurišete "Nginx" da prosledi saobraćaj "Gunicorn" procesu.

Počnite tako što ćete kreirati i otvoriti novi blok servera u "Nginx"-ovom "sites-available" direktorijumu:

```sh
sudo nano /etc/nginx/sites-available/myproject
```

Unutra, otvorite novi blok servera. Počećete tako što ćete navesti da ovaj blok treba da sluša na normalnom portu `80` i da treba da odgovara na ime domena ili IP adresu vašeg servera:

"/etc/nginx/sites-available/myproject"

```sh
server {
    listen 80;                          # Port na kome server sluša
    server_name server_domain_or_IP;    # Domensko ime servera ili IP adresa
}
```

Zatim ćete reći "Nginx"-u da ignoriše sve probleme sa pronalaženjem "fav" ikona. Takođe ćete mu reći gde da pronađe statičke elemente koje ste sakupili u svom direktorijumu. Sve ove datoteke imaju standardni URI prefiks "/static", tako da možete kreirati `location` blok koji odgovara tim zahtevima: `~/myprojectdir/static`

"/etc/nginx/sites-available/myproject"

```sh
server {
    listen 80;
    server_name server_domain_or_IP;

    location = /favicon.ico { access_log off; log_not_found off; }
    location /static/ {
        root /home/sammy/myprojectdir;
    }
}
```

Konačno, kreirajte `location /` blok koji će odgovarati svim ostalim zahtevima. Unutar ove lokacije, uključićete standardnu `proxy_params` datoteku koja je uključena u "Nginx" instalaciju, a zatim ćete proslediti saobraćaj direktno do "Gunicorn" socket-a:

"/etc/nginx/sites-available/myproject"

```sh
server {
    listen 80;
    server_name server_domain_or_IP;

    location = /favicon.ico { access_log off; log_not_found off; }
    location /static/ {
        root /home/sammy/myprojectdir;
    }

    location / {
        include proxy_params;
        proxy_pass http://unix:/run/gunicorn.sock;
    }
}
```

Sačuvajte i zatvorite datoteku kada završite. Sada možete omogućiti datoteku tako što ćete je povezati sa direktorijumom "sites-enabled":

```sh
sudo ln -s /etc/nginx/sites-available/myproject /etc/nginx/sites-enabled
```

Testirajte svoju "Nginx" konfiguraciju za sintaksičke greške tako što ćete otkucati:

```sh
sudo nginx -t
```

Ako se ne prijave greške, ponovo pokrenite "Nginx" tako što ćete otkucati:

```sh
sudo systemctl restart nginx
```

Konačno, potrebno je da otvorite svoj zaštitni zid (fajervol) za normalan saobraćaj na portu 80. Pošto vam više nije potreban pristup razvojnom serveru, možete ukloniti pravilo za otvaranje i porta 8000:

```sh
sudo ufw delete allow 8000
sudo ufw allow '"Nginx" Full'
```

Sada bi trebalo da možete da odete na domen ili IP adresu vašeg servera da biste videli svoju aplikaciju.

**Napomena** : Nakon konfigurisanja "Nginx"-a, sledeći korak bi trebalo da bude obezbeđivanje saobraćaja ka serveru pomoću SSL/TLS-a. Ovo je važno jer se bez njega sve informacije, uključujući lozinke, šalju preko mreže u običnom tekstu.

Ako imate ime domena, najlakši način da dobijete SSL sertifikat za zaštitu vašeg saobraćaja je korišćenje "Let's Encrypt"-a. Pratite ovaj vodič za "Ubuntu" 22.04 / Ubuntu 20.04 / Ubuntu" 18.04" da biste podesili "Let's Encrypt" sa "Nginx"-om na "Ubuntu" 22.04. Pratite proceduru koristeći "Nginx" serverski blok koji ste kreirali u ovom vodiču.

### 11. Rešavanje problema sa "Nginx"-om i "Gunicorn"-om

Ako ovaj poslednji korak ne prikazuje vašu aplikaciju, moraćete da rešite problem sa instalacijom.

#### "Nginx" prikazuje podrazumevanu stranicu umesto "Django" aplikacije

Ako "Nginx" prikazuje podrazumevanu stranicu umesto da se povezuje sa vašom aplikacijom, to obično znači da treba da podesite `server_name` unutar datoteke "/etc/nginx/sites-available/myproject" da bi ukazivalo na IP adresu ili ime domena vašeg servera.

"Nginx" koristi `server_name` da bi odredio koji blok servera da koristi za odgovor na zahteve. Ako dobijete podrazumevanu "Nginx" stranicu, to je znak da "Nginx" nije bio u stanju da eksplicitno upari zahtev sa blokom servera, pa se vraća na podrazumevani blok definisan u "/etc/nginx/sites-available/default".

Blok `server_name` servera u vašem projektu mora biti specifičniji od onog u podrazumevanom bloku servera da bi bio izabran.

#### "Nginx" prikazuje grešku "502 Bad Gateway" umesto "Django" aplikacije

"Greška 502" ukazuje da "Nginx" ne može uspešno da proksira zahtev. Širok spektar problema sa konfiguracijom se izražava greškom 502, tako da je potrebno više informacija za pravilno rešavanje problema.

Glavno mesto za traženje više informacija je u "Nginx"-ovim evidencijama grešaka. Generalno, ovo će vam reći koji su uslovi izazvali probleme tokom proksiranja. Pratite "Nginx" evidencije grešaka tako što ćete otkucati:

```sh
sudo tail -F /var/log/nginx/error.log
```

Sada, napravite još jedan zahtev u pregledaču da biste generisali novu grešku (pokušajte da osvežite stranicu). Trebalo bi da dobijete novu poruku o grešci koja će biti zapisana u dnevnik. Ako pogledate poruku, trebalo bi da vam pomogne da suzite problem.

- Možda ćete dobiti sledeću poruku:
  
  ```sh
  connect() to unix:/run/gunicorn.sock failed (2: No such file or directory)
  ```
  
  Ovo ukazuje da "Nginx" nije mogao da pronađe "gunicorn.sock" datoteku na datoj lokaciji. Trebalo 
bi da uporedite `proxy_pass` lokaciju definisanu u okviru "/etc/nginx/sites-available/myproject" datoteke sa stvarnom lokacijom datoteke "gunicorn.sock" koju je generisala "gunicorn.socket"   sistemska jedinica.

  Ako ne možete da pronađete "gunicorn.sock" datoteku u "/run" direktorijumu, to generalno znači da 
`systemd` socket datoteka nije mogla da je kreira. Vratite se na odeljak o proveri "Gunicorn" socket datoteke da biste prošli kroz korake za rešavanje problema za "Gunicorn".
  
- Poruka može biti i:

  ```sh
  connect() to unix:/run/gunicorn.sock failed (13: Permission denied)
  ```
  
  Ovo ukazuje da "Nginx" nije mogao da se poveže sa "Gunicorn" socketom zbog problema sa dozvolama. 
Ovo se može desiti kada se postupak sprovede koristeći `root` korisnika umesto `sudo` korisnika. Dok `systemd` može da kreira "Gunicorn" socket datoteku, "Nginx" ne može da joj pristupi.
  
  Ovo se može desiti i ako postoje ograničene dozvole u ​​bilo kojoj tački između korenskog 
direktorijuma ( "/" ) i "gunicorn.sock" datoteke. Možete pregledati dozvole i vrednosti vlasništva za socket datoteke i svakog od njenih roditeljskih direktorijuma tako što ćete komandi proslediti apsolutnu putanju do vaše socket datoteke namei:
  
  ```sh
  namei -l /run/gunicorn.sock
  ```
  
  ```sh
  f: /run/gunicorn.sock
  drwxr-xr-x root root /
  drwxr-xr-x root root run
  srw-rw-rw- root root gunicorn.sock
  ```
  
  Izlaz prikazuje dozvole svake od komponenti direktorijuma. Posmatranjem dozvola (prva kolona),
vlasnika (druga kolona) i vlasnika grupe (treća kolona), možete utvrditi koja vrsta pristupa je dozvoljena datoteci socketa.
  
  U gornjem primeru, socket datoteka i svaki od direktorijuma koji vode do socket datoteke imaju  
dozvole za čitanje i izvršavanje (kolona dozvola za direktorijume se završava sa `r-x` umesto `---`). "Nginx" proces bi trebalo da bude u mogućnosti da uspešno pristupi socketu.
  
  Ako bilo koji od direktorijuma koji vode do socketa nema dozvolu za čitanje i izvršavanje, 
"Nginx" neće moći da pristupi socketu bez dozvoljavanja dozvola za čitanje i izvršavanje ili bez osiguravanja da je vlasništvo nad grupom dato grupi čiji je "Nginx" deo.
  
#### "Django" prikazuje: "nije moguće povezati se sa serverom: Veza odbijena“

Jedna poruka koju možete dobiti od "Django"-a kada pokušavate da pristupite delovima aplikacije u veb pregledaču je:

```sh
OperationalError at /admin/login/
could not connect to server: Connection refused
    Is the server running on host "localhost" (127.0.0.1) and accepting
    TCP/IP connections on port 5432?
```

Ovo ukazuje da "Django" ne može da se poveže sa "Postgres" bazom podataka. Uverite se da je "Postgres" instanca pokrenuta tako što ćete otkucati:

```sh
sudo systemctl status postgresql
```

Ako nije, možete je pokrenuti i omogućiti da se automatski pokrene pri pokretanju sistema (ako već nije konfigurisana za to) tako što ćete otkucati:

```sh
sudo systemctl start postgresql
sudo systemctl enable postgresql
```

Ako i dalje imate problema, proverite da li su podešavanja baze podataka definisana u datoteci "~/myprojectdir/myproject/settings.py" ispravna.

#### Dalje rešavanje problema

Za dodatno rešavanje problema, evidencije mogu pomoći u sužavanju uzroka. Proverite svaku od njih redom i potražite poruke koje ukazuju na problematična područja.

Sledeći dnevnici mogu biti korisni:

- Proverite logove "Nginx" procesa tako što ćete otkucati:  
  `sudo journalctl -u nginx`
- Proverite logove pristupa "Nginx"-u tako što ćete otkucati:  
  `sudo less /var/log/nginx/access.log`
- Proverite evidenciju grešaka "Nginx"-a tako što ćete otkucati:  
  `sudo less /var/log/nginx/error.log`
- Proverite logove aplikacije "Gunicorn" tako što ćete otkucati:  
  `sudo journalctl -u gunicorn`
- Proverite logove "Gunicorn" socketa tako što ćete otkucati:  
  `sudo journalctl -u gunicorn.socket`

Kako ažurirate konfiguraciju ili aplikaciju, verovatno ćete morati ponovo pokrenuti procese da biste se prilagodili promenama.

Ako ažurirate svoju "Django" aplikaciju, možete ponovo pokrenuti "Gunicorn" proces da biste primenili promene tako što ćete otkucati:

```sh
sudo systemctl restart gunicorn
```

Ako promenite "Gunicorn" socket ili servisne datoteke, ponovo učitajte demon i restartujte proces kucanjem:

```sh
sudo systemctl daemon-reload
sudo systemctl restart gunicorn.socket gunicorn.service
```

Ako promenite konfiguraciju bloka "Nginx" servera, testirajte konfiguraciju, a zatim restARTUJTE "Nginx" tako što ćete otkucati:  
`sudo nginx -t && sudo systemctl restart nginx`

Ove komande su korisne za prihvatanje promena dok podešavate konfiguraciju.

### Napredne optimizacije performansi

#### Podešavanje performansi kompanije "Gunicorn"

Podrazumevana konfiguracija "Gunicorn"-a funkcioniše za osnovne instalacije, ali proizvodne aplikacije koje obrađuju hiljade istovremenih korisnika zahtevaju pažljivo podešavanje. Evo zašto je svaki parametar važan i kako ga optimizovati za vaše specifično radno opterećenje:

Razumevanje "Gunicorn" radnih procesa: "Gunicorn" koristi model glavnog radnog procesa gde glavni proces upravlja radnim procesima koji obrađuju stvarne zahteve. Broj radnih procesa direktno utiče na sposobnost vaše aplikacije da obrađuje istovremene zahteve, ali više nije uvek bolje.

"/etc/systemd/system/gunicorn.service"

```sh
[Unit]
Description=gunicorn daemon
Requires=gunicorn.socket
After=network.target

[Service]
User=sammy
Group=www-data
WorkingDirectory=/home/sammy/myprojectdir
ExecStart=/home/sammy/myprojectdir/myprojectenv/bin/gunicorn \
          --access-logfile - \
          --error-logfile - \
          --workers 4 \
          --worker-class gevent \
          --worker-connections 1000 \
          --max-requests 1000 \
          --max-requests-jitter 100 \
          --timeout 30 \
          --keep-alive 2 \
          --bind unix:/run/gunicorn.sock \
          myproject.wsgi:application

[Install]
WantedBy=multi-user.target
```

#### Objašnjenje ključnih parametara performansi

- **--workers 4** : Formula (2 × CPU jezgra) + 1 dobro funkcioniše za aplikacije vezane za CPU, ali
"Django" aplikacije su često vezane za I/O operacije (čekaju upite u bazu podataka, eksterne API-je). Za aplikacije koje zahtevaju mnogo I/O operacija, možete bezbedno koristiti 2-4 puta veći broj CPU jezgara. Pratite korišćenje CPU-a da biste pronašli idealnu vrednost.

- **--worker-class gevent** : Ovo koristi zelene niti (kooperativni multitasking) umesto OS niti.
Gevent je savršen za "Django" aplikacije koje prave mnogo upita u bazi podataka ili API poziva jer omogućava jednom radniku da istovremeno obrađuje više zahteva dok čeka na I/O operacije. Ovo može povećati propusnost za 3-5 puta u poređenju sa sinhronim radnicima.

- **--worker-connections 1000** : Ovo ograničava koliko istovremenih veza svaki gevent vorker može  da obradi. Sa 4 vorkera i 1000 veza po njemu, teoretski možete da obradite 4000 istovremenih veza. Podesite ovo na osnovu očekivanog saobraćaja i raspoložive memorije.

- **--max-requests 1000** Curenje memorije u dugotrajnim Pajton procesima je uobičajeno. Ovaj
  parametar ponovo pokreće procese nakon 1000 zahteva kako bi se sprečilo akumuliranje memorije. Parametar podrhtavanja (100) dodaje slučajnost tako da se svi procesi ne ponovo pokreću istovremeno, što bi izazvalo kratak prekid usluge.

- **--timeout 30** :Ovo ukida radnike kojima je potrebno više od 30 sekundi da odgovore. Podesite ovo na osnovu najsporije očekivane operacije. Za većinu "Django" aplikacija, 30 sekundi je razumno, ali prilagodite na osnovu vašeg specifičnog slučaja upotrebe.

- **--keep-alive 2** : Ovo održava HTTP veze aktivnim 2 sekunde nakon slanja odgovora, smanjujući
  opterećenje veze za klijente koji podnose više zahteva.

#### Optimizacija performansi "Nginx"-a

"Nginx" deluje kao obrnuti proksi i statički fajl server, obrađujući klijentske veze i direktno poslužujući statičke resurse. Ova konfiguracija optimizuje "Nginx" za visoko-performansne "Django" aplikacije:

Zašto su ove optimizacije važne:

- **Statičko serviranje datoteka** : "Nginx" servira statičke datoteke (CSS, JS, slike) mnogo brže nego "Django"

- **Rukovanje konekcijama** : "Nginx" može efikasno da obrađuje hiljade istovremenih konekcija

- **Keširanje** : Smanjuje opterećenje vaše "Django" aplikacije prikazivanjem keširanog sadržaja

- **Kompresija** : Smanjuje upotrebu propusnog opsega i poboljšava vreme učitavanja stranice

"/etc/nginx/sites-available/myproject"

```sh
server {
    listen 80;
    server_name server_domain_or_IP;

    # Security headers
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header Referrer-Policy "no-referrer-when-downgrade" always;

    # Gzip compression
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_proxied any;
    gzip_comp_level 6;
    gzip_types
        text/plain
        text/css
        text/xml
        text/javascript
        application/json
        application/javascript
        application/xml+rss
        application/atom+xml
        image/svg+xml;

    # Static files with caching
    location = /favicon.ico { 
        access_log off; 
        log_not_found off; 
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
    
    location /static/ {
        root /home/sammy/myprojectdir;
        expires 1y;
        add_header Cache-Control "public, immutable";
        access_log off;
    }

    # Media files
    location /media/ {
        root /home/sammy/myprojectdir;
        expires 1M;
        add_header Cache-Control "public";
    }

    # Main application
    location / {
        include proxy_params;
        proxy_pass http://unix:/run/gunicorn.sock;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_connect_timeout 30s;
        proxy_send_timeout 30s;
        proxy_read_timeout 30s;
    }
}
```

**Raspodela konfiguracije** :

- **Bezbednosni zaglavlja** :

  - **X-Frame-Options "SAMEORIGIN"** : Sprečava klikdžeking napade kontrolišući da li se vaš sajt može ugraditi u okvire.
  
  - **X-Content-Type-Options "nosniff"** : Sprečava pregledače da odgovore MIME-sniffing-om, smanjujući vektore XSS napada
  
  - **X-XSS-Protection "1; mode=block"** : Omogućava ugrađenu XSS zaštitu pregledača

  - **Referrer-Policy "no-referrer-when-downgrade"** : Kontroliše koliko informacija o referentu se šalje sa zahtevima

- **Gzip kompresija** :

  - **gzip on** : Omogućava kompresiju tekstualnih datoteka

  - **gzip_comp_level 6** Nivo kompresije (1-9, gde 6 pruža dobru ravnotežu između korišćenja procesora i odnosa kompresije)
  - **gzip_types** : Određuje koje tipove datoteka treba kompresovati. Tekstualne datoteke se dobro kompresuju (smanjenje od 60-80%), dok su slike i video zapisi već kompresovani.

- **Statičko keširanje datoteka**:

  - **expires 1y** : Govori pregledačima da keširaju statičke datoteke godinu dana
  - **Cache-Control "public, immutable"** : Označava da se datoteke neće menjati, što omogućava agresivno keširanje
  - **access_log off** : Onemogućava evidentiranje statičkih datoteka radi smanjenja I/O opterećenja

- **Konfiguracija proksija**:

  - **proxy_set_header** : Prosleđuje informacije o klijentu kompaniji "Django" radi pravilnog obrađivanja zahteva
  - **proxy_connect_timeout 30s** : Maksimalno vreme za uspostavljanje veze sa "Gunicorn"-om
  - **proxy_send_timeout 30s** : Maksimalno vreme za slanje zahteva kompaniji "Gunicorn"
  - **proxy_read_timeout 30s** : Maksimalno vreme čekanja na odgovor od Gunicorn-a

#### Podešavanje performansi "PostgreSQL"-a

Podrazumevana konfiguracija "PostgreSQL"-a je konzervativna i dizajnirana je da radi na bilo kom hardveru. Za proizvodne "Django" aplikacije, ove optimizacije mogu poboljšati performanse baze podataka za 2-5 puta:

Zašto je podešavanje baze podataka važno:

- **Alokacija memorije** : Pravilna podešavanja memorije smanjuju ulazno-izlazne operacije diska tako što često pristupane podatke čuvaju u RAM-u.
- **Upravljanje vezama** : Optimizacija ograničenja veza sprečava iscrpljivanje resursa
- **Performanse upita** : Bolje evidentiranje pomaže u identifikaciji sporih upita kojima je potrebna optimizacija

"/etc/postgresql/14/main/postgresql.conf"

```sh
# Memory settings
shared_buffers = 256MB
effective_cache_size = 1GB
work_mem = 4MB
maintenance_work_mem = 64MB

# Connection settings
max_connections = 100
listen_addresses = 'localhost'

# Logging
log_statement = 'mod'
log_min_duration_statement = 1000
log_line_prefix = '%t [%p]: [%l-1] user=%u,db=%d,app=%a,client=%h '

# Checkpoint settings
checkpoint_completion_target = 0.9
wal_buffers = 16MB
```

**Objašnjenje konfiguracije "PostgreSQL"**-a :

- **Podešavanja memorije** :

  - **shared_buffers = 256MB** Ovo je glavna keš memorija "PostgreSQL"-a. Podesite je na 25% ukupne RAM memorije za namenske servere baza podataka ili 15% za deljene servere. Ova keš memorija čuva često pristupane stranice podataka, smanjujući ulazno-izlazne operacije diska.
  
  - **effective_cache_size = 1GB** : Govori "PostgreSQL"-u koliko memorije je dostupno za keširanje (uključujući keš memoriju operativnog sistema). Postavlja se na 75% ukupne RAM memorije. Ovo pomaže planeru upita da donosi bolje odluke o korišćenju indeksa.
  
  - **work_mem = 4MB** : Memorija koja se koristi za sortiranje, heš, spajanje i druge operacije. Svaka veza može da koristi ovu količinu. Za "Django" aplikacije sa složenim upitima, povećajte na 8-16MB, ali pratite ukupnu upotrebu (**work_mem × max_connections**).
  
  - **maintenance_work_mem = 64MB** : Memorija za operacije održavanja kao što su VACUUM, CREATE INDEX i ALTER TABLE. Može se podesiti na mnogo veću vrednost od **work_mem** (do **2GB**) jer je koristi samo jedna operacija istovremeno.
  
- **Podešavanja veze**:

  - **max_connections = 100** : Maksimalan broj istovremenih veza. Svaka veza koristi memoriju, zato uravnotežite ovo sa potrebama vaše aplikacije. Za "Django" aplikacije, obično je dovoljno 50-100 veza.

  - **listen_addresses = 'localhost'** Slušajte samo na lokalnom hostu radi bezbednosti. Promenite na '*' samo ako su vam potrebne udaljene veze (ne preporučuje se za produkciju).

- **Podešavanja evidentiranja**

  - **log_statement = 'mod'** : Zabeležava sve naredbe koje menjaju podatke (INSERT, UPDATE, DELETE). Pomaže pri otklanjanju grešaka i analizi performansi.

  - **log_min_duration_statement = 1000** : Beleži upite koji traju duže od 1 sekunde. Neophodno za identifikaciju sporih upita kojima je potrebna optimizacija.

  - **log_line_prefix** : Prilagođava format dnevnika kako bi uključio vremensku oznaku, ID procesa, korisnika, bazu podataka i IP adresu klijenta radi boljeg otklanjanja grešaka.

- **Podešavanja kontrolne tačke**

  - **checkpoint_completion_target = 0.9** : Raspodeljuje U/I kontrolne tačke na preko 90% intervala kontrolne tačke, smanjujući skokove U/I.

  - **wal_buffers = 16MB** : Baferi dnevnika za unapred pisanje. Podesite na 16MB za bolje performanse sa velikim opterećenjem pisanja.

### Integracija veštačke inteligencije i moderne strategije primene

#### Integracija veštačke inteligencije u "Django"

Moderne "Django" aplikacije mogu da iskoriste mogućnosti veštačke inteligencije i mašinskog učenja kako bi pružile inteligentne funkcije poput preporuka sadržaja, automatizovane korisničke podrške i prediktivne analitike. Evo kako ih efikasno integrisati:

#### Zašto integrisati veštačku inteligenciju sa "Django"-om

- **Poboljšano korisničko iskustvo** : Veštačka inteligencija može da personalizuje sadržaj, predvidi ponašanje korisnika i automatizuje rutinske zadatke
- **Poslovna inteligencija** : Modeli mašinskog učenja mogu analizirati obrasce podataka i pružiti korisne uvide
- **Automatizacija** : Veštačka inteligencija može da obavlja ponavljajuće zadatke poput moderiranja sadržaja, korisničke podrške i obrade podataka
- **Konkurentska prednost** : Funkcije zasnovane na veštačkoj inteligenciji razlikuju vašu aplikaciju na tržištu

- **Podešavanje zavisnosti veštačke inteligencije**

Dodajte AI biblioteke u svoje virtuelno okruženje:

```sh
pip install tensorflow scikit-learn pandas numpy openai langchain
```

- **Kreiranje sloja veštačke inteligencije (AI) usluga**

Napravite novu "Django" aplikaciju za AI funkcionalnost:

```sh
python manage.py startapp ai_services
```

Dodajte svom "settings.py" :

```py
INSTALLED_APPS = [
    #... existing apps
    'ai_services',
]
```

- **Implementacija veštačke inteligencije**

Kreirajte modele usluga veštačke inteligencije u "ai_services/models.py":

```py
from "Django".db import models
from "Django".contrib.auth.models import User

class AIModel(models.Model):
    name = models.CharField(max_length=100)
    model_type = models.CharField(max_length=50)
    version = models.CharField(max_length=20)
    created_at = models.DateTimeField(auto_now_add=True)
    is_active = models.BooleanField(default=True)

class PredictionLog(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    model = models.ForeignKey(AIModel, on_delete=models.CASCADE)
    input_data = models.JSONField()
    prediction = models.JSONField()
    confidence = models.FloatField()
    created_at = models.DateTimeField(auto_now_add=True)
```

Sloj veštačke inteligencije (AI) servisa deluje kao most između vaše "Django" aplikacije i eksternih AI servisa. Ovaj obrazac odvaja AI logiku od vaše poslovne logike, što olakšava testiranje, održavanje i prebacivanje između različitih AI dobavljača:

- **Zašto koristiti sloj usluga**:

- **Razdvajanje briga** : Odvaja logiku veštačke inteligencije od "Django" prikaza i modela
- **Testibilnost** : Lako je imitirati AI servise za jedinično testiranje
- **Fleksibilnost** : Možete prelaziti između različitih dobavljača veštačke inteligencije bez promene vaših pogleda
- **Obrada grešaka** : Centralizovana obrada grešaka za kvarove AI servisa
- **Keširanje** : Može da implementira strategije keširanja za skupe AI operacije

Kreiraj "ai_services/services.py" :

```py
import openai
from "Django".conf import settings
import json

class AIService:
    def __init__(self):
        openai.api_key = settings.OPENAI_API_KEY
    
    def generate_text(self, prompt, max_tokens=150):
        """Generate text using OpenAI GPT"""
        try:
            response = openai.Completion.create(
                engine="text-davinci-003",
                prompt=prompt,
                max_tokens=max_tokens,
                temperature=0.7
            )
            return response.choices[0].text.strip()
        except Exception as e:
            return f"Error: {str(e)}"
    
    def analyze_sentiment(self, text):
        """Analyze text sentiment using a simple model"""
        # This is a placeholder - implement your actual sentiment analysis
        positive_words = ['good', 'great', 'excellent', 'amazing', 'wonderful']
        negative_words = ['bad', 'terrible', 'awful', 'horrible', 'disappointing']
        
        text_lower = text.lower()
        positive_count = sum(1 for word in positive_words if word in text_lower)
        negative_count = sum(1 for word in negative_words if word in text_lower)
        
        if positive_count > negative_count:
            return {'sentiment': 'positive', 'confidence': 0.8}
        elif negative_count > positive_count:
            return {'sentiment': 'negative', 'confidence': 0.8}
        else:
            return {'sentiment': 'neutral', 'confidence': 0.6}
```

- **Implementacija Dokera za veštačku inteligenciju**

  Kontejnerizacija vaše "Django" aplikacije sa AI mogućnostima pruža nekoliko prednosti za produkcijsku primenu:

  - Zašto koristiti Doker za veštačku inteligenciju:

    - **Konzistentnost** : Obezbeđuje da vaši veštačka inteligencija modeli rade na isti način tokom razvoja, pripreme i produkcije
    - **Skalabilnost** : Lako skaliranje pojedinačnih servisa (veb aplikacija, veštačkih inteligencija, baza podataka) nezavisno
    - **Upravljanje zavisnostima** : Izoluje zavisnosti biblioteke veštačke inteligencije koje bi mogle biti u sukobu sa drugim aplikacijama
    - **Jednostavnost implementacije** : Jedna komanda implementira ceo vaš stek aplikacija
    - **Izolacija resursa** : Sprečava da korišćenje memorije veštačke inteligencije utiče na druge aplikacije

  - **Strategija Dockerfile-a**: Višestepeni pristup izgradnji optimizuje konačnu veličinu slike odvajanjem zavisnosti izgradnje od zahteva izvršavanja. Ovo je posebno važno za AI aplikacije koje često imaju velike "Python" pakete.
  
    - **Kreirajte "Dockerfile" za kontejnerizovano raspoređivanje**
  
      ```py
      FROM python:3.11-slim
      
      # Set environment variables
      ENV PYTHONDONTWRITEBYTECODE=1
      ENV PYTHONUNBUFFERED=1
      
      # Set work directory
      WORKDIR /app
      
      # Install system dependencies
      RUN apt-get update \
          && apt-get install -y --no-install-recommends \
              postgresql-client \
              build-essential \
              libpq-dev \
          && rm -rf /var/lib/apt/lists/*
      
      # Install "Python" dependencies
      COPY requirements.txt.
      RUN pip install --no-cache-dir -r requirements.txt
      
      # Copy project
      COPY..
      
      # Collect static files
      RUN "Python" manage.py collectstatic --noinput
      
      # Expose port
      EXPOSE 8000
      
      # Run the application
      CMD ["gunicorn", "--bind", "0.0.0.0:8000", "myproject.wsgi:application"]
      ```
  
    - **Kreiraj "docker-compose.yml"** :
  
      ```py
      version: '3.8'
      
      services:
        db:
          image: postgres:15
          volumes:
            - postgres_data:/var/lib/postgresql/data/
          environment:
            - POSTGRES_DB=myproject
            - POSTGRES_USER=myprojectuser
            - POSTGRES_PASSWORD=password
          ports:
            - "5432:5432"
      
        web:
          build:.
          command: gunicorn --bind 0.0.0.0:8000 myproject.wsgi:application
          volumes:
            -.:/app
          ports:
            - "8000:8000"
          depends_on:
            - db
          environment:
            - DEBUG=0
            - DATABASE_URL=postgresql://myprojectuser:password@db:5432/myproject
      
        nginx:
          image: nginx:alpine
          ports:
            - "80:80"
            - "443:443"
          volumes:
            -./nginx.conf:/etc/nginx/nginx.conf
            -./static:/app/static
          depends_on:
            - web
      
      volumes:
        postgres_data:
      ```

### Praćenje i posmatranje

- **Praćenje performansi aplikacije**

  Instalirajte alate za praćenje:

  ```sh
  pip install django-extensions django-debug-toolbar django-silk
  ```

- **Dodaj u "settings.py"**:

  ```py
  INSTALLED_APPS = [
      #... existing apps
      '"Django"_extensions',
      'debug_toolbar',
      'silk',
  ]
  
  MIDDLEWARE = [
      'silk.middleware.SilkyMiddleware',
      'debug_toolbar.middleware.DebugToolbarMiddleware',
      #... existing middleware
  ]
  
  # Silk configuration
  SILKY_PYTHON_PROFILER = True
  SILKY_PYTHON_PROFILER_BINARY = True
  ```

- **Konfiguracija evidentiranja**

  Poboljšano podešavanje evidentiranja u settings.py:

  ```py
  LOGGING = {
      'version': 1,
      'disable_existing_loggers': False,
      'formatters': {
          'verbose': {
              'format': '{levelname} {asctime} {module} {process:d} {thread:d} {message}',
              'style': '{',
          },
          'simple': {
              'format': '{levelname} {message}',
              'style': '{',
          },
      },
      'handlers': {
          'file': {
              'level': 'INFO',
              'class': 'logging.FileHandler',
              'filename': '/var/log/"Django"/myproject.log',
              'formatter': 'verbose',
          },
          'console': {
              'level': 'DEBUG',
              'class': 'logging.StreamHandler',
              'formatter': 'simple',
          },
      },
      'root': {
          'handlers': ['console', 'file'],
          'level': 'INFO',
      },
      'loggers': {
          '"Django"': {
              'handlers': ['console', 'file'],
              'level': 'INFO',
              'propagate': False,
          },
          'myproject': {
              'handlers': ['console', 'file'],
              'level': 'DEBUG',
              'propagate': False,
          },
      },
  }
  ```

### Skaliranje i visoka dostupnost

#### Horizontalno skaliranje sa balansiranjem opterećenja

Kako vaša "Django" aplikacija raste, moraćete da se skalirate van jednog servera kako biste podneli povećani saobraćaj. Horizontalno skaliranje podrazumeva dodavanje više aplikacijskih servera i raspodelu opterećenja među njima:

**Zašto je horizontalno skaliranje važno** :

- **Rukovanje saobraćajem** : Više servera može da obradi više istovremenih korisnika nego jedan server
- **Tolerancija na greške** : Ako jedan server otkaže, ostali nastavljaju da opslužuju zahteve
- **Geografska distribucija** : Serveri u različitim regionima smanjuju latenciju za globalne korisnike
- **Optimizacija resursa** : Različiti serveri mogu biti optimizovani za različita radna opterećenja

**Strategije balansiranja opterećenja** :

- **Kružna šema sistema** : Ravnomerno raspoređuje zahteve po serverima
- **Najmanje konekcija** : Rutira do servera sa najmanje aktivnih konekcija
- **Ponderisano** : Daje više saobraćaja moćnijim serverima
- **Provere ispravnosti** : Automatski uklanja nezdrave servere iz rotacije

**Višestruke instance "Gunicorn"-a** :

- Kreirajte više "Gunicorn" servisa na različitim portovima:

  "/etc/systemd/system/gunicorn-1.service"

  ```sh
  [Unit]
  Description=gunicorn daemon instance 1
  After=network.target
  
  [Service]
  User=sammy
  Group=www-data
  WorkingDirectory=/home/sammy/myprojectdir
  ExecStart=/home/sammy/myprojectdir/myprojectenv/bin/gunicorn \
            --workers 2 \
            --bind 127.0.0.1:8001 \
            myproject.wsgi:application
  
  [Install]
  WantedBy=multi-user.target
  ```

- **Konfiguracija "Nginx" Load Balancer-a**

  "/etc/nginx/sites-available/myproject"

  ```sh
  upstream "Django"_backend {
      least_conn;
      server 127.0.0.1:8000 weight=3;
      server 127.0.0.1:8001 weight=3;
      server 127.0.0.1:8002 weight=2;
      keepalive 32;
  }
  
  server {
      listen 80;
      server_name server_domain_or_IP;
  
      location / {
          proxy_pass http://"Django"_backend;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
      }
  }
  ```

#### Strategije skaliranja baze podataka

- **Pročitajte replike**

  Podesite "PostgreSQL" replike za čitanje za opterećenja sa velikim brojem čitanja:

  ```sh
  # On replica server
  sudo -u "Postgres" pg_basebackup -h primary_server -D /var/lib/postgresql/14/main -U replicator -v -P -W
  ```

- **Grupisanje veza**

  Implementirajte PgBouncer za objedinjavanje konekcija:

  ```sh
  sudo apt install pgbouncer
  ```

  Konfiguriši "/etc/pgbouncer/pgbouncer.ini":
  
  ```sh
  [databases]
  myproject = host=localhost port=5432 dbname=myproject
  
  [pgbouncer]
  listen_port = 6432
  listen_addr = 127.0.0.1
  auth_type = md5
  auth_file = /etc/pgbouncer/userlist.txt
  pool_mode = transaction
  max_client_conn = 100
  default_pool_size = 20
  ```
  
#### Strategije keširanja

Keširanje je ključno za visoko-performansne "Django" aplikacije. Smanjuje opterećenje baze podataka, poboljšava vreme odziva i omogućava vašoj aplikaciji da obradi više istovremenih korisnika:

Zašto je keširanje važno:

- **Smanjenje opterećenja baze podataka** : Keširani podaci smanjuju skupe upite u bazu podataka
- **Poboljšanje vremena odziva** : Keširani odgovori se prikazuju 10-100 puta brže od upita u bazi podataka
- **Isplativost** : Manje upita bazi podataka znači niže troškove infrastrukture
- **Korisničko iskustvo** : Brže učitavanje stranica poboljšava zadovoljstvo korisnika i SEO rangiranje

- **Keširanje slojeva**:

  - **Keširanje na nivou aplikacije** : "Django"-ov okvir za keširanje rezultata pregleda i upita baze podataka
  - **Keširanje upita baze podataka** : Ugrađeno keširanje rezultata upita u "PostgreSQL"-u
  - **Obrnuto proksi keširanje** : "Nginx" keširanje za statički i dinamički sadržaj
  - **CDN keširanje** : Edge keširanje za globalnu isporuku sadržaja

#### Keširanje Redisa

Redis je skladište podataka u memoriji koje je savršeno za keširanje u "Django"u jer je brzo, podržava složene strukture podataka i može da podnese visoku konkurentnost:

- **Zašto Redis za "Django" keširanje**:

  - **Brzina** : Skladištenje u memoriji obezbeđuje vreme odziva od mikrosekunde
  - **Perzistentnost** : Može da sačuva podatke na disku radi izdržljivosti
  - **Strukture podataka** : Podržava stringove, liste, skupove i heševe za složene potrebe keširanja
  - **Klasterisanje** : Može se klasterisati za visoku dostupnost i skalabilnost
  - **Upravljanje memorijom** : Politike automatskog izbacivanja sprečavaju prelivanje memorije

  ```sh
  sudo apt install redis-server
  pip install redis django-redis
  ```

  Ažuriranje settings.py:

  ```py
  CACHES = {
      'default': {
          'BACKEND': '"Django"_redis.cache.RedisCache',
          'LOCATION': 'redis://127.0.0.1:6379/1',
          'OPTIONS': {
              'CLIENT_CLASS': '"Django"_redis.client.DefaultClient',
          }
      }
  }
  
  # Session storage
  SESSION_ENGINE = '"Django".contrib.sessions.backends.cache'
  SESSION_CACHE_ALIAS = 'default'
  ```

- **Keširanje Nginx-a**

  Implementirajte "Nginx" keširanje za statički i dinamički sadržaj:

  ```sh
  # Cache zone definition
  proxy_cache_path /var/cache/nginx levels=1:2 keys_zone=my_cache:10m max_size=10g inactive=60m use_temp_path=off;
  
  server {
      #... existing configuration
      
      location / {
          proxy_cache my_cache;
          proxy_cache_valid 200 302 10m;
          proxy_cache_valid 404 1m;
          proxy_cache_use_stale error timeout updating http_500 http_502 http_503 http_504;
          proxy_cache_lock on;
          
          proxy_pass http://"Django"_backend;
          #... other proxy settings
      }
  }
  ```

## Često postavljana pitanja (FAQs)

### Zašto koristiti "Gunicorn" sa "Django"-om umesto "Django"-ovog ugrađenog razvojnog servera?

"Django"ov ugrađeni razvojni server je dizajniran samo za razvoj i testiranje. Jednonitan je, nije optimizovan za performanse i nedostaju mu funkcije spremne za proizvodnju. Gunikorn pruža:

- **Podrška za procese sa više radnika** : Obrađuje više zahteva istovremeno
- **Stabilnost proizvodnog nivoa** : Dizajnirana za rad 24/7
- **Usklađenost sa WSGI** : Standardni interfejs za "Python" veb aplikacije
- **Upravljanje procesima** : Automatsko ponovno pokretanje radnika i balansiranje opterećenja
- **Bezbednosne karakteristike** : Bolje rukovanje istovremenim vezama i obrada zahteva

Za proizvodna okruženja, "Gunicorn" može da obradi hiljade istovremenih zahteva, dok bi se "Django"-ov razvojni server mučio čak i sa umerenim saobraćajem.

### Mogu li da koristim "PostgreSQL" umesto SQLite-a za "Django" produkcione instalacije?

Apsolutno, i toplo se preporučuje za produkcijske primene. Evo zašto je "PostgreSQL" bolji od SQLite-a za produkciju:

Prednosti "PostgreSQL"-a:

- **Istovremene veze** : Podržava više istovremenih veza sa bazom podataka
- **Usklađenost sa ACID-om** : Puna podrška za transakcije sa mogućnostima vraćanja unazad
- **Napredni tipovi podataka** : JSON, nizovi, prilagođeni tipovi i pretraga celog teksta
- **Performanse** : Optimizovano za aplikacije sa velikim prometom uz pravilno indeksiranje
- **Skalabilnost** : Može efikasno da obrađuje velike skupove podataka i složene upite
- **Bezbednost** : Napredno upravljanje korisnicima, bezbednost na nivou redova i šifrovanje

Ograničenja SQLite-a:

- **Ograničenje za jednog pisca**
- **Nema istovremenih operacija pisanja**
- **Ograničena skalabilnost**
- **Podrška za osnovne tipove podataka**

Za proizvodne "Django" aplikacije, "PostgreSQL" pruža pouzdanost, performanse i funkcije neophodne za implementacije na nivou preduzeća.

### Kako da konfigurišem "Nginx" za "Django" na "Ubuntu"-u za optimalne performanse?

Konfiguracija "Nginx"-a za "Django" uključuje nekoliko strategija optimizacije:

#### Statičko rukovanje datotekama

```sh
# static managed files
location /static/ {
    root /path/to/your/project;
    expires 1y;
    add_header Cache-Control "public, immutable";
    access_log off;
}

#### Gzip kompresija
gzip on;
gzip_vary on;
gzip_min_length 1024;
gzip_comp_level 6;
gzip_types text/plain text/css application/json application/javascript;

#### Konfiguracija proksija
location / {
    proxy_pass http://unix:/run/gunicorn.sock;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_connect_timeout 30s;
    proxy_send_timeout 30s;
    proxy_read_timeout 30s;
}

#### Bezbednosni hederi
add_header X-Frame-Options "SAMEORIGIN" always;
add_header X-Content-Type-Options "nosniff" always;
add_header X-XSS-Protection "1; mode=block" always;
```

### Koja je razlika između "Gunicorn"-a i uWSGI-ja za "Django" implementaciju?

Oba su WSGI serveri, ali imaju različite prednosti:

- **Gunicorn** :

  - **Jednostavnost** : Lako se konfiguriše i implementira
  - **Python-nativno** : Napisano u Python-u, dobro se integriše sa "Python" ekosistemom
  - **Upravljanje radnicima** : Automatsko upravljanje procesima radnika
  - **Efikasnost memorije** : Manja zauzetost memorije
  - **Najbolje za** : Većinu "Django" aplikacija, posebno one sa umerenim saobraćajem

- **uWSGI** :

  - **Performanse** : Generalno brže za aplikacije sa velikim prometom
  - **Jezička agnostičnost** : Podržava više jezika (Python, Ruby, Perl, itd.)
  - **Napredne funkcije** : Više opcija konfiguracije i dodataka
  - **Potrošnja memorije** : Veća potrošnja memorije, ali bolje performanse
  - **Najbolje za** : Primene sa velikim prometom koje zahtevaju maksimalne performanse

**Preporuka** : Počnite sa "Gunicorn"-om za većinu "Django" aplikacija. Razmotrite uWSGI samo ako imate specifične zahteve za performanse koje "Gunicorn" ne može da ispuni.

### Kako da obezbedim svoju "Django" aplikaciju pomoću SSL-a na "Ubuntu"-u?

Obezbeđivanje vaše "Django" aplikacije pomoću SSL-a uključuje nekoliko koraka:

- **Instalirajte Certbot** :

  ```sh
  sudo apt install certbot python3-certbot-nginx
  ```

- **Nabavite SSL sertifikat** :

  ```sh
  sudo certbot --nginx -d yourdomain.com
  ```

- **Konfigurišite "Django" za HTTPS** :

```py
# settings.py
SECURE_SSL_REDIRECT = True
SECURE_HSTS_SECONDS = 31536000
SECURE_HSTS_INCLUDE_SUBDOMAINS = True
SECURE_HSTS_PRELOAD = True
SECURE_CONTENT_TYPE_NOSNIFF = True
SECURE_BROWSER_XSS_FILTER = True
SESSION_COOKIE_SECURE = True
CSRF_COOKIE_SECURE = True
```

- **Ažurirajte konfiguraciju "Nginx"-a** :

```sh
server {
    listen 443 ssl http2;
    server_name yourdomain.com;
    
    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;
    
    # SSL configuration
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512;
    ssl_prefer_server_ciphers off;
}
```

- **Podesite automatsko obnavljanje** :

```sh
sudo crontab -e # Add: 0 12 * * * /usr/bin/certbot renew --quiet
```

  Ovo osigurava da je vaša "Django" aplikacija pravilno zaštićena SSL/TLS enkripcijom za svu komunikaciju.
  
## Zaključak

U ovom sveobuhvatnom tutorijalu, uspešno ste podesili "Django" aplikaciju spremnu za produkciju sa "PostgreSQL"-om, "Nginx"-om i "Gunicorn"-om na "Ubuntu"-u. Naučili ste kako da:

- Konfigurišite robusnu postavku baze podataka pomoću "PostgreSQL"-a za produkcijska okruženja
- Implementirajte napredne optimizacije performansi za "Gunicorn" i "Nginx"
- Integrišite mogućnosti veštačke inteligencije i moderne strategije primene koristeći Docker
- Postavite sveobuhvatne sisteme za praćenje i evidentiranje
- Obezbedite svoju aplikaciju SSL/TLS enkripcijom
- Skalirajte svoju aplikaciju za scenarije sa velikim prometom

"Django" pojednostavljuje kreiranje projekata i aplikacija pružajući mnoge uobičajene delove, omogućavajući vam da se fokusirate na jedinstvene elemente. Korišćenjem naprednog lanca alata i tehnika optimizacije opisanih u ovom članku, možete primeniti skalabilne, bezbedne i inteligentne aplikacije koje mogu da obrade saobraćaj na nivou preduzeća.

Tražite još strategija za implementaciju "Django"-a i naprednih konfiguracija? Pogledajte ove preporučene tutorijale:

- Kako koristiti "PostgreSQL" sa vašom "Django" aplikacijom na "Ubuntu" 22.04
- Kako koristiti "PostgreSQL" sa vašom "Django" aplikacijom na "Ubuntu" 20.04
- SQLite vs MySQL vs "PostgreSQL": Poređenje sistema za upravljanje relacionim bazama podataka

Istražite ove tutorijale za dublji uvid i sledeće korake u vašem putu implementacije "Django"-a.
