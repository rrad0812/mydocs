
# Ubuntu - Upravljanje softverom

Ako ste novi u Ubuntuu, možda se pitate šta da radite nakon instalacije. Na kraju krajeva, Ubuntu se može beskrajno prilagoditi vašim potrebama. U Ubuntuu postoje dve vrste softvera: i – učićemo o oba!

Da bismo vam pomogli da izvučete maksimum iz svog Ubuntu iskustva, ovaj tutorijal će vas provesti kroz upravljanje softverom na vašem Ubuntu računaru. Ovaj tutorijal možete završiti koristeći Ubuntu Server ili Ubuntu Desktop.

Da bismo izbegli pravljenje izmena na vašem računaru, podesićemo virtuelnu mašinu (VM), koja će nam pružiti bezbedno okruženje za pokretanje komandi. `Multipas` je odličan za brzo kreiranje virtuelnih mašina za Ubuntu, pa ćemo ga koristiti.

## Prethodni zahtevi

- Znanje:
  Ništa! Ne morate čak ni da koristite Ubuntu mašinu – Multipass će nam
  dati Ubuntu okruženje za igru.

- Hardver
  Podrazumevanoj Multipass virtuelnoj mašini će biti potrebno 5 GB na disku i 1GiB memorije.

- Softver - Multipass
  
  - Na Ubuntuu možete instalirati `Multipas` pokretanjem sledeće komande u
    terminalu (prozor terminala možete otvoriti pritiskom na `Ctrl + Alt + T` zajedno):

    ```sh
    sudo snap install multipass
    ```

    Ili ga možete instalirati direktno sa `Multipass` stranice u onlajn
    prodavnici Snap datoteka (obavezno izaberite "najnoviju/stabilnu" verziju iz padajućeg menija pored dugmeta za instalaciju).

  - Multipas se može instalirati na Windows, Mac i druge Linux
    distribucije koristeći ova uputstva .

## Napravite virtuelnu mašinu

Kada instalirate i pokrenete `Multipass`, pokretanje nove virtuelne mašine je jednostavno. Pokrenimo virtuelnu mašinu koristeći izdanje Ubuntu 24.04 LTS ( kodni naziv `noble` ) i dajmo našoj virtuelnoj mašini ime `tutorial` pomoću sledeće komande u terminalu:

```sh
multipass launch noble --name tutorial
```

`Multipas` će preuzeti najnoviju dnevnu sliku i kreirati virtuelnu mašinu za nas. Može potrajati malo vremena, u zavisnosti od brzine vaše internet veze.

Ubuntu  je kolekcija datoteka koje su nam potrebne za instaliranje i pokretanje Ubuntua. Ne moramo da navodimo "server" ili "desktop" bilo gde u našoj komandi, jer je slika ista za oba. Jedina razlika između Ubuntu servera i Ubuntu desktopa je podskup softverskih paketa koje koristimo iz Ubuntu arhive - ovo ćemo videti kasnije!

Sada možemo pristupiti pokretanju virtuelne mašine:

```sh
multipass shell tutorial
```

Dobićemo poruku "Dobrodošli u Ubuntu". Obratite pažnju da kada pokrenemo ovu komandu, korisničko ime terminala se menja u ubuntu a ime hosta se menja u tutorial:

```sh
ubuntu@tutorial
```

Ovo pokazuje da smo unutar virtuelne mašine i da je to mesto gde ćemo izvršavati sve naše komande.

## Ažuriranje sistema pomoću APT-a

Prva stvar koju uvek želimo da uradimo sa novim sistemom (bilo da je u pitanju virtuelna mašina, kontejner, bare metal ili instanca u oblaku) jeste da se uverimo da imamo najnovije verzije svih prethodno instaliranih softvera.

Debian paketi, obično nazivani `debs`, su standardni format softverskih paketa u Ubuntuu. Mogu se identifikovati po `.deb` ekstenziji datoteke.

Svaka Linuks distribucija ima svoj preferirani  za instaliranje, ažuriranje i uklanjanje paketa. U Ubuntuu, podrazumevani menadžer paketa je `Advanced Packaging Tool` ( skraćeno `APT` ).

APT upravlja svim vašim sistemskim softverom (i drugim deb paketima). On pruža interfejs ka repozitorijumu Ubuntu arhive, tako da može pristupiti i bazi podataka svih paketa dostupnih u Ubuntu-u i sredstvima za rukovanje samim paketima.

Postoje dve APT komande koje su nam potrebne za ažuriranje našeg sistema: `update` i `upgrade`, koje ćemo uvek izvršavati tim redosledom.

### apt update

Komanda `apt update` se odnosi na bazu podataka. Sve ispravke grešaka u paketu (ili nove verzije od vašeg poslednjeg ažuriranja) biće sačuvane u metapodacima o tom paketu u bazi podataka ( indeks paketa ).

Kada pokrenemo `update` komandu, ona ažurira APT bazu podataka na našem računaru, preuzimajući najnovije dostupne metapodatke iz indeksa paketa:

```sh
sudo apt update
```

Videćemo izlaz poput ovog:

```sh
Hit:1 http://security.ubuntu.com/ubuntu noble-security InRelease
Hit:2 http://archive.ubuntu.com/ubuntu noble InRelease
Hit:3 http://archive.ubuntu.com/ubuntu noble-updates InRelease
Hit:4 http://archive.ubuntu.com/ubuntu noble-backports InRelease
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
88 packages can be upgraded. Run 'apt list --upgradable' to see them.
```

Kao što vidimo, proverava („pogađa“) različite arhive ( džepove ) iz kojih mogu doći ažuriranja za izdanje 24.04 LTS ( noble-security , noble , noble-updates i noble-backports – zapamtite ih, jer ćemo se vratiti na njih kasnije ). Pronašao je neke pakete koji se mogu
nadograditi na novije verzije. Ako želimo da vidimo koji su to paketi, možemo pokrenuti komandu:

```sh
apt list --upgradable
```

Izlaz nam govori:

- naziv paketa i odakle će ažuriranje doći (npr. base-files/noble-updates ),

- najnovija dostupna verzija paketa (npr. 13ubuntu10.1 )

- verziju hardvera za koju je ažuriranje namenjeno (npr amd64 .) i

- koja je verzija paketa trenutno instalirana (npr. 13ubuntu10 )

Konkretni paketi uključeni u ovu listu se menjaju tokom vremena, tako da će tačno prikazani paketi biti drugačiji, ali će izlaz biti strukturiran ovako:

```sh
Listing... Done
base-files/noble-updates 13ubuntu10.1 amd64 [upgradable from: 13ubuntu10]
bsdextrautils/noble-updates 2.39.3-9ubuntu6.1 amd64 [upgradable from: 2.39.3-9ubuntu6]
bsdutils/noble-updates 1:2.39.3-9ubuntu6.1 amd64 [upgradable from: 1:2.39.3-9ubuntu6]
bsdutils/noble-updates 1:2.39.3-9ubuntu6.1 amd64 [upgradable from: 1:2.39.3-9ubuntu6]
cloud-init/noble-updates 24.2-0ubuntu1~24.04.2 all [upgradable from: 24.1.3-0ubuntu3.3]
[...]
```

### apt upgrade

Komanda `apt upgrade` se odnosi na pkete na vašem sistemu. Ona pregleda metapodatke u indeksu paketa koje smo upravo ažurirali, pronalazi pakete sa dostupnim nadogradnjama i prikazuje ih nam. Kada proverimo predloženu nadogradnju i budemo zadovoljni da nastavimo, instaliraće nam novije verzije.

Nakon što smo ažurirali bazu podataka (što smo uradili pokretanjem `apt update`), možemo nadograditi pakete na njihove najnovije verzije pokretanjem:

```sh
sudo apt upgrade
```

Kada pokrenemo ovu komandu, od nas će se tražiti da potvrdimo da li je rezime predloženih promena koje će biti napravljene na našem sistemu ono što želimo.

Hajde da otkucamo Y, a zatim pritisnemo Enter da potvrdimo da želimo to, i onda će se nadogradnja nastaviti. Ovo može potrajati nekoliko minuta.

> [!Note]
>
> Možete koristiti `-y` zastavicu, što je skraćenica za `--assume-yes`.
> A ako pokrenemo komandu,
>
> sudo apt upgrade -y man
>
> nadogradnja bi se nastavila bez traženja potvrde. Skraćene verzije
> zastavica su uobičajene – za većinu paketa možete proveriti koje su
> zastavice ekvivalentne koristeći stranice uputstva ili koristeći
> komandu, kao što ćemo videti kasnije.

U izlazu ćemo videti odakle se preuzima nadogradnja za svaki paket. Na primer:

```sh
apt upgrade

Get:1 <http://archive.ubuntu.com/ubuntu> noble-updates/main amd64 libopeniscsiusr amd64 2.1.9
```

APT kombinuje različite elemente; naziv paketa ( libopeniscsiusr ), verziju ( 2.1.9-3ubuntu5.1 ), izvor ( noble-updates/main ) itd. u jedan URL koji se može da koristi za preuzimanje. Paket se zatim raspakuje, a nadogradnja se primenjuje na sistem.

> [!Note]
>
> Ove komande nadograđuju samo pakete za izdanje Ubuntua koje koristimo
> (24.04 LTS). Ako bismo želeli da nadogradimo ceo sistem na sledeće
> izdanje Ubuntua (npr. sa 22.04 LTS na 24.04 LTS), koristili bismo
> `do-release-upgrade` komandu. Pogledajte vodič o tome kako nadograditi > vaše izdanje za više informacija.

Važno je znati da će `apt upgrade` obraditi samo pakete koji se mogu direktno nadograditi. Ako paket ima probleme zavisnosti (tj. verzija koju imate "zavisi" od drugih paketa koje takođe treba dodati, nadograditi ili ukloniti), umesto toga biste morali da koristite:

```sh
sudo apt dist-upgrade
```

`dist-upgrade` komanda je u stanju da reši sukobe između verzija paketa, ali bi mogla da dovede do uklanjanja nekih paketa – pa iako je `apt upgrade` bezbedna za upotrebu bez nadzora (na primer, u skripti), trebalo bi da `dist-upgrade` koristite samo kada možete da joj obratite pažnju.

### Pretraga sa apt

Sada kada smo ažurirani, možemo početi sa istraživanjem! Kao i sa bilo kojom drugom bazom podataka, možemo pretraživati listu dostupnih paketa koristeći APT kako bismo pronašli softver. Recimo, na primer, da želimo da pronađemo veb server. Možemo pokrenuti sledeću komandu:

```sh
apt search webserver
```

Ovo će nam vratiti dugačku listu svih paketa "webserver" koje može da pronađe. Ali neki od opisa zapravo ne sadrže tekst "webserver" – kao u ovom delu liste:

```sh
inotify-tools/noble 3.22.6.0-4 amd64
  command-line programs providing a simple interface to inotify

ipcalc/noble 0.51-1 all
  parameter calculator for IPv4 addresses

iwatch/noble 0.2.2-10 all
  realtime filesystem monitoring program using inotify
```

Možemo koristiti `apt show ipcalc` za pregled opisa i rezimea bilo kog paketa, pa hajde da detaljnije pogledamo sa naše liste:

```sh
apt show ipcalc
```

Rezime je zamenjen sa [...] radi kratkoće, ali možemo videti da se tekst "vebserver" nalazi u dugom opisu polja "Opis".

```sh
Package: ipcalc
Version: 0.51-1
[...]
APT-Sources: http://archive.ubuntu.com/ubuntu noble/universe amd64 Packages
Description: parameter calculator for IPv4 addresses
 ipcalc takes an IPv4 address and netmask and calculates the resulting
 broadcast, network, Cisco wildcard mask, and host range. By giving a
 second netmask, you can design sub- and supernetworks. It is also
 intended to be a teaching tool and presents the results as
 easy-to-understand binary values.
 .
 Originally, ipcalc was intended for use from the shell prompt, but a
 CGI wrapper is provided to enable colorful HTML display through a
 webserver.
 You can find it in /usr/share/doc/ipcalc/examples directory.
```

Na mnogim mestima ćete videti referencu na `apt-get` i `apt-cache` umesto `apt`. Istorijski gledano, delu baze podataka APT-a se pristupalo pomoću `apt-cache` (npr. `apt-cache show ipcalc`), a za deo paketa APT-a se koristilo `apt-get` (npr. `apt-get install ipcalc`).

APT je nedavno pojednostavljen, tako da iako koristi `apt-get` i `apt-cache` komande "iza kulisa" (i ove komande i dalje rade), ne moramo da brinemo o tome da li ćemo pamtiti koju komandu da koristimo – možemo  direktno koristiti onu `apt` koja nam je praktičnija. Da bismo
saznali više o ovim paketima i kako da ih koristimo (ili zapravo bilo koji paket u Ubuntu-u!), možemo pogledati stranice uputstva.

Pokrenite `man apt`, `man apt-get` ili `man apt-cache` u terminalu da biste pristupili priručnicima za ove pakete na komandnoj liniji ili
pogledajte isti sadržaj na stranicama onlajn priručnika.

## Instaliranje deb paketa

Za primere u ovom odeljku, koristićemo popularni paket veb servera, `Apache2`.

APT nam daje mnogo detalja o tome šta će biti uključeno u instalaciju i uvek je važno razumeti implikacije komande pre nego što je pokrenemo. Pažljivo ćemo pogledati detalje koje nam APT daje, tako da moramo biti oprezni u ovom odeljku.

Kada pokrenemo komandu koja nas pita "Do you want to continue? [Y/n]", obavezno otkucajte "N" i zatim pritisnite Enter, osim
ako nije drugačije naznačeno – ovo će nam omogućiti da vidimo izlaz komandi bez pravljenja promena koje zatim treba poništiti.

Instaliranje deb paketa pomoću APT-a se vrši pomoću `apt install` komande. Možemo instalirati ili jedan paket ili listu paketa odjednom, uključivanjem njihovih imena u listu odvojenu razmacima nakon `install` komande, u ovom formatu:

```sh
sudo apt install <package 1> <package 2> <package 3>
```

### O sudo komandi

Već smo videli sudo prefiks u nekoliko komandi i možda se pitate o čemu se radi. U Linuksu, sistemski zadaci (kao što je instaliranje softvera) zahtevaju povišena administratorska prava.

Ova prava se često nazivaju "root prava", a korisnik sa root pravima se naziva `root` korisnik.

Međutim, može biti opasno koristiti računar kao `root` korisnik – pošto vam root pristup daje potpunu kontrolu nad sistemom sve vreme, omogućava vam da menjate ili brišete važne sistemske datoteke. Veoma je lako slučajno pokvariti sistem u `root` režimu!

Umesto toga, koristimo sudo (što je skraćenica od `superuser do`). Ova komanda je bezbednosna funkcija koja redovnim korisnicima daje privremene (po komandi) administratorske privilegije za izvršavanje sistemskih promena. I dalje je važno da uvek razumemo šta komanda radi pre nego što je pokrenemo, ali korišćenje znači `sudo` da namerno ograničavamo sve potencijalne greške na jednu komandu.

### O zavisnostima

Kao što smo ranije nagovestili, paketi često dolaze sa zavisnostima – drugim paketima koji su vašem paketu potrebni da bi mogao da funkcioniše. Ponekad, paket može zavisiti od određene verzije drugog paketa. Ako paket ima zavisnosti, instaliranje paketa putem `apt` će takođe instalirati sve zavisnosti, što osigurava da softver može pravilno da funkcioniše.

APT nam govori kako će rešiti eventualne sukobe ili probleme zavisnosti kada pokrenemo install komandu. Hajde da ovo sami isprobamo, ali zapamtite, još ne želimo da nastavimo sa instalacijom, pa ćemo otkucati N kada nas pita da li želimo da nastavimo:

```sh
sudo apt install apache2
```

Izlaz bi trebalo da bude sličan onom ispod. On nam govori:

- koje pakete imamo, ali nam nisu potrebni (o tome ćemo govoriti u odeljku „automatsko uklanjanje“),

- dodatni paketi koji će biti instalirani (to su naše zavisnosti),

- predložene pakete (o kojima ćemo govoriti u sledećem odeljku) i

- rezime novih paketa koji će biti prisutni na sistemu nakon završetka instalacije (što je u ovom slučaju sam `apache2` sistem i sve njegove zavisnosti).

```sh
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
The following additional packages will be installed:
  apache2-bin apache2-data apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libaprutil1-lda
Suggested packages:
  apache2-doc apache2-suexec-pristine | apache2-suexec-custom www-browser
The following NEW packages will be installed:
  apache2 apache2-bin apache2-data apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libapru
0 upgraded, 10 newly installed, 0 to remove and 2 not upgraded.
Need to get 2084 kB of archives.
After this operation, 8094 kB of additional disk space will be used.

Do you want to continue? [Y/n]
```

Hajde da pokušamo da razumemo ovaj izlaz.

#### Tipovi zavisnosti

Odnos između paketa i bilo kojih drugih paketa prati Debian politiku o binarnim zavisnostima, koju ćemo ovde ukratko pogledati. Najčešće na koje možete naići su: depends, recommends , i suggests (mada postoje i drugi!), pa ćemo pogledati ova tri.

- `depedens`: Apsolutno neophodno, paket neće raditi bez njega. Ako pokušamo da uklonimo
paket od kog zavisi drugi, oba će biti uklonjena!

- `recommends` : Jako zavisno, ali nije apsolutno neophodno (što znači da će paket bolje raditi
sa njim, ali može i bez njega)

- `suggested` : Nije potrebno, ali može na neki način poboljšati korisnost paketa.

Možemo videti, koristeći `apt show`, tačno koji paketi spadaju u svaku od ovih kategorija. Hajde ponovo da upotrebimo Apache2 kao primer:

```sh
apt show apache2
```

Ako pogledamo samo odeljke o zavisnostima, možemo videti da je `ssl-cert` preporučeni paket:

```sh
[...]
Provides: httpd, httpd-cgi
Pre-Depends: init-system-helpers (>= 1.54~)
Depends: apache2-bin (= 2.4.58-1ubuntu8.4), apache2-data (= 2.4.58-1ubuntu8.4), apache2-uti
Recommends: ssl-cert
Suggests: apache2-doc, apache2-suexec-pristine | apache2-suexec-custom, www-browser, ufw
[...]
```

U Ubuntuu, podrazumevana konfiguracija `apt install` je podešena da instalira `suggested` pakete zajedno sa `depends`, tako da kada smo pokrenuli `apt install apache2` komandu, `ssl-cert` je bio uključen u predložene pakete za instaliranje (iako je samo preporučen, a ne strogo neophodan).

Ovo ponašanje možemo poništiti tako što ćemo `--no-install-recommends` zastavicu proslediti našoj komandi, ovako:

```sh
sudo apt install apache2 --no-install-recommends
```

Tada izlaz postaje sledeći ( ponovo otkucajte N na promptu da biste za sada izbegli instaliranje):

```sh
[...]
The following additional packages will be installed:
  apache2-bin apache2-data apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libaprutil1-lda
Suggested packages:
  apache2-doc apache2-suexec-pristine | apache2-suexec-custom www-browser
Recommended packages:
  ssl-cert
The following NEW packages will be installed:
  apache2 apache2-bin apache2-data apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libapru
0 upgraded, 9 newly installed, 0 to remove and 25 not upgraded.
[...]
```

Sada vidimo da `ssl-cert` se pominje samo kao preporučeni paket, ali je isključen sa liste paketa koji treba instalirati.

Postoji i druga zastavica koju bismo mogli da prosledimo – `--install-suggests` zastavica.
Ovo neće instalirati samo stroge zavisnosti i preporučene pakete, već i predložene pakete. Iz našeg prethodnog rezultata, ne izgleda previše, zar ne? To su samo četiri dodatna paketa.

Ali zapravo, ako pokrenemo ovu komandu:

```sh
sudo apt install apache2 --install-suggests
```

Sada postoji izuzetno duga lista predloženih paketa (koju neću ovde prikazati, ali možete sami da isprobate!). U stvari, broj predloženih paketa je toliko dugačak da nema dovoljno prostora u ovoj virtuelnoj mašini da ih sve instalira, tako da nam neće ni dati mogućnost da nastavimo:

```sh
[...]
0 upgraded, 4598 newly installed, 2 to remove and 0 not upgraded.
Need to get 7415 MB of archives.
After this operation, 19.6 GB of additional disk space will be used.
E: You don't have enough free space in /var/cache/apt/archives/.
```

To je zato što svaki od ovih predloženih paketa takođe dolazi sa sopstvenim listama zavisnosti, uključujući predložene pakete, koji bi svi takođe bili instalirani. Verovatno je jasno zašto ovo nije podrazumevano podešavanje!

#### Šta ako uklonimo zavisnosti?

Kasnije ćemo detaljnije objasniti uklanjanje paketa, ali za sada, hajde da vidimo šta se dešava ako uklonimo potrebnu zavisnost. Prvo, trebalo bi (konačno!) da instaliramo apache2 paket.

Pokrenimo sledeću komandu ponovo, ali ovog puta kada nas pitaju da li želimo da nastavimo pritisnimo Y, a zatim Enter da potvrdimo, i APT će instalirati paket:

```sh
sudo apt install apache2
```

Jedna od potrebnih zavisnosti je `apache2-data` paket. Pokušajmo da ga uklonimo koristeći `apt remove`:

```sh
sudo apt remove apache2-data
```

Još jednom, apt neće nastaviti bez potvrde, pa dobijamo sledeći izlaz – pogledajmo pre nego što bilo šta izaberemo:

```sh
[...]
The following packages were automatically installed and are no longer required:
  apache2-bin apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libaprutil1-ldap libaprutil1
Use 'sudo apt autoremove' to remove them.
The following packages will be REMOVED:
  apache2 apache2-data
0 upgraded, 0 newly installed, 2 to remove and 2 not upgraded.
After this operation, 1342 kB disk space will be freed.
Do you want to continue? [Y/n]
```

Hajde da ovo malo razložimo, jer postoje neke suptilne razlike koje želimo da razumemo pre nego što nastavimo.

- "Sledeći paketi su automatski instalirani i više nisu potrebni"

  To su bile druge zavisnosti koje `apache2` su bile potrebne, ali nijedna od njih ne zavisi od `apache2-data`, tako da čak i ako ih uklonimo apache2, apache2-data one bi i dalje bile funkcionalne – jednostavno ih ne koriste drugi instalirani paketi... i stoga nemaju razloga da više budu tamo. Neće biti uklonjene, APT nam korisno govori da bismo bili svesni njih.

- "Sledeći paketi će biti UKLONJENI"

  Ovo su paketi koji će biti direktno uklonjeni - rekli smo APT-u da želimo da uklonimo `apache2-data`, tako da očekujemo da će i to biti uključeno, ali će se ukloniti i apache2! To je zato što `apache2-data` je neophodna zavisnost i `apache2` neće uopšte funkcionisati bez nje.

Hajde sada Y da potvrdimo da želimo da uklonimo ovu zavisnost.

> [!Warning]
>
> Uklanjanje zavisnosti može, u najgorem slučaju, dovesti do toga da sistem postane neupotrebljiv  
  – uvek treba biti oprezan kada to radite. Ako uklonite zavisnost koja je deo lanca, uklanjanja će se kaskadno širiti po lancu kako se svaka zavisnost i paket koji zavisi od nje budu uklanjali. Možete na kraju ukloniti više nego što ste prvobitno očekivali!

#### Autoremove zavisnosti

Dakle, uklonili smo pakete `apache2` i `apache2-data`, ali ostale zavisnosti koje su instalirane pored `apache2` su i dalje tu. Izlaz naše remove komande nam je dao naznaku kako da se nosimo sa ovim suvišnim paketima – `autoremove` komanda:

```sh
sudo apt autoremove
```

Kada pokrenemo ovu komandu, apt ponovo nam se prikazuje rezime operacije koju smo tražili, ali hajde da N za sada izaberemo kada nas pita da li želimo da nastavimo:

```sh
[...]
The following packages will be REMOVED:
  apache2-bin apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libaprutil1-ldap libaprutil1
0 upgraded, 0 newly installed, 8 to remove and 2 not upgraded.
After this operation, 6751 kB disk space will be freed.
Do you want to continue? [Y/n]
```

Možda se pitate zašto ne moramo da navodimo nikakve pakete kada pozivamo `autoremove` komandu – na kraju krajeva, upravo smo se bavili paketima povezanim sa `apache2`. To je zato što će apt proveriti sve pakete na vašem sistemu. Ispituje stablo zavisnosti i ako prvobitni razlog za instaliranje paketa više ne postoji (tj. nije ničemu potreban), biće označen za automatsko uklanjanje.

Ali!

Možda ćemo u budućnosti deinstalirati `Apache2` bez deinstaliranja suvišnih paketa u tom trenutku. Možda smo pronašli drugu upotrebu za `ssl-cert`, možda u skripti koja koristi SSL sertifikate. Pa kako možemo zadržati `ssl-cert paket`, iako je označen za automatsko uklanjanje?

Ovaj problem možemo rešiti i ukloniti `ssl-cert` paket ručnom instalacijom:

```sh
sudo apt install ssl-cert
```

Ovim se podešava ssl-cert ručna  . Mogli bismo se zapitati „zašto nas APT ovog puta nije tražio da potvrdimo bilo šta?“. U ovom slučaju, to je zato što ssl-cert je već prisutan na sistemu, tako da APT ne mora ništa novo da instalira.

```sh
[...]
ssl-cert is already the newest version (1.1.2ubuntu1).
ssl-cert set to manually installed.
The following packages were automatically installed and are no longer required:
  apache2-bin apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libaprutil1-ldap libaprutil1
  apache2-bin apache2-utils libapr1t64 libaprutil1-dbd-sqlite3 libaprutil1-ldap libaprutil1
Use 'sudo apt autoremove' to remove them.
0 upgraded, 0 newly installed, 0 to remove and 2 not upgraded.
```

Ako `ssl-cert` je paket ručno instaliran na našem sistemu, od strane nas, onda `apt` zna da je paket željen i možemo videti da je uklonjen sa liste za automatsko uklanjanje, tako da ga naše sledeće automatsko uklanjanje neće deinstalirati. Hajde da ovo testiramo, samo da bismo bili sigurni!

```sh
sudo apt autoremove
```

Ovog puta ćemo izabrati "Y" kada se to od nas zatraži, a zatim možemo pokrenuti da brzo vidimo da li je naš paket još uvek na sistemu: apt list ssl-cert ssl-cert

```sh
apt list ssl-cert
```

Što nam daje ovaj izlaz, potvrđujući da ssl-cert je trenutno instalirano:

```sh
Listing... Done
ssl-cert/noble,now 1.1.2ubuntu1 all [installed]
```

Ako ste radoznali, možete pokrenuti `apt list apache2` da vidite kako se izlaz razlikuje za paket koji je jednom instaliran, a zatim uklonjen!

U svakom slučaju, nismo sasvim završili sa `Apache2` paketom, pa hajde da ga ponovo instaliramo:

```sh
sudo apt install apache2
```

I ovaj put izaberite "Y" potvrdu kada se to zatraži.

## Prilagodjena konfiguracija

Generalno, podrazumevana konfiguracija paketa bi trebalo da radi dobro i da radi "odmah po pokretanju“ kada se instalira. Ali gotovo je neizbežno da ćemo, pre ili kasnije, želeti da prilagodimo paket kako bi bolje odgovarao našim potrebama.

Pre nego što pokušamo da prilagodimo paket, verovatno bi trebalo da pogledamo koje datoteke su uključene u njega. To možemo proveriti pomoću `dpkg(1)` , što je Debian menadžer paketa. Iako se APT sada češće koristi za osnovno rukovanje paketima, `dpkg` zadržava neke zaista korisne komande za ispitivanje datoteka i pronalaženje informacija o paketu. Podrazumevano je instaliran na Ubuntu sistemima tako da ga možemo direktno koristiti:

```sh
dpkg --listfiles ssl-cert
```

Ovo nam daje sledeću listu datoteka i njihovu strukturu direktorijuma (kraj liste je skraćen radi kratkoće):

```sh
/.
/etc
/etc/ssl
/etc/ssl/certs
/etc/ssl/private
/lib
diverted by base-files to: /lib.usr-is-merged
/lib/systemd
/lib/systemd/system
/lib/systemd/system/ssl-cert.service
/usr
/usr/sbin
/usr/sbin/make-ssl-cert
/usr/share
/usr/share/doc
/usr/share/doc/ssl-cert
/usr/share/doc/ssl-cert/README
[...]
```

Ako pronađemo datoteku, ali nismo sigurni iz kog paketa dolazi, `dpkg` i to nam može pomoći!

Koristimo primer jedne od datoteka iz prethodnog izlaza: `/usr/share/ssl-cert/ssleay.cnf` i pretražimo je koristeći `dpkg`:

```sh
dpkg --search /usr/share/ssl-cert/ssleay.cnf
```

Ovo će nam dati ime paketa za datu datoteku:

```sh
ssl-cert: /usr/share/ssl-cert/ssleay.cnf
```

Iako nam ovo deluje očigledno, jer već znamo izvor ove datoteke, `dpkg` funkcija pretrage je zaista korisna za praćenje izvora datoteka za koje ne znamo!

### Konfiguracioni fajlovi

Većina konfiguracije paketa se obrađuje putem konfiguracionih datoteka (često poznatih kao `conffiles`). Conf fajlovi često sadrže stvari poput putanja do datoteka, logova i konfiguracije za otklanjanje grešaka, parametara jezgra (koji se mogu promeniti radi optimizacije performansi sistema), kontrole pristupa i drugih podešavanja konfiguracije.

Stvarni dostupni parametri će se razlikovati od jednog paketa do drugog.

Konfiguracione datoteke paketa se razlikuju od svih ostalih datoteka koje se isporučuju u paketu. Paket može imati bilo koji broj konfiguracionih datoteka (uključujući i nijednu!).

Konfiguracione datoteke eksplicitno označava održavalac paketa tokom razvoja kako bi zaštitio lokalnu konfiguraciju od prepisivanja tokom nadogradnji i kako bi se vaše izmene sačuvale. To nije slučaj ni sa jednim drugim tipovima datoteka – izmene koje napravite na redovnim datotekama u tom paketu biće prepisane tokom nadogradnje.

### Kako su nadogradnje upravljane

Pošto mi možemo da menjamo konfiguracionu datoteku, možemo doći do sukoba kada
održavalac paketa promeni te iste datoteke. Stoga je važno razumeti kako se takvi sukobi rešavaju.

Možemo prikazati četiri moguća scenarija nadogradnje koristeći sledeću tabelu. Šta se dešava tokom nadogradnje zavisi od toga da li smo mi promenili konfiguracioni fajl na našem sistemu ("promenio/nije promenio korisnik") i da li je podrazumevani sadržaj verzije promenio održavalac paketa ("promenio/nije promenio održavalac"):

 Conf. fajl            | Nije promenio održavaoc   | Promenio održavaoc
-----------------------|---------------------------|-----------------------------
Promenio korisnik      | Zadrži korisničke promene | Pitajte korisnika
Nije promenio korisnik | Nema promena              | Primeni promene iz nadogradnje

Dakle, možemo videti da ako napravimo izmene u konfiguracionoj datoteci, APT nikada neće prepisati naše izmene bez prethodnog pitanja.

### Identifikacija conf fajlova

Iz liste datoteka u paketu, kako znamo koje su conf datoteke?

Na kraju krajeva, nisu označeni nikakvom posebnom ekstenzijom datoteke, i iako se često nalaze u /etc/ direktorijumu, ne moraju biti tamo. Kao što smo ranije videli, jedina stvar koju conf-fajlovi imaju zajedničko jeste da je održavalac paketa odlučio da ih označi kao takve.

Ali to je naš trag! Dakle, još jednom, `dpkg` može nam priskočiti u pomoć. Sledeća komanda će nam prikazati ( `--show` ) podskup datoteka u paketu koje je održavalac apache2 označio kao " Conffiles " ( `-f='${Conffiles}\n'` ) i prikazuje svaku u novom redu ( `\n` ) na izlazu:

```sh
dpkg-query --show -f='${Conffiles}\n' apache2
```

Ako želite da saznate više o tome šta ova komanda radi, možete pogledati stranicu uputstva tako što ćete otkucati `man dpkg-query --show`, a ona će vas provesti kroz sve opcije.

Za razliku od `dpkg --listfiles`,  nam takođe daje niz slova i brojeva. Ovaj niz je poznat kao ili `MD5 hash`.  

```sh
/etc/apache2/apache2.conf 354c9e6d2b88a0a3e0548f853840674c
/etc/apache2/conf-available/charset.conf e6fbb8adf631932851d6cc522c1e48d7
/etc/apache2/conf-available/security.conf 332668933023a463046fa90d9b057193
/etc/apache2/envvars e4431a53c868ae0dfcde68564f3ce6a7
/etc/apache2/magic a6d370833a02f53db6a0a30800704994
[...]
```

Kontrolnu sumu određene datoteke možemo videti pokretanjem ove komande:

```sh
md5sum /etc/apache2/apache2.conf
```

Što nam vraća kontrolnu sumu praćenu datotekom i njenom lokacijom:

```sh
354c9e6d2b88a0a3e0548f853840674c /etc/apache2/apache2.conf
```

Možda se pitate zašto nas to zanima, pošto se podudaraju (u ovom primeru).

Kontrolna suma je poput otiska prsta - jedinstvena je za svaku verziju datoteke, tako da će svaki put kada se datoteka promeni dobiti novu kontrolnu sumu - što nam omogućava da vidimo da li je fajl promenjen u odnosu na podrazumevanu vrednost.

### Verifikacija checksum-a

Hajde da postavimo situaciju kako bismo mogli malo da isprobamo ovu ideju. Možemo početi tako što ćemo napraviti neke izmene u conf fajlu. U Apache2, glavni conf fajl je `/etc/apache2/apache2.conf`, pa hajde da to iskoristimo. U situaciji kada podešavamo novi veb server, razumno je da želimo da povećamo LogLevel sa "warn" na "debug" da bismo dobili više poruka o debagovanju, pa hajde da pokrenemo ovu komandu i koristimo sed da bismo napravili tu promenu u kondi fajlu:

```sh
sudo sed -e 's/LogLevel warn/LogLevel debug/' -i /etc/apache2/apache2.conf
```

Nećemo biti upitani da potvrdimo da li želimo da napravimo ove izmene – ali nam je potreban root pristup, pa ga koristimo sudo u našoj komandi. Kao što smo nagovestili u odeljku o `sudo`, činjenica da možemo da napravimo ove izmene bez potrebe za potvrdom je razlog zašto može biti tako lako pokvariti vaš sistem kada radite kao root! Pokušajte da pokrenete komandu bez sudo i dobićete grešku "dozvola odbijena".

Zatim ćemo ponovo pokrenuti naš Apache2 server kako bismo mogli da aktiviramo naše izmene konfiguracije:

```sh
sudo systemctl restart apache2
```

Sada, ako ponovo pokrenemo md5sum komandu, možemo videti da se heš promenio:

```sh
ubuntu@tutorial:~$ md5sum /etc/apache2/apache2.conf
1109a77001754a836fb4a1378f740702 /etc/apache2/apache2.conf
```

Ovo odlično funkcioniše ako znamo da postoji datoteka koju smo promenili, ali šta ako je neko drugi menjao datoteku, a mi ne znamo koju? U tom slučaju, možemo koristiti:

```sh
dpkg --verify apache2
```

Ovo će proveriti kontrolne sume datoteka na našem sistemu u odnosu na one koje se nalaze u indeksu paketa za `apache2` i vratiti prilično čudan rezultat ako (ili kada) pronađe neslaganje:

```sh
??5?????? c /etc/apache2/apache2.conf
```

Što je upravo ono što smo očekivali da vidimo, pošto znamo da smo promenili ovu datoteku.

Ali šta ako je nešto drugo pokvareno... nešto što ne bi trebalo, a nešto što mi nismo promenili? Hajde da napravimo "glupu" promenu u drugoj datoteci da bismo ovo testirali – u ovom slučaju, menjamo sve instance reči "upozorenje" u "glupu" u nasumičnoj datoteci paketa:

```sh
sudo sed -e 's/warning/silly/' -i /usr/sbin/a2enmod
```

A zatim ponovo pokrenite verifikaciju sa:

```sh
dpkg --verify apache2
```

Sada vidimo nešto što izgleda ovako:

```sh
??5?????? c /etc/apache2/apache2.conf
??5?????? /usr/sbin/a2enmod
```

> [!Note]
>
> Možda ste primetili da postoji `c` u gornjem redu, ali ne i u donjem – "c" pokazuje da je
> datoteka conffile.

`dpkg` može da kaže da je datoteka promenjena, ali nam neće reći kakva je promena bila.

`dpkg` može da kaže da je datoteka promenjena, ali nam neće reći kakva je promena bila.

Međutim, pošto dotična datoteka nije conf datoteka, znamo da promena neće biti sačuvana ako nadogradimo paket. To znači da možemo prebrisati izmene i vratiti podrazumevani sadržaj paketa "ponovnom instalacijom" Apache2:

```sh
sudo apt install --reinstall apache2
```

Korišćenjem `--reinstall` zastavice, možemo prisiliti `apt` na ponovno raspakivanje celog podrazumevanog sadržaja. Ako zatim ponovo proverimo…

```sh
dpkg --verify apache2
```

Onda ćemo dobiti ovaj izlaz:

```sh
??5?????? c /etc/apache2/apache2.conf
```

...dakle, možemo videti da je naša promena u conf fajlu sačuvana jer su kontrolne sume drugačije, ali `a2enmod` fajl više nije naveden jer je vraćen na podrazumevane vrednosti. Uf!

> [!Note]
>
> Možemo koristiti `sudo apt install <package>` za nadogradnju instaliranog paketa, ali ovo će
> nadograditi samo na najnoviju verziju. U našem slučaju, već smo bili na najnovijoj verziji
> `Apache2`, pa smo morali da nateramo APT da ponovo raspakuje sadržaj kako bi prepisao naše
> "glupe" izmene.

## Uklanjanje paketa

Pošto smo upravo ponovo instalirali `Apache2` paket, znamo da je u dobrom stanju. Ali šta ako odlučimo da smo završili sa njim i samo želimo da ga uklonimo? Onda možemo pokrenuti:

```sh
sudo apt remove apache2
```

Što će nam dati izlaz poput ovog:

```sh
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
The following packages were automatically installed and are no longer required:
  apache2-bin apache2-data apache2-utils libapr1t64 libaprutil1-dbd-sqlite3
  libaprutil1-ldap libaprutil1t64 liblua5.4-0
Use 'sudo apt autoremove' to remove them.
The following packages will be REMOVED:
  apache2
0 upgraded, 0 newly installed, 1 to remove and 44 not upgraded.
After this operation, 465 kB disk space will be freed.
Do you want to continue? [Y/n]
```

Hajde da otkucamo "Y" da bismo nastavili.

Kao i ranije, vidimo da će zavisnosti i dalje biti tu čak i kada apache2 je uklonjeno. Hajde da proverimo sa `dpkg` …

```sh
dpkg --listfiles apache2
```

...i videti šta bi još moglo da ostane iza...

```sh
/etc
/etc/apache2
/etc/apache2/apache2.conf
/etc/apache2/conf-available
/etc/apache2/conf-available/charset.conf
/etc/apache2/conf-available/localized-error-pages.conf
/etc/apache2/conf-available/other-vhosts-access-log.conf
/etc/apache2/conf-available/security.conf
/etc/apache2/conf-available/serve-cgi-bin.conf
/etc/apache2/conf-enabled
/etc/apache2/envvars
/etc/apache2/magic
/etc/apache2/mods-available
/etc/apache2/mods-available/access_compat.load
/etc/apache2/mods-available/actions.conf
[...]
```

Ovo sumnjivo liči na listu konfiguracionih datoteka koju smo ranije videli, zar ne?

### Uklanjanje i konfiguracije

Kako se ispostavilo, uklanjanje paketa ne uklanja automatski i conf datoteke. Ali – ovo je namerno, radi naše udobnosti.

Ostavljanjem konfiguracionih datoteka na mestu, ako odlučimo da ponovo instaliramo `apache2` u budućnosti, nećemo morati da trošimo vreme na ponovno podešavanje cele naše konfiguracije.

Da vidimo razliku u instaliranju `apache2` nakon što je instalirano (i uklonjeno) u poređenju sa prvim putem kada smo ga instalirali:

```sh
sudo apt install apache2
```

Primetite da nas ovaj put nije pitalo da potvrdimo da li želimo da nastavimo. Zašto ne? Kao što smo ranije videli, potvrda "Y/n" se prikazuje kada postoje zavisnosti, a mi znamo da `Apache2` ima zavisnosti.

...Ah! Ali ovog puta, nismo pokrenuli autoremove kada smo deinstalirali `Apache2`, tako da su zavisnosti i dalje instalirane na našem sistemu. To znači da kada zatražimo apt instalaciju `apache2` sada, ništa ne nedostaje i dobijamo tačno ono što tražimo.

Pošto su zavisnosti i konfiguracione datoteke još uvek tu, možemo odmah koristiti našu prethodnu konfiguraciju. Ona čak zadržava i izmene koje smo ranije napravili, što možemo proveriti ponovnim pogledom na kontrolnu sumu:

```sh
md5sum /etc/apache2/apache2.conf
```

### Uklanjanje i čišćenje

Šta ako odlučimo da ne želimo izmenjene konfiguracione datoteke? Možda želimo da se vratimo na podrazumevanu instalaciju ili znamo da više nikada nećemo želeti da koristimo paket – kako možemo osigurati da se sve konfiguracione datoteke uklone istovremeno kada uklonimo paket?

U tom slučaju, možemo koristiti `--purge` opciju komande `remove`:

```sh
sudo apt remove --purge apache2
```

Što će nam dati ovaj izlaz:

```sh
[...]
The following packages were automatically installed and are no longer required:

apache2-bin apache2-data apache2-utils libapr1t64 libaprutil1-dbd-sqlite3
libaprutil1-ldap libaprutil1t64 liblua5.4-0

Use 'sudo apt autoremove' to remove them.
The following packages will be REMOVED:

apache2*
0 upgraded, 0 newly installed, 1 to remove and 9 not upgraded.
After this operation, 465 kB disk space will be freed.
Do you want to continue? [Y/n]
```

Ako pažljivo pogledamo, vidimo malu zvezdicu (*) u izlazu.

```sh
The following packages will be REMOVED:
apache2*
```

Ovaj mali indikator nam govori da će paket biti uklonjen I očišćen. Međutim, on i dalje ne uklanja zavisnosti (ili konfiguracione datoteke tih zavisnosti).

Hajde da ponovo otkucamo "Y" da bismo potvrdili da želimo da nastavimo. Zatim, kada se uklanjanje završi, možemo još jednom proveriti listu:

```sh
dpkg --listfiles apache2
```

I ovog puta, rezultat je veoma drugačiji!

```sh
dpkg-query: package 'apache2' is not installed
Use dpkg --contents (= dpkg-deb --contents) to list archive files contents.
```

> [!Note]
>
> Takođe bismo mogli da koristimo komandu od ranije `dpkg-query --show -f='${Conffiles}\n'
> apache2` i `dpkg-query` neće pronaći pakete koji odgovaraju.

Postoje i drugi načini za promenu datoteka paketa. Ako želite da pročitate više, pogledajte naš vodič za promenu datoteka paketa.

## Šta je još na sistemu?

Kao što smo ranije videli, možemo pretraživati bazu podataka APT paketa po ključnim rečima koristeći da bismo pronašli softver koji bismo možda želeli da instaliramo. Takođe možemo videti sve pakete koje već imamo koristeći `apt search <keyword>`, mada može biti lakše za navigaciju i informativnije ako umesto toga koristimo `apt list dpkg -l Q` – tada možemo koristiti tastere sa strelicama gore i dole na tastaturi za skrolovanje (ili pritisnuti da bismo se vratili na terminalnu liniju).  

Za svaki paket možemo videti koje verzije postoje u bazi podataka:

```sh
apt policy apache2
```

Ovo će vratiti rezime svih verzija koje postoje na našem konkretnom izdanju Ubuntua, poređanih po redu "najnovije":

```sh
apache2:
  Installed: (none)
  Candidate: 2.4.58-1ubuntu8.4
  Version table:
     2.4.58-1ubuntu8.4 500
        500 http://archive.ubuntu.com/ubuntu noble-updates/main amd64 Packages
        500 http://security.ubuntu.com/ubuntu noble-security/main amd64 Packages
        100 /var/lib/dpkg/status
     2.4.58-1ubuntu8 500
        500 http://archive.ubuntu.com/ubuntu noble/main amd64 Packages
```

Znamo da Apache2 trenutno nije instaliran, jer smo ga uklonili i obrisali, zbog čega se instalirana verzija prikazuje kao "None":

```sh
Installed: (none)
```

Ako bismo instalirali podrazumevani paket, dobili bismo ovaj:

```sh
Candidate: 2.4.58-1ubuntu8.4
```

Ispod svake verzije prikazuje se i "source". Najnovija verzija ( 2.4.58-1ubuntu8.4 ) dolazi sa noble-updates (main) i noble-security (main). Originalna verzija ( 2.4.58-1ubuntu8 ) dolazi sa noble (main). Ovo nam govori da je ovo bila verzija objavljena sa 24.04 LTS (Noble Numbat).

Možemo instalirati određene starije verzije ako želimo, na primer, da zadovoljimo zahteve zavisnosti drugog paketa. To možemo učiniti tako što ćemo navesti ime i verziju paketa:

```sh
sudo apt install <package=version>
```

### Instaliranje starijh verzija

Međutim, ovo može biti komplikovano i često dovodi do sukoba u verzijama zavisnosti jer APT uvek želi da instalira najnoviju verziju. Primer ovoga možemo videti ako pokrenemo sledeću komandu:

```sh
sudo apt install apache2=2.4.58-1ubuntu8
```

APT nas upozorava da verzija apache2 koju želimo da instaliramo zavisi od ranijih verzija zavisnosti, ali nam korisno govori koje verzije zavisnosti su nam potrebne da bismo uspešno instalirali paket koji želimo.

```sh
[...]
Some packages could not be installed. This may mean that you have
requested an impossible situation or if you are using the unstable
distribution that some required packages have not yet been created
or been moved out of Incoming.
The following information may help to resolve the situation:

The following packages have unmet dependencies:
 apsache2 : Depends: apache2-bin (= 2.4.58-1ubuntu8) but 2.4.58-1ubuntu8.4 is to be installe
           Depends: apache2-data (= 2.4.58-1ubuntu8) but 2.4.58-1ubuntu8.4 is to be install
           Depends: apache2-utils (= 2.4.58-1ubuntu8) but 2.4.58-1ubuntu8.4 is to be instal
E: Unable to correct problems, you have held broken packages.
```

Dakle, sve što treba da uradimo je da prvo instaliramo zavisnosti, a zatim ponovo pokrenemo komandu install. Zapamtite da možemo instalirati više paketa odjednom tako što ćemo ih odvojiti razmacima:

```sh
sudo apt install apache2-bin=2.4.58-1ubuntu8 \
apache2-data=2.4.58-1ubuntu8 \
apache2-utils=2.4.58-1ubuntu8 \
apache2=2.4.58-1ubuntu8
```

U ovom slučaju takođe razbijamo komandu na više redova koristeći obrnute kose crte ( \ ) kako bismo je olakšali za čitanje, ali će i dalje biti izvršena kao jedna komanda.

APT će nas upozoriti da vraćamo paket na stariju verziju, ali pritisnimo dugme Y za potvrdu (kada se to od nas zatraži), i on će ipak nastaviti sa vraćanjem na stariju verziju. Ponovo pokrenimo sledeću komandu:

apt policy apache2

I dobićemo potvrdu da koristimo stariju verziju:

```sh
apache2:
  Installed: 2.4.58-1ubuntu8
  Candidate: 2.4.58-1ubuntu8.4
  Version table:
     2.4.58-1ubuntu8.4 500
        500 http://archive.ubuntu.com/ubuntu noble-updates/main amd64 Packages
        500 http://security.ubuntu.com/ubuntu noble-security/main amd64 Packages
 *** 2.4.58-1ubuntu8 500
        500 http://archive.ubuntu.com/ubuntu noble/main amd64 Packages
        100 /var/lib/dpkg/status
```

### Odakle dolaze svi ovi paketi?

Možda se sada pitate "odakle tačno dolaze svi ovi paketi"? U ovom tutorijalu smo ukratko pomenuli nekoliko izvora, ali im još nismo posvetili direktnu pažnju. Hajde da sada odvojimo malo vremena da definišemo šta podrazumevamo pod svim ovim različitim izvorima iz kojih APT može da preuzima pakete.

Izvor iza APT-a je "Ubuntu Package Archive". Ova arhiva je podeljena na mnogo slojeva, svaki sa
svojom terminologijom. Različita terminologija je na prvi pogled prilično zbunjujuća, ali smo već videli neke od termina. Dakle, ako pogledamo, sloj po sloj, videćemo ne samo šta svi termini znače, već i kako se svi oni uklapaju zajedno.

Hajde da ukratko pogledamo ovaj dijagram. Opšti tok je da se `Arhiva` deli na Ubuntu `serije`. Svaka `serija` je podeljena na `pockete`, a zatim svaki džep sadrži četiri `komponente`. Ako bismo pokušali da sve ovo prikažemo na jednom dijagramu, bilo bi prilično opširno, pa hajde da pogledamo jednu putanju.

```sh
                                    Ubuntu Package Archive
                                    ======================
            
                                  Splits into Ubuntu series
                                  -------------------------
                        e.g., mantic      noble       oracular, etc
            
                                  Series split into pockets
                                  -------------------------
                -release    -proposed   -updates    -security   -backports
            
                                  Splits into components
                                  -----------------------
                    main    universe    restricted    multiverse
```

#### Serije

Serija je skup paketa koji se objavljuju sa određenom verzijom Ubuntua – obično se nazivaju po svom kodnom imenu (npr. mantic, noble i oracular na našem dijagramu). Svaka verzija Ubuntua može imati više izdanja (na primer, LTS će imati početno izdanje kada se pokrene (npr. 24.04 LTS), a zatim "naknadna izdanja" (npr. 24.04.1 LTS) – sva su to dela iste serije (noble).

U praksi, ljudi često koriste termine „Ubuntu izdanje“ i „Ubuntu serija“ naizmenično.

#### Pakete

Svaka Ubuntu serija ( noble , jammy , itd.) je podeljena na pokete, koji su povezani sa životnim ciklusom razvoja/objavljivanja softvera:

- `release` - sadrži pakete kakvi su u vreme objavljivanja.

- `proposed` - sadrži ažuriranja paketa dok se testiraju.

- `resrtricted` - kada se ažuriranje objavi, ono dolazi ili iz `-security` ili `-updates`, u
  zavisnosti od toga da li je u pitanju ažuriranje vezano za bezbednost ili ne.

- `-backports` - koji sadrži pakete koji nisu bili dostupni u vreme objavljivanja.

Zato smo ranije videli da neka ažuriranja dolaze iz noble-updates ili noble-security. Ova ažuriranja se odnose na ažuriranja i bezbednosna ažuriranja iz serije Noble (respektivno).
Pocketi se obično dodaju na kraj serije, i prilično je uobičajeno videti crticu ( - ) kada se govori o džepovima.

Zapamtite – originalna verzija paketa apache2 koju smo videli potiče iz "noble". -release džep sadrži samo softver koji je bio deo originalnog LTS izdanja, pa zato podrazumevano preuzima ime Ubuntu serije (tj. džep -release se podrazumeva).

#### Komponente

Svaki džep je podeljen na četiri komponente, u zavisnosti od toga da li su paketi koje sadrže otvorenog ili zatvorenog koda, i da li ih zvanično podržava Canonical ili ih održava Ubuntu zajednica:

 ...                 | Open source | Closed source
---------------------|-------------|---------------
Officially supported | main        | restricted
Community supported  | universe    | multiverse

- `main` - sadrži pakete otvorenog koda koje zvanično podržava Canonical. Ovi paketi su ili instalirani na svakoj Ubuntu mašini ili se veoma široko koriste za različite tipove sistema i slučajeva upotrebe.

- `universe` - sadrži sve ostale pakete otvorenog koda u Ubuntuu, koje obično održavaju Debian i Ubuntu zajednice, ali može da uključuje i dodatnu bezbednosnu pokrivenost od strane Kanonikala u okviru Ubuntu Pro , koji je dostupan besplatno za ličnu upotrebu na do pet mašina.

- `restricted` - sadrži pakete koje zvanično podržava Canonical, ali nisu dostupni pod potpuno besplatnom licencom.

- `multiverse` - sadrži vlasnički softver koji održava zajednica – ove pakete kompanija Canonical uopšte ne podržava.

Ako želite više informacija o procesu objavljivanja Ubuntua, kako se paketi proizvode ili da saznate više o vrsti terminologije na koju možete naići, možda će vas zanimati Vodič za pakovanje Ubuntua, koji je odličan resurs koji sadrži sve ove informacije (i još mnogo toga!).

## Instaliranje .deb fajla

Iako je APT preferirani način za instaliranje paketa na vaš sistem, zbog svoje sposobnosti da obrađuje zavisnosti i održava softver ažuriranim, nisu svi paketi dostupni u APT repozitorijumu – posebno ako su toliko stari da se više ne održavaju ili, obrnuto, ako je najnovija verzija još uvek u razvoju!

Možemo instalirati `.deb` datoteke koje nisu u APT repozitorijumu koristeći `dpkg` – sve što nam je potrebno je da preuzmemo .deb datoteku i možemo pokrenuti komandu poput ove da bismo je instalirali:

```sh
sudo dpkg -i <file-name.deb>
```

Ali – APT je i ovde koristan. Čak i ako dobijemo `.deb` datoteku koja nije iz Ubuntu arhive, i dalje je možemo instalirati pomoću APT-a tako da ako postoje zavisnosti koje se mogu automatski rešiti iz arhive – one će biti!

```sh
sudo apt install ./file-name.deb
```

Ako ikada budemo želeli da instaliramo `.deb` datoteku, APT je dedinitivno najpogodniji način da to uradimo. Možda ćemo i dalje morati ručno da obrađujemo neke zavisnosti, ali sada imamo znanje da to možemo da uradimo.

Srećom, većinu paketa koji će vam ikada zatrebati verovatno ćete pronaći preko APT-a. Ako nije, vredi proveriti da li je softver dostupan kao `snap`.

## Snaps

Snepovi su noviji, samostalni softverski format koji je razvijen kao prenosivija i jednostavnija alternativa debovima. Dolaze sa svim unapred instaliranim zavisnostima tako da nema potrebe za alatom za upravljanje paketima za praćenje zavisnosti, i rade unutar izolovanih okruženja koja ograničavaju njihovu interakciju sa ostatkom sistema.

Umesto  kao što ih imamo u debovima, snapovi koriste koncept kanala da bi definisali koja verzija snapa je instalirana.

Podrazumevano, snimci se automatski ažuriraju, tako da ne moramo da pamtimo da ih ažuriramo i nadograđujemo. Postoje trenuci na živom sistemu, kao što je server u proizvodnom okruženju, kada možda ne želimo da se ažuriranja automatski primenjuju. U tim slučajevima, možemo isključiti automatska ažuriranja i osvežiti sistemske snimke kada nam je to zgodno (na primer, tokom perioda održavanja).

Ako želite da isprobate snap funkcije, preporučujemo odličan vodič za brzi početak u dokumentaciji za snap funkcije. Slobodno nastavite da koristite virtuelnu mašinu koju smo koristili u ovom vodiču dok istražujete!

## Kompletiranje

Kada završite i želite da napustite tutorijal, možete pokrenuti:

```sh
exit
```

Ovo će vas vratiti iz virtuelne mašine na vašu aktivnu mašinu. Zatim možete pokrenuti sledeće komande da biste obrisali virtuelnu mašinu i potpuno je uklonili sa vaše mašine:

```sh
multipass delete tutorial
multipass purge
```

## Zaključak

Čestitamo, stigli smo do kraja! Obradili smo dosta materijala u ovom tutorijalu, pa hajde da ukratko rezimiramo šta smo naučili:

- Kako ažurirati i nadograditi sav softver našeg sistema pomoću APT-a:
  
  ```sh
  sudo apt update && sudo apt upgrade
  ```

- Kako pretraživati softver pomoću ključnih reči ili nizova znakova:
  
  ```sh
  apt search <keyword> ili apt search "some content string"
  ```

- Kako videti opis paketa, uključujući i zavisnosti koje on ima:

  ```sh
  apt show <package name>
  ```

- Kako da proverite koje su verzije paketa dostupne:

  ```sh
  apt policy <package>
  ```

- Kako instalirati pakete…

  ```sh
  sudo apt install <package1> <package2>
  ```

- Kako videti sve datoteke koje paket sadrži

  ```sh
  dpkg --listfiles <package>
  ```

- Kako saznati kom paketu pripada datoteka:

  ```sh
  dpkg --search <path/to/file>
  ```

- Kako ponovo ukloniti pakete! Kao i razlika između uklanjanja i čišćenja.
  
  ```sh
  sudo apt remove <package>
  ```

- Čak smo naučili kako da vratimo starije verzije APT paketa i sve o APT izvorima.

- Kako pronaći config datoteke u paketu:

  ```sh
  dpkg-query --show -f='${Conffiles}\n' <package>
  ```

- Kako videti da li su datoteke paketa promenjene:

  ```sh
  dpkg --verify <package>
  ```

- ...A ako je ne-konfiig datoteka slučajno promenjena, možemo je popraviti sa:

  ```sh
  sudo apt install --reinstall <package>
  ```

- Znamo da su naše izmene u conf datotekama uvek bezbedno sačuvane, dok se izmene u datotekama
  koje nisu conf datoteke vraćaju na prethodno stanje pri sledećoj nadogradnji ili bezbednosnoj ispravci.

- Važno je napomenuti da znamo kako da proverimo kontrolne sume pomoću md5sum ili sličnih alata,
  što nam pomaže da bezbednije gradimo pakete iz izvornog koda.

- I konačno, naučili smo o snimcima!
