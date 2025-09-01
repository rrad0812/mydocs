[[Sadržaj]](toc.md) [[Promenljive i konstante]](02_Promenljive.md)

# Uvod u Go!

Zdravo, dobrodošli na kurs i hvala što želite da učite Go. Nadam se da će vam ovaj kurs pružiti sjajno iskustvo učenja.

### Šta je Go?
Go (takođe poznat kao Golang) je programski jezik razvijen u kompaniji Gugl 2007. godine, a otvorenog koda objavljen 2009. godine.

Go se fokusira se na jednostavnost, pouzdanost i efikasnost. Dizajniran je da kombinuje efikasnost, brzinu i bezbednost statički tipiziranog i kompajliranog jezika sa lakoćom programiranja dinamičkog jezika, kako bi programiranje ponovo bilo zabavno.

Na neki način, Go želi da kombinuje najbolje delove Pajtona i C++-a kako bi mogao da izgradi pouzdane sisteme koji mogu da iskoriste prednosti višejezgarnih procesora.

### Zašto učiti Go?

Pre nego što počnemo sa ovim kursom, hajde da razgovaramo o tome zašto bi trebalo da naučimo Go.

- **Lako se uči**
	
	Go je prilično lak za učenje i ima podržavajuću i aktivnu zajednicu. A pošto je višenamenski jezik, možete ga koristiti za stvari poput:
	- 	razvoja bekenda, 
	- 	računarstva u oblaku i u skorije vreme
	- 	nauke o podacima.

- **Brz i pouzdan**

	što ga čini veoma pogodnim za distribuirane sisteme. Projekti kao što su **Kubernetes** i **Docker** su napisani u Go-u.

- **Jednostavan, ali moćan**

	Sam jezik je koncizan. Go ima samo **25** ključnih reči što ga čini lakim za čitanje, pisanje i održavanje.  Ali nemojte da vas jednostavnost zavara, Go ima nekoliko moćnih funkcija koje ćemo kasnije naučiti na kursu.

- **Mogućnosti za karijeru**

	Go brzo raste i usvajaju ga kompanije svih veličina. A sa tim dolaze i nove dobro plaćene mogućnosti za posao.

Nadam se da Vas je ovo zainteresovalo za Go. Hajde da počnemo sa ovim kursom.

### Instalacija i podešavanje

##### Preuzimanje

Možemo instalirati Go sa [mesta za preuzimanja](https://go.dev/dl/).

##### Instalacija

Ova uputstva su sa zvanične web stranice.

1. **MacOS**
	
	Otvorite datoteku paketa koju ste preuzeli i pratite uputstva da biste instalirali Go. Paket instalira Go distribuciju na `/usr/local/go`. Paket bi trebalo da smesti `/usr/local/go/bin` direktorijum u vašu PATH promenljivu okruženja. Možda ćete morati da ponovo pokrenete sve otvorene terminalne sesije da bi promena stupila na snagu.

2. **Linux** 

	Uklonite sve prethodne instalacije Goa brisanjem `/usr/local/go` direktorijuma (ako postoji), a zatim raspakujte arhivu koju ste upravo preuzeli u `/usr/local`, kreirajući novo Go stablo `/usr/local/go`:

        $ rm -rf /usr/local/go && tar -C /usr/local -xzf go1.18.1.linux-amd64.tar.gz

    **Napomena**: Možda ćete morati da pokrenete komandu kao *root* ili preko *sudo*-a.

    Ne raspakujte arhivu u postojeće `/usr/local/go` stablo. Poznato je da ovo dovodi do neispravnih Go instalacija.

    Dodajte `/usr/local/go/bin` u promenljivu okruženja PATH. To možete učiniti dodavanjem sledeće linije u vašu `$HOME/.profile` or `/etc/profile` (za
    instalaciju na nivou celog sistema):

        export PATH=$PATH:/usr/local/go/bin

    **Napomena**: Promene napravljene u datoteci profila možda neće biti primenjene do sledećeg puta kada se prijavite na računar. Da biste odmah primenili promene, jednostavno pokrenite komande školjke direktno ili ih izvršite iz profila pomoću komande kao što je

        $ source $HOME/.profile.

3. **Windows**

	Otvorite MSI datoteku koju ste preuzeli i pratite uputstva za instaliranje Go-a.

    Podrazumevano, instaler će instalirati program Go u "Program Files" ili "Program Files (x86)". Lokaciju možete promeniti po potrebi. <br><br>
	
	Nakon instalacije, moraćete da zatvorite i ponovo otvorite sve otvorene komandne linije kako bi se promene u okruženju koje je napravio instalater odrazile u komandnoj liniji.

Proverite da li ste instalirali Go tako što ćete otvoriti komandnu liniju i otkucati sledeću komandu:

    $ go version

Potvrdite da komanda ispisuje instaliranu verziju programa Go.

##### Uređivač koda

U ovom kursu ću koristiti **VS Code**, a možete ga preuzeti [odavde](https://code.visualstudio.com/download).

Slobodno koristite bilo koji drugi uređivač koda koji vam odgovara.

##### Go VS Code extension

Obavezno instalirajte i Go ekstenziju koja olakšava rad sa Go-om u VS Code-u.

To je to što se tiče instalacije i podešavanja Go-a, hajde da započnemo kurs i napišemo naš prvi "Hello, World!" program.

### Hello world

Počnimo inicijalizacijom modula. Za to možemo koristiti `go mod` komandu.

    $ go mod init example

Za sada, pretpostavimo da je modul u osnovi kolekcija Go paketa.

Hajde sada da kreiramo `main.go` datoteku i napišemo program koji jednostavno ispisuje "Hello world".

```
package main
import "fmt"

func main() {
    fmt.Println("Hello World!")
}
```

Ako se pitate, paket `fmt` je deo je standardne Go biblioteke, koja je skup osnovnih paketa koje pruža jezik.

### Struktura Go programa

Prvo, definisali smo paket `main`.
```
package main
```
Zatim, imamo uvoz.
```
import "fmt"
```
Na kraju, `main` funkcija deluje kao ulazna tačka za našu aplikaciju, baš kao i u drugim jezicima poput C, Java ili C#.
```
func main() {
	...
}
```
Konačno, da bismo pokrenuli naš kod, možemo jednostavno koristiti `go run` komandu.

	$ go run main.go
	Hello World!

[[Sadržaj]](toc.md) [[Promenljive i konstante]](02_Promenljive.md)