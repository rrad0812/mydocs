[[Paketi]](07_Paketi.md) [[Sadržaj]](toc.md) [[Korisne komande]](09_Korisne_komande.md)

# Radni prostori

Višemodulni radni prostoria su predstavljeni u Go 1.18.

Radni prostori nam omogućavaju da radimo sa više modula istovremeno bez potrebe za uređivanjem `go.mod` datoteka za svaki modul. Svaki modul unutar radnog prostora se tretira kao korenski modul prilikom rešavanja zavisnosti.

Da bismo ovo bolje razumeli, počnimo sa kreiranjem `hello` modula.

    $ mkdir workspaces && cd workspaces
    $ mkdir hello && cd hello
    $ go mod init hello

U svrhu demonstracije, dodaću jednostavan `main.go` i instalirati primer paketa.
```
package main
import (
	"fmt"
	"golang.org/x/example/stringutil"
)

func main() {
	result := stringutil.Reverse("Hello Workspace")
	fmt.Println(result)
}
```
    $ go get golang.org/x/example
    go: downloading golang.org/x/example v0.0.0-20220412213650-2e68773dfca0
    go: added golang.org/x/example v0.0.0-20220412213650-2e68773dfca0

I ako ovo pokrenemo, trebalo bi da vidimo naš izlaz u obrnutom redosledu.

    $ go run main.go
    ecapskroW olleH

Šta ako želimo da izmenimo `stringutil` modul od kog zavisi naš kod?

Do sada smo to morali da radimo koristeći `replace` direktivu u `go.mod` datoteci, ali sada hajde da vidimo kako možemo da koristimo radne prostore ovde.

Dakle, hajde da kreiramo naš radni prostor u workspaces direktorijumu.

    $ go work init

Ovo će kreirati `go.work` datoteku.

    $ cat go.work
    go 1.18

Takođe ćemo dodati naš `hello` modul u radni prostor.

    $ go work use ./hello

Ovo bi trebalo da ažurira `go.work` datoteku sa referencom na naš `hello` modul.
```
go 1.18
use ./hello
```
Sada, hajde da preuzmemo i izmenimo `stringutil` paket i ažuriramo implementaciju `Reverse` funkcije.

    $ git clone https://go.googlesource.com/example
    Cloning into 'example'...
    remote: Total 204 (delta 39), reused 204 (delta 39)
    Receiving objects: 100% (204/204), 467.53 KiB | 363.00 KiB/s, done.
    Resolving deltas: 100% (39/39), done.

*example/stringutil/reverse.go*
```
func Reverse(s string) string {
	return fmt.Sprintf("I can do whatever!! %s", s)
}
```
Dodajmo example paket našem radnom prostoru.
```
$ go work use ./example
$ cat go.work
go 1.18
use (
	./example
	./hello
)
```
Sada ako pokrenemo naš hello modul primetićemo da je `Reverse` funkcija izmenjena.

    $ go run hello
    I can do whatever!! Hello Workspace

Ovo je veoma korisna funkcija iz Go 1.18.

[[Paketi]](07_Paketi.md) [[Sadržaj]](toc.md) [[Korisne komande]](09_Korisne_komande.md)