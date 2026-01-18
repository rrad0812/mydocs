[[Funkcije]](05_Funkcije.md) [[Sadržaj]](toc.md) [[Paketi]](07_Paketi.md)

# Moduli

`modul` je kolekcija Go paketa smeštenih u stablu datoteka sa `go.mod` datotekom u korenu, pod uslovom da je direktorijum unutar `$GOPATH/src`.

Go moduli su predstavljeni u Go 1.11, donose nativnu podršku za verzije i module. Ranije nam je bila potrebna GO111MODULE=on zastavica da bismo uključili funkcionalnost modula kada je bila eksperimentalna. Ali sada, nakon Go 1.13, režim modula je podrazumevani za sav razvoj.

`GOPATH` je promenljiva koja definiše koren našeg radnog prostora i sadrži sledeće direktorijume:

- `src` : sadrži izvorni kod Go-a organizovan u hijerarhiju.
- `pkg` : sadrži kompajlirani kod paketa.
- `bin` : sadrži kompajlirane binarne datoteke i izvršne datoteke.

Kao i ranije, kreirajmo novi modul koristeći `go mod init` komandu koja kreira
novi modul i inicijalizuje `go.mod` datoteku koja ga opisuje.

    $ go mod init example

Važno je napomenuti da Go modul može odgovarati i Github repozitorijumu ako planirate da objavite modul.

Sada, hajde da istražimo `go.mod` koja je datoteka koja definiše putanju modula, a takođe i putanju uvoza koja se koristi za korenski direktorijum i njegove zahteve zavisnosti.
```
module <name>
go <version>
require (
	...
)
```
Ako želimo da dodamo novu zavisnost, koristićemo `go install` komandu:

    $ go install github.com/rs/zerolog

Kao što vidimo, kreirana je i `go.sum` datoteka. Ova datoteka sadrži očekivane heševe sadržaja novih modula.

Možemo navesti sve zavisnosti koristeći `go list` komandu:

    $ go list -m all

Ako se zavisnost ne koristi, možemo je jednostavno ukloniti pomoću `go mod tidy` komande:

    $ go mod tidy

Završavajući našu diskusiju o modulima, hajde da razgovaramo i o prodaji.

`Vendoring` je čin pravljenja sopstvene kopije paketa trećih strana koje vaš projekat koristi. Te kopije se tradicionalno smeštaju unutar svakog projekta, a zatim čuvaju u repozitorijumu projekata.

To se može uraditi putem `go mod vendor` komande.

Dakle, hajde da ponovo instaliramo uklonjeni modul koristeći `go mod tidy`.
```
package main
import "github.com/rs/zerolog/log"

func main() {
	log.Info().Msg("Hello")
}
```
    $ go mod tidy
    go: finding module for package github.com/rs/zerolog/log
    go: found github.com/rs/zerolog/log in github.com/rs/zerolog v1.26.1

    $ go mod vendor

Nakon što se `go mod vendor` komanda izvrši, biće kreiran `vendor` direktorijum.

├─ go.mod <br>
├─ go.sum <br>
├─ go.work <br>
├─ main.go <br>
└─ vendor <br>
...├─ github.com <br>
....|....└─ rs <br>
....|........└─ zerolog <br>
....|........└─ .. <br>
....└─ modules.txt <br>

[[Funkcije]](05_Funkcije.md) [[Sadržaj]](toc.md) [[Paketi]](07_Paketi.md)
