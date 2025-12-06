[[Moduli]](06_Moduli.md) [[Sadržaj]](toc.md) [[Radni prostori]](08_Radni_prostori.md)

# Paketi

Paket je direktorijum koji sadrži jednu ili više Go izvornih datoteka ili drugih Go paketa.

To znači da svaka Go datoteka sa izvornim kodom mora pripadati paketu, a deklaracija paketa se vrši na vrhu svake datoteke sa izvornim kodom na sledeći način:
```
package <package_name>
```
Do sada smo sve uradili unutar package `main`. Po konvenciji, izvršni programi 
(pod tim mislim na one sa `main` paketom) se nazivaju *komande*, drugi se jednostavno nazivaju *paketi*.

Paket `main` bi takođe trebalo da sadrži `main()` funkciju koja je posebna funkcija koja deluje kao ulazna tačka izvršnog programa.

Pogledajmo primer kreiranjem sopstvenog paketa `custom` i dodavanjem nekih izvornih datoteka u njega kao što je `code.go`.
```
package custom
```
Pre nego što nastavimo dalje, trebalo bi da razgovaramo o uvozu i izvozu. Baš kao i drugi jezici, i go ima koncept uvoza i izvoza, ali je veoma elegantan.

U osnovi, bilo koja vrednost (kao što je promenljiva ili funkcija) može se izvesti i biti vidljiva iz drugih paketa ako je definisana velikim slovima.

Hajde da pokušamo sa primerom u našem `custom` paketu.
```
package custom

var value int = 10 // Will not be exported
var Value int = 20 // Will be exported
```
Kao što vidimo, mali identifikatori neće biti izvezeni i biće privatni za paket
u kojem su definisani. U našem slučaju, `custom` paket.

To je odlično, ali kako da ga uvezemo ili mu pristupimo? Pa, isto kao što smo
do sada radili nesvesno. Hajde da odemo do naše main.go datoteke i uvezemo naš
`custom` paket.

Ovde se na to možemo pozivati koristeći module smo ranije inicijalizovali u
našoj `go.mod` datoteci.
```
---go.mod---
module example
go 1.18

---main.go--
package main
import "example/custom"

func main() {
	custom.Value
}
```
Obratite pažnju kako je ime paketa kombinovano sa putanjom uvoza.

Možemo uvesti više paketa i na ovaj način.
```
package main
import (
	"fmt"
	"example/custom"
)

func main() {
	fmt.Println(custom.Value)
}
```
Takođe možemo da koristimo aliase za naše uvoze kako bismo izbegli kolizije u nazivu paketa.
```
package main

import (
	"fmt"
	abcd "example/custom"
)

func main() {
	fmt.Println(abcd.Value)
}
```
### Spoljne zavisnosti

U Gou nismo ograničeni samo na rad sa lokalnim paketima, već možemo instalirati i eksterne pakete koristeći go install komandu kao što smo ranije videli.

Dakle, hajde da preuzmemo jednostavan paket za evidentiranje github.com/rs/zerolog/log.

    $ go install github.com/rs/zerolog
```
package main
import (
	"github.com/rs/zerolog/log"
	abcd "example/custom"
)

func main() {
	log.Print(abcd.Value)
}
```
Takođe, obavezno proverite `go doc` paketa koje instalirate, koji se obično nalazi u `readme` datoteci projekta. `go doc` analizira izvorni kod i generiše dokumentaciju u HTML formatu. Referenca na njega se obično nalazi u `readme` datotekama.

Na kraju, dodaću da Go nema posebnu konvenciju "strukture foldera", uvek pokušajte da organizujete svoje pakete na jednostavan i intuitivan način.

[[Moduli]](06_Moduli.md) [[Sadržaj]](toc.md) [[Radni prostori]](08_Radni_prostori.md)
