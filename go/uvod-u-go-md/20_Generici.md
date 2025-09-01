[[Testiranje]](19_Testiranje.md) [[Sadržaj]](toc.md) [[Konkurentnost]](21_Konkurentnost.md)

# Generici

Generici su parametrizovani tipovi. Generici omogućavaju programerima da pišu kod gde se tip može navesti kasnije, jer nije odmah relevantan.

Za naš primer, imamo jednostavne funkcije sumiranja za različite tipove kao što su `int`, `float64` i `string`. Pošto `overloading` metoda nije dozvoljen u Go-u, obično moramo da kreiramo nove funkcije.
```
package main
import "fmt"

func sumInt(a, b int) int {
	return a + b
}
func sumFloat(a, b float64) float64 {
	return a + b
}
func sumString(a, b string) string {
	return a + b
}

func main() {
	fmt.Println(sumInt(1, 2))
	fmt.Println(sumFloat(4.0, 2.0))
	fmt.Println(sumString("a", "b"))
}
```
Osim tipova, ove funkcije su prilično slične.

Da vidimo kako možemo definisati generičku funkciju.
```
func fnName[T constraint]() {
	...
}
```
Ovde je `T` naš parametar tipa i `constraint` biće interfejs koji dozvoljava nekom tipu da implementira taj interfejs. Ovo je zbunjujuće. Dakle, hajde da počnemo da gradimo našu generičku `sum` funkciju.

Ovde ćemo koristiti `T` kao parametar tipa sa praznim interfejsom `interface{}` kao ograničenjem.
```
func sum[T interface{}](a, b T) T {
	fmt.Println(a, b)
}
```
Počevši od Go 1.18 možemo koristiti `any`, što je manje-više ekvivalentno praznom interfejsu - `interface{}`.
```
func sum[T any](a, b T) T {
	fmt.Println(a, b)
}
```
Sa parametrima tipa dolazi do potrebe za prosleđivanjem `argumenata tipa`, što može učiniti naš kod opširnim.
```
sum[int](1, 2) 		// explicit type argument
sum[float64](4.0, 2.0)	// explicit type argument
sum[string]("a", "b")	// explicit type argument
```
Srećom, Go 1.18 dolazi sa zaključivanjem tipa što nam pomaže da pišemo kod koji poziva generičke funkcije bez eksplicitnih tipova.
```
sum(1, 2)
sum(4.0, 2.0)
sum("a", "b")
```
Hajde da ovo pokrenemo i vidimo da li radi.

	$ go run main.go
	1 2
	4 2
	a b

Sada da ažuriramo *sum* funkciju da bismo sabrali naše promenljive.
```
func sum[T any](a, b T) T {
	return a + b
}

fmt.Println(sum(1, 2))
fmt.Println(sum(4.0, 2.0))
fmt.Println(sum("a", "b"))
```
Ali sada ako ovo pokrenemo, dobićemo grešku da operator `+` nije definisan u ograničenju.

	$ go run main.go
	./main.go:6:9: invalid operation: operator + not defined on a (variable of type T constrained by any)

Iako ograničenje tipa `any` generalno funkcioniše, ono ne podržava operatore.

Dakle, hajde da definišemo sopstveno prilagođeno ograničenje koristeći interfejs. Naš interfejs treba da definiše skup tipova koji sadrži `int`, `float` i `string`.

Evo kako izgleda naš `SumConstraint` interfejs.
```
type SumConstraint interface {
	int | float64 | string
}
func sum[T SumConstraint](a, b T) T {
	return a + b
}
func main() {
	fmt.Println(sum(1, 2))
	fmt.Println(sum(4.0, 2.0))
	fmt.Println(sum("a", "b"))
}
```
I ovo bi trebalo da funkcioniše kako se očekuje.

	$ go run main.go
	3
	6
	ab

Takođe možemo koristiti `constraints` paket koji definiše skup korisnih ograničenja koja se koriste sa parametrima tipa.
```
type Signed interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}
type Unsigned interface {
	~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr
}
type Integer interface {
	Signed | Unsigned
}
type Float interface {
	~float32 | ~float64
}
type Complex interface {
	~complex64 | ~complex128
}
type Ordered interface {
	Integer | Float | ~string
}
```
Za to ćemo morati da instaliramo `constraints` paket.

	$ go get golang.org/x/exp/constraints
	go: added golang.org/x/exp v0.0.0-20220414153411-bcd21879b8fd
```
import (
	"fmt"
	"golang.org/x/exp/constraints"
)
func sum[T constraints.Ordered](a, b T) T {
	return a + b
}
func main() {
	fmt.Println(sum(1, 2))
	fmt.Println(sum(4.0, 2.0))
	fmt.Println(sum("a", "b"))
}
```
Ovde koristimo `Ordered` ograničenje.
```
type Ordered interface {
	Integer | Float | ~string
}
```
`~` je novi token dodat u Go, a izraz `~string` označava skup svih tipova čiji je osnovni tip string.

I dalje radi kako se očekuje.

	$ go run main.go
	3
	6
	ab

Generici su neverovatna karakteristika jer omogućavaju pisanje apstraktnih funkcija koje mogu drastično smanjiti dupliranje koda u određenim slučajevima.

### Kada koristiti generike

Možemo uzeti sledeće slučajeve upotrebe kao primer:

- Funkcije koje rade na nizovima, isečcima, mapama i kanalima.
- Strukture podataka opšte namene kao što su stek ili povezana lista.
- Uvek da bi se smanjilo dupliranje koda.

Na kraju, iako su generički izrazi odličan dodatak jeziku, treba ih koristiti štedljivo. Savetuje se da počnete sa jednostavnim kodom i da pišete generički kod tek kada smo napisali veoma sličan kod najmanje 2 ili 3 puta.

[[Testiranje]](19_Testiranje.md) [[Sadržaj]](toc.md) [[Konkurentnost]](21_Konkurentnost.md)
