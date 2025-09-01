[[Panic i recover]](18_Panic_i_recover.md) [[Sadržaj]](toc.md) [[Generici]](20_Generici.md)

# Testiranje

Napravili smo `math` paket koji sadrži `Add` funkciju koja, kao što i samo ime sugeriše, sabira dva cela broja.
```
package math
func Add(a, b int) int {
	return a + b
}
```
Koristi se u našem `main` paketu ovako:
```
package main
import (
	"example/math"
	"fmt"
)
func main() {
	result := math.Add(2, 2)
	fmt.Println(result)
}
```
I, ako ovo pokrenemo, trebalo bi da vidimo rezultat.

	$ go run main.go
	4

Sada želimo da testiramo `Add` funkciju. 

U programskom jeziku Go deklarišemo test datoteke sa `_test` sufiksom u nazivu datoteke. Dakle, za naš `add.go`, kreiraćemo test kao `add_test.go`. Struktura našeg projekta bi trebalo da izgleda ovako:

├── go.mod<br>
├── main.go<br>
└── math<br>
....├── add.go<br>
....└── add_test.go

Počećemo korišćenjem `math_test` paketa i njegovim uvozom `testing` iz standardne biblioteke. Testiranje je ugrađeno u Go, za razliku od mnogih drugih jezika.

Zašto moramo da koristimo `math_test` kao naš paket, zar ne možemo jednostavno da koristimo isti `math` paket?

Možemo napisati naš test u istom paketu ako želimo, ali rad u odvojenom paketu pomaže da pišemo testove na odvojeniji način.

Sada možemo da kreiramo `TestAdd`funkciju. Ona će uzimati argument tipa `testing.T` koji će nam pružiti korisne metode.
```
package math_test
import "testing"

func TestAdd(t *testing.T) {}
```
Pre nego što dodamo bilo kakvu logiku za testiranje, pokušaćemo da je pokrenemo. Ali ovog puta, ne možemo koristiti `go run` komandu, umesto toga ćemo koristiti `go test` komandu.

	$ go test ./math
	ok      example/math 0.429s

Ovde ćemo imati ime našeg paketa koje je `math`, ali možemo koristiti i relativnu putanju ./.. za testiranje svih paketa.

	$ go test ./..
	?       example [no test files]
	ok      example/math 0.348s

A ako Go ne pronađe nijedan test u paketu, obavestiće nas.

Odlično, hajde da napišemo test kod. Da bismo to uradili, proverićemo naš rezultat sa očekivanom vrednošću i ako se ne poklapaju, možemo koristiti `t.Fail` metodu da test nije prošao.
```
package math_test
import "testing"

func TestAdd(t *testing.T) {
	got := math.Add(1, 1)
	expected := 2
	if got != expected {
		t.Fail()
	}
}
```
Izgleda da je test prošao.

	$ go test math
	ok      example/math    0.412s

Da vidimo šta se dešava ako ne prođemo test, za to možemo jednostavno promeniti očekivani rezultat.
```
package math_test
import "testing"

func TestAdd(t *testing.T) {
	got := math.Add(1, 1)
	expected := 3
	if got != expected {
		t.Fail()
	}
}
```
	$ go test ./math
	ok      example/math    (cached)

Ako ovo vidite, ne brinite. Radi optimizacije, Go testovi su keširani. Možemo koristiti `go clean` komandu da obrišemo keš memoriju, a zatim ponovo pokrenemo test.

	$ go clean -testcache
	$ go test ./math
	--- FAIL: TestAdd (0.00s)
	FAIL
	FAIL    example/math    0.354s
	FAIL

Dakle, ovako će izgledati neuspeh na testu.

### Testovi vođeni isečkom

Ranije smo imali argumente funkcija i očekivane promenljive koje smo upoređivali da bismo utvrdili da li su naši testovi prošli ili ne. Ali šta ako sve to definišemo u jednom isečku i iteriramo ga? Ovo će učiniti naše testove malo fleksibilnijim i pomoći nam da lakše pokrenemo više slučajeva.

Ne brinite, naučićemo ovo na primeru. Zato ćemo početi definisanjem naše `addTestCase` strukture.
```
package math_test
import (
	"example/math"
	"testing"
)
type addTestCase struct {
	a, b, expected int
}
var testCases = []addTestCase{
	{1, 1, 3},
	{25, 25, 50},
	{2, 1, 3},
	{1, 10, 11},
}
func TestAdd(t *testing.T) {
	for _, tc := range testCases {
		got := math.Add(tc.a, tc.b)
		if got != tc.expected {
			t.Errorf("Expected %d but got %d", tc.expected, got)
		}
	}
}
```
Obratite pažnju kako smo deklarisali `addTestCase` malim slovom. Tačno, ne želimo da ga eksportujemo jer nije korisno van naše logike testiranja. Hajde da pokrenemo naš test.

	$ go run main.go
	--- FAIL: TestAdd (0.00s)
	    add_test.go:25: Expected 3 but got 2
	FAIL
	FAIL    example/math    0.334s
	FAIL

Izgleda da su nam testovi otkazali, hajde da ih popravimo ažuriranjem naših test slučajeva.
```
var testCases = []addTestCase{
	{1, 1, 2},
	{25, 25, 50},
	{2, 1, 3},
	{1, 10, 11},
}
```
Radi!

	$ go run main.go
	ok      example/math    0.589s

### Pokrivenost koda

Prilikom pisanja testova, često je važno znati koliki deo vašeg stvarnog koda testovi pokrivaju. Ovo se generalno naziva pokrivenošću koda.

Da bismo izračunali i izvezli pokrivenost za naš test, možemo jednostavno koristiti `-coverprofile` argument sa `go test` komandom.

	$ go test ./math -coverprofile=coverage.out
	ok      example/math    0.385s  coverage: 100.0% of statements

Izgleda da imamo odličnu pokrivenost. Hajde da proverimo i izveštaj koristeći `go tool cover` komandu koja nam daje detaljan izveštaj.

	$ go tool cover -html=coverage.out

Kao što vidimo, ovo je mnogo čitljiviji format. A najbolje od svega je što je ugrađen direktno u standardne alate.

### Fuzzing testiranje

`fuzzing testiranje` je predstavljeno u Go verziji 1.18.

Fuzzing je vrsta automatizovanog testiranja koja kontinuirano manipuliše ulazima u program kako bi pronašla greške.

Go fuzzing koristi smernice za pokrivanje kako bi inteligentno prošao kroz kod koji se fazira kako bi pronašao i prijavio greške korisniku.

Pošto može doći do graničnih slučajeva koje ljudi često propuste, `fuzzing` testiranje može biti posebno vredno za pronalaženje grešaka i bezbednosnih propusta.

Hajde da pokušamo sa primerom:
```
func FuzzTestAdd(f *testing.F) {
	f.Fuzz(func(t *testing.T, a, b int) {
		math.Add(a , b)
	})
}
```
Ako ovo pokrenemo, videćemo da će se automatski kreirati test slučajevi. Pošto je `Add` funkcija prilično jednostavna, testovi će proći.

	$ go test -fuzz FuzzTestAdd example/math
	fuzz: elapsed: 0s, gathering baseline coverage: 0/192 completed
	fuzz: elapsed: 0s, gathering baseline coverage: 192/192 completed, now fuzzing with 8 workers
	fuzz: elapsed: 3s, execs: 325017 (108336/sec), new interesting: 11 (total: 202)
	fuzz: elapsed: 6s, execs: 680218 (118402/sec), new interesting: 12 (total: 203)
	fuzz: elapsed: 9s, execs: 1039901 (119895/sec), new interesting: 19 (total: 210)
	fuzz: elapsed: 12s, execs: 1386684 (115594/sec), new interesting: 21 (total: 212)
	PASS
	ok      foo 12.692s

Ali ako ažuriramo našu `Add` funkciju slučajnim graničnim slučajem tako da će program paničiti ako je b + 10 veće od a.
```
func Add(a, b int) int {
	if a > b + 10 {
		panic("B must be greater than A")
	}
	return a + b
}
```
Ako ponovo pokrenemo test, ovaj granični slučaj će biti uhvaćen `fuzz` testiranjem.

	$ go test -fuzz FuzzTestAdd example/math
	warning: starting with empty corpus
	fuzz: elapsed: 0s, execs: 0 (0/sec), new interesting: 0 (total: 0)
	fuzz: elapsed: 0s, execs: 1 (25/sec), new interesting: 0 (total: 0)
	--- FAIL: FuzzTestAdd (0.04s)
	    --- FAIL: FuzzTestAdd (0.00s)
	        testing.go:1349: panic: B must be greater than A

Mislim da je ovo zaista sjajna karakteristika Go 1.18. Više o `fuzz` testiranju možete saznati sa zvaničnog Go bloga.

[[Panic i recover]](18_Panic_i_recover.md) [[Sadržaj]](toc.md) [[Generici]](20_Generici.md)
