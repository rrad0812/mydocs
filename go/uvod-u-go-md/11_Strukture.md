[[Pointeri]](10_Pointeri.md) [[Sadržaj]](toc.md) [[Metode]](12_Metode.md)

# Strukture

Dakle, `struct` je korisnički definisan tip koji sadrži kolekciju imenovanih polja. Koristi se za grupisanje povezanih podataka zajedno u jednu jedinicu.

Ako dolazite iz objektno orijentisanog okruženja, zamislite strukture kao lagane klase koje podržavaju **kompoziciju**, ali ne i **nasleđivanje**.

### Definisanje

Možemo definisati struct ovako:
```
type Person struct {}
```
Koristimo `type` ključnu reč da bismo uveli novi tip, nakon čega sledi ime tipa, a zatim `struct` ključna reč da bismo naznačili osnovni tip, koji je u ovom slučaju `struct`.

Sada, hajde da dodamo strukturi *Person* neka polja:
```
type Person struct {
	FirstName string
	LastName  string
	Age       int
}
```
A ako polja imaju isti tip, možemo ih napisati u kompaktnoj formi:
```
type Person struct {
	FirstName, LastName string
	Age                 int
}
```
### Deklarisanje i inicijalizacija

Sada kada imamo definisanu strukturu, možemo deklarisati njenu instancu isto kao i za druge tipove podataka.
```
func main() {
	var p1 Person
	fmt.Println("Person 1:", p1)
}
```
	$ go run main.go
	Person 1: {  0}

Kao što vidimo, sva polja strukture su inicijalizovana svojim nultim vrednostima. Dakle, FirstName i LastName su postavljeni na "" - prazan string, a Age je postavljeno na 0.

Takođe strukturu možemo inicijalizovati sa `struct literal`-om.
```
func main() {
	var p1 Person
	fmt.Println("Person 1:", p1)
	var p2 = Person{FirstName: "Karan", LastName: "Pratap Singh", Age: 22}
	fmt.Println("Person 2:", p2)
}
```
Radi čitljivosti, možemo razdvojiti polja literala novim redom, ali će to zahtevati završni zarez.
```
	var p2 = Person{
		FirstName: "Karan",
		LastName:  "Pratap Singh",
		Age:       22,
	}
```
    $ go run main.go
    Person 1: {  0}
    Person 2: {Karan Pratap Singh 22}

Takođe možemo inicijalizovati samo podskup polja.
```
func main() {
	var p1 Person
	fmt.Println("Person 1:", p1)
	
	var p2 = Person{
		FirstName: "Karan",
		LastName:  "Pratap Singh",
		Age:       22,
	}
	fmt.Println("Person 2:", p2)

	var p3 = Person{
		FirstName: "Tony",
		LastName:  "Stark"`,
	}
	fmt.Println("Person 3:", p3)
}
```
    $ go run main.go
    Person 1: {  0}
    Person 2: {Karan Pratap Singh 22}
    Person 3: {Tony Stark 0}

Kao što vidimo, polje *Age* strukture p3 3 je podrazumevano podešeno na nultu vrednost. 

### Inicijalizacija strukture poljima bez imena

Go strukture takođe podržavaju inicijalizaciju bez imena polja.
```
func main() {
	var p1 Person
	fmt.Println("Person 1:", p1)

	var p2 = Person{
		FirstName: "Karan",
		LastName:  "Pratap Singh",
		Age:       22,
	}
	fmt.Println("Person 2:", p2)

	var p3 = Person{
		FirstName: "Tony",
		LastName:  "Stark",
	}
	fmt.Println("Person 3:", p3)

	var p4 = Person{"Bruce", "Wayne"}
	fmt.Println("Person 4:", p4)
}
```
Ali evo u čemu je problem, moraćemo da obezbedimo sve vrednosti tokom inicijalizacije ili ona neće uspeti.

	$ go run main.go
	# command-line-arguments
	./main.go:30:27: too few values in Person{...}
```
	var p4 = Person{"Bruce", "Wayne", 40}
	fmt.Println("Person 4:", p4)
```
Takođe možemo deklarisati anonimnu strukturu.
```
func main() {
	var p1 Person
	fmt.Println("Person 1:", p1)

	var p2 = Person{
		FirstName: "Karan",
		LastName:  "Pratap Singh",
		Age:       22,
	}
	fmt.Println("Person 2:", p2)

	var p3 = Person{
		FirstName: "Tony",
		LastName:  "Stark",
	}
	fmt.Println("Person 3:", p3)

	var p4 = Person{"Bruce", "Wayne", 40}
	fmt.Println("Person 4:", p4)

	var a = struct {
		Name string
	}{"Golang"}
	fmt.Println("Anonymous:", a)
}
```
### Pristupanje poljima struktura

Pristupanje pojedinačnim poljima strukture:
```
func main() {
	var p = Person{
		FirstName: "Karan",
		LastName:  "Pratap Singh",
		Age:       22,
	}
	fmt.Println("FirstName", p.FirstName)
}
```
Takođe možemo kreirati pointer na strukture, i pristupati preko pointera na pojedina polja strukture:
```
func main() {
	var p = Person{
		FirstName: "Karan",
		LastName:  "Pratap Singh",
		Age:       22,
	}
	ptr := &p
	fmt.Println((*ptr).FirstName)
	fmt.Println(ptr.FirstName)
}
```
Ovde je jedna od značajnih osobina Goa, obe izjave su jednake jer **u Go-u ne moramo eksplicitno dereferencirati pointer**. 

Takođe možemo koristiti ugrađenu `new` funkciju.
```
func main() {
	p := new(Person)
	p.FirstName = "Karan"
	p.LastName = "Pratap Singh"
	p.Age = 22
	fmt.Println("Person", p)
}
```
	$ go run main.go
	Person &{Karan Pratap Singh 22}

Uzgred, dve strukture su jednake ako su sva njihova odgovarajuća polja takođe jednaka.
```
func main() {
	var p1 = Person{"a", "b", 20}
	var p2 = Person{"a", "b", 20}
	fmt.Println(p1 == p2)
}
```
	$ go run main.go
	true

### Izvezena polja

Isto kao i kod pravila za promenljive i funkcije, ako je polje strukture deklarisano malim slovima, ono neće biti izvezeno i biće vidljivo samo paketu u kojem je definisano.
```
type Person struct {
	FirstName, LastName  string
	Age                  int
	zipCode              string
}
```
Dakle, *zipCode* polje neće biti izvezeno. Takođe, isto važi i za Person strukturu, ako je preimenujemo u *person*, ona se takođe neće izvesti.
```
type person struct {
	FirstName, LastName  string
	Age                  int
	zipCode              string
}
```
### Ugrađivanje i kompozicija

Go ne podržava nasleđivanje, ali možemo uraditi nešto slično sa **ugrađivanjem**.
```
type Person struct {
	FirstName, LastName  string
	Age                  int
}

type SuperHero struct {
	Person
	Alias string
}
```
Naša nova struktura će imati sva svojstva originalne strukture. I trebalo bi da se ponaša isto kao i naša normalna struktura.
```
func main() {
	s := SuperHero{}
	s.FirstName = "Bruce"
	s.LastName = "Wayne"
	s.Age = 40
	s.Alias = "batman"
	fmt.Println(s)
}
```
	$ go run main.go
	{{Bruce Wayne 40} batman}

Međutim, ovo se obično ne preporučuje i u većini slučajeva, **kompozicija** je poželjnija. Dakle, umesto ugrađivanja, jednostavno ćemo ga definisati kao normalno polje.
```
type Person struct {
	FirstName, LastName  string
	Age                  int
}

type SuperHero struct {
	Person Person
	Alias  string
}
```
Dakle, možemo prepisati naš primer i sa kompozicijom.
```
func main() {
	p := Person{"Bruce", "Wayne", 40}
	s := SuperHero{p, "batman"}

	fmt.Println(s)
}
```
	$ go run main.go
	{{Bruce Wayne 40} batman}

Ovde nema tačnog ili pogrešnog, jer ugrađivanje ponekad bude korisno.

### Strukturne oznake

Strukturna oznaka je samo oznaka koja nam omogućava da polju dodamo metapodatke koje se mogu koristiti za prilagođeno ponašanje pomoću `reflect` paketa.

Hajde da naučimo kako možemo definisati strukturne oznake.
```
type Animal struct {
	Name    string `key:"value1"`
	Age     int    `key:"value2"`
}
```
Oznake ćete često pronaći u paketima za kodiranje, kao što su XML, JSON, YAML, ORM i upravljanje konfiguracijom.

Evo primera oznaka za JSON enkoder.
```
type Animal struct {
	Name    string `json:"name"`
	Age     int    `json:"age"`
}
```
### Svojstva struktura

Strukture su **vrednosni** tipovi. Kada dodelimo jednu struct promenljivu drugoj, kreira se i dodeljuje se novoj struct kopija te promenljive.

Slično, kada prosledimo struct drugoj funkciji, funkcija dobija svoju struct kopiju.
```
package main
import "fmt"

type Point struct {
	X, Y float64
}

func main() {
	p1 := Point{1, 2}
	p2 := p1 // Copy of p1 is assigned to p2
	p2.X = 2
	fmt.Println(p1) // Output: {1 2}
	fmt.Println(p2) // Output: {2 2}
}
```
Prazna struktura zauzima nula bajtova memorije.
```
package main
import (
	"fmt"
	"unsafe"
)

func main() {
	var s struct{}
	fmt.Println(unsafe.Sizeof(s)) // Output: 0
}
```

[[Pointeri]](10_Pointeri.md) [[Sadržaj]](toc.md) [[Metode]](12_Metode.md)