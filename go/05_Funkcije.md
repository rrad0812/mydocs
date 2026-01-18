[[Kontrola toka]](04_Kontrola_toka.md)[[Sadržaj]](toc.md) [[Moduli]](06_Moduli.md)

# Funkcije

### Deklaracija funkcije
```
func myFunction() {}
```
I možemo je pozvati ili izvršiti na sledeći način.
```
...
myFunction()
...
```
Hajde da joj prosledimo neke parametre.
```
func main() {
	myFunction("Hello")
}

func myFunction(p1 string) {
	fmt.Println(p1)
}
```
    $ go run main.go

Kao što vidimo, ispisuje našu poruku. Takođe možemo da napravimo skraćenu deklaraciju ako uzastopni parametri imaju isti tip. Na primer:
```
func myNextFunction(p1, p2 string) {}
```
### Vraćanje vrednosti

Sada hajde da vratimo vrednost iz funkcije.
```
func main() {
	s := myFunction("Hello")
	fmt.Println(s)
}

func myFunction(p1 string) string {
	msg := fmt.Sprintf("%s function", p1)
	return msg
}
```
### Višestruki povratak

Go takođe podržava višestruko vraćanje!
```
func main() {
	s, i := myFunction("Hello")
	fmt.Println(s, i)
}

func myFunction(p1 string) (string, int) {
	msg := fmt.Sprintf("%s function", p1)
	return msg, 10
}
```
### Imenovani povratak

Još jedna sjajna funkcija je `named return`, gde se povratna vrednost može imenovati i tretirati kao sopstvena promenljiva funkcije.
```
func myFunction(p1 string) (s string, i int) {
	s = fmt.Sprintf("%s function", p1)
	i = 10
	return
}
```
Obratite pažnju kako smo dodali `return` iskaz bez ikakvih argumenata, ovo je takođe poznato kao `naked return`.

Reći ću da, iako je ova funkcija zanimljiva, molim vas da je koristite pažljivo jer bi to moglo smanjiti čitljivost većih funkcija.

### Funkcije kao vrednosti

Zatim, hajde da pričamo o funkcijama kao vrednostima, u Go-u su funkcije građani prve prve klase i možemo ih koristiti kao vrednosti. Dakle, hajde da sredimo našu funkciju i isprobamo je!
```
func myFunction() {
	fn := func() {
		fmt.Println("inside fn")
	}
	fn()
}
```
Takođe možemo pojednostaviti ovo tako što ćemo napraviti ananonimnu funkciju.
```
func myFunction() {
	func() {
		fmt.Println("inside fn")
	}()
}
```
Obratite pažnju kako to izvršavamo koristeći zagradu na kraju.

### Zatvaranja

Hajde da vratimo funkciju i tako kreiramo nešto što se zove zatvaranje. Jednostavna definicija može biti da je zatvaranje funkcija koja referencira promenljive izvan svog tela.
 
Zatvaranja su leksički ograničena, što znači da funkcije mogu pristupiti vrednostima u opsegu svog definisanja.
```
func myFunction() func(int) int {
	sum := 0
	return func(v int) int {
		sum += v
		return sum
	}
}
...
add := myFunction()
add(5)
fmt.Println(add(10))
...
```
Kao što vidimo, dobijamo rezultat 15 jer *sum* je promenljiva povezana sa funkcijom. Ovo je veoma moćan koncept i definitivno ga morate znati.

### Varijadičke funkcije

Varijadičke funkcije mogu prihvatiti nula ili više argumenata koristeći elipsis `...` operator.

Primer ovde bi bila funkcija koja može da primi gomilu vrednosti.
```
func main() {
	sum := add(1, 2, 3, 5)
	fmt.Println(sum)
}

func add(values ...int) int {
	sum := 0

	for _, v := range values {
		sum += v
	}

	return sum
}
```
O `range` ključnoj reči ćemo kasnije razgovarati u kursu.

**Zanimljivost**: `fmt.Println` je varijadička funkcija, zato smo joj mogli proslediti više vrednosti.

### Init

U Gou, `init` je posebna funkcija životnog ciklusa aplikacije koja se izvršava pre `main` funkcije.

Slično kao `main`, `init` funkcija ne prima nikakve argumente niti vraća bilo kakvu vrednost. Pogledajmo kako to funkcioniše na primeru.
```
package main
import "fmt"

func init() {
	fmt.Println("Before main!")
}

func main() {
	fmt.Println("Running main")
}
```
Kao što se i očekivalo, `init` funkcija je izvršena pre main funkcije.

    $ go run main.go
    Before main!
    Running main

Za razliku od `main`, može postojati više od jedne `init` funkcije u jednoj ili više datoteka.

Za više `init` funkcije u jednoj datoteci, njihova obrada se vrši redosledom njihove deklaracije, dok se `init` funkcije deklarisane u više datoteka obrađuju prema leksikografskom redosledu imena datoteke.
```
package main
import "fmt"

func init() {
	fmt.Println("Before main!")
}

func init() {
	fmt.Println("Hello again?")
}

func main() {
	fmt.Println("Running main")
}
```
I ako ovo pokrenemo, videćemo da su `init` funkcije izvršene redosledom kojim su deklarisane.

    $ go run main.go
    Before main!
    Hello again?
    Running main

Funkcija `init` je opcionalna i posebno se koristi za bilo koje globalno podešavanje koje može biti neophodno za naš program, kao što je: 
- uspostavljanje veze sa bazom podataka, 
- preuzimanje konfiguracionih datoteka, 
- podešavanje promenljivih okruženja itd.

### Defer
Hajde da razgovaramo o `defer` ključnoj reči koja nam omogućava da odložimo izvršavanje funkcije dok se okolna funkcija ne vrati.
```
func main() {
	defer fmt.Println("I am finished")
	fmt.Println("Doing some work...")
}
```
Možemo koristiti više odloženih funkcija, ovo nas dovodi do onoga što je poznato kao `defer` stek. Pogledajmo primer:
```
func main() {
	defer fmt.Println("I am finished")
	defer fmt.Println("Are you?")
	fmt.Println("Doing some work...")
}
```
    $ go run main.go
    Doing some work...
    Are you?
    I am finished

Kao što vidimo, `defer` izjave se slažu i izvršavaju po principu `LIFO` steka.

Dakle, `defer` je neverovatno koristna i često se koristi za čišćenje ili rukovanje greškama.

Funkcije se takođe mogu koristiti sa generičkim tipovima, ali ćemo o njima razgovarati kasnije u kursu.

[[Kontrola toka]](04_Kontrola_toka.md)[[Sadržaj]](toc.md) [[Moduli]](06_Moduli.md)