[[Interfejsi]](16_Interfejsi.md) [[Sadržaj]](toc.md) [[Panic i recover]](18_Panic_i_recover.md)

# Greške

U Go-u nema obrade izuzetaka. Umesto toga, možemo jednostavno vratiti ugrađeni `error` tip koji je tipa interfejsa.
```
type error interface {
    Error() string
}
```
Deklarišimo jednostavnu *Divide* funkciju koja, kao što ime sugeriše, deli ceo broj *a* sa brojem *b*.
```
func Divide(a, b int) int {
	return a/b
}
```
Sada želimo da u slučaju pokušaja deljenja sa nulom, vratimo grešku. To nas dovodi do konstrukcije greške.

### Konstrukcija grešaka

Postoji više načina da se to uradi, mi ćemo pogledati dva najčešća.

##### errors paket

Prvi je korišćenjem `New` funkcije koju pruža `errors` paket.
```
package main
import "errors"

func main() {}

func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, errors.New("cannot divide by zero")
	}
	return a/b, nil
}
```
Obratite pažnju kako vraćamo `error` sa rezultatom funkcije. Ako nema greške, jednostavno vraćamo ` nil` jer je to `nulta vrednost` greške, jer je na kraju krajeva, to interfejs.

Kako da upravljamo sa ovim načinom tretiranja greške? Ako pozovemo `Divide` funkciju u našoj `main` funkciji:
```
package main
import (
	"errors"
	"fmt"
)

func main() {
	result, err := Divide(4, 0)
	if err != nil {
		fmt.Println(err)
		// Do something with the error
		return
	}
	// Use the result
	fmt.Println(result)
}

func Divide(a, b int) (int, error) {...}
```
	$ go run main.go
	cannot divide by zero

Kao što vidite, jednostavno proveravamo da li je greška `nil` i u skladu sa tim gradimo našu logiku. Ovo se smatra prilično idiomatskim u Go jeziku i videćete da se često koristi.

##### fmt.Errorf funkcija

Drugi način da konstruišemo naše greške je korišćenje `fmt.Errorf` funkcije.

Ova funkcija je slična `fmt.Sprintf` i omogućava nam da formatiramo grešku. Ali umesto vraćanja stringa, vraća grešku.

Često se koristi da doda kontekst ili detalj našim greškama.
```
func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, fmt.Errorf("cannot divide %d by zero", a)
	}
	return a/b, nil
}
```
I trebalo bi da funkcioniše slično.

	$ go run main.go
	cannot divide 4 by zero

##### Očekivane greške

Još jedna važna tehnika u Gou je definisanje očekivanih grešaka kako bi se one mogle eksplicitno proveriti u drugim delovima koda. One se ponekad nazivaju `sentinel` greškama.
```
package main
import (
	"errors"
	"fmt"
)

var ErrDivideByZero = errors.New("cannot divide by zero")

func main() {...}

func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, ErrDivideByZero
	}
	return a/b, nil
}
```
U jeziku Go, smatra se konvencionalnim da se ispred promenljive stavlja prefiks `Err`. Na primer, `ErrNotFound`.

Ali koja je poenta?  Dakle, ovo postaje korisno kada treba da izvršimo drugu granu koda ako se naiđe na određenu vrstu greške.

Na primer, sada možemo eksplicitno proveriti koja se greška dogodila korišćenjem `errors.Is` funkcije.
```
package main
import (
	"errors"
	"fmt"
)

func main() {
	result, err := Divide(4, 0)
	if err != nil {
		switch {
		case errors.Is(err, ErrDivideByZero):
			fmt.Println(err)
			// Do something with the error
		default:
			fmt.Println("no idea!")
		}
		return
	}
	fmt.Println(result)	// Use the result
}

func Divide(a, b int) (int, error) {...}
```
	$ go run main.go
	cannot divide by zero

##### Prilagođene greške

Ove strategije pokrivaju većinu slučajeva upotrebe za obradu grešaka. Ali ponekad su nam potrebne dodatne funkcionalnosti kao što su dinamičke vrednosti unutar naših grešaka.

Ranije smo videli da je error samo interfejs. Dakle, u osnovi, bilo šta može biti `error` sve dok implementira `Error()` metodu koja vraća poruku o grešci kao string.

Dakle, hajde da definišemo našu prilagođenu `DivisionError` strukturu koja će sadržati kod greške i poruku.
```
package main
import (
	"errors"
	"fmt"
)
type DivisionError struct {
	Code int
	Msg  string
}
func (d DivisionError) Error() string {
	return fmt.Sprintf("code %d: %s", d.Code, d.Msg)
}
func main() {...}

func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, DivisionError{
			Code: 2000,
			Msg:  "cannot divide by zero",
		}
	}
	return a/b, nil
}
```
Ovde ćemo koristiti `errors.As` umesto `errors.Is` funkcije da bismo konvertovali grešku u ispravan tip.
```
func main() {
	result, err := Divide(4, 0)
	if err != nil {
		var divErr DivisionError
		switch {
		case errors.As(err, &divErr):
			fmt.Println(divErr)
			// Do something with the error
		default:
			fmt.Println("no idea!")
		}
		return
	}
	fmt.Println(result)	// Use the result
}
func Divide(a, b int) (int, error) {...}
```
	$ go run main.go
	code 2000: cannot divide by zero

Koja je razlika između funkcija `errors.Is` i `errors.As`? Razlika je u tome što ova `As` funkcija proverava da li greška ima određeni tip, za razliku od `Is` funkcije koja ispituje da li je u pitanju određeni objekat greške.

Takođe možemo koristiti tvrdnju tipa, ali to nije poželjno.
```
func main() {
	result, err := Divide(4, 0)
	if e, ok := err.(DivisionError); ok {
		fmt.Println(e.Code, e.Msg) // Output: 2000 cannot divide by zero
		return
	}
	fmt.Println(result)
}
```
Na kraju, treba reći ću da je obrada grešaka u Gou prilično drugačija u poređenju sa tradicionalnim `try/catch` idiomom u drugim jezicima. Ali je veoma moćna jer podstiče programera da obradi grešku na eksplicitan način, što poboljšava čitljivost koda.

[[Interfejsi]](16_Interfejsi.md) [[Sadržaj]](toc.md) [[Panic i recover]](18_Panic_i_recover.md)