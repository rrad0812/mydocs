[[Napredni obrasci konkurentnosti]](26_Napredni_obrasci_konkurentnosti.md) [[Sadržaj]](toc.md)

# Kontekst

Context je interface tip koji je definisan na sledeći način:
```
type Context interface {
	Deadline() (deadline time.Time, ok bool)
	Done() <-chan struct{}
	Err() error
	Value(key any) any
}
```
Tip Context ima sledeće metode:

- `Done() <- chan struct{}` vraća kanal koji je zatvoren kada se kontekst otkaže ili 
  kada istekne vreme. `Done` može vratiti vrednost `nil` ako se kontekst nikada ne može otkazati.
- `Deadline() (deadline time.Time, ok bool)` vraća vreme kada će kontekst biti otkazan  
  ili će vremenski isteći. Rok vraća okkao falsekada rok nije podešen.
- `Err() error` vraća grešku koja objašnjava zašto je kanal Done zatvoren. Ako `Done` još 
  nije zatvoren, vraća `nil`.
- `Value(key any) any` vraća vrednost povezanu sa ključem ili `nil` ako je nema.

### CancelFunc

`CancelFunc` naređuje operaciji da prekine svoj rad i ne čeka da se rad zaustavi. Ako je pozove više gorutina istovremeno, nakon prvog poziva, naredni pozivi kategorije za `CancelFunc` ne rade ništa.

	type CancelFunc func()

### Upotreba

Funkcije koje su izložene u `context` paketu:

##### Background

`Background` vraća praznu vrednost, različitu od `nil` Context. Nikada se ne otkazuje, nema vrednosti i nema rok.

Obično je koriste `main` funkcija, inicijalizacija i testovi, i kao kontekst najvišeg nivoa za dolazne zahteve.

	func Background() Context

##### ToDo

Slično `Background` funkciji, `ToDo` funkcija takođe vraća vrednost koja nije nula, praznu vrednost Context.

Međutim, trebalo bi je koristiti samo kada nismo sigurni koji kontekst da koristimo ili ako funkcija nije ažurirana da bi primila kontekst. To znači da planiramo da dodamo kontekst funkciji u budućnosti.

	func TODO() Context

##### WithValue

Ova funkcija uzima kontekst i vraća izvedeni kontekst gde je vrednost *val* povezana sa kontekstom *key* i prolazi kroz stablo konteksta sa njim.

To znači da kada dobijete kontekst sa vrednošću, svaki kontekst koji iz njega proizilazi dobija tu vrednost.

Ne preporučuje se prosleđivanje kritičnih parametara koristeći kontekstualne vrednosti, umesto toga, funkcije bi trebalo da prihvate te vrednosti u potpisu, čineći ga eksplicitnim.

	func WithValue(parent Context, key, val any) Context

**Primer**

Uzmimo jednostavan primer da vidimo kako možemo dodati par ključ-vrednost u kontekst.
```
package main
import (
	"context"
	"fmt"
)

func main() {
	processID := "abc-xyz"

	ctx := context.Background()
	ctx = context.WithValue(ctx, "processID", processID)

	ProcessRequest(ctx)
}

func ProcessRequest(ctx context.Context) {
	value := ctx.Value("processID")
	fmt.Printf("Processing ID: %v", value)
}
```
I ako ovo pokrenemo, videćemo da processID se prenosi preko našeg konteksta.

	$ go run main.go
	Processing ID: abc-xyz

##### WithCancel

Ova funkcija kreira novi kontekst iz roditeljskog konteksta i izvedenog konteksta i funkcije otkazivanja. Roditelj može biti `context.Background` ili kontekst koji je prosleđen u funkciju.

Otkazivanje ovog konteksta oslobađa resurse povezane sa njim, tako da bi kod trebalo da pozove funkciju otmena čim se operacije koje se izvršavaju u ovom kontekstu završe.

Prenošenje `cancel` funkcije se ne preporučuje jer može dovesti do neočekivanog ponašanja.

	func WithCancel(parent Context) (ctx Context, cancel CancelFunc)

##### WithDeadline

Ova funkcija vraća izvedeni kontekst iz svog roditelja koji se otkazuje kada istekne rok ili kada se pozove funkcija otkazivanja.

Na primer, možemo kreirati kontekst koji će se automatski otkazati u određenom trenutku u budućnosti i to proslediti podređenim funkcijama. Kada se taj kontekst otkaza zbog isteka roka, sve funkcije koje su dobile kontekst dobijaju obaveštenje da prestanu sa radom i vrate se.

	func WithDeadline(parent Context, d time.Time) (Context, CancelFunc)

##### WithTimeout

Ova funkcija je samo omotač oko WithDeadlinefunkcije sa dodatnim vremenskim ograničenjem.

	func WithTimeout(parent Context, timeout time.Duration) (Context, CancelFunc) {
		return WithDeadline(parent, time.Now().Add(timeout))
	}

##### Primer

U primeru ispod, imamo jednostavan HTTP server koji obrađuje zahtev.
```
package main

import (
	"fmt"
	"net/http"
	"time"
)

func handleRequest(w http.ResponseWriter, req *http.Request) {
	fmt.Println("Handler started")
	context := req.Context()

	select {
	// Simulating some work by the server, waits 5 seconds and then responds.
	case <-time.After(5 * time.Second):
		fmt.Fprintf(w, "Response from the server")

	// Handling request cancellation
	case <-context.Done():
		err := context.Err()
		fmt.Println("Error:", err)
	}

	fmt.Println("Handler complete")
}

func main() {
	http.HandleFunc("/request", handleRequest)

	fmt.Println("Server is running...")
	http.ListenAndServe(":4000", nil)
}
```
Otvorimo dva terminala. U terminalu jedan ćemo pokrenuti naš primer.

	$ go run main.go
	Server is running...
	Handler started
	Handler complete

U drugom terminalu, jednostavno ćemo poslati zahtev našem serveru. I ako sačekamo 5 sekundi, dobićemo odgovor.

	$ curl localhost:4000/request
	Response from the server

Sada, da vidimo šta se dešava ako otkažemo zahtev pre nego što se završi.

**Napomena**: možemo koristiti `ctrl + c` da otkažemo zahtev na pola puta.

	$ curl localhost:4000/request
	^C

I kao što vidimo, u mogućnosti smo da detektujemo otkazivanje zahteva zbog konteksta zahteva.

	$ go run main.go
	Server is running...
	Handler started
	Error: context canceled
	Handler complete

Ovo možemo koristiti da otkažemo bilo koji posao koji zahteva mnogo resursa ako više nije potreban ili je prekoračio rok ili vremenski istekao.
*/

[[Napredni obrasci konkurentnosti]](26_Napredni_obrasi_konkurentnosti.md) [[Sadržaj]](toc.md)
