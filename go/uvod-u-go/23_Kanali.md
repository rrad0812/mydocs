[[Gorutine]](22_Gorutine.md) [[Sadržaj]](toc.md) [[Select]](24_Select.md)

# Kanali

Kanal je komunikaciona cev između gorutina. Stvari ulaze na jedan kraj, a izlaze na drugi istim redosledom dok se kanal ne zatvori.

Kanali u Go-u su zasnovani na komunikaciji sekvencijalnih procesa (CSP).

### Kreiranje kanala

Hajde da vidimo kako ih možemo deklarisati:

	var ch chan T

Ovde ispred tipa, T koji je tip vrednosti podataka koju želimo da pošaljemo i primimo, dodajemo ključnu reč `chan` koja predstavlja kanal.

Hajde da pokušamo da ispišemo vrednost našeg kanala tipa string.
```
func main() {
	var ch chan string
	fmt.Println(ch)
}
```
	$ go run main.go
	<nil>

Kao što vidimo, nulta vrednost kanala je `nil` i ako pokušamo da pošaljemo podatke preko kanala, naš program će paničiti.

Dakle, slično kao kod isečaka ili mapa, moramo inicijalizovati kanal koristeći ugrađenu `make` funkciju.
```
func main() {
	ch := make(chan string)
	fmt.Println(ch)
}
```
I ako ovo pokrenemo, možemo videti vrednost pointera na alociranu memoriju različitu od `nil`, što znači da je kanal inicijalizovan.

	$ go run main.go
	0x1400010e060

### Slanje i primanje podataka 

Sada kada imamo osnovno razumevanje kanala, hajde da implementiramo naš prethodni primer koristeći kanale da bismo naučili kako ih možemo koristiti za komunikaciju između naših gorutina.
```
package main
import "fmt"

func speak(arg string, ch chan string) {
	ch <- arg // Send
}

func main() {
	ch := make(chan string)
	go speak("Hello World", ch)
	data := <-ch // Receive
	fmt.Println(data)
}
```
Obratite pažnju kako možemo slati podatke koristeći `channel <- data` i primati podatke koristeći `data := <-channel` sintaksu.

	$ go run main.go
	Hello World

Program je uspešno izvršen.

### Baferovani kanali

Takođe možemo imati baferovane kanale koji prihvataju ograničen broj vrednosti bez odgovarajućeg prijemnika za te vrednosti.

Ova dužina ili kapacitet bafera može se odrediti korišćenjem drugog argumenta funkcije `make`.
```
func main() {
	ch := make(chan string, 2)
	
	go speak("Hello World", ch)
	go speak("Hi again", ch)
	
	data1 := <-ch
	fmt.Println(data1)
	
	data2 := <-ch
	fmt.Println(data2)
}
```
Pošto je ovaj kanal baferovan, možemo poslati ove vrednosti u kanal bez odgovarajućeg konkurentnog prijema. To znači da slanje baferovanom kanalu će biti blokirano samo kada je bafer pun a primanje sa baferovang kanala će biti blokirano samo kada je bafer prazan.

Podrazumevano, kanal je nebaferovan i ima kapacitet 0, stoga možemo da izostavimo drugi argument funkcije `make`.

### Usmereni kanali (unidirectional)

Kada koristimo kanale kao parametre funkcije, možemo da odredimo da li je kanal namenjen samo za slanje ili primanje vrednosti. Ovo povećava bezbednost tipa našeg programa jer podrazumevano kanal može i da šalje i da prima vrednosti.

U našem primeru, možemo ažurirati funkciju *speak* tako da drugi argument naše funkcije može samo poslati vrednost.
```
func speak(arg string, ch chan <- string) {
	ch <- arg // Send Only
}
```
Ovde se `chan <-` može koristiti samo za slanje vrednosti i doći će do panike ako pokušamo da primimo vrednosti.

### Zatvaranje kanala

Kada završimo sa našim kanalom, potrebno ga je zatvoriti. To se može postići pomoću ugrađene `close` funkcije.

Pri zatvaranu, možemo samo da prosledimo naš kanal funkciji `close`.
```
func main() {
	ch := make(chan string, 2)
	
	go speak("Hello World", ch)
	go speak("Hi again", ch)
	
	data1 := <-ch
	fmt.Println(data1)
	
	data2 := <-ch
	fmt.Println(data2)
	
	close(ch)
}
```
Opciono, prijemnici mogu da testiraju da li je kanal zatvoren dodeljivanjem drugog parametra izrazu za prijem.
```
func main() {
	ch := make(chan string, 2)
	
	go speak("Hello World", ch)
	go speak("Hi again", ch)
	
	data1 := <-ch
	fmt.Println(data1)
	
	data2, ok := <-ch		// test on closed channel
	fmt.Println(data2, ok)
	
	close(ch)
}
```
Ako je ok == false onda nema više vrednosti za primanje i kanal je zatvoren.

Na neki način, ovo je slično načinu na koji proveravamo da li ključ postoji ili ne u mapi.

### Svojstva kanala

##### Slanje na `nil` kanal blokira zauvek
```
var c chan string
c <- "Hello, World!" // Panic: all goroutines are asleep - deadlock!
```
##### Prijem sa `nil` kanala se blokira zauvek
```
var c chan string
fmt.Println(<-c) // Panic: all goroutines are asleep - deadlock!
```
##### Slanje na zatvoreni kanal izaziva paniku
```
var c = make(chan string, 1)
c <- "Hello, World!"
close(c)
c <- "Hello, Panic!" // Panic: send on closed channel
```
##### Prijem iz zatvorenog kanala odmah vraća nultu vrednost
```
var c = make(chan int, 2)
c <- 5
c <- 4
close(c)
for i := 0; i < 4; i++ {
    fmt.Printf("%d ", <-c) // Output: 5 4 0 0
}
```
##### Range preko kanala

Možemo koristiti `for range` za iteraciju kroz vrednosti primljene iz kanala.
```
package main
import "fmt"

func main() {
	ch := make(chan string, 2)
	
	ch <- "Hello"
	ch <- "World"
	
	close(ch)
	
	for data := range ch {
		fmt.Println(data)
	}
}
```

[[Gorutine]](22_Gorutine.md) [[Sadržaj]](toc.md) [[Select]](24_Select.md)