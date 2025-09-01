[[Kanali]](23_Kanali.md) [[Sadržaj]](toc.md) [[Sync paket]](25_Sync_paket.md)

# Select izraz 

Izraz `select` blokira kod i čeka na više operacija kanala istovremeno.

`select` blokira dok se jedan od njegovih slučajeva ne može pokrenuti, a zatim izvršava taj slučaj. Nasumično bira jedan ako je više njih spremno.
```
package main
import (
	"fmt"
	"time"
)

func main() {
	one := make(chan string)
	two := make(chan string)

	go func() {
		time.Sleep(time.Second * 2)
		one <- "One"
	}()

	go func() {
		time.Sleep(time.Second * 1)
		two <- "Two"
	}()

	select {
	case result := <-one:
		fmt.Println("Received:", result)
	case result := <-two:
		fmt.Println("Received:", result)
	}

	close(one)
	close(two)
}
```
Slično kao `switch`, `select` takođe ima podrazumevani slučaj koji se pokreće ako nijedan drugi slučaj nije spreman. Ovo će nam pomoći da šaljemo ili primamo bez blokiranja.
```
func main() {
	one := make(chan string)
	two := make(chan string)

	for x := 0; x < 10; x++ {
		go func() {
			time.Sleep(time.Second * 2)
			one <- "One"
		}()

		go func() {
			time.Sleep(time.Second * 1)
			two <- "Two"
		}()
	}

	for x := 0; x < 10; x++ {
		select {
		case result := <-one:
			fmt.Println("Received:", result)
		case result := <-two:
			fmt.Println("Received:", result)
		default:
			fmt.Println("Default...")
			time.Sleep(200 * time.Millisecond)
		}
	}

	close(one)
	close(two)
}
```
Takođe je važno znati da prazni `select {}` blokira zauvek.
```
func main() {
	...
	select {}

	close(one)
	close(two)
}
```

[[Kanali]](23_Kanali.md) [[Sadržaj]](toc.md) [[Sync paket]](25_Sync_paket.md)