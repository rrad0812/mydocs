[[Sync paket]](25_Sync_paket.md) [[Sadržaj]](toc.md) [[Kontekst]](27_Kontekst.md)

# Napredni obrasci konkurentnosti

### Generator pattern

`Generator pattern` se koristi za generisanje niza vrednosti koji se koriste za proizvodnju nekog izlaza.

U našem primeru, imamo `generator` funkciju koja jednostavno vraća kanal iz kojeg možemo da čitamo vrednosti.

Ovo funkcioniše na osnovu činjenice da se slanje i prijem blokiraju dok i pošiljalac i primalac nisu spremni. Ovo svojstvo nam je omogućilo da sačekamo dok se ne zatraži sledeća vrednost.
```
package main
import "fmt"
func main() {
	ch := generator()

	for i := 0; i < 5; i++ {
		value := <-ch
		fmt.Println("Value:", value)
	}
}
func generator() <-chan int {
	ch := make(chan int)
	go func() {
		for i := 0; ; i++ {
			ch <- i
		}
	}()
	return ch
}
```
Ako ovo pokrenemo, primetićemo da možemo da konzumiramo vrednosti koje su proizvedene na zahtev.

	$ go run main.go
	Value: 0
	Value: 1
	Value: 2
	Value: 3
	Value: 4

Ovo je slično ponašanje `yield` u Javaskriptu i Pajtonu.

### Ventilator-in

Šema uključivanja ventilatora kombinuje više ulaza u jedan jedinstveni izlazni kanal. U osnovi, `multipleksiramo` naše ulaze.

U našem primeru, kreiramo ulaze i1,i2 koristeći funkciju `generateWork`. Zatim koristimo varijabilnu funkciju `fanIn` da kombinujemo vrednosti sa ovih ulaza u jedan izlazni kanal iz kojeg možemo da konzumiramo vrednosti.

**Napomena**: redosled unosa neće biti zagarantovan.
```
package main
import (
	"fmt"
	"sync"
)

func main() {
	i1 := generateWork([]int{0, 2, 6, 8})
	i2 := generateWork([]int{1, 3, 5, 7})
	out := fanIn(i1, i2)
	for value := range out {
		fmt.Println("Value:", value)
	}
}
func fanIn(inputs ...<-chan int) <-chan int {
	var wg sync.WaitGroup
	out := make(chan int)
	wg.Add(len(inputs))
	for _, in := range inputs {
		go func(ch <-chan int) {
			for {
				value, ok := <-ch

				if !ok {
					wg.Done()
					break
				}
				out <- value
			}
		}(in)
	}
	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}
func generateWork(work []int) <-chan int {
	ch := make(chan int)
	go func() {
		defer close(ch)
		for _, w := range work {
			ch <- w
		}
	}()
	return ch
}
```
	$ go run main.go
	Value: 0
	Value: 1
	Value: 2
	Value: 6
	Value: 8
	Value: 3
	Value: 5
	Value: 7

### Raspodela

Šabloni raspodele nam u suštini omogućavaju da podelimo naš jedan ulazni kanal na više izlaznih kanala. Ovo je koristan šablon za distribuciju radnih elemenata u više uniformnih aktera.

U našem primeru, delimo ulazni kanal na 4 različita izlazna kanala. Za dinamički broj izlaza, možemo spojiti izlaze u zajednički "agregirani" kanal i koristiti select.

**Napomena**: Obrazac širenja se razlikuje od pub/sub.
```
package main

import "fmt"

func main() {
	work := []int{1, 2, 3, 4, 5, 6, 7, 8}
	in := generateWork(work)

	out1 := fanOut(in)
	out2 := fanOut(in)
	out3 := fanOut(in)
	out4 := fanOut(in)

	for range work {
		select {
		case value := <-out1:
			fmt.Println("Output 1 got:", value)
		case value := <-out2:
			fmt.Println("Output 2 got:", value)
		case value := <-out3:
			fmt.Println("Output 3 got:", value)
		case value := <-out4:
			fmt.Println("Output 4 got:", value)
		}
	}
}

func fanOut(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for data := range in {
			out <- data
		}
	}()

	return out
}

func generateWork(work []int) <-chan int {
	ch := make(chan int)

	go func() {
		defer close(ch)

		for _, w := range work {
			ch <- w
		}
	}()

	return ch
}
```
Kao što vidimo, naš rad je podeljen između više gorutina.

	$ go run main.go
	Output 1 got: 1
	Output 2 got: 3
	Output 4 got: 4
	Output 1 got: 5
	Output 3 got: 2
	Output 3 got: 6
	Output 3 got: 7
	Output 1 got: 8

### Cevovod

Šablon cevovoda je niz faza povezanih kanalima, gde je svaka faza grupa gorutina koje izvršavaju istu funkciju.

U svakoj fazi, gorutine:

- Primaju vrednosti iz uzvodnog sistema putem dolaznih kanala.
- Izvršavaju neku funkciju na tim podacima, obično proizvodeći nove vrednosti.
- Šalju vrednosti nizvodno putem odlaznih kanala.

Svaka faza ima neograničen broj dolaznih i odlaznih kanala, osim prve i poslednje faze, koje imaju samo odlazne odnosno dolazne kanale. Prva faza se ponekad naziva izvor ili proizvođač; poslednja faza je ponor ili potrošač.

Korišćenjem cevovoda, razdvajamo brige svake faze, što pruža brojne prednosti kao što su:

- Izmene faze nezavisno jednu od druge.
- Kombinovanje načina kombinovanja faza nezavisno od same izmene faze.

U našem primeru, definisali smo tri faze, filter, square i half.
```
import (
	"fmt"
	"math"
)

func main() {
	in := generateWork([]int{0, 1, 2, 3, 4, 5, 6, 7, 8})

	out := filter(in) // Filter odd numbers
	out = square(out) // Square the input
	out = half(out)   // Half the input

	for value := range out {
		fmt.Println(value)
	}
}

func filter(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for i := range in {
			if i%2 == 0 {
				out <- i
			}
		}
	}()

	return out
}

func square(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for i := range in {
			value := math.Pow(float64(i), 2)
			out <- int(value)
		}
	}()

	return out
}

func half(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for i := range in {
			value := i / 2
			out <- value
		}
	}()

	return out
}

func generateWork(work []int) <-chan int {
	ch := make(chan int)

	go func() {
		defer close(ch)

		for _, w := range work {
			ch <- w
		}
	}()

	return ch
}
```
Izgleda da je naš unos ispravno obrađen od strane cevovoda na istovremeni način.

	$ go run main.go
	0
	2
	8
	18
	32

### Worker pool

`Worker pool` je zaista moćan obrazac koji nam omogućava da istovremeno raspodelimo posao na više workera (gorutina).

U našem primeru, imamo jobs kanal na koji ćemo slati naše poslove i results kanal na koji će naši radnici slati rezultate kada završe posao.

Nakon toga, možemo istovremeno pokrenuti radnike i jednostavno primati rezultate sa results kanala.

Idealno `totalWorkers` bi bilo, da bude podešeno na `runtime.NumCPU()` što nam daje broj logičkih procesora koje trenutni proces može da koristi.
```
package main

import (
	"fmt"
	"sync"
)

const totalJobs = 4
const totalWorkers = 2

func main() {
	jobs := make(chan int, totalJobs)
	results := make(chan int, totalJobs)

	for w := 1; w <= totalWorkers; w++ {
		go worker(w, jobs, results)
	}

	// Send jobs
	for j := 1; j <= totalJobs; j++ {
		jobs <- j
	}

	close(jobs)

	// Receive results
	for a := 1; a <= totalJobs; a++ {
		<-results
	}

	close(results)
}

func worker(id int, jobs <-chan int, results chan<- int) {
	var wg sync.WaitGroup

	for j := range jobs {
		wg.Add(1)

		go func(job int) {
			defer wg.Done()

			fmt.Printf("Worker %d started job %d\n", id, job)

			// Do work and send result
			result := job * 2
			results <- result

			fmt.Printf("Worker %d finished job %d\n", id, job)
		}(j)
	}

	wg.Wait()
}
```
Kao što se i očekivalo, naši poslovi su bili raspoređeni među našim radnicima.

	$ go run main.go
	Worker 2 started job 4
	Worker 2 started job 1
	Worker 1 started job 3
	Worker 2 started job 2
	Worker 2 finished job 1
	Worker 1 finished job 3
	Worker 2 finished job 2
	Worker 2 finished job 4

### Čekanje u redu

Šablon čekanja nam omogućava da obrađujemo više elemenata istovremeno.

U našem primeru, koristimo baferovani kanal da simuliramo ponašanje reda čekanja. Jednostavno šaljemo praznu strukturu našem queuekanalu i čekamo da je prethodni proces oslobodi kako bismo mogli da nastavimo.

To je zato što šalje blok baferovanom kanalu samo kada je bafer pun, a prima blok kada je bafer prazan.

Ovde imamo ukupan obim posla od 10 stavki i ograničenje od 2. To znači da možemo obraditi 2 stavke istovremeno.

Obratite pažnju na to kako je naš queuekanal tipa struct{}, jer prazna struktura zauzima nula bajtova memorije.
```
package main

import (
	"fmt"
	"sync"
	"time"
)

const limit = 2
const work = 10

func main() {
	var wg sync.WaitGroup

	fmt.Println("Queue limit:", limit)
	queue := make(chan struct{}, limit)

	wg.Add(work)

	for w := 1; w <= work; w++ {
		process(w, queue, &wg)
	}

	wg.Wait()

	close(queue)
	fmt.Println("Work complete")
}

func process(work int, queue chan struct{}, wg *sync.WaitGroup) {
	queue <- struct{}{}

	go func() {
		defer wg.Done()

		time.Sleep(1 * time.Second)
		fmt.Println("Processed:", work)

		<-queue
	}()
}
```
Ako ovo pokrenemo, primetićemo da se nakratko pauzira kada se obradi svaka druga stavka (što je naš limit) dok naš red čeka da bude izbačen iz reda.

	$ go run main.go
	Queue limit: 2
	Processed: 1
	Processed: 2
	Processed: 4
	Processed: 3
	Processed: 5
	Processed: 6
	Processed: 8
	Processed: 7
	Processed: 9
	Processed: 10
	Work complete

### Dodatni obrasci

Neki dodatni obrasci koje bi moglo biti korisno znati:

- T-kanal
- Kanal mosta
- Kanal prstenastog bafera
- Ograničeni paralelizam

[[Sync paket]](25_Sync_paket.md) [[Sadržaj]](toc.md) [[Kontekst]](27_Kontekst.md)
