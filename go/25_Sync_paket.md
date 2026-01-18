[[Select]](24_Select.md) [[Sadržaj]](toc.md) [[Napredni obrasci konkurentnosti]](26_Napredni_obrasci_konkurentnosti.md)

# Sync paket

`gorutine` se izvršavaju u istom adresnom prostoru, tako da pristup deljenoj memoriji mora biti sinhronizovan. `sync` paket pruža korisne primitive.

### WaitGroup

Grupa čeka da se završi izvršavanje kolekcije gorutina. *Main* gorutina poziva *Add* da bi podesila broj gorutina koje treba čekati. Zatim se svaka od gorutina pokreće i poziva `Done` kada se završi. Istovremeno, `Wait` se može koristiti za blokiranje dok se sve gorutine ne završe.

##### Upotreba

Možemo koristiti `sync.WaitGroup` sledeće metode:

- `Add(delta int)` uzima celobrojnu vrednost koja je u suštini broj gorutina koje `WaitGroup` treba da 
  čeka. Ovo mora biti pozvano pre nego što izvršimo gorutinu.
- `Done()` se poziva unutar gorutine da signalizira da je gorutina uspešno izvršena.
- `Wait()` blokira program dok se sve gorutine koje je odredio Add()ne pozovu Done()iznutra.

Hajde da pogledamo jedan primer.
```
package main
import (
	"fmt"
	"sync"
)
func work() {
	fmt.Println("working...")
}

func main() {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		work()
	}()
	wg.Wait()
}
```
Ako ovo pokrenemo, možemo videti da naš program radi kako se očekuje.

	$ go run main.go
	working...

Takođe možemo `WaitGroup` direktno proslediti funkciji.
```
func work(wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Println("working...")
}

func main() {
	var wg sync.WaitGroup
	wg.Add(1)
	go work(&wg)
	wg.Wait()
}
```
Ali je važno znati da se `WaitGroup` ne sme kopirati nakon prve upotrebe. A ako se eksplicitno prosleđuje u funkcije, to treba da se uradi pomoću pointera. To je zato što može uticati na naš brojač, što će poremetiti logiku našeg programa.

Hajde da povećamo i broj gorutina pozivanjem `Add` metode koja čeka 4 gorutine.
```
func main() {
	var wg sync.WaitGroup
	
	wg.Add(4)
	go work(&wg)
	go work(&wg)
	go work(&wg)
	go work(&wg)

	wg.Wait()
}
```
I kao što se i očekivalo, sve naše gorutine su izvršene.

	$ go run main.go
	working...
	working...
	working...
	working...

### Mutex

Mutex je međusobno isključujuća brava koja sprečava druge procese da uđu u kritični deo podataka dok ga neki proces zauzima kako bi se sprečilo nastajanje uslova trke.

##### Šta je kritični sekcija?

Dakle, kritična sekcija može biti deo koda koji ne sme da se izvršava od strane više niti istovremeno jer kod sadrži deljene resurse.

##### Upotreba

Možemo `sync.Mutex` koristiti sledeće metode:

- `Lock()` stiče ili drži bravu.
- `Unlock()` otključava bravu.
- `TryLock()` pokušava da zaključa i javlja da li je uspelo.

Hajde da pogledamo primer, kreiraćemo `Counter` strukturu i dodati `Update` metodu koja će ažurirati internu vrednost.
```
package main
import (
	"fmt"
	"sync"
)
type Counter struct {
	value int
}
func (c *Counter) Update(n int, wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Printf("Adding %d to %d\n", n, c.value)
	c.value += n
}

func main() {
	var wg sync.WaitGroup

	c := Counter{}

	wg.Add(4)

	go c.Update(10, &wg)
	go c.Update(-5, &wg)
	go c.Update(25, &wg)
	go c.Update(19, &wg)

	wg.Wait()
	fmt.Printf("Result is %d", c.value)
}
```
Hajde da ovo pokrenemo i vidimo šta će se desiti.

	$ go run main.go
	Adding -5 to 0
	Adding 10 to 0
	Adding 19 to 0
	Adding 25 to 0
	Result is 49

To ne izgleda tačno, izgleda kao da je naša vrednost uvek nula, ali smo nekako dobili tačan odgovor.

Pa, to je zato što, u našem primeru, više gorutina ažurira *value* promenljivu. I kao što ste verovatno pretpostavili, ovo nije idealno.

Ovo je savršen slučaj upotrebe za Mutex. Dakle, hajde da počnemo tako što ćemo koristiti `sync`.Mutex umotava našu kritičnu sekciju između metoda `Lock()` i `Unlock()`.
```
package main
import (
	"fmt"
	"sync"
)
type Counter struct {
	m     sync.Mutex
	value int
}
func (c *Counter) Update(n int, wg *sync.WaitGroup) {
	c.m.Lock()
	defer wg.Done()
	fmt.Printf("Adding %d to %d\n", n, c.value)
	c.value += n
	c.m.Unlock()
}

func main() {
	var wg sync.WaitGroup
	c := Counter{}
	wg.Add(4)
	go c.Update(10, &wg)
	go c.Update(-5, &wg)
	go c.Update(25, &wg)
	go c.Update(19, &wg)

	wg.Wait()
	fmt.Printf("Result is %d", c.value)
}
```
	$ go run main.go
	Adding -5 to 0
	Adding 19 to -5
	Adding 25 to 14
	Adding 10 to 39
	Result is 49

Izgleda da smo rešili problem i rezultat takođe izgleda ispravno.

**Napomena**: Slično kao kod `WaitGroup`, `Mutex` se ne sme kopirati nakon prve upotrebe.

### RWMutex

RWMutex je međusobno isključujuća brava čitača/pisača. Bravu može da drži proizvoljan broj čitača ili jedan pisac.

Drugim rečima, čitaoci ne moraju da čekaju jedni druge. Treba samo da čekaju pisce koji drže bravu.

`sync.RWMutex` je stoga poželjnije za podatke koji se uglavnom čitaju, a resurs koji se štedi u poređenju sa `sync.Mutex` je vreme.

##### Upotreba

Slično kao `sync.Mutex`, možemo koristiti `sync.RWMutex` sa sledećim metodama:

`Lock()` stiče ili drži bravu.
`Unlock()` otključava bravu.
`RLock()` stiče ili drži zaključavanje za čitanje.
`RUnlock()` otključava za čitanje.

Obratite pažnju kako RWMutex ima dodatne RLockmetode RUnlocku poređenju sa Mutex-om.

##### Primer

Dodajmo `GetValue` metodu koja će čitati vrednost brojača. Takođe ćemo promeniti `sync.Mutex` u `sync.RWMutex`.

Sada možemo jednostavno koristiti metode `RLock` i `RUnlock` tako da čitaoci ne moraju da čekaju jedni druge.
```
package main
import (
	"fmt"
	"sync"
	"time"
)

type Counter struct {
	m     sync.RWMutex
	value int
}
func (c *Counter) Update(n int, wg *sync.WaitGroup) {
	defer wg.Done()

	c.m.Lock()
	fmt.Printf("Adding %d to %d\n", n, c.value)
	c.value += n
	c.m.Unlock()
}
func (c *Counter) GetValue(wg *sync.WaitGroup) {
	defer wg.Done()

	c.m.RLock()
	defer c.m.RUnlock()
	fmt.Println("Get value:", c.value)
	time.Sleep(400 * time.Millisecond)
}

func main() {
	var wg sync.WaitGroup

	c := Counter{}

	wg.Add(4)

	go c.Update(10, &wg)
	go c.GetValue(&wg)
	go c.GetValue(&wg)
	go c.GetValue(&wg)

	wg.Wait()
}
```
	$ go run main.go
	Get value: 0
	Adding 10 to 0
	Get value: 10
	Get value: 10

**Napomena**: I `sync.Mutex` i `sync.RWMutex` implementiraju `sync.Locker` interfejs.
```
type Locker interface {
    Lock()
    Unlock()
}
```
### Cond

Uslovna `sync.Cond` promenljiva može se koristiti za koordiniranje onih gorutina koje žele da dele resurse. Kada se stanje deljenih resursa promeni, može se koristiti za obaveštavanje gorutina ko je blokirao mutex.

Svaki `Cond` ima pridruženu bravu (često `*Mutex` ili `*RWMutex`), koja mora biti zaključana pri promeni uslova i pri pozivanju `Wait` metode.

Ali zašto nam je to potrebno? Jedan scenario može biti kada jedan proces prima podatke, a drugi procesi moraju da čekaju da ovaj proces primi podatke pre nego što mogu da pročitaju ispravne podatke.

Ako jednostavno koristimo kanal ili mutex, samo jedan proces može da čeka i čita podatke. Ne postoji način da se obaveste drugi procesi da čitaju podatke. Stoga možemo sa `sync.Cond` da koordiniramo deljene resurse.

##### Upotreba

sync.Cond dolazi sa sledećim metodama:

`NewCond(l Locker)` vraća novi Uslov.
`Broadcast()` budi sve gorutine koje čekaju na uslov.
`Signal()` budi jednu gorutinu čekajući uslov ako ga ima.
`Wait()`atomski otključava osnovni mutex zaključavanja.

##### Primer

Evo primera koji demonstrira interakciju različitih gorutina koristeći `Cond`.
```
package main

import (
	"fmt"
	"sync"
	"time"
)

var done = false

func read(name string, c *sync.Cond) {
	c.L.Lock()
	for !done {
		c.Wait()
	}
	fmt.Println(name, "starts reading")
	c.L.Unlock()
}

func write(name string, c *sync.Cond) {
	fmt.Println(name, "starts writing")
	time.Sleep(time.Second)

	c.L.Lock()
	done = true
	c.L.Unlock()

	fmt.Println(name, "wakes all")
	c.Broadcast()
}

func main() {
	var m sync.Mutex
	cond := sync.NewCond(&m)

	go read("Reader 1", cond)
	go read("Reader 2", cond)
	go read("Reader 3", cond)
	write("Writer", cond)

	time.Sleep(4 * time.Second)
}
```
	$ go run main.go
	Writer starts writing
	Writer wakes all
	Reader 2 starts reading
	Reader 3 starts reading
	Reader 1 starts reading

Kao što vidimo, čitaoci su bili suspendovani korišćenjem `Wait` metode sve dok pisac nije koristio `Broadcast` metodu da probudi proces.

### Once

Onse osigurava da će se izvršiti samo jedno izvršenje čak i među nekoliko gorutina.

##### Upotreba

Za razliku od drugih primitiva, `sync.Once` ima samo jednu metodu:

`Do(f func())` poziva funkciju `f` samo jednom. Ako se `Do` poziva više puta, samo prvi poziv će pozvati funkciju `f`.

##### Primer

Ovo deluje prilično jednostavno, uzmimo primer:
```
package main

import (
	"fmt"
	"sync"
)

func main() {
	var count int

	increment := func() {
		count++
	}

	var once sync.Once

	var increments sync.WaitGroup
	increments.Add(100)

	for i := 0; i < 100; i++ {
		go func() {
			defer increments.Done()
			once.Do(increment)
		}()
	}

	increments.Wait()
	fmt.Printf("Count is %d\n", count)
}
```
	$ go run main.go
	Count is 1

Kao što vidimo, čak i kada smo pokrenuli 100 gorutina, broj se povećao samo jednom.

### Pool

`Pool` je skalabilni `pool` privremenih objekata i takođe je bezbedan za konkurentnost. Bilo koja sačuvana vrednost u `pool`-u može se izbrisati u bilo kom trenutku bez prijema obaveštenja. Pored toga, pod velikim opterećenjem, `pool` objekata se može dinamički proširiti, a kada se ne koristi ili konkurentnost nije visoka, `pool` objekata će se smanjiti.

Ključna ideja je ponovna upotreba objekata kako bi se izbeglo ponovno stvaranje i uništavanje, što će uticati na performanse.

Ali zašto nam je to potrebno? Svrha pool-a je da kešira dodeljene, ali nekorišćene stavke za kasniju ponovnu upotrebu, smanjujući pritisak na sakupljač smeća. To jest, olakšava kreiranje efikasnih, nitno bezbednih lista slobodnih stavki. Međutim, nije pogodan za sve liste slobodnih stavki.

Odgovarajuća upotreba `pool-a` je upravljanje grupom privremenih stavki koje se tiho dele između i potencijalno ponovo koriste od strane konkurentno nezavisnih klijenata paketa. Bazen pruža način da se troškovi alokacije raspodele na više klijenata.

Važno je napomenuti da `pool` takođe ima svoju cenu u pogledu performansi. Mnogo je sporiji za korišćenje `sync.Pooload` jednostavne inicijalizacije. Takođe, `pool` se ne sme kopirati nakon prve upotrebe.

##### Upotreba

`sync.Pool` nam daje sledeće metode:

- `Get()` bira proizvoljnu stavku iz bazena, uklanja je iz bazena i vraća je pozivaocu.
- `Put(x any)` dodaje stavku u pul.

##### Primer

Sada, pogledajmo jedan primer.

Prvo, kreiraćemo novi `sync.Pool`, gde opciono možemo da navedemo funkciju koja će generisati vrednost kada pozovemo `Get` u suprotnom će vratiti `nil` vrednost.
```
package main
import (
	"fmt"
	"sync"
)

type Person struct {
	Name string
}
var pool = sync.Pool{
	New: func() any {
		fmt.Println("Creating a new person...")
		return &Person{}
	},
}

func main() {
	person := pool.Get().(*Person)
	fmt.Println("Get object from sync.Pool for the first time:", person)

	fmt.Println("Put the object back in the pool")
	pool.Put(person)

	person.Name = "Gopher"
	fmt.Println("Set object property name:", person.Name)

	fmt.Println("Get object from pool again (it's updated):", pool.Get().(*Person))
	fmt.Println("There is no object in the pool now (new one will be created):", pool.Get().(*Person))
}
```
I ako ovo pokrenemo, videćemo zanimljiv izlaz:

	$ go run main.go
	Creating a new person...
	Get object from sync.Pool for the first time: &{}
	Put the object back in the pool
	Set object property name: Gopher
	Get object from pool again (it's updated): &{Gopher}
	Creating a new person...
	There is no object in the pool now (new one will be created): &{}

**Napomena**: Obratite pažnju kako smo uradili tvrdnju tipa kada smo pozvali Get.

Može se videti da je `sync.Pool` isključivo privremeni objektni pul, koji je pogodan za čuvanje nekih privremenih objekata koji će se deliti između gorutina.

### Mapa

Mapa je kao standardna, `map[any]any` ali je bezbedna za istovremenu upotrebu od strane više gorutina bez dodatnog zaključavanja ili koordinacije. Učitavanja, čuvanja i brisanja su raspoređena tokom konstantnog vremena.

Ali zašto nam je to potrebno? Tip Map je specijalizovan. Većina koda bi trebalo da koristi običnu Go mapu umesto toga, sa odvojenim zaključavanjem ili koordinacijom, radi bolje bezbednosti tipa i kako bi se olakšalo održavanje drugih invarijanti zajedno sa sadržajem mape.

Tip mape je optimizovan za dva uobičajena slučaja upotrebe:

- Kada se unos za dati ključ upisuje samo jednom, ali čita više puta, kao u keš memorijama koje   
  samo rastu.
- Kada više gorutina čita, piše i prepisuje zapise za disjunktne skupove ključeva. U ova dva  
  slučaja, upotreba sync.Mapmože značajno smanjiti konkurenciju za zaključavanje u poređenju sa Go mapom uparenom sa odvojenim `Mutex` ili `RWMutex`.

Nulta mapa je prazna i spremna za upotrebu. Mapa se ne sme kopirati nakon prve upotrebe.

##### Upotreba

sync.Map daje nam sledeće metode:

- Delete()briše vrednost za ključ.
- Load(key any)vraća vrednost sačuvanu u mapi za ključ, ili nil ako vrednost nije prisutna.
- LoadAndDelete(key any)briše vrednost za ključ, vraćajući prethodnu vrednost ako postoji. Učitani 
  rezultat izveštava da li je ključ bio prisutan.
- LoadOrStore(key, value any)vraća postojeću vrednost za ključ ako je prisutan. U suprotnom, čuva  
  i vraća datu vrednost. Učitani rezultat je tačno ako je vrednost učitana, a netačno ako je sačuvana.
- Store(key, value any)podešava vrednost za ključ.
- Range(f func(key, value any) bool)poziva fsekvencijalno za svaki ključ i vrednost prisutne u 
  mapi. Ako fvrati vrednost "false", opseg zaustavlja iteraciju.

**Napomena**: `Range` ne mora nužno odgovarati bilo kom konzistentnom snimku sadržaja mape.

##### Primer

Ovde ćemo pokrenuti nekoliko gorutina koje će istovremeno dodavati i preuzimati vrednosti sa naše mape.
```
package main
import (
	"fmt"
	"sync"
)

func main() {
	var wg sync.WaitGroup
	var m sync.Map

	wg.Add(10)
	for i := 0; i <= 4; i++ {
		go func(k int) {
			v := fmt.Sprintf("value %v", k)

			fmt.Println("Writing:", v)
			m.Store(k, v)
			wg.Done()
		}(i)
	}

	for i := 0; i <= 4; i++ {
		go func(k int) {
			v, _ := m.Load(k)
			fmt.Println("Reading: ", v)
			wg.Done()
		}(i)
	}

	wg.Wait()
}
```
Kao što se i očekivalo, naša operacija skladištenja i preuzimanja biće bezbedna za istovremenu upotrebu.

	$ go run main.go
	Reading: <nil>
	Writing: value 0
	Writing: value 1
	Writing: value 2
	Writing: value 3
	Writing: value 4
	Reading: value 0
	Reading: value 1
	Reading: value 2
	Reading: value 3

### Atomic

Paket `atomic` pruža niskonivoske atomske memorijske primitive za cele brojeve i pointere koji su korisni za implementaciju algoritama sinhronizacije.

##### Upotreba

Paket `atomic` pruža nekoliko funkcija koje obavljaju sledećih 5 operacija za tipove `int`, `uint` i `uintptr`:

    Add
    Load
    Store
    Swap
    Compare and swap

##### Primer

Nećemo moći da pokrijemo sve funkcije ovde. Zato, hajde da pogledamo najčešće korišćenu funkciju `AddInt32` da bismo stekli predstavu.
```
package main
import (
  "fmt"
	"sync"
	"sync/atomic"
)
func add(w *sync.WaitGroup, num *int32) {
	defer w.Done()
	atomic.AddInt32(num, 1)
}
func main() {
	var n int32 = 0
	var wg sync.WaitGroup

	wg.Add(1000)
	for i := 0; i < 1000; i = i + 1 {
		go add(&wg, &n)
	}

	wg.Wait()

	fmt.Println("Result:", n)
}
```
Ovde `atomic.AddInt32` garantuje da će rezultat n biti 1000 jer se izvršavanje instrukcija atomskih operacija ne može prekinuti.

	$ go run main.go
	Result: 1000

[[Select]](24_Select.md) [[Sadržaj]](toc.md) [[Napredni obrasci konkurentnosti]](26_Napredni_obrasci_konkurentnosti.md)

