[[Mape]](15_Mape.md) [[Sadržaj]](toc.md) [[Greške]](17_Greške.md)

# Interfejsi

Interfejs u ​​Gou je apstraktni tip koji je definisan pomoću skupa potpisa metoda. Interfejs definiše ponašanje za slične tipove objekata. Ovde je ponašanje ključni pojam o kome ćemo uskoro razgovarati.

Jedan od najboljih primera interfejsa iz stvarnog sveta je utičnica. Zamislite da treba da povežemo različite uređaje na utičnicu (u zidu).

### Bez interfejsa

Hajde da pokušamo da ovo implementiramo. Evo tipova uređaja koje ćemo koristiti.
```
type mobile struct {
	brand string
}
type laptop struct {
	cpu string
}
type toaster struct {
	amount int
}
type kettle struct {
	quantity string
}
type socket struct{}
```
Sada, hajde da definišemo *Draw* metodu na tipu, recimo `mobile`. Ovde ćemo jednostavno ispisati svojstva tipa.
```
func (m mobile) Draw(power int) {
	fmt.Printf("%T -> brand: %s, power: %d", m, m.brand, power)
}
```
Odlično, sada ćemo definisati *Plug* metodu na `socket` tipu koja prihvata naš `mobile` tip kao argument.
```
func (socket) Plug(device mobile, power int) {
	device.Draw(power)
}
```
Hajde da pokušamo da "povežemo" ili "priključimo" tip `mobile` našem `socket` tipu u *main* funkciji.
```
package main

import "fmt"

func main() {
	m := mobile{"Apple"}

	s := socket{}
	s.Plug(m, 10)
}
```
I ako ovo pokrenemo, videćemo sledeće.

	$ go run main.go
	main.mobile -> brand: Apple, power: 10

Ovo je zanimljivo, ali recimo da sada želimo da povežemo naš `laptop` tip.
```
package main
import "fmt"

func main() {
	m := mobile{"Apple"}
	l := laptop{"Intel i9"}
	s := socket{}
	s.Plug(m, 10)
	s.Plug(l, 50) // Error: cannot use l as mobile value in argument
}
```
Kao što vidimo, ovo će izbaciti grešku.

Šta bi trebalo sada da uradimo? Definišemo drugi metod? Kao na primer PlugLaptop? Naravno, ali onda svaki put kada dodamo novi tip uređaja, moraćemo da dodamo i novu metodu tipu `socket`, a to nije idealno.

Tu `interface` dolazi do izražaja. U suštini, želimo da definišemo ugovor koji se u budućnosti mora sprovesti.

Možemo jednostavno definisati interfejs kao što je `PowerDrawer` i koristiti ga u našoj *Plug* funkciji da bismo dozvolili bilo koji uređaj koji zadovoljava kriterijume, a to je da tip mora imati Drawmetod koji odgovara potpisu koji interfejs zahteva.

I u svakom slučaju, `socket` ne mora ništa da zna o našem uređaju i može jednostavno da pozove *Draw* metodu.

### Sa interfejsom

Sada hajde da pokušamo da implementiramo naš `PowerDrawer` interfejs. Evo kako će izgledati.

Konvencija je da se u imenu koristi sufiks `"-er"` . I kao što smo ranije pomenuli, interfejs treba samo da opisuje očekivano ponašanje. Što je u našem slučaju *Draw* metoda.

##### Implementacija interfejsa
```
type PowerDrawer interface {
	Draw(power int)
}
```
Sada, moramo ažurirati našu *Plug* metodu da prihvati uređaj koji implementira `PowerDrawer` interfejs kao argument.
```
func (socket) Plug(device PowerDrawer, power int) {
	device.Draw(power)
}
```
A da bismo zadovoljili interfejs, možemo jednostavno dodati Draw metode svim tipovima uređaja.
```
type mobile struct {
	brand string
}
func (m mobile) Draw(power int) {
	fmt.Printf("%T -> brand: %s, power: %d\n", m, m.brand, power)
}
type laptop struct {
	cpu string
}
func (l laptop) Draw(power int) {
	fmt.Printf("%T -> cpu: %s, power: %d\n", l, l.cpu, power)
}
type toaster struct {
	amount int
}
func (t toaster) Draw(power int) {
	fmt.Printf("%T -> amount: %d, power: %d\n", t, t.amount, power)
}
type kettle struct {
	quantity string
}
func (k kettle) Draw(power int) {
	fmt.Printf("%T -> quantity: %s, power: %d\n", k, k.quantity, power)
}
```
Sada možemo povezati sve naše uređaje sa utičnicom pomoću interfejsa!
```
func main() {
	m := mobile{"Apple"}
	l := laptop{"Intel i9"}
	t := toaster{4}
	k := kettle{"50%"}
	s := socket{}
	s.Plug(m, 10)
	s.Plug(l, 50)
	s.Plug(t, 30)
	s.Plug(k, 25)
}
```
I funkcioniše baš onako kako smo očekivali.

	$ go run main.go
	main.mobile -> brand: Apple, power: 10
	main.laptop -> cpu: Intel i9, power: 50
	main.toaster -> amount: 4, power: 30
	main.kettle -> quantity: Half Empty, power: 25

Zašto se interfejsi smatraju tako moćnim konceptom? Pa, interfejs nam može pomoći da razdvojimo naše tipove. Na primer, pošto imamo interfejs, ne moramo da ažuriramo našu `socket` implementaciju. Možemo jednostavno definisati novi tip uređaja pomoću *Draw* metode.

Za razliku od drugih jezika, Go interfejsi su implicitno implementirani, tako da nam nije potrebno nešto poput `implements` ključne reči. To znači da tip automatski zadovoljava interfejs kada ima "sve metode" interfejsa.

### Prazan interfejs

Prazan interfejs može da poprimi vrednost bilo kog tipa.

Evo kako to deklarišemo.

	var x interface{}

Zašto nam je potreban prazan interfejs? Prazni interfejsi se mogu koristiti za rukovanje vrednostima nepoznatih tipova. Neki primeri su:

- Čitanje heterogenih podataka iz API-ja.
- Promenljive nepoznatog tipa, kao u `fmt.Println` funkciji.

Da bismo koristili vrednost tipa interface{}, možemo koristiti tvrdnju tipa ili prekidač tipa da bismo odredili tip vrednosti.

### Tvrdnja tipa

Tvrdnja tipa pruža pristup osnovnoj konkretnoj vrednosti vrednosti interfejsa.

Na primer:
```
func main() {
	var i interface{} = "hello"
	
	s := i.(string)		// <<=== tvrdnja tipa
	fmt.Println(s)
}
```
Ova izjava tvrdi da vrednost interfejsa sadrži konkretan tip i dodeljuje osnovnu vrednost tipa promenljivoj.

Takođe možemo testirati da li vrednost interfejsa sadrži određeni tip.

Tvrdnja tipa može vratiti dve vrednosti:

- Prva je osnovna vrednost.
- Druga je bulova vrednost koja izveštava da li je tvrdnja uspešna.
```
s, ok := i.(string) 	// << ==== tvrdnja tipa u obliku zarez, ok 
fmt.Println(s, ok)
```
Na neki način, ovo je slično načinu na koji čitamo vrednosti sa mape. Ako je `ok` jednako `false` i vrednost će biti nulta vrednost tipa, i neće doći do panike.
```
f, ok := i.(float64)
fmt.Println(f, ok)
```
Ali ako interfejs ne sadrži taj tip, tvrdnja tipa će izazvati paniku.
```
f = i.(float64)
fmt.Println(f) // Panic!
```
	$ go run main.go
	hello
	hello true
	0 false
	panic: interface conversion: interface {} is string, not float64

### Prekidač tipa

`switch` se može koristiti kao iskaz za određivanje tipa promenljive tipa interface{}.
```
var t interface{}
t = "hello"

switch t := t.(type) {
case string:
	fmt.Printf("string: %s\n", t)
case bool:
	fmt.Printf("boolean: %v\n", t)
case int:
	fmt.Printf("integer: %d\n", t)
default:
	fmt.Printf("unexpected: %T\n", t)
}
```
I ako ovo pokrenemo, možemo proveriti da imamo string tip.

	$ go run main.go
	string: hello

### Svojstva interfejsa

##### Nulta vrednost

Nulta vrednost interfejsa je `nil`.
```
package main
import "fmt"

type MyInterface interface {
	Method()
}
func main() {
	var i MyInterface

	fmt.Println(i) // Output: <nil>
}
```

##### Ugrađivanje

Možemo ugrađivati interfejse poput struktura. Na primer:
```
type interface1 interface {
    Method1()
}
type interface2 interface {
    Method2()
}
type interface3 interface {
    interface1
    interface2
}
```
##### Poređenje interfejsa

Vrednosti interfejsa su uporedive.
```
package main
import "fmt"

type MyInterface interface {
	Method()
}
type MyType struct{}
func (MyType) Method() {}

func main() {
	t := MyType{}
	var i MyInterface = MyType{}
	fmt.Println(t == i)
}
```
##### Vrednosti interfejsa

U suštini, vrednost interfejsa se može smatrati torkom koja se sastoji od vrednosti i konkretnog tipa.
```
package main
import "fmt"

type MyInterface interface {
	Method()
}
type MyType struct {
	property int
}

func (MyType) Method() {}

func main() {
	var i MyInterface
	i = MyType{10}
	fmt.Printf("(%v, %T)\n", i, i) 	// Output: ({10}, main.MyType)
}
```
Interfejsi su zaista moćna funkcija, ali zapamtite, "Što je interfejs veći, to je apstrakcija slabija" - Rob Pajk.

[[Mape]](15_Mape.md) [[Sadržaj]](toc.md) [[Greške]](17_Greške.md)