[[Tipovi podataka]](03_Tipovi_podataka.md) [[Sadržaj]](toc.md) [[Funkcije]](05_Funkcije.md)

# Kontrola toka

### if/else

Ovo funkcioniše manje-više isto kao što očekujete, ali if izraz ne mora biti okružen zagradama ().
```
func main() {
	x := 10

	if x > 5 {
		fmt.Println("x is gt 5")
	} else if x > 10 {
		fmt.Println("x is gt 10")
	} else {
		fmt.Println("else case")
	}
}
```
    $ go run main.go
    x is gt 5

##### Kompaktno if

Takođe možemo sažeti naše if izjave.
```
func main() {
	if x := 10; x > 5 {
		fmt.Println("x is gt 5")
	}
}
```
**Napomena**: Ovaj obrazac je prilično čest.

### Switch

Zatim, imamo switch izjavu, što je često kraći način za pisanje uslovne logike.

U programskom jeziku Go, izjava `switch` pokreće samo prvi slučaj čija je vrednost jednaka uslovnom izrazu, a ne sve slučajeve koji slede. Stoga, za razliku od drugih jezika, `break` izjava se automatski dodaje na kraj svakog slučaja.

To znači da switch procenjuje slučajeve od vrha do dna, zaustavljajući se kada je slučaj uspešan. 

Pogledajmo primer:
```
func main() {
	day := "monday"
	switch day {
	case "monday":
		fmt.Println("time to work!")
	case "friday":
		fmt.Println("let's party")
	default:
		fmt.Println("browse memes")
	}
}
```
    $ go run main.go
    time to work!

Switch takođe podržava skraćene deklaracije poput ove:
```
	switch day := "monday"; day {
	case "monday":
		fmt.Println("time to work!")
	case "friday":
		fmt.Println("let's party")
	default:
		fmt.Println("browse memes")
	}
```
Takođe možemo koristiti `fallthrough` ključnu reč da prenesemo kontrolu na sledeći slučaj čak i ako se trenutni slučaj možda nije podudarao.
```
	switch day := "monday"; day {
	case "monday":
		fmt.Println("time to work!")
		fallthrough
	case "friday":
		fmt.Println("let's party")
	default:
		fmt.Println("browse memes")
	}
```
I ako ovo pokrenemo, videćemo da nakon prvog podudaranja slučaja, izjava `switch` nastavlja na sledeći slučaj zbog `fallthrough` ključne reči.

    $ go run main.go
    time to work!
    let's party

Takođe `switch` možemo koristiti bez ikakvog uslova, što je isto kao i `switch true`.
```
x := 10
switch {
	case x > 5:
		fmt.Println("x is greater")
	default:
		fmt.Println("x is not greater")
}
```
### Petlje

U Gou imamo samo jednu vrstu petlje, a to je `for` petlja.  Baš kao i `if` izjava, `for` petlja, ne zahteva nikakve zagrade () za razliku od drugih jezika.

##### Definicija for petlje

Počnimo sa osnovnim `for` ciklusom.
```
func main() {
	for i := 0; i < 10; i++ {
		fmt.Println(i)
	}
}
```
Osnovna for petlja ima tri komponente odvojene tačka-zarezom:

- `init` izjava       : koja se izvršava pre prve iteracije.
- `conditions` izraz   : koji se izračunava pre svake iteracije.
- `post` izjava       : koja se izvršava na kraju svake iteracije.

##### Break i continue

Go takođe podržava i `break` i `continue` izjave za kontrolu petlje. Hajde da pokušamo sa brzim primerom:
```
func main() {
	for i := 0; i < 10; i++ {
		if i < 2 {
			continue
		}
		fmt.Println(i)
		if i > 5 {
			break
		}
	}
	fmt.Println("We broke out!")
}
```
`continue` izjava se koristi kada želimo da preskočimo preostali deo petlje, a `break` izjava se koristi kada želimo da izađemo iz petlje.

Takođe, `init` i `post` izjave for petlje su opcione, tako da možemo da nateramo našu `for` petlju da se ponaša kao `while` petlja.
```
func main() {
	i := 0
	for ;i < 10; {
		i += 1
	}
}
```
**Napomena**: možemo ukloniti i dodatne tačke-zareze da bismo je učinili malo čistijim.

##### Beskonačna petlja

Ako izostavimo uslov petlje, ona se ponavlja zauvek, tako da se beskonačna petlja može kompaktno izraziti ovako:
```
func main() {
	for {
		// do stuff here
	}
}
```

[[Tipovi podataka]](03_Tipovi_podataka.md) [[Sadržaj]](toc.md) [[Funkcije]](05_Funkcije.md)