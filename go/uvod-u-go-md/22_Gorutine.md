[[Konkurentnost]](21_Konkurentnost.md) [[Sadržaj]](toc.md) [[Kanali]](23_Kanali.md)

# Gorutine

Pre nego što započnemo našu diskusiju, želeo bih da podelim jednu važnu poslovicu o Gou: *"Ne komunicirajte deljenjem memorije, delite memoriju komunikacijom."* - Rob Pajk

`Gorutina` je lagana nit izvršavanja kojom upravlja Go runtime i u suštini nam omogućava da pišemo asinhroni kod na sinhroni način.

Važno je znati da `gorutine` nisu stvarne OS niti i da se sama `main` funkcija izvršava kao gorutina.

Jedna nit OS-a može da pokreće hiljade gorutina koristeći Go raspoređivač vremena izvršavanja koji koristi kooperativno zakazivanje. To podrazumeva da ako je trenutna gorutina blokirana ili je završena, raspoređivač će premestiti ostale gorutine u drugu nit operativnog sistema. Stoga postižemo efikasnost u zakazivanju gde nijedna rutina nije blokirana zauvek.

Možemo pretvoriti bilo koju funkciju u gorutinu jednostavnim korišćenjem `go` ključne reči.
```
go fn(x, y, z)
```
Pre nego što napišemo bilo koji kod, važno je ukratko razmotriti model `fork-join`.

### Model račvanja-spajanja

Go koristi ideju `fork-join` modela konkurentnosti koji stoji iza `gorutina`. Model `fork-join` u suštini podrazumeva da se podređeni proces odvaja od svog roditeljskog procesa da bi se pokrenuo konkurentno sa roditeljskim procesom. 

Tačka razdvajanja se zove `fork point`. Nakon završetka izvršavanja, podređeni proces se ponovo spaja sa roditeljskim procesom. Tačka gde se ponovo spaja naziva se `join point`.

Sada, hajde da napišemo malo koda i kreiramo sopstvenu gorutinu.
```
package main
import "fmt"

func speak(arg string) {
	fmt.Println(arg)
}
func main() {
	go speak("Hello World")
}
```
Ovde poziv *speak* funkcije ima prefiks ključne reči `go`. Ovo će joj omogućiti da se pokreće kao zasebna gorutina. I to je to, upravo smo kreirali našu prvu gorutinu. Toliko je jednostavno!

Odlično, hajde da pokrenemo ovo:

	$ go run main.go

Zanimljivo je da izgleda da se naš program nije potpuno pokrenuo jer mu nedostaje deo izlaza. To je zato što je naša glavna gorutina izašla i nije čekala gorutinu koju smo kreirali.

Šta ako nateramo naš program da čeka koristeći `time.Sleep` funkciju?
```
func main() {
	...
	time.Sleep(1 * time.Second)
}
```
	$ go run main.go
	Hello World

Eto, sada možemo videti naš kompletan rezultat.

U redu, ovo funkcioniše, ali nije idealno. Pa kako da ovo poboljšamo?

Najzahtevniji deo korišćenja gorutina je znati kada će se zaustaviti. Važno je razumeti da se gorutine izvršavaju u istom adresnom prostoru, tako da pristup deljenoj memoriji mora biti sinhronizovan.

[[Konkurentnost]](21_Konkurentnost.md) [[Sadržaj]](toc.md) [[Kanali]](23_Kanali.md)
