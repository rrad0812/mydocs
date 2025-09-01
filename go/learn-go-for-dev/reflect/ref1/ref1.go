/*
Paket 'reflect' u Go-u
======================

Paket 'reflect' u Go-u je moćan alat za ispitivanje i manipulaciju ponašanja Go
programa tokom izvršavanja. Omogućava programerima da ispitaju tip, vrednost i
druge karakteristike promenljivih i izraza tokom izvršavanja, kao i da
modifikuju vrednost promenljivih i pozivaju funkcije koristeći refleksiju.

Paket 'reflect' je deo standardne biblioteke i uvozi se sledećom izjavom:

	import "reflect"

Vrednost i tip
--------------
U srži paketa reflect nalaze se tipovi Value i Type. Value predstavlja vrednost,
a Type predstavlja tip u Go-u. Ovi tipovi se koriste za predstavljanje ponašanja
promenljivih i izraza tokom izvršavanja i pružaju način za njihovu manipulaciju
pomoću refleksije.

Da biste dobili vrednost "Value" za promenljivu ili izraz, možete koristiti
funkciju "reflect.ValueOf()". Ova funkcija uzima vrednost "interface{}" kao
argument i vraća vrednost "Value" koja predstavlja vrednost interfejsa.
Na primer:
*/
package main

import (
	"fmt"
	"reflect"
)

func refValueType() {

	fmt.Println("\n --- Value and Type types ---")

	x := 123
	val := reflect.ValueOf(x)
	typ := reflect.ValueOf(x)

	fmt.Printf("%v --- %T\n", val, val)
	fmt.Printf("%v --- %T\n", typ, typ)
}

/*
Vrsta vrednosti ili tipa predstavlja osnovnu vrstu vrednosti ili tipa. Može biti
jedna od sledećih konstanti:

    Bool: bulova vrednost
    Int, Int8, Int16, Int32, Int64: celobrojna vrednost
    Uint, Uint8, Uint16, Uint32, Uint64, Uintptr: neoznačena celobrojna vrednost
    Float32, Float64: vrednost sa pokretnim zarezom
    Complexs64, Complexs128: kompleksan broj
    Array: niz
    Chan: kanal
    Function: funkcija
    Interface: interfejs
    Mapa: mapa
    Ptr: pokazivač
    Slice: isečak
    Strng: niz
    Structure: struktura
    UnsafePointer: nebezbedan pokazivač

	Ispred ide Reflect. na primer: Reflect.Int!!!

Vrstu vrednosti ili tipa možete dobiti korišćenjem metode Kind(). Na primer:
*/

func refKind() {
	fmt.Println("\n --- Kind type ---")

	x := 123
	val := reflect.ValueOf(x)
	typ := reflect.TypeOf(x)

	kind := val.Kind()
	fmt.Printf("%v --- %T\n", kind, kind)

	kind2 := typ.Kind()
	fmt.Printf("%v --- %T\n", kind2, kind2)
}

/*
Manipulisanje vrednostima
-------------------------
Kada imate vrednost "Value" koja predstavlja vrednost, možete koristiti
različite metode da je manipulišete. Na primer, možete koristiti metodu "Set()"
da podesite vrednost te vrednosti:

	val.Set(reflect.ValueOf(456))

Takođe možete koristiti metodu "Elem()" da biste dobili vrednost "Value" koja
predstavlja vrednost na koju pokazuje pointer:

	ptr := reflect.ValueOf(&x)
	elem := ptr.Elem()
	elem.Set(reflect.ValueOf(789))

Primer:
*/
func refSetValue() {

	fmt.Println("\n --- Set value ---")

	x := 123
	val := reflect.ValueOf(x)
	fmt.Printf("val: %v --- %T\n", val, val)

	valNew := reflect.ValueOf(&x).Elem()
	fmt.Printf("valNew before Set(): %v --- %T\n", valNew, valNew)

	valNew.Set(reflect.ValueOf(456))
	fmt.Printf("valNew after Set() %v --- %T\n", valNew, valNew)
	fmt.Printf("x after set: %v\n", x)
}

/*
Napomena o reflect.ValueOf, reflect.TypeOf, Set() i Elem():
    - Parametari prve dve funkcije su interface{}, što znači pointeri
    - Vraćena vrednost reflect.ValueOf i reflect.TypeOf su Type and Value koji
      su pointeri.
    - Da bi promenili vrednost u interfejsu moramo proslediti pointer na
      osnovnu vrednost da bi dobili odgovarajući Value
    - "Elem" vraća Value ali ovaj put imajući u vidu da je ulaz pointer na
      interface{}.
    - Sada možemo da izmenimo osnovnu vrednost interface{}, koristeći funkciju
      Set(), ali parametr joj je Value osnovne nove vrednosti.

Pozivanje funkcija
------------------
Paket "reflect" vam takođe omogućava pozivanje funkcija koristeći refleksiju.
Da biste pozvali funkciju koristeći refleksiju, prvo morate da dobijete
vrednost "Value" koja predstavlja funkciju koristeći funkciju "ValueOf()".
Zatim, možete koristiti "Call()" metodu na vrednosti "Value" da biste pozvali
funkciju. Metod "Call()"" uzima deo vrednosti "Value" kao argumente, koji
predstavljaju argumente funkcije, i vraća deo vrednosti "Values" koji p
redstavljaju povratne vrednosti funkcije.

Na primer, razmotrite sledeću funkciju:
*/

func add(x int, y int) int {
	return x + y
}

/*
Da biste pozvali ovu funkciju koristeći refleksiju, možete uraditi sledeće:
*/

func refFunctionCall() {

	fmt.Println("\n --- Function Call ---")

	funcVal := reflect.ValueOf(add)
	args := []reflect.Value{reflect.ValueOf(8), reflect.ValueOf(12)}
	result := funcVal.Call(args)

	fmt.Println(result[0]) // should be 20
}

/*
Rezultatna promenljiva će biti deo vrednosti (Values) koji sadrži jednu vrednost
"Value", koja predstavlja povratnu vrednost funkcije add().

Tvrdnje tipa
------------
Paket "reflect" takođe pruža način za izvršavanje tvrdnji tipa na vrednostima
korišćenjem metode "TypeAssert()"" na vrednosti "Value". Ova metoda uzima
vrednost interface{} kao argument i pokušava da dodeli vrednost te vrednosti
interface{}. Ako su tipovi kompatibilni, vraća vrednost "Value" koja predstavlja
vrednost interface{} i bulovu vrednost koja ukazuje na uspeh. Ako tipovi nisu
kompatibilni, vraća vrednost "Value" nula i vrednost "false".

Na primer:
*/

func refTypeAssert() {

	fmt.Println("\n --- Type assert na reflect.Value ---")

	val := reflect.ValueOf("Hello")
	str, ok := val.Interface().(string)
	if ok {
		fmt.Println(str)
	}
}

/*
Ovaj kod će ispisati "Hello" u konzoli.

Iteriranje kroz strukturna polja
--------------------------------
Paket 'reflect' takođe pruža način za iteraciju kroz polja strukture koristeći
metode "Type()"" i "NumField()"" na vrednosti "Value". Metod "Type()" vraća
tip koji predstavlja tip vrednosti, a metod NumField() vraća broj polja u
strukturi. Zatim možete koristiti metod Field() da biste dobili vrednost Value
koja predstavlja određeno polje u strukturi.

Na primer:
*/

type User struct {
	Name string
	Age  int
}

func refIterateStructFields() {

	fmt.Println("\n --- Iterate struct fields ---")

	user := User{Name: "Alice", Age: 30}
	val := reflect.ValueOf(user)

	for i := 0; i < val.NumField(); i++ {
		field := val.Field(i)
		fmt.Printf("%s: %v\n", val.Type().Field(i).Name, field.Interface())
	}
}

/*
Ovaj kod će ispisati sledeće u konzolu:

	>> Name: Alice
	>> Age: 30

Jedna napredna upotreba paketa reflect u Go-u je mogućnost dinamičkog pozivanja
metoda na bilo kom objektu. Ovo može biti korisno u situacijama kada želite da
pozovete metodu na objektu, ali ime metode nije poznato do izvršavanja programa.

Da biste dinamički pozvali metodu na objektu koristeći refleksiju, prvo morate
da dobijete vrednost "Value" koja predstavlja objekat. Zatim, možete koristiti
metod "MethodByName()"" na vrednosti "Value" da biste dobili vrednost "Value"
koja predstavlja metodu. Na kraju, možete koristiti metod "Call()"" na metodi
"Value" da biste pozvali metodu.

Evo primera kako dinamički pozvati metodu na objektu koristeći refleksiju:
*/

type MyStruct struct {
}

func (m *MyStruct) Hello(name string) string {
	return "Hello, " + name
}

func refDynamicCall() {

	fmt.Println("\n --- Dynamic Call ---")

	obj := MyStruct{}
	objVal := reflect.ValueOf(&obj)
	methodVal := objVal.MethodByName("Hello")
	args := []reflect.Value{reflect.ValueOf("Alice")}
	result := methodVal.Call(args)
	fmt.Println(result[0])
}

/*
U ovom primeru, metoda Hello() se poziva na objektu MyStruct koristeći
refleksiju. Metoda uzima jedan argument tipa string i vraća string. Metoda
MethodByName() se koristi za dobijanje vrednosti (Value) koja predstavlja
metodu Hello(), a metoda Call() se koristi za pozivanje metode sa argumentom
"Alice". Vraćeni isečak Value sadrži jednu vrednost (Value) koja predstavlja
povratnu vrednost metode, koja se zatim ispisuje na konzolu.

Ovaj pristup može biti koristan kada želite da pozovete metodu na objektu,
ali ime metode nije poznato do izvršavanja programa. Omogućava vam da dinamički
pozovete bilo koju metodu na objektu koristeći refleksiju, sve dok znate ime
metode i argumente koje očekuje.

Imajte na umu da je ovaj pristup manje efikasan od direktnog pozivanja metode,
jer zahteva korišćenje refleksije. Stoga ga treba koristiti štedljivo i samo
kada je to neophodno. Generalno, bolje je pozivati metode direktno kad god je
to moguće kako bi se izbeglo dodatno trošenje resursa korišćenjem refleksije.
*/

func main() {
	fmt.Println("\n --- Reflect package ---")

	refValueType()
	refKind()
	refSetValue()
	refFunctionCall()
	refTypeAssert()
	refIterateStructFields()
	refDynamicCall()
}
