package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"time"
)

/*
Refleksija u Gou
================

Refleksija je moćan alat u Gou koji omogućava programima da ispituju i
manipulišu sopstvenom strukturom tokom izvršavanja. Razumevanjem tipova i
vrednosti dinamički, programeri mogu da kreiraju fleksibilan i prilagodljiv kod.

U ovom vodiču ćemo istražiti osnove refleksije, njene praktične primene i
najbolje prakse.

U jeziku Go, refleksiju olakšava  paket "reflect", koji pruža skup alata
za dinamičku manipulaciju tipovima i vrednostima. U svojoj suštini, refleksija
u jeziku Go se vrti oko dve ključne komponente: "reflect.Type" i "reflect.Value".

"reflect.Type" predstavlja tip vrednosti, nudeći metode za istraživanje njenih
svojstava, kao što su njena vrsta (npr. struktura, isečak, mapa), polja i metode.
S druge strane, "reflect.Value" predstavlja vrednost promenljive, omogućavajući
dinamički pristup i modifikaciju.

Ova mogućnost je posebno korisna u nekoliko scenarija, uključujući kreiranje
generičkog koda, prilagođene procese serijalizacije i deserijalizacije,
fleksibilan razvoj API-ja, dinamičku validaciju podataka i izgradnju raznovrsnih
uslužnih programa za testiranje.

Korišćenjem refleksije, Go programeri mogu da pišu fleksibilniji, ponovo
upotrebljiv i dinamičniji kod, koji se može prilagoditi različitim tipovima i
strukturama tokom izvršavanja.

Uvod u paket reflect
--------------------
Paket reflectu Go-u pruža osnovne alate potrebne za izvršavanje refleksije,
omogućavajući vam dinamički pregled i manipulaciju tipovima i vrednostima tokom
izvršavanja.

Uvoz reflect paketa
...................
Da biste koristili refleksiju u svojim Go programima, prvo morate da uvezete
reflect paket:

	import "reflect"

Ključni tipovi i funkcije u reflect paketu
------------------------------------------
Paket reflect uključuje nekoliko ključnih tipova i funkcija koje čine osnovu
refleksije u jeziku Go. Evo najvažnijih:

reflect.Type
------------
reflect.Type se koristi za predstavljanje tipa vrednosti. Pruža metode za
ispitivanje svojstava tipa, kao što su njegova vrsta, polja i metode. Možete
dobiti vrednost reflect.Type korišćenjem TypeOf funkcije:

	t := reflect.TypeOf(yourVariable)
	fmt.Println("Type:", t)
	fmt.Println("Kind:", t.Kind())

reflect.Value
.............
reflect.Value predstavlja vrednost promenljive, omogućavajući vam da joj
pristupite i dinamički je menjate. Možete je dobiti reflect.Value korišćenjem
ValueOf funkcije:

	v := reflect.ValueOf(yourVariable)
	fmt.Println("Value:", v)
	fmt.Println("Can Set:", v.CanSet())

Sa reflect.Value, možete čitati i pisati osnovnu vrednost, pod uslovom da je
podesiva.

reflect.Kind
............
reflect.Kind je nabrajanje koje predstavlja specifičnu vrstu tipa (npr. Int,
Float64, Struct, ...). Koristi se i sa reflect.Type i reflect.Value da bi se
odredila vrsta osnovnog tipa ili vrednosti:

	t := reflect.TypeOf(yourVariable)
	if t.Kind() == reflect.Struct {
		fmt.Println("It's a struct")
	}

Razumevanjem i korišćenjem ovih ključnih komponenti paketa reflect, možete
iskoristiti moć refleksije da biste kreirali dinamičnije i fleksibilnije Go
programe.

Rad sa tipovima
---------------
Refleksija u programskom jeziku Go vam omogućava da dobijete i pregledate
informacije o tipu tokom izvršavanja, omogućavajući dinamičku interakciju sa
različitim tipovima. Evo kako možete da radite sa tipovima koristeći "reflect"
paket:

Dobijanje informacija o tipu pomoću reflect.TypeOf
..................................................
Prvi korak u refleksivnom radu sa tipovima jeste dobijanje informacija o tipu
promenljive. To se radi pomoću reflect.TypeOf funkcije:
*/

func refTypeOF() {

	fmt.Println("\n --- TypeOf ---")

	var x int = 42
	t := reflect.TypeOf(x)
	fmt.Println("Type:", t)
}

/*
Ispitivanje vrste tipa pomoću Type.Kind()
.........................................
Kada imate reflect.Type, možete proveriti njegovu vrstu. Vrsta tipa predstavlja
njegovu specifičnu kategoriju, kao što su Int, Float, Struct, itd. Vrstu tipa
možete odrediti koristeći "Kind()" metodu:
*/
func refKind() {

	fmt.Println("\n --- Kind ---")

	var x float64 = 3.14
	t := reflect.TypeOf(x)
	fmt.Println("Kind:", t.Kind())
}

/*
Primeri rada sa osnovnim tipovima (int, string, itd.)
-----------------------------------------------------
Evo nekoliko primera dobijanja informacija o tipu i ispitivanja osnovnih tipova:
*/
func refBasicKindOfTypes() {

	fmt.Println("\n --- Basic Kind Of Types ---")

	var a int = 10
	var b string = "hello"
	var c bool = true

	fmt.Println("Type of a:", reflect.TypeOf(a))
	fmt.Println("Kind of a:", reflect.TypeOf(a).Kind())

	fmt.Println("Type of b:", reflect.TypeOf(b))
	fmt.Println("Kind of b:", reflect.TypeOf(b).Kind())

	fmt.Println("Type of c:", reflect.TypeOf(c))
	fmt.Println("Kind of c:", reflect.TypeOf(c).Kind())
}

/*
Rad sa strukturama: Pregled polja i metoda
------------------------------------------
Rad sa strukturama korišćenjem refleksije je moćan način za dinamičku
interakciju sa poljima i metodama tipova struktura. Evo kako možete pregledati
polja i metode strukture:

Inspekcija polja struktura:
...........................
*/

type Person struct {
	Name string
	Age  int
}

func refFieldIntrospection() {

	fmt.Println("\n --- Field introspection ---")

	p := Person{Name: "John", Age: 30}
	t := reflect.TypeOf(p)

	for i := 0; i < t.NumField(); i++ {
		field := t.Field(i)
		fmt.Printf("Field Name: %s, Field Type: %s\n", field.Name, field.Type)
	}
}

/*
Inspekcija metoda struktura:
............................
Ako vaša struktura ima metode, možete i njih da pregledate:
*/

type Person2 struct {
	Name string
	Age  int
}

func (p Person2) Greet() {
	fmt.Println("Hello, my name is", p.Name)
}

func refMethodIntrospection() {

	fmt.Println("\n --- Method introspection ---")

	p := Person2{Name: "John", Age: 30}
	t := reflect.TypeOf(p)

	for i := 0; i < t.NumMethod(); i++ {
		method := t.Method(i)
		fmt.Printf("Method Name: %s, Method Type: %s\n", method.Name, method.Type)
	}
}

/*
Razumevanjem kako se radi sa tipovima koristeći reflect.TypeOf i Type.Kind(),
i kako se ispituju polja i metode struktura, možete iskoristiti refleksiju za
izgradnju dinamičnijih i prilagodljivijih Go programa.

Rad sa vrednostima
------------------
Refleksija u Gou vam takođe omogućava dinamički rad sa vrednostima promenljivih
tokom izvršavanja. reflectPaket pruža reflect.Valuetip za ovu svrhu.

Dobijanje informacija o vrednosti korišćenjem reflect.ValueOf
.............................................................
Da biste reflektivno dobili vrednost promenljive, koristite reflect.ValueOf
funkciju:
*/

func refValueOf() {

	fmt.Println("\n --- ValueOf ---")

	var x int = 42
	v := reflect.ValueOf(x)
	fmt.Println("Value:", v) // Output: Value: 42
}

/*
# Korišćenje Value metoda za dinamičko dobijanje i postavljanje vrednosti
.........................................................................
Kada imate reflect.Value, možete koristiti njegove metode da dinamički dobijete
i podesite osnovnu vrednost. Evo nekoliko primera:

Dobijanje vrednosti:
....................
*/

func refGetValue() {

	fmt.Println("\n --- Get value ---")

	var a int = 10
	v := reflect.ValueOf(a)

	// Getting the value
	fmt.Println("Value:", v.Int())
}

/*
Podešavanje vrednosti:
----------------------
Da biste dinamički postavili vrednost, vrednost mora biti adresabilna (tj. mora
biti Pointer ili polje strukture). Evo kako možete podesiti vrednosti:
*/

func refSetValue() {

	fmt.Println("\n --- Set value ---")

	var a int = 10
	vp := reflect.ValueOf(&a).Elem() // Via pointer, next is Elem()

	// Setting the value
	vp.SetInt(20)
	fmt.Println("Updated Value:", a)
}

/*
Primeri rada sa pointerima, isečcima i mapama
---------------------------------------------

Pointeri:
.........
Refleksija može da obrađuje pointere, omogućavajući vam da manipulišete
vrednostima na koje ukazuju:
*/

func refPointers() {

	fmt.Println("\n --- Pointers ---")
	var x int = 100
	p := &x

	vp := reflect.ValueOf(p).Elem()
	fmt.Println("Original Value:", vp.Int()) // Output: Original Value: 100

	vp.SetInt(200)
	fmt.Println("Updated Value:", *p) // Output: Updated Value: 200
}

/*
Isečci:
.......
Možete koristiti refleksiju za pregled i modifikaciju isečaka:
*/
func refSlices() {

	fmt.Println("\n --- Slices ---")

	s := []int{1, 2, 3}
	vs := reflect.ValueOf(s)

	for i := 0; i < vs.Len(); i++ {
		fmt.Println("Element", i, ":", vs.Index(i).Int())
	}

	// Modifying slice elements
	vs.Index(0).SetInt(10)
	fmt.Println("Updated Slice:", s) // Output: Updated Slice: [10 2 3]
}

/*
Mape :
......
Refleksija takođe omogućava dinamičku interakciju sa mapama:
*/

func refMaps() {

	fmt.Println("\n --- Maps ---")

	m := map[string]int{"foo": 1, "bar": 2}
	vm := reflect.ValueOf(m)

	for _, key := range vm.MapKeys() {
		fmt.Printf("Key: %s, Value: %d\n", key.String(), vm.MapIndex(key).Int())
	}

	// Setting a map value
	vm.SetMapIndex(reflect.ValueOf("foo"), reflect.ValueOf(42))
	fmt.Println("Updated Map:", m) // Output: Updated Map: map[bar:2 foo:42]
}

/*
Razumevanjem načina rada sa reflect.Value i njegovim metodama, možete dinamički
pristupati i menjati vrednosti, omogućavajući moćan i fleksibilan kod koji se
prilagođava različitim scenarijima izvršavanja.

Praktični slučajevi upotrebe refleksije u Golangu
-------------------------------------------------
Refleksija u Gou može se primeniti na razne praktične scenarije, poboljšavajući
fleksibilnost i dinamičnost vašeg koda. Evo tri slučaja upotrebe koji
demonstriraju moć refleksije.

Slučaj upotrebe 1:
Implementacija generičke funkcije korišćenjem refleksije
--------------------------------------------------------
Jedna uobičajena upotreba refleksije je implementacija generičkih funkcija koje
mogu da rade na različitim tipovima. Evo primera funkcije koja ispisuje bilo
koji tip isečka:
*/

func PrintSlice(slice interface{}) {

	v := reflect.ValueOf(slice)
	if v.Kind() != reflect.Slice {
		fmt.Println("Expected a slice")
		return
	}

	for i := 0; i < v.Len(); i++ {
		fmt.Printf("%v ", v.Index(i))
	}
	fmt.Println()
}

func refUsingSlices() {

	fmt.Println("\n --- Using ref on slices of diff types ---")

	ints := []int{1, 2, 3}
	strings := []string{"a", "b", "c"}
	slices := []string{"Radosav", "Radovanović"}
	maps := []map[int]string{{1: "To je to", 2: "To nije to"}, {1: "Hej, hej, hej"}}

	PrintSlice(ints)
	PrintSlice(strings)
	PrintSlice(slices)
	PrintSlice(maps)
}

/*
Ova funkcija koristi refleksiju da bi ispitala ulaz i iterirala kroz elemente
isečka, ispisujući svaki od njih bez obzira na tip isečka.

Slučaj upotrebe 2:
Izgradnja jednostavnog mehanizma serijalizacije/deserijalizacije
----------------------------------------------------------------
Refleksija se takođe može koristiti za implementaciju prilagođenih mehanizama
serijalizacije i deserijalizacije. Evo osnovnog primera koji serijalizuje
strukturu u mapu i deserijalizuje je nazad u struct:
*/

type Person3 struct {
	Name string
	Age  int
}

func Serialize(v interface{}) map[string]interface{} {

	result := make(map[string]interface{})

	rv := reflect.ValueOf(v)
	rt := reflect.TypeOf(v)

	for i := 0; i < rv.NumField(); i++ {
		field := rt.Field(i)
		result[field.Name] = rv.Field(i).Interface()
	}
	return result
}

func Deserialize(m map[string]interface{}, out interface{}) {

	rv := reflect.ValueOf(out).Elem()
	rt := rv.Type()

	for i := 0; i < rv.NumField(); i++ {
		field := rt.Field(i)
		if val, ok := m[field.Name]; ok {
			rv.Field(i).Set(reflect.ValueOf(val))
		}
	}
}

func refSerDeser() {

	fmt.Println("\n --- Using ref on serialize/deserialize data ---")

	p := Person3{Name: "Alice", Age: 30}
	serialized := Serialize(p)
	fmt.Println("Serialized:", serialized)

	var p2 Person3
	Deserialize(serialized, &p2)
	fmt.Println("Deserialized:", p2)
}

/*
Ovaj primer pokazuje kako dinamički pregledati i podesiti strukturna polja za
konverziju između strukturnih i mapiranih reprezentacija.

Slučaj upotrebe 3:
Pisanje dinamičkog JSON parsera
-------------------------------
Refleksija može pomoći u dinamičkom raščlanjivanju JSON-a bez poznavanja
njegove strukture unapred. Evo primera koji dekodira JSON u mapu polja i
njihovih vrednosti:
*/

func parseValue(v reflect.Value) map[string]interface{} {
	// This function parses reflect.Value type to map
	result := make(map[string]interface{}) // Map of string:interface() pairs

	switch v.Kind() {
	case reflect.Map:
		for _, key := range v.MapKeys() {
			result[key.String()] = v.MapIndex(key).Interface()
		}
	case reflect.Struct:
		for i := 0; i < v.NumField(); i++ {
			field := v.Type().Field(i)
			result[field.Name] = v.Field(i).Interface()
		}
	}
	return result
}

func DynamicJSONParser(data []byte) map[string]interface{} {
	var raw interface{} // bilo koji JSON ulaz. Nepoznat oblik unapred!!!
	json.Unmarshal(data, &raw)

	return parseValue(reflect.ValueOf(raw))
}

func refJSONDynamicParser() {

	fmt.Println("\n --- JSON Dynamic Parser ---")

	jsonData := `{"name": "Bob", "age": 25}`
	parsed := DynamicJSONParser([]byte(jsonData)) // Convert string to slice bytes
	fmt.Println("Parsed JSON:", jsonData, "=>", parsed)
}

/*
Ova funkcija koristi refleksiju za rdinamičko rukovanje strukturom JSON-a,
pretvarajući je u Go mapu za dalju obradu. Korišćenjem refleksije u ovim
slučajevima upotrebe, možete kreirati generičkiji, prilagodljiviji i moćniji
kod koji dinamički obrađuje različite tipove i strukture.

Razmatranja performansi
-----------------------
Korišćenje refleksije u Go jeziku dolazi sa troškovima u pogledu performansi
koji se moraju pažljivo razmotriti. Refleksija je generalno sporija od
direktnog izvršavanja koda zbog dinamičke provere tipova, indirektnog pristupa
i dodatnih alokacija memorije. Ovi faktori mogu dovesti do značajnog dodatnog
opterećenja, posebno u putanjama kritičnim za performanse.

Da bi se smanjili uticaji na performanse, najbolje je koristiti refleksiju
štedljivo i samo kada je to neophodno. Keširanje refleksivnih rezultata, kao
što su tipovi i pretrage metoda, može pomoći u smanjenju opterećenja. Na primer,
prethodno izračunavanje tipova tokom inicijalizacije i njihovo čuvanje u kešu
može izbeći troškove ponovljene refleksije.

Pored toga, kombinovanje refleksije sa interfejsima i korišćenje tvrdnji tipa
gde je to moguće takođe može poboljšati performanse. Preporučljivo je izbegavati
refleksiju u "vrućim putanjama", čestim operacijama, jednostavnim konverzijama
tipova i sa velikim strukturama podataka kako bi se sprečilo nepotrebno
smanjenje performansi.

Evo primera koji ilustruje razliku u performansama između reflektivnog i
nereflektivnog koda:
*/
type Example struct {
	Value int
}

func DirectAccess(e *Example) int {
	return e.Value
}

func ReflectiveAccess(e *Example) int {
	v := reflect.ValueOf(e).Elem()
	return int(v.FieldByName("Value").Int())
}

func refDiffRefVsNotref() {

	fmt.Println("\n --- Diff Reflect Vs Notreflect code ---")

	e := &Example{Value: 42}
	start := time.Now()
	for i := 0; i < 1000000; i++ {
		DirectAccess(e)
	}
	fmt.Println("Direct access time:", time.Since(start))

	start = time.Now()
	for i := 0; i < 1000000; i++ {
		ReflectiveAccess(e)
	}
	fmt.Println("Reflective access time:", time.Since(start))
}

/*
Obrada grešaka u refleksiji
---------------------------
Refleksija u Golangu  može biti moćna, ali dolazi i sa nekoliko zamki kojih
programeri moraju biti svesni. Pravilno rukovanje greškama je ključno kako bi
se osiguralo da je vaš refleksivni kod robustan i pouzdan.

Uobičajene zamke i kako se nositi sa njima
..........................................

	Nevažeći tipovi : Refleksija zahteva rad sa važećim tipovima. Korišćenje
	nevažećih ili nepodržanih tipova može izazvati paniku tokom izvršavanja.

	Rešenje : Uvek proverite tip pre nego što izvršite operacije.

	func PrintTypeInfo(i interface{}) {
	    v := reflect.ValueOf(i)
	    if v.Kind() == reflect.Invalid {
	        fmt.Println("Invalid type")
	        return
	    }
	    fmt.Println("Type:", v.Type())
	    fmt.Println("Kind:", v.Kind())
	}

	Neadresabilne vrednosti : Ne možete izmeniti vrednost ako nije adresabilna
	(tj. ako nije pointer ili polje strukture).

	Rešenje : Uverite se da radite sa adresabilnim vrednostima kada je potrebna
	modifikacija.

	func SetFieldValue(obj interface{}, name string, value interface{}) error {
	    v := reflect.ValueOf(obj).Elem()
	    if !v.IsValid() || v.Kind() != reflect.Struct {
	        return fmt.Errorf("expected a struct pointer")
	    }
	    field := v.FieldByName(name)
	    if !field.IsValid() || !field.CanSet() {
	        return fmt.Errorf("cannot set field %s", name)
	    }
	    val := reflect.ValueOf(value)
	    if field.Type() != val.Type() {
	        return fmt.Errorf("provided value type didn't match field type")
	    }
	    field.Set(val)
	    return nil
	}

	type Person struct {
	    Name string
	    Age  int
	}

	func main() {
	    p := &Person{Name: "Alice", Age: 30}
	    err := SetFieldValue(p, "Age", 35)
	    if err != nil {
	        fmt.Println("Error:", err)
	    } else {
	        fmt.Println("Updated person:", p)
	    }
	}

	Neusklađenost tipova : Pokušaj postavljanja vrednosti neusklađenog tipa
	može izazvati paniku tokom izvršavanja.

	Rešenje : Uvek proverite da li se tipovi podudaraju pre nego što podesite
	vrednost.

	func SetIntField(obj interface{}, name string, value int) error {
	    v := reflect.ValueOf(obj).Elem()
	    if v.Kind() != reflect.Struct {
	        return fmt.Errorf("expected a struct")
	    }
	    field := v.FieldByName(name)
	    if !field.IsValid() {
	        return fmt.Errorf("no such field: %s", name)
	    }
	    if field.Kind() != reflect.Int {
	        return fmt.Errorf("field %s is not an int", name)
	    }
	    if !field.CanSet() {
	        return fmt.Errorf("cannot set field %s", name)
	    }
	    field.SetInt(int64(value))
	    return nil
	}

Primeri robusnog rukovanja greškama u reflektivnom kodu
-------------------------------------------------------
Evo nekoliko primera koji demonstriraju robusno rukovanje greškama u
reflektivnom kodu:

Bezbedan pristup poljima:
.........................

	func GetFieldValue(obj interface{}, fieldName string) (interface{}, error) {
	    v := reflect.ValueOf(obj)
	    if v.Kind() != reflect.Struct {
	        return nil, fmt.Errorf("expected a struct")
	    }
	    field := v.FieldByName(fieldName)
	    if !field.IsValid() {
	        return nil, fmt.Errorf("no such field: %s", fieldName)
	    }
	    return field.Interface(), nil
	}

	func main() {
	    p := Person{Name: "Alice", Age: 30}
	    value, err := GetFieldValue(p, "Age")
	    if err != nil {
	        fmt.Println("Error:", err)
	    } else {
	        fmt.Println("Field Value:", value)
	    }
	}

Pozivanje dinamičke metode:
...........................

	func CallMethod(obj interface{}, methodName string, args ...interface{}) (interface{}, error) {
	    v := reflect.ValueOf(obj)
	    method := v.MethodByName(methodName)
	    if !method.IsValid() {
	        return nil, fmt.Errorf("no such method: %s", methodName)
	    }
	    if len(args) != method.Type().NumIn() {
	        return nil, fmt.Errorf("incorrect number of arguments")
	    }
	    in := make([]reflect.Value, len(args))
	    for i, arg := range args {
	        in[i] = reflect.ValueOf(arg)
	    }
	    results := method.Call(in)
	    if len(results) != 1 {
	        return nil, fmt.Errorf("expected one return value")
	    }
	    return results[0].Interface(), nil
	}

	type Person struct {
	    Name string
	    Age  int
	}

	func (p Person) Greet(greeting string) string {
	    return fmt.Sprintf("%s, my name is %s", greeting, p.Name)
	}

	func main() {
	    p := Person{Name: "Bob"}
	    result, err := CallMethod(p, "Greet", "Hello")
	    if err != nil {
	        fmt.Println("Error:", err)
	    } else {
	        fmt.Println("Result:", result)
	    }
	}

Robusnim rukovanjem greškama, možete osigurati da je vaš reflektivni kod
otporan i manje sklon panikama tokom izvršavanja, što dovodi do stabilnijih i
pouzdanijih aplikacija.

Napredne tehnike refleksije
---------------------------

Refleksija u Golangu  ne samo da vam omogućava da ispitate tipove i vrednosti,
već omogućava i naprednije operacije poput modifikovanja strukturnih polja,
dinamičkog pozivanje metoda i korišćenja strukturnih oznaka za upravljanje
ponašanjem. Evo nekih naprednih tehnika i primera.

Modifikacija strukturnih polja i dinamičko pozivanje metoda
-----------------------------------------------------------
Refleksija vam omogućava da menjate strukturna polja i dinamički pozivate metode
tokom izvršavanja. Ovo može biti korisno za kreiranje fleksibilnih i ponovo
upotrebljivih komponenti.

Modifikacija strukturnih polja:
-------------------------------

	type Person struct {
	    Name string
	    Age  int
	}

	func SetField(obj interface{}, name string, value interface{}) error {
	    v := reflect.ValueOf(obj).Elem()
	    if !v.IsValid() || v.Kind() != reflect.Struct {
	        return fmt.Errorf("expected a struct pointer")
	    }
	    field := v.FieldByName(name)
	    if !field.IsValid() || !field.CanSet() {
	        return fmt.Errorf("cannot set field %s", name)
	    }
	    val := reflect.ValueOf(value)
	    if field.Type() != val.Type() {
	        return fmt.Errorf("provided value type didn't match field type")
	    }
	    field.Set(val)
	    return nil
	}

	func main() {
	    p := &Person{Name: "Alice", Age: 30}
	    fmt.Println("Before:", p)
	    if err := SetField(p, "Age", 35); err != nil {
	        fmt.Println("Error:", err)
	    } else {
	        fmt.Println("After:", p)
	    }
	}

Dinamičko pozivanje metoda:
---------------------------

	func CallMethod(obj interface{}, methodName string, args ...interface{}) ([]interface{}, error) {
	    v := reflect.ValueOf(obj)
	    method := v.MethodByName(methodName)
	    if !method.IsValid() {
	        return nil, fmt.Errorf("no such method: %s", methodName)
	    }
	    if len(args) != method.Type().NumIn() {
	        return nil, fmt.Errorf("incorrect number of arguments")
	    }
	    in := make([]reflect.Value, len(args))
	    for i, arg := range args {
	        in[i] = reflect.ValueOf(arg)
	    }
	    results := method.Call(in)
	    out := make([]interface{}, len(results))
	    for i, result := range results {
	        out[i] = result.Interface()
	    }
	    return out, nil
	}

	type Person struct {
	    Name string
	}

	func (p Person) Greet(greeting string) string {
	    return fmt.Sprintf("%s, my name is %s", greeting, p.Name)
	}

	func main() {
	    p := Person{Name: "Bob"}
	    result, err := CallMethod(p, "Greet", "Hello")
	    if err != nil {
	        fmt.Println("Error:", err)
	    } else {
	        fmt.Println("Result:", result[0])
	    }
	}

Korišćenje oznaka sa refleksijom za podsticanje ponašanja
---------------------------------------------------------
Strukturne oznake pružaju način za dodavanje metapodataka strukturnim poljima,
što se može koristiti za dinamičko upravljanje ponašanjem putem refleksije. Ovo
se obično koristi u validaciji, serijalizaciji i drugim scenarijima gde je
potrebno ponašanje specifično za polja.

Primer: Implementacija prilagođenog validatora strukture korišćenjem oznaka i
refleksije

Evo primera implementacije prilagođenog validatora koji koristi strukturne
oznake za definisanje pravila validacije:

import (

	"errors"
	"fmt"
	"reflect"
	"strconv"
	"strings"

)

	type Person struct {
	    Name string `validate:"required"`
	    Age  int    `validate:"min=18"`
	}

	func ValidateStruct(s interface{}) error {
	    v := reflect.ValueOf(s)
	    if v.Kind() != reflect.Struct {
	        return errors.New("expected a struct")
	    }

	    t := v.Type()
	    for i := 0; i < t.NumField(); i++ {
	        field := t.Field(i)
	        value := v.Field(i).Interface()
	        tag := field.Tag.Get("validate")
	        if tag == "" {
	            continue
	        }

	        rules := strings.Split(tag, ",")
	        for _, rule := range rules {
	            if err := applyRule(rule, field.Name, value); err != nil {
	                return err
	            }
	        }
	    }

	    return nil
	}

	func applyRule(rule, fieldName string, value interface{}) error {
	    parts := strings.Split(rule, "=")
	    switch parts[0] {
	    case "required":
	        if isEmptyValue(value) {
	            return fmt.Errorf("%s is required", fieldName)
	        }
	    case "min":
	        minValue, err := strconv.Atoi(parts[1])
	        if err != nil {
	            return fmt.Errorf("invalid min value for %s", fieldName)
	        }
	        if v, ok := value.(int); ok {
	            if v < minValue {
	                return fmt.Errorf("%s should be at least %d", fieldName, minValue)
	            }
	        }
	    }
	    return nil
	}

	func isEmptyValue(v interface{}) bool {
	    return reflect.DeepEqual(v, reflect.Zero(reflect.TypeOf(v)).Interface())
	}

	func main() {
	    p := Person{Name: "Alice", Age: 17}
	    if err := ValidateStruct(p); err != nil {
	        fmt.Println("Validation error:", err)
	    } else {
	        fmt.Println("Validation passed")
	    }
	}

U ovom primeru, ValidateStructfunkcija koristi refleksiju da bi čitala pravila
validacije iz oznaka strukture i primenila ih na polja strukture. Ovaj pristup
omogućava fleksibilnu i ponovo upotrebljivu logiku validacije vođenu
metapodacima. Korišćenjem ovih naprednih tehnika refleksije, možete kreirati
dinamične i moćne Go aplikacije koje se prilagođavaju različitim zahtevima i
uslovima izvršavanja.

Zaključak
---------
U ovom blogu smo istražili moćnu funkciju refleksije u jeziku Go, pokrivajući
njenu definiciju, značaj i praktičnu primenu. Počeli smo sa uvodom u refleksiju,
objašnjavajući njenu ulogu u omogućavanju programima da pregledaju i menjaju
sopstvenu strukturu i ponašanje tokom izvršavanja.

Udubili smo se u osnove paketa  reflect, učeći kako da dobijemo informacije o
tipu i vrednosti koristeći reflect.TypeOfi reflect.ValueOf. Razgovarali smo o
tome kako da radimo sa tipovima i vrednostima, uključujući inspekciju polja i
metoda struktura i rešavanje uobičajenih grešaka i scenarija grešaka.

Praktični slučajevi upotrebe pokazali su kako implementirati generičke funkcije,
izgraditi mehanizme serijalizacije i napisati dinamičke JSON parsere koristeći
refleksiju. Takođe smo obradili napredne tehnike refleksije kao što su
modifikovanje strukturnih polja, dinamičko pozivanje metoda i korišćenje
strukturnih oznaka za prilagođenu validaciju.

Refleksiju treba koristiti pažljivo, jer može povećati performanse i povećati
složenost. Najkorisnija je u scenarijima koji zahtevaju generičko programiranje,
dinamičko ponašanje i prilikom izgradnje fleksibilnih API-ja. Da bi se
refleksija efikasno koristila, neophodno je robusno rukovati greškama, keširati
refleksivne rezultate i izbegavati korišćenje refleksije u kritičnim putanjama
za performanse.

Na kraju, ohrabrujemo vas da eksperimentišete sa refleksijom u Gou. Ona može
značajno poboljšati fleksibilnost i mogućnost ponovne upotrebe vašeg koda,
omogućavajući vam da se nosite sa složenim i dinamičnim programskim izazovima.
Razumevanjem i primenom koncepata i tehnika o kojima se govori u ovom blogu,
možete koristiti refleksiju za pisanje dinamičnijih, prilagodljivijih i
moćnijih Go programa.
*/
func main() {
	fmt.Println("\n ---  ---")
	fmt.Println("\n --- Reflect in Go ---")

	refTypeOF()
	refKind()
	refBasicKindOfTypes()
	refFieldIntrospection()
	refMethodIntrospection()
	refValueOf()
	refGetValue()
	refSetValue()
	refPointers()
	refSlices()
	refMaps()
	refUsingSlices()
	refSerDeser()
	refJSONDynamicParser()
	refDiffRefVsNotref()
}
