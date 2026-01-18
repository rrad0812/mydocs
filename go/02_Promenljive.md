[[Uvod]](01_uvod.md) [[Sadržaj]](toc.md) [[Tipovi podataka]](03_Tipovi_podataka.md)

# Promenljive i konstante

### Promenljive

Počnimo sa deklarisanjem promenljive.

1. **Deklaracija bez inicijalizacije**:
	```
    var foo string
    ```
2. **Deklaracija sa inicijalizacijom**:
    ```
    var foo string = "Go is awesome"
    ```
3. **Višestruke deklaracije**:
    ```
    var foo, bar string = "Hello", "World"
    // ili
    var (
        foo string = "Hello"
        bar string  = "World"
    )
    ```
4. **Deklaracija sa zaključenim tipom**

	Tip je izostavljen, ali će biti zaključen:
    ```
    var foo = "What's my type?"
    ```
5. **Skraćena deklaracija**
	
	Skraćena deklaracija, ovde izostavljamo i var ključnu reč, a tip je uvek implicitan. Ovako ćemo videti promenljive koje se deklarišu većinu vremena. Takođe koristimo walrus operator dodele `:=` za deklaraciju plus dodelu.
    ```
    foo := "Shorthand!"
    ```
    **Napomena**: Skraćena deklaracija rade samo unutar tela funkcije.

### Konstante

Konstante možemo deklarisati pomoću `const` ključne reči. Koje, kao što ime sugeriše, konstante predstavljaju fiksne vrednosti koje se ne mogu ponovo dodeliti.
```
const constant = "This is a constant"
```
Takođe je važno napomenuti da se samo konstante mogu dodeliti drugim konstantama.
```
const a = 10
const b = a // ✅ Works

var a = 10
const b = a // ❌ a (variable of type int) is not constant (InvalidConstInit)
```

[[Uvod]](01_uvod.md) [[Sadržaj]](toc.md) [[Tipovi podataka]](03_Tipovi_podataka.md)