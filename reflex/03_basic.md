
# Refleks osnove

Ova stranica daje uvod u najčešće koncepte koje ćete koristiti za izgradnju refleks aplikacija.

Naučićete kako da:

- Kreirate i ugnezdite komponente
- Prilagodite i stilizujete komponente
- Razlikujete između vremena kompilacije i vremena izvođenja
- Prikažete podatke koji se menjaju tokom vremena
- Odgovorite na događaje i ažurirate ekran
- Renderujete uslovni i kreirate liste
- Kreirajte stranice i krećete se između njih
- Instalirate refleks pomoću pip-a.

```sh
pip install reflex
```

Uvezite refleks biblioteku da biste započeli.

```py
import reflex as rx
```

## Kreiranje i gneždjenje komponenti

Komponente su građevinski blokovi za korisnički interfejs vaše aplikacije (UI). One su vizuelni elementi koji čine vašu aplikaciju, poput dugmadi, teksta i slika. Refleks ima širok izbor ugrađenih komponenti da biste brzo započeli.

Komponente se kreiraju koristeći funkcije koje vraćaju objekat komponente.

```py
def my_button():
    return rx.button("Click Me")
```

Komponente se mogu ugnezditi jedna u drugu da bi stvorili složeni UI.

Da bi se komponente gnezdile kao deca, prosledite ih kao pozicioni argument roditeljskoj komponenti. U donjem primeru, `rx.text` i `my_button` komponente su deca komponente `rx.box`.

```py
def my_page():
    return rx.box(
        rx.text("This is a page"),
        # Reference components defined in other functions.
        my_button(),
    )
```

Takođe možete da koristite bilo koji osnovni HTML element kroz prostor imena `rx.el.div`. Ovo vam omogućava da koristite standardne HTML elemente direktno u vašoj refleks aplikaciji kada vam je potrebno više kontrole ili kada određena komponenta nije dostupna u biblioteci refleks komponenti.

```py
def my_div():
    return rx.el.div(
        rx.el.p("Use base html!"),
    )
```

Ako vam je potrebna komponenta koju ne pruža refleks, možete da proverite treću stranu ekosistema ili omotajte sopstvenu React komponentu.

## Prilagođavanje i stilizovanje komponente

Komponente se mogu prilagoditi pomoću `props`-ova, koji se prenose kao argumente ključnih reči na funkciju komponente.

Svaka komponenta ima `props` koje su specifične za tu komponentu. Proverite dokumentaciju za komponentu koju koristite da biste videli koji su `props` dostupni.

```py
def half_filled_progress():
    return rx.progress(value=50)
```

Pored komponentnih specifičnih `props`-a, komponente se takođe mogu oblikovati pomoću CSS svojstava koja su prosleđena kao props.

```py
def round_button():
    return rx.button(
        "Click Me", border_radius="15px", font_size="18px"
    )
```

> **Koristite `snake_case` CSS property imena imovine kao `prop` ime**.

Pogledajte vodič za stajling za više informacija o načinima stilizovanja komponenti.

Ukratko, komponente su napravljene od `children` komponenti i `props` propertija.

### Children

- Tekst ili druge refleks komponente ugnežđene unutar komponente.
- Prosledjuju se kao **positional arguments**.

### Props

- Atributi koji utiču na ponašanje i izgled komponente.
- Prosledjuju se kao **keyword arguments**.

## Prikazivanje podataka koji se menjaju tokom vremena

Aplikacije moraju da čuvaju i prikazuju podatke koji se menjaju tokom vremena. Refleks je to obezbedio kroz `state` klasu, što je Pajton klasa koja čuva promenljive koje se mogu promeniti kada se aplikacija pokrene, kao i funkcije koje mogu promeniti te promenljive.

Da biste definisali `state` klasu, izvedite iz `rx.State` klase i definišite polja koja čuvaju stanje vaše aplikacije. `State` promenljive ( `vars` ) treba da imaju `napomenu tipa` i mogu se inicijalizovati podrazumevanom vrednošću.

```py
class MyState(rx.State):
    count: int = 0
```

## Reference state vars u komponentama

Da biste referencali state `var` u komponenti, možete ga prozvati kao `children` ili `prop`. Komponenta će se automatski ažurirati kada se `state var` promene.

`Vars` se pozivaju putem atributa state klase. Na primer, da biste referencali "count" `var` u komponenti "MyState", koristite "MyState.count".

```py
class MyState(rx.State):
    count: int = 0
    color: str = "red"

def counter():
    return rx.hstack(
        
        # The heading `color` prop is set to the `color` var in MyState.
        rx.heading("Count: ", color=MyState.color),
        
        # The `count` var in `MyState` is passed as a child to the heading component.
        rx.heading(MyState.count),
    )
```

`Vars` se može referencirati iz više komponenti i automatski će se ažurirati kada se `state` promeni.

## Odgovaranje na događaje i ažuriranje ekrana

Do sada smo definisali state vars, ali nismo pokazali kako da ih promenimo.Sve promene state rukuje se funkcijama u state klasi, koja se nazivaju `event handlers`.

> **Event handlers su jedini način da se promeni state u Refleksu.**

Komponente imaju posebne props, kao što su `on_click`, nazvani `event trigers` koji se mogu koristiti za interaktivne komponente. 

## Event trigers

Povežite komponente na rukovodioce događaja, koje ažuriraju state.

```py
class CounterState(rx.State):
    count: int = 0

    @rx.event
    def increment(self):
        self.count += 1


def counter_increment():
    return rx.hstack(
        rx.heading(CounterState.count),
        rx.button(
            "Increment", on_click=CounterState.increment
        ),
    )
```

Kada se aktivira okidač događaja, poziva se rukovaoc događajem, koji ažurira stanje. UI se automatski preispituje da odražava novi state.

## What is the @rx.event decorator?

### Event handlers with arguments

Event handlers can also take in arguments. For example, the increment event handler can take an argument to increment the count by a specific amount.

```py
class CounterState2(rx.State):
    count: int = 0

    @rx.event
    def increment(self, amount: int):
        self.count += amount

def counter_variable():
    return rx.hstack(
        rx.heading(CounterState2.count),
        rx.button(
            "Increment by 1",
            on_click=lambda: CounterState2.increment(1),
        ),
        rx.button(
            "Increment by 5",
            on_click=lambda: CounterState2.increment(5),
        ),
    )
```

The on_click event trigger doesn't pass any arguments here, but some event triggers do. For example, the on_blur event trigger passes the text of an input as an argument to the event handler.

```py
class TextState(rx.State):
    text: str = ""

    @rx.event
    def update_text(self, new_text: str):
        self.text = new_text

def text_input():
    return rx.vstack(
        rx.heading(TextState.text),
        rx.input(
            default_value=TextState.text,
            on_blur=TextState.update_text,
        ),
    )
```

Make sure that the event handler has the same number of arguments as the event trigger, or an error will be raised.

## Compile-time vs. runtime (IMPORTANT)

Before we dive deeper into state, it's important to understand the difference between compile-time and runtime in Reflex.

When you run your app, the frontend gets compiled to Javascript code that runs in the browser (compile-time). The backend stays in Python and runs on the server during the lifetime of the app (runtime).

When can you not use pure Python?
We cannot compile arbitrary Python code, only the components that you define. What this means importantly is that you cannot use arbitrary Python operations and functions on state vars in components.

However, since any event handlers in your state are on the backend, you can use any Python code or library within your state.

Examples that work
Within an event handler, use any Python code or library.

```py
def check_even(num: int):
    return num % 2 == 0

class MyState3(rx.State):
    count: int = 0
    text: str = "even"

    @rx.event
    def increment(self):
        # Use any Python code within state.
        # Even reference functions defined outside the state.
        if check_even(self.count):
            self.text = "even"
        else:
            self.text = "odd"
        self.count += 1

def count_and_check():
    return rx.box(
        rx.heading(MyState3.text),
        rx.button("Increment", on_click=MyState3.increment),
    )
```

Use any Python function within components, as long as it is defined at compile time (i.e. does not reference any state var)

```sh
0true
1false
2true
3false
4true
5false
6true
7false
8true
9false
```

```py
def show_numbers():
    return rx.vstack(
        *[rx.hstack(i, check_even(i)) for i in range(10)]
    )
```

Examples that don't work
You cannot do an if statement on vars in components, since the value is not known at compile time.

```py
class BadState(rx.State):
    count: int = 0

def count_if_even():
    return rx.box(
        rx.heading("Count: "),
        # This will raise a compile error, as BadState.count is a var and not known at compile time.
        rx.text(
            BadState.count
            if BadState.count % 2 == 0
            else "Odd"
        ),
        # Using an if statement with a var as a prop will NOT work either.
        rx.text(
            "hello",
            color=(
                "red" if BadState.count % 2 == 0 else "blue"
            ),
        ),
    )
```

You cannot do a for loop over a list of vars.

```py
class BadState(rx.State):
    items: list[str] = ["Apple", "Banana", "Cherry"]

def loop_over_list():
    return rx.box(
        # This will raise a compile error, as BadState.items is a list and not known at compile time.
        *[rx.text(item) for item in BadState.items]
    )
```

You cannot do arbitrary Python operations on state vars in components.

```py
class BadTextState(rx.State):
    text: str = "Hello world"

def format_text():
    return rx.box(
        # Python operations such as `len` will not work on state vars.
        rx.text(len(BadTextState.text)),
    )
```

In the next sections, we will show how to handle these cases.

Conditional rendering
As mentioned above, you cannot use Python if/else statements with state vars in components. Instead, use the rx.cond function to conditionally render components.

```py
class LoginState(rx.State):
    logged_in: bool = False

    @rx.event
    def toggle_login(self):
        self.logged_in = not self.logged_in

def show_login():
    return rx.box(
        rx.cond(
            LoginState.logged_in,
            rx.heading("Logged In"),
            rx.heading("Not Logged In"),
        ),
        rx.button(
            "Toggle Login", on_click=LoginState.toggle_login
        ),
    )
```

Rendering lists
To iterate over a var that is a list, use the rx.foreach function to render a list of components.

Pass the list var and a function that returns a component as arguments to rx.foreach.

```py
class ListState(rx.State):
    items: list[str] = ["Apple", "Banana", "Cherry"]

def render_item(item: rx.Var[str]):
    """Render a single item."""
    # Note that item here is a Var, not a str!
    return rx.list.item(item)

def show_fruits():
    return rx.box(
        rx.foreach(ListState.items, render_item),
    )
```

The function that renders each item takes in a Var, since this will get compiled up front.

## Var Operations

You can't use arbitrary Python operations on state vars in components, but Reflex has var operations that you can use to manipulate state vars.

For example, to check if a var is even, you can use the % and == var operations.

```py
class CountEvenState(rx.State):
    count: int = 0

    @rx.event
    def increment(self):
        self.count += 1

def count_if_even():
    return rx.box(
        rx.heading("Count: "),
        rx.cond(
            # Here we use the `%` and `==` var operations to check if the count is even.
            CountEvenState.count % 2 == 0,
            rx.text("Even"),
            rx.text("Odd"),
        ),
        rx.button(
            "Increment", on_click=CountEvenState.increment
        ),
    )
```

## App and Pages

Reflex apps are created by instantiating the rx.App class. Pages are linked to specific URL routes, and are created by defining a function that returns a component.

```py
def index():
    return rx.text("Root Page")

rx.app = rx.App()
app.add_page(index, route="/")
```

## Next Steps

Now that you have a basic understanding of how Reflex works, the next step is to start coding your own apps. Try one of the following tutorials:

- Dashboard Tutorial
- Chatapp Tutorial
