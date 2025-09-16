# Tutorial: Data Dashboard

Tokom ovog tutorijala izgradićete malu kontrolnu tablu za podatke, gde možete da unesete podatke i to će se pružiti u tabeli i grafikonu.Ovaj vodič ne preuzima bilo kakvo postojeće znanje Refleksa, ali preporučujemo da prvo proveramo vodič za brze osnove.

Tehnike koje ćete naučiti u vodiču su od suštinskog značaja za izgradnju bilo koje Refleks aplikacije i u potpunosti razumeti da će vam pružiti duboko razumevanje Refleksa.

Ovaj vodič je podeljen na nekoliko odeljka:

- **Podešavanje za tutorial**: Polazište za praćenje vodiča
- **Pregled**: Osnove reflekskog UI (komponente i rekvizite)
- **Pokazivanje dinamičnih podataka**: Kako se koristi država da se  
  prikaže podaci koji će se promeniti u vašoj aplikaciji.
- **Dodajte podatke na svoju aplikaciju**: pomoću obrasca da biste 
  omogućili da korisnik dodaje podatke u vašu aplikaciju i uvesti rukovatelje događajima.
- **Izveštavanje podataka na grafikonu**: Kako se koristi rezervne 
  komponente refleksa.
- **Završno čišćenje i zaključak**: Kako dalje prilagoditi svoju 
  aplikaciju i dodajte neko dodatno stiling na to.

**Šta gradite?**

U ovom tutorialu gradite interaktivnu kontrolnu tablu sa Refleksom.

```py
import reflex as rx

from collections import Counter

class User(rx.Base):
    """The user model."""
    name: str
    email: str
    gender: str

class State(rx.State):
    users: list[User] = [
        User(
            name="Danilo Sousa",
            email="danilo@example.com",
            gender="Male",
        ),
        User(
            name="Zahra Ambessa",
            email="zahra@example.com",
            gender="Female",
        ),
    ]
    users_for_graph: list[dict] = []

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))
        self.transform_data()

    def transform_data(self):
        """Transform user gender group data into a format suitable for visualization in graphs."""
        # Count users of each gender group
        gender_counts = Counter(
            user.gender for user in self.users
        )

        # Transform into list of dict so it can be used in the graph
        self.users_for_graph = [
            {"name": gender_group, "value": count}
            for gender_group, count in gender_counts.items()
        ]

def show_user(user: User):
    """Show a user in a table row."""
    return rx.table.row(
        rx.table.cell(user.name),
        rx.table.cell(user.email),
        rx.table.cell(user.gender),
        style={"_hover": {"bg": rx.color("gray", 3)}},
        align="center",
    )

def add_customer_button() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.trigger(
            rx.button(
                rx.icon("plus", size=26),
                rx.text("Add User", size="4"),
            ),
        ),
        rx.dialog.content(
            rx.dialog.title(
                "Add New User",
            ),
            rx.dialog.description(
                "Fill the form with the user's info",
            ),
            rx.form(
                rx.flex(
                    rx.input(
                        placeholder="User Name",
                        name="name",
                        required=True,
                    ),
                    rx.input(
                        placeholder="user@reflex.dev",
                        name="email",
                    ),
                    rx.select(
                        ["Male", "Female"],
                        placeholder="male",
                        name="gender",
                    ),
                    rx.flex(
                        rx.dialog.close(
                            rx.button(
                                "Cancel",
                                variant="soft",
                                color_scheme="gray",
                            ),
                        ),
                        rx.dialog.close(
                            rx.button(
                                "Submit", type="submit"
                            ),
                        ),
                        spacing="3",
                        justify="end",
                    ),
                    direction="column",
                    spacing="4",
                ),
                on_submit=State.add_user,
                reset_on_submit=False,
            ),
            max_width="450px",
        ),
    )

def graph():
    return rx.recharts.bar_chart(
        rx.recharts.bar(
            data_key="value",
            stroke=rx.color("accent", 9),
            fill=rx.color("accent", 8),
        ),
        rx.recharts.x_axis(data_key="name"),
        rx.recharts.y_axis(),
        data=State.users_for_graph,
        width="100%",
        height=250,
    )

def index() -> rx.Component:
    return rx.vstack(
        add_customer_button(),
        rx.table.root(
            rx.table.header(
                rx.table.row(
                    rx.table.column_header_cell("Name"),
                    rx.table.column_header_cell("Email"),
                    rx.table.column_header_cell("Gender"),
                ),
            ),
            rx.table.body(
                rx.foreach(State.users, show_user),
            ),
            variant="surface",
            size="3",
            width="100%",
        ),
        graph(),
        align="center",
        width="100%",
    )

app = rx.App(
    theme=rx.theme(radius="full", accent_color="grass"),
)

app.add_page(
    index,
    title="Customer Data App",
    description="A simple app to manage customer data.",
    on_load=State.transform_data,
)
```

Ne brinite ako ne razumete gornji kod, u ovom tutorialu ćemo vas prošetati kroz celu stvar korak po korak.

## Podešavanje za tutorial

Pogledajte dokumente u instalaciju da biste dobili Refleks postavljen na svoju mašinu. Sledite ih da biste kreirali direktorijum koja se zove Dashboard_Tutorial, u koji ćete sa u i pip instalirati Refleks.

Biraćemo šablon `0` kada pokrećemo `reflex init` da biste dobili prazan obrazac. Na kraju pokrenite `reflex run` da biste pokrenuli aplikaciju i potvrdili da je sve ispravno postavljeno.

## Pregled

Sad kad smo postavljeni, hajde da uzmemo pregled Refleksa!

### Pregled starterskog koda

Unutar našeg dashboard_tutorial direktorijuma, upravo smo se `cd`, nalazi se i `rxconfig.py` datoteka koja sadrži konfiguraciju za našu Refleks aplikaciju. 

Postoji i direktorijum `assets` u koji se mogu postaviti statičke datoteke poput slika i stilova, da bi se referencirali u vašoj aplikaciji.

Ono što je najvažnije, postoji i direktorijum koja se takođe zove Dashboard_Tutorial, koji sadrži sav kod za vašu aplikaciju. Unutar ovog direktorijuma nalazi se datoteka po imenu `Dashboard_Tutorial.py`. Da biste započeli ovaj vodič, izbrisaćemo sve kodove u ovoj datoteci tako da možemo početi ispočetka i objasniti svaki korak dok idemo napred.

Prvo što moramo da uradimo je uvoz Refleks-a. Jednom kada to učinimo, možemo da stvorimo komponentu, koja je deo korisničkog interfejsa. Komponente se koriste za prikazivanje, upravljanje i ažuriranje UI elemenata u vašoj aplikaciji.

Pogledajmo primer ispod. Ovde imamo funkciju pod nazivom `index` koji vraća tekstualnu komponentu (ugrađena Refleks UI komponenta) koja prikazuje tekst "Hello World!".

Zatim definišemo našu aplikaciju koristeći app = rx.App () i dodajemo komponentu koju smo upravo definisali (index) na stranicu pomoću `app.add_page(index)`. Ime funkcije (u ovom primeru `index`) koji definiše komponentu, mora biti ono što prosledjujemo u `add_page`. Definicija aplikacije i dodavanje komponente na stranicu potrebna je za svaku `Refleks` aplikaciju.

```py
import reflex as rx

def index() -> rx.Component:
    return rx.text("Hello World!")

app = rx.App()
app.add_page(index)
```

Ovaj kod će renderisati stranu sa tekstom "Hello World!" kada pokrenemo aplikaciju:

```sh
Hello World!
```

Za ostatak tutorijala kod:

```py
app = rx.App()
app.add_page 
```

će se podrazumevati i da nije prikazan u isečcima koda.

### Kreiranje tabele

Kreirajmo novu komponentu koja će prikazati tabelu. Koristićemo komponentu `rx.table` da to uradimo.Komponenta rx.table ima `root`, koji uzima `header` i `body`, koje zauzvrat uzima u komponente `row`. Komponenta `row` uzima `cell` komponente koje su stvarni podaci koji će se prikazati u tabeli.

```py
def index() -> rx.Component:
    return rx.table.root(
        rx.table.header(
            rx.table.row(
                rx.table.column_header_cell("Name"),
                rx.table.column_header_cell("Email"),
                rx.table.column_header_cell("Gender"),
            ),
        ),
        rx.table.body(
            rx.table.row(
                rx.table.cell("Danilo Sousa"),
                rx.table.cell("danilo@example.com"),
                rx.table.cell("Male"),
            ),
            rx.table.row(
                rx.table.cell("Zahra Ambessa"),
                rx.table.cell("zahra@example.com"),
                rx.table.cell("Female"),
            ),
        ),
    )
```

Komponente u Refleksu imaju `props`, koje se mogu koristiti za prilagođavanje komponenti i prenose se kao argumenti ključnih reči na funkciju komponente.

rx.table.root komponenta ima `variant` i `size` props, koji prilagođavaju tabelu kao što se vidi  u daljem tekstu.

```py
def index() -> rx.Component:
    return rx.table.root(
        rx.table.header(
            rx.table.row(
                rx.table.column_header_cell("Name"),
                rx.table.column_header_cell("Email"),
                rx.table.column_header_cell("Gender"),
            ),
        ),
        rx.table.body(
            rx.table.row(
                rx.table.cell("Danilo Sousa"),
                rx.table.cell("danilo@example.com"),
                rx.table.cell("Male"),
            ),
            rx.table.row(
                rx.table.cell("Zahra Ambessa"),
                rx.table.cell("zahra@example.com"),
                rx.table.cell("Female"),
            ),
        ),
        variant="surface",
        size="3",
    )
```

### Prikazivanje dinamičkih podataka (State)

Do ove tačke sve podatke koje se prikazujemo u aplikaciji je statički. Ovo nije baš korisno za kontrolnu tablu za podatke. Moramo biti u mogućnosti da pokažemo dinamičke podatke koji se mogu dodati i ažurirati.

Ovde ulazi klasa `state`. `State` je pajton klasa koja čuva promenljive koje se mogu promeniti kada se aplikacija pokrene, kao i funkcije koje mogu promeniti te promenljive.

Da biste definisali `State` klasu, potklasirajte `rx.state` i definišite polja koja čuvaju stanje vaše aplikacije. State promenljive ( vars ) treba da imaju napomenu tipa i mogu se inicijalizovati podrazumevanom vrednošću.

U donjem primeru definišemo `state` klasu pod nazivom "State" koje ima promenljivu koja se zove "users" koja je lista lista stringova. Svaka lista u listi "users" predstavlja korisnike i sadrži njihovo ime, e-poštu i pol.

```py
class State(rx.State):
    users: list[list[str]] = [
        ["Danilo Sousa", "danilo@example.com", "Male"],
        ["Zahra Ambessa", "zahra@example.com", "Female"],
    ]
```

Da bi se iteriralo preko state var koji je lista, koristimo funkciju `rx.foreach` da renderujemo listu komponenti. `rx.foreach` uzima iterable (listu, tuple ili dict) i funkciju koja renderuje svaku  stavku u iterable-u.

> [!Note]
>
> Zašto ne možemo samo da prodjemo samo sa for petljom?
> Pogledajte dokumentaciju da biste saznali zašto.

Ovde je render funkcija "show_user" koja uzima u jednog korisnika i vraća `tabelu.row` komponentu koja prikazuje ime korisnika, e-poštu i pol.

```py
class State(rx.State):
    users: list[list[str]] = [
        ["Danilo Sousa", "danilo@example.com", "Male"],
        ["Zahra Ambessa", "zahra@example.com", "Female"],
    ]

def show_user(person: list):
    """Show a person in a table row."""
    return rx.table.row(
        rx.table.cell(person[0]),
        rx.table.cell(person[1]),
        rx.table.cell(person[2]),
    )

def index() -> rx.Component:
    return rx.table.root(
        rx.table.header(
            rx.table.row(
                rx.table.column_header_cell("Name"),
                rx.table.column_header_cell("Email"),
                rx.table.column_header_cell("Gender"),
            ),
        ),
        rx.table.body(
            rx.foreach(State.users, show_user),
        ),
        variant="surface",
        size="3",
    )
```

Kao što vidite, izlaz gore izgleda isto kao i pre, osim sada, podaci više nisu statički i mogu se menjati iz korisničkog ulaza u aplikaciju.

### Korišćenje odgovarajuće strukture klase za naše podatke

Do sada su naši podaci definisani na listi lista, gde se podacima pristupa indeksom tj users[0], users[1]. Ovo nije baš održivo, jer će naša aplikacija postajati sve veća.

Bolji način strukturiranja naših podataka u Refleksu je da koristite klasu da bi predstavljali korisnika. Na ovaj način možemo pristupiti podacima koji koriste atribute, tj. "user.name", "user.email".

U Refleksu kada kreiramo ove klase da bismo prikazali naše podatke, klasa mora naslediti od `rx.Base` klase.

`rx.Base` je takođe neophodna ako želimo da imamo state var koji je iterable sa različitim tipovima. Na primer, ako bismo želeli da imamo `age` kao `int`, morali bismo da koristimo rx.Base jer to nismo mogli da uradimo sa state var definisanim kao `list[list[str]]`.

Render funkcija "show_user" se takođe ažurira da bi pristupila podacima imenovanim atributima, umesto indeksiranja.

```py
class User(rx.Base):
    """The user model."""
    name: str
    email: str
    gender: str

class State(rx.State):
    users: list[User] = [
        User(
            name="Danilo Sousa",
            email="danilo@example.com",
            gender="Male",
        ),
        User(
            name="Zahra Ambessa",
            email="zahra@example.com",
            gender="Female",
        ),
    ]

def show_user(user: User):
    """Show a person in a table row."""
    return rx.table.row(
        rx.table.cell(user.name),
        rx.table.cell(user.email),
        rx.table.cell(user.gender),
    )

def index() -> rx.Component:
    return rx.table.root(
        rx.table.header(
            rx.table.row(
                rx.table.column_header_cell("Name"),
                rx.table.column_header_cell("Email"),
                rx.table.column_header_cell("Gender"),
            ),
        ),
        rx.table.body(
            rx.foreach(State.users, show_user),
        ),
        variant="surface",
        size="3",
    )
```

Zatim dodajemo formu na aplikaciju, tako da možemo dodati nove korisnike na tabelu.

### Korišćenje forme za dodavanje podataka

Izgrađujemo obrazac koji koristi `rx.form`, koji uzima nekoliko komponenti kao što su `rx.input` i `rx.select`, koji predstavljaju polja u formi, koji vam omogućavaju da dodate informacije.

`rx.input` komponenta uzima u nekoliko props-ova. `placeholder` je tekst koji je prikazan u polju za unos kada je prazno. `name` Prop je ime ulaznog polja, koje se prenosi u rečniku kada se obrazac submituje. `required` prop je `boolean` koji određuje da li je polje obavezno za unos.

`rx.select` komponenta uzima listu opcija koje su prikazane u padajućem meniju. Ostali props ovde su identični `rx.input` komponenti.

```py
rx.form(
    rx.input(
        placeholder="User Name", name="name", required=True
    ),
    rx.input(
        placeholder="user@reflex.dev",
        name="email",
    ),
    rx.select(
        ["Male", "Female"],
        placeholder="Male",
        name="gender",
    ),
)
```

This form is all very compact as you can see from the example, so we need to add some styling to make it look better. We can do this by adding a vstack component around the form fields. The vstack component stacks the form fields vertically. Check out the layout docs for more information on how to layout your app.

```py
rx.form(
    rx.vstack(
        rx.input(
            placeholder="User Name",
            name="name",
            required=True,
        ),
        rx.input(
            placeholder="user@reflex.dev",
            name="email",
        ),
        rx.select(
            ["Male", "Female"],
            placeholder="Male",
            name="gender",
        ),
    ),
)
```

Now you have probably realised that we have all the form fields, but we have no way to submit the form. We can add a submit button to the form by adding a rx.button component to the vstack component. The rx.button component takes in the text that is displayed on the button and the type prop which is the type of button. The type prop is set to submit so that the form is submitted when the button is clicked.

In addition to this we need a way to update the users state variable when the form is submitted. All state changes are handled through functions in the state class, called event handlers.

Components have special props called event triggers, such as on_submit, that can be used to make components interactive. Event triggers connect components to event handlers, which update the state. Different event triggers expect the event handler that you hook them up to, to take in different arguments (and some do not take in any arguments).

The on_submit event trigger of rx.form is hooked up to the add_user event handler that is defined in the State class. This event trigger expects to pass a dict, containing the form data, to the event handler that it is hooked up to. The add_user event handler takes in the form data as a dictionary and appends it to the users state variable.

```py
class State(rx.State):

    ...

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))


def form():
    return rx.form(
        rx.vstack(
            rx.input(
                placeholder="User Name",
                name="name",
                required=True,
            ),
            rx.input(
                placeholder="user@reflex.dev",
                name="email",
            ),
            rx.select(
                ["Male", "Female"],
                placeholder="Male",
                name="gender",
            ),
            rx.button("Submit", type="submit"),
        ),
        on_submit=State.add_user,
        reset_on_submit=True,
    )
```

Finally we must add the new form() component we have defined to the index() function so that the form is rendered on the page.

Below is the full code for the app so far. If you try this form out you will see that you can add new users to the table by filling out the form and clicking the submit button. The form data will also appear as a toast (a small window in the corner of the page) on the screen when submitted.

```py
class State(rx.State):
    users: list[User] = [
        User(
            name="Danilo Sousa",
            email="danilo@example.com",
            gender="Male",
        ),
        User(
            name="Zahra Ambessa",
            email="zahra@example.com",
            gender="Female",
        ),
    ]

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))

def show_user(user: User):
    """Show a person in a table row."""
    return rx.table.row(
        rx.table.cell(user.name),
        rx.table.cell(user.email),
        rx.table.cell(user.gender),
    )

def form():
    return rx.form(
        rx.vstack(
            rx.input(
                placeholder="User Name",
                name="name",
                required=True,
            ),
            rx.input(
                placeholder="user@reflex.dev",
                name="email",
            ),
            rx.select(
                ["Male", "Female"],
                placeholder="Male",
                name="gender",
            ),
            rx.button("Submit", type="submit"),
        ),
        on_submit=State.add_user,
        reset_on_submit=True,
    )

def index() -> rx.Component:
    return rx.vstack(
        form(),
        rx.table.root(
            rx.table.header(
                rx.table.row(
                    rx.table.column_header_cell("Name"),
                    rx.table.column_header_cell("Email"),
                    rx.table.column_header_cell("Gender"),
                ),
            ),
            rx.table.body(
                rx.foreach(State.users, show_user),
            ),
            variant="surface",
            size="3",
        ),
    )
```

Putting the Form in an Overlay

In Reflex, we like to make the user interaction as intuitive as possible. Placing the form we just constructed in an overlay creates a focused interaction by dimming the background, and ensures a cleaner layout when you have multiple action points such as editing and deleting as well.

We will place the form inside of a rx.dialog component (also called a modal). The rx.dialog.root contains all the parts of a dialog, and the rx.dialog.trigger wraps the control that will open the dialog. In our case the trigger will be an rx.button that says "Add User" as shown below.

```py
rx.dialog.trigger(
    rx.button(
        rx.icon("plus", size=26),
        rx.text("Add User", size="4"),
    ),
)
```

After the trigger we have the rx.dialog.content which contains everything within our dialog, including a title, a description and our form. The first way to close the dialog is without submitting the form and the second way is to close the dialog by submitting the form as shown below. This requires two rx.dialog.close components within the dialog.

```py
rx.dialog.close(
    rx.button(
        "Cancel",
        variant="soft",
        color_scheme="gray",
    ),
),
rx.dialog.close(
    rx.button("Submit", type="submit"),
)
```

The total code for the dialog with the form in it is below.

```py
rx.dialog.root(
    rx.dialog.trigger(
        rx.button(
            rx.icon("plus", size=26),
            rx.text("Add User", size="4"),
        ),
    ),
    rx.dialog.content(
        rx.dialog.title(
            "Add New User",
        ),
        rx.dialog.description(
            "Fill the form with the user's info",
        ),
        rx.form(
            # flex is similar to vstack and used to layout the form fields
            rx.flex(
                rx.input(
                    placeholder="User Name",
                    name="name",
                    required=True,
                ),
                rx.input(
                    placeholder="user@reflex.dev",
                    name="email",
                ),
                rx.select(
                    ["Male", "Female"],
                    placeholder="Male",
                    name="gender",
                ),
                rx.flex(
                    rx.dialog.close(
                        rx.button(
                            "Cancel",
                            variant="soft",
                            color_scheme="gray",
                        ),
                    ),
                    rx.dialog.close(
                        rx.button("Submit", type="submit"),
                    ),
                    spacing="3",
                    justify="end",
                ),
                direction="column",
                spacing="4",
            ),
            on_submit=State3.add_user,
            reset_on_submit=False,
        ),
        # max_width is used to limit the width of the dialog
        max_width="450px",
    ),
)
```

At this point we have an app that allows you to add users to a table by filling out a form. The form is placed in a dialog that can be opened by clicking the "Add User" button. We change the name of the component from form to add_customer_button and update this in our index component. The full app so far and code are below.

```py
class User(rx.Base):
    """The user model."""
    name: str
    email: str
    gender: str

class State(rx.State):
    users: list[User] = [
        User(
            name="Danilo Sousa",
            email="danilo@example.com",
            gender="Male",
        ),
        User(
            name="Zahra Ambessa",
            email="zahra@example.com",
            gender="Female",
        ),
    ]

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))

def show_user(user: User):
    """Show a person in a table row."""
    return rx.table.row(
        rx.table.cell(user.name),
        rx.table.cell(user.email),
        rx.table.cell(user.gender),
    )

def add_customer_button() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.trigger(
            rx.button(
                rx.icon("plus", size=26),
                rx.text("Add User", size="4"),
            ),
        ),
        rx.dialog.content(
            rx.dialog.title(
                "Add New User",
            ),
            rx.dialog.description(
                "Fill the form with the user's info",
            ),
            rx.form(
                rx.flex(
                    rx.input(
                        placeholder="User Name",
                        name="name",
                        required=True,
                    ),
                    rx.input(
                        placeholder="user@reflex.dev",
                        name="email",
                    ),
                    rx.select(
                        ["Male", "Female"],
                        placeholder="Male",
                        name="gender",
                    ),
                    rx.flex(
                        rx.dialog.close(
                            rx.button(
                                "Cancel",
                                variant="soft",
                                color_scheme="gray",
                            ),
                        ),
                        rx.dialog.close(
                            rx.button(
                                "Submit", type="submit"
                            ),
                        ),
                        spacing="3",
                        justify="end",
                    ),
                    direction="column",
                    spacing="4",
                ),
                on_submit=State.add_user,
                reset_on_submit=False,
            ),
            max_width="450px",
        ),
    )

def index() -> rx.Component:
    return rx.vstack(
        add_customer_button(),
        rx.table.root(
            rx.table.header(
                rx.table.row(
                    rx.table.column_header_cell("Name"),
                    rx.table.column_header_cell("Email"),
                    rx.table.column_header_cell("Gender"),
                ),
            ),
            rx.table.body(
                rx.foreach(State.users, show_user),
            ),
            variant="surface",
            size="3",
        ),
    )
```

Plotting Data in a Graph

The last part of this tutorial is to plot the user data in a graph. We will use Reflex's built-in graphing library recharts to plot the number of users of each gender.

Transforming the data for the graph
The graphing components in Reflex expect to take in a list of dictionaries. Each dictionary represents a data point on the graph and contains the x and y values. We will create a new event handler in the state called transform_data to transform the user data into the format that the graphing components expect. We must also create a new state variable called users_for_graph to store the transformed data, which will be used to render the graph.

```py
from collections import Counter

class State(rx.State):
    users: list[User] = []
    users_for_graph: list[dict] = []

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))
        self.transform_data()

    def transform_data(self):
        """Transform user gender group data into a format suitable for visualization in graphs."""
        # Count users of each gender group
        gender_counts = Counter(
            user.gender for user in self.users
        )

        # Transform into list of dict so it can be used in the graph
        self.users_for_graph = [
            {"name": gender_group, "value": count}
            for gender_group, count in gender_counts.items()
        ]
```

As we can see above the transform_data event handler uses the Counter class from the collections module to count the number of users of each gender. We then create a list of dictionaries from this which we set to the state var users_for_graph.

Finally we can see that whenever we add a new user through submitting the form and running the add_user event handler, we call the transform_data event handler to update the users_for_graph state variable.

Rendering the graph
We use the rx.recharts.bar_chart component to render the graph. We pass through the state variable for our graphing data as data=State.users_for_graph. We also pass in a rx.recharts.bar component which represents the bars on the graph. The rx.recharts.bar component takes in the data_key prop which is the key in the data dictionary that represents the y value of the bar. The stroke and fill props are used to set the color of the bars.

The rx.recharts.bar_chart component also takes in rx.recharts.x_axis and rx.recharts.y_axis components which represent the x and y axes of the graph. The data_key prop of the rx.recharts.x_axis component is set to the key in the data dictionary that represents the x value of the bar. Finally we add width and height props to set the size of the graph.

```py
def graph():
    return rx.recharts.bar_chart(
        rx.recharts.bar(
            data_key="value",
            stroke=rx.color("accent", 9),
            fill=rx.color("accent", 8),
        ),
        rx.recharts.x_axis(data_key="name"),
        rx.recharts.y_axis(),
        data=State.users_for_graph,
        width="100%",
        height=250,
    )
```

Finally we add this graph() component to our index() component so that the graph is rendered on the page. The code for the full app with the graph included is below. If you try this out you will see that the graph updates whenever you add a new user to the table.

```py
from collections import Counter

class State(rx.State):
    users: list[User] = [
        User(
            name="Danilo Sousa",
            email="danilo@example.com",
            gender="Male",
        ),
        User(
            name="Zahra Ambessa",
            email="zahra@example.com",
            gender="Female",
        ),
    ]
    users_for_graph: list[dict] = []

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))
        self.transform_data()

    def transform_data(self):
        """Transform user gender group data into a format suitable for visualization in graphs."""
        # Count users of each gender group
        gender_counts = Counter(
            user.gender for user in self.users
        )

        # Transform into list of dict so it can be used in the graph
        self.users_for_graph = [
            {"name": gender_group, "value": count}
            for gender_group, count in gender_counts.items()
        ]

def show_user(user: User):
    """Show a person in a table row."""
    return rx.table.row(
        rx.table.cell(user.name),
        rx.table.cell(user.email),
        rx.table.cell(user.gender),
    )

def add_customer_button() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.trigger(
            rx.button(
                rx.icon("plus", size=26),
                rx.text("Add User", size="4"),
            ),
        ),
        rx.dialog.content(
            rx.dialog.title(
                "Add New User",
            ),
            rx.dialog.description(
                "Fill the form with the user's info",
            ),
            rx.form(
                rx.flex(
                    rx.input(
                        placeholder="User Name",
                        name="name",
                        required=True,
                    ),
                    rx.input(
                        placeholder="user@reflex.dev",
                        name="email",
                    ),
                    rx.select(
                        ["Male", "Female"],
                        placeholder="male",
                        name="gender",
                    ),
                    rx.flex(
                        rx.dialog.close(
                            rx.button(
                                "Cancel",
                                variant="soft",
                                color_scheme="gray",
                            ),
                        ),
                        rx.dialog.close(
                            rx.button(
                                "Submit", type="submit"
                            ),
                        ),
                        spacing="3",
                        justify="end",
                    ),
                    direction="column",
                    spacing="4",
                ),
                on_submit=State.add_user,
                reset_on_submit=False,
            ),
            max_width="450px",
        ),
    )

def graph():
    return rx.recharts.bar_chart(
        rx.recharts.bar(
            data_key="value",
            stroke=rx.color("accent", 9),
            fill=rx.color("accent", 8),
        ),
        rx.recharts.x_axis(data_key="name"),
        rx.recharts.y_axis(),
        data=State.users_for_graph,
        width="100%",
        height=250,
    )

def index() -> rx.Component:
    return rx.vstack(
        add_customer_button(),
        rx.table.root(
            rx.table.header(
                rx.table.row(
                    rx.table.column_header_cell("Name"),
                    rx.table.column_header_cell("Email"),
                    rx.table.column_header_cell("Gender"),
                ),
            ),
            rx.table.body(
                rx.foreach(State.users, show_user),
            ),
            variant="surface",
            size="3",
        ),
        graph(),
    )
```

One thing you may have noticed about your app is that the graph does not appear initially when you run the app, and that you must add a user to the table for it to first appear. This occurs because the transform_data event handler is only called when a user is added to the table. In the next section we will explore a solution to this.

### Final Cleanup

Revisiting app.add_page

At the beginning of this tutorial we mentioned that the app.add_page function is required for every Reflex app. This function is used to add a component to a page.

The app.add_page currently looks like this app.add_page(index). We could change the route that the page renders on by setting the route prop such as route="/custom-route", this would change the route to http://localhost:3000/custom-route for this page.

We can also set a title to be shown in the browser tab and a description as shown in search results.

To solve the problem we had above about our graph not loading when the page loads, we can use on_load inside of app.add_page to call the transform_data event handler when the page loads. This would look like on_load=State.transform_data. Below see what our app.add_page would look like with some of the changes above added.

```py
app.add_page(
    index,
    title="Customer Data App",
    description="A simple app to manage customer data.",
    on_load=State.transform_data,
)
```

Revisiting app=rx.App()

At the beginning of the tutorial we also mentioned that we defined our app using app=rx.App(). We can also pass in some props to the rx.App component to customize the app.

The most important one is theme which allows you to customize the look and feel of the app. The theme prop takes in an rx.theme component which has several props that can be set.

The radius prop sets the global radius value for the app that is inherited by all components that have a radius prop. It can be overwritten locally for a specific component by manually setting the radius prop.

The accent_color prop sets the accent color of the app. Check out other options for the accent color here.

To see other props that can be set at the app level check out this documentation

```py
app = rx.App(
    theme=rx.theme(radius="full", accent_color="grass"),
)
```

Unfortunately in this tutorial here we cannot actually apply this to the live example on the page, but if you copy and paste the code below into a reflex app locally you can see it in action.

## Conclusion

Finally let's make some final styling updates to our app. We will add some hover styling to the table rows and center the table inside the show_user with style=\{"_hover": {"bg": rx.color("gray", 3)}}, align="center".

In addition, we will add some width="100%" and align="center" to the index() component to center the items on the page and ensure they stretch the full width of the page.

Check out the full code and interactive app below:

```py
import reflex as rx
from collections import Counter

class User(rx.Base):
    """The user model."""

    name: str
    email: str
    gender: str

class State(rx.State):
    users: list[User] = [
        User(
            name="Danilo Sousa",
            email="danilo@example.com",
            gender="Male",
        ),
        User(
            name="Zahra Ambessa",
            email="zahra@example.com",
            gender="Female",
        ),
    ]
    users_for_graph: list[dict] = []

    def add_user(self, form_data: dict):
        self.users.append(User(**form_data))
        self.transform_data()

    def transform_data(self):
        """Transform user gender group data into a format suitable for visualization in graphs."""
        # Count users of each gender group
        gender_counts = Counter(
            user.gender for user in self.users
        )

        # Transform into list of dict so it can be used in the graph
        self.users_for_graph = [
            {"name": gender_group, "value": count}
            for gender_group, count in gender_counts.items()
        ]

def show_user(user: User):
    """Show a user in a table row."""
    return rx.table.row(
        rx.table.cell(user.name),
        rx.table.cell(user.email),
        rx.table.cell(user.gender),
        style={"_hover": {"bg": rx.color("gray", 3)}},
        align="center",
    )

def add_customer_button() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.trigger(
            rx.button(
                rx.icon("plus", size=26),
                rx.text("Add User", size="4"),
            ),
        ),
        rx.dialog.content(
            rx.dialog.title(
                "Add New User",
            ),
            rx.dialog.description(
                "Fill the form with the user's info",
            ),
            rx.form(
                rx.flex(
                    rx.input(
                        placeholder="User Name",
                        name="name",
                        required=True,
                    ),
                    rx.input(
                        placeholder="user@reflex.dev",
                        name="email",
                    ),
                    rx.select(
                        ["Male", "Female"],
                        placeholder="male",
                        name="gender",
                    ),
                    rx.flex(
                        rx.dialog.close(
                            rx.button(
                                "Cancel",
                                variant="soft",
                                color_scheme="gray",
                            ),
                        ),
                        rx.dialog.close(
                            rx.button(
                                "Submit", type="submit"
                            ),
                        ),
                        spacing="3",
                        justify="end",
                    ),
                    direction="column",
                    spacing="4",
                ),
                on_submit=State.add_user,
                reset_on_submit=False,
            ),
            max_width="450px",
        ),
    )

def graph():
    return rx.recharts.bar_chart(
        rx.recharts.bar(
            data_key="value",
            stroke=rx.color("accent", 9),
            fill=rx.color("accent", 8),
        ),
        rx.recharts.x_axis(data_key="name"),
        rx.recharts.y_axis(),
        data=State.users_for_graph,
        width="100%",
        height=250,
    )

def index() -> rx.Component:
    return rx.vstack(
        add_customer_button(),
        rx.table.root(
            rx.table.header(
                rx.table.row(
                    rx.table.column_header_cell("Name"),
                    rx.table.column_header_cell("Email"),
                    rx.table.column_header_cell("Gender"),
                ),
            ),
            rx.table.body(
                rx.foreach(State.users, show_user),
            ),
            variant="surface",
            size="3",
            width="100%",
        ),
        graph(),
        align="center",
        width="100%",
    )

app = rx.App(
    theme=rx.theme(radius="full", accent_color="grass"),
)

app.add_page(
    index,
    title="Customer Data App",
    description="A simple app to manage customer data.",
    on_load=State.transform_data,
)
```

And that is it for your first dashboard tutorial. In this tutorial we have created

- a table to display user data
- a form to add new users to the table
- a dialog to showcase the form
- a graph to visualize the user data

In addition to the above we have we have

- explored state to allow you to show dynamic data that changes over time
- explored events to allow you to make your app interactive and respond to user actions
- added styling to the app to make it look better
