
# Event manager u pajtonu

## Uopšteno o event menadžeru

Ako sam te dobro razumeo:

- Hoćeš da zadržiš Jam.py framework (server, ORM, modeli itd.),
- ali da zameniš njegov postojeći mehanizam za rukovanje UI događajima (klik, tasteri, prečice) verovatno deo koji sada koristi *on_event, on_click, ili client_action* mehaniku.

To znači da:

- nećeš dirati core engine ni REST sloj,
- nego želiš *novi event dispatcher* koji bolje reaguje i lakše se proširuje.

**Šta predstavlja “custom event manager” u tom kontekstu**:

Pod “custom event managerom” (tvoja tačka 3.) misli se na klasu koja:

1. Registruje event handlere,
2. Čuva ih u mapi po imenu ili tipu događaja,
3. Emituje (dispatchuje) događaje asinhrono ili sinhrono.

Minimalan primer:

```python
class EventManager:
    def __init__(self):
        self.listeners = {}

    def on(self, event_name, func):
        self.listeners.setdefault(event_name, []).append(func)

    def emit(self, event_name, *args, **kwargs):
        for func in self.listeners.get(event_name, []):
            func(*args, **kwargs)
```

Onda možeš:

```python
em = EventManager()

def on_click(button):
    print(f"Clicked {button}")

em.on("click", on_click)
em.emit("click", "save_button")
```

**Kako to možeš spojiti sa Jam.py**:

Jam.py ima backend evente (na modelima, datasetima, server-side), ali tvoj slučaj je *frontend interaktivnost*.

Ako hoćeš da to rešiš na nivou browsera, imaš tri opcije:

- **JS event manager u client delu** — interceptuješ klikove i keydown evente i šalješ ih serveru putem websocket-a ili HTTP.
(Malo više rada, ali najbrže reaguje.)

- **Python event dispatcher na serveru** — koristiš WebSocket vezu i tvoj `EventManager` emituje dogadjaje ka handlerima. (Idealno ako već imaš socket bridge.)

- **Hibridno** — lagani JS “input forwarder” + tvoj `EventManager` u Pythona koji odlučuje *šta dalje*.

**“Instant” reakcija na klik/tastere**:

Onda ti preporučujem da napraviš **mini event broker** (poput iznad) i povežeš ga sa frontendom preko `websocket` (recimo `aiohttp.web` ili `FastAPI` + `WebSocketRoute`).

Recimo:

```python
import asyncio
from fastapi import FastAPI, WebSocket

app = FastAPI()
em = EventManager()

@app.websocket("/events")
async def websocket_endpoint(ws: WebSocket):
    await ws.accept()
    while True:
        data = await ws.receive_json()
        em.emit(data["event"], data.get("payload"))
```

Sad iz frontenda (JS) samo:

```js
ws.send(JSON.stringify({event: "click", payload: "save_button"}));
```

**Zaključak**:

Ako hoćeš da Jam.py zadržiš, ali preuzmeš kontrolu nad input eventima:

- napravi lagani event dispatcher (kao iznad),
- ubaci ga u Jam.py kao servis (modul koji se importuje),
- frontendu dodaj mali JS bridge (1-2 funkcije),
- i dobiješ potpuno nezavisan event loop — bez čekanja backend load-a.

## Mini prototip event sistem

Pokazaću ti **mini-prototip event sistema** koji možeš ubaciti u Jam.py *client* sloj (JavaScript) i povezati sa *serverom* (Python).

Ideja je da *klikovi i tasteri* idu kroz tvoj **custom EventManager** umesto kroz Jam.py built-in `client_event` mehanizam.

**Frontend deo — “Event Forwarder”**:

Ovo ide u `static/js/event_bridge.js` (ili da ga ubaciš u `app.js` ako već imaš).

```js
// event_bridge.js
const ws = new WebSocket(`ws://${window.location.host}/events`);

ws.onopen = () => console.log("Event socket connected");

function sendEvent(event, payload = {}) {
  if (ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify({ event, payload }));
  }
}

// --- Klikovi ---
document.addEventListener("click", e => {
  const target = e.target.closest("[data-event]");
  if (target) {
    sendEvent("click", { id: target.dataset.event, x: e.clientX, y: e.clientY });
  }
});

// --- Tastatura ---
document.addEventListener("keydown", e => {
  sendEvent("key", { key: e.key, ctrl: e.ctrlKey, shift: e.shiftKey });
});
```

Svakom HTML elementu dodaš `data-event="save_button"`, i sistem sam šalje event serveru čim klikneš ili pritisneš taster.

**Backend deo — “EventManager” u Pythonu**:

U `event_manager.py` (u Jam.py root folderu):

```python
class EventManager:
    def __init__(self):
        self.listeners = {}

    def on(self, event_name, func):
        self.listeners.setdefault(event_name, []).append(func)

    def emit(self, event_name, *args, **kwargs):
        for func in self.listeners.get(event_name, []):
            func(*args, **kwargs)


# Globalni event manager
em = EventManager()
```

**3. WebSocket endpoint**:

Ako koristiš Jam.py standalone (tj. imaš `run_jam.py`), možeš ubaciti ovo uz `FastAPI` (ili `aiohttp`).

Na primer u `run_jam.py`:

```python
import json
from fastapi import FastAPI, WebSocket
from event_manager import em

app = FastAPI()

@app.websocket("/events")
async def events_socket(ws: WebSocket):
    await ws.accept()
    while True:
        data = await ws.receive_text()
        event = json.loads(data)
        em.emit(event["event"], event["payload"])
```

**4. Registracija handlera**:

Negde u tvom startup kodu (npr. `app_init.py`):

```python
from event_manager import em

def on_click(payload):
    print("Klik:", payload)

def on_key(payload):
    print("Taster:", payload)

em.on("click", on_click)
em.on("key", on_key)
```

Kad klikneš element sa `data-event="save_button"`, u server logu ćeš dobiti:

```sh
Key: {'id': 'save_button', 'x': 521, 'y': 184}
```

A pritisak tastera:

```sh
Taster: {'key': 's', 'ctrl': True, 'shift': False}
```

**5. Šta si time dobio**:

- Potpuno *nezavisan event pipeline* (ne koristi Jam.py `client_event`)
- Reaguje *trenutno*, jer WebSocket šalje async
- Možeš sam da menjaš, blokiraš, filtriraš događaje
- Ne zavisi od backend load-a (osim ako ne preopteretiš event petlju)
- Možeš da implementiraš *key mappings* i *macro akcije*

**6. Bonus: event prioritizacija i async**:

Ako želiš da handleri budu asinhroni i da podržavaju prioritete:

```python
class EventManager:
    def __init__(self):
        self.listeners = {}

    def on(self, event_name, func, priority=0):
        self.listeners.setdefault(event_name, []).append((priority, func))
        self.listeners[event_name].sort(key=lambda x: -x[0])  # veći prioritet prvi

    async def emit(self, event_name, *args, **kwargs):
        for _, func in self.listeners.get(event_name, []):
            if asyncio.iscoroutinefunction(func):
                await func(*args, **kwargs)
            else:
                func(*args, **kwargs)
```

## Integracija

**1. Gde se Jam.py “kači” na evente**:

Jam.py ima nekoliko tačaka gde eventi ulaze:

1. **Frontend** (JS u browseru) — šalje `ajax` zahtev ili `socket.emit` nazad serveru.
2. **Server** — `server.py` ima *event router* koji traži handler u modelima (`on_event`, `on_click`, `before_post`, …).
3. **Dataset / form handler** — poziva prave Python metode.

Mi ćemo *preskočiti* njegov event pipeline i ubaciti naš — tako da *mi hvatamo klik/taster*, a ako želimo — prosledimo Jam.py-u (ili ne).

**2. Povezivanje `EventManager`-a sa Jam formama**:

Pretpostavimo da imaš Jam.py formu, npr. `customers`. U njoj postoji dugme sa `data-event="save_button"`.

**Dodaj “adapter”**:

U `event_adapter.py`:

```python
from event_manager import em
from jam import app

def trigger_jam_action(form_name, action_name, **payload):
    form = app.project.forms.get(form_name)
    if not form:
        print(f"[EventAdapter] Form {form_name} not found")
        return
    action = getattr(form, action_name, None)
    if callable(action):
        print(f"[EventAdapter] Triggering {form_name}.{action_name}()")
        action(**payload)
    else:
        print(f"[EventAdapter] Action {action_name} not found on {form_name}")
```

Sad možeš povezati našeg menadžera sa Jam-om:

```python
em.on("click", lambda payload: trigger_jam_action("customers", payload["id"], **payload))
```

Kad klikneš element sa `data-event="save_button"`, ako `customers` forma ima `save_button()` metodu, ona se poziva.

**3. Povezivanje tastera (keyboard shortcuts)**:

Recimo da želiš da pritisak `Ctrl+S` pozove istu akciju:

```python
def key_handler(payload):
    if payload["ctrl"] and payload["key"].lower() == "s":
        trigger_jam_action("customers", "save_button")

em.on("key", key_handler)
```

Sad ti `Ctrl+S` u bilo kom delu aplikacije aktivira `customers.save_button()`.

**4. Integracija u Jam.py lifecycle**:

Jam.py ima `before_run()` i `after_run()` hookove — koristi `after_run()` u `run_jam.py`:

```python
from event_manager import em
from event_adapter import trigger_jam_action

def after_run():
    em.on("click", lambda p: trigger_jam_action("customers", p["id"], **p))
    em.on("key", key_handler)
```

Sad sve funkcioniše *odmah po startu servera*, bez menjanja core Jam.py koda.

**5. Kako to radi u praksi**:

- Tvoj frontend `event_bridge.js` hvata klik/taster,
- Šalje ga na `/events` (WebSocket),
- `EventManager` ga prima i emituje,
- `event_adapter` prevodi ime događaja u Jam.py formu i akciju,
- poziva odgovarajuću Python metodu.

Rezultat:

- Klik na dugme → tvoj handler → Jam.py forma radi kao da si kliknuo iznutra
- Tastatura (Ctrl+S, Enter itd.) → direktna Jam.py akcija
- Nema čekanja na Jam dispatch → sve asinhrono i momentalno

**6. Prednost ovog sistema**:

- Možeš da registruješ više listenera po događaju (logovanje, audit, statistika).
- Možeš da “blokiraš” Jam akciju (ako validator kaže da ne treba).
- Možeš da menjaš ponašanje UI-a bez menjanja core koda.
- Kasnije lako dodaš “context menu”, “double-click”, “drag” i slične evente.

## Prioriteti i asinhrona obrada

Evo sledećeg koraka, da tvoj `EventManager` postane *ozbiljan sistem* koji podržava:

- prioritete (`priority` argument)
- asinhrone handlere (`async def ...`)
- “event chain” — gde handler može da *zaustavi propagaciju* (kao u Qt / browser eventima)

**1. Napredni `EventManager` (drop-in zamena)**

U `event_manager.py` samo zameni staru klasu ovim kodom:

```python
import asyncio
import inspect

class EventManager:
    def __init__(self):
        # event_name -> list of (priority, func)
        self.listeners = {}

    def on(self, event_name, func=None, *, priority=0):
        """Decorator-style ili direktno registrovanje."""
        if func is None:
            def wrapper(f):
                self.on(event_name, f, priority=priority)
                return f
            return wrapper

        self.listeners.setdefault(event_name, []).append((priority, func))
        # sortira po prioritetu (veći prvi)
        self.listeners[event_name].sort(key=lambda x: -x[0])

    async def emit(self, event_name, *args, **kwargs):
        """Poziva listenere po prioritetu. Ako handler vrati False — prekida lanac."""
        if event_name not in self.listeners:
            return

        for _, func in self.listeners[event_name]:
            result = None
            if inspect.iscoroutinefunction(func):
                result = await func(*args, **kwargs)
            else:
                result = func(*args, **kwargs)
            # Ako handler vrati False, prekidamo dalju obradu
            if result is False:
                break
```

**2. Primer upotrebe**:

```python
from event_manager import EventManager

em = EventManager()

# običan sync handler
@em.on("click", priority=10)
def log_click(payload):
    print("[LOG]", payload)

# async handler sa manjim prioritetom
@em.on("click", priority=5)
async def handle_click(payload):
    await asyncio.sleep(0.1)
    print("[ASYNC]", payload)
    if payload.get("id") == "cancel_button":
        print("Prekidam lanac događaja.")
        return False  # prekida emitovanje dalje

async def test():
    await em.emit("click", {"id": "save_button"})
    await em.emit("click", {"id": "cancel_button"})

asyncio.run(test())
```

Izlaz:

```sh
[LOG] {'id': 'save_button'}
[ASYNC] {'id': 'save_button'}
[LOG] {'id': 'cancel_button'}
[ASYNC] {'id': 'cancel_button'}
Prekidam lanac događaja.
```

**3. Integracija u Jam.py**:

Tvoj `events_socket` endpoint (iz ranijeg primera) sada treba da koristi `await`:

```python
@app.websocket("/events")
async def events_socket(ws: WebSocket):
    await ws.accept()
    while True:
        data = await ws.receive_json()
        await em.emit(data["event"], data["payload"])
```

To znači da će Jam.py backend sada moći da:

- **obradi evente asinhrono**
- **poštuje prioritete**
- **prekine lanac** kad treba (npr. validator ili filter vrati `False`)

**4. Primer integracije s Jam.py formom**:

```python
from event_manager import em
from event_adapter import trigger_jam_action

@em.on("click", priority=100)
def guard(payload):
    if payload.get("id") == "delete_button":
        print("Provera dozvole za brisanje…")
        # ako korisnik nema pravo:
        # return False  # stop lanac

@em.on("click", priority=10)
def forward_to_jam(payload):
    trigger_jam_action("customers", payload["id"], **payload)
```

Sad imaš pun sistem: `guard` proverava, `forward_to_jam` reaguje, i sve radi paralelno (async-safe).

---

**5. U praksi**:

- Radi asinhrono — ne blokira server
- Ima prioritete
- Možeš da “progutaš” događaj (return False)
- Možeš da registruješ `@em.on("event")` iz bilo kog modula
- Tvoj UI sada može da reaguje instantno i bez zagušenja

## Context-aware event sistem

Svaki *form*, *view*, *komponenta* ili *sub-modul* može da ima **svoj mali EventManager**, koji je vezan za globalni (`em`), ali nezavisan.

Ovo ti daje:

- izolaciju događaja (npr. `customers.on("click")` ne smeta `orders.on("click")`),
- lakše testiranje,
- i mogućnost da lokalni event “progutaš” pre nego što ide dalje (kao *event bubbling* u browseru).

**1. Novi fajl: `scoped_event_manager.py`**

```python
import asyncio
import inspect

class EventManager:
    def __init__(self, name=None, parent=None):
        self.name = name
        self.parent = parent
        self.listeners = {}

    def on(self, event_name, func=None, *, priority=0):
        """Decorator-style ili direktno registrovanje."""
        if func is None:
            def wrapper(f):
                self.on(event_name, f, priority=priority)
                return f
            return wrapper

        self.listeners.setdefault(event_name, []).append((priority, func))
        self.listeners[event_name].sort(key=lambda x: -x[0])

    async def emit(self, event_name, *args, **kwargs):
        """Poziva local listenere, pa opcionalno bubble-uje parentu."""
        if event_name in self.listeners:
            for _, func in self.listeners[event_name]:
                result = await self._call(func, *args, **kwargs)
                if result is False:
                    # prekida lanac — ne ide ni ka parentu
                    return False

        # ako nije zaustavljeno i postoji parent — bubble
        if self.parent:
            await self.parent.emit(event_name, *args, **kwargs)

    async def _call(self, func, *args, **kwargs):
        if inspect.iscoroutinefunction(func):
            return await func(*args, **kwargs)
        else:
            return func(*args, **kwargs)

    def create_scope(self, name):
        """Pravi child EventManager koji bubble-uje ka ovom."""
        return EventManager(name=name, parent=self)
```

**2. Upotreba u Jam.py kontekstu**:

U `event_manager.py` (ili `run_jam.py`) definiši **globalni event menadžer**:

```python
from scoped_event_manager import EventManager

em = EventManager(name="root")
```

A za svaku formu možeš da napraviš njen “scope”:

```python
customers_events = em.create_scope("customers")
orders_events = em.create_scope("orders")
```

**3. Registracija formi**:

```python
@customers_events.on("click")
def on_customers_click(payload):
    print("[customers] Klik:", payload)
    if payload.get("id") == "delete_button":
        print("Brisanje u toku…")
        return False  # blokira bubble ka globalu

@em.on("click")
def global_click(payload):
    print("[global] Klik:", payload)
```

Ako sada uradiš:

```python
await customers_events.emit("click", {"id": "delete_button"})
```

dobiješ:

```sh
[customers] Klik: {'id': 'delete_button'}
Brisanje u toku…
```

I tu se lanac zaustavlja (ne ide do globalnog `em`).

Ali ako uradiš:

```python
await customers_events.emit("click", {"id": "save_button"})
```

onda:

```sh
[customers] Klik: {'id': 'save_button'}
[global] Klik: {'id': 'save_button'}
```

To je “bubbling” ponašanje kao u DOM-u — savršeno za Jam forme.

**4. Povezivanje s tvojim adapterom**:

Sad možeš da zoveš:

```python
def connect_form_events(form_name):
    form_em = em.create_scope(form_name)

    @form_em.on("click")
    def forward(payload):
        trigger_jam_action(form_name, payload["id"], **payload)

    return form_em
```

I u `after_run()`:

```python
customers_events = connect_form_events("customers")
orders_events = connect_form_events("orders")
```

Frontend (kao i pre) samo šalje:

```js
sendEvent("click", { id: "save_button", form: "customers" });
```

A backend radi:

```python
await locals()[f"{payload['form']}_events"].emit("click", payload)
```

**5. Dobijaš sistem kao u Qt ili Reactu**:

- Lokalni scope za svaki modul ili formu
- Event bubbling ka parentu (root `em`)
- Mogućnost blokiranja propagacije
- Radi i sa async handlerima
- Zero dependency — čista Python logika

## Registracija

**1. Ideja**:

Jam.py već zna sve forme u projektu:

```python
app.project.forms  # dict: {"customers": <Form>, "orders": <Form>, ...}
```

Zato ćemo:

1. Proći kroz sve forme,
2. Napraviti za svaku lokalni event scope (`form_events[form_name]`),
3. Uvezati osnovne evente (`click`, `key`, itd.) automatski,
4. Dodati referencu `form.events = form_events[form_name]`, tako da se može koristiti i direktno iz forme.

**2. Novi fajl: `event_autoreg.py`**:

```python
from scoped_event_manager import EventManager
from event_adapter import trigger_jam_action

# global root event manager
em = EventManager(name="root")

# registri za forme
form_events = {}

def auto_register_forms(app):
    """
    Automatski prolazi kroz sve Jam forme i kreira im local event scope.
    """
    for form_name, form in app.project.forms.items():
        form_em = em.create_scope(form_name)
        form_events[form_name] = form_em
        form.events = form_em  # opciono, da možeš raditi form.events.on(...)

        # univerzalni click listener
        @form_em.on("click")
        def on_click(payload, form_name=form_name):
            trigger_jam_action(form_name, payload["id"], **payload)

        # key handler, ako želiš
        @form_em.on("key")
        def on_key(payload, form_name=form_name):
            key = payload.get("key")
            if payload.get("ctrl") and key.lower() == "s":
                trigger_jam_action(form_name, "save_button")

    print(f"[EventAutoReg] Registered {len(form_events)} form event scopes.")
```

**3. Poziv u `run_jam.py`**

Samo dodaj ispod `app.run()`:

```python
from event_autoreg import auto_register_forms, em

def after_run():
    auto_register_forms(app)
```

I naravno, koristi `await em.emit(...)` u svom WebSocket endpointu.

**4. Šta se sada događa**:

Kada Jam.py startuje:

1. `auto_register_forms(app)` pregleda sve forme u projektu;

2. Svaka dobija **svoj EventManager scope** (`form.events`);

3. Ako frontend pošalje event sa `"form": "customers"`, backend zna tačno gde da ga pošalje:

   ```python
   form_name = payload.get("form")
   await form_events[form_name].emit("click", payload)
   ```

4. Ako forma ne “proguta” event, on ide ka globalnom `em` kao fallback.

**5. Bonus — globalni hook**:

Možeš da dodaš “default handler” koji sluša sve evente (za logovanje, telemetry, itd.):

```python
@em.on("click")
def global_click_log(payload):
    print(f"[Global] Click on {payload.get('form')}:{payload.get('id')}")
```

**6. Rezultat**:

- **Sve forme se automatski registruju**
- **Eventi se šalju u odgovarajući lokalni scope**
- **Moguće blokirati propagaciju ili async-izovati**
- **Možeš dinamički dodavati ili skidati listenere po formi**
- **Jam.py ostaje netaknut** — sve kroz plug-in sloj

## Sve kompletno

Evo kompletnog “plug-and-play” paketa — možeš ga ubaciti kao folder `jam_events/` u tvoj Jam.py projekat (pored `run_jam.py`) i samo importovati.

**Struktura paketa**:

```sh
jam_events/
│
├── __init__.py
├── event_manager.py
├── scoped_event_manager.py
├── event_adapter.py
└── event_autoreg.py
```

**`__init__.py`**:

```python
from .event_autoreg import auto_register_forms, em, form_events
__all__ = ["auto_register_forms", "em", "form_events"]
```

**`scoped_event_manager.py`**:

```python
import asyncio
import inspect

class EventManager:
    def __init__(self, name=None, parent=None):
        self.name = name
        self.parent = parent
        self.listeners = {}

    def on(self, event_name, func=None, *, priority=0):
        if func is None:
            def wrapper(f):
                self.on(event_name, f, priority=priority)
                return f
            return wrapper

        self.listeners.setdefault(event_name, []).append((priority, func))
        self.listeners[event_name].sort(key=lambda x: -x[0])

    async def emit(self, event_name, *args, **kwargs):
        if event_name in self.listeners:
            for _, func in self.listeners[event_name]:
                result = await self._call(func, *args, **kwargs)
                if result is False:
                    return False
        if self.parent:
            await self.parent.emit(event_name, *args, **kwargs)

    async def _call(self, func, *args, **kwargs):
        if inspect.iscoroutinefunction(func):
            return await func(*args, **kwargs)
        return func(*args, **kwargs)

    def create_scope(self, name):
        return EventManager(name=name, parent=self)
```

**`event_manager.py`**:

Ako hoćeš samo globalni, bez scope-a:

```python
import asyncio
import inspect

class EventManager:
    def __init__(self):
        self.listeners = {}

    def on(self, event_name, func=None, *, priority=0):
        if func is None:
            def wrapper(f):
                self.on(event_name, f, priority=priority)
                return f
            return wrapper

        self.listeners.setdefault(event_name, []).append((priority, func))
        self.listeners[event_name].sort(key=lambda x: -x[0])

    async def emit(self, event_name, *args, **kwargs):
        if event_name not in self.listeners:
            return
        for _, func in self.listeners[event_name]:
            result = await self._call(func, *args, **kwargs)
            if result is False:
                break

    async def _call(self, func, *args, **kwargs):
        if inspect.iscoroutinefunction(func):
            return await func(*args, **kwargs)
        return func(*args, **kwargs)
```

**`event_adapter.py`**:

```python
from jam import app

def trigger_jam_action(form_name, action_name, **payload):
    form = app.project.forms.get(form_name)
    if not form:
        print(f"[EventAdapter] Form {form_name} not found")
        return
    action = getattr(form, action_name, None)
    if callable(action):
        print(f"[EventAdapter] Triggering {form_name}.{action_name}()")
        action(**payload)
    else:
        print(f"[EventAdapter] Action {action_name} not found on {form_name}")
```

**`event_autoreg.py`**:

```python
from .scoped_event_manager import EventManager
from .event_adapter import trigger_jam_action

em = EventManager(name="root")
form_events = {}

def auto_register_forms(app):
    """
    Automatski kreira event scope za svaku Jam.py formu.
    """
    for form_name, form in app.project.forms.items():
        form_em = em.create_scope(form_name)
        form_events[form_name] = form_em
        form.events = form_em

        @form_em.on("click")
        def on_click(payload, form_name=form_name):
            trigger_jam_action(form_name, payload["id"], **payload)

        @form_em.on("key")
        def on_key(payload, form_name=form_name):
            key = payload.get("key")
            if payload.get("ctrl") and key.lower() == "s":
                trigger_jam_action(form_name, "save_button")

    print(f"[EventAutoReg] Registered {len(form_events)} form event scopes.")
```

## Integracija u `run_jam.py`

```python
from fastapi import FastAPI, WebSocket
from jam_events import auto_register_forms, em, form_events

app = FastAPI()

@app.on_event("startup")
async def startup():
    from jam import app as jam_app
    auto_register_forms(jam_app)
    print("[jam_events] Initialized all form event scopes.")

@app.websocket("/events")
async def events_socket(ws: WebSocket):
    await ws.accept()
    while True:
        data = await ws.receive_json()
        event = data["event"]
        payload = data.get("payload", {})
        form = payload.get("form")
        if form and form in form_events:
            await form_events[form].emit(event, payload)
        else:
            await em.emit(event, payload)
```

## Frontend bridge (isti kao ranije)

```js
const ws = new WebSocket(`ws://${window.location.host}/events`);

document.addEventListener("click", e => {
  const target = e.target.closest("[data-event]");
  if (target) {
    sendEvent("click", { form: target.dataset.form, id: target.dataset.event });
  }
});

document.addEventListener("keydown", e => {
  sendEvent("key", { key: e.key, ctrl: e.ctrlKey, shift: e.shiftKey });
});

function sendEvent(event, payload = {}) {
  if (ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify({ event, payload }));
  }
}
```

## Šta sada imaš

- **Potpuno modularan paket** (`jam_events/`) — nema potrebe dirati Jam.py core.

- Automatska registracija formi i local event scope-ova.

- Event bubbling, async handleri, prioritizacija.

- Možeš direktno u formi pisati:

  ```python
  @form.events.on("click")
  def handle_form_click(payload):
      ...
  ```
