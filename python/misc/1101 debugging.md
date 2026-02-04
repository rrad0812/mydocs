
# Dodavanje remote attach debugging

- ne smeta normalnom radu,
- možeš ga **upaliti ili ugasiti po potrebi**, bez menjanja koda,
- radi i lokalno i ako server dižeš na drugoj mašini.

## Plug&play za `run_jam.py`

Ubaci na sam vrh fajla **run_jam**, posle `import sys`, recimo ovako:

```python
import os
import sys

# --- Remote debugging (optional) ---
if os.getenv("JAM_REMOTE_DEBUG") == "1":
    try:
        import debugpy
        host = os.getenv("JAM_DEBUG_HOST", "0.0.0.0")
        port = int(os.getenv("JAM_DEBUG_PORT", "5678"))
        debugpy.listen((host, port))
        print(f"Debugger listening on {host}:{port} ...")
        if os.getenv("JAM_DEBUG_WAIT", "0") == "1":
            print("Waiting for debugger to attach...")
            debugpy.wait_for_client()
            print("Debugger attached, continuing...")
    except Exception as e:
        print(f"[WARN] Remote debug setup failed: {e}")
```

## Kako se koristi

**1. Lokalno pokretanje (bez remote debug-a):**

Normalno:

```bash
python run_jam.py
```

→ radi bez ikakvog uticaja.

**2. Pokretanje sa remote debugging-om:**

Uključi ga **privremeno**:

```sh
export JAM_REMOTE_DEBUG=1
python run_jam.py
```

**Rezultat**:

```sh
Debugger listening on 0.0.0.0:5678 ...
```

Ako želiš da server *sačeka dok se ne priključiš*, dodaj i:

```sh
export JAM_DEBUG_WAIT=1
```

Tada će stajati na liniji:

```sh
Waiting for debugger to attach...
```

dok VS Code ne napravi konekciju.

**3. U VS Code-u napravi ovaj attach profil**:

```json
{
    "name": "Attach to JamPy (remote)",
    "type": "python",
    "request": "attach",
    "connect": {
        "host": "127.0.0.1",
        "port": 5678
    },
    "justMyCode": false
}
```

Ako se kačiš na udaljeni server (Docker, VM, produkcija), promeni `"host"` na IP tog sistema — ili koristi `ssh -L` forwarding kao:

```bash
ssh -L 5678:localhost:5678 user@remote
```

**4. I onda samo…**:

- Pokreneš server s `JAM_REMOTE_DEBUG=1`,
- u VS Code-u klikneš **Run → Attach to JamPy (remote)**,
- i radiš **kao da je lokalno** — breakpointi, variables, call stack, sve.

**5. Dodatni trik (ako koristiš VS Code tasks)**:

Ako hoćeš da ne kucaš export svaki put, možeš dodati u `launch.json` i varijable:

```json
"env": {
    "JAM_REMOTE_DEBUG": "1",
    "JAM_DEBUG_WAIT": "1"
}
```

Tako VS Code automatski aktivira remote debug svaki put kad startuješ.

## Ukratko

| Varijabla            | Šta radi                         |
| -------------------- | -------------------------------- |
| `JAM_REMOTE_DEBUG=1` | pali debugpy listener            |
| `JAM_DEBUG_HOST`     | IP adresa (default: `0.0.0.0`)   |
| `JAM_DEBUG_PORT`     | port (default: `5678`)           |
| `JAM_DEBUG_WAIT=1`   | pauzira dok se debugger ne spoji |

Ovo je praktično “šalter” — pali/gasi remote debug bez menjanja koda.

Možeš ga čak ostaviti i u produkciji, samo bez aktiviranja — neće praviti nikakav overhead.

## Tri zlatna pravila za debugging

**1. Debugiraj tamo gde si — ne gde se desilo**:

Kad vidiš da se nešto ponaša čudno, ne trči odmah u srce frameworka. Kreni iz svog koda — modela, hooka, taska — i postavi breakpoint ili log baš tamo gde ti sumnjaš.

Jam.py (kao i svaki server framework) ima puno slojeva, ali sve prođe kroz tvoju logiku.

**Primer:**
Ako ti `on_insert` ne radi, ne kopaj odmah po `wsgi_server.py`. Stavi `logger.debug()` ili breakpoint u svoj `on_insert` i vidi da li se uopšte poziva.

**2. Print ubija, logging spašava**:

`print()` je kao cigla — leti brzo, ali pravi haos.
`logging` ti daje:

- timestamp,
- ime fajla i linije,
- nivo poruke (INFO, DEBUG, ERROR),
- i možeš ga filtrirati kad tražiš greške.

**Zlatni recept:**

```python
logger.debug("Ulazim u funkciju x sa param: %s", param)
logger.error("Puko upis: %s", e, exc_info=True)
```

Umesto beskonačnih printova — imaš trag koji ostaje i posle restarta.

**3. Debugger nije čarobnjak, nego lupa**:

Koristi **debugger (VS Code, debugpy)** da vidiš šta se stvarno dešava u runtime-u, ne da “popravljaš” grešku iznutra.

Gledaj vrednosti promenljivih, sledi korake, i pitaj se:

> “Da li ovaj deo radi ono što ja mislim da radi?”

**Pro tip:**
Kad naiđeš na misterioznu grešku, **ne pokušavaj odmah fix** —
već proveri da li tvoj mentalni model i realnost koda uopšte odgovaraju.

Debugger je tvoj “rentgen”, ne hirurg.

**4. BONUS — pravilo iz garaže**:

> Ako ništa drugo ne radi, napravi pauzu i popij kafu.
>
> Kad se vratiš, bug više neće imati gde da se sakrije.
