
# Kako `logging` zapravo radi

Kada prvi put uradiš (u procesu):

```python
import logging
```

dobiješ pristup **globalnom logger sistemu** iz standardne Python biblioteke.
Nije obična klasa - to je **singleton** koji drži zajedničku konfiguraciju za ceo proces.

Znači:

- Kad u `run_jam.py` pozoveš `logging.basicConfig(...)` ili dodaš handlere,
  ti konfigurišeš **celi logging sistem**.
- Od tog trenutka, bilo koji modul u istom procesu (bilo `models/users.py` ili
  `server/events.py`) može samo da uradi `import logging` i `logging.info("nešto")` - i poruka će završiti u istom log fajlu koji si konfigurisao.

## Zašto ne koristiš `logger = logging.getLogger(__name__)` svuda?

To je elegantnija varijanta, jer svaki modul dobija svoj **ime-tag** u logu:

```python
import logging
logger = logging.getLogger(__name__)
logger.info("Ovo je poruka iz models/users.py")
```

Ako imaš u `run_jam.py` ovakav formatter:

```python
"%(asctime)s [%(levelname)s] %(name)s: %(message)s"
```

onda ćeš u log fajlu videti nešto poput:

```sh
2025-10-24 17:08:01 [INFO] models.users: Ovo je poruka iz models/users.py
```

što je mnogo preglednije nego kad sve ide pod “root”.

Ali to je **isti sistem**, samo više “po pravilima umetnosti”.

## Moj predlog za praksu

1. **Jednom u `run_jam.py`** — podesi format, nivo i handlere (kao što smo uradili).
2. **U svakom drugom modulu** (models, hooks itd.) — samo stavi na vrh:

   ```python
   import logging
   logger = logging.getLogger(__name__)
   ```

   pa koristi `logger.debug()`, `logger.info()`, `logger.error()`.

To ti daje strukturu, ali ostaje jednostavno.

Ne moraš ništa dodatno instancirati — `logging` radi kao “jedan sistem, više izlaza”.

## Logika iza svega

Kad u `run_jam.py` napraviš konfiguraciju:

```python
import logging
logging.basicConfig(...)
```

ili kao što smo uradili — napraviš `RotatingFileHandler`, `StreamHandler`, formatter, i dodaš ih `loggeru` - to ne moraš **uvoziti** nigde.

Zašto? Zato što `logging` modul u Pythonu ima **globalni singleton** koji se automatski deli kroz sve uvoze.

Drugim rečima:

- Tvoj `run_jam.py` je prvi koji “diže” proces (pokreće app).
- On podesi logging sistem (nivo, format, handlere).
- Od tog trenutka, svaki `import logging` bilo gde u kodu
  dobija *taj već konfigurisani sistem*.
  Nema potrebe da “uvoziš” tu konfiguraciju eksplicitno.

## Ako hoćeš ipak centralizaciju (nije obavezno)

Ako baš voliš “statičan” pristup, možeš u `run_jam.py` dodati:

```python
logger = logging.getLogger("jam_app")
```

I onda u drugim modulima:

```python
from run_jam import logger
logger.info("Poruka iz modela")
```

To isto radi, ali sad imaš **jedan “imenovani” logger** koji eksplicitno vučeš.
Razlika je samo u stilu, ne u funkcionalnosti.

## Ukratko o loggingu

| Pitanje                                      | Odgovor
| -------------------------------------------- | -----------------------------
| Treba li da importujem `conf` ili `run_jam`? | ❌ Ne
| Treba li da importujem `logging`?            | ✅ Da
| Treba li da opet konfigurišem logging?       | ❌ Ne, samo jednom u `run_jam.py`
| Da li će drugi moduli “videti” logger?       | ✅ Da, automatski
