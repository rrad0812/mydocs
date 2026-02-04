
# Zamena Jam.py event driven sistema

Umesto da “čačkam” Jam core, daću ti minimalno, bezbedno i jasno rešenje koje:

- ne dira Jam.py server ni njegov dispečer,
- drži klikove i prečice u potpunosti na klijentu (JS),
- daje ti custom event manager (prioriteti, stop-propagation),
- lako se lemi na postojeći DOM (putem `data-` atributa),
- po želji samo pozove tvoju postojeću Jam akciju (npr. klik na realno dugme, ili tvoj JS handler koji već razgovara sa Jam-om).

Ako ti ovo legne, posle toga možemo da dodamo server hook kakav ti želiš — ali tek kad vidimo gde u tvojoj Jam app to smisleno prolazi.

## Ultra-jasan “event manager” (čisto JS, drop-in)

Stavi kao `static/js/event_bus.js` i učitaj ga u layout-u **posle** glavnog Jam skripta.

```html
<script src="/static/js/event_bus.js"></script>
<script>
  // primer: registracija globalnih prečica
  EventBus.on("key", ({key, ctrl, shift, alt, target}) => {
    if (ctrl && key.toLowerCase() === "s") {
      // 1) pozovi realno Jam dugme (ako postoji u DOM-u)
      const btn = document.querySelector('[data-action="save"]');
      if (btn) btn.click();

      // 2) ili pozovi tvoju JS funkciju koja radi sa Jam-om
      // MyJamActions.saveCurrentRecord();

      return false; // zadrži “stop-propagation”
    }
  }, { priority: 100 }); // viši prio radi pre drugih
</script>
```

```js
// /static/js/event_bus.js
(function () {
  const listeners = new Map(); // Map<eventName, Array<{prio, fn}>>

  function on(event, fn, opts={}) {
    const prio = opts.priority ?? 0;
    const arr = listeners.get(event) || [];
    arr.push({ prio, fn });
    arr.sort((a,b) => b.prio - a.prio);
    listeners.set(event, arr);
  }

  function emit(event, payload) {
    const arr = listeners.get(event);
    if (!arr) return;
    for (const { fn } of arr) {
      const res = fn(payload);
      if (res === false) break; // stop chain
    }
  }

  // --- DOM glue: klikovi i tastatura ---
  document.addEventListener("click", (e) => {
    const el = e.target.closest("[data-action]");
    if (!el) return;
    const action = el.dataset.action;    // npr. "save", "delete"
    const scope  = el.dataset.scope || ""; // npr. "customers"
    const payload = { action, scope, x: e.clientX, y: e.clientY, target: el };
    const res = emit(`click:${scope}`, payload);
    if (res === false) e.stopPropagation();
    const res2 = emit("click", payload);
    if (res2 === false) e.stopPropagation();
  }, true); // capture faza da stignemo pre ostalih

  document.addEventListener("keydown", (e) => {
    const payload = {
      key: e.key,
      ctrl: e.ctrlKey,
      shift: e.shiftKey,
      alt: e.altKey,
      target: e.target
    };
    const r1 = emit("key", payload);
    if (r1 === false) { e.preventDefault(); e.stopPropagation(); return; }

    // scope po fokusu (opciono):
    const scopedEl = document.querySelector("[data-scope].scope-active");
    if (scopedEl) {
      const scope = scopedEl.dataset.scope;
      const r2 = emit(`key:${scope}`, payload);
      if (r2 === false) { e.preventDefault(); e.stopPropagation(); }
    }
  }, true);

  // public API
  window.EventBus = { on, emit };
})();
```

**Kako se kačiš na elemente:**
Na bilo koje dugme/ikonicu staviš `data-action` i (opciono) `data-scope`.

```html
<button data-action="save" data-scope="customers" id="btnSave">
  Save
</button>
```

**2) Primeri registracije (jasno i kratko)**:

*A) Globalni klik handler sa prioritetom i stop-propagation*:

```html
<script>
EventBus.on("click", ({action, scope, target}) => {
  if (action === "delete") {
    if (!confirm("Sigurno brisanje?")) return false; // stop chain
  }
}, { priority: 100 });
</script>
```

*B) Scope-specifični handler (npr. forma “customers”)*:

```html
<script>
EventBus.on("click:customers", ({action, target}) => {
  if (action === "save") {
    // pozovi baš to dugme koje Jam već zna:
    target.click(); // ili document.getElementById("btnSave").click();
    return false;   // spreči duple triggere
  }
}, { priority: 50 });
</script>
```

*C) Prečica Ctrl+S za sve forme*:

```html
<script>
EventBus.on("key", ({key, ctrl}) => {
  if (ctrl && key.toLowerCase() === "s") {
    const btn = document.querySelector('[data-action="save"]');
    if (btn) btn.click();
    return false;
  }
}, { priority: 100 });
</script>
```

---

**3) Zašto je ovo “bezbedno” za tvoj Jam.py**

- **Ne diramo server**: nema WebSocket-a, nema “podrazumevanih” poziva u Jam core.
- **Ne pretpostavljamo ništa** o internim Jam form objektima.
- **Radi odmah** na nivou DOM-a: ili *klikne* postojeći kontroler, ili zove tvoju JS funkciju koja *već* priča sa Jam-om (na način na koji to tvoja app radi).
- Daje ti **prioritete** i **stop-propagation** (ako handler vrati `false`).

Ako/ kad poželiš da određeni događaj ipak ide na backend **preko onoga što ti već imaš** (XHR/JSON-RPC/što god Jam koristi u tvojoj app), samo u handleru napravi `fetch()` / `$.ajax()` ka tvojoj ruti. Ali to je *posle* — prvo da ti legne model.

**4) Minimalni test (bez Jam-a, čisto da vidiš ponašanje)**:

Ubaci ovo na praznu stranu i proveri konzolu:

```html
<button data-action="save" id="btnSave">Save</button>
<button data-action="delete" id="btnDelete">Delete</button>

<script>
  // logovi:
  EventBus.on("click", p => { console.log("[global click]", p); });
  EventBus.on("click", p => {
    if (p.action === "delete") {
      console.log("Blokiram delete.");
      return false; // stop dalje
    }
  }, { priority: 100 });

  // key:
  EventBus.on("key", ({key, ctrl}) => {
    if (ctrl && key.toLowerCase() === "s") {
      console.log("Pretvaram se da klikćem save…");
      document.querySelector('[data-action="save"]').click();
      return false;
    }
  }, { priority: 100 });
</script>
```

Pritisni **Ctrl+S** → trebalo bi da vidiš “Pretvaram se…” i da se simulira klik na Save.
Klik na **Delete** → treba da iskoči confirm/ili da se blokira, prema gore postavljenom handleru.

---

## Šta dalje (kad ti ovo proradi)

Kad potvrdiš da ti ovakav **klijentski event menadžer** odgovara, možemo:

1. dodati **debounce/throttle** za “brze tastere” (npr. da Ctrl+S ne puca 10×),
2. definisati **mapu prečica** po scope-u (npr. `shortcuts["customers"] = {...}`),
3. *ako želiš*, uvesti **server-side hook** koji (bez menjanja Jam core) prima samo *odabrane* evente (npr. audit log), koristeći tvoj POST endpoint (a ne izmišljeni WS).

Ako hoćeš, pokaži mi *kako tačno sada pozivaš Jam akcije iz JS-a* (tvoj jedan mali primer) i ja ću odmah u ove snippete ubaciti **tačan poziv** umesto “klikni dugme”.
