
# Dockering

Recimo da imam neki *python* project, koji radi na *Postgres* db. Kako da to bude
u dva kontejnera, tako da imam mogućnost da menjam tip baze, kao i kako sve to
povezati izmedju sebe i da bude vidljivo spolja?

To je odličan primer — pravi mali *multicontainer setup*:

- jedan kontejner za Python aplikaciju
- drugi kontejner za PostgreSQL
- mreža između njih
- mogućnost da se baza menja (npr. PostgreSQL → MySQL ili SQLite)
- otvoreni portovi da sve bude vidljivo spolja

## Implementacija

- **Uobičajena struktura projekta**

  ```sh
  myapp/
  │── app/
  │   └── main.py
  │── requirements.txt
  │── Dockerfile
  │── docker-compose.yml
  ```

- **`Dockerfile` za Python aplikaciju**

  ```yml
  FROM python:3.11-slim
  WORKDIR /app
  COPY requirements.txt .
  RUN pip install --no-cache-dir -r requirements.txt
  COPY . .
  CMD ["python", "app/main.py"]
  ```

- **`main.py` (aplikacija koja se povezuje na DB)**

  ```py
  import os
  import psycopg2
  
  DB_HOST = os.getenv("DB_HOST", "localhost")
  DB_NAME = os.getenv("DB_NAME", "mydb")
  DB_USER = os.getenv("DB_USER", "user")
  DB_PASS = os.getenv("DB_PASS", "password")
  
  print("Connecting to database...")
  
  conn = psycopg2.connect(
      host=DB_HOST,
      database=DB_NAME,
      user=DB_USER,
      password=DB_PASS
  )
  
  print("Connected!")
  ```

- **`docker-compose.yml` – dva kontejnera + mreža**

  ```yml
  Version: "3.9"
  
  services:
    db:
      image: postgres:16
      container_name: mydb
      environment:
        POSTGRES_USER: user
        POSTGRES_PASSWORD: password
        POSTGRES_DB: mydb
      volumes:
        - postgres_data:/var/lib/postgresql/data
      ports:
        - "5432:5432"   # vidljivo spolja
      networks:
        - backend
    app:
      build: .
      container_name: myapp
      environment:
        DB_HOST: db
        DB_NAME: mydb
        DB_USER: user
        DB_PASS: password
      depends_on:
        - db
      ports:
        - "8000:8000"   # ako aplikacija sluša na portu 8000
      networks:
        - backend
  
  networks:
    backend:
  
  volumes:
    postgres_data:
  ```
  
- **Bitne činjenice**

  Šta je ovde bitno?                        | Objašnjenje
  ------------------------------------------|--------------------------------------------
  db i app su dve usluge                    | svaka je u svom kontejneru
  db koristi Docker Hub image "postgres:16" | nema Dockerfile za bazu
  app se gradi iz Dockerfile                | znači mi smo backend image
  DB_HOST=db                                | hostname je ime servisa u compose-u
  depends_on                                | app se pokreće tek kad db kontejner pokrene
  ports: "5432:5432"                        | 5432 je otvoren spolja

- **Pokretanje kontejnera**

  ```sh
  docker compose up –build
  ```

- **Provera kontejnera**:

  ```sh
  docker ps
  ```

- **Testiranje povezivanja sa host mašine**

  ```sh
  psql -h localhost -U user -d mydb
  ```

- **Promena baze podataka (Postgres → MySQL)**

  Menjaš samo sekciju `db` u `docker-compose.yml`.
  
  Primer sa MySQL:
  
  ```yml
    db:
      image: mysql:8
      environment:
        MYSQL_ROOT_PASSWORD: root
        MYSQL_DATABASE: mydb
        MYSQL_USER: user
        MYSQL_PASSWORD: password
      ports:
        - "3306:3306"
  ```

  I promeniš python dependencije:

  ```sh
  requirements.txt → mysqlclient ili sqlalchemy + pymysql
  ```

  A u kodu umesto `psycopg2` koristiš `MySQL` konektor.

- **Kako app pristupa bazi iz koda?**

  **Važno**: Ne koristi `localhost` u kontejneru, nego ime servisa iz compose-a:

  ```yml
  host="db"  # jer je db naziv servisa u docker-compose.yml
  ```

- **Kako da aplikacija bude vidljiva spolja?**

  U compose-u:
  
  ```yml
  ports:
    - "8000:8000"
  ```
  
  To znači:
  
  - spolja na host mašini → `localhost:8000`
  - kontejner interno sluša na `0.0.0.0:8000`
    Ako aplikacija sluša samo na `localhost` unutra → neće raditi.  

  U Flask-u, npr.:

  ```py
  app.run(host="0.0.0.0", port=8000)
  ```

## Dockerfile

```yml
# Baza (osnovni image) — zvanični Python image sa Debian Slim bazom
# - "slim" znači manji image (~60MB umesto ~900MB kao full Debian)
FROM python:3.11-slim

# Postavi radni direktorijum unutar kontejnera
# Sve naredne instrukcije izvršavaće se unutar /app
WORKDIR /app

# Kopiramo requirements prvo, bez ostalog koda,
# jer ovo omogućava Docker-u da kešira slojeve i izbegne reinstall
COPY requirements.txt .

# Instaliramo Python zavisnosti
# --no-cache-dir sprečava da pip čuva download cache → smanjuje image
RUN pip install --no-cache-dir -r requirements.txt

# Tek sada kopiramo ostatak aplikacije
# (ovo je bitno zbog Docker keša: ako se kod menja, ne reinstalira se pip)
COPY . .

# Default komandа koju kontejner izvršava kad se pokrene
# Koristimo exec formu (array), bolja radi signal handling i PID 1
CMD ["python", "app/main.py"]
```

**Zašto je Dockerfile organizovan ovako?**

- **Optimalno keširanje**

  - `COPY requirements.txt` i `RUN pip install` su pre kopiranja ostatka koda
  - tako Docker ne radi ponovnu instalaciju paketa svaki put kad se promeni Python kod
  - znači mnogo brže build-ovanje
  
  Ako bi uradio ovako:
  
  ```yml
  COPY . .
  RUN pip install -r requirements.txt
  ```
  
  → svaka promena u kodu briše keš i ponovo instalira sve pakete ==> sporo.

- **Slim image**

  - koristimo python:3.11-slim, jer je mali i dovoljan
  - ako treba OS paket (gcc, libpq-dev, usw.), možemo dodati apt install
  - ako treba alpine, može, ali ponekad ima problema sa C-based paketima (npr. psycopg2)

- **CMD vs ENTRYPOINT**

  Ovo:
  
  ```yml
  CMD ["python", "app/main.py"]
  ```
  
  znači: kada se `docker run ...` izvrši, ovo je podrazumevana komanda.
  
  Ako hoćeš komandа se može zameniti npr:
  
  ```sh
  docker run myapp python manage.py shell
  ```
  
  onda je `CMD` dobar.
  
  Ako želiš da uvek pokreneš Python, a argumenti budu skripta — koristi `ENTRYPOINT`.

- **Gde da se dodaju OS paketi (npr. PostgreSQL libovi)?**

  Ako app koristi `psycopg2`, moramo dodati:
  
  ```yml
  RUN apt-get update && apt-get install -y \
    libpq-dev gcc \
    && rm -rf /var/lib/apt/lists/*
  ```
  
  To ide pre `pip install`, jer psycopg2 mora da kompilira binarni deo.
  
  ```yml
  FROM python:3.11-slim
  WORKDIR /app
  RUN apt-get update && apt-get install -y \
    libpq-dev gcc \
    && rm -rf /var/lib/apt/lists/*
  COPY requirements.txt .
  RUN pip install --no-cache-dir -r requirements.txt
  COPY . .
  CMD ["python", "app/main.py"]
  ```

## Odnosi izmedju app na hostu i app u kontejneru

- `App` na hostu ≠ `App` u kontejneru
- `App` na hostu je tvoj normalni lokalni kod, u folderu, npr. `~/Projects/myapp/`
- `App` u slici (image) je „spakovani snapshot“ koji se pravi iz Dockerfile-a
- `App` u kontejneru je pokrenuta instanca tog image-a

Znači:

```sh
HOST code → (Dockerfile build) → IMAGE → (run) → CONTAINER
```

- **docker build**

  Docker uzme fajlove iz tvog host foldera samo dok traje build i kopira ih u image.
  
  Primer iz Dockerfile-a:
  
  ```yml
  COPY . .
  ```
  
  Ovo znači: "Uzmi sve fajlove iz trenutnog foldera na hostu i ubaci ih u folder "/app" unutra image-a".
  
  Posle build-ovanja:
  
  - image ima svoju kopiju koda
  - kontejner više ne zavisi od tvog lokalnog fajl sistema
  
  Zato kada promeniš kod na hostu, to ne menja ništa u kontejneru — moraš da radiš:
  
  ```sh
  docker build --no-cache .
  docker compose up –build .
  ```

- **Kako da app u kontejneru koristi kod sa hosta u realnom vremenu?**

  To se rešava `bind mount`-om u `docker-compose.yml`:
  
  ```yml
  app:
    volumes:
      - .:/app
  ```
  
  To znači:
  
  ```sh
  HOST: current folder  →  CONTAINER: /app
  ```
  
  - Sad kontejner "vidi" tvoj kod uživo
  - Promene u editoru se reflektuju u kontejneru
  - Koristi se najčešće u development modu
  - Ali ovaj pristup gazi kod iz image-a (Dockerfile COPY postaje nebitan)
  
  Dakle postoje dva moda rada:
  
  Mod               | Dockerfile COPY| volumes: .:/app | Kada se koristi
  ------------------|----------------|-----------------|------------------------------
  Production build  | Da             | Ne              | Deployment, server, CI/CD
  Local development | Ne             | Da              | Kodiranje bez rebuildovanja

- **Primer Flask razvojni server**

  ```yml
  app:
    build: .
    volumes:
      - .:/app
    command: flask run --host=0.0.0.0 --reload
  ```
  
  - **Ako ne koristiš `volumes`**  
    Ako izmeniš kod na hostu kontejner se ne menja, moraš da rebuilduješ!

    ```sh
    docker compose build app
    docker compose up -d
    ```
  
  - **Ako koristiš volumes**
  
    Ako izmeniš kod na hostu, kontejner automatski radi sa novim kodom, nema rebuild-a, radi kao lokalno.
  
  - **Zašto se ljudi zbune?**
  
    Jer Docker ima dva "sveta":

    Pogled                                      | Realnost
    ------------------------------------------- | --------------------------------------------------
    “Ja imam app na disku”                      | Docker ga ignoriše nakon build-a
    “Zašto moj kod u kontejneru nije ažuriran?” | Jer image je snapshot napravljen u vremenu
    “Zašto mi app radi drugačije u kontejneru?” | Jer ima svoj filesystem, environment, dependencies

    Najčešće greške:

    - Menjam kod, ali ga Docker ne vidi → TO JE NORMALNO BEZ VOLUMES,
    - Instalirao sam modul lokalno, ali kontejner ga ne vidi → naravno, jer je u drugom OS-u,
    - pip install u kontejneru ne menja `requirements.txt` → tačno, environment nije vezan za host.

## Razlike i sličnosti izmedju sekvenci buildovanja

Odlično pitanje — ovo zbunjuje skoro sve koji počnu da rade s Docker-om, jer se čini da rade skoro istu stvar, ali zapravo nisu ista operacija i koriste se u različitim situacijama.
Hajde da objasnimo razliku jasno i precizno.

- **Prva sekvenca**:

  ```sh
  docker build --no-cache .
  docker compose up --build
  ```
  
   Komanda                     | Šta radi
   --------------------------- | ------------------------------------------------------------------
   `docker build --no-cache .` | Ručno build-uje image iz Dockerfile-a (bez keša)
   `docker compose up --build` | Pokreće kontejnere i, ako image ne postoji ili se promenio, ponovo ga build-uje.
  
  **Kada se koristi?**
  
  - kada želiš da kontrolišeš build proces,
  - kada želiš potpuno svež build (bez Docker keš slojeva)
  - kada ne koristiš docker-compose da gradi image, nego ga gradiš unapred
  - kada želiš prvo samo da builduješ, testiraš image, tek onda da ga pokreneš
  
  Ovo je najčešće **CI/CD ili production** način rada (jer `build` i `run` su odvojeni koraci).
  
- **Druga sekvenca**

  ```sh
  docker compose build app
  docker compose up -d
  ```
  
   Komanda                    | Šta radi
   -------------------------- | ---------------------------------------------------------------
   `docker compose build app` | Docker Compose gradi image samo za servis `app` (koristeći build sekciju iz compose.yml)
   `docker compose up -d`     | Pokreće kontejnere bez interaktivnih logova (detached mode)
  
  **Kada se koristi?**
  
  - kada želiš da Docker Compose upravlja i build-om i pokretanjem
  - kada imaš više servisa u compose fajlu, i želiš da builduješ samo neke (app, ne i db)
  - kada hoćeš fast-dev workflow: build šta je promenjeno → up
  - kad korišćenje keša nije problem (za brzi dev loop)
  
  Ovo je najčešći **development** workflow.

- **Suštinska razlika**

  Pitanje                                        | `docker build`      | `docker compose build`
  ---------------------------------------------- | ------------------- | ---------------------------
  Ko gradi image?                                | Docker CLI          | Docker Compose
  Gde se čita Dockerfile?                        | direktno iz foldera | iz `build:` sekcije u compose.yml
  Može li da gradi više servisa?                 | ne                  | da (`compose build --parallel`)
  Može li da targetira jedan servis?             | ne                  | da (`compose build app`)
  Da li zna za networks, volumes, depends_on...? | ne                  | zna (ali samo u `compose up`)

- **Šta radi `--build` u `docker compose up --build`?**

  To znači: Ako image ne postoji ili je Dockerfile izmenjen, izgradi ga pre pokretanja.
  
  Bez `--build`, Docker Compose neće automatski rebuildovati image čak i ako si izmenio kod — koristiće stari image.
  
- **Česta zabuna**  
  Mnogi misle da je:

  ```sh
  docker build .
  ```

  isto što i:

  ```sh
  docker compose build

  ```

  Nije isto.
  - `docker build` ignoriše `docker-compose.yml`. On uzima Dockerfile, gradi image i završava.
  - `docker compose build` koristi sekciju `build:` iz compose fajla, zna ime servisa, tag, args, context, sve.

- **Kada koristiti koju varijantu?**

  Situacija                                   | Preporuka
  ------------------------------------------- | --------------------------------------------------
  Lokalan razvoj, menjam kod stalno           | `docker compose build app` + `docker compose up`
  Menjaš samo Python kod, koristiš bind mount | `docker compose up` (nije potrebno build-ovati)
  CI/CD pipeline, deployment, produkcija  | `docker build -t myapp:1.0.0 .` pa `docker compose up -d`
  Hoćeš potpuno čist build bez keša       | `docker build --no-cache .` ili `docker compose build --no-cache`
  Imaš više servisa, gradiš samo jedan    | `docker compose build app`
  Samo želiš da startuješ bez build-a     | `docker compose up -d`

- **Brzi primer razlike**

  Ako imaš ovaj deo u `compose.yml`:
  
  ```yml
  services:
    app:
      build:
        context: .
        dockerfile: Dockerfile.app
  ```
  
  - `docker compose build app` koristi `Dockerfile.app`
  - `docker build .` koristi Dockerfile (podrazumevani naziv)
  
  Znači ako imaš više Dockerfile-ova → `docker build` se ne koristi!

## Praksa za razvoj i produkciju

- **Razvoj**

  `docker-compose.dev.yml`:
  
  ```yml
  services:
    app:
      build: .
      volumes:
        - .:/app
      environment:
        PYTHONUNBUFFERED: 1
      command: python app/main.py
  ```

- **Produkcija**

  `docker-compose.prod.yml`:
  
  ```yml
  services:
    app:
      image: myapp:latest
      ports:
        - "8000:8000"
  ```

  Ukratko u jednoj rečenici:
  
  - `Image` je snapshot,  
  - `Container` je proces,  
  - Host folder i kod u kontejneru su potpuno odvojeni, osim ako ih ne "povežeš" preko `volumes`.

- **Primeri**

  Napravićemo dva primera za Python aplikaciju u Docker-u:
  
  - Sa `volumes`: (`bind mount`) → koristi host kod u realnom vremenu (idealno za razvoj)
  - Bez `volumes`: (`copy u image`) → koristi kod iz image-a (idealno za produkciju)
  
  Takođe ću ti pokazati razliku između:
  
  - `volumes`: (Docker-ov persistent volume)
  - `bind mount`: (host folder → kontejner folder)
  
  - **Development verzija – bind mount (.:/app)**
  
    - Kontejner koristi tvoj lokalni kod direktno.
    - Svaka izmena u editoru odmah se vidi u kontejneru.
    - Nema rebuild-a, nema restart-a (ako koristiš `--reload`).

    `docker-compose.dev.yml`:

    ```yml
    Version: "3.9"
    
    services:
      app:
        build: .
        container_name: myapp_dev
        ports:
          - "8000:8000"
        environment:
          DB_HOST: db
          DB_NAME: mydb
          DB_USER: user
          DB_PASS: password
        volumes:
          - .:/app         # <-- ovo je bind mount (host → container)
        command: python app/main.py
        depends_on:
          - db
      db:
        image: postgres:16
        container_name: mydb_dev
        environment:
          POSTGRES_USER: user
          POSTGRES_PASSWORD: password
          POSTGRES_DB: mydb
        volumes:
          - postgres_data:/var/lib/postgresql/data   # ovo nije bind mount, već Docker volume
        ports:
          - "5432:5432"
    volumes:
      postgres_data:
    ```
  
  - **Produkciona verzija**
  
    `docker-compose.yml (produkcija)`

    ```yml
    Version: "3.9"
    
    services:
      app:
        image: myapp:latest
        container_name: myapp_prod
        ports:
          - "8000:8000"
        environment:
          DB_HOST: db
          DB_NAME: mydb
          DB_USER: user
          DB_PASS: password
        depends_on:
          - db
      db:
        image: postgres:16
        container_name: mydb_prod
        environment:
          POSTGRES_USER: user
          POSTGRES_PASSWORD: password
          POSTGRES_DB: mydb
        volumes:
          - postgres_data:/var/lib/postgresql/data
        ports:
          - "5432:5432"
    volumes:
      postgres_data:
    ```

    Bitno:

    - **Dev verzija**
      - `.:/app` nije Docker volume, nego bind mount → host folder == kontejner folder.  
        Ako obrišeš fajl lokalno → nestaje i u kontejneru.
    - **Production verzija** – bez mount-a, samo kod iz image-a.
      - Kod je ugrađen u image.
      - Kontejner je potpuno nezavisan od host sistema.
      - Promena koda lokalno zahteva docker compose build.

### Razlika - volume vs bind mount

Tip              | Primer             | Gde se čuva                    | Kada se koristi
-----------------|--------------------|--------------------------------|--------------------
Bind mount       |.:/app              | host filesystem                | razvoj, live reload
Named volume     | postgres_data:/var/lib/postgresql/data | Docker internal storage | baza, persistent storage
Anonymous volume | /app bez imena     | random Docker path             | retko, temporary

### Kako koristiti obe verzije u istoj aplikaciji?

Ovo radiš kad želiš da samo baza bude persistenta, a kod da se menja:

```yml
  volumes:
    .:/app                                   # host → container (code hot reload)
    postgres_data:/var/lib/postgresql/data   # docker volume (persistent database)
```

#### Komande

- **Dev setup** (live kod):

  ```sh
  docker compose -f docker-compose.dev.yml up --build
  ```

- **Prod setup** (immutable image):

  ```sh
  docker build -t myapp:latest .
  docker compose up -d
  ```

**Šta se dešava kad promeniš fajl?**

Ažuriraš main.py lokalno          | Dev setup (.:/app)            | Prod setup (bez volume)
----------------------------------|-------------------------------|------------------------
Potrebno rebuild-ovanje?          | Ne                            | Da
Potrebno restart-ovanje?          | Ne (ako koristiš auto-reload) | Da
Kontejner ima uvek najnoviji kod? | Da                            | Ne
Ne zavisi od host fajlova?        | Ne                            | Da

## Primer Flask projekta

### Struktura projekta flask primera

```sh
myapp/
│── app/
│   └── main.py
│── requirements.txt
│── Dockerfile.dev
│── docker-compose.dev.yml
```

`main.py` (Flask API primer)

```py
from flask import Flask

app = Flask(__name__)

@app.get("/")
def index():
    return {"message": "Hello from Docker + hot reload!"}

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000, debug=True)
```

`requirements.txt`

```sh
flask
psycopg2-binary
# (ako želiš SQLAlchemy, kasnije možemo da dodamo)
```

`Dockerfile.dev` (za razvoj)

```yml
FROM python:3.11-slim
WORKDIR /app

# OS paketi za psycopg2
RUN apt-get update && apt-get install -y \
    gcc libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Instaliramo dependencije pre COPY . zbog Docker cache-a
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Kopiramo ostatak koda (nije ključan u dev režimu jer imamo mount)
COPY . .

# Flask će biti pokretan preko docker-compose komandе
```

`docker-compose.dev.yml`

```yml
version: "3.9"

services:
  app:
    build:
      context: .
      dockerfile: Dockerfile.dev
    container_name: myapp_dev
    ports:
      - "8000:8000"
    environment:
      FLASK_ENV: development
      DB_HOST: db
      DB_NAME: mydb
      DB_USER: user
      DB_PASS: password
    volumes:
      - .:/app        # <-- hot reload, koristi host kod
    command: flask run --host=0.0.0.0 --port=8000 --reload
    depends_on:
      - db
  db:
    image: postgres:16
    container_name: mydb_dev
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
      POSTGRES_DB: mydb
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"
volumes:
  postgres_data:
```

### Pokretanje flask projekta

```sh
docker compose -f docker-compose.dev.yml up –build
```

Kad vidiš log:

```sh
- Debugger is active!
- Restarting with stat
- Running on <http://0.0.0.0:8000>
```

Otvori browser:

```sh
<http://localhost:8000>
```

Videćeš:

```sh
{"message": "Hello from Docker + hot reload!"}
```

**Test automatskog reload-a**:

- Promeni main.py, npr.:
  
  ```py
  return {"message": "Reload radi!"}
  ```

- Sačuvaj fajl
- Pogledaj terminal → Flask kaže: "Detected change, restarting..."
- Osveži browser → vidiš novu poruku

Dakle: nema rebuild, nema restart kontejnera, samo edit → save → refresh.

## Mreže u Docker Compose

Osnovna stvar: svaki `docker-compose.yml` automatski pravi mrežu.

Ako samo napraviš ovo:

```yml
services:
  app:
  db:
```

Docker automatski kreira mrežu imena: `<ime_foldera>_default`. I svi servisi u compose-u su međusobno vidljivi preko DNS-a.

Zato u Python app-u pišemo:

```py
host="db"
```

→ jer je `db` DNS ime servisa, ne `localhost`.

### Ručno definisanje mreže

Možemo eksplicitno dodati mrežu:

```yml
networks:  
  backend:
```

A zatim je dodeliti servisima:

```yml
services:
  app:
    networks:
      - backend
  db:
    networks:
      - backend
```

To je isto kao implicitna mreža, samo eksplicitno definisana.

### Primer sa više mreža

Ako imaš API, bazu i frontend koji ne treba da direktno vidi bazu:

```yml
services:
  frontend:
    networks:
      - web
  api:
    networks:
      - web
      - backend
  db:
    networks:
      - backend

networks:
  web:
  backend:
```

Container      | Može da vidi | Ne može da vidi
---------------|--------------|------------------
frontend       | api          | db
api, frontend  | db           | —
db             | api          | frontend

Dakle mreža = izolacija (kao mali LAN-ovi).

### Kako se servisi međusobno vide?

- preko DNS imena (ime servisa u compose-u)
- ne koriste IP adrese ručno (Docker menja IP posle svakog pokretanja)

Primer:

```sh
app → "db:5432"
frontend → "api:8000"
worker → "redis:6379"
```

DNS imena ne trebaš podesiti ručno, Docker ih pravi automatski.

### Eksplicitno podešavanje mreže sa driver-om

```yml
networks:
  backend:
    driver: bridge
```

`bridge` je default (normalno izolovana mreža).

Drugi drajveri:

Driver    | Koristi se kada…
----------|-------------------------------------------------
`bridge`  | obični kontejneri, default
`host`    | koristi mrežu host sistema, nema izolacije
`overlay` | swarm mode, multi-host setup
`macvlan` | kad kontejner treba pravi LAN IP

U 99% slučajeva → `bridge`.

### Da li ti treba mreža kad koristiš ports:?

- `ports`: otvara port prema host mašini
- `networks`: određuje vidljivost između kontejnera

Dakle:

Šta želiš?                      | Šta koristiš?
--------------------------------|--------------------
da pristupiš kontejneru spolja  | ports: "5432:5432"
da kontejneri pričaju međusobno | networks:

### Kratki rezime

Stvar                                                | Objašnjenje
-----------------------------------------------------|------------------------------------
Docker Compose automatski pravi mrežu                | svi servisi su u istom LAN-u
Ne koristiš localhost za inter-container konekciju   | već ime servisa (db, redis, api...)
Custom mreže služe za izolaciju                      | frontend vidi API, ali ne DB
ports: otvara prema hostu, ne ka drugim kontejnerima | mreža je zasebna stvar

## Scripta wait-for-it.sh

Problem koji rešava:

- `docker-compose up` ne garantuje da je DB spremna pre nego što se app pokrene.
- `Compose depends_on`: pokreće DB pre, ali ne čeka da DB zaista bude spremna.
- DB startuje, ali konekcija odbija narednih 2–4 sekunde → app puca zbog `connection error`-a.  
  Primer error-a:  
  `psycopg2.OperationalError: could not connect to server: Connection refused`

Rešenje: čekaj dok port ne odgovori

wait-for-it.sh je mali bash skript koji radi:

```sh
while port nije otvoren:
    sleep 1
startuj app
```

Primer upotrebe:

```sh
command: ["./wait-for-it.sh", "db:5432", "--", "python", "app/main.py"]
```

Znači:

- čekaj da db:5432 proradi
- kad proradi → pokreni Python app

Bez toga → app često puca na startu.

### Instalacija wait-for-it.sh u Dockerfile

Dodaj u Dockerfile:

```sh
COPY wait-for-it.sh /wait-for-it.sh
RUN chmod +x /wait-for-it.sh
```

Onda u compose:

```sh
command: ["./wait-for-it.sh", "db:5432", "--", "python", "app/main.py"]
```

**Alternativa**: alpine/wait ili dockerize

Postoji i gotov image:

```sh
command: ["dockerize", "-wait", "tcp://db:5432", "-timeout", "30s", "python", "app/main.py"]
```

Ili Python rešenje u kodu (retry loop).

### Zašto nije dovoljno depends_on?

- Stvar Radi? Ali... `depends_on` pokreće servise redom, ne čeka dostupnost porta,
- restart: always restartuje ako padne loše rešenje, "spin loop",
- wait-for-it.sh čeka dok DB stvarno ne radi pravilno!

Super — evo kompletno pripremljenog primera gde Python aplikacija ne pokušava da se poveže na bazu dok ona ne postane dostupna, koristeći wait-for-it.sh.
Ovo je najčešći profesionalni obrazac u Docker stack-u sa bazom, jer rešava problem da se app pokrene pre nego što DB spremi TCP socket.

### Projekat sa `wait-for-it.sh`

```sh
myapp/
│── app/
│   └── main.py
│── wait-for-it.sh
│── requirements.txt
│── Dockerfile
│── docker-compose.yml
```

`main.py` (minimalni Python test konekcije)

```sh
import os
import psycopg2

DB_HOST = os.getenv("DB_HOST")
DB_NAME = os.getenv("DB_NAME")
DB_USER = os.getenv("DB_USER")
DB_PASS = os.getenv("DB_PASS")

print("Trying database connection...")

conn = psycopg2.connect(
    host=DB_HOST,
    database=DB_NAME,
    user=DB_USER,
    password=DB_PASS
)

print("Connected to database successfully!")
```

Bez retry logike — jer `wait-for-it.sh` radi posao umesto nas.

- requirements.txt  
- psycopg2-binary  
- (Flask ili FastAPI može se dodati kasnije)  
- wait-for-it.sh  

Ovo je zvanična skripta sa GitHub-a (skraćena verzija zbog prostora). U realnom projektu preuzmeš je celu, ali ovde je dovoljno za demonstraciju:

```sh
#!/usr/bin/env bash
# wait-for-it.sh: wait until a host:port is available

host="$1"
shift
cmd="$@"

until nc -z $host; do
  echo "Waiting for $host..."
  sleep 1
done

echo "$host is available, starting app..."
exec $cmd
```

U praksi, uzme se full verzija odavde: <https://github.com/eficode/wait-for/blob/master/wait-for-it.sh>

`Dockerfile`

```yml
FROM python:3.11-slim
WORKDIR /app
RUN apt-get update && apt-get install -y \
    netcat-openbsd gcc libpq-dev \
    && rm -rf /var/lib/apt/lists/*
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
COPY . .
RUN chmod +x wait-for-it.sh
CMD ["python", "app/main.py"]
```

netcat-openbsd se instalira jer `wait-for-it.sh` koristi nc za proveru porta

`docker-compose.yml`

```yml
version: "3.9"

services:
  app:
    build: .
    container_name: myapp
    depends_on:
      - db
    environment:
      DB_HOST: db
      DB_NAME: mydb
      DB_USER: user
      DB_PASS: password
    command: ["./wait-for-it.sh", "db:5432", "--", "python", "app/main.py"]
    ports:
      - "8000:8000"

  db:
    image: postgres:16
    container_name: mydb
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
      POSTGRES_DB: mydb
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

volumes:
  postgres_data:
```

- depends_on: pokreće DB prvo
- wait-for-it.sh čeka da port zaista proradi
- Tek onda se startuje python app/main.py

### Pokretanje projekta sa `wait-for-it.sh`

```sh
docker compose up --build
```

Prvo ćeš videti nešto ovako:

```sh
Waiting for db:5432...  
Waiting for db:5432...  
Waiting for db:5432...  
db:5432 is available, starting app...  
Trying database connection...  
Connected to database successfully!  
```

Nema grešaka, nema ručnih retry-eva. Sve radi predvidljivo.

Zašto je ovo bolje nego da app radi retry loop?

- DB start time se rešava izvan aplikacije
- Nema dodatnog koda u Python-u
- Radi sa bilo kojim jezikom (Go, Node, Java, Rust…)
- Compose build ostaje čist
- Može se koristiti i za čekanje Redis-a, Rabbitmq-a itd.

Bonus: verzija sa više čekanja

Ako app zavisi i od DB i od Redis-a:

```yml
command: ["./wait-for-it.sh", "db:5432", "--", "./wait-for-it.sh", "redis:6379", "--", "python", "app/main.py"]
```

Ili sa alternativom:

```sh
command: ["dockerize", "-wait", "tcp://db:5432", "-wait", "tcp://redis:6379", "-timeout", "30s", "python", "app/main.py"]
```

Ako želiš mogu da ti dodam dockerize varijantu.

## Healthcheck u Docker Compose

### Šta je healthcheck?

Healthcheck je komanda koju Docker periodično izvršava unutar kontejnera da proveri da li servis radi ispravno, a ne samo da li je proces živ.

Kod PostgreSQL-a:

- proces postgres može da postoji, ali
- veza na port 5432 može da ne radi još 3 sekunde

Znači — running ≠ healthy.

#### Primer healthcheck-a za PostgreSQL

U docker-compose.yml:

```yml
  db:
    image: postgres:16
    container_name: mydb
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
      POSTGRES_DB: mydb
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U user -d mydb"]
      interval: 3s
      timeout: 3s
      retries: 5
    ports:
      - "5432:5432"
```

**Objašnjenje:**

Podešavanje    | Značenje
---------------|-----------------------------------------------------------------
test           | komanda koju Docker izvršava
pg_isready     | Postgres CLI alat koji proverava konekciju
interval: 3s   | Proverava svakih 3 sekunde
timeout: 3s    | Komanda mora da završi u 3 sekunde
retries: 5     | Ako 5 puta uzastopno padne → označen kao unhealthy

### Kako Docker prikazuje health status?

docker ps

Prikazaće:

```sh
mydb     healthy
myapp    starting
```

Ili:

```sh
mydb     unhealthy
```

Možeš da pogledaš detalje sa:

```sh
docker inspect --format='{{json .State.Health}}' mydb | jq
```

### Kako healthy baze utiče na app?

Možemo reći aplikaciji: "Ne pokreći se dok DB health status ≠ healthy“

To se radi ovako:

```yml
  app:
    depends_on:
      db:
        condition: service_healthy
```

Dakle umesto:

```yml
depends_on:
  - db
```

koristi se:

```yml
depends_on:
  db:
    condition: service_healthy
```

Tada app sigurno ne počinje prerano.

#### Kompletan primer `docker-compose.yml` sa `healthcheck`-om

```yml
version: "3.9"

services:
  app:
    build: .
    container_name: myapp
    depends_on:
      db:
        condition: service_healthy
    environment:
      DB_HOST: db
      DB_NAME: mydb
      DB_USER: user
      DB_PASS: password
    command: ["./wait-for-it.sh", "db:5432", "--", "python", "app/main.py"]
    ports:
      - "8000:8000"

  db:
    image: postgres:16
    container_name: mydb
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
      POSTGRES_DB: mydb
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U user -d mydb"]
      interval: 3s
      timeout: 3s
      retries: 5
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

volumes:
  postgres_data:
```

Šta se dešava pri startu?

- DB se pokreće
- Docker healthcheck je označi kao starting
- Kad pg_isready počne da vraća OK → status postaje healthy
- Tek tada app se startuje

Znači još pouzdanije od samog `wait-for-it.sh`.

### Kombinovanje `healthcheck` i `wait-for-it.sh`

- `healthcheck` obezbeđuje compose-level kontrolu
- `wait-for-it` obezbeđuje in-app čekanje porta

Najbolja praksa: koristi oba.

Da dodamo u app:

```yml
healthcheck:
  test: ["CMD-SHELL", "curl -f http://localhost:8000/ || exit 1"]
  interval: 5s
  timeout: 3s
  retries: 5
```

To proverava da li API endpoint radi, ne samo proces.

## PgAdmin kao zaseban kontejner

To je tipičan lokalni develop stack i često izgleda ovako:

- `app`  →  koristi DB preko hostname `db`
- `pgAdmin` → pristupa DB preko hostname `db`
- `host browser` → pristupa `pgAdmin` na `localhost:5050`

Dodajemo `pgAdmin` u `docker-compose.yml`

Evo kompletiranog YAML fajla sa 3 servisa:

```yml
version: "3.9"

services:
  app:
    build: .
    container_name: myapp
    depends_on:
      - db
    environment:
      DB_HOST: db
      DB_NAME: mydb
      DB_USER: user
      DB_PASS: password
    ports:
      - "8000:8000"
    networks:
      - backend

  db:
    image: postgres:16
    container_name: mydb
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
      POSTGRES_DB: mydb
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"
    networks:
      - backend

  pgadmin:
    image: dpage/pgadmin4:8
    container_name: pgadmin
    environment:
      PGADMIN_DEFAULT_EMAIL: admin@local.com
      PGADMIN_DEFAULT_PASSWORD: admin
    ports:
      - "5050:80"         # host:container
    depends_on:
      - db
    networks:
      - backend
    volumes:
      - pgadmin_data:/var/lib/pgadmin

networks:
  backend:

volumes:
  postgres_data:
  pgadmin_data:
```

### Šta je ovde bitno?

Stvar                 | Objašnjenje
----------------------|------------------------------------------------------------------
image: dpage/pgadmin4 | zvanični pgAdmin image
ports: "5050:80"      | pgAdmin će biti dostupan na <http://localhost:5050>
PGADMIN_DEFAULT_EMAIL i PGADMIN_DEFAULT_PASSWORD | login za pgAdmin
depends_on:           | db pokreće se nakon PostgreSQL-a
networks:             | backend isto kao i app, mogu da "vide" db
volumes:              | pgadmin_data: čuva pgAdmin podešavanja (servere, konekcije itd.)

**Pokretanje sa pgAdmin-om**:

```sh
docker compose up --build
```

Otvoriti u browseru: <http://localhost:5050>

**Login**:  
**email**: [admin@local.com](mailto:admin.com)  
**password**: admin

Kako dodati konekciju na DB unutar pgAdmin?

Kada se uloguješ u UI:

- Add New Server
- Name: Local DB (bilo šta)
- Tab Connection:
  - Hostname/address: db (ne localhost)
  - Port: 5432
  - Username: user
  - Password: password
  - Save

**Zašto db, a ne localhost?**

- pgAdmin je u svom kontejneru, a PostgreSQL je u drugom
- u docker mreži, servisi se vide po imenu — dakle: **db**

**Kako izgleda mreža sada?**

```c#
+-------------+        +--------------+        +-------------+
|   app       | -----> | PostgreSQL   | <----- |  pgAdmin    |
| container   |        | db container |        | container   |
| DB_HOST=db  |        | port=5432    |        | connects to |
+-------------+        +--------------+        | host=db     |
                                               +-------------+
```

**Opciona poboljšanja (mogu dodati ako hoćeš)**:

Poboljšanje                           | Šta donosi
--------------------------------------|---------------------------------------------------------
Healthcheck za pgAdmin                | logičan status u docker ps
Auto-register servers.json            | da pgAdmin automatski ima konekciju bez ručnog dodavanja
Backups folder mount                  | čuvanje dump fajlova na hostu
SSL config                            | ako želiš HTTPS
Traefik / nginx reverse proxy         | ako hoćeš pgadmin.localhost umesto porta 5050

## Produkciona verzija docker-compose-a

**Bez pgAdmin-a, bez bind mount-a, bez hot-reload-a — potpuno "immutable" deployment stil**:

To znači:

- app se pokreće iz prethodno izgrađenog image-a
- kod nije mountovan sa hosta (nema .:/app)
- DB podaci su u Docker volume-u
- koristi se `wait-for-it.sh` ili `healthcheck` + `depends_on` da se start redosled ispravno reši
- ports su eksplicitno otvoreni samo ako treba (često u produkciji nije otvoren DB port ka svetu)

**Production `docker-compose.yml`**

```yml
version: "3.9"

services:
  app:
    image: myapp:latest
    container_name: myapp_prod
    depends_on:
      db:
        condition: service_healthy
    environment:
      DB_HOST: db
      DB_NAME: mydb
      DB_USER: user
      DB_PASS: password
    command: ["./wait-for-it.sh", "db:5432", "--", "python", "app/main.py"]
    ports:
      - "8000:8000"  # only if API needs public access
    networks:
      - backend

  db:
    image: postgres:16
    container_name: mydb_prod
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
      POSTGRES_DB: mydb
    volumes:
      - postgres_data:/var/lib/postgresql/data
    # In production you often DO NOT expose this port publicly:
    # ports:
    #   - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U user -d mydb"]
      interval: 3s
      timeout: 3s
      retries: 5
    networks:
      - backend

networks:
  backend:

volumes:
  postgres_data:
```

**Razlike u odnosu na development verziju**:

Feature                      | Development Compose | Production Compose
-----------------------------|---------------------|-------------------------------
build: sekcija               | `Dockerfile.dev`    | koristi se već izgrađeni image
volumes: .:/app              | da (hot reload)     | nema bind mount-a
pgAdmin                      | možda               | uklonjeno
live reload (flask --reload) | da                  | ne
DB port 5432 exposed         | da                  | obično ne (security)
koristimo wait-for-it.sh     | i ovde              | i ovde
healthcheck DB               | opciono             | preporučeno

**Kako se builduje image za produkciju**:

Dockerfile (production verzija, bez dev paketa):

```sh
docker build -t myapp:latest .
```

Ili verzionisano:

```sh
docker build -t myapp:1.0.0 .
```

**Kako se pokreće production compose**:

```sh
docker compose up -d
```

ili full rebuild + start:

```sh
docker compose down --volumes
docker compose up -d
```

**Zašto DB port nije otvoren u produkciji?**

U produkciji se najčešće NE radi ovo:

```sh
ports:
  - "5432:5432"
```

jer time bi baza bila dostupna svima spolja.

DB treba da vidi samo app preko interne mreže, ne cela internet publika. Ako ti treba pristup, koristi SSH tunnel ili VPN, ne `ports:`.

**Tipičan prod deployment flow**:

```sh
git push
↓
# CI/CD pipeline builduje image
↓
docker build --platform linux/amd64 -t registry/myapp:1.2.3 .
docker push registry/myapp:1.2.3
↓
#prod server:
docker compose pull
docker compose up -d
```

Nema uploadovanja koda, nema reinstaliranja pip-a na serveru — sve je u image-u.

## Volumes

**Gde se stvarno nalaze podaci — u kontejneru ili na host-u?**

**Kratak odgovor:** Ako koristiš **volume**, podaci se fizički nalaze na **host** mašini, u Docker storage prostoru - ne u **kontejneru**.

Kontejner "vidi" u svom fajl sistemu `/var/lib/postgresql/data`, ali to je **mount point**, a ne pravi folder.

Znači:

```sh
CONTAINER PATH:  /var/lib/postgresql/data   (virtualni mount)
HOST PATH:       /var/lib/docker/volumes/postgres_data/_data/
```

Pa dakle:

 Mesto             | Sadrži prave fajlove? | Traje posle gašenja kontejnera?
 ----------------- | --------------------- | -----------------------------------
 U kontejneru      | ne                    | briše se kad se obriše kontejner
 U Docker volume-u | da                    | ostaje i kada obrišeš kontejner

**Dakle prvi deo je host folder, drugi deo je kontejnerski folder?**

Tačno.

```sh
volumes:
  - postgres_data:/var/lib/postgresql/data
```

Razložimo ovako:

 Deo                        | Značenje
--------------------------- | -----------------------------------------
 `postgres_data`            | ime volume-a na **hostu**
 `/var/lib/postgresql/data` | folder u **kontejneru** gde Postgres piše

Ali u kontejneru taj folder zapravo nije pravi folder — nego **mount point**.

**Da li se podaci nalaze na oba mesta?**

Ne. Podaci se *fizički* nalaze samo na **hostu**, u Docker-ovom delu filesystem-a.
Kontejner ih samo vidi kao da su unutra, ali ih ne poseduje.

Ako uđeš u kontejner:

```bash
docker exec -it mydb bash
ls /var/lib/postgresql/data
```

→ vidiš fajlove, ali oni nisu unutra, nego su mount-ovani spolja.

**Kako se kontrolišu ti podaci u Docker host fajl sistemu?**

Docker vodi evidenciju volume-a kao zasebnih objekata:

```bash
docker volume ls
docker volume inspect postgres_data
```

Ti možeš:

 Radnja                       | Komanda
------------------------------|--------------------------------------
 Listanje volume-a            | `docker volume ls`
 Detalji o volume-u           | `docker volume inspect postgres_data`
 Ručno brisanje               | `docker volume rm postgres_data`
 Obrisati sve „viseće“ volume | `docker volume prune`
 Obrisati sve sa kontejnerima | `docker compose down -v`

Dakle, **volume** je Docker resurs kao i **container** i **image**. Svaki ima životni ciklus i moraš ga ručno brisati ako ga više ne želiš.

**Ako želim da sve obrišem, moram: image, container, volume?**

Da — potpuno ispravno shvataš:

```sh
docker rm <container>         # obriše kontejner, ali ne i podatke
docker rmi <image>            # obriše image, ali ne i kontejnere ni podatke
docker volume rm <volume>     # obriše podatke
```

Ako hoćeš **totalni reset** celog compose projekta:

```bash
docker compose down --volumes --rmi all
```

To uradi sve:

- ugasi kontejnere
- obriše kontejnere
- obriše image-e
- obriše volume-e (znači i bazu!)

**Gde je korist?**

Evo realnog pogleda:

Okruženje           | Da li koristiti volume za DB? | Zašto
------------------- | ------------------------------|-------------------------------------------
Production          | OBAVEZNO                      | inače svaka deploy akcija briše sve podatke
Local dev           | Da, ali samo za DB, ne za app | da ne resetuješ bazu svaki put
CI / test pipelines | NE                            | svaki run ima fresh DB, pa se koriste tmpfs ili init scripts

- Za Python app kod — ne koristiš **volume**, nego **bind mount** (`.:/app`) u dev-u.
- Za bazu — koristiš **volume**, jer niko neće ručno praviti tabelu 200 puta.
- **Volume** ima smisla za **stateful** podatke.
- **Container** je **ephemeral**, volume je **persistent**.
- Ono najbitnije: **Volumes** postoje da bi kontejneri mogli da budu **disposable**.

Docker filozofija je:

```sh
containers = always replaceable
volumes = never replace automatically
images = immutable
```

Ako bi baza bila unutar kontejnera → DB = disposable → neupotrebljivo.

## Kako napraviti SQL dump iz Docker PostgreSQL kontejnera

**Opcija A - `pg_dump` unutar DB kontejnera**:

```sh
docker exec -t mydb pg_dump -U user mydb > backup.sql
```

Objašnjenje:

 Deo                    | Značenje
 ---------------------- | -----------------------------------
 `docker exec -t mydb`  | pokreće komandu u kontejneru `mydb`
 `pg_dump -U user mydb` | pravi dump baze `mydb`
 `> backup.sql`         | snima ga na host mašinu

- radi uvek, nezavisno od volume-a
- SQL fajl ostaje u tvom projektu - ne u Docker storage-u.

**Opcija B — dump direktno iz volume-a - bez kontejnera**:

Ako kontejner ne radi ili želiš samo “pick-up” podataka:

```sh
docker run --rm \
  -v postgres_data:/var/lib/postgresql/data \
  -v $(pwd):/backup \
  alpine sh -c "apk add --no-cache postgresql-client && \
  pg_dump -U user -h db mydb > /backup/backup.sql"
```

- ne treba da ti radi glavni DB kontejner
- koristi **volume mount** da vidi podatke
- i dalje mora da postoji mreža i DB servis ako dump radi live

**Opcija C — napravi RAW backup celog volume-a (ne SQL dump)**:

```sh
docker run --rm -v postgres_data:/data -v $(pwd):/backup alpine tar czf /backup/postgres_volume_backup.tar.gz -C /data .
```

- pravi arhivu celog volume-a (uključujući WAL, config, sve fajlove)
- To je **file-level backup**, ne SQL dump (ima i prednosti i mana)

## Kako backup-ovati i restore-ovati ceo Docker volume

**Backup named volume → tar fajl**:

```sh
docker run --rm \
  -v postgres_data:/volume \
  -v $(pwd):/backup \
  alpine \
  tar czf /backup/postgres_data.tar.gz -C /volume .
```

Rezultat: `postgres_data.tar.gz` se nalazi u trenutnom folderu na hostu.
To je **pravi backup** koji možeš poneti na drugi server.

**Restore volume iz tar fajla**:

- Prvo kreiraš novi (prazan) volume:

  ```sh
  docker volume create postgres_data
  ```

  **Zatim ga napuniš**:
  
  ```sh
  docker run --rm \
    -v postgres_data:/volume \
    -v $(pwd):/backup \
    alpine \
    sh -c "cd /volume && tar xzf /backup/postgres_data.tar.gz"
  ```

- Sada je volume restauriran
- Možeš da pokreneš novi DB kontejner nad istim volume-om:

  ```bash
  docker run --rm -it \
    -v postgres_data:/var/lib/postgresql/data \
    postgres:16
  ```

Ako se podaci vide → restore je uspeo.

**Razlika: dump SQL vs backup volume**:

 Metod                 | Šta čuva                 | Korišćenje                              | Portabilnost
 --------------------- | ------------------------ | --------------------------------------- | -------------------------------------
 `pg_dump`             | samo SQL (schema + data) | migracija, verzije, CI/CD               | laka
 Volume backup (`tar`) | fizički fajlove baze     | disaster recovery, move whole DB engine |  mora odgovarati verzija PostgreSQL
 `pg_dumpall`          | sve baze + users, roles  | full logical export                     | dobro za migracije

**Kada koristiš šta?**

 Scenario                                         | Preporučeno
 ------------------------------------------------ | ------------------------------
 Želim da migriram bazu na drugi server           | `pg_dump`
 Želim da snapshotujem stanje baze tokom dev-a    | volume backup (`tar`)
 Želim CI/CD migracije                            | `pg_dump` + migrations
 Želim 1-klik restore tačke (kao snapshot)        | volume backup
 Želim da menjam verziju PostgreSQL (npr 14 → 16) | `pg_dump` (nikako RAW volume!)
 Želim samo da pošaljem dump nekome               | `backup.sql`

## DB-ops toolkit za Docker PostgreSQL

- `backup.sh` – pravi **SQL dump** (`pg_dump`)
- `restore.sh` – vraća dump u tekuću bazu
- `volume-backup.sh` – pravi **tar.gz snapshot celog volume-a**
- `volume-restore.sh` – vraća ceo Docker volume iz `tar.gz`

Bonus: cron-style `auto-backup.sh` (rotira backup-e po datumu)

I sve to ću napisati tako da radi cross-platform (Linux/Mac), i da je spremno za `.env` varijable, da ti ne budu hardcodovane lozinke u skriptama.

**Predlog folder strukture**:

```sh
db-tools/
│── backup.sh               # SQL dump
│── restore.sh              # load from .sql
│── volume-backup.sh        # tar backup
│── volume-restore.sh       # volume restore
│── auto-backup.sh          # cron-style daily backup
│── .env                    # DB_HOST, DB_USER, DB_NAME...
```

**`backup.sh` (pg_dump → backup.sql)**

```sh
#!/usr/bin/env bash
set -e

source .env

OUTFILE="backup_$(date +%F_%H-%M-%S).sql"

echo "Dumping database '${DB_NAME}' from container '${DB_CONTAINER}'..."
docker exec -t $DB_CONTAINER pg_dump -U $DB_USER $DB_NAME > $OUTFILE

echo "Backup created: $OUTFILE"
```

**`restore.sh` (psql < file.sql)**

```sh
#!/usr/bin/env bash
set -e

source .env

if [ -z "$1" ]; then
    echo "Usage: ./restore.sh <backup.sql>"
    exit 1
fi

FILE=$1

echo "Restoring '$FILE' into DB '$DB_NAME' (container: $DB_CONTAINER)..."
cat "$FILE" | docker exec -i $DB_CONTAINER psql -U $DB_USER $DB_NAME

echo "Restore complete."
```

**`volume-backup.sh` (full raw volume tar.gz snapshot)**:

```bash
#!/usr/bin/env bash
set -e

source .env

ARCHIVE="volume_$(date +%F_%H-%M-%S).tar.gz"

echo "Creating volume backup of '$DB_VOLUME' → $ARCHIVE..."
docker run --rm \
  -v ${DB_VOLUME}:/volume \
  -v $(pwd):/backup \
  alpine sh -c "cd /volume && tar czf /backup/$ARCHIVE ."

echo "Volume archive created: $ARCHIVE"
```

**`volume-restore.sh` (restore full volume)**

```bash
#!/usr/bin/env bash
set -e

source .env

if [ -z "$1" ]; then
    echo "Usage: ./volume-restore.sh <archive.tar.gz>"
    exit 1
fi

ARCHIVE=$1

echo "Removing existing volume '$DB_VOLUME'..."
docker volume rm -f $DB_VOLUME || true

echo "Creating empty volume '$DB_VOLUME'..."
docker volume create $DB_VOLUME > /dev/null

echo "Restoring from archive '$ARCHIVE' into volume..."
docker run --rm \
  -v ${DB_VOLUME}:/volume \
  -v $(pwd):/backup \
  alpine sh -c "cd /volume && tar xzf /backup/$ARCHIVE"

echo "Volume restored."
echo "If DB container is running, restart it manually."
```

**BONUS: `auto-backup.sh` (rotating daily backup)**

```bash
#!/usr/bin/env bash
set -e

source .env

BACKUP_DIR="./auto_backups"
mkdir -p "$BACKUP_DIR"

OUTFILE="$BACKUP_DIR/backup_$(date +%F).sql"

echo "Auto-backup running..."
docker exec -t $DB_CONTAINER pg_dump -U $DB_USER $DB_NAME > "$OUTFILE"

echo "Saved: $OUTFILE"

# Optional: cleanup backups older than N days
find "$BACKUP_DIR" -type f -mtime +7 -delete
```

Pokreneš ga ručno, ili ubaciš u cron:

```sh
0 3 * * * cd /path/to/db-tools && ./auto-backup.sh >> backup.log 2>&1
```

**`.env` primer**

```env
DB_CONTAINER=mydb
DB_NAME=mydb
DB_USER=user
DB_VOLUME=postgres_data
```
