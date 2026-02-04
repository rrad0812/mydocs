
# SQLAlchemy

SQLAlchemy je najpopularniji ORM (Object Relational Mapper) u Pythonu. Omogućava ti da radiš sa bazom kroz Python objekte, umesto da pišeš SQL ručno.

## Osnovni pojmovi

- `Engine` – konekcija ka bazi (npr. SQLite, PostgreSQL, MySQL).
- `Session` – "radna sesija" koja drži transakcije i objekte dok ne pozoveš `commit()`.
- `Model` – Python klasa koja predstavlja tabelu u bazi.
- `Query` – način da izvlačiš podatke iz baze.

## Početak rada sa SQLAlchemy

### Instalacija

```sh
pip install sqlalchemy
```

### Definisanje modela

```py
from sqlalchemy import create_engine, Column, Integer, String
from sqlalchemy.orm import declarative_base, sessionmaker

# --- Engine (konekcija sa bazom) ---
engine = create_engine("sqlite:///example.db", echo=True)

# --- Bazna klasa za modele ---
Base = declarative_base()

# --- Model (tabela) ---
class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    age = Column(Integer)

    def __repr__(self):
        return f"User(id={self.id}, name='{self.name}', age={self.age})"

# --- Kreiraj tabele ---
Base.metadata.create_all(engine)
```

Ovo ti kreira SQLite bazu `example.db` sa tabelom `users`.

### Session (rad sa bazom)

```py
Session = sessionmaker(bind=engine)
session = Session()
```

### Dodavanje podataka

**Novi korisnik**:

```py
new_user = User(name="Pera", age=30)

session.add(new_user)   # ubaci u transakciju
session.commit()        # potvrdi u bazi
```

### Čitanje podataka

**Svi korisnici**:

```py
users = session.query(User).all()
print(users)
```

**Filtriranje**:

```py
pera = session.query(User).filter_by(name="Pera").first()
print(pera)
```

### Ažuriranje podataka

```py
pera.age = 31
session.commit()
```

### Brisanje podataka

```py
session.delete(pera)
session.commit()
```

### Ukratko

- Definišeš `model` (klasu) <=> tabela.
- Napraviš `session` da radiš sa bazom.
- Dodaješ/čitaš/menjaš/brišeš objekte kao obične Python objekte.

## Primer sa relacijom 1:N

> Jedan korisnik ima više postova.

### Setup 1:n relacije

```py
from sqlalchemy import create_engine, Column, Integer, String, ForeignKey
from sqlalchemy.orm import declarative_base, relationship, sessionmaker

# --- Konekcija na SQLite bazu ---
engine = create_engine("sqlite:///blog.db", echo=True)
Base = declarative_base()

# --- Session ---
Session = sessionmaker(bind=engine)
session = Session()
```

### Definicija modela za 1:n relaciju

```py
class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    # Relacija 1:N (User → Post)
    posts = relationship("Post", back_populates="author")

    def __repr__(self):
        return f"User(id={self.id}, name='{self.name}')"


class Post(Base):
    __tablename__ = "posts"

    id = Column(Integer, primary_key=True)
    title = Column(String)
    content = Column(String)

    user_id = Column(Integer, ForeignKey("users.id"))

    # Veza ka User modelu
    author = relationship("User", back_populates="posts")

    def __repr__(self):
        return f"Post(id={self.id}, title='{self.title}')"

# Kreiranje tabela
Base.metadata.create_all(engine)
```

### Ubacivanje podataka u 1:n relaciju

**Novi korisnik**:

```py
pera = User(name="Pera")
```

**Dodaj u 1: n relaciju**:

```py
pera.posts = [
    Post(title="Prvi post", content="Ovo je moj prvi post."),
    Post(title="Drugi post", content="Ovo je moj drugi post."),
]

session.add(pera)
session.commit()
```

### Čitanje iz relacija 1:n

**Nađi korisnika u 1:n relaciji**:

```py
pera = session.query(User).filter_by(name="Pera").first()
print(pera)
print("Postovi:", pera.posts)  # lista povezanih postova

# Nađi post i vidi autora
post = session.query(Post).first()
print(post)
print("Autor:", post.author)
```

### Rezultat 1:n relacije

Kada pokreneš ovaj kod:

- Kreiraće se tabele users i posts.
- Pera će imati 2 posta.
- Možeš iz User da ideš ka Post i obrnuto.

## N:M relacija

User ↔ Group (korisnik može biti u više grupa, grupa može imati više korisnika).

### Setup n:m relacije

```py
from sqlalchemy import create_engine, Column, Integer, String, Table, ForeignKey
from sqlalchemy.orm import declarative_base, relationship, sessionmaker

engine = create_engine("sqlite:///nm.db", echo=True)
Base = declarative_base()
Session = sessionmaker(bind=engine)
session = Session()
```

### Definisanje asociativne table za n:m relaciju

Za N:M relacije u SQLAlchemy se koristi pomoćna tabela:

```py
user_group = Table(
    'user_group',
    Base.metadata,
    Column('user_id', Integer, ForeignKey('users.id')),
    Column('group_id', Integer, ForeignKey('groups.id'))
)
```

### Definisanje modela za n:m relaciju

```py
class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    # Relacija N:M sa Group
    groups = relationship( "Group", secondary=user_group,
        back_populates="users"
    )

    def __repr__(self):
        return f"User(id={self.id}, name='{self.name}')"


class Group(Base):
    __tablename__ = "groups"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    # Relacija N:M sa User
    users = relationship("User", secondary=user_group, 
        back_populates="groups")

    def __repr__(self):
        return f"Group(id={self.id}, name='{self.name}')"

# Kreiranje tabela
Base.metadata.create_all(engine)
```

### Ubacivanje podataka u n:m relaciju

```py
# Kreiraj korisnike
pera = User(name="Pera")
mika = User(name="Mika")

# Kreiraj grupe
admin = Group(name="Admin")
user = Group(name="User")

# Dodaj korisnike u grupe
pera.groups = [admin, user]  # Pera je u obe grupe
mika.groups = [user]         # Mika je samo u User grupi

session.add_all([pera, mika, admin, user])
session.commit()
```

### Čitanje podataka n:m relacije

**Nađi korisnika i njegove grupe u n:m relaciji**:

```py
pera = session.query(User).filter_by(name="Pera").first()
print(pera)
print("Grupe:", pera.groups)
```

**Nađi grupu i njene korisnike u n:m relaciji**:

```py
user_group_obj = session.query(Group).filter_by(name="User").first()
print(user_group_obj)
print("Korisnici:", user_group_obj.users)
```

### Rezultat n:m relacije

- SQLAlchemy automatski vodi evidenciju kroz pomoćnu tabelu `user_group`.
- Možeš da ideš sa korisnika na grupe i obrnuto, sve Python objektima.
- Ovo je standardni način za N:M relacije u SQLAlchemy.

## declarative_base vs DeclarativeBase

### declarative_base

`declarative_base()` je jedan od najčešćih načina da se grade modeli u SQLAlchemy, ali nije jedini. Evo kako to izgleda:

```py
from sqlalchemy.orm import declarative_base

Base = declarative_base()

class User(Base):
    __tablename__ = "users"
    
    id = Column(Integer, primary_key=True)
    name = Column(String)
```

Ovde:

- `Base` je metaklasa – svaki model koji je nasleđuje se registruje u `Base.metadata`. Kasnije kad pozoveš `Base.metadata.create_all(engine)`, SQLAlchemy zna koje tabele da napravi.

Ovo je "stari" stil, ali i dalje super popularan i radi u SQLAlchemy 2.x.

### DeclarativeBase

Od verzije 2.0 postoji elegantniji način:

```py
from sqlalchemy.orm import DeclarativeBase

class Base(DeclarativeBase):
    pass

class User(Base):
    __tablename__ = "users"
    
    id = Column(Integer, primary_key=True)
    name = Column(String)
```

Prednost:

- Tipovi su jasniji (bolja podrška za IDE i mypy).
- Više u duhu modernog Pythona.
- Ako praviš veće projekte i koristiš SQLAlchemy 2.x, ovo bih preporučio.

### Poređenje

| Stari stil (declarative_base())    | Novi stil (DeclarativeBase)
|------------------------------------|-----------------------------------------
| Funkcija koja vraća Base klasu     | Klasa koju sam definišeš
| Široko korišćen, gomila tutorijala | Moderniji, preporučen za nove projekte
| Dobro radi sa starim kodom         | Bolje radi sa type-hinting alatima

### Preporuka

- Ako učiš i praviš prvi projekat, slobodno koristi `declarative_base()` jer ima više primera i resursa.
- Ako krećeš od nule i koristiš SQLAlchemy 2.x, idi odmah na DeclarativeBase – čistije je i dugoročno bolje.

## Modeli User, Post, Group u oba stila

Hajde da uradimo isti model User–Post (1:N) i User–Group (N:M) u oba stila, pa da jasno vidiš razliku.

### Stari stil – declarative_base()

```py
from sqlalchemy import create_engine, Column, Integer, String, ForeignKey, Table
from sqlalchemy.orm import declarative_base, relationship, sessionmaker

engine = create_engine("sqlite:///oldstyle.db", echo=True)
Base = declarative_base()
Session = sessionmaker(bind=engine)
session = Session()

# N:M pomoćna tabela
user_group = Table(
    "user_group",
    Base.metadata,
    Column("user_id", Integer, ForeignKey("users.id")),
    Column("group_id", Integer, ForeignKey("groups.id"))
)

class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    posts = relationship("Post", back_populates="author")
    groups = relationship("Group", secondary=user_group, 
        back_populates="users")

class Post(Base):
    __tablename__ = "posts"

    id = Column(Integer, primary_key=True)
    title = Column(String)
    user_id = Column(Integer, ForeignKey("users.id"))

    author = relationship("User", back_populates="posts")

class Group(Base):
    __tablename__ = "groups"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    users = relationship("User", secondary=user_group, back_populates="groups")

Base.metadata.create_all(engine)
```

Ovde sve ide preko klase `Base = declarative_base()`.

### Novi stil – DeclarativeBase (SQLAlchemy 2.x)

```py
from sqlalchemy import create_engine, Column, Integer, String, ForeignKey, Table
from sqlalchemy.orm import DeclarativeBase, relationship, sessionmaker, Mapped, mapped_column

engine = create_engine("sqlite:///newstyle.db", echo=True)
Session = sessionmaker(bind=engine)
session = Session()

# Nova bazna klasa
class Base(DeclarativeBase):
    pass

# N:M pomoćna tabela
user_group = Table(
    "user_group",
    Base.metadata,
    Column("user_id", ForeignKey("users.id"), primary_key=True),
    Column("group_id", ForeignKey("groups.id"), primary_key=True)
)

class User(Base):
    __tablename__ = "users"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(String)

    posts: Mapped[list["Post"]] = relationship("Post", back_populates="author")
    groups: Mapped[list["Group"]] = relationship("Group", secondary=user_group, 
        back_populates="users")

class Post(Base):
    __tablename__ = "posts"

    id: Mapped[int] = mapped_column(primary_key=True)
    title: Mapped[str] = mapped_column(String)
    user_id: Mapped[int] = mapped_column(ForeignKey("users.id"))

    author: Mapped["User"] = relationship("User", back_populates="posts")

class Group(Base):
    __tablename__ = "groups"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(String)

    users: Mapped[list["User"]] = relationship("User", secondary=user_group, 
        back_populates="groups")

Base.metadata.create_all(engine)
```

Ovde koristiš:

- `DeclarativeBase` umesto `declarative_base()`.
- `Mapped[]` + `mapped_column()` za tipove (super korisno sa IDE/mypy).

Isto ponašanje, ali moderniji i čitljiviji API.

### Razlika u praksi

- Stari stil → kraći kod, ali bez type-hintova.
- Novi stil → eksplicitno kažeš tipove (Mapped[int], Mapped[str]) i
  dobićeš autocomplete + type-checking u editoru.

## Primer: Insert + Query (SQLAlchemy 2.x stil)

```py
from sqlalchemy import create_engine, String, ForeignKey, Table
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship, sessionmaker

# Engine & Session
engine = create_engine("sqlite:///example.db", echo=True)
Session = sessionmaker(bind=engine)

# Base klasa
class Base(DeclarativeBase):
    pass

# N:M pomoćna tabela
user_group = Table(
    "user_group",
    Base.metadata,
    mapped_column("user_id", ForeignKey("users.id"), primary_key=True),
    mapped_column("group_id", ForeignKey("groups.id"), primary_key=True)
)

# Modeli
class User(Base):
    __tablename__ = "users"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(String)

    posts: Mapped[list["Post"]] = relationship(back_populates="author")
    groups: Mapped[list["Group"]] = relationship(secondary=user_group, back_populates="users")

    def __repr__(self):
        return f"User(id={self.id}, name={self.name})"

class Post(Base):
    __tablename__ = "posts"

    id: Mapped[int] = mapped_column(primary_key=True)
    title: Mapped[str] = mapped_column(String)
    user_id: Mapped[int] = mapped_column(ForeignKey("users.id"))

    author: Mapped["User"] = relationship(back_populates="posts")

    def __repr__(self):
        return f"Post(id={self.id}, title={self.title})"

class Group(Base):
    __tablename__ = "groups"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(String)

    users: Mapped[list["User"]] = relationship(secondary=user_group, back_populates="groups")

    def __repr__(self):
        return f"Group(id={self.id}, name={self.name})"

# Kreiranje šema
Base.metadata.create_all(engine)

# Sesija
session = Session()

# Insert podataka
pera = User(name="Pera")
mika = User(name="Mika")

admin = Group(name="Admin")
general = Group(name="General")

pera.posts = [
    Post(title="Pera prvi post"),
    Post(title="Pera drugi post")
]

mika.posts = [
    Post(title="Mika jedini post")
]

pera.groups = [admin, general]
mika.groups = [general]

session.add_all([pera, mika])
session.commit()
```

### Upiti

```py
# Upit za sve korisnike
print("\n--- Users ---")
for u in session.query(User).all():
    print(u, "Posts:", u.posts, "Groups:", u.groups)

# Nađi jednog korisnika
print("\n--- Find Pera ---")
pera = session.query(User).filter_by(name="Pera").first()
print(pera, pera.posts, pera.groups)

# Svi postovi
print("\n--- Posts ---")
for p in session.query(Post).all():
    print(p, "Author:", p.author)
```

**Šta ćeš dobiti?**

- `User → Post` (1:N) i `User → Group` (N:M) relacije rade kroz Python objekte.
- Kad nadješ korisnika (Peru), automatski dobijaš njegove i `posts` i `groups`.
- Sve se ponaša kao ORM, nema ručnog pisanja SQL-a.

## Relacija u SQLAlchemy-u je uvek dvosmerna

- User ima `posts` (lista postova).
- Post ima `author` (jedan korisnik).

Ako ne povežeš te dve strane, ORM ne zna da su one deo iste veze – tretira ih odvojeno.

### Primer bez back_populates

```py
class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    posts = relationship("Post")  # nema veze sa 'author'


class Post(Base):
    __tablename__ = "posts"

    id = Column(Integer, primary_key=True)
    title = Column(String)
    user_id = Column(Integer, ForeignKey("users.id"))

    author = relationship("User")  # nema veze sa 'posts'
```

Ovdje `User.posts` i `Post.author` nisu povezani.

Kad dodaš post u `user.posts`, SQLAlchemy ne zna da automatski postavi `post.author`. Moraš oba polja sam da održavaš.

### Primer sa back_populates

```py
class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True)
    name = Column(String)

    posts = relationship("Post", back_populates="author")

class Post(Base):
    __tablename__ = "posts"

    id = Column(Integer, primary_key=True)
    title = Column(String)
    user_id = Column(Integer, ForeignKey("users.id"))

    author = relationship("User", back_populates="posts")
```

Sada su povezani.

- Kad ubaciš post u `user.posts`, ORM automatski postavi `post.author =
  user`.
- Kad postaviš `post.author = user`, ORM automatski ubaci post u `user.
  posts`.

To je "dvosmerna sinhronizacija".

### Primer korišćenja

```py
pera = User(name="Pera")
post1 = Post(title="Prvi post")

pera.posts.append(post1)

print(post1.author)  # automatski -> User(name="Pera")
```

Bez back_populates, ovo ne bi radilo – `post1.author` bi ostao `None` dok ručno ne postaviš.

### Ukratko o back_populates

- `back_populates` - kaže SQLAlchemy-u da su ova dva polja dve strane iste
  veze. Bez toga ORM ne može da ih automatski sinhronizuje.
