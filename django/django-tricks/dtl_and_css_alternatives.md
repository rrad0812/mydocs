
# Alternativa za DTL i CSS

`Django templates + CSS` (posebno ako pokušaš da sve radiš „ručno“) umeju da budu frustrirajući. Evo nekoliko opcija koje ti mogu olakšati život, zavisno od toga šta te najviše muči:

- Ako ti je Django Template Language (DTL) smor pokušaj sa sledćim altern

  Alternativa:
  - `Jinja2` (sličan DTL-u, ali fleksibilniji i sa boljim sintaksama).
  - Ili frontend framework (`React`, `Vue`, `Svelte`) + `Django REST`  
    Framework kao backend.

- Ako ti je `CSS` noćna mora

  - `TailwindCSS` - odličan za Django projekte. Instalacija je laka, a
    posle pišeš stilove kroz klase direktno u HTML (nema više jurcanja po ogromnim .css fajlovima).

  - `Bootstrap` - ako želiš gotove komponente (forme, dugmad, gridevi)
    bez previše filozofije.

  - `Django-Crispy-Forms` - ako ti je muka da ručno stilizuješ forme.

- Ako ti treba brzi start

  - `django-tailwind` paket ti omogućava da ubaciš Tailwind direktno u
    Django projekat.
  - `django-allauth` reši login/registraciju da ne kucaš sve ručno.
  - `django-debug-toolbar` ti olakšava razvoj

Ako praviš nešto samo za sebe ili interni alat, `Admin + malo CSS-a` je sasvim dovoljno. Ako praviš nešto za ljude spolja (public-facing), bolje je da uložiš trud u frontend, makar u `Bootstrap` ili `Tailwind`, da ne lomiš glavu s „čistim“ CSS-om.

Evo ti jednostavan step-by-step vodič kako da ubaciš `TailwindCSS` u Django
projekat tako da odmah imaš moderni izgled, bez glavobolje sa čistim CSS-om.

## Instaliraj paket

U `root` folderu projekta (gde je `manage.py`), pokreni:

```sh
pip install django-tailwind
```

Dodaj app u `INSTALLED_APPS` u `settings.py`:

```py
INSTALLED_APPS = [
  ...
  "tailwind",
  "theme",   # naziv app-a koji ćemo napraviti
]
```

Kreiraj Tailwind "theme" app

```sh
python manage.py tailwind init theme
```

Ovo će napraviti app theme/ gde stoji Tailwind konfiguracija.
U `settings.py` dodaj:

```py
TAILWIND_APP_NAME = 'theme'
```

Instaliraj `Node.js` zavisnosti

Idi u folder `theme/` i pokreni:

```sh
npm install
```

Ovo povuče Tailwind i potrebne alate.

## Pokreni Tailwind watcher

```sh
python manage.py tailwind start
```

To će generisati CSS fajl i automatski ga obnavljati dok radiš.

## Uključi Tailwind u base template

U `theme/static/css/dist/styles.css` se nalazi generisani CSS.
U `base.html` dodaj u `<head>`:

```html
<link rel="stylesheet" href="{% static 'css/dist/styles.css' %}">
```

Ne zaboravi da u `settings.py` imaš `STATICFILES_DIRS` i `{% load static %}` u templejtu.

## Test

Sad možeš da probaš recimo:

```html
<div class="min-h-screen flex items-center justify-center bg-gray-100">
  <div class="p-6 max-w-sm bg-white rounded-2xl shadow-lg">
    <h1 class="text-2xl font-bold mb-4">Ćao, Django + Tailwind!</h1>
    <p class="text-gray-600">Radi! 🎉</p>
  </div>
</div>
```

Imaćeš odmah lep, moderan box bez da si pisao liniju čistog CSS-a.

Evo ti gotov `base.html` koji možeš odmah da ubaciš u svoj Django projekat.
Ovo je jednostavan layout sa `navbarom` i `content` containerom, koristi Tailwind:

```html
{% load static %}

<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{% block title %}Moj sajt{% endblock %}</title>
  <link rel="stylesheet" href="{% static 'css/dist/styles.css' %}">
</head>
<body class="bg-gray-100 text-gray-800">
  
  <!-- NAVBAR -->
  <nav class="bg-white shadow-md">
    <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
      <div class="flex justify-between h-16">
        <div class="flex">
          <a href="/" class="flex items-center text-xl font-bold text-blue-600">
            🚀 MojSajt
          </a>
        </div>
        <div class="flex space-x-4 items-center">
          <a href="/" class="text-gray-700 hover:text-blue-600">Početna</a>
          <a href="/about/" class="text-gray-700 hover:text-blue-600">O nama</a>
          <a href="/contact/" class="text-gray-700 hover:text-blue-600">Kontakt</a>
        </div>
      </div>
    </div>
  </nav>

  <!-- CONTENT -->
  <main class="max-w-5xl mx-auto px-4 py-8">
    {% block content %}
    <h1 class="text-3xl font-bold mb-4">Dobrodošao! 🎉</h1>
    <p>Ovo je osnovni Django + Tailwind template.</p>
    {% endblock %}
  </main>

  <!-- FOOTER -->
  <footer class="bg-white shadow-inner mt-12">
    <div class="max-w-7xl mx-auto px-4 py-6 text-center text-gray-500 text-sm">
      &copy; {{ now|date:"Y" }} MojSajt. Sva prava zadržana.
    </div>
  </footer>

</body>
</html>
```

Sad sve što praviš ide u `{% block content %}`.

`Tailwind` klase ti daju responsive dizajn automatski (probaj da smanjiš browser – navbar ostaje čist).

To je u startu najveći „šok“ sa Tailwind-om – odjednom vidiš gomilu klasa u HTML-u i izgleda kao šifra iz „Matrixa“.

Ali – dobra vest je da: Ne moraš sve da znaš napamet. Ima jako dobra dokumentacija i resursi.

### Gde učiti Tailwind klase

Zvanična dokumentacija (najbolja i najpreglednija):
  <https://tailwindcss.com/docs>

Ima search bar – ukucaš šta ti treba (npr. „rounded“, „shadow“, „flex“, „grid“).

Svaka klasa ima preview u realnom vremenu.

Cheatsheet (sve na jednom mestu):
  <https://tailwindcomponents.com/cheatsheet/>
  <https://tailwindcheat.com/>

Ovo je super ako želiš brz pregled svih dostupnih klasa.

Praktični primeri:
  <https://tailwindcomponents.com/>
  <https://tailwindui.com/components>
 (plaćeni deo, ali ima i besplatnih).

Nađeš gotove navbar-e, kartice, forme, footere i samo copy/paste u svoj projekat.

### Kako da učiš „usput“

Počni od:

- Layout-a: flex, grid, justify-, items-, gap-.
- Boje: bg-gray-100, text-blue-600, border-red-500.
- Spacing: p-4, px-6, m-2.
- Tipografija: text-lg, font-bold, leading-relaxed.
- Dekoracija: rounded-lg, shadow-md, hover:bg-gray-200.

Najlakše je da gledaš gotove primere i menjaš brojke (p-2 → p-6, text-sm → text-2xl) da vidiš efekat.

Evo ti TailwindCSS survival kit – skup najčešćih klasa koje ćeš koristiti 80% vremena. Ako ovo pohvataš, bićeš „operativan“ i možeš da radiš skoro sve.

Layout

Flexbox

```css
flex           <!-- aktivira flex -->
flex-col       <!-- kolona -->
flex-row       <!-- red -->
justify-start / justify-center / justify-end / justify-between  
items-start / items-center / items-end  
gap-2 gap-4 gap-8   <!-- razmak -->
```

Grid

```css
grid grid-cols-2   <!-- 2 kolone -->
grid-cols-3        <!-- 3 kolone -->
gap-4              <!-- razmak između kolona/redova -->
```

Sizing

```css
w-full   <!-- širina 100% -->
w-1/2    <!-- pola širine -->
h-screen <!-- visina celog ekrana -->
min-h-screen <!-- minimalno visina celog ekrana -->
```

Boje

Tailwind koristi skalu boja (100–900):

```css
bg-gray-100   <!-- svetlo siva pozadina -->
bg-gray-800   <!-- tamna siva pozadina -->
text-blue-600 <!-- plavi tekst -->
border-red-500 <!-- crvena ivica -->
```

Spacing

Padding (unutrašnji razmak)

```css
p-4    <!-- svuda padding 1rem -->
px-6   <!-- horizontalno -->
py-2   <!-- vertikalno -->
```

Margin (spoljašnji razmak)

```css
m-4    <!-- margin svuda -->
mx-auto <!-- centriranje horiz. -->
mt-6    <!-- margin-top -->
mb-2    <!-- margin-bottom -->
```

Tipografija

```css
text-sm / text-base / text-lg / text-xl / text-2xl  
font-light / font-normal / font-bold  
text-center / text-left / text-right  
leading-tight / leading-relaxed
uppercase / lowercase / capitalize
```

Dekoracija & efekti

```css
rounded / rounded-lg / rounded-2xl   <!-- zaobljeni uglovi -->
shadow / shadow-md / shadow-lg       <!-- senke -->
border / border-2 / border-gray-300  <!-- ivice -->
hover:bg-gray-200 hover:text-blue-600
```

Komponente (kombinacije)

Dugme

```css
<button class="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
  Klikni me
</button>
```

Kartica

```css
<div class="p-6 bg-white rounded-xl shadow-md">
  <h2 class="text-xl font-bold mb-2">Naslov</h2>
  <p class="text-gray-600">Opis kartice...</p>
</div>
```

Form input

```css
<input type="text" class="w-full border border-gray-300 rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500">
```

Pro tip: Kad god ne znaš klasu idi na Tailwind docs i ukucaj „padding“, „shadow“, „font“, „grid“… odmah dobiješ primer.

Evo ti primer login forme u Django template-u koja koristi Tailwind.
Možeš je staviti u templates/login.html i extendovati iz base.html.

`# login.html`

```html
{% extends "base.html" %}
{% load static %}

{% block title %}Prijava{% endblock %}

{% block content %}
<div class="min-h-screen flex items-center justify-center">
  <div class="w-full max-w-md bg-white rounded-2xl shadow-lg p-8">
    
    <h2 class="text-2xl font-bold text-center text-gray-800 mb-6">Prijava</h2>
    
    <form method="post" class="space-y-4">
      {% csrf_token %}
      
      <!-- Username -->
      <div>
        <label for="username" class="block text-sm font-medium text-gray-700">Korisničko ime</label>
        <input type="text" name="username" id="username" 
               class="mt-1 block w-full rounded-lg border border-gray-300 px-3 py-2 
                      focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
               required>
      </div>
      
      <!-- Password -->
      <div>
        <label for="password" class="block text-sm font-medium text-gray-700">Lozinka</label>
        <input type="password" name="password" id="password" 
               class="mt-1 block w-full rounded-lg border border-gray-300 px-3 py-2 
                      focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
               required>
      </div>
      
      <!-- Dugme -->
      <button type="submit" 
              class="w-full py-2 px-4 bg-blue-600 text-white rounded-lg shadow-md 
                     hover:bg-blue-700 focus:outline-none focus:ring-2 
                     focus:ring-offset-2 focus:ring-blue-500">
        Uloguj se
      </button>
    </form>
    
    <!-- Linkovi -->
    <p class="mt-6 text-center text-sm text-gray-600">
      Nemaš nalog? 
      <a href="{% url 'signup' %}" class="text-blue-600 hover:underline">Registruj se</a>
    </p>
  </div>
</div>
{% endblock %}
```

Rezultat:

- Forma stoji centrirana na ekranu.
- Polja su zaobljena, imaju lep hover i fokus efekat.
- Dugme je moderno (plavo, hover animacija).

Hoćeš da ti odradim i verziju za Django crispy-forms + Tailwind, da ne moraš ručno da pišeš `<label>` i `<input>` za svako polje?

Evo kako možeš da koristiš Django Crispy Forms + Tailwind da se ne cimaš oko ručnog pisanja label/input polja.

- Instalacija

  Prvo instaliraj pakete:

```sh
  pip install django-crispy-forms crispy-tailwind
```

- Podesi `settings.py`

  Dodaj u `INSTALLED_APPS`:

```py
  INSTALLED_APPS = [
    ...
    "crispy_forms",
    "crispy_tailwind",
  ]
```

- I reci Crispy-ju da koristi Tailwind template pack:

```py
  CRISPY_ALLOWED_TEMPLATE_PACKS = "tailwind"
  CRISPY_TEMPLATE_PACK = "tailwind"
```

- Primer forme (forms.py)

```py
  from django import forms

  class LoginForm(forms.Form):
    username = forms.CharField(label="Korisničko ime", max_length=100)
    password = forms.CharField(label="Lozinka", widget=forms.PasswordInput)
```

- Template (login.html)

  Sada u template-u ne moraš ništa da pišeš ručno – samo koristiš {{ form|crispy }}:

```html
  {% extends "base.html" %}
  {% load crispy_forms_tags %}
  
  {% block title %}Prijava{% endblock %}
  
  {% block content %}
  <div class="min-h-screen flex items-center justify-center">
    <div class="w-full max-w-md bg-white rounded-2xl shadow-lg p-8">
  
      <h2 class="text-2xl font-bold text-center text-gray-800 mb-6">Prijava</h2>
  
      <form method="post">
        {% csrf_token %}
        {{ form|crispy }}
  
        <button type="submit" 
                class="w-full py-2 px-4 bg-blue-600 text-white rounded-lg shadow-md 
                       hover:bg-blue-700 focus:outline-none focus:ring-2 
                       focus:ring-offset-2 focus:ring-blue-500 mt-4">
          Uloguj se
        </button>
      </form>
  
      <p class="mt-6 text-center text-sm text-gray-600">
        Nemaš nalog? 
        <a href="{% url 'signup' %}" class="text-blue-600 hover:underline">Registruj se</a>
      </p>
    </div>
  </div>
  {% endblock %}
```

Rezultat:

- {{ form|crispy }} sam renderuje polja sa Tailwind klasama.

- Ti se brineš samo za layout oko forme i dugmad.

Hoćeš da ti pokažem i kako da u login view-u koristiš ovu formu (čak i sa Django-ovim LoginView), da imaš kompletan flow?

Evo kako da kompletiraš flow sa Django LoginView + Crispy Forms + Tailwind.

- Form (`# forms.py`)

  Ako već koristiš Django auth sistem, najbolje je da extends-uješ njegovu AuthenticationForm:

```py
  from django import forms
  from django.contrib.auth.forms import AuthenticationForm
  
  class TailwindLoginForm(AuthenticationForm):
      username = forms.CharField(
          label="Korisničko ime",
          widget=forms.TextInput(attrs={"placeholder": "Unesi korisničko ime"})
      )
      password = forms.CharField(
          label="Lozinka",
          widget=forms.PasswordInput(attrs={"placeholder": "Unesi lozinku"})
      )
```

- View (`# views.py`)

  Možeš iskoristiti ugrađeni LoginView i samo mu proslediti našu formu i template:

```py
  from django.contrib.auth.views import LoginView
  from .forms import TailwindLoginForm
  
  class UserLoginView(LoginView):
      template_name = "login.html"
      authentication_form = TailwindLoginForm
```

- URL-ovi (`# urls.py`)

Dodaj u urls.py:

```py
  from django.urls import path
  from .views import UserLoginView
  
  urlpatterns = [
      path("login/", UserLoginView.as_view(), name="login"),
  ]
```

- Template (`# login.html`)

  Već ga imamo (onaj sa {{ form|crispy }}). Za podsetnik:

```html
  {% extends "base.html" %}
  {% load crispy_forms_tags %}
  
  {% block title %}Prijava{% endblock %}
  
  {% block content %}
  <div class="min-h-screen flex items-center justify-center">
    <div class="w-full max-w-md bg-white rounded-2xl shadow-lg p-8">
      
      <h2 class="text-2xl font-bold text-center text-gray-800 mb-6">Prijava</h2>
      
      <form method="post">
        {% csrf_token %}
        {{ form|crispy }}
        <button type="submit" 
                class="w-full py-2 px-4 bg-blue-600 text-white rounded-lg shadow-md 
                       hover:bg-blue-700 focus:outline-none focus:ring-2 
                       focus:ring-offset-2 focus:ring-blue-500 mt-4">
          Uloguj se
        </button>
      </form>
  
      <p class="mt-6 text-center text-sm text-gray-600">
        Nemaš nalog? 
        <a href="{% url 'signup' %}" class="text-blue-600 hover:underline">Registruj se</a>
      </p>
    </div>
  </div>
  {% endblock %}
```

Rezultat:

- /login/ ruta radi automatski sa Django auth backendom.

- {{ form|crispy }} prikazuje lepo stilizovana polja sa Tailwind klasama.

- Nakon uspešne prijave, korisnik se po defaultu šalje na settings.  
  LOGIN_REDIRECT_URL.

Ali dobro, sad imaš:

- Base layout sa Tailwind
- Survival kit klasa
- Login forma (ručna verzija i crispy verzija)
- LoginView integraciju
