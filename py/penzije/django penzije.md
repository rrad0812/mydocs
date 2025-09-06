
# Kalkulator penzija u Republici Srbiji

## Varijanta komandna linija

Možemo napraviti mali program koji računa približnu visinu penzije u Republici Srbiji.

Da bi izračunali penziju, koristi se formula za obračun penzije prema `Zakonu o PIO`:

Osnovna formula:

*P= OLP x LPF x liicni_bod*:

Gde je:

- OLP - Opšti lični poen (objavljuje ga RF PIO svake godine, npr. 1.121,48 za 2024.)
- LPF - Lični ponder faktor (zavisi od godina staža i prosečnih zarada)
- Lični bod = zbir svih godišnjih bodova (računa se kao odnos prosečne plate i prosečne plate u Srbiji, pa pomnožen sa godinama staža).

Pošto pravi obračun može biti komplikovan, napravićemo pojednostavljeni model:

- Uneseš broj godina radnog staža.
- Uneseš prosečnu mesečnu platu tokom radnog veka.
- Program koristi prosečnu platu u Srbiji (npr. ~90.000 RSD u 2024.).

Izračuna se lični bod i približna penzija.

Evo primera koda u Pythonu:

```py
def izracunaj_penziju(godine_staza, prosecna_plata, 
    prosecna_plata_rs=90000, opsti_licni_poen=1121.48):
    
    # godišnji bod = plata / prosek
    godisnji_bod = prosecna_plata / prosecna_plata_rs

    # ukupan lični bod = godisnji_bod * godine_staza
    licni_bod = godisnji_bod * godine_staza
    
    # penzija = opšti lični poen * lični bod
    penzija = opsti_licni_poen * licni_bod
    
    return penzija

# primer
godine = int(input("Unesi broj godina staža: "))
plata = float(input("Unesi prosečnu mesečnu platu tokom radnog veka (RSD): "))

penzija = izracunaj_penziju(godine, plata)
print(f"Procenjena mesečna penzija iznosi: {penzija:.2f} RSD")
```

## GUI Tkinter varijanta

Napravićemo GUI verziju pomoću Tkinter-a (standardna Python biblioteka).
Korisnik će uneti:

- godine staža
- prosečnu platu tokom radnog veka

A aplikacija će izračunati i prikazati procenjenu penziju.

Evo koda:

```py
import tkinter as tk
from tkinter import messagebox

def izracunaj_penziju():
    try:
        godine_staza = int(entry_staz.get())
        prosecna_plata = float(entry_plata.get())

        # fiksne vrednosti (mogu se menjati po potrebi)
        prosecna_plata_rs = 90000    # prosečna plata u Srbiji
        opsti_licni_poen = 1121.48   # opšti lični poen
        
        # računanje
        godisnji_bod = prosecna_plata / prosecna_plata_rs
        licni_bod = godisnji_bod * godine_staza
        penzija = opsti_licni_poen * licni_bod
        
        messagebox.showinfo("Rezultat", f"Procenjena mesečna penzija: {penzija:.2f} RSD")
    except ValueError:
        messagebox.showerror("Greška", "Molim unesite ispravne brojeve!")

# GUI
root = tk.Tk()
root.title("Kalkulator penzije - Srbija")
root.geometry("400x250")

label_staz = tk.Label(root, text="Broj godina staža:")
label_staz.pack(pady=5)
entry_staz = tk.Entry(root)
entry_staz.pack(pady=5)

label_plata = tk.Label(root, text="Prosečna plata tokom radnog veka (RSD)  
    :")
label_plata.pack(pady=5)
entry_plata = tk.Entry(root)
entry_plata.pack(pady=5)

btn_izracunaj = tk.Button(root, text="Izračunaj penziju", 
    command=izracunaj_penziju)
btn_izracunaj.pack(pady=15)

root.mainloop()
```

Ovo će otvoriti prozor gde korisnik unosi podatke i klikne na dugme za izračunavanje.

Dodaću grafikon pomoću `matplotlib` biblioteke koji će prikazivati kako penzija raste u zavisnosti od prosečne plate.

Korisnik će:

- Uneti broj godina staža.
- Kliknuti na dugme.

Dobiti rezultat + grafikon zavisnosti penzije od prosečne plate.

Evo koda:

```py
import tkinter as tk
from tkinter import messagebox
import matplotlib.pyplot as plt

def izracunaj_penziju(godine_staza, prosecna_plata, prosecna_plata_rs=90000, opsti_licni_poen=1121.48):
    godisnji_bod = prosecna_plata / prosecna_plata_rs
    licni_bod = godisnji_bod * godine_staza
    penzija = opsti_licni_poen * licni_bod
    return penzija

def prikazi_rezultat():
    try:
        godine_staza = int(entry_staz.get())
        prosecna_plata = float(entry_plata.get())
        
        # izračunaj penziju
        penzija = izracunaj_penziju(godine_staza, prosecna_plata)
        
        # prikaži poruku
        messagebox.showinfo("Rezultat", f"Procenjena mesečna penzija: {penzija:.2f} RSD")
        
        # napravi grafikon
        plate = list(range(40000, 160000, 5000))  # od 40k do 150k
        penzije = [izracunaj_penziju(godine_staza, p) for p in plate]
        
        plt.figure(figsize=(8,5))
        plt.plot(plate, penzije, marker="o")
        plt.axvline(x=prosecna_plata, color="red", linestyle="--", label="Uneta plata")
        plt.axhline(y=penzija, color="green", linestyle="--", label="Izračunata penzija")
        plt.title(f"Penzija u zavisnosti od prosečne plate\nza {godine_staza} godina staža")
        plt.xlabel("Prosečna plata (RSD)")
        plt.ylabel("Mesečna penzija (RSD)")
        plt.legend()
        plt.grid(True)
        plt.show()
        
    except ValueError:
        messagebox.showerror("Greška", "Molim unesite ispravne brojeve!")

# GUI

root = tk.Tk()
root.title("Kalkulator penzije - Srbija")
root.geometry("420x260")

label_staz = tk.Label(root, text="Broj godina staža:")
label_staz.pack(pady=5)
entry_staz = tk.Entry(root)
entry_staz.pack(pady=5)

label_plata = tk.Label(root, text="Prosečna plata tokom radnog veka (RSD)
    :")
label_plata.pack(pady=5)
entry_plata = tk.Entry(root)
entry_plata.pack(pady=5)

btn_izracunaj = tk.Button(root, text="Izračunaj penziju i prikaži 
    grafikon", command=prikazi_rezultat)
btn_izracunaj.pack(pady=15)

root.mainloop()
```

## Verzija Django

Minimalan Django web‑projekat koji računa približnu mesečnu penziju po pojednostavljenom modelu i prikazuje 2 grafikona (penzija u zavisnosti od plate i u zavisnosti od godina staža). Dodali smo mogućnost preuzimanja CSV/Excel rezultata i stilizovali formu pomoću Bootstrap-a.

Napomena: Model je edukativan i uprošćen. Brojke poput prosečne zarade i opšteg ličnog poena su podesive u formi.

### Kreiraj i aktiviraj virtuelno okruženje (preporučeno)

```sh
python -m venv .venv

**Windows**:
.venv\Scripts\activate

# macOS / Linux
source .venv/bin/activate

# Instaliraj potrebne pakete
pip install "Django>=5.0,<6" openpyxl pandas

# 3) Kreiraj Django projekat i app
django-admin startproject penzija_project .
python manage.py startapp calculator
```

### Migracije i start

python manage.py migrate
python manage.py runserver

### Struktura

```sh
.
├─ manage.py
├─ penzija_project/
│  ├─ settings.py
│  ├─ urls.py
│  └─ ...
└─ calculator/
   ├─ urls.py
   ├─ views.py
   ├─ forms.py
   ├─ templates/
   │  └─ calculator/form.html
   └─ static/
      └─ calculator/styles.css
```

`# calculator/forms.py`

(isto kao ranije)

`# calculator/views.py`

Dodajemo dve nove funkcije za izvoz CSV i Excel.

```py
from django.shortcuts import render

years = form.cleaned_data['years_of_service']
avg_salary = form.cleaned_data['avg_salary']
rs_avg = form.cleaned_data['rs_avg_salary']
olp = form.cleaned_data['opsti_licni_poen']

pension_value = round(estimate_pension(years, avg_salary, rs_avg, olp), 2)

  salary_axis = list(range(40000, 160000, 5000))
  pensions_vs_salary = [
      round(estimate_pension(years, s, rs_avg, olp), 2) for s in salary_axis
  ]


  years_axis = list(range(5, 61, 1))
  pensions_vs_years = [
      round(estimate_pension(y, avg_salary, rs_avg, olp), 2) for y in years_axis
  ]


  context.update({
      'form': form,
      'result': {
          'pension_value': pension_value,
          'years': years,
          'avg_salary': float(avg_salary),
          'rs_avg': float(rs_avg),
          'olp': float(olp),
          'salary_axis': salary_axis,
          'pensions_vs_salary': pensions_vs_salary,
          'years_axis': years_axis,
          'pensions_vs_years': pensions_vs_years,
      }
  })
  else:
      context['form'] = form
else:
    context['form'] = PensionForm()

return render(request, 'calculator/form.html', context)

def export_csv(request):
    years = int(request.GET.get('years', 40))
    avg_salary = float(request.GET.get('avg_salary', 90000))
    rs_avg = float(request.GET.get('rs_avg', 90000))
    olp = float(request.GET.get('olp', 1121.48))

    years_axis = list(range(5, 61, 1))
    pensions = [estimate_pension(y, avg_salary, rs_avg, olp) for y in years_axis]

    df = pd.DataFrame({"Godine staža": years_axis, "Penzija (RSD)": pensions})
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="penzija.csv"'
    df.to_csv(path_or_buf=response, index=False)
    return response

def export_excel(request):
    years = int(request.GET.get('years', 40))
    avg_salary = float(request.GET.get('avg_salary', 90000))
    rs_avg = float(request.GET.get('rs_avg', 90000))
    olp = float(request.GET.get('olp', 1121.48))


    years_axis = list(range(5, 61, 1))
    pensions = [estimate_pension(y, avg_salary, rs_avg, olp) for y in years_axis]


    df = pd.DataFrame({"Godine staža": years_axis, "Penzija (RSD)": pensions})
    response = HttpResponse(content_type='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
    response['Content-Disposition'] = 'attachment; filename="penzija.xlsx"'
    df.to_excel(response, index=False)
    return response
```

`# calculator/urls.py`

```py
from django.urls import path
from .views import home, export_csv, export_excel

urlpatterns = [
    path('', home, name='home'),
    path('export/csv/', export_csv, name='export_csv'),
    path('export/excel/', export_excel, name='export_excel'),
]
```

`# calculator/templates/calculator/form.html`

Koristimo Bootstrap 5 sa CDN‑a:

```html
  <div class="form-text">{{ form.years_of_service.help_text }}</div>
  </div>
  <div class="col-md-6">
    {{ form.avg_salary.label_tag }}
    {{ form.avg_salary }}
  </div>
  <div class="col-md-6">
    {{ form.rs_avg_salary.label_tag }}
    {{ form.rs_avg_salary }}
  </div>
  <div class="col-md-6">
    {{ form.opsti_licni_poen.label_tag }}
    {{ form.opsti_licni_poen }}
    <div class="form-text">{{ form.opsti_licni_poen.help_text }}</div>
  </div>
  <div class="col-12">
    <button type="submit" class="btn btn-primary">Izračunaj</button>
  </div>
</form>

        {% if result %}
        <div class="alert alert-info mt-3">
          <strong>Procenjena mesečna penzija:</strong>
          {{ result.pension_value|floatformat:2 }} RSD
        </div>


        <a href="{% url 'export_csv' %}?years={{ result.years }}&avg_salary={{ result.avg_salary }}&rs_avg={{ result.rs_avg }}&olp={{ result.olp }}" class="btn btn-outline-secondary btn-sm">Preuzmi CSV</a>
        <a href="{% url 'export_excel' %}?years={{ result.years }}&avg_salary={{ result.avg_salary }}&rs_avg={{ result.rs_avg }}&olp={{ result.olp }}" class="btn btn-outline-secondary btn-sm">Preuzmi Excel</a>
        {% endif %}
      </div>

  {% if result %}
  <div class="row g-3">
    <div class="col-md-6">
      <div class="card p-3">
        <h2 class="h6">Penzija vs. plata</h2>
        <canvas id="chartSalary"></canvas>
      </div>
    </div>
    <div class="col-md-6">
      <div class="card p-3">
        <h2 class="h6">Penzija vs. godine staža</h2>
        <canvas id="chartYears"></canvas>
      </div>
    </div>
  </div>
  {% endif %}

  <footer class="text-center text-muted mt-4 small">
    Edukativni primer. Za stvarni obračun konsultovati RF PIO i važeće propise.
  </footer>
</div>

  {% if result %}
  <script>
    const salaryAxis = {{ result.salary_axis|safe }};
    const pensionsVsSalary = {{ result.pensions_vs_salary|safe }};

    const yearsAxis = {{ result.years_axis|safe }};
    const pensionsVsYears = {{ result.pensions_vs_years|safe }};

    new Chart(document.getElementById('chartSalary'), {
      type: 'line',
      data: { labels: salaryAxis, datasets: [{ label: 'Penzija (RSD)', data: pensionsVsSalary, tension: 0.2, pointRadius: 2, borderColor: '#0d6efd' }] },
    });

    new Chart(document.getElementById('chartYears'), {
      type: 'line',
      data: { labels: yearsAxis, datasets: [{ label: 'Penzija (RSD)', data: pensionsVsYears, tension: 0.2, pointRadius: 2, borderColor: '#198754' }] },
    });
  </script>
  {% endif %}
</body>
</html>
```

Sada dobijamo moderan Bootstrap izgled, dugmiće za preuzimanje rezultata u CSV ili Excel formatu, dva interaktivna grafikona.

Dodao sam CSV/Excel izvoz i ubacio Bootstrap stilizaciju – aplikacija sad izgleda modernije i korisnik može da preuzme rezultate.

### PythonAnywhere (besplatan plan)

Jednostavan, namenjen baš za Django aplikacije
Besplatni domen je ime.pythonanywhere.com

Koraci:

Registruj nalog na <https://www.pythonanywhere.com>.
Uploadaj svoj projekat (možeš zip pa upload ili git push).
U njihovom dashboardu idi na Web → Add new web app → izaberi Django.
Podesi putanju do svog penzija_project foldera i wsgi.py.
Migracije: u njihovom Bash Console uradi:

```sh
python manage.py migrate
python manage.py collectstatic
```

Restartuj web app i otvori je na svom <https://ime.pythonanywhere.com>.

### Railway.app (moderniji hosting)

- može da ti da besplatan hosting i your-app.railway.app domen
- potrebno je da koristiš GitHub za deploy

Koraci:

Registruj se na <https://railway.app>.
Poveži svoj GitHub repo sa Django projektom.
Railway automatski pokrene build (potreban `requirements.txt`).
Dodaj start komandu:

```sh
python manage.py migrate && gunicorn penzija_project.wsgi
```

Railway će ti dati URL gde će aplikacija biti dostupna.

### Render.com (takođe popularan)

Vrlo slično Railway-u, koristi GitHub i `requirements.txt`. Ima free plan.

Preporuka: ako hoćeš najlakši put bez Git-a i terminala kreni sa PythonAnywhere.

Ako želiš da se vežbaš sa GitHub i CI/CD stilom, onda je Railway bolja opcija.
