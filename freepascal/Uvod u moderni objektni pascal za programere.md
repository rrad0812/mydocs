
# Uvod u moderni objektni paskal za programere

## 1 Zašto ova knjiga

Želeo sam da opišem moderni Objekat Paskal : programski jezik sa klasama, unitima, generičkim klasama, interfejsima i drugim modernim karakteristikama koje očekujete. Želeo sam da pokažem kako se sve karakteristike jezika, osnovne i napredne, povezuju u konzistentnu celinu.

Takođe sam želeo da ova knjiga bude praktična i sažeta za kolege programere. Stoga, pretpostavljam da već imate neko iskustvo u programiranju i da možemo razgovarati o stvarima poput "kako deklarisati promenljivu" i izbeći dugačko objašnjenje "šta je uopšte promenljiva i koja je njena svrha" . Kada obrađujem osnove, daću kratak opis, a zatim preći na ovo: promenljiva je kontejner za neku vrednost; kontejner ima ime; vrednost koju sadrži može se menjati tokom vremena.

Naglašavam reč moderan u modernom Objektu Paskal . To je zato što se Paskal mnogo razvio i prilično se razlikuje od npr. Turbo Paskala koji su mnogi ljudi učili u školama pre mnogo vremena. Što se tiče karakteristika, moderni Paskal je prilično sličan C++ ili Javi ili C#:

- Ima sve moderne funkcije koje očekujete — klase, unite, interfejse, generike…
- Kompajlira se u brz, izvorni kod,
- Veoma je bezbedan po pitanju tipa,
- Visok nivo, ali može biti i nizak nivo ako vam je potrebno.

Takođe ima aktivan ekosistem alata i biblioteka. Da navedemo samo neke:

- Paskal ima odličan, prenosivi kompajler otvorenog koda pod nazivom `Free Pascal Compiler`, <http://freepascal.org/>.

- Prateći IDE (editor, debager, biblioteka vizuelnih komponenti, dizajner obrazaca) pod nazivom `Lazarus` <http://lazarus.freepascal.org/>.

- Takođe postoji vlasnički i komercijalni kompajler i IDE `Delphi` <https://www.embarcadero.com/products/Delphi>.

- Dostupno je mnogo biblioteka (i za FPC i za Delphi), pogledajte <https://github.com/Fr0sT-Brutal/awesome-pascal>.

- Takođe podržavamo postojeće editore poput VS Code-a, pogledajte <https://castle-engine.io/vscode>.

- Lično sam tvorac Castle Game Engine-a, <https://castle-engine.io/>, koji je 3D i 2D gejm endžin otvorenog koda i koristi moderni Paskal za kreiranje igara na mnogim platformama (Windows, Linux, FreeBSD, macOS, Android, iOS, Nintendo Switch, WebGL).

## 2 Osnove

### 2.1 Program "Zdravo svete"

```pascal
// Just use this line in all modern FPC sources.
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

// Below is needed for console programs on Windows,
// otherwise (with Delphi) the default is GUI program without console.
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

program MyProgram;
begin
  WriteLn('Hello world!');
end.
```

Ovo je kompletan program koji možete kompajlirati i pokrenuti.

- Ako koristite FPC iz komandne linije, samo kreirajte novu datoteku "myprogram.dpr" i pokrenite `fpc myprogram.dpr`.

- Ako koristite Lazarus, kreirajte novi projekat ( meni "Projekat → Novi projekat → Jednostavan program" ). Nalepite ga kao izvorni kod programa. Kompajlirajte koristeći stavku menija "Pokreni → Kompajliraj" ( ili koristite prečicu Ctrl + F9 ).

- Ako koristite Delfi, kreirajte i novi projekat ( meni "Datoteka → Novo → Konzolna aplikacija → Delfi" ). Nalepite ga kao izvorni kod programa. Kompajlirajte koristeći stavku menija "Projekat → Kompajliraj" (ili koristite prečicu Ctrl + F9 ).

Ovo je program iz komandne linije, pa samo pokrenite kompajliranu izvršnu datoteku iz komandne linije.

> [!Note]  
> Takođe ga možete pokrenuti iz Lazarus ili Delphi IDE koristeći stavku menija "Pokreni" (prečica F9 u oba IDE-a). U ovom slučaju, imajte na umu da će se konzola brzo pojavljivati i nestajati. Najjednostavniji način da to izbegnete je da dodate `Readln` ( sačekajte Enter ) na kraj aplikacije.

Ostatak ovog članka govori o jeziku Object Pascal, tako da ne očekujete da vidite nešto otmenije od komandne linije. Ako želite da vidite nešto kul, samo kreirajte novi GUI projekat u Lazarus-u ( "Project → New Project → Application" ) ili Delphi-ju ( "File → New → Multi-Device Application" ). Voila — funkcionalna GUI aplikacija, cross-platformm, sa izvornim izgledom svuda, koristeći udobnu biblioteku vizuelnih komponenti.

Paskal kompajleri dolaze sa mnoštvom standardnih unita za umrežavanje, grafički korisnički interfejs, bazu podataka, formate datoteka (XML, json, slike…​), rad sa nitima i sve ostalo što vam može zatrebati. Već sam ranije pomenuo svoj sjajni Castle Game Engine :)

### 2.2 Kompilatori i FPC "sintaksni režimi"

Ova knjiga, sav tekst i primeri za Paskal, napisana je da podrži dva moderna Paskal kompajlera:

- Slobodni Paskal kompajler (FPC), Paskal kompajler otvorenog koda, koji koristi i Lazarus IDE.
- Delfi, vlasnički Paskal kompajler kompanije Embarcadero.

U ovoj knjizi, u potpunosti podržavamo oba kompajlera.

Da bismo malo zakomplikovali stvari, FPC kompajler ima više "sintaksnih režima". U ovoj knjizi, odlučili smo da prikažemo ObjFpc sintaksni režim, koji preporučuju FPC programeri i koji je podrazumevani za nove Pascal projekte kreirane pomoću Lazarus-a ili Castle Game Engine-a . Malo se razlikuje od Delphi sintakskog režima, koji je najkompatibilniji sa Pascal jezikom kako ga implementira Delphi . Napisali smo detaljno poređenje ovde .

Ali ne želite sada da čitate o ovim razlikama, ako tek počinjete da učite Paskal!

Razlike su male, kako između kompajlera, tako i između FPC ObjFpc režima i Delphi režima. Samo imajte na umu da možete videti neke `{$ifdef FPC} …​ {$endif}` klauzule u primerima, koje čine kod važećim i za FPC ObjFpc režim i za Delphi. Korišćenje `{$ifdef FPC_OBJFPC} …​ {$endif}` u nekim od ovih slučajeva bi bilo preciznije, ali bi izgledalo još komplikovanije. Ako vaš projekat cilja samo jedan od ovih kompajlera, možete pojednostaviti svoj kod, samo izaberite varijantu za svoj kompajler i uklonite `{$ifdef …​}, {$endif}` stvari.

### 2.3 Funkcije, procedure, primitivni tipovi

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

program MyProgram;

procedure MyProcedure(const A: Integer);
begin
  WriteLn('A + 10 is: ', A + 10);
end;

function MyFunction(const S: string): string;
begin
  Result := S + 'strings are automatically managed';
end;

var
  X: Single;
begin
  WriteLn(MyFunction('Note: '));
  MyProcedure(5);

  // Division using "/" always makes float result, use "div" for integer division
  X := 15 / 5;
  WriteLn('X is now: ', X); // scientific notation
  WriteLn('X is now: ', X:1:2); // 2 decimal places
end.
```

Da biste vratili vrednost iz funkcije, dodelite nešto magičnoj `Result` promenljivoj. Možete slobodno čitati i podešavati vrednost `Result`, baš kao i lokalnu promenljivu.

```pascal
function MyFunction(const S: string): string;
begin
  Result := S + 'something';
  Result := Result + ' something more!';
  Result := Result + ' and more!';
end;
```

Takođe možete tretirati ime funkcije (kao "MyFunction" u gornjem primeru) kao promenljivu kojoj možete dodeliti vrednost. Ali ne bih to ohrabrio u novom kodu, jer izgleda "sumnjivo" kada se koristi na desnoj strani izraza za dodelu. Samo koristite `Result` uvek kada želite da pročitate ili podesite rezultat funkcije.

Ako želite da pozovete samu funkciju rekurzivno, naravno da to možete učiniti. Ako rekurzivno pozivate funkciju bez parametara, obavezno navedite zagrade () (iako u Paskalu obično možete izostaviti zagradu za funkciju bez parametara), ovo čini rekurzivni poziv funkcije bez parametara drugačijim od pristupa trenutnom rezultatu ove funkcije. Ovako:

```pascal
function SumIntegersUntilZero: Integer;
var
  I: Integer;
begin
  ReadLn(I);
  Result := I;
  if I <> 0 then
    Result := Result + SumIntegersUntilZero();
end;
```

Možete pozvati `Exit` da biste završili izvršavanje procedure ili funkcije pre nego što stigne do poslednjeg `end;`. Ako pozovete `Exit` funkciju bez parametara, ona će vratiti poslednju stvar koju ste postavili kao `Result`. Takođe možete koristiti `Exit(X)` construct, da biste postavili rezultat funkcije i odmah izašli  — ovo je baš kao `return X` construct u C-sličnim jezicima.

```pascal
function AddName(const ExistingNames, NewName: string): string;
begin
  if ExistingNames = '' then
    Exit(NewName);
  Result := ExistingNames + ', ' + NewName;
end;
```

Imajte na umu da rezultat funkcije može biti odbačen. Bilo koja funkcija može se koristiti baš kao procedura. Ovo ima smisla ako funkcija ima neki sporedni efekat (npr. menja globalnu promenljivu) pored izračunavanja rezultata. Na primer:

```pascal
var
  Count: Integer;
  MyCount: Integer;

function CountMe: Integer;
begin
  Inc(Count);
  Result := Count;
end;

begin
  Count := 10;
  CountMe; // the function result is discarded, but the function is executed, Count is now 11
  MyCount := CountMe; // use the result of the function, MyCount equals to Count which is now 12
end.
```

### 2.4 Testiranje ( if )

Koristite `if .. then` or `if .. then .. else` da biste pokrenuli neki kod kada je zadovoljen neki uslov. Za razliku od C-sličnih jezika, u Paskalu ne morate da stavljate uslov u zagrade.

```pascal
var
  A: Integer;
  B: boolean;
begin
  if A > 0 then
    DoSomething;

  if A > 0 then
  begin
    DoSomething;
    AndDoSomethingMore;
  end;

  if A > 10 then
    DoSomething
  else
    DoSomethingElse;

  // equivalent to above
  B := A > 10;
  if B then
    DoSomething
  else
    DoSomethingElse;
end;
```

Ovde je else uparen sa poslednjim if. Dakle, ovo funkcioniše kako očekujete:

```pascal
if A <> 0 then
  if B <> 0 then
    AIsNonzeroAndBToo
  else
    AIsNonzeroButBIsZero;
```

Iako je primer sa ugnežđenim klauzulama `if` iznad tačan, često je bolje postaviti ugnežđenu klauzulu `if` unutar `begin... end` bloka u takvim slučajevima. Ovo čini kod očiglednijim čitaocu i ostaće očigledan čak i ako pokvarite uvlačenje. Poboljšana verzija primera je ispod. Kada dodate ili uklonite neku elseklauzulu u kodu ispod, očigledno je na koji uslov će se primeniti (na A test ili na B test), pa je manje podložno greškama.

```pascal
if A <> 0 then
begin
  if B <> 0 then
    AIsNonzeroAndBToo
  else
    AIsNonzeroButBIsZero;
end;
```

### 2.5 Logički, relacioni i bitwise operatori

Logički operatori se zovu `and`, `or`, `not`, `xor`. Njihovo značenje je verovatno očigledno ( potražite "isključivo ili" ako niste sigurni šta xor radi :)). Oni prihvataju bulove argumente i vraćaju bulovu vrednost . Takođe mogu da deluju kao bitwise operatori kada su oba argumenta celobrojne vrednosti, u kom slučaju vraćaju ceo broj.

Relacioni (operatori poređenja) su `=`, `<>`, `>`, `<`, `<=`, `>=`. Ako ste navikli na jezike slične C-u, imajte na umu da se u Paskalu dve vrednosti (proveravaju se da li su jednake) porede koristeći jedan znak jednakosti `A = B` (za razliku od C-a gde se koristi A == B). Specijalni operator dodele u Paskalu je `:=`.

Logički (ili bitski) operatori imaju veći prioritet od relacionih operatora. Možda ćete morati da koristite zagrade oko nekih izraza da biste dobili željeni redosled izračunavanja.

Na primer, ovo je greška pri kompajlaciji:

```pascal
var
  A, B: Integer;  
begin  
  if A = 0 and B <> 0 then ... // INCORRECT example
```

Gore navedeni kod se ne kompajlira, jer kompajler prvo želi da izvrši bitsku operaciju `and` u sredini izraza: ( 0 and B ). Ovo je bitska operacija koja vraća celobrojnu vrednost. Zatim kompajler primenjuje = operator koji daje bulovu vrednost A = (0 and B). I na kraju se javlja greška "neusklađenost tipova" nakon pokušaja upoređivanja bulove vrednosti A = (0 and B) i celobrojne vrednosti 0.

Ovaj kod će se kompajlirati ispravno:

```pascal
var
  A, B: Integer;
begin
  if (A = 0) and (B <> 0) then ...
```

Pri izračubavanju relacionih operatora koristi se procena kratkog spoja. Razmotrite ovaj izraz :

```pascal
if MyFunction(X) and MyOtherFunction(Y) then...
```

- Garantovano je da će MyFunction(X) biti prva procenjena. Ako MyFunction(X) vrati `false`, onda je vrednost izraza poznata (vrednost false `and` bilo šta drugo je uvek `false`), i MyOtherFunction(Y) neće se uopšte izvršiti.

- Analogno pravilo važi i za `or` izraz. Tu, ako je poznato da je izraz `true` (jer je prvi operand `true`), drugi operand se ne izračunava.

- Ovo je posebno korisno prilikom pisanja izraza kao što je:
  
  ```pascal
  if (A <> nil) and A.IsValid then...
  ```
  
  Ovo će raditi u redu, čak i kada je A `nil`. Ključna reč `nil` je pokazivač jednak nuli (kada je predstavljen kao broj). U mnogim drugim programskim jezicima `nil` se naziva `null` pokazivač.

### 2.6 Testiranje jednog izraza za više vrednosti ( case )

Ako treba izvršiti različitu akciju u zavisnosti od vrednosti nekog izraza, `case .. of .. end`  iskaz je koristan.

```pascal
case SomeValue of
  0: DoSomething;
  1: DoSomethingElse;
  2: begin
       IfItsTwoThenDoThis;
       AndAlsoDoThis;
     end;
  3..10: DoSomethingInCaseItsInThisRange;
  11, 21, 31: AndDoSomethingForTheseSpecialValues;
  else DoSomethingInCaseOfUnexpectedValue;
end;
```

Klauzula `else` je opcionalna (i odgovara defaultu C-sličnim jezicima). Kada se nijedan uslov ne podudara i nema `else`, onda se ništa ne dešava.

Kada dolazite iz C-sličnih jezika i uporedite ovo sa `switch` izjavom u tim jezicima, primetićete da ne postoji automatsko prebacivanje . Ovo je namerna prednost u Paskalu. Ne morate da pamtite da postavite `break` instrukcije. U svakom izvršavanju, izvršava se najviše jedna grana naredbe `case`, to je to.

### 2.7 Nabrojani i ordinalni tipovi i skupovi i nizovi konstantne dužine

Nabrojani tip u Paskalu je veoma lep, neproziran tip. Verovatno ćete ga koristiti mnogo češće nego nabrajanja u drugim jezicima :)

```pascal
type
  TAnimalKind = (akDuck, akCat, akDog);
```

Konvencija je da se ispred imena nabrajanja doda prečica od dva slova tipa ime, dakle "ak" prečica za "Životinjska vrsta" . Ovo je korisna konvencija, jer se imena nabrajanja nalaze u unit(globalnom) imenskom prostoru. Dakle, dodavanjem prefiksa "ak", minimizirate šanse za kolizije sa drugim identifikatorima.

> [! Note]
> Kolizije u imenima nisu prepreka. U redu je da različiti uniti definišu isti identifikator. Ali je dobra ideja pokušati izbeći kolizije u svakom slučaju, kako bi kod bio jednostavan za razumevanje i grep.  

Možete izbeći postavljanje imena nabrajanja u globalni imenski prostor pomoću direktive kompajlera `{$scopedenums on}`. To znači da ćete morati da im pristupite kvalifikovani imenom tipa, kao što je `TAnimalKind.akDuck`. Potreba za ak prefiksom nestaje u ovoj situaciji i verovatno ćete jednostavno pozvati nabrajanja Duck, Cat, Dog. Ovo je slično C# nabrajanjima.

Činjenica da je nabrojani tip neproziran znači da mu se ne može samo dodeliti vrednosti celog broja i od njega. Međutim, za posebnu upotrebu, možete koristiti `Ord(MyAnimalKind)` za prisilno pretvaranje `enum` u broj ili `typecast` TAnimalKind(MyInteger) za prisilno pretvaranje broja u `enum`. U ovom drugom slučaju, prvo proverite da li MyInteger je u dobrom opsegu (od `0` do `Ord(High(TAnimalKind)`.

Nabrojani i ordinalni tipovi mogu se koristiti kao indeksi nizova:

```pascal
type
  TArrayOfTenStrings = array [0..9] of string;
  TArrayOfTenStrings1Based = array [1..10] of string;

  TMyNumber = 0..9;
  TAlsoArrayOfTenStrings = array [TMyNumber] of string;

  TAnimalKind = (akDuck, akCat, akDog);
  TAnimalNames = array [TAnimalKind] of string;
```

Takođe se mogu koristiti za kreiranje skupova (internih bitnih polja):

```pascal
type
  TAnimalKind = (akDuck, akCat, akDog);
  TAnimals = set of TAnimalKind;
var
  A: TAnimals;
begin
  A := [];
  A := [akDuck, akCat];
  A := A + [akDog];
  A := A * [akCat, akDog];
  Include(A, akDuck);
  Exclude(A, akDuck);
end;
```

### 2.8 Petlje (for, while, repeat, for .. in)

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}
{$R+} // range checking on - nice for debugging

var
  MyArray: array [0..9] of Integer;
  I: Integer;
begin
  // initialize
  for I := 0 to 9 do
    MyArray[I] := I * I;

  // show
  for I := 0 to 9 do
    WriteLn('Square is ', MyArray[I]);

  // does the same as above
  for I := Low(MyArray) to High(MyArray) do
    WriteLn('Square is ', MyArray[I]);

  // does the same as above
  I := 0;
  while I < 10 do
  begin
    WriteLn('Square is ', MyArray[I]);
    I := I + 1; // or "I += 1", or "Inc(I)"
  end;

  // does the same as above
  I := 0;
  repeat
    WriteLn('Square is ', MyArray[I]);
    Inc(I);
  until I = 10;

  // does the same as above
  // note: here I enumerates MyArray values, not indexes
  for I in MyArray do
    WriteLn('Square is ', I);
end.
```

#### O petljama repeat i while

Postoje dve razlike između ovih tipova petlji:

- Uslov petlje ima suprotno značenje. U `while ..` kažete kada da nastavi, ali u `repeat .. until` kažete kada da stane.

- U slučaju `repeat`, uslov se ne proverava na početku. Dakle, `repeat` petlja se uvek izvršava barem jednom.

#### O for ... to ... do petljama

Konstrukcija `for I := .. to .. do` ​je slična C-ovoj `for` petlji. Međutim, ograničenija je, jer ne možete da odredite proizvoljne akcije/testove za kontrolu iteracije petlje. Ovo je isključivo za iteraciju kroz uzastopne brojeve (ili druge ordinalne tipove). Jedina fleksibilnost koju imate je da možete da koristite `downto` umesto `to`, da bi brojevi išli nadole.

Zauzvrat, izgleda čisto i veoma je optimizovano u izvršenju. Konkretno, izrazi za donju i gornju granicu se izračunavaju samo jednom, pre nego što petlja počne.

Imajte na umu da vrednost promenljive brojača petlje ( Iu ovom primeru) treba smatrati nedefinisanom nakon što se petlja završi, zbog mogućih optimizacija. Pristup vrednosti Inakon petlje može izazvati upozorenje kompajlera. Osim ako prerano ne izađete iz petlje pomoću Breakili Exit: u tom slučaju, promenljiva brojača će garantovano zadržati poslednju vrednost.

#### O for ... in ... ​petljama

Slično `for I in .. do ..` je `foreach` konstrukciji u mnogim modernim jezicima. Inteligentno funkcioniše na mnogim ugrađenim tipovima:

- Može da iterira kroz sve vrednosti u nizu (primer iznad).

- Može da iterira kroz sve moguće vrednosti nabrojanog tipa:

  ```pascal
  var
    AK: TAnimalKind;
  begin
    for AK in TAnimalKind do...
  ```

- Može da iterira preko svih stavki uključenih u skup:

  ```pascal
  var
    Animals: TAnimals;
    AK: TAnimalKind;
  begin
    Animals := [akDog, akCat];
    for AK in Animals do ...
  ```

- I radi na prilagođenim tipovima lista, generičkim ili ne, kao što `TObjectList` i `TFPGObjectList`.

  ```pascal
  {$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
  {$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

  uses
    SysUtils, Generics.Collections;

  type
    TMyClass = class
      I, Square: Integer;
  end;
  
  TMyClassList = {$ifdef FPC}specialize{$endif} TObjectList<TMyClass>;

  var
  List: TMyClassList;
  C: TMyClass;
  I: Integer;
  begin
    List := TMyClassList.Create(true); // true = owns children
    try
      for I := 0 to 9 do
      begin
        C := TMyClass.Create;
        C.I := I;
        C.Square := I * I;
        List.Add(C);
      end;

      for C in List do
        WriteLn('Square of ', C.I, ' is ', C.Square);

    finally
      FreeAndNil(List);
    end;
  end.
  ```

Još uvek nismo objasnili koncept klasa, tako da vam poslednji primer možda još nije očigledan — samo nastavite, kasnije će biti logično.

### 2.9 Izlaz, evidentiranje

Da biste jednostavno ispisali stringove u Paskalu, koristite rutinu `Write` or `WriteLn`. Potonja automatski dodaje novi red na kraj.

Ovo je "magična" rutina u Paskal jeziku. Prihvata promenljiv broj argumenata i oni mogu biti bilo kog tipa. Svi se konvertuju u stringove prilikom prikazivanja, sa posebnom sintaksom za određivanje popunjavanja i preciznosti broja.

```pascal
WriteLn('Hello world!');
WriteLn('You can output an integer: ', 3 * 4);
WriteLn('You can pad an integer: ', 666:10);
WriteLn('You can output a float: ', Pi:1:4);
```

Da biste eksplicitno koristili novi red u stringu, koristite `LineEnding` konstantu (iz FPC RTL). Paskal stringovi ne interpretiraju nikakve posebne sekvence obrnutih kosih crtica, tako da pisanje

```pascal
WriteLn('One line.\nSecond line.'); // INCORRECT example
```

ne funkcioniše kao što neki od vas misle. Ovo će funkcionisati:

```pascal
WriteLn('One line.' + LineEnding + 'Second line.');
```

ili samo ovo:

```pascal
WriteLn('One line.');
WriteLn('Second line.');
```

Imajte na umu da će ovo raditi samo u konzolnim aplikacijama. Uverite se da imate definisan`{$apptype CONSOLE}` (a ne `{$apptype GUI}` ) u glavnoj programskoj datoteci. Na nekim operativnim sistemima to zapravo nije važno i uvek će raditi (Unix), ali na nekim operativnim sistemima pokušaj pisanja nečega iz GUI aplikacije je greška (Windows).

### 2.10. Konvertovanje u string

Da biste konvertovali proizvoljan broj argumenata u string (umesto da ih samo direktno izvedete), imate nekoliko opcija.

- Možete konvertovati određene tipove u stringove koristeći specijalizovane funkcije kao što su `IntToStr` i `FloatToStr`. Štaviše, možete spajati stringove u Paskalu jednostavnim sabiranjem. Tako možete kreirati string ovako: `'My int number is ' + IntToStr(MyInt) + ', and the value of Pi is ' + FloatToStr(Pi)`.

- Prednost: Apsolutno fleksibilno. Postoji mnogo XxxToStr preopterećenih verzija i prijatelja ( kao što je `FormatFloat` ), koji pokrivaju mnoge tipove. Većina njih je u SysUtils unitu.

- Još jedna prednost: Konzistentnost sa obrnutim funkcijama. Da biste konvertovali string (na primer, korisnički unos) nazad u ceo broj ili broj sa pokretnim decimalom, koristite `StrToInt` i `StrToFloat` prijatelje (kao što je `StrToIntDef`).

- Mana : Dugačko spajanje mnogih XxxToStrpoziva i nizova ne izgleda lepo.

Funkcija `Format`, koja se koristi slično `Format('%d %f %s', [MyInt, MyFloat, MyString])`. Ovo je slična `sprintf` funkciji u C-sličnim jezicima. Ona ubacuje argumente u rezervisana mesta u šablonu. Rezervisana mesta mogu koristiti posebnu sintaksu da bi uticala na formatiranje, npr. `%.4f` rezultat je format sa pokretnim zarezom i 4 cifre posle decimalne tačke.

- Prednost : Odvajanje šablona od argumenata izgleda jasno. Ako treba da promenite šablon bez diranja argumenata (npr. prilikom prevođenja), to možete lako uraditi.

- Još jedna prednost : Nema magije kompajlera. Možete koristiti istu sintaksu da prosledite bilo koji broj argumenata proizvoljnog tipa u sopstvenim rutinama (deklarišite parametar kao array of const). Zatim možete proslediti ove argumente nadole do Format, ili dekonstruisati listu parametara i raditi šta god želite sa njima.

- Mana : Kompilator ne proverava da li se obrazac podudara sa argumentima. Korišćenje pogrešnog tipa čuvara mesta će rezultirati izuzetkom tokom izvršavanja ( `EConvertError` izuzetak, a ne nešto loše poput greške kršenja pristupa (greška segmentacije) ).

`WriteStr(TargetString, …​)` rutina se ponaša slično kao `Write(…​)`, osim što se rezultat čuva u `TargetString`.

- Prednost : Podržava sve funkcije `Write`, uključujući posebnu sintaksu za formatiranje kao što je Pi:1:4.

- Mana : Posebna sintaksa za formatiranje je "magija kompajlera", implementirana posebno za rutine poput ove. Ovo je ponekad problematično, npr. ne možete kreirati sopstvenu rutinu MyStringFormatter(…​) koja bi takođe dozvolila posebnu sintaksu poput Pi:1:4. Iz tog razloga (a takođe i zato što dugo nije bila implementirana u glavnim Paskal kompajlerima), ova konstrukcija nije baš popularna.

## 3 Uniti

### 3.1 Pregled

Uniti vam omogućavaju da grupišete zajedničke stvari (sve što se može deklarisati) za upotrebu od strane drugih unita i programa. One su ekvivalentne modulima ili paketima u drugim jezicima. Imaju odeljak `interface`, gde deklarišete šta je dostupno za druge unite i programe, a zatim `implementation` odeljak.

```pascal
unit MyUnit;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

interface

procedure MyProcedure(const A: Integer);
function MyFunction(const S: string): string;

implementation

procedure MyProcedure(const A: Integer);
begin
  WriteLn('A + 10 is: ', A + 10);
end;

function MyFunction(const S: string): string;
begin
  Result := S + 'strings are automatically managed';
end;

end.
```

Program može koristiti unit pomoću `uses` ključne reči:

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

program MyProgram;

uses
  MyUnit;

begin
  WriteLn(MyFunction('Note: '));
  MyProcedure(5);
end.
```

### 3.2 Proširenja koja se koriste za unite i programe

Sačuvajte datoteku unita MyUnit kao "myunit.pas". To jest, malim slovima sa ".pas" ekstenzijom.

> [! Note]
> Moguće su i druge konvencije.
>
> Npr. FPC dozvoljava druge ekstenzije datoteka za unite. A neki ljudi koriste ".pp" za unit  datoteke, kao što je "myunit.pp".  
>
> Moguće je i korišćenje različitih velikih i malih slova. Na Windows fajl sistemima, velika i mala slova nisu bitna. Ali na Unix fajl sistemima jesu bitna i FPC dozvoljava korišćenje samo potpuno istih velikih i malih slova kao što je navedeno u Pascal `uses` klauzuli ( MyUnit.pas ) ili samo malih slova ("myunit.pas"). Pošto Pascal ne razlikuje velika i mala slova, prvo pravilo ponekad izaziva probleme kada ljudi navedu imena unita sa različitim velikim i malim slovima na različitim mestima.

Sve u svemu, preporučujemo jednostavno pravilo iznad, sa svim malim slovima, ".pas" ekstenziju za vaše projekte. Ovo se poklapa sa najčešćim ustaljenim praksama i radi sa svim kompajlerima i fajl sistemima bez problema.

Sačuvajte programu datoteku sa:

- ".dpr" ekstenzija ( skraćeno od "Delphi Project" ), ako želite da projekat bude kompatibilan i sa FPC/Lazarus-om i sa Delphi-jem ,

- ".lpr" ekstenzija (skraćeno od "Lazarus Project" ), ako želite da koristite samo FPC/Lazarus.

> [! Note]
> Moguće su i druge konvencije koje koriste neki projekti. Npr. neki projekti koriste ".pas" za glavnu programsku datoteku. Neki projekti koriste ".pp" za unite ili programe. Postoje razumni razlozi za to (npr. za FPC programe, koji ne koriste Lazarus LCL, ni opis "Lazarus Project" ni "Delphi Project" nisu potpuno tačni). Ali radi jednostavnosti, preporučujemo gore navedene konvencije ( ".dpr" ili ".lpr" ), jer pokrivaju najčešće ustaljene prakse.

### 3.3 Inicijalizacija i finalizacija

Unit može takođe da sadrži `initialization` i `finalization` odeljke. Ovo je kod koji se izvršava kada se program pokreće i završava.

```pascal
unit initialization_finalization;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

interface

implementation

initialization
  WriteLn('Hello world!');

finalization
  WriteLn('Goodbye world!');
end.
```

### 3.4 Jedinice koje koriste jedna drugu

Jedan unit može koristiti drugi unit. Drugi unit se može koristiti u odeljku interfejsa ili samo u odeljku implementacije. Prvi omogućava definisanje novih javnih stvari (procedura, tipova…) preko stvari drugog unita. Drugi je ograničeniji (ako koristite unit samo u odeljku implementacije, možete koristiti njene identifikatore samo u vašoj implementaciji).

```pascal
unit AnotherUnit;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

interface

uses
  Classes;

{ 
  The "TComponent" type (class) is defined in the Classes unit.
  That's why we had to use the Classes unit above. 
}

procedure DoSomethingWithComponent(var C: TComponent);

implementation

uses SysUtils;

procedure DoSomethingWithComponent(var C: TComponent);
begin
  { The FreeAndNil procedure is defined in the SysUtils unit.
    Since we only refer to its name in the implementation,
    it was OK to use the SysUtils unit in the "implementation" section. }
  FreeAndNil(C);
end;

end.
```

Nije dozvoljeno imati kružne zavisnosti unita u interfejsu. To jest, dva unita ne mogu koristiti jedna drugu u sekciji interfejsa. Razlog je taj što, da bi "razumeo" sekciju interfejsa unita, kompajler prvo mora "razumeti" sve unite koje koristi u sekciji interfejsa. Paskal jezik strogo prati ovo pravilo i omogućava brzu kompajlaciju i potpuno automatsko otkrivanje na strani kompajlera koje unite treba ponovo kompajlirati. Nema potrebe za korišćenjem komplikovanih Makefile datoteka za jednostavan zadatak kompajliranja u Paskalu, i nema potrebe da se sve ponovo kompajlira samo da bi se osiguralo da su sve zavisnosti ispravno ažurirane.

U redu je napraviti kružnu zavisnost između unita kada je barem jedna "upotreba" samo u implementaciji. Dakle, u redu je da unit A koristi unit B u interfejsu, a zatim B da unit koristi unit A u implementaciji.

### 3.5 Kvalifikujući identifikatori sa nazivom unita

Različiti uniti mogu definisati isti identifikator. Da biste kod učinili jednostavnim za čitanje i pretraživanje, obično bi trebalo da to izbegavate, ali to nije uvek moguće. U takvim slučajevima, poslednja stavka u klauzuli `uses` "pobeđuje", što znači da identifikatori koje ona uvodi skrivaju iste identifikatore koji su uveli raniji uniti.

Uvek možete eksplicitno definisati unit datog identifikatora, koristeći ga kao MyUnit.MyIdentifier. Ovo je uobičajeno rešenje kada je identifikator koji želite da koristite iz MyUnit skriven drugim unitom. Naravno, možete i preurediti redosled unita u vašoj klauzuli `uses`, iako to može uticati na druge deklaracije osim one koju pokušavate da popravite.

```pascal
program showcolor;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

// Both Graphics and GoogleMapsEngine units define TColor type.
uses Graphics, GoogleMapsEngine;

var
  { This doesn't work like we want, as TColor ends up
    being defined by GoogleMapsEngine. }
  // Color: TColor;
  { This works Ok. }
  Color: Graphics.TColor;

begin
  Color := clYellow;
  WriteLn(Red(Color), ' ', Green(Color), ' ', Blue(Color));
end.
```

U slučaju unita, zapamtite da one imaju dve `uses` klauzule: jednu u interfejsu, a drugu u implementaciji. Pravilo da "kasnije učitani uniti skrivaju stvari od ranije učitanih unita" se ovde dosledno primenjuje, što znači da uniti korišćene u `uses` kaluzuli odeljka `implementation` mogu sakriti identifikatore od unita korišćenih u odeljku `interface`. Međutim, zapamtite da su prilikom čitanja `interface` odeljka bitni samo unti korišćeni u interface. Ovo može stvoriti zbunjujuću situaciju, gde kompajler smatra dve naizgled jednake deklaracije različitim:

```pascal
unit UnitUsingColors;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

// INCORRECT example

interface

uses Graphics;

procedure ShowColor(const Color: TColor);

implementation

uses GoogleMapsEngine;

procedure ShowColor(const Color: TColor);
begin
  // WriteLn(ColorToString(Color));
end;

end.
```

Unit `Graphics` (iz Lazarus LCL-a) definiše `TColor` tip. Ali kompajler neće uspeti da kompajlira gornji unit, tvrdeći da niste implementirali proceduru `ShowColor` koja odgovara deklaraciji interfejsa. Problem je u tome što unit `GoogleMapsEngine` takođe definiše `TColor` tip. I koristi se samo u implementationsekciji, stoga zaklanja definiciju `TColor` samo u implementaciji. Ekvivalentna verzija gornjeg unita, gde je greška očigledna, izgleda ovako:

```pascal
unit UnitUsingColors;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

// INCORRECT example.
// This is what the compiler "sees" when trying to compile previous example

interface

uses Graphics;

procedure ShowColor(const Color: Graphics.TColor);

implementation

uses GoogleMapsEngine;

procedure ShowColor(const Color: GoogleMapsEngine.TColor);
begin
  // WriteLn(ColorToString(Color));
end;

end.
```

Rešenje je trivijalno u ovom slučaju, samo promenite implementaciju da se eksplicitno koristi `TColor` iz `Graphics` unita. Možete to popraviti i premeštanjem `GoogleMapsEngine` upotrebe u odeljak interfejsa i ranije od `Graphics`, iako bi ovo moglo dovesti do drugih posledica u realnim slučajevima, kada bi `UnitUsingColors` definisalo više stvari.

```pascal
unit UnitUsingColors;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

interface

uses Graphics;

procedure ShowColor(const Color: TColor);

implementation

uses GoogleMapsEngine;

procedure ShowColor(const Color: Graphics.TColor);
begin
  // WriteLn(ColorToString(Color));
end;

end.
```

### 3.6 Izlaganje identifikatora jednog unita iz drugog

Ponekad želite da uzmete identifikator iz jednog unita i da ga izložite u novom unitu. Krajnji rezultat bi trebalo da bude da korišćenje novog unita identifikator dostupnim u imenskom prostoru.

Ponekad je ovo neophodno da bi se sačuvala kompatibilnost sa prethodnim verzijama unita. Ponekad je lepo "sakriti" unutrašnji unit na ovaj način.

To se može uraditi redefinisanjem identifikatora u vašem novom unitu.

```pascal
unit MyUnit;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

interface

uses Graphics;

type
  { Expose TColor from Graphics unit as TMyColor. }
  TMyColor = TColor;

  { Alternatively, expose it under the same name.
    Qualify with unit name in this case, otherwise
    we would refer to ourselves with "TColor = TColor" definition. }
  TColor = Graphics.TColor;

const
  { This works with constants too. }
  clYellow = Graphics.clYellow;
  clBlue = Graphics.clBlue;

implementation

end.
```

Imajte na umu da se ovaj trik ne može tako lako izvesti sa globalnim procedurama, funkcijama i promenljivim. Sa procedurama i funkcijama, mogli biste izložiti konstantni pokazivač na proceduru u drugom unitu (pogledajte Povratne pozive tj. događaje, tj. pokazivače na funkcije, tj. proceduralne promenljive) ), ali to izgleda prilično prljavo.

Uobičajeno rešenje je kreiranje trivijalnih "omotačkih" funkcija koje jednostavno pozivaju funkcije iz internog unit, prosleđujući parametre i vraćajuće vrednosti po potrebi.

Da bi ovo funkcionisalo sa globalnim promenljivim, mogu se koristiti globalna svojstva (na nivou unita).

## 4 Klase

### 4.1 Osnove

U ObjectPascalu imamo klase. Na osnovnom nivou, klasa je samo kontejner za:

- `polja` (što je lep naziv za "promenljivu unutar klase" ),

- `metode` (što je lep naziv za "proceduru ili funkciju unutar klase" ),

- `svojstva` (što je otmena sintaksa za nešto što izgleda kao polje, ali je u stvari par metoda za dobijanje i postavljanje nečega; više u Svojstvima ).

Zapravo, postoji više mogućnosti, opisanih u odeljku Više stvari unutar klasa i ugnežđenih klasa.

```pascal
type
  TMyClass = class
    MyInt: Integer;                                           // this is a field
    property MyIntProperty: Integer read MyInt write MyInt;   // this is a property
    procedure MyMethod;                                       // this is a method
  end;

procedure TMyClass.MyMethod;
begin
  WriteLn(MyInt + 10);
end;
```

### 4.2 Nasleđivanje, virtuelne metode, nadjačavanje, ponovno uvođenje

Imamo nasleđivanje i virtuelne metode.

U donjem primeru, klasa TMyClassDescendant nasleđuje od klase TMyClass. TMyClassDescendant je potomak od TMyClass, i TMyClassje predak od TMyClassDescendant.

```pascal
program MyProgram;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils;

type
  TMyClass = class
    MyInt: Integer;
    procedure MyVirtualMethod; virtual;
  end;

  TMyClassDescendant = class(TMyClass)
    procedure MyVirtualMethod; override;
  end;

procedure TMyClass.MyVirtualMethod;
begin
  WriteLn('TMyClass shows MyInt + 10: ', MyInt + 10);
end;

procedure TMyClassDescendant.MyVirtualMethod;
begin
  WriteLn('TMyClassDescendant shows MyInt + 20: ', MyInt + 20);
end;

var
  C: TMyClass;

begin
  C := TMyClass.Create;
  try
    C.MyVirtualMethod;
  finally
    FreeAndNil(C);
  end;

  C := TMyClassDescendant.Create;
  try
    C.MyVirtualMethod;
  finally
    FreeAndNil(C);
  end;

end.
```

Kada je metoda virtuelna, to znači da kompajler traži implementaciju metode tokom izvršavanja, na osnovu stvarne klase instance. Šta to znači u praksi?

- Pokrenite gornji primer bez izmena. Imajte na umu da je metoda MyVirtualMethod virtuelna. Poziv C.MyVirtualMethod bira odgovarajuću implementaciju na osnovu stvarne klase instance C. Kada je C klase TMyClassDescendant, TMyClassDescendant.MyVirtualMethod implementacija se poziva. Stoga bi izlaz trebalo da bude:

  ```sh
  TMyClass shows MyInt + 10: 10
  TMyClassDescendant shows MyInt + 20: 20
  ```

- Sada izmenite gornji primer uklanjanjem delova `virtual` i `override`. Oba poziva C.MyVirtualMethod će sada pozivati implementaciju iz `TMyClass`, jer je C deklarisano kao `TMyClass`, tako da je tokom kompajliranja sve što kompajler zna da je C `TMyClass.` Izlaz će biti:

  ```pascal
  TMyClass shows MyInt + 10: 10
  TMyClass shows MyInt + 10: 10
  ```

  Ukratko, ovo obično nije ono što želite. Želite virtuelne metode.

Podrazumevano, metode nisu virtuelne, deklarišite ih sa `virtual` da biste ih učinili virtuelnim. Nadjačavanje moraju biti označena sa `override`, u suprotnom ćete dobiti upozorenje. Da biste sakrili metodu (deklarisanu u pretku kao virtual) bez njenog prepisivanja (obično ne želite da to uradite, osim ako ne znate šta radite) koristite `reintroduce`.

### 4.3 Klase i instance klasa, konstruktori, destruktori

Primer u gornjem odeljku prikazuje klasu pod nazivom TMyClass (i drugu klasu pod nazivom TMyClassDescendant). Klasa je tipa TMyClass, možete je smatrati i šablonom. Sama klasa ne sadrži nikakve vrednosti — nema memorije rezervisane za polje MyInt: Integer deklarisano u gornjem primeru.

> [! Note]  
> Zapravo je moguće da klasa "čuva vrednosti" koristeći promenljive klase, ali za sada zaboravimo na ovu mogućnost. Fokusirajte se na jednostavne klase koje imaju samo regularna polja.

Da bismo rezervisali memoriju za polja, potrebno je da kreiramo instancu klase.

Kreiranje instance klase se vrši pozivanjem konstruktora.

- Konstruktor je posebna vrsta metode, koja koristi ključnu reč `constructor`.

- Pre pozivanja konstruktora, memorija za instancu klase se alocira, a zatim se poziva kod konstruktora.

- Ne morate definisati konstruktor u svim vašim klasama. Sve klase implicitno potiču od `TObject` koji ima konstruktor bez parametara pod nazivom `Create`. Dakle, uvek imate konstruktor, čak i ako ga niste definisali.

- Ali možete definisati konstruktor u vašoj klasi. To je najbolji način za inicijalizaciju instance klase. Ako želite kasnije da zavisi od toga da je npr. "početna vrednost polja X <- Y", onda to učinite tako ( X := Y; ) u konstruktoru.

- Vaši sopstveni konstruktori se obično nazivaju samo `Create`. Više detalja o imenovanju konstruktora i destruktora možete pronaći u odeljku Virtuelni destruktor pod nazivom Destroy.

Pozivate konstruktor, dodeljujući instancu klase, ovako:

```pascal
X := TMyClass.Create;
```

Definišete svoj konstruktor ovako:

```pascal
type
  TMyClass = class
  public
    X: Integer;
    constructor Create;
  end;

constructor TMyClass.Create;
begin
  inherited Create;           // Call the ancestor constructor
  // Initialization code here
  X := 123;
end;
```

Suprotno tome, kada se klasa uništi, poziva se `destructor`.

- To je opet posebna vrsta metode, koja koristi ključnu reč `destructor`.

- Nakon pozivanja destruktora, oslobađa se memorija za instancu klase. Pristup poljima uništene instance više nije dozvoljen.

- Ponovo, ne morate definisati destruktor u svim vašim klasama. Sve klase implicitno potiču od `TObject` koja ima `destruktor` bez parametara pod nazivom `Destroy`.

- Ali možete definisati destruktor u vašoj klasi. Ovo je vaša poslednja šansa da uradite bilo kakvo "čišćenje". Npr. možda je vaša instanca klase kreirala neke druge instance klase, interne, i sada ih treba osloboditi.

- Ako definišete, trebalo bi da postoji samo jedan destruktor, nazvan `Destroy`, uvek sa `override`. Više detalja o tome zašto bi to trebalo da bude tako je u odeljku Virtuelni destruktor pod nazivom Destroy .

Evo jednog primera:

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils;

type
  TMyClass = class
  private
    InternalStuff: TObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMyClass.Create;
begin
  inherited Create; // Call the ancestor constructor at the beginning
  InternalStuff := TObject.Create;
  Writeln('TMyClass.Create');
end;

destructor TMyClass.Destroy;
begin
  Writeln('TMyClass.Destroy');
  FreeAndNil(InternalStuff); // will call InternalStuff.Destroy
  inherited Destroy; // Call the ancestor destructor at the end
end;

var
  C: TMyClass;
begin
  C := TMyClass.Create;
  try
    // use C
  finally
    FreeAndNil(C); // will call C.Destroy
  end;
end.
```

### 4.4 Testiranje klase (is), tipizacija (as, TMyClass(X))

Da biste testirali klasu instance tokom izvršavanja, koristite `is` operator. Da biste pretvorili instancu u određenu klasu, koristite `as` operator.

```pascal
program is_as;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils;

type
  TMyClass = class
    procedure MyMethod;
  end;

  TMyClassDescendant = class(TMyClass)
    procedure MyMethodInDescendant;
  end;

procedure TMyClass.MyMethod;
begin
  WriteLn('MyMethod');
end;

procedure TMyClassDescendant.MyMethodInDescendant;
begin
  WriteLn('MyMethodInDescendant');
end;

var
  Descendant: TMyClassDescendant;
  C: TMyClass;

begin
  Descendant := TMyClassDescendant.Create;
  try
    Descendant.MyMethod;
    Descendant.MyMethodInDescendant;

    { Descendant has all functionality expected of
      the TMyClass, so this assignment is OK }
    C := Descendant;
    C.MyMethod;

    { this cannot work, since TMyClass doesn't define this method }
    //C.MyMethodInDescendant;
    if C is TMyClassDescendant then
      (C as TMyClassDescendant).MyMethodInDescendant;
  finally
    FreeAndNil(Descendant);
  end;
end.
```

Umesto pretvaranja tipa pomoću `X as TMyClass`, možete koristiti i neoznačeno pretvaranje tipa kao u `TMyClass(X)`. Ovo je brže, ali rezultira nedefinisanim ponašanjem ako X nije TMyClass potomak. Zato nemojte koristiti `TMyClass(X)` pretvaranje tipa ili ga koristite samo u kodu gde je potpuno očigledno da je ispravno, na primer odmah nakon testiranja sa is:

```pascal
if A is TMyClass then
  (A as TMyClass).CallSomeMethodOfMyClass;

// below is marginally faster
if A is TMyClass then
  TMyClass(A).CallSomeMethodOfMyClass;
```

### 4.5 Svojstva

Svojstva su veoma lep "sintaksički šećer":

- Da napravite nešto što izgleda kao polje ( može se čitati i postavljati), ali ispod toga se realizuje pozivanjem metoda za dobijanje i postavljanje. Tipična upotreba je izvršavanje nekog sporednog efekta (npr. ponovno crtanje ekrana) svaki put kada se neka vrednost promeni.

- Napravite nešto što izgleda kao polje, ali je samo za čitanje. U stvari, to je kao konstanta ili funkcija bez parametara.

```pascal
type
  TWebPage = class
  private
    FURL: string;
    FColor: TColor;
    function SetColor(const Value: TColor);
  public
    { No way to set it directly.
      Call the Load method, like Load('http://www.freepascal.org/'),
      to load a page and set this property. }
    property URL: string read FURL;
    procedure Load(const AnURL: string);
    property Color: TColor read FColor write SetColor;
  end;

procedure TWebPage.Load(const AnURL: string);
begin
  FURL := AnURL;
  NetworkingComponent.LoadWebPage(AnURL);
end;

function TWebPage.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    // for example, cause some update each time value changes
    Repaint;
    // as another example, make sure that some underlying instance,
    // like a "RenderingComponent" (whatever that is),
    // has a synchronized value of Color.
    RenderingComponent.Color := Value;
  end;
end;
```

Imajte na umu da umesto navođenja metode, možete takođe navesti polje (obično privatno polje) za direktno dobijanje ili postavljanje. U gornjem primeru, svojstvo `Color` koristi metodu za postavljanje `SetColor`. Ali za dobijanje vrednosti, `Color` svojstvo se direktno odnosi na privatno polje `FColor`. Direktno referenciranje na polje je brže od implementacije trivijalnih metoda za dobijanje ili postavljanje (brže za vas i brže pri izvršavanju).

Prilikom deklarisanja svojstva navodite:

- Da li se može pročitati i kako (direktnim čitanjem polja ili korišćenjem metode "getter").

- I, na sličan način, da li se može podesiti i kako (direktnim pisanjem u određeno polje ili pozivanjem metode "setter").

Kompilator proverava da li se tipovi i parametri naznačenih polja i metoda podudaraju sa tipom svojstva. Na primer, da biste pročitali `Integer` svojstvo, morate ili da navedete `Integer` polje ili metodu bez parametara koja vraća `Integer`.

Tehnički gledano, za kompajler, metode "getter" i "setter" su samo normalne metode i mogu da urade apsolutno sve (uključujući sporedne efekte ili randomizaciju). Ali je dobra konvencija da se svojstva dizajniraju tako da se ponašaju manje-više kao polja:

- Funkcija za dobijanje ne bi trebalo da ima vidljive sporedne efekte (npr. ne bi trebalo da čita neke unose iz datoteke/tastature). Trebalo bi da bude deterministička (bez randomizacije, čak ni pseudo-randomizacije). Čitanje svojstva više puta trebalo bi da bude validno i da vrati istu vrednost, ako se ništa nije promenilo između.

- Imajte na umu da je u redu da funkcija za dobijanje podataka ima neki nevidljivi sporedni efekat, na primer, da kešira vrednost nekog proračuna (za koji se zna da proizvodi iste rezultate za datu instancu), kako bi ga sledeći put brže vratila. Ovo je zapravo jedna od sjajnih mogućnosti funkcije "za dobijanje podataka".

- Funkcija setera treba uvek da podesi traženu vrednost, tako da pozivanje funkcije za dobijanje vraća tu vrednost.

- Nemojte tiho odbacivati nevažeće vrednosti u "funkciji setera" (pokrenite izuzetak ako morate).

- Nemojte konvertovati ili skalirati traženu vrednost. Ideja je da nakon što `MyClass.MyProperty := 123;` programer može očekivati da MyClass.MyProperty = 123.

- Svojstva koja su samo za čitanje se često koriste da bi se neko polje spolja učinilo samo za čitanje. Ponovo, dobra konvencija je da se ponaša kao konstanta, barem konstanta za ovu instancu objekta sa ovim stanjem. Vrednost svojstva ne bi trebalo neočekivano da se menja. Napravite ga funkcijom, a ne svojstvom, ako njegovo korišćenje ima sporedni efekat ili vraća nešto nasumično.

- Polje "backing" (podloška) svojstva je skoro uvek privatno, jer je ideja svojstva da obuhvati svaki spoljni pristup njemu.

- Tehnički je moguće napraviti svojstva koja se mogu samo podesiti, ali još uvek nisam video dobar primer tako nešto :)

> [! Note]
> Svojstva se takođe mogu definisati van klase, na nivou jedinice. Ona tada služe analognoj svrsi: izgledaju kao globalna promenljiva, ali su podržana rutinama za dobijanje i postavljanje.

#### 4.5.1 Serijalizacija svojstava

Objavljena svojstva su osnova serijalizacije ( poznate i kao striming komponente ) u Paskalu. Serijalizacija znači da se podaci instance snimaju u tok ( stream ) ( kao što je datoteka ), iz kojeg se kasnije mogu obnoviti.

Serijalizacija je ono što se dešava kada Lazarus čita (ili piše) stanje komponente iz xxx.lfm datoteke. (U Delfiju, ekvivalentna datoteka ima .dfm ekstenziju.) Ovaj mehanizam možete koristiti i eksplicitno, koristeći rutine kao što je `ReadComponentFromTextStream` iz `LResources` unita. Takođe možete koristiti i druge algoritme serijalizacije, npr. `FpJsonRtti` unit (serijalizacija u JSON).

U svakom svojstvu možete deklarisati neke dodatne stvari koje će biti korisne za bilo koji algoritam serijalizacije:

- Možete odrediti podrazumevanu vrednost svojstva (koristeći `default` ključnu reč ). Imajte na umu da je i dalje potrebno inicijalizovati svojstvo u konstruktoru na ovu tačnu podrazumevanu vrednost (to se ne radi automatski). Deklaracija `default` je samo informacija algoritmu serijalizacije: "kada se konstruktor završi, dato svojstvo ima datu vrednost".

- Da li svojstvo uopšte treba čuvati (koristeći `stored` ključnu reč).

### 4.6 Izuzeci - kratak primer

Imamo izuzetke. Mogu se uhvatiti pomoću `try …​ except …​ end` klauzula, a imamo i `finally` odeljke poput `try …​ finally …​ end`.

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

program MyProgram;

uses
  SysUtils;

type
  TMyClass = class
    procedure MyMethod;
  end;

procedure TMyClass.MyMethod;
begin
  if Random > 0.5 then
    raise Exception.Create('Raising an exception!');
end;

var
  C: TMyClass;
begin
  Randomize;
  C := TMyClass.Create;
  try
    C.MyMethod;
  finally
    FreeAndNil(C);
  end;
end.
```

Imajte na umu da se `finally` klauzula izvršava čak i ako izađete iz bloka koristeći `Exit` (iz funkcije / procedure / metode) ili `Break` ili `Continue` (iz tela petlje).

Pogledajte poglavlje Izuzeci za detaljniji opis izuzetaka.

### 4.7 Specifikatori vidljivosti

Kao i u većini objektno orijentisanih jezika, imamo specifikatore vidljivosti da bismo sakrili polja / metode / svojstva.

Osnovni nivoi vidljivosti su:

- **public**  
Svi mogu da mu pristupe, uključujući i kod u drugim unit.

- **private**  
Dostupno samo u ovoj klasi.

- **protected**  
Dostupno samo u ovoj klasi i njenim potomcima.

Objašnjenje vidljivosti `private` i `protected` iznad nije sasvim tačno. Kod u istom unitu može prevazići svoja ograničenja i slobodno pristupiti `private` i `protected`. Ponekad je ovo lepa funkcija koja vam omogućava da implementirate čvrsto povezane klase. Koristite `strict private` i `strict protected` da biste čvršće obezbedili svoje klase. Pogledajte `private` i `strict private`.

Podrazumevano, ako ne navedete vidljivost, onda je vidljivost deklarisanih stvari `public`. Izuzetak je za klase kompajlirane sa `{$M+}`, ili potomke klasa kompajliranih sa `{$M+}`, što uključuje sve potomke `TPersistent`, što takođe uključuje sve potomke `TComponent` (pošto TComponent potiče od `TPersistent`). Za njih, podrazumevani specifikator vidljivosti je `published`, što je kao `public`, ali pored toga sistem za strimovanje zna da ovo obrađuje.

Nije svaki tip polja i svojstva dozvoljen u `published` odeljku (ne može se svaki tip strimovati, a samo klase se mogu strimovati iz jednostavnih polja). Koristite ovo `public` ako vam strimovanje nije važno, ali želite da nešto bude dostupno svim korisnicima.

### 4.8 Podrazumevani predak

Ako ne deklarišete tip pretka, svaka `class` nasleđuje od `TObject`.

### 4.9 Self

Ključna reč `Self` može se koristiti unutar implementacije klase da bi se eksplicitno referencirala vaša instanca. Ekvivalentna je ključnoj reči `this` iz C++, Jave i sličnih jezika.

### 4.10 Pozivanje nasleđene metode

Unutar implementacije metode, ako pozovete drugu metodu, onda podrazumevano pozivate metodu sopstvene klase. U primeru koda ispod, "TMyClass2.MyOtherMethod" poziva "MyMethod", što na kraju poziva "TMyClass2.MyMethod".

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils;

type
  TMyClass1 = class
    procedure MyMethod;
  end;

  TMyClass2 = class(TMyClass1)
    procedure MyMethod;
    procedure MyOtherMethod;
  end;

procedure TMyClass1.MyMethod;
begin
  Writeln('TMyClass1.MyMethod');
end;

procedure TMyClass2.MyMethod;
begin
  Writeln('TMyClass2.MyMethod');
end;

procedure TMyClass2.MyOtherMethod;
begin
  MyMethod; // this calls TMyClass2.MyMethod
end;

var
  C: TMyClass2;
begin
  C := TMyClass2.Create;
  try
    C.MyOtherMethod;
  finally FreeAndNil(C) 
  end;
end.
```

Ako metod nije definisan u datoj klasi, onda poziva metod klase pretka. U stvari, kada pozovete "MyMethod" instance klase "TMyClass2", onda

- Kompilator traži TMyClass2.MyMethod.

- Ako se ne pronađe, traži se TMyClass1.MyMethod.

- Ako se ne pronađe, traži se TObject.MyMethod.

- ako se ne pronađe, kompilacija ne uspeva.

Možete to testirati tako što ćete komentarisati "TMyClass2.MyMethod" definiciju u gornjem primeru. U stvari, "TMyClass1.MyMethod" biće pozvana od strane "TMyClass2.MyOtherMethod".

Ponekad ne želite da pozovete metod sopstvene klase. Želite da pozovete metod pretka (ili pretka pretka, i tako dalje). Da biste to uradili, dodajte ključnu reč `inherited` pre poziva metode "MyMethod", ovako:

```pascal
inherited MyMethod;
```

Na ovaj način primoravate kompajler da počne pretragu od klase pretka. U našem primeru, to znači da kompajler traži "MyMethod" unutar "TMyClass1.MyMethod", zatim "TObject.MyMethod", a onda odustaje. Čak ni ne razmatra korišćenje implementacije "TMyClass2.MyMethod".

**Bakšiš**:  
Samo napred, promenite implementaciju "TMyClass2.MyOtherMethod" iznad da koristite "inherited MyMethod" i vidite razliku u rezultatu.

Poziv `inherited` se često koristi za pozivanje metode pretka istog imena. Na ovaj način potomci mogu poboljšati pretke (zadržavajući funkcionalnost pretka, umesto da zamene funkcionalnost pretka). Kao u primeru ispod.

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils;

type
  TMyClass1 = class
    constructor Create;
    procedure MyMethod(const A: Integer);
  end;

  TMyClass2 = class(TMyClass1)
    constructor Create;
    procedure MyMethod(const A: Integer);
  end;

constructor TMyClass1.Create;
begin
  inherited Create; // this calls TObject.Create
  Writeln('TMyClass1.Create');
end;

procedure TMyClass1.MyMethod(const A: Integer);
begin
  Writeln('TMyClass1.MyMethod ', A);
end;

constructor TMyClass2.Create;
begin
  inherited Create; // this calls TMyClass1.Create
  Writeln('TMyClass2.Create');
end;

procedure TMyClass2.MyMethod(const A: Integer);
begin
  inherited MyMethod(A); // this calls TMyClass1.MyMethod
  Writeln('TMyClass2.MyMethod ', A);
end;

var
  C: TMyClass2;
begin
  C := TMyClass2.Create;
  try
    C.MyMethod(123);
  finally FreeAndNil(C) end;
end.
```

Pošto je korišćenje `inherited` za pozivanje metode sa istim imenom, sa istim argumentima, veoma čest slučaj, postoji posebna prečica za to: možete jednostavno napisati inherited; ( `inherited` ključna reč praćena tačkom-zarezom, umesto imena metode). To znači " pozovite nasleđenu metodu sa istim imenom, prosleđujući joj iste argumente kao trenutnoj metodi ".

**Bakšiš**:  
U gornjem primeru, svi inherited …​; pozivi bi mogli biti zamenjeni jednostavnim `inherited;`.

**Napomena 1**:  
`inherited;` je zapravo samo prečica za pozivanje metode pretka sa istim promenljivim koje su prosleđene u . Ako ste izmenili sopstveni parametar (što je moguće, ako parametar nije const), onda metoda pretka može primiti različite ulazne vrednosti od vašeg potomka. Razmotrite ovo:

```pascal
procedure TMyClass2.MyMethod(A: Integer);
begin
  WriteLn('TMyClass2.MyMethod beginning ', A);
  A := 456;
  { This calls TMyClass1.MyMethod with A = 456,
    regardless of the A value passed to this method (TMyClass2.MyMethod). }
  inherited;
  WriteLn('TMyClass2.MyMethod ending ', A);
end;
```

**Napomena 2**:  
Obično želite da napravite "MyMethod" virtuelnu metodu kada je definiše više klasa ( duž "lanca nasleđivanja"). Više o virtuelnim metodama u odeljku ispod. Ali ključna reč inherited radi bez obzira na to da li je metoda virtuelna ili ne. `inherited always` znači da kompajler počinje da traži metodu u pretku, i to ima smisla i za virtuelne i za nevirtuelne metode.

### 4.11 Virtuelne metode, nadjačavanje i ponovno uvođenje

Podrazumevano, metode nisu virtuelne . Ovo je slično C++-u, a za razliku od Jave.

Kada metoda nije virtuelna , kompajler određuje koju metodu da pozove na osnovu trenutno deklarisanog tipa klase, a ne na osnovu stvarno kreiranog tipa klase. Razlika deluje suptilno, ali je važna kada je vaša promenljiva deklarisana da ima klasu poput TFruit, ali ona u stvari može biti potomak klase poput TApple.

Ideja objektno orijentisanog programiranja je da je potomak klase uvek jednako dobar kao i predak , tako da kompajler dozvoljava korišćenje potomka klase uvek kada se očekuje predak. Kada vaša metoda nije virtuelna, ovo može imati neželjene posledice. Razmotrite primer ispod:

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils;

type
  TFruit = class
    procedure Eat;
  end;

  TApple = class(TFruit)
    procedure Eat;
  end;

procedure TFruit.Eat;
begin
  Writeln('Eating a fruit');
end;

procedure TApple.Eat;
begin
  Writeln('Eating an apple');
end;

procedure DoSomethingWithAFruit(const Fruit: TFruit);
begin
  Writeln('We have a fruit with class ', Fruit.ClassName);
  Writeln('We eat it:');
  Fruit.Eat;
end;

var
  Apple: TApple; // Note: you could also declare "Apple: TFruit" here
begin
  Apple := TApple.Create;
  try
    DoSomethingWithAFruit(Apple);
  finally FreeAndNil(Apple) end;
end.
```

Ovaj primer će se odštampati

```sh
Imamo voće klase TApple
Mi ga jedemo:
Jedenje voća
```

U stvari, poziv "Fruit.Eat" je pozvao "TFruit.Eat" implementaciju, a ništa ne poziva "TApple.Eat" implementaciju.

Ako razmislite o tome kako kompajler radi, ovo je prirodno: kada ste napisali Fruit.Eat, Fruitpromenljiva je deklarisana da sadrži klasu TFruit. Dakle, kompajler je tražio metodu koja se poziva Eatunutar TFruitklase. Ako TFruitklasa ne bi sadržala takvu metodu, kompajler bi tražio unutar pretka ( TObjectu ovom slučaju). Ali kompajler ne može da pretražuje unutar potomaka (kao što je TApple) , jer ne zna da li je stvarna klasa , , ili neki drugi potomak (kao što je , što nije prikazano u gornjem primeru).FruitTAppleTFruitTFruitTOrange

Drugim rečima, metoda koja će biti pozvana određuje se tokom kompajliranja.

Korišćenje virtuelnih metoda menja ovo ponašanje. Ako Eatbi metoda bila virtuelna (primer je prikazan ispod), onda se stvarna implementacija koja će se pozivati određuje tokom izvršavanja . Ako Fruitpromenljiva sadrži instancu klase TApple(čak i ako je deklarisana kao TFruit), onda Eatće se metoda prvo pretraživati unutar TAppleklase.

U Object Paskal-u, da biste definisali metodu kao virtuelnu , potrebno je

- Označite njegovu prvu definiciju (u najvišem pretku) ključnom virtualreči.

- Označite sve ostale definicije (u potomcima) ključnom overridereči . Sve prepisane verzije moraju imati potpuno iste parametre (i vraćati iste tipove, u slučaju funkcija).

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils;

type
  TFruit = class
    procedure Eat; virtual;
  end;

  TApple = class(TFruit)
    procedure Eat; override;
  end;

procedure TFruit.Eat;
begin
  Writeln('Eating a fruit');
end;

procedure TApple.Eat;
begin
  Writeln('Eating an apple');
end;

procedure DoSomethingWithAFruit(const Fruit: TFruit);
begin
  Writeln('We have a fruit with class ', Fruit.ClassName);
  Writeln('We eat it:');
  Fruit.Eat;
end;

var
  Apple: TApple; // Note: you could also declare "Apple: TFruit" here
begin
  Apple := TApple.Create;
  try
    DoSomethingWithAFruit(Apple);
  finally FreeAndNil(Apple) end;
end.
```

Ovaj primer će se odštampati

```sh
Imamo voće klase TApple
Mi ga jedemo:
Jedenje jabuke
```

Interno, virtuelne metode funkcionišu tako što imaju takozvanu tabelu virtuelnih metoda povezanu sa svakom klasom. Ova tabela je lista pokazivača na implementacije virtuelnih metoda za ovu klasu. Prilikom pozivanja Eatmetode, kompajler pretražuje tabelu virtuelnih metoda povezanu sa stvarnom klasom Fruiti koristi pokazivač na Eatimplementaciju koja je tamo sačuvana.

Ako ne koristite overrideključnu reč , kompajler će vas upozoriti da skrivate ( zamaskirate) virtuelnu metodu pretka definicijom koja nije virtuelna. Ako ste sigurni da je to ono što želite, možete dodati reintroduceključnu reč. Ali u većini slučajeva, radije ćete želeti da metod ostane virtuelni i da dodate overrideključnu reč , čime ćete osigurati da se uvek ispravno poziva.

## 5 Oslobađanje instanci klase

### 5.1 Ne zaboravite da oslobodite instance klase

Instance klase moraju biti ručno oslobođene, u suprotnom dolazi do curenja memorije.

Savetujemo da automatski detektujete curenje memorije koristeći:

- Opcije komandne linije FPC-a-gl -gh

- DelfiReportMemoryLeaksOnShutdown := true

- Zamkov mehanizam igre detect_memory_leaks="true" u CastleEngineManifest.xml

Više informacija potražite na https://castle-engine.io/memory_leaks .

**Napomena**:  
Ne morate da oslobađate instance podignutih izuzetaka. Iako kreirate instancu kada podižete izuzetak (i to je sasvim normalna instanca klase). Ali ova instanca klase se automatski oslobađa.

### 5.2 Kako osloboditi

Da biste oslobodili instancu klase, najbolje je pozvati FreeAndNil(A)from SysUtilsunit na vašoj instanci klase. Proverava se da li Aje nil, ako nije — poziva se njen destruktor i postavlja se Ana nil. Dakle, pozivanje više puta zaredom nije greška.

To je manje-više prečica za

```pascal
if A <> nil then
begin
  A.Destroy;
  A := nil;
end;
```

Zapravo, to je preterano pojednostavljivanje, kao i FreeAndNilkoristan trik koji postavlja promenljivu Ana nil pre nego što pozove destruktor na odgovarajućoj referenci. Ovo pomaže u sprečavanju određene klase grešaka — ideja je da "spoljašnji" kod nikada ne bi trebalo da pristupi poluuništenoj instanci klase.

Često ćete videti i ljude koji koriste A.Freemetodu, što je kao da rade

```pascal
if A <> nil then
  A.Destroy;
```

Ovo oslobađa A, osim ako nije nil.

Imajte na umu da u normalnim okolnostima nikada ne bi trebalo da pozivate metodu na instanci koja može biti nil. Dakle, poziv A.Freemože izgledati sumnjivo na prvi pogled, ako Amože biti nil. Međutim, Freemetoda je izuzetak od ovog pravila. Ona radi nešto prljavo u implementaciji — naime, proverava da li je Self <> nil.

**Napomena**:
Ovaj trik (zvanično dozvoljava da se metoda koristi sa `Self equal nil`) je moguć samo u nevirtuelnim metodama.

U implementaciji takve metode, koliko god `Self = nil` je to moguće, metoda ne može pozivati nijednu virtuelnu metodu niti pristupati bilo kom polju, jer bi to izazvalo grešku kršenja pristupa (greška segmentacije) kada se pozove na nilinstanci. Pogledajte primer koda method_with_self_nil.dpr.

Ne preporučujemo korišćenje ovog trika u vašem kodu (za virtuelne ili nevirtuelne metode) jer je kontraintuitivan u odnosu na normalnu upotrebu. Generalno, sve metode instanci trebalo bi da budu u stanju da pretpostave da rade na validnim (ne-nil) instancama i da mogu da pristupe poljima i pozivaju bilo koje druge metode (virtuelne ili ne).

Savetujemo upotrebu metode FreeAndNil(A)always, bez izuzetaka, i nikada direktno pozivanje `Free` metode ili `Destroy` destruktora.

Castle Game Engine to radi tako. Pomaže u održavanju lepe tvrdnje da su sve reference ili nil ili ukazuju na validne instance . Imajte na umu da korišćenje `FreeAndNil(A)` ne garantuje ovu tvrdnju, već pomaže samo u ovome. Na primer, ako kopirate referencu instance i pozovete `FreeAndNil(A)` kopiju, druga kopija će biti viseći pokazivač koji nije nil.

```pascal
A := TMyClass.Create;
B := A;
FreeAndNil(A);
// B now contains a dangling pointer
```

Više o rešavanju ovog problema u kasnijem odeljku o "Besplatnom obaveštenju".

Ipak, `FreeAndNil(A)` bavi se i najtrivijalnijim slučajevima, tako da je dobra navika koristiti ga po mom skromnom mišljenju. Cenićete ga prilikom otklanjanja grešaka, lepo je lako primetiti "X je već oslobođeno, jer X je nil sada".

### 5.3 Ručno i automatsko oslobađanje

U mnogim situacijama, potreba za oslobađanjem instance ne predstavlja veliki problem. Samo napišete destruktor koji se poklapa sa konstruktorom i dealocira sve što je alocirano u konstruktoru (ili, potpunije, tokom celog životnog veka klase). Vodite računa da svaku stvar oslobodite samo jednom . Obično je dobra ideja postaviti oslobođenu referencu na nil, obično je najpogodnije to uraditi pozivanjem funkcije FreeAndNil(A).

Dakle, ovako:

```pascal
uses SysUtils;

type
  TGun = class
  end;

  TPlayer = class
    Gun1, Gun2: TGun;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPlayer.Create;
begin
  inherited;
  Gun1 := TGun.Create;
  Gun2 := TGun.Create;
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(Gun1);
  FreeAndNil(Gun2);
  inherited;
end;
```

Da bi se izbegla potreba za eksplicitnim oslobađanjem instance, može se koristiti i funkcija TComponent" vlasništva" . Objekat koji je u vlasništvu biće automatski oslobođen od strane vlasnika . Mehanizam je pametan i nikada neće osloboditi već oslobođenu instancu (tako da će stvari ispravno funkcionisati i ako ranije ručno oslobodite objekat u vlasništvu). Prethodni primer možemo promeniti u ovo:

```pascal
uses SysUtils, Classes;

type
  TGun = class(TComponent)
  end;

  TPlayer = class(TComponent)
    Gun1, Gun2: TGun;
    constructor Create(AOwner: TComponent); override;
  end;

constructor TPlayer.Create(AOwner: TComponent);
begin
  inherited;
  Gun1 := TGun.Create(Self);
  Gun2 := TGun.Create(Self);
end;
```

Imajte na umu da ovde moramo da prepišemo virtuelni TComponentkonstruktor. Dakle, ne možemo da menjamo parametre konstruktora. (Zapravo, možete — deklarisati novi konstruktor sa reintroduce. Ali budite oprezni, jer će neke funkcionalnosti, npr. strimovanje, i dalje koristiti virtuelni konstruktor, zato se uverite da ispravno radi u oba slučaja.)

Imajte na umu da uvek možete koristiti nilvrednost za vlasnika. Na ovaj način mehanizam "vlasništva" neće biti korišćen za ovu komponentu. Ima smisla ako treba da koristite TComponentpotomka, ali želite da ga uvek ručno oslobodite. Da biste to uradili, kreirali biste potomka komponente ovako: ManualGun := TGun.Create(nil);.

Još jedan mehanizam za automatsko oslobađanje je OwnsObjectsfunkcionalnost (već podrazumevano true!) klasa lista kao što TFPGObjectListsu ili TObjectList. Tako možemo napisati i:

```pascal
uses SysUtils, Classes, FGL;

type
  TGun = class
  end;

  TGunList = {$ifdef FPC}specialize{$endif} TFPGObjectList<TGun>;

  TPlayer = class
    Guns: TGunList;
    Gun1, Gun2: TGun;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPlayer.Create;
begin
  inherited;
  // Actually, the parameter true (OwnsObjects) is already the default
  Guns := TGunList.Create(true);
  Gun1 := TGun.Create;
  Guns.Add(Gun1);
  Gun2 := TGun.Create;
  Guns.Add(Gun2);
end;

destructor TPlayer.Destroy;
begin
  { We have to take care to free the list.
    It will automatically free its contents. }
  FreeAndNil(Guns);

  { 
    No need to free the Gun1, Gun2 anymore. It's a nice habit to set to "nil"
    their references now, as we know they are freed. In this simple class,
    with so simple destructor, it's obvious that they cannot be accessed
    anymore -- but doing this pays off in case of larger and more complicated
    destructors.

    Alternatively, we could avoid declaring Gun1 and Gun2,
    and instead use Guns[0] and Guns[1] in own code.
    Or create a method like Gun1 that returns Guns[0]. 
  }
  Gun1 := nil;
  Gun2 := nil;
  inherited;
end;
```

Imajte na umu da je mehanizam "vlasništva" nad klasama lista jednostavan i da ćete dobiti grešku ako oslobodite instancu koristeći neki drugi način, iako se ona takođe nalazi unutar liste. Koristite metodu Extractda uklonite nešto sa liste bez oslobađanja, čime preuzimate odgovornost da je sami oslobodite.

U Castle Game Engine-u : Potomci TX3DNodeimaju automatsko upravljanje memorijom kada se ubace kao deca drugog TX3DNode. Korenski X3D čvor, TX3DRootNode, je zauzvrat obično u vlasništvu TCastleSceneCore. Neke druge stvari takođe imaju jednostavan mehanizam vlasništva — potražite parametre i svojstva koja se zovu OwnsXxx.

### 5.4 Virtuelni destruktor pod nazivom Destroy

Kao što ste videli u gornjim primerima, kada se klasa uništi, poziva se ona koja se destructorpoziva Destroy.

U teoriji, mogli biste imati više destruktora, ali u praksi to skoro nikada nije dobra ideja. Mnogo je lakše imati samo jedan destruktor Destroykoji se zove , koji se zatim poziva od strane Freemetode, koju zatim poziva FreeAndNilprocedura.

Destruktor Destroyu TObjectje definisan kao virtuelna metoda, tako da bi trebalo uvek da ga označite overrideključnom reči u svim vašim klasama (pošto sve klase potiču od TObject). Ovo čini da Freemetoda ispravno radi. Podsetite se kako virtuelne metode rade iz Virtuelnih metoda, prevazilaženja i ponovnog uvođenja .

**Napomena**:  
Ove informacije o destruktorima su, zaista, nekonzistentne sa konstruktorima .

Normalno je da klasa ima više konstruktora. Obično se svi zovu Createi imaju samo različite parametre, ali je takođe u redu izmisliti i druga imena za konstruktore.

Takođe, `Create` konstruktor u `TObject` nije virtuelan, tako da ga ne obeležavate sa `override` u potomcima.

Sve ovo vam daje malo dodatne fleksibilnosti pri definisanju konstruktora. Često nije potrebno da ih napravite virtuelnim, tako da podrazumevano niste primorani da to uradite.

Međutim, imajte na umu da se ovo menja za `TComponent` potomke. `TComponent` definiše virtuelni konstruktor `Create(AOwner: TComponent)`. Potreban mu je virtuelni konstruktor da bi sistem strimovanja radio. Prilikom definisanja potomaka `TComponent`, trebalo bi da prepišete ovaj konstruktor (i označite ga `override` ključnom reči ) i da izvršite svu inicijalizaciju unutar njega. I dalje je u redu definisati dodatne konstruktore, ali oni treba da deluju samo kao "pomoćnici" . Instanca treba uvek da radi kada se kreira pomoću `Create(AOwner: TComponent)` konstruktora, u suprotnom neće biti ispravno konstruisana prilikom strimovanja. Strimovanje se koristi npr. prilikom čuvanja i učitavanja ove komponente na Lazarus formi.

### 5.5 Besplatno obaveštenje

Ako kopirate referencu na instancu, tako da imate dve reference na istu memoriju, a zatim se jedna od njih oslobodi — druga postaje "viseći pokazivač" . Ne bi trebalo da joj se pristupa, jer ukazuje na memoriju koja više nije alocirana. Pristupanje njoj može dovesti do greške tokom izvršavanja ili vraćanja "smeća" (jer se memorija može ponovo koristiti za druge stvari u vašem programu).

Korišćenje `FreeAndNil` za oslobađanje instance ovde ne pomaže. `FreeAndNil` postavlja `nil` samo referencu koju je dobio — ne postoji način da automatski postavi sve ostale reference. Razmotrite ovaj kod:

```pascal
var
  Obj1, Obj2: TObject;
begin
  Obj1 := TObject.Create;
  Obj2 := Obj1;
  FreeAndNil(Obj1);

  // what happens if we access Obj1 or Obj2 here?
end;
```

- Na kraju ovog bloka, nalazi se "Obj1" `nil`. Ako neki kod mora da mu pristupi, može pouzdano da ga koristi `if Obj1 <> nil then` …​da bi izbegao pozivanje metoda na oslobođenoj instanci, kao što je:

  ```pascal
  if Obj1 <> nil then
    WriteLn(Obj1.ClassName);
  ```  

  Pokušaj pristupa polju instance `nil` rezultira predvidljivim izuzetkom tokom izvršavanja. Dakle, čak i ako neki kod neće proveriti Obj1 <> nili slepo će pristupiti "Obj1" polju, dobićete jasan izuzetak tokom izvršavanja.

  Isto važi i za pozivanje virtuelne metode ili pozivanje nevirtuelne metode koja je pristupila polju instance nil.

  Sa Obj2, stvari su manje predvidljive. Nije nil, ali je nevažeće. Pokušaj pristupa polju nevažeće instance koja nije nula rezultira nepredvidivim ponašanjem — možda izuzetkom kršenja pristupa, možda vraćenim podacima o smeću.

Postoje različita rešenja za to:

- Jedno rešenje je, pa, biti pažljiv i pročitati dokumentaciju. Ne pretpostavljati ništa o životnom veku reference, ako ju je kreirao drugi kod. Ako klasa TCarima polje koje ukazuje na neku instancu klase , konvencijaTWheel je da je referenca na klasu ` wheel` validna sve dok postoji referenca na klasu ` car` , a klasa ` car` će osloboditi svoje točkove unutar svog destruktora. Ali to je samo konvencija, dokumentacija bi trebalo da pomene ako se dešava nešto komplikovanije.

  U gornjem primeru, odmah nakon oslobađanja Obj1instance, možete jednostavno Obj2eksplicitno postaviti promenljivu na nil. To je trivijalno u ovom jednostavnom slučaju.

- Najbolje rešenje za budućnost je korišćenje TComponentmehanizma klase "obaveštavanja o slobodi". Jedna komponenta može biti obaveštena kada se druga komponenta oslobodi i tako postaviti njenu referencu na nil.

Tako dobijate nešto poput slabe reference . Može se nositi sa različitim scenarijima upotrebe, na primer, možete dozvoliti kodu izvan klase da postavi vašu referencu, a spoljni kod takođe može osloboditi instancu u bilo kom trenutku.

Ovo zahteva da obe klase potiču od TComponent. Njegova upotreba se generalno svodi na pozivanje FreeNotification, RemoveFreeNotificationi prepisivanje Notification.

Evo kompletnog primera koji pokazuje kako se koristi ovaj mehanizam, zajedno sa konstruktorom/destruktorom i svojstvom setera. Ponekad se može uraditi jednostavnije, ali ovo je potpuna verzija koja je uvek ispravna :)
  
```pascal
type
  TControl = class(TComponent)
  end;
 TContainer = class(TComponent)
  private
    FSomeSpecialControl: TControl;
    procedure SetSomeSpecialControl(const Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    property SomeSpecialControl: TControl
      read FSomeSpecialControl write SetSomeSpecialControl;
  end;
implementation
procedure TContainer.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSomeSpecialControl) then
    { set to nil by SetSomeSpecialControl to clean nicely }
    SomeSpecialControl := nil;
  end;
procedure TContainer.SetSomeSpecialControl(const Value: TControl);
begin
  if FSomeSpecialControl <> Value then
  begin
    if FSomeSpecialControl <> nil then
      FSomeSpecialControl.RemoveFreeNotification(Self);
    FSomeSpecialControl := Value;
    if FSomeSpecialControl <> nil then
      FSomeSpecialControl.FreeNotification(Self);
  end;
end;

destructor TContainer.Destroy;
begin
  { set to nil by SetSomeSpecialControl, to detach free notification }
  SomeSpecialControl := nil;
  inherited;
end;
```

### 5.6 Besplatni posmatrač obaveštenja (Castle Game Engine)

U Castle Game Engine - u podstičemo korišćenje `TFreeNotificationObserver` iz `CastleClassUtils` unita umesto direktnog pozivanja `FreeNotification` i `RemoveFreeNotification` prevazilaženja `Notification`.

Generalno, korišćenje `TFreeNotificationObserver` izgleda malo jednostavnije nego `FreeNotification` direktno korišćenje mehanizma (mada priznajem da je to stvar ukusa). Ali posebno kada se ista instanca klase mora posmatrati zbog više razloga , onda `TFreeNotificationObserver` je mnogo jednostavnije za korišćenje (direktno korišćenje `FreeNotification` u ovom slučaju može postati komplikovano, jer morate paziti da ne odjavite obaveštenje prerano).

Ovo je primer koda koji koristi `TFreeNotificationObserver`, da bi se postigao isti efekat kao u primeru u prethodnom odeljku:

```pascal
type
  TControl = class(TComponent)
  end;

  TContainer = class(TComponent)
  private
    FSomeSpecialControlObserver: TFreeNotificationObserver;
    FSomeSpecialControl: TControl;
    procedure SetSomeSpecialControl(const Value: TControl);
    procedure SomeSpecialControlFreeNotification(const Sender: TFreeNotificationObserver);
  public
    constructor Create(AOwner: TComponent); override;
    property SomeSpecialControl: TControl
      read FSomeSpecialControl write SetSomeSpecialControl;
  end;

implementation

uses CastleComponentSerialize;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited;
  FSomeSpecialControlObserver := TFreeNotificationObserver.Create(Self);
  FSomeSpecialControlObserver.OnFreeNotification := {$ifdef FPC}@{$endif} SomeSpecialControlFreeNotification;
end;

procedure TContainer.SetSomeSpecialControl(const Value: TControl);
begin
  if FSomeSpecialControl <> Value then
  begin
    FSomeSpecialControl := Value;
    FSomeSpecialControlObserver.Observed := Value;
  end;
end;

procedure TContainer.SomeSpecialControlFreeNotification(const Sender: TFreeNotificationObserver);
begin
  // set property to nil when the referenced component is freed
  SomeSpecialControl := nil;
end;
```

Pogledajte https://castle-engine.io/custom_components.

## 6 Izuzeci

### 6.1 Pregled

Izuzeci dozvoljavaju prekid normalnog izvršavanja koda.

    U bilo kom trenutku u programu, možete pokrenuti izuzetak koristeći raiseključnu reč . U stvari, linije koda koje slede nakon raise …​poziva se neće izvršiti.

    Izuzetak se može uhvatiti pomoću try …​ except …​ endkonstrukcije. Hvatanje izuzetka znači da se na neki način "obrađujete" sa izuzetkom, i sledeći kod bi trebalo da se izvrši kao i obično, izuzetak se više ne širi naviše.

    Napomena: Ako se izuzetak podigne, ali nikada ne bude uhvaćen, to će prouzrokovati da se cela aplikacija zaustavi sa greškom.

        Ali u LCL aplikacijama, izuzeci se uvek hvataju oko događaja (i uzrokuju LCL dijaloški prozor) ako ih ne hvatate ranije.

        U aplikacijama Castle Game Engine-aCastleWindow koje koriste , slično tome uvek hvatamo izuzetke oko vaših događaja (i prikazujemo odgovarajući dijaloški prozor).

        Dakle, nije tako lako napraviti izuzetak koji nije nigde uhvaćen (nije uhvaćen u vašem kodu, LCL kodu, CGE kodu…).

    Iako izuzetak prekida izvršavanje, možete koristiti konstrukciju da uvektry …​ finally …​ end izvršite neki kod , čak i ako je kod prekinut izuzetkom.

    Konstrukcija try …​ finally …​ endtakođe funkcioniše kada je kod prekinut ključnim rečima Break`or` Continueili Exit`or`. Poenta je da se kod uvek izvrši u finallyodeljku.

"Izuzetak" je, generalno, bilo koja instanca klase.

    Kompilator ne nameće nijednu određenu klasu. Samo morate pozvati ` raise XXXwhere` XXXje instanca bilo koje klase. Bilo koja klasa (dakle, sve što potiče od TObject) je u redu.

    Standardna je konvencija da klase izuzetaka potiču od posebne Exceptionklase. ExceptionKlasa proširuje TObject, dodajući svojstvo stringa Messagei konstruktor za lako podešavanje ovog svojstva. Svi izuzeci koje generiše standardna biblioteka potiču od Exception. Savetujemo da se prati ova konvencija.

    Klase izuzetaka (po konvenciji) imaju imena koja počinju sa E, a ne Tsa . Kao ESomethingBadHappened.

    Kompilator će automatski osloboditi objekat izuzetka kada se obradi. Nemojte ga sami oslobađati.

    U većini slučajeva, objekat se konstruiše istovremeno kada se poziva raise, kao raise ESomethingBadHappened.Create('Description of what bad thing happened.').

### 6.2. Podizanje

Ako želite da pokrenete sopstveni izuzetak, deklarišite ga i pozovite raise …​kada je to potrebno:

```pascal
type
  EInvalidParameter = class(Exception);

function ReadParameter: String;
begin
  Result := Readln;
  if Pos(' ', Result) <> 0 then
    raise EInvalidParameter.Create('Invalid parameter, space is not allowed');
end;
```

Imajte na umu da izraz koji sledi raisetreba da bude validna instanca klase koju treba podići. Skoro uvek ćete ovde kreirati instancu izuzetka.

Takođe možete koristiti CreateFmtkonstruktor, što je praktična prečica za Create(Format(MessageFormat, MessageArguments)). Ovo je uobičajen način da se poruci o izuzetku pruži više informacija. Prethodni primer možemo poboljšati ovako:

```pascal
type
  EInvalidParameter = class(Exception);

function ReadParameter: String;
begin
  Result := Readln;
  if Pos(' ', Result) <> 0 then
    raise EInvalidParameter.CreateFmt('Invalid parameter %s, space is not allowed', [Result]);
end;
```

### 6.3. Hvatanje

Možete uhvatiti izuzetak ovako:

```pascal
var
  Parameter1, Parameter2, Parameter3: String;
begin
  try
    WriteLn('Input 1st parameter:');
    Parameter1 := ReadParameter;
    WriteLn('Input 2nd parameter:');
    Parameter2 := ReadParameter;
    WriteLn('Input 3rd parameter:');
    Parameter3 := ReadParameter;
  except
    // capture EInvalidParameter raised by one of the above ReadParameter calls
    on EInvalidParameter do
      WriteLn('EInvalidParameter exception occurred');
  end;
end;
```

Da bismo poboljšali gornji primer, možemo deklarisati ime za instancu izuzetka (koristićemo ga Eu primeru). Na ovaj način možemo ispisati poruku o izuzetku:

```pascal
try
...
except
  on E: EInvalidParameter do
    WriteLn('EInvalidParameter exception occurred with message: ' + E.Message);
end;
```

Takođe bi se moglo testirati za više klasa izuzetaka:

```pascal
try
...
except
  on E: EInvalidParameter do
    WriteLn('EInvalidParameter exception occurred with message: ' + E.Message);
  on E: ESomeOtherException do
    WriteLn('ESomeOtherException exception occurred with message: ' + E.Message);
end;
```

Takođe možete reagovati na bilo koji izuzetak, ako ne koristite nijedan onizraz:

```pascal
try
...
except
  WriteLn('Warning: Some exception occurred');
end;
// WARNING: DO NOT FOLLOW THIS EXAMPLE WITHOUT READING A WARNING BELOW
// ABOUT "CAPTURING ALL EXCEPTIONS"
```

Generalno, trebalo bi da hvatate samo izuzetke određene klase, koji signaliziraju određeni problem sa kojim znate šta da radite . Budite oprezni sa hvatanjem izuzetaka opšteg tipa (kao što je hvatanje bilo kog Exceptionili bilo kog TObject), jer možete lako hvatati previše izuzetaka i kasnije izazvati probleme prilikom debagovanja drugih problema. Kao i u svim programskim jezicima sa izuzecima, dobro pravilo kojeg se treba pridržavati je da nikada ne hvatate izuzetak koji ne znate kako da rešite . Posebno, nemojte hvatati izuzetak samo kao jednostavno rešenje problema, bez prethodnog istraživanja zašto se izuzetak javlja.

    Da li izuzetak ukazuje na problem u korisničkom unosu? Onda bi trebalo da ga prijavite korisniku.

    Da li izuzetak ukazuje na grešku u vašem kodu? Onda bi trebalo da ispravite kod kako biste uopšte sprečili da se izuzetak pojavi.

Drugi način da se uhvate svi izuzeci je korišćenje:

```pascal
try
...
except
  on E: TObject do
    WriteLn('Warning: Some exception occurred');
end;
// WARNING: DO NOT FOLLOW THIS EXAMPLE WITHOUT READING A WARNING ABOVE
// ABOUT "CAPTURING ALL EXCEPTIONS"
```

Iako je obično dovoljno da se snimi Exception:

```pascal
try
...
except
  on E: Exception do
    WriteLn('Warning: Some exception occurred: ' + E.ClassName + ', message: ' + E.Message);
end;
// WARNING: DO NOT FOLLOW THIS EXAMPLE WITHOUT READING A WARNING ABOVE
// ABOUT "CAPTURING ALL EXCEPTIONS"
```

Možete "ponovo pokrenuti" izuzetak u except …​ endbloku, ako tako odlučite. Možete to učiniti samo raise Eako je instanca izuzetka E, takođe možete koristiti bez parametara raise. Na primer:

```pascal
try
...
except
  on E: EInvalidSoundFile do
  begin
    if E.InvalidUrl = 'http://example.com/blablah.wav' then
      WriteLn('Warning: loading http://example.com/blablah.wav failed, ignore it')
    else
      raise;
  end;
end;
```

Imajte na umu da, iako je izuzetak instanca objekta, nikada ga ne bi trebalo ručno oslobađati nakon pokretanja. Kompilator će generisati odgovarajući kod koji osigurava oslobađanje objekta izuzetka nakon što se obradi.

### 6.4. Konačno (izvršavanje stvari bez obzira na to da li se dogodio izuzetak)

Često se konstrukcija koristi `try .. finally .. end` za oslobađanje instance nekog objekta, bez obzira na to da li se dogodio izuzetak prilikom korišćenja ovog objekta. Način pisanja izgleda ovako:

```pascal
procedure MyProcedure;
var
  MyInstance: TMyClass;
begin
  MyInstance := TMyClass.Create;
  try
    MyInstance.DoSomething;
    MyInstance.DoSomethingElse;
  finally
    FreeAndNil(MyInstance);
  end;
end;
```

Ovo uvek funkcioniše i ne izaziva curenje memorije, čak i ako "MyInstance.DoSomething" ili "MyInstance.DoSomethingElse" izazove izuzetak.

Imajte na umu da ovo uzima u obzir da lokalne promenljive, kao "MyInstance" gore, imaju nedefinisane vrednosti (mogu sadržati slučajno "memorijsko smeće") pre prvog dodeljivanja. To jest, pisanje nečega poput ovoga ne bi bilo validno:

```pascal
// INCORRECT EXAMPLE:
procedure MyProcedure;
var
  MyInstance: TMyClass;
begin
  try
    CallSomeOtherProcedure;
    MyInstance := TMyClass.Create;
    MyInstance.DoSomething;
    MyInstance.DoSomethingElse;
  finally
    FreeAndNil(MyInstance);
  end;
end;
```

Gore navedeni primer nije validan: ako se izuzetak desi unutar TMyClass.Create(konstruktor takođe može da podigne izuzetak) ili unutar CallSomeOtherProcedure, tada MyInstancepromenljiva nije inicijalizovana. Pozivanje FreeAndNil(MyInstance)će pokušati da pozove destruktor MyInstance, što će najverovatnije dovesti do pada programa sa greškom kršenja pristupa (greška segmentacije) . U stvari, jedan izuzetak izaziva drugi izuzetak, što će izveštaj o grešci učiniti ne baš korisnim: nećete videti poruku originalnog izuzetka.

Ponekad je opravdano popraviti gornji kod tako što se prvo inicijalizuju sve lokalne promenljive na nil(na kojima FreeAndNilje pozivanje bezbedno i neće ništa uraditi). Ovo ima smisla ako oslobodite mnogo instanci klase. Dakle, dva primera koda ispod podjednako dobro funkcionišu:

```pascal
procedure MyProcedure;
var
  MyInstance1: TMyClass1;
  MyInstance2: TMyClass2;
  MyInstance3: TMyClass3;
begin
  MyInstance1 := TMyClass1.Create;
  try
    MyInstance1.DoSomething;

    MyInstance2 := TMyClass2.Create;
    try
      MyInstance2.DoSomethingElse;

      MyInstance3 := TMyClass3.Create;
      try
        MyInstance3.DoYetAnotherThing;
      finally
        FreeAndNil(MyInstance3);
      end;
    finally
      FreeAndNil(MyInstance2);
    end;
  finally
    FreeAndNil(MyInstance1);
  end;
end;
```

Verovatno je čitljivije u donjem obliku:

```pascal
procedure MyProcedure;
var
  MyInstance1: TMyClass1;
  MyInstance2: TMyClass2;
  MyInstance3: TMyClass3;
begin
  MyInstance1 := nil;
  MyInstance2 := nil;
  MyInstance3 := nil;
  try
    MyInstance1 := TMyClass1.Create;
    MyInstance1.DoSomething;

    MyInstance2 := TMyClass2.Create;
    MyInstance2.DoSomethingElse;

    MyInstance3 := TMyClass3.Create;
    MyInstance3.DoYetAnotherThing;
  finally
    FreeAndNil(MyInstance3);
    FreeAndNil(MyInstance2);
    FreeAndNil(MyInstance1);
  end;
end;
```

**Napomena**:  
U ovom jednostavnom primeru, takođe biste mogli dati validan argument da kod treba podeliti na 3 odvojene procedure, od kojih jedna poziva drugu.

Poslednji deo bloka `try .. finally .. end` se izvršava u većini mogućih scenarija kada napustite glavni kod. Razmotrite ovo:

```pascal
try
  A;
finally
  B;
end;
```

Dakle, Bizvršiće se ako

- Izazvalo Aje (i nije ga uhvatilo) izuzetak.

- Ili ćete pozvati Exitili (ako ste u toku) Breakili Continueodmah nakon pozivanja A.

- Ili se ništa od navedenog nije desilo, i kod se Ajednostavno izvršio bez ikakvog izuzetka, a vi niste pozvali Exit, Breakili Continuebilo šta drugo.

Jedini način da se zaista izbegne Bizvršavanje jeste da se bezuslovno prekine proces aplikacije korišćenjem `` Haltili nekih platformski specifičnih API-ja (kao što je libc exit na Unix-u ) unutar ` A. Što se generalno ne bi trebalo raditi — fleksibilnije je koristiti izuzetke za prekid aplikacije, jer to omogućava nekom drugom kodu da ima priliku da se očisti.

**Napomena**:  
Ne `try .. finally .. end` hvata izuzetak. Izuzetak će se i dalje širiti naviše i može ga hvatati blok `try .. except .. end` van ovog.

Primer `try .. finally .. end` zajedno sa `Exit` pozivima:

```pascal
procedure MyProcedure;
begin
  try
    WriteLn('Do something');
    Exit;
    WriteLn('This will not happen');
  finally
    WriteLn('This will happen regardless of whether we have left the block through Exception, Exit, Continue, Break, etc.');
  end;
  WriteLn('This will not happen');
end;
```

Pogledajte poglavlje o izuzecima za detaljniji opis izuzetaka, uključujući kako `raise` koristiti i `try …​ except …​ end` kako ih hvatati.

### 6.5. Kako različite biblioteke prikazuju izuzetke

- U slučaju Lazarus LCL-a, izuzeci koji se javljaju tokom događaja (razni povratni pozivi dodeljeni OnXxxsvojstvima LCL komponenti) biće zabeleženi i rezultiraće lepom dijaloškom porukom koja omogućava korisniku da nastavi i zaustavi aplikaciju. To znači da vaši sopstveni izuzeci ne "izlaze" iz Application.ProcessMessages, tako da automatski ne prekidaju aplikaciju. Možete konfigurisati šta se dešava koristeći TApplicationProperties.OnException.

- Slično tome, u slučaju Castle Game Engine-a sa CastleWindow: izuzetak se interno beleži i rezultira lepom porukom o grešci. Dakle, izuzeci ne "izlaze" iz Application.ProcessMessages. Ponovo, možete konfigurisati šta se dešava koristeći Application.OnException.

- Neke druge GUI biblioteke mogu da urade slično kao gore navedeno.

- U slučaju drugih aplikacija, možete konfigurisati kako se izuzetak prikazuje dodeljivanjem globalnog povratnog poziva funkciji OnHaltProgram.

## 7. Runtime biblioteka

### 7.1. Ulaz/izlaz korišćenjem tokova podataka

Moderni programi bi trebalo da koriste `TStream` klasu i njene brojne potomke za obavljanje ulaz/izlaz. Ona ima mnogo korisnih potomaka, kao što su `TFileStream`, `TMemoryStream`, `TStringStream`.

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils, Classes;

var
  S: TStream;
  InputInt, OutputInt: Integer;
begin
  InputInt := 666;

  S := TFileStream.Create('my_binary_file.data', fmCreate);
  try
    S.WriteBuffer(InputInt, SizeOf(InputInt));
  finally
    FreeAndNil(S);
  end;

  S := TFileStream.Create('my_binary_file.data', fmOpenRead);
  try
    S.ReadBuffer(OutputInt, SizeOf(OutputInt));
  finally
    FreeAndNil(S);
  end;

  WriteLn('Read from file got integer: ', OutputInt);
end.
```

U Castle Game Engine-u : Trebalo bi da koristite Downloadfunkciju da biste kreirali strim koji dobija podatke sa bilo koje URL adrese. Na ovaj način se podržavaju regularne datoteke, HTTP i HTTPS resursi, Android resursi i još mnogo toga. Štaviše, da biste otvorili resurs unutar podataka vaše igre (u datapoddirektorijumu), koristite posebnu castle-data:/xxxURL adresu. Primeri:

```pascal
EnableNetwork := true;
S := Download('https://castle-engine.io/latest.zip');

S := Download('file:///home/michalis/my_binary_file.data');

S := Download('castle-data:/gui/my_image.png');
```

Za čitanje tekstualnih datoteka, savetujemo korišćenje TCastleTextReaderklase . Ona pruža API orijentisan na linije i obuhvata TStreamunutrašnjost. TCastleTextReaderKonstruktor može da uzme gotov URL ili možete da mu prosledite svoj prilagođeni TStreamizvorni kod.

```pascal
Text := TCastleTextReader.Create('castle-data:/my_data.txt');
try
  while not Text.Eof do
    WriteLnLog('NextLine', Text.ReadLn);
finally
  FreeAndNil(Text);
end;
```

Dokumentacija svih funkcija Castle Game Engine-a za učitavanje i čuvanje strimova, uključujući Downloadfunkciju i TCastleTextReaderklasu, nalazi se na <https://castle-engine.io/url>.

### 7.2. Kontejneri (liste, rečnici) korišćenjem generičkih klasifikacija

Jezik i biblioteka za izvršavanje nude razne fleksibilne kontejnere. Postoji veliki broj negeneričkih klasa (kao što su TListi TObjectListiz Contnrsjedinice), a postoje i dinamički nizovi ( array of TMyType). Ali da biste postigli najveću fleksibilnost i bezbednost tipa, savetujem da za većinu vaših potreba koristite generičke kontejnere .

Generički kontejneri vam pružaju mnogo korisnih metoda za dodavanje, uklanjanje, iteraciju, pretragu, sortiranje… Kompilator takođe zna (i proverava) da kontejner sadrži samo stavke odgovarajućeg tipa.

Trenutno postoje tri biblioteke koje pružaju generičke kontejnere u FPC-u:

- Generics.Collectionsjedinica i prijatelji (od FPC >= 3.2.0)

- FGLjedinica

- GVector unit i prijatelji (zajedno u fcl-stl)

Savetujemo korišćenje Generics.Collectionsjedinice. Generički kontejneri koje ona implementira su

- prepun korisnih karakteristika,

- veoma efikasno (posebno važno za pristup rečnicima pomoću ključeva),

- kompatibilan između FPC-a i Delphi-ja,

- Imenovanje je u skladu sa drugim delovima standardne biblioteke (kao što su negenerički kontejneri iz Contnrsjedinice).

U Castle Game Engine-u : Intenzivno ga koristimo Generics.Collectionsu celom endžinu i savetujemo vam da ga koristite Generics.Collectionsi u svojim aplikacijama!

Najvažnije klase iz ove Generics.Collections unita su:

**TList**:  
Generička lista tipova.

**Lista objekata**  
Generička lista instanci objekata. Može da "poseduje" decu, što znači da će ih automatski osloboditi.

**TDictionary**  
Generički rečnik.

**TObjectDictionary**:
Generički rečnik koji može da "poseduje" ključeve i/ili vrednosti.

Evo kako se koristi jednostavan generički izraz TObjectList:

```pascal
{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, Generics.Collections;

type
  TApple = class
    Name: string;
  end;

  TAppleList = {$ifdef FPC}specialize{$endif} TObjectList<TApple>;

var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := 'my apple';
    Apples.Add(A);

    A := TApple.Create;
    A.Name := 'another apple';
    Apples.Add(A);

    Writeln('Count: ', Apples.Count);
    Writeln(Apples[0].Name);
    Writeln(Apples[1].Name);
  finally FreeAndNil(Apples) end;
end.
```

Imajte na umu da neke operacije zahtevaju poređenje dve stavke, kao što su sortiranje i pretraživanje (npr. pomoću Sorti IndexOfmetoda). Generics.CollectionsKontejneri koriste upoređivač za ovo. Podrazumevani upoređivač je razuman za sve tipove, čak i za zapise (u kom slučaju upoređuje sadržaj memorije, što je razuman podrazumevani podešavanje barem za pretraživanje pomoću IndexOf).

Prilikom sortiranja liste možete dati prilagođeni uporednik kao parametar. Uporednik je klasa koja implementira IComparerinterfejs. U praksi, obično definišete odgovarajući povratni poziv i koristite TComparer<T>.Constructmetodu da biste ovaj povratni poziv upakovali u IComparerinstancu. Primer za to je dat u nastavku:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{ If GENERICS_CONSTREF is defined, then various routines used with Generics.Collections
  (like callbacks we pass to TComparer, or OnNotify callback or Notify virtual method)
  should have "constref" parameter, not "const".
  This was the case of FPC<= 3.2.0, FPC changed it in
  https://gitlab.com/freepascal.org/fpc/source/-/commit/693491048bf2c6f9122a0d8b044ad0e55382354d .
  It is also applied to FPC fixes branch 3.2.3 and later 3.2.4(rc1). }
{$ifdef VER3_0} {$define GENERICS_CONSTREF} {$endif}
{$ifdef VER3_2_0} {$define GENERICS_CONSTREF} {$endif}
{$ifdef VER3_2_2} {$define GENERICS_CONSTREF} {$endif}

uses SysUtils, Generics.Defaults, Generics.Collections;

type
  TApple = class
    Name: string;
  end;

  TAppleList = {$ifdef FPC}specialize{$endif} TObjectList<TApple>;

function CompareApples(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif}
  Left, Right: TApple): Integer;
begin
  Result := AnsiCompareStr(Left.Name, Right.Name);
end;

type
  TAppleComparer = {$ifdef FPC}specialize{$endif} TComparer<TApple>;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Sort(TAppleComparer.Construct({$ifdef FPC}@{$endif} CompareApples));

    Writeln('Count: ', L.Count);
    Writeln(L[0].Name);
    Writeln(L[1].Name);
    Writeln(L[2].Name);
  finally FreeAndNil(L) end;
end.

Klasa TDictionaryimplementira rečnik , takođe poznat kao mapa (ključ → vrednost) , takođe poznata kao asocijativni niz . Njen API je donekle sličan C# TDictionaryklasi. Ima korisne iteratore za ključeve, vrednosti i parove ključ → vrednost.

Primer korišćenja rečnika:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, Generics.Collections;

type
  TApple = class
    Name: string;
  end;

  TAppleDictionary = {$ifdef FPC}specialize{$endif} TDictionary<String, TApple>;

var
  Apples: TAppleDictionary;
  A, FoundA: TApple;
  ApplePair: {$ifdef FPC} TAppleDictionary.TDictionaryPair {$else} TPair<String, TApple> {$endif};
  AppleKey: string;
begin
  Apples := TAppleDictionary.Create;
  try
    A := TApple.Create;
    A.Name := 'my apple';
    Apples.AddOrSetValue('apple key 1', A);

    if Apples.TryGetValue('apple key 1', FoundA) then
      Writeln('Found apple under key "apple key 1" with name: ' +
        FoundA.Name);

    for AppleKey in Apples.Keys do
      Writeln('Found apple key: ' + AppleKey);
    for A in Apples.Values do
      Writeln('Found apple value: ' + A.Name);
    for ApplePair in Apples do
      Writeln('Found apple key->value: ' +
        ApplePair.Key + '->' + ApplePair.Value.Name);

    { Line below works too, but it can only be used to set
      an *existing* dictionary key.
      Instead of this, usually use AddOrSetValue
      to set or add a new key, as necessary. }
    // Apples['apple key 1'] := ... ;

    Apples.Remove('apple key 1');

    { Note that the TDictionary doesn't own the items,
      you need to free them yourself.
      We could use TObjectDictionary to have automatic ownership
      mechanism. }
    A.Free;
  finally FreeAndNil(Apples) end;
end.

Takođe TObjectDictionarymože da poseduje ključeve i/ili vrednosti rečnika, što znači da će biti automatski oslobođeni. Vodite računa da posedujete ključeve i/ili vrednosti samo ako su instance objekta . Ako podesite na "poseduje" neki drugi tip, kao što je Integer(na primer, ako su vaši ključevi Integer, i uključite doOwnsKeys), dobićete gadan pad kada se kod izvrši.

Primer koda koji koristi TObjectDictionaryje dat ispod. Kompajlirajte ovaj primer sa detekcijom curenja memorije , kao što je fpc -gl -gh generics_object_dictionary.dpr, da biste videli da li je sve oslobođeno kada se program završi.

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, Generics.Collections;

type
  TApple = class
    Name: string;
  end;

  TAppleDictionary = {$ifdef FPC}specialize{$endif} TObjectDictionary<String, TApple>;

var
  Apples: TAppleDictionary;
  A: TApple;
  ApplePair: {$ifdef FPC} TAppleDictionary.TDictionaryPair {$else} TPair<String, TApple> {$endif};
begin
  Apples := TAppleDictionary.Create([doOwnsValues]);
  try
    A := TApple.Create;
    A.Name := 'my apple';
    Apples.AddOrSetValue('apple key 1', A);

    for ApplePair in Apples do
      Writeln('Found apple key->value: ' +
        ApplePair.Key + '->' + ApplePair.Value.Name);

    Apples.Remove('apple key 1');
  finally FreeAndNil(Apples) end;
end.

Ako više volite da koristite FGLjedinicu umesto Generics.Collections, najvažnije klase iz FGLjedinice su:

TFPGLista

    Generički spisak tipova.
TFPGObjekatLista

    Generička lista instanci objekata. Može da "poseduje" decu.
TFPGMap

    Generički rečnik.

U FGLjedinici, TFPGListmože se koristiti samo za tipove za koje je definisan operator jednakosti (=). Za tip ključa moraju biti definisani TFPGMapoperatori "veće od" (>) i "manje od" (<). Ako želite da koristite ove liste sa tipovima koji nemaju ugrađene operatore poređenja (npr. sa zapisima), morate preopteretiti njihove operatore kao što je prikazano u odeljku Preopterećenje operatora .

U Castle Game Engine- u uključujemo jedinicu CastleGenericListskoja dodaje klase TGenericStructListi TGenericStructMap. Slične su klasama TFPGListi TFPGMap, ali ne zahtevaju definiciju operatora poređenja za odgovarajući tip (umesto toga, upoređuju sadržaj memorije, što je često prikladno za zapise ili pokazivače metoda). Međutim, jedinica CastleGenericListsje zastarela od verzije endžina 6.3, jer umesto nje savetujemo upotrebu Generics.Collections.

Ako želite da saznate više o generičkim lekovima, pogledajte Generici .
7.3. Kloniranje: TPersistent.Assign

Kopiranje instanci klase jednostavnim operatorom dodele kopira referencu .

var
  X, Y: TMyObject;
begin
  X := TMyObject.Create;
  Y := X;
  // X and Y are now two pointers to the same data
  Y.MyField := 123; // this also changes X.MyField
  FreeAndNil(X);
end;

Da biste kopirali sadržaj instance klase , standardni pristup je da izvedete svoju klasu iz TPersistenti prepišete njenu Assignmetodu. Kada je pravilno implementirana u TMyObject, koristite je ovako:

var
  X, Y: TMyObject;
begin
  X := TMyObject.Create;
  Y := TMyObject.Create;
  Y.Assign(X);
  Y.MyField := 123; // this does not change X.MyField
  FreeAndNil(X);
  FreeAndNil(Y);
end;

Da bi ovo funkcionisalo, potrebno je da implementirate Assignmetodu koja zapravo kopira željena polja. Trebalo bi pažljivo da implementirate Assignmetodu, da biste kopirali iz klase koja može biti potomak trenutne klase.

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils, Classes;

type
  TMyClass = class(TPersistent)
  public
    MyInt: Integer;
    procedure Assign(Source: TPersistent); override;
  end;

  TMyClassDescendant = class(TMyClass)
  public
    MyString: string;
    procedure Assign(Source: TPersistent); override;
  end;

procedure TMyClass.Assign(Source: TPersistent);
var
  SourceMyClass: TMyClass;
begin
  if Source is TMyClass then
  begin
    SourceMyClass := TMyClass(Source);
    MyInt := SourceMyClass.MyInt;
    // Xxx := SourceMyClass.Xxx; // add new fields here
  end else
    { Since TMyClass is a direct TPersistent descendant,
      it calls inherited ONLY when it cannot handle Source class.
      See comments below. }
    inherited Assign(Source);
end;

procedure TMyClassDescendant.Assign(Source: TPersistent);
var
  SourceMyClassDescendant: TMyClassDescendant;
begin
  if Source is TMyClassDescendant then
  begin
    SourceMyClassDescendant := TMyClassDescendant(Source);
    MyString := SourceMyClassDescendant.MyString;
    // Xxx := SourceMyClassDescendant.Xxx; // add new fields here
  end;

  { Since TMyClassDescendant has an ancestor that already overrides
    Assign (in TMyClass.Assign), it calls inherited ALWAYS,
    to allow TMyClass.Assign to handle remaining fields.
    See comments below for a detailed reasoning. }
  inherited Assign(Source);
end;

var
  C1, C2: TMyClass;
  CD1, CD2: TMyClassDescendant;
begin
  // test TMyClass.Assign
  C1 := TMyClass.Create;
  C2 := TMyClass.Create;
  try
    C1.MyInt := 666;
    C2.Assign(C1);
    WriteLn('C2 state: ', C2.MyInt);
  finally
    FreeAndNil(C1);
    FreeAndNil(C2);
  end;

  // test TMyClassDescendant.Assign
  CD1 := TMyClassDescendant.Create;
  CD2 := TMyClassDescendant.Create;
  try
    CD1.MyInt := 44;
    CD1.MyString := 'blah';
    CD2.Assign(CD1);
    WriteLn('CD2 state: ', CD2.MyInt, ' ', CD2.MyString);
  finally
    FreeAndNil(CD1);
    FreeAndNil(CD2);
  end;
end.

Ponekad je udobnije alternativno prepisati AssignTometodu u izvornoj klasi, umesto prepisati Assignmetodu u odredišnoj klasi.

Budite oprezni kada pozivate inheritedzamenjenu Assignimplementaciju. Postoje dve situacije:

Vaša klasa je direktni potomak klase TPersistent. (Ili, nije direktni potomak klase TPersistent, ali nijedan predak nije nadjačao Assignmetodu.)

    U ovom slučaju, vaša klasa treba da koristi inheritedključnu reč (za pozivanje TPersistent.Assign) samo ako ne možete da obradite dodelu u svom kodu .
Vaša klasa potiče od neke klase koja je već prepisala Assignmetodu.

    U ovom slučaju, vaša klasa treba uvek da koristi inheritedključnu reč (za pozivanje pretka Assign). Generalno, pozivanje inheritedprepisanih metoda je obično dobra ideja.

Da bismo razumeli razlog koji stoji iza gore navedenog pravila (kada treba pozvati, a kada ne treba pozvati inheritediz Assignimplementacije) i kako se ono odnosi na AssignTometodu, pogledajmo implementacije TPersistent.Assigni TPersistent.AssignTo:

procedure TPersistent.Assign(Source: TPersistent);
begin
  if Source <> nil then
    Source.AssignTo(Self)
  else
    raise EConvertError...
end;

procedure TPersistent.AssignTo(Destination: TPersistent);
begin
  raise EConvertError...
end;

Napomena
	Ovo nije tačna implementacija od TPersistent. Kopirao sam kod standardne biblioteke FPC, ali sam ga zatim pojednostavio da bih sakrio nevažne detalje o poruci o izuzetku.

Zaključci koje možete izvući iz gore navedenog su sledeći:

    Ako nijedno od , Assignni AssignTonisu zamenjeni , njihovo pozivanje će rezultirati izuzetkom.

    Takođe, imajte na umu da ne postoji kod u TPersistentimplementaciji koji automatski kopira sva polja (ili sva objavljena polja) klasa. Zato to morate sami da uradite, tako što ćete prepisati Assignsve klase. Za to možete koristiti RTTI (informacije o tipu izvršavanja), ali za jednostavne slučajeve verovatno ćete samo ručno navesti polja koja treba kopirati.

Kada imate klasu poput TApple, vaša TApple.Assignimplementacija se obično bavi kopiranjem polja koja su specifična za tu TAppleklasu (ne za TApplepretka, kao što je TFruit). Dakle, TApple.Assignimplementacija obično proverava Source is TApplena početku, pre nego što kopira polja vezana za apple. Zatim, poziva inherited`allow` TFruitda bi obradila ostatak polja.

Pod pretpostavkom da ste implementirali TFruit.Assigni TApple.Assignpratili standardni obrazac (kao što je prikazano u gornjem primeru), efekat je ovakav:

    Ako prosledite TAppleinstancu na TApple.Assign, radiće i kopiraće sva polja.

    Ako prosledite TOrangeinstancu na TApple.Assign, ona će raditi i kopirati samo zajednička polja koja dele TOrangei i TApple. Drugim rečima, polja definisana u TFruit.

    Ako prosledite TWerewolfinstancu funkciji TApple.Assign, ona će izazvati izuzetak (jer TApple.Assignće pozvati funkciju TFruit.Assign, koja će pozvati funkciju TPersistent.Assign, koja će izazvati izuzetak).

Napomena
	Zapamtite da je prilikom spuštanja od TPersistent, podrazumevani specifikator vidljivostipublished , da bi se omogućilo strimovanje TPersistentpotomaka. Nisu svi tipovi polja i svojstava dozvoljeni u publishedodeljku. Ako dobijete greške u vezi sa tim, a strimovanje vas ne zanima, samo promenite vidljivost na public. Pogledajte specifikatore vidljivosti .
8. Različite jezičke karakteristike
8.1. Lokalne (ugnežđene) rutine

Unutar veće rutine (funkcije, procedure, metode) možete definisati pomoćnu rutinu.

Lokalna rutina može slobodno da pristupi (čita i piše) svim parametrima roditelja i svim lokalnim promenljivim roditelja koje su deklarisane iznad nje . Ovo je veoma moćno. Često omogućava podelu dugih rutina na nekoliko manjih bez mnogo napora (jer ne morate da prosleđujete sve potrebne informacije u parametrima). Pazite da ne preterate sa ovom funkcijom — ako mnoge ugnežđene funkcije koriste (pa čak i menjaju) istu promenljivu roditelja, kod može biti težak za praćenje.

Ova dva primera su ekvivalentna:

function SumOfSquares(const N: Integer): Integer;

  function Square(const Value: Integer): Integer;
  begin
    Result := Value * Value;
  end;

var
  I: Integer;
begin
  Result := 0;
  for I := 0 to N do
    Result := Result + Square(I);
end;

Druga verzija, gde dozvoljavamo lokalnoj rutini direktan Squarepristup I:

function SumOfSquares(const N: Integer): Integer;
var
  I: Integer;

  function Square: Integer;
  begin
    Result := I * I;
  end;

begin
  Result := 0;
  for I := 0 to N do
    Result := Result + Square;
end;

Lokalne rutine mogu ići do bilo koje dubine — što znači da možete definisati lokalnu rutinu unutar druge lokalne rutine. Dakle, možete preterivati (ali molim vas, nemojte previše preterivati , jer će kod postati nečitljiv :).
8.2. Povratni pozivi (tj. događaji, tj. pokazivači na funkcije, tj. proceduralne promenljive)

Oni omogućavaju indirektno pozivanje funkcije, preko promenljive. Promenljiva se može dodeliti tokom izvršavanja da bi ukazivala na bilo koju funkciju sa odgovarajućim tipovima parametara i tipovima povratnih vrednosti .

Povratni poziv može biti:

    Normalno, što znači da može ukazivati na bilo koju normalnu rutinu (ne metodu, ne lokalnu).

    {$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
    {$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

    function Add(const A, B: Integer): Integer;
    begin
      Result := A + B;
    end;

    function Multiply(const A, B: Integer): Integer;
    begin
      Result := A * B;
    end;

    type
      TMyFunction = function (const A, B: Integer): Integer;

    function ProcessTheList(const F: TMyFunction): Integer;
    var
      I: Integer;
    begin
      Result := 1;
      for I := 2 to 10 do
        Result := F(Result, I);
    end;

    var
      SomeFunction: TMyFunction;
    begin
      SomeFunction := @Add;
      WriteLn('1 + 2 + 3 ... + 10 = ', ProcessTheList(SomeFunction));

      SomeFunction := @Multiply;
      WriteLn('1 * 2 * 3 ... * 10 = ', ProcessTheList(SomeFunction));
    end.

    Metod: deklarišite sa of objectna kraju.

    {$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
    {$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

    uses
      SysUtils;

    type
      TMyMethod = procedure (const A: Integer) of object;

      TMyClass = class
        CurrentValue: Integer;
        procedure Add(const A: Integer);
        procedure Multiply(const A: Integer);
        procedure ProcessTheList(const M: TMyMethod);
      end;

    procedure TMyClass.Add(const A: Integer);
    begin
      CurrentValue := CurrentValue + A;
    end;

    procedure TMyClass.Multiply(const A: Integer);
    begin
      CurrentValue := CurrentValue * A;
    end;

    procedure TMyClass.ProcessTheList(const M: TMyMethod);
    var
      I: Integer;
    begin
      CurrentValue := 1;
      for I := 2 to 10 do
        M(I);
    end;

    var
      C: TMyClass;
    begin
      C := TMyClass.Create;
      try
        C.ProcessTheList({$ifdef FPC}@{$endif} C.Add);
        WriteLn('1 + 2 + 3 ... + 10 = ', C.CurrentValue);

        C.ProcessTheList({$ifdef FPC}@{$endif} C.Multiply);
        WriteLn('1 * 2 * 3 ... * 10 = ', C.CurrentValue);
      finally
        FreeAndNil(C);
      end;
    end.

    Imajte na umu da ne možete proslediti globalne procedure/funkcije kao metode. One su nekompatibilne. Ako morate da obezbedite of objectpovratni poziv, ali ne želite da kreirate probnu instancu klase, možete proslediti metode klase kao metode.

    {$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
    {$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

    type
      TMyMethod = function (const A, B: Integer): Integer of object;

      TMyClass = class
        class function Add(const A, B: Integer): Integer;
        class function Multiply(const A, B: Integer): Integer;
      end;

    class function TMyClass.Add(const A, B: Integer): Integer;
    begin
      Result := A + B;
    end;

    class function TMyClass.Multiply(const A, B: Integer): Integer;
    begin
      Result := A * B;
    end;

    var
      M: TMyMethod;
    begin
      {$ifdef FPC}
      // Unfortunately, this requires a bit of hack to work in FPC ObjFpc mode.
      M := @TMyClass(nil).Add;
      M := @TMyClass(nil).Multiply;
      {$else}
      M := TMyClass.Add;
      M := TMyClass.Multiply;
      {$endif}
    end.

    (Moguće) lokalna rutina: deklarišite sa is nestedna kraju i obavezno koristite {$modeswitch nestedprocvars}direktivu za kod. Ovo ide ruku pod ruku sa lokalnim (ugnežđenim) rutinama .

8.3. Anonimne funkcije

Delphi i novije FPC verzije (>= 3.3.1) podržavaju:

    anonimne funkcije (definišite implementaciju funkcije odmah kada je dodelite promenljivoj ili je prosledite kao argument),

    i reference funkcija (novi tip "povratnog poziva funkcije" koji može da prihvati širok spektar tipova funkcija, uključujući globalne funkcije, metode i anonimne funkcije).

Primer:

{ Example of Map, ForEach methods and processing list with anonymous functions. }

{$ifdef FPC}
  {$ifdef VER3_2} {$message warn 'This code needs FPC >= 3.3.x'} begin end. {$endif}
  {$mode objfpc}{$H+}{$J-}
  {$modeswitch functionreferences}
  {$modeswitch anonymousfunctions}
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, Generics.Collections;

type
  { Note about below TIntMapFunc and TIntMapProc definition, what to use?

    In short, use "reference to". You can assign to them anonymous functions
    reliably in both Delphi and FPC.

    With Delphi 12.1, only the "reference to" version will compile.

    With FPC 3.3.1, other variants will also compile.
    You can assign anonymous function to any of them.
    So if you only target FPC, you can decide which version to use
    based on what you want to assign to them *aside*
    from anonymous functions:

    - The 1st version (without "of object", without "reference to")
      allows to store a reference to a global function,

    - The 2nd (with "of object")
      allows to store a reference to a method of an object,

    - The 3rd (with "reference to") is the most universal,
      allows a lot of things --
      see https://forum.lazarus.freepascal.org/index.php?topic=59468.0 .
  }

  TIntMapFunc =
    //function(const Index, Item: Integer): Integer;
    //function(const Index, Item: Integer): Integer of object;
    reference to function(const Index, Item: Integer): Integer;
  TIntMapProc =
    //procedure(const Index, Item: Integer);
    //procedure(const Index, Item: Integer) of object;
    reference to procedure(const Index, Item: Integer);

  TMyInts = class({$ifdef FPC}specialize{$endif} TList<Integer>)
    { Change every item in the list using AFunc. }
    procedure Map(const AFunc: TIntMapFunc);
    { Call AProc for every item in the list. }
    procedure ForEach(const AProc: TIntMapProc);
  end;

procedure TMyInts.Map(const AFunc: TIntMapFunc);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    Items[Index] := AFunc(Index, Items[Index]);
end;

procedure TMyInts.ForEach(const AProc: TIntMapProc);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    AProc(Index, Items[Index]);
end;

var
  MyList: TMyInts;
  I: Integer;
  F: TIntMapFunc;
begin
  MyList := TMyInts.Create;
  try
    for I := 0 to 10 do
      MyList.Add(I);

    F := function(const Index, Item: Integer): Integer
      begin
        Result := Item + 1;
      end;
    // effectively this increases all numbers on the list by 3
    MyList.Map(F);
    MyList.Map(F);
    MyList.Map(F);

    // change all items to their squares
    MyList.Map(function(const Index, Item: Integer): Integer
      begin
        Result := Item * Item;
      end);

    // print all items
    MyList.ForEach(procedure(const Index, Item: Integer)
      begin
        WriteLn('Index: ', Index, ', Item: ', Item);
        WriteLn('  If we would process it by F: ', F(Index, Item));
      end);
  finally FreeAndNil(MyList) end;
end.

Više informacija:

    Delfi dokumentacija: https://docwiki.embarcadero.com/RADStudio/Sydney/en/Anonymous_Methods_in_Delphi

    Objava na forumu FPC: https://forum.lazarus.freepascal.org/index.php/topic,59468.0.html

    Dnevnik promena funkcija FPC-a: https://wiki.freepascal.org/FPC_New_Features_Trunk#Support_for_Function_References_and_Anonymous_Functions

Da biste dobili FPC 3.3.1, preporučujemo da koristite FpcUpDeluxe: https://castle-engine.io/fpcupdeluxe .
8.4. Generički lekovi

Moćna karakteristika svakog modernog jezika. Definicija nečega (obično, klase) može se parametrizovati drugim tipom. Najtipičniji primer je kada treba da kreirate kontejner (listu, rečnik, drvo, graf…): možete definisati listu tipa T , a zatim je specijalizovati da biste odmah dobili listu celih brojeva , listu nizova , listu TMyRecord i tako dalje.

Generici u Paskalu rade slično genericima u C++. Što znači da se "proširuju" u vreme specijalizacije, pomalo kao makroi (ali mnogo bezbedniji od makroa; na primer, identifikatori se razrešavaju u vreme generičke definicije, a ne u specijalizaciji, tako da ne možete "ubrizgati" nikakvo neočekivano ponašanje prilikom specijalizacije generika). U stvari, to znači da su veoma brzi (mogu se optimizovati za svaki pojedinačni tip) i rade sa tipovima bilo koje veličine. Možete koristiti primitivni tip (ceo broj, broj sa pokretnim zapreminom), kao i zapis, kao i klasu prilikom specijalizacije generika.

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$ifndef FPC}
  {$message warn 'Delphi does not allow addition on types that are generic parameters'}
  begin end.
{$endif}

uses SysUtils;

type
  generic TMyCalculator<T> = class
    Value: T;
    procedure Add(const A: T);
  end;

procedure TMyCalculator.Add(const A: T);
begin
  Value := Value + A;
end;

type
  TMyFloatCalculator = {$ifdef FPC}specialize{$endif} TMyCalculator<Single>;
  TMyStringCalculator = {$ifdef FPC}specialize{$endif} TMyCalculator<string>;

var
  FloatCalc: TMyFloatCalculator;
  StringCalc: TMyStringCalculator;
begin
  FloatCalc := TMyFloatCalculator.Create;
  try
    FloatCalc.Add(3.14);
    FloatCalc.Add(1);
    WriteLn('FloatCalc: ', FloatCalc.Value:1:2);
  finally
    FreeAndNil(FloatCalc);
  end;

  StringCalc := TMyStringCalculator.Create;
  try
    StringCalc.Add('something');
    StringCalc.Add(' more');
    WriteLn('StringCalc: ', StringCalc.Value);
  finally
    FreeAndNil(StringCalc);
  end;
end.

Generici nisu ograničeni na klase, možete imati i generičke funkcije i procedure:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$ifndef FPC}
  {$message warn 'Delphi does not support global generic functions'}
  begin end.
{$endif}

uses SysUtils;

{ Note: this example requires FPC 3.1.1 (will not compile with FPC 3.0.0 or older). }

generic function Min<T>(const A, B: T): T;
begin
  if A < B then
    Result := A else
    Result := B;
end;

begin
  WriteLn('Min (1, 0): ', specialize Min<Integer>(1, 0));
  WriteLn('Min (3.14, 5): ', specialize Min<Single>(3.14, 5):1:2);
  WriteLn('Min (''a'', ''b''): ', specialize Min<string>('a', 'b'));
end.

Pogledajte takođe Kontejneri (liste, rečnici) koristeći generike o važnim standardnim klasama koje koriste generike.
8.5. Preopterećenje

Metode (i globalne funkcije i procedure) sa istim imenom su dozvoljene, sve dok imaju različite parametre. Tokom kompajliranja, kompajler detektuje koji želite da koristite, znajući parametre koje prosleđujete.

Podrazumevano, preopterećenje koristi FPC pristup, što znači da su sve metode u datom imenskom prostoru (klasi ili jedinici) jednake, a ostale metode se skrivaju u imenskim prostorima sa manjim prioritetom. Na primer, ako definišete klasu sa metodama Foo(Integer)i Foo(string), a ona je potomak klase sa metodom Foo(Float), onda korisnici vaše nove klase neće moći Foo(Float)lako da pristupe metodi (i dalje mogu --- ako pretvore tip klase u njenog pretka). Da biste ovo prevazišli, koristite overloadključnu reč .
8.6. Pretprocesor

Možete koristiti jednostavne pretprocesorske direktive za

    uslovna kompilacija (kod zavisi od platforme ili nekih prilagođenih prekidača),

    da uključi jednu datoteku u drugu,

    Takođe možete koristiti makroe bez parametara.

Imajte na umu da makroi sa parametrima nisu dozvoljeni. Generalno, trebalo bi da izbegavate korišćenje pretprocesorskih stvari... osim ako to nije zaista opravdano. Prethodna obrada se dešava pre parsiranja, što znači da možete "prekršiti" normalnu sintaksu jezika Paskal. Ovo je moćna, ali i pomalo prljava, funkcija.

unit PreprocessorStuff;

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

interface

{$ifdef FPC}
{ This is only defined when compiled by FPC, not other compilers (like Delphi). }
procedure Foo;
{$endif}

{ Define a NewLine constant. Here you can see how the normal syntax of Pascal
  is "broken" by preprocessor directives. When you compile on Unix
  (includes Linux, Android, macOS), the compiler sees this:

    const NewLine = #10;

  When you compile on Windows, the compiler sees this:

    const NewLine = #13#10;

  On other operating systems, the code will fail to compile,
  because a compiler sees this:

    const NewLine = ;

  It's a *good* thing that the compilation fails in this case -- if you
  will have to port the program to an OS that is not Unix, not Windows,
  you will be reminded by a compiler to choose the newline convention
  on that system. }

const
  NewLine =
    {$ifdef UNIX} #10 {$endif}
    {$ifdef MSWINDOWS} #13#10 {$endif} ;

{$define MY_SYMBOL}

{$ifdef MY_SYMBOL}
procedure Bar;
{$endif}

{$define CallingConventionMacro := unknown}
{$ifdef UNIX}
  {$define CallingConventionMacro := cdecl}
{$endif}
{$ifdef MSWINDOWS}
  {$define CallingConventionMacro := stdcall}
{$endif}
procedure RealProcedureName; CallingConventionMacro; external 'some_external_library';

implementation

{$include some_file.inc}
// $I is just a shortcut for $include
{$I some_other_file.inc}

end.

Uključene datoteke obično imaju .incekstenziju i koriste se u dve svrhe:

    Datoteka za uključivanje može da sadrži samo druge direktive kompajlera koje "konfigurišu" vaš izvorni kod. Na primer, možete da kreirate datoteku myconfig.incsa ovim sadržajem:

    {$ifdef FPC}
      {$mode objfpc}
      {$H+}
      {$J-}
      {$modeswitch advancedrecords}
      {$ifdef VER2}
        {$message fatal 'This code can only be compiled using FPC version >= 3.0.'}
      {$endif}
    {$endif}

    Sada možete uključiti ovu datoteku koristeći {$I myconfig.inc}u sve svoje izvore.

    Druga uobičajena upotreba je podela velike jedinice na više datoteka, a da se ona i dalje zadrži kao jedna jedinica što se tiče jezičkih pravila. Nemojte preterano koristiti ovu tehniku — vaš prvi instinkt bi trebalo da bude da jednu jedinicu podelite na više jedinica, a ne da je jednu jedinicu podelite na više uključenih datoteka. Ipak, ovo je korisna tehnika.

        Omogućava da se izbegne "eksplozija" broja jedinica, a da se pritom datoteke izvornog koda ostanu kratke. Na primer, možda je bolje imati jednu jedinicu sa "uobičajeno korišćenim kontrolama korisničkog interfejsa" nego kreirati jednu jedinicu za svaku klasu kontrola korisničkog interfejsa , jer bi drugi pristup učinio tipičnu klauzulu "upotreba" dugačkom (pošto će tipičan kod korisničkog interfejsa zavisiti od nekoliko klasa korisničkog interfejsa). Ali smeštanje svih ovih klasa korisničkog interfejsa u jednu myunit.pasdatoteku učinilo bi je dugačkom datotekom, nezgodnom za navigaciju, pa bi njeno deljenje na više uključenih datoteka moglo imati smisla.

        Omogućava laku implementaciju zavisnu od platforme, što omogućava kros-platformski interfejs jedinice. U osnovi, možete to da uradite.

        {$ifdef UNIX} {$I my_unix_implementation.inc} {$endif}
        {$ifdef MSWINDOWS} {$I my_windows_implementation.inc} {$endif}

        Ponekad je ovo bolje nego pisanje dugačkog koda sa mnogo {$ifdef UNIX}, {$ifdef MSWINDOWS}pomešanog sa normalnim kodom (deklaracije promenljivih, implementacija rutina). Kod je na ovaj način čitljiviji. Čak možete koristiti ovu tehniku agresivnije, koristeći -Fiopciju komandne linije FPC-a da biste uključili neke poddirektorijume samo za određene platforme. Tada možete imati više verzija datoteke za uključivanje {$I my_platform_specific_implementation.inc}i jednostavno ih uključiti, dozvoljavajući kompajleru da pronađe ispravnu verziju.

8.7. Zapisi

Zapis je samo kontejner za druge promenljive. To je kao mnogo, mnogo pojednostavljena klasa : nema nasleđivanja niti virtuelnih metoda. To je kao struktura u C-sličnim jezicima.

Ako koristite {$modeswitch advancedrecords}direktivu, zapisi mogu imati metode i specifikatore vidljivosti. Generalno, jezičke funkcije koje su dostupne za klase i ne narušavaju jednostavan predvidljivi raspored memorije zapisa su tada moguće.

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$modeswitch advancedrecords}
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

type
  TMyRecord = record
  public
    I, Square: Integer;
    procedure WriteLnDescription;
  end;

procedure TMyRecord.WriteLnDescription;
begin
  WriteLn('Square of ', I, ' is ', Square);
end;

var
  A: array [0..9] of TMyRecord;
  R: TMyRecord;
  I: Integer;
begin
  for I := 0 to 9 do
  begin
    A[I].I := I;
    A[I].Square := I * I;
  end;

  for R in A do
    R.WriteLnDescription;
end.

U modernom Object Paskalu, vaš prvi instinkt bi trebalo da bude da dizajnirate class, a ne record — jer su klase pune korisnih funkcija, poput konstruktora i nasleđivanja.

Ali zapisi su i dalje veoma korisni kada vam je potrebna brzina ili predvidljiv raspored memorije:

    Zapisi nemaju konstruktor ni destruktor. Samo definišete promenljivu tipa zapisa. Ona ima nedefinisan sadržaj (memorijsko smeće) na početku (osim automatski upravljanih tipova, kao što su stringovi; oni su garantovano inicijalizovani da budu prazni, a finalizovani da bi se oslobodio broj referenci). Dakle, morate biti pažljiviji kada radite sa zapisima, ali vam to daje određeno poboljšanje performansi.

    Nizovi zapisa su lepo linearni u memoriji, tako da su pogodni za keširanje.

    Raspored zapisa u memoriji (veličina, razmak između polja) je jasno definisan u nekim situacijama: kada zahtevate C raspored ili kada koristite packed record. Ovo je korisno:

        da komunicira sa bibliotekama napisanim u drugim programskim jezicima, kada one otkrivaju API zasnovan na zapisima,

        da čita i piše binarne datoteke,

        da implementiraju prljave trikove niskog nivoa (kao što je nebezbedno pretvaranje jednog tipa u drugi, svesnost njihove memorijske reprezentacije).

    Zapisi takođe mogu imati casedelove, koji funkcionišu kao unije u jezicima sličnim C-u. Oni omogućavaju da se isti deo memorije tretira kao različit tip, u zavisnosti od vaših potreba. Kao takvo, ovo omogućava veću efikasnost memorije u nekim slučajevima. I omogućava više prljavih, niskonivoskih nebezbednih trikova :)

8.8. Zapisi varijanti i povezani koncepti

Varijanta koncepta može se odnositi na 3 različite (mada, duboko povezane) stvari u Paskalu:
8.8.1. Zapisi varijanti

Varijantni zapisi omogućavaju definisanje odeljka na kraju vašeg zapisa gde se istoj memoriji može pristupiti pomoću nekoliko različitih imena/tipova.

Ovo je opisano na https://en.wikipedia.org/wiki/Tagged_union na Vikipediji. "Unija" je češći naziv za ovo u drugim jezicima. Vidite takođe https://www.freepascal.org/docs-html/ref/refsu15.html .

Primer:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

type
  TVector2 = packed record
    case Integer of
      0: (X, Y: Single);
      1: (Data: array [0..1] of Single);
  end;

  TVector3 = packed record
    case Integer of
      0: (X, Y, Z: Single);
      1: (Data: array [0..2] of Single);
      2: (XY: TVector2);
  end;


var
  V2: TVector2;
  V: TVector3;
  I: Integer;
begin
  Writeln('Size of TVector2 is ', SizeOf(TVector2));
  Writeln('  Should be equal to ', SizeOf(Single) * 2);

  Writeln('Size of TVector3 is ', SizeOf(TVector3));
  Writeln('  Should be equal to ', SizeOf(Single) * 3);

  V.X := 1;
  V.Y := 2;
  V.Z := 3;

  for I := 0 to 2 do
    Writeln('V.Data[', I, '] is ', V.Data[I]:1:2);

  V2 := V.XY;

  for I := 0 to 1 do
    Writeln('V2.Data[', I, '] is ', V2.Data[I]:1:2);
end.

8.8.2. Tip varijante

Variantje poseban tip u Paskalu koji može da sadrži vrednosti različitih tipova. Štaviše, operatori su definisani tako da omogućavaju operacije nad njima i konvertovanje njihovih vrednosti tokom izvršavanja.

Efekat je pomalo sličan pisanju skriptnih programskih jezika sa dinamičkim kucanjem.

Nemojte ih koristiti bez razmatranja: stvari su malo manje bezbedne (ne kontrolišete tipove, konverzije se dešavaju implicitno). Takođe, postoji mali pad performansi, jer sve operacije moraju da proveravaju i sinhronizuju tipove tokom izvršavanja.

Ali ponekad ima smisla. Naime, kada morate da obrađujete podatke koji suštinski mogu imati različite tipove, a vi te tipove znate samo u vreme izvršavanja. Npr. kada želite da obradite rezultat SQL-a select * from some_tableu generičkom pregledaču baze podataka (ne znajući strukturu tabele u vreme kompajliranja).

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

uses Variants;
var
  V1, V2, V3: Variant;
begin
  V1 := 'My String';
  V1 := 123; // V1 no longer holds String, it has Integer now
  V2 := 456.789;
  V3 := V1 + V2; // result is float
  Writeln('V3 = ', V3);
end.

Napomena
	Tehnički, Variantto se realizuje korišćenjem TVarDatainternog tipa, koji je zapis sa varijantama. Dakle, ovi koncepti su povezani. Ali ne bi trebalo da znate ovo , ne bi trebalo da ga koristite TVarDataeksplicitno.
8.8.3. TVarRec u nizu konstanti

Kada koristite array of constposeban tip parametra, on se prosleđuje kao niz od TVarRec. Vidite

    TVarRecu FPC-u: https://www.freepascal.org/docs-html/rtl/system/tvarrec.html

    TVarRecu Delfiju: https://docwiki.embarcadero.com/Libraries/Sydney/en/System.TVarRec

Ovo je korisno za prosleđivanje rutini parametara proizvoljnih (nepoznatih u vreme kompajliranja) tipova. Na primer, za implementaciju rutina kao što su standardne Format(slično kao sprintfu C-u) ili Castle Game Game WriteLnLog / WriteLnWarning.

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils;

{ Example function that concatenates all elements of an array of const
  into a String. }
function GlueEverything(const MyArray: array of const): String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to High(MyArray) do
  begin
    // treat MyArray[I] as TVarRec, check for type and do something
    case MyArray[I].VType of
      vtInteger:
        begin
          Writeln('Integer: ', MyArray[I].VInteger);
          Result := Result + IntToStr(MyArray[I].VInteger) + ' ';
        end;
      vtAnsiString:
        begin
          Writeln('Ansi String (8-bit chars): ', AnsiString(MyArray[I].VAnsiString));
          Result := Result + AnsiString(MyArray[I].VAnsiString) + ' ';
        end;
      vtUnicodeString:
        begin
          Writeln('Unicode String (16-bit chars): ', UnicodeString(MyArray[I].VUnicodeString));
          Result := Result + UnicodeString(MyArray[I].VUnicodeString) + ' ';
        end;
      else
        Writeln('Something else, ignoring');
    end;
  end;
end;

var
  S: String;
begin
  S := GlueEverything([123, 'Hello', 'World', 456]);
  Writeln(S);
end.

8.9. Predmeti starog stila

U stara vremena, Turbo Paskal je uveo drugu sintaksu za funkcionalnost sličnu klasi, koristeći objectključnu reč . To je donekle mešavina koncepta `a` recordi modernog ` class.

    Objekti starog stila mogu biti dodeljeni / oslobođeni, i tokom te operacije možete pozvati njihov konstruktor / destruktor.

    Ali oni se takođe mogu jednostavno deklarisati i koristiti, kao zapisi. Jednostavan tip record`or` objectnije referenca (pokazivač) na nešto, već jednostavno podaci. Zbog toga su pogodni za male podatke, gde bi pozivanje `allocation / free` bilo mučno.

    Objekti starog stila nude nasleđivanje i virtuelne metode, mada sa malim razlikama u odnosu na moderne klase. Budite oprezni —  desiće se loše stvari ako pokušate da koristite objekat bez pozivanja njegovog konstruktora, a objekat ima virtuelne metode.

U većini slučajeva se ne preporučuje korišćenje objekata starog stila. Moderne klase pružaju mnogo više funkcionalnosti. A kada je potrebno, zapisi (uključujući i napredne zapise ) mogu se koristiti za poboljšanje performansi. Ovi koncepti su obično bolja ideja od objekata starog stila.
8.10. Pokazivači

Možete kreirati pokazivač na bilo koji drugi tip. Pokazivač na tip TMyRecordje deklarisan kao ^TMyRecord, i po konvenciji se naziva PMyRecord. Ovo je tradicionalni primer povezane liste celih brojeva korišćenjem zapisa:

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    Value: Integer;
    Next: PMyRecord;
  end;

Imajte na umu da je definicija rekurzivna (tip PMyRecordje definisan pomoću tipa TMyRecord, dok TMyRecordje definisan pomoću PMyRecord). Dozvoljeno je definisati pokazivački tip na tip koji još nije definisan , sve dok će biti razrešen unutar istog typebloka.

Možete dodeliti i oslobađati pokazivače koristeći New/ Disposemetode ili (nižeg nivoa, koje nisu bezbedne za tip) GetMem/ FreeMemmetode. Dereferencirate pokazivač (da biste pristupili stvarima na koje pokazuje ) dodajete ^operator (npr MyInteger := MyPointerToInteger^.). Da biste izvršili inverznu operaciju, a to je da biste dobili pokazivač postojeće promenljive , dodajete joj prefiks @operatora (npr MyPointerToInteger := @MyInteger.).

Postoji i netipizovani Pointertip, sličan onom void*u jezicima sličnim C-u. On je potpuno nebezbedan i može se tipovizovati u bilo koji drugi tip pokazivača.

Zapamtite da je instanca klase zapravo i pokazivač, iako joj nisu potrebni nikakvi operatori ` ^` ili `` @da bi se koristila. Povezana lista koja koristi klase je svakako moguća, jednostavno bi izgledala ovako:

type
  TMyClass = class
    Value: Integer;
    Next: TMyClass;
  end;

8.11. Preopterećenje operatora

Možete poništiti značenje mnogih jezičkih operatora, na primer da biste omogućili sabiranje i množenje vaših prilagođenih tipova.

I FPC i Delphi podržavaju preopterećenje operatora definisanjem class operatormetoda unutar naprednih zapisa. Ovako:

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$modeswitch advancedrecords}
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils;

type
  TVector3 = record
  public
    X, Y, Z: Single;
    class operator {$ifdef FPC}+{$else}Add{$endif}
      (const A, B: TVector3): TVector3;
    class operator {$ifdef FPC}*{$else}Multiply{$endif}
      (const V: TVector3; const Scalar: Single): TVector3;
    function ToString: String;
  end;

class operator TVector3.{$ifdef FPC}+{$else}Add{$endif}
  (const A, B: TVector3): TVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVector3.{$ifdef FPC}*{$else}Multiply{$endif}
  (const V: TVector3; const Scalar: Single): TVector3;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
  Result.Z := V.Z * Scalar;
end;

function TVector3.ToString: String;
begin
  Result := Format('(%f, %f, %f)', [X, Y, Z]);
end;

var
  V1, V2: TVector3;
begin
  V1.X := 1.0; V1.Y := 2.0;  V1.Z := 3.0;
  V2.X := 4.0; V2.Y := 5.0;  V2.Z := 6.0;
  WriteLn('V1: ', V1.ToString);
  WriteLn('V2: ', V2.ToString);
  WriteLn('V1 + V2: ', (V1 + V2).ToString);
  WriteLn('V1 * 10: ', (V1 * 10).ToString);
end.

Napomena
	Sa FPC-om, obavezno obavestite kompajler da koristite funkciju "naprednih zapisa" pomoću {$modeswitch advancedrecords}.

Pogledajte dokumentaciju da biste saznali sve moguće operatore koji mogu biti preopterećeni:

    Preopterećenje FPC operatora

    Preopterećenje operatora u Delfiju

FPC takođe podržava alternativnu sintaksu za operatore preopterećenja, definisanjem globalne funkcije kao što je operator*. Na primer:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$ifndef FPC}
  {$message warn 'Delphi does not support global operator overloading'}
  begin end.
{$endif}

uses
  StrUtils;

operator* (const S: string; const A: Integer): string;
begin
  Result := DupeString(S, A);
end;

begin
  WriteLn('bla' * 10);
end.

Ovaj pristup (globalne operatorfunkcije) može se koristiti i za definisanje operatora na klasama. Pošto obično kreirate nove instance svojih klasa unutar operatorske funkcije, pozivalac mora da zapamti da oslobodi rezultat.

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$ifndef FPC}
  {$message warn 'Delphi does not support global operator overloading'}
  begin end.
{$endif}

uses
  SysUtils;

type
  TMyClass = class
    MyInt: Integer;
  end;

operator* (const C1, C2: TMyClass): TMyClass;
begin
  Result := TMyClass.Create;
  Result.MyInt := C1.MyInt * C2.MyInt;
end;

var
  C1, C2: TMyClass;
begin
  C1 := TMyClass.Create;
  try
    C1.MyInt := 12;
    C2 := C1 * C1;
    try
      WriteLn('12 * 12 = ', C2.MyInt);
    finally
      FreeAndNil(C2);
    end;
  finally
    FreeAndNil(C1);
  end;
end.

Možete zameniti operatore na zapisima i koristeći globalne operatorfunkcije. Ovo je obično lakše nego preopterećivati ih za klase, jer pozivalac tada ne mora da se bavi upravljanjem memorijom.

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$ifndef FPC}
  {$message warn 'Delphi does not support global operator overloading'}
  begin end.
{$endif}

uses SysUtils;

type
  TMyRecord = record
    MyInt: Integer;
  end;

operator* (const C1, C2: TMyRecord): TMyRecord;
begin
  Result.MyInt := C1.MyInt * C2.MyInt;
end;

var
  R1, R2: TMyRecord;
begin
  R1.MyInt := 12;
  R2 := R1 * R1;
  WriteLn('12 * 12 = ', R2.MyInt);
end.

Međutim, za zapise, ne savetujemo korišćenje globalnih operatorfunkcija. Umesto toga, koristite {$modeswitch advancedrecords}i zamenite operatore kao class operatorunutar zapisa. Razlozi:

    Ovo je kompatibilno sa Delfijem.

    Ovo omogućava korišćenje generičkih klasa koje zavise od postojanja nekog operatora (kao što je TFPGList, što zavisi od dostupnosti operatora jednakosti) sa takvim zapisima. U suprotnom, "globalna" definicija operatora (koja nije unutar zapisa) ne bi bila pronađena (jer nije dostupna u kodu koji implementira TFPGList), i ne biste mogli specijalizovati listu kao što je specialize TFPGList<TMyRecord>.

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$modeswitch advancedrecords}
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

{$ifndef FPC}
  {$message warn 'Delphi does not have FGL unit'}
  begin end.
{$endif}

uses
  SysUtils, FGL;

type
  TMyRecord = record
    MyInt: Integer;
    class operator+ (const C1, C2: TMyRecord): TMyRecord;
    class operator= (const C1, C2: TMyRecord): boolean;
  end;

class operator TMyRecord.+ (const C1, C2: TMyRecord): TMyRecord;
begin
  Result.MyInt := C1.MyInt + C2.MyInt;
end;

class operator TMyRecord.= (const C1, C2: TMyRecord): boolean;
begin
  Result := C1.MyInt = C2.MyInt;
end;

type
  TMyRecordList = {$ifdef FPC}specialize{$endif} TFPGList<TMyRecord>;

var
  R, ListItem: TMyRecord;
  L: TMyRecordList;
begin
  L := TMyRecordList.Create;
  try
    R.MyInt := 1;   L.Add(R);
    R.MyInt := 10;  L.Add(R);
    R.MyInt := 100; L.Add(R);

    R.MyInt := 0;
    for ListItem in L do
      R := ListItem + R;

    WriteLn('1 + 10 + 100 = ', R.MyInt);
  finally
    FreeAndNil(L);
  end;
end.

9. Karakteristike naprednih časova
9.1. Privatno i strogo privatno

Specifikator privatevidljivosti znači da polje (ili metod) nije dostupno izvan ove klase. Ali dozvoljava izuzetak: sav kod definisan u istoj jedinici može da prekine ovo i da pristupi privatnim poljima i metodama. C++ programer bi rekao da su u Paskalu sve klase unutar jedne jedinice prijateljske . Ovo je često korisno i ne prekida vašu enkapsulaciju, jer je ograničeno na jedinicu.

Međutim, ako kreirate veće jedinice, sa mnogo klasa (koje nisu čvrsto integrisane jedna sa drugom), bezbednije je koristiti strict private. To znači da polje (ili metod) nije dostupno van ove klase — tačka. Nema izuzetaka.

Na sličan način, postoji protectedvidljivost (vidljivo potomcima ili prijateljima u istoj jedinici) i strict protected(vidljivo potomcima, tačka).
9.2. Više stvari unutar klasa i ugnežđenih klasa

Možete otvoriti odeljak konstanti ( const) ili tipova ( type) unutar klase. Na ovaj način možete čak definisati klasu unutar klase. Specifikatori vidljivosti rade kao i uvek, posebno ugnežđena klasa može biti privatna (nevidljiva spoljašnjem svetu), što je često korisno.

Imajte na umu da da biste deklarisali polje nakon konstante ili tipa, potrebno je da otvorite varblok.

type
  TMyClass = class
  private
    type
      TInternalClass = class
        Velocity: Single;
        procedure DoSomething;
      end;
    var
      FInternalClass: TInternalClass;
  public
    const
      DefaultVelocity = 100.0;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMyClass.Create;
begin
  inherited;
  FInternalClass := TInternalClass.Create;
  FInternalClass.Velocity := DefaultVelocity;
  FInternalClass.DoSomething;
end;

destructor TMyClass.Destroy;
begin
  FreeAndNil(FInternalClass);
  inherited;
end;

{ note that method definition is prefixed with
  "TMyClass.TInternalClass" below. }
procedure TMyClass.TInternalClass.DoSomething;
begin
end;

9.3. Metode klase

To su metode koje možete pozvati imajući referencu klase ( TMyClass), ne nužno instancu klase.

type
  TEnemy = class
    procedure Kill;
    class procedure KillAll;
  end;

var
  E: TEnemy;
begin
  E := TEnemy.Create;
  try
    E.Kill;
  finally FreeAndNil(E) end;
  TEnemy.KillAll;
end;

Imajte na umu da mogu biti virtuelni — to ima smisla, a ponekad je i veoma korisno, kada se kombinuje sa referencama klasa .

Metode klase takođe mogu biti ograničene specifikatorima vidljivosti , kao što su privateili protected. Baš kao i obične metode.

Imajte na umu da se konstruktor uvek ponaša kao metoda klase kada se poziva na normalan način ( MyInstance := TMyClass.Create(…​);). Iako je moguće pozvati konstruktor i iz same klase, kao normalnu metodu, i tada se on ponaša kao normalna metoda. Ovo je korisna funkcija za "lančanje" konstruktora, kada jedan konstruktor (npr. preopterećen da primi ceo broj parametara) obavi neki posao, a zatim poziva drugi konstruktor (npr. bez parametara).
9.4. Reference klasa

Referenca klase vam omogućava da izaberete klasu tokom izvršavanja, na primer, da pozovete metodu ili konstruktor klase bez poznavanja tačne klase tokom kompajliranja. To je tip deklarisan kao class of TMyClass.

type
  TMyClass = class(TComponent)
  end;

  TMyClass1 = class(TMyClass)
  end;

  TMyClass2 = class(TMyClass)
  end;

  TMyClassRef = class of TMyClass;

var
  C: TMyClass;
  ClassRef: TMyClassRef;
begin
  // Obviously you can do this:

  C := TMyClass.Create(nil); FreeAndNil(C);
  C := TMyClass1.Create(nil); FreeAndNil(C);
  C := TMyClass2.Create(nil); FreeAndNil(C);

  // In addition, using class references, you can also do this:

  ClassRef := TMyClass;
  C := ClassRef.Create(nil); FreeAndNil(C);

  ClassRef := TMyClass1;
  C := ClassRef.Create(nil); FreeAndNil(C);

  ClassRef := TMyClass2;
  C := ClassRef.Create(nil); FreeAndNil(C);
end;

Reference klasa mogu se kombinovati sa metodama virtuelnih klasa. Ovo daje sličan efekat kao korišćenje klasa sa virtuelnim metodama — stvarna metoda koja će se izvršiti određuje se tokom izvršavanja programa.

type
  TMyClass = class(TComponent)
    class procedure DoSomething; virtual; abstract;
  end;

  TMyClass1 = class(TMyClass)
    class procedure DoSomething; override;
  end;

  TMyClass2 = class(TMyClass)
    class procedure DoSomething; override;
  end;

  TMyClassRef = class of TMyClass;

var
  C: TMyClass;
  ClassRef: TMyClassRef;
begin
  ClassRef := TMyClass1;
  ClassRef.DoSomething;

  ClassRef := TMyClass2;
  ClassRef.DoSomething;

  { And this will cause an exception at runtime,
    since DoSomething is abstract in TMyClass. }
  ClassRef := TMyClass;
  ClassRef.DoSomething;
end;

Ako imate instancu i želite da dobijete referencu na njenu klasu (ne deklarisanu klasu, već na konačnu klasu potomka koja se koristi pri njenoj konstrukciji), možete koristiti svojstvo ClassType. Deklarisani tip ClassTypeje TClass, što je skraćenica od class of TObject. Često ga možete bezbedno pretvoriti u nešto specifičnije, kada znate da je instanca nešto specifičnije od TObject.

Konkretno, možete koristiti referencu ClassTypeza pozivanje virtuelnih metoda, uključujući virtuelne konstruktore. Ovo vam omogućava da kreirate metodu poput Cloneone koja konstruiše instancu tačne klase izvršavanja trenutnog objekta . Možete je kombinovati sa Cloning: TPersistent.Assign da biste imali metodu koja vraća novokonstruisani klon trenutne instance.

Zapamtite da radi samo kada je konstruktor vaše klase virtuelan. Na primer, može se koristiti sa standardnim TComponentpotomcima, jer svi oni moraju da nadjačaju TComponent.Create(AOwner: TComponent)virtuelni konstruktor.

type
  TMyClass = class(TComponent)
    procedure Assign(Source: TPersistent); override;
    function Clone(AOwner: TComponent): TMyClass;
  end;

  TMyClassRef = class of TMyClass;

function TMyClass.Clone(AOwner: TComponent): TMyClass;
begin
  // This would always create an instance of exactly TMyClass:
  //Result := TMyClass.Create(AOwner);
  // This can potentially create an instance of TMyClass descendant:
  Result := TMyClassRef(ClassType).Create(AOwner);
  Result.Assign(Self);
end;

9.5. Statičke metode klase

Da biste razumeli statičke metode klase , morate razumeti kako funkcionišu normalne metode klase (opisane u prethodnim odeljcima). Interno, normalne metode klase dobijaju referencu klase svoje klase (ona se prosleđuje kroz skriveni, implicitno dodatni prvi parametar metode). Ovoj referenci klase se može pristupiti čak i eksplicitno korišćenjem Selfključne reči unutar metode klase. Obično je to dobra stvar: ova referenca klase vam omogućava da pozovete virtuelne metode klase (kroz tabelu virtuelnih metoda klase).

Iako je ovo lepo, čini normalne metode klase nekompatibilnim prilikom dodeljivanja globalnom pokazivaču procedure . To jest, ovo se neće kompajlirati :

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}

type
  TMyCallback = procedure (A: Integer);

  TMyClass = class
    class procedure Foo(A: Integer);
  end;

class procedure TMyClass.Foo(A: Integer);
begin
end;

var
  Callback: TMyCallback;
begin
  // Error: TMyClass.Foo not compatible with TMyCallback
  Callback := {$ifdef FPC} @TMyClass(nil).Foo {$else} TMyClass.Foo {$endif};
end.

Napomena
	

U Delfi režimu biste mogli da pišete TMyClass.Fooumesto ružnog TMyClass(nil).Fookao u gornjem primeru. Istina, TMyClass.Fooizgleda mnogo elegantnije, a i bolje ga proverava kompajler. Korišćenje TMyClass(nil).Fooje trik… nažalost, neophodan (za sada) u ObjFpc režimu koji je predstavljen u ovoj knjizi.

U svakom slučaju, dodeljivanje TMyClass.Foogore navedenom Callbackbi i dalje ne uspelo u Delfi režimu, iz potpuno istih razloga.

Gore navedeni primer se ne kompajlira, jer Callbackje nekompatibilan sa metodom klase Foo. A nekompatibilan je zato što interno metod klase ima taj poseban skriveni implicitni parametar za prosleđivanje reference klase.

Jedan od načina da se popravi gornji primer jeste da se promeni definicija metode TMyCallback. Radiće ako je u pitanju povratni poziv metode, deklarisan kao TMyCallback = procedure (A: Integer) of object;. Ali ponekad to nije poželjno.

Evo dolazi staticmetoda klase. U suštini, to je samo globalna procedura/funkcija, ali je njen imenski prostor ograničen unutar klase. Nema nikakvu implicitnu referencu klase (i stoga, ne može biti virtuelna i ne može pozivati metode virtuelne klase ). Sa pozitivne strane, kompatibilna je sa normalnim (neobjektnim) povratnim pozivima. Dakle, ovo će raditi:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

type
  TMyCallback = procedure (A: Integer);

  TMyClass = class
    class procedure Foo(A: Integer); static;
  end;

class procedure TMyClass.Foo(A: Integer);
begin
end;

var
  Callback: TMyCallback;
begin
  Callback := @TMyClass.Foo;
end.

9.6. Svojstva i promenljive klase

Svojstvo klase je svojstvo kojem se može pristupiti preko reference klase (ne zahteva instancu klase).

Slično je običnom svojstvu (videti Svojstva ), ali sve klase pristupaju (čitaju i pišu) istoj vrednosti. Za svojstvo klase možete definisati metod za dobijanje i/ili postavljanje . Oni mogu da se odnose na promenljivu klase ili statičku metodu klase .

Promenljiva klase je , pogodili ste, kao obično polje, ali vam nije potrebna instanca klase da biste joj pristupili. U stvari, to je baš kao globalna promenljiva, ali sa imenskim prostorom ograničenim na klasu koja je sadrži. Može se deklarisati unutar sekcije class varklase. Alternativno, može se deklarisati tako što se nakon definicije normalnog polja doda ključna reč static.

A statička metoda klase je baš kao globalna procedura/funkcija, ali sa imenskim prostorom ograničenim na klasu koja je sadrži. Više o statičkim metodama klase u gornjem odeljku, pogledajte Statičke metode klase .

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

type
  TMyClass = class
  strict private
    // Alternative:
    // FMyProperty: Integer; static;
    class var
      FMyProperty: Integer;
    class procedure SetMyProperty(const Value: Integer); static;
  public
    class property MyProperty: Integer
      read FMyProperty write SetMyProperty;
  end;

class procedure TMyClass.SetMyProperty(const Value: Integer);
begin
  Writeln('MyProperty changes!');
  FMyProperty := Value;
end;

begin
  TMyClass.MyProperty := 123;
  Writeln('TMyClass.MyProperty is now ', TMyClass.MyProperty);
end.

9.7. Pomoćnici u razredu

Metoda je samo procedura ili funkcija unutar klase. Spolja iz klase, pozivate je posebnom sintaksom MyInstance.MyMethod(…​). Posle nekog vremena se naviknete na razmišljanje da ako želim da izvršim akciju Action na instanci X, pišem `X.Action(…​)` .

Ali ponekad je potrebno da implementirate nešto što je konceptualno akcija na klasi TMyClass bez modifikacije izvornog koda TMyClass . Ponekad je to zato što to nije vaš izvorni kod i ne želite da ga menjate. Ponekad je to zbog zavisnosti — dodavanje metode Renderklasi poput TMy3DObjectdeluje kao jednostavna ideja, ali možda bi osnovna implementacija klase TMy3DObjecttrebalo da bude nezavisna od koda za renderovanje? Bilo bi bolje "poboljšati" postojeću klasu, dodati joj funkcionalnost bez promene njenog izvornog koda.

Jednostavan način da se to uradi je da se kreira globalna procedura koja uzima instancu TMy3DObjectkao svoj prvi parametar.

procedure Render(const Obj1: TMy3DObject; const Color: TColor);
var
  I: Integer;
begin
  for I := 0 to Obj1.ShapesCount - 1 do
    RenderMesh(Obj1.Shape[I].Mesh, Color);
end;

Ovo funkcioniše savršeno, ali mana je što pozivanje izgleda malo ružno. Dok obično pozivate akcije kao X.Action(…​), u ovom slučaju morate da ih pozovete kao Render(X, …​). Bilo bi sjajno kada bismo mogli samo da napišemo X.Render(…​), čak i kada Rendernije implementirano u istoj jedinici kao TMy3DObject.

I tu koristite pomoćne metode klase. Oni su samo način za implementaciju procedura/funkcija koje rade na datoj klasi i koje se nazivaju sličnim metodama, ali zapravo nisu normalne metode — dodate su van definicije TMy3DObject.

type
  TMy3DObjectHelper = class helper for TMy3DObject
    procedure Render(const Color: TColor);
  end;

procedure TMy3DObjectHelper.Render(const Color: TColor);
var
  I: Integer;
begin
  { note that we access ShapesCount, Shape without any qualifiers here }
  for I := 0 to ShapesCount - 1 do
    RenderMesh(Shape[I].Mesh, Color);
end;

Napomena
	Opštiji koncept je "pomoćnik tipa" . Koristeći ih, možete dodati metode čak i primitivnim tipovima, kao što su celi brojevi ili nabrajanja. Takođe možete dodati "pomoćnike zapisa " (pogodili ste…) zapisima. Vidite http://lists.freepascal.org/fpc-announce/2013-February/000587.html .
9.8. Da li konstruktori i destruktori treba da budu virtuelni?

Odgovor je zapravo različit za konstruktore i destruktore, a takođe zavisi i od toga da li koristite TObjectili TComponentkao klasu pretka.
9.8.1. Destruktori

U klasi postoji samo jedan destruktor . Njegovo ime je uvek Destroy, virtuelan je (jer ga možete pozvati bez poznavanja tačne klase tokom kompajliranja) i bez parametara. Definišite ga ovako:

type
  TMyClass = class(TObject)
  public
    destructor Destroy; override;
  end;

destructor TMyClass.Destroy;
begin
  // Cleanup code here
  inherited; // Call the ancestor destructor
end;

Iako je teoretski moguće odstupiti od ovog pristupa i definisati dodatne destruktore sa različitim imenima, mi to ne preporučujemo. Jedan destruktor bi trebalo da bude sposoban da očisti instancu (bez obzira na to kako je kreirana), ovo je očigledno programerima i na ovaj način se Freemože FreeAndNilpouzdano koristiti (oba će indirektno pozivati Destroymetodu).
9.8.2. Konstruktori

Ime konstruktora je po konvenciji Create.

U osnovnoj TObjectklasi postoji jedan jednostavan konstruktor, bez ikakvih parametara, nazvan Create. Nije virtuelan. Prilikom kreiranja potomaka, slobodni ste da definišete sopstveni konstruktor(e) sa bilo kojim parametrima koje želite. Novi konstruktor će sakriti konstruktor u pretku (mada pogledajte dole neka upozorenja o ovom "skrivanju"). Ako nemate dobar razlog da konstruktor učinite virtuelnim u potomcima, nemojte to raditi, nije neophodno.

Na primer, možete definisati konstruktor ovako:

type
  TMyClass = class(TObject)
  public
    constructor Create(AValue: Integer; const AName: String);
  end;

constructor TMyClass.Create(AValue: Integer; const AName: String);
begin
  inherited Create; // Call the ancestor constructor
  // Initialization code here
end;

Više konstruktora može biti definisano, korišćenjem preopterećenja (više konstruktora sa istim imenom, ali sa različitim parametrima) i/ili jednostavnim izmišljanjem novog imena za konstruktor, kao što je TMyClass.CreateFromJson(const JsonFileName: String). Korisna je konvencija da sva imena konstruktora počinju sa Create.

Prilikom kreiranja klase, poziva se ispravan konstruktor, pošto eksplicitno navodite ime konstruktora i parametre prilikom kreiranja instance, ovako:

X1 := TMyClass.CreateFromJson('myfile.json');
X2 := TMyClass.Create(10, 'hello');

Upozorenje
	Ako definišete konstruktor sa drugačijim imenom, kao što je CreateFromJson, obično želite da definišete i konstruktor sa standardnim imenom Createu istoj klasi. U suprotnom, korisnik i dalje može da pristupi konstruktoru pretka Create, čime kreira instancu vaše klase bez korišćenja vašeg prilagođenog konstruktora. Ovo obično nije ono što je programer nameravao.
Upozorenje
	Iz sličnog razloga, budite oprezni kada koristite overload(Delfi način preopterećenja, koji ne skriva u potpunosti konstruktor pretka). Ako niste redefinisali sve konstruktore pretka, korisnik i dalje može da koristi konstruktor pretka (dakle, kreirajte instancu vaše nove klase bez pozivanja bilo kog od vaših konstruktora).

Standardna TComponentklasa ima virtuelni konstruktor deklarisan ovako: constructor Create(AOwner: TComponent); virtual;. Korišćenje virtuelnog konstruktora je neophodno za funkcionalnost strimovanja klase TComponent, da bi se kreirala klasa bez poznavanja njenog tipa u vreme kompajliranja (pogledajte Reference klasa za objašnjenje). U TComponentpotomcima, trebalo bi da ga prepišete.

Na primer, ovako možete definisati konstruktor u TComponentpotomku. Imajte na umu da u ovom slučaju ne možemo uzimati dodatne parametre (npr. uzimati početnu vrednost za svojstvo MyInt), jer moramo zadržati isti potpis kao u klasi pretka. Svako prilagođavanje će morati da izvrši programer postavljanjem svojstava ili pozivanjem drugih metoda nakon što je instanca kreirana.

type
  TMyComponent = class(TComponent)
  private
    FMyInt: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    property MyInt: Integer read FMyInt write FMyInt;
  end;

constructor TMyComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // Call the ancestor constructor
  FMyInt := 0; // Default value
end;

Neke druge klase, takođe u vašem kodu, mogu imati virtuelne konstruktore (kako bi se omogućilo njihovo strimovanje ili samo njihovo kreiranje pomoću referenci klasa). U takvim slučajevima, konstruktor u potomcima bi generalno trebalo da koristi iste parametre i da bude deklarisan sa override(osim ako zaista ne znate šta se dešava, tj. razumete zašto je konstruktor pretka bio virtuelan i taj razlog se ne odnosi na vašu potomačku klasu).
9.9. Izuzetak u konstruktoru

Šta se dešava ako se izuzetak desi tokom konstruktora? Linija

X := TMyClass.Create;

ne završava izvršavanje u ovom slučaju, promenljiva Xnije dodeljena, pa ko će čistiti posle delimično konstruisane klase?

Rešenje Objekat Paskala je da, ako se izuzetak dogodi unutar konstruktora, onda se poziva destruktor. To je razlog zašto vaš destruktor mora biti robustan , što znači da bi trebalo da radi u svim okolnostima, čak i na polukreiranoj instanci klase. Obično je to lako ako sve bezbedno otpustite, kao što je slučaj sa FreeAndNil.

Korisna osobina koju možemo koristiti za pisanje robusnih destruktora (koji mogu da obrade polukreirane instance) jeste da je memorija klase garantovano nulta neposredno pre nego što se izvrši konstruktorski kod . Dakle, znamo da su na početku sve reference klase nil, svi celi brojevi 0i tako dalje. Strategija za pisanje robusnog destruktora je sledeća: "budite spremni da bilo koje polje i dalje može biti nula i obradite ga bez grešaka" .

U stvari, kod ispod radi bez ikakvih curenja memorije, čak i ako je izvršavanje konstruktora prekinuto, ostavljajući samo, Gun1ali ne i Gun2kreirano:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils;

type
  TGun = class
  end;

  TPlayer = class
    Gun1, Gun2: TGun;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPlayer.Create;
begin
  inherited;
  Gun1 := TGun.Create;
  raise Exception.Create('Raising an exception from constructor!');
  Gun2 := TGun.Create;
end;

destructor TPlayer.Destroy;
begin
  { in case since the constructor crashed, we can
    have Gun1 <> nil and Gun2 = nil now. Deal with it.
    ...Actually, in this case, FreeAndNil deals with it without
    any additional effort on our side, because FreeAndNil checks
    whether the instance is nil before calling its destructor. }
  FreeAndNil(Gun1);
  FreeAndNil(Gun2);
  inherited;
end;

begin
  try
    TPlayer.Create;
  except
    on E: Exception do
      WriteLn('Caught ' + E.ClassName + ': ' + E.Message);
  end;
end.

10. Interfejsi

Interfejs deklariše API, slično kao klasa, ali ne definiše implementaciju. Klasa može implementirati mnogo interfejsa, ali može imati samo jednu klasu pretka. Po konvenciji, imena tipova interfejsa počinjemo slovom I, kao što je IMyInterface.

Možete pretvoriti klasu u bilo koji interfejs koji ona implementira, a zatim pozivati metode kroz taj interfejs . Ovo omogućava jednoobrazno tretiranje klasa koje ne potiču jedna od druge, ali ipak dele neke zajedničke funkcionalnosti. Korisno kada jednostavno nasleđivanje klasa nije dovoljno.

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$interfaces corba} // See below why we recommend CORBA interfaces
{$else}
  {$message warn 'Delphi does not support CORBA interfaces, only COM, that change how memory is managed. This example is not valid in Delphi.'}
  begin end.
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils, Classes;

type
  IMyInterface = interface
  ['{79352612-668B-4E8C-910A-26975E103CAC}']
    procedure Shoot;
  end;

  TMyClass1 = class(IMyInterface)
    procedure Shoot;
  end;

  TMyClass2 = class(IMyInterface)
    procedure Shoot;
  end;

  TMyClass3 = class
    procedure Shoot;
  end;

procedure TMyClass1.Shoot;
begin
  WriteLn('TMyClass1.Shoot');
end;

procedure TMyClass2.Shoot;
begin
  WriteLn('TMyClass2.Shoot');
end;

procedure TMyClass3.Shoot;
begin
  WriteLn('TMyClass3.Shoot');
end;

procedure UseThroughInterface(I: IMyInterface);
begin
  Write('Shooting... ');
  I.Shoot;
end;

var
  C1: TMyClass1;
  C2: TMyClass2;
  C3: TMyClass3;
begin
  C1 := TMyClass1.Create;
  C2 := TMyClass2.Create;
  C3 := TMyClass3.Create;
  try
    if C1 is IMyInterface then
      UseThroughInterface(C1 as IMyInterface);
    if C2 is IMyInterface then
      UseThroughInterface(C2 as IMyInterface);
    // The "C3 is IMyInterface" below is false,
    // so "UseThroughInterface(C3 as IMyInterface)" will not execute.
    if C3 is IMyInterface then
      UseThroughInterface(C3 as IMyInterface);
  finally
    FreeAndNil(C1);
    FreeAndNil(C2);
    FreeAndNil(C3);
  end;
end.

10.1. GUID-ovi interfejsa

GUID-ovi su naizgled nasumični znakovi ['{ABCD1234-…​}']koje vidite postavljene u svakoj definiciji interfejsa. Da, oni su samo nasumični. Nažalost, neophodni su.

GUID-ovi nemaju značenje ako ne planirate integraciju sa komunikacionim tehnologijama kao što je COM . Ali su neophodni, iz razloga implementacije. Nemojte da vas zavara kompajler, koji vam nažalost dozvoljava da deklarišete interfejse bez GUID-ova. Bez (jedinstvenih) GUID-ova, vaši interfejsi će biti tretirani jednako od strane operatora is. U stvari, vratiće se trueako vaša klasa podržava bilo koji od vaših interfejsa. Magična funkcija Supports(ObjectInstance, IMyInterface)se ovde ponaša malo bolje, jer odbija da se kompajlira za interfejse bez GUID-a.

Da biste olakšali umetanje GUID-ova, možete koristiti Lazarus GUID generator ( Ctrl + Shift + Gprečica u editoru). Alternativno, možete koristiti uuidgenprogram na Unix-u ili koristiti onlajn servis kao što je https://www.guidgenerator.com/ . Ili možete napisati sopstveni alat za ovo, koristeći funkcije CreateGUIDi GUIDToStringu RTL-u. Pogledajte primer ispod:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils;
var
  MyGuid: TGUID;
begin
  Randomize;
  CreateGUID(MyGuid);
  WriteLn('[''' + GUIDToString(MyGuid) + ''']');
end.

10.2. Tipizovanje interfejsa

Pretpostavimo da imamo proceduru sa sledećim potpisom:

procedure UseThroughInterface(I: IMyInterface);

Kada ga pozivamo sa promenljivom InterfacedVariablekoja nije baš tipa IMyInterface, moramo da ga pretvorimo u tip. Postoji nekoliko opcija koje možete izabrati:

    Pretvaranje u oblik pomoću asoperatora:

    UseThroughInterface(InterfacedVariable as IMyInterface);

    Ako se izvrši, izvršiće proveru tokom izvršavanja i izazvalo bi izuzetak ako InterfacedVariablene implementira IMyInterface.

    Korišćenje asoperatora funkcioniše konzistentno bez obzira na to da li InterfacedVariableje deklarisan kao instanca klase (kao TSomeClass) ili interfejs (kao ISomeInterface). Međutim, pretvaranje interfejsa u drugi interfejs na ovaj način nije dozvoljeno pod {$interfaces corba}- tu temu ćemo obraditi kasnije.

    Eksplicitno pretvaranje u tip:

    UseThroughInterface(IMyInterface(InterfacedVariable));

    Obično, takva sintaksa pretvaranja tipa ukazuje na nebezbedno, neprovereno pretvaranje tipa. Loše stvari će se desiti ako pretvorite tip u pogrešan interfejs. I to je tačno, ako pretvorite klasu u klasu 1 ili interfejs u ​​interfejs 2 , koristeći ovu sintaksu.

    Ovde postoji mali izuzetak: ako InterfacedVariableje deklarisano kao klasa (kao TSomeClass), onda je ovo pretvaranje tipa (typecast) koje mora biti validno u vreme kompajliranja. Dakle, pretvaranje klase u interfejs na ovaj način je bezbedno, brzo (provereno u vreme kompajliranja) pretvaranje tipa.

    Implicitno pretvaranje tipova:

    UseThroughInterface(InterfacedVariable);

    U ovom slučaju, typecast mora biti validan u vreme kompajliranja. Ovo će se kompajlirati samo ako tip `` InterfacedVariable(klasa ili interfejs)`` implementira `` IMyInterface.

    U suštini, ovo pretvaranje tipa u tip izgleda i radi baš kao i za regularne klase. Gde god je potrebna instanca klase TSomeClass, uvek možete koristiti promenljivu tamo koja je deklarisana sa klasom TSomeClass, ili TSomeClasspotomkom . Isto pravilo važi i za interfejse. U takvim situacijama nema potrebe za bilo kakvim eksplicitnim pretvaranjem tipa u tip.

Da biste sve testirali, poigrajte se sa ovim primernim kodom:

{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

// {$interfaces corba} // note that "as" typecasts will not compile with CORBA interfaces

uses Classes;

type
  IMyInterface = interface
  ['{7FC754BC-9CA7-4399-B947-D37DD30BA90A}']
    procedure One;
  end;

  IMyInterface2 = interface(IMyInterface)
  ['{A72B7008-3F90-45C1-8F4C-E77C4302AA3E}']
    procedure Two;
  end;

  IMyInterface3 = interface(IMyInterface2)
  ['{924BFB98-B049-4945-AF17-1DB08DB1C0C5}']
    procedure Three;
  end;

  TMyClass = class(TComponent, IMyInterface)
    procedure One;
  end;

  TMyClass2 = class(TMyClass, IMyInterface, IMyInterface2)
    procedure One;
    procedure Two;
  end;

procedure TMyClass.One;
begin
  Writeln('TMyClass.One');
end;

procedure TMyClass2.One;
begin
  Writeln('TMyClass2.One');
end;

procedure TMyClass2.Two;
begin
  Writeln('TMyClass2.Two');
end;

procedure UseInterface2(const I: IMyInterface2);
begin
  I.One;
  I.Two;
end;

procedure UseInterface3(const I: IMyInterface3);
begin
  I.One;
  I.Two;
  I.Three;
end;

var
  MyInterface: IMyInterface;
  MyClass: TMyClass;
begin
  MyInterface := TMyClass2.Create(nil);
  MyClass := TMyClass2.Create(nil);

  // This doesn't compile, since at compile-time it's unknown if MyInterface is IMyInterface2.
  // UseInterface2(MyInterface);
  // UseInterface2(MyClass);

  // This compiles and works OK.
  UseInterface2(IMyInterface2(MyInterface));
  // This does not compile. Casting InterfaceType(ClassType) is checked at compile-time.
  // UseInterface2(IMyInterface2(MyClass));

  // This compiles and works OK.
  UseInterface2(MyInterface as IMyInterface2);
  // This compiles and works OK.
  UseInterface2(MyClass as IMyInterface2);

  // This compiles, but will fail at runtime, with ugly "Access violation".
  // UseInterface3(IMyInterface3(MyInterface));
  // This does not compile. Casting InterfaceType(ClassType) is checked at compile-time.
  // UseInterface3(IMyInterface3(MyClass));

  // This compiles, but will fail at runtime, with nice "EInvalidCast: Invalid type cast".
  // UseInterface3(MyInterface as IMyInterface3);
  // This compiles, but will fail at runtime, with nice "EInvalidCast: Invalid type cast".
  // UseInterface3(MyClass as IMyInterface3);

  Writeln('Finished');
end.

10.3. CORBA i COM tipovi interfejsa
Napomena
	Ovaj odeljak je relevantan samo za FPC. Delphi ima samo COM interfejse.

Zašto se interfejsi (predstavljeni gore) nazivaju "CORBA"?

    Zato što se ove vrste interfejsa mogu koristiti zajedno sa CORBA (Common Object Request Broker Architecture) tehnologijom (videti Vikipediju o CORBA ).

    Ali oni nisu zaista vezani za CORBA tehnologiju.

    Naziv CORBA je možda nesrećan. Bolji naziv bi bio goli interfejsi . Poenta ovih interfejsa je da su oni "čisto jezička funkcija" . Koristite ih kada želite da pretvorite različite klase u isti interfejs, jer dele zajednički API, a ne želite druge funkcije (kao što su brojanje referenci ili COM integracija).
Kako se ovi jezici porede sa drugim programskim jezicima?

    CORBA interfejsi u Object Pascal-u rade prilično slično interfejsima u Javi ( https://docs.oracle.com/javase/tutorial/java/concepts/interface.html ) ili C#-u ( https://msdn.microsoft.com/en-us/library/ms173156.aspx ).

    Iako jezici Java i C# imaju sakupljanje smeća , poređenje je donekle pogrešno, bez obzira da li se poredi sa CORBA ili COM interfejsima. Po našem iskustvu, CORBA interfejsi u Paskalu su slični Javi i C# interfejsima po načinu na koji se koriste . To jest, CORBA interfejse koristite kada želite da nepovezane (ne potiču jedna od druge) klase dele zajednički API i ne želite da se bilo šta drugo menja.
Da li je {$interfaces corba}deklaracija potrebna?

    Da, zato što podrazumevano kreirate COM interfejse . Ovo se može eksplicitno navesti rekavši {$interfaces com}, ali obično nije potrebno jer je to podrazumevano stanje.

    I ne savetujem korišćenje COM interfejsa , posebno ako tražite nešto ekvivalentno interfejsima iz drugih programskih jezika. CORBA interfejsi u Paskalu su upravo ono što očekujete ako tražite nešto ekvivalentno interfejsima u C# i Javi. Dok COM interfejsi donose dodatne funkcije koje verovatno ne želite.

    Imajte na umu da {$interfaces xxx}deklaracija utiče samo na interfejse koji nemaju eksplicitnog pretka (samo ključnu reč interface, ne interface(ISomeAncestor)). Kada interfejs ima pretka, on ima isti tip kao i pretak, bez obzira na deklaraciju {$interfaces xxx}.
Šta su COM interfejsi?

    COM interfejs je sinonim za interfejs koji potiče od posebnog IUnknowninterfejsa . Silazi od IUnknown:

        Zahteva da vaše klase definišu metode _AddRefi _ReleaseRef. Pravilna implementacija ovih metoda može upravljati životnim vekom vaših objekata koristeći brojanje referenci.

        Dodaje QueryInterfacemetod.

        Omogućava interakciju sa COM (Component Object Model) tehnologijom .

Zašto savetujete da se ne koriste COM interfejsi?

    Zato što COM interfejsi "isprepliću" dve funkcije koje bi, po mom mišljenju, trebalo da budu nepovezane (ortogonalne): višestruko nasleđivanje i brojanje referenci . Drugi programski jezici s pravom koriste odvojene koncepte za ove dve funkcije.

    Da bude jasno: brojanje referenci , koje omogućava automatsko upravljanje memorijom (u jednostavnim situacijama, tj. bez ciklusa), je veoma koristan koncept . Ali preplitanje ove funkcije sa interfejsima (umesto da se od njih naprave ortogonalne funkcije) je nečisto po mom mišljenju . Definitivno ne odgovara mojim slučajevima upotrebe.

        Ponekad želim da pretvorim svoje (inače nepovezane) klase u zajednički interfejs.

        Ponekad želim da upravljam memorijom koristeći pristup brojanja referenci.

        Možda ću jednog dana želeti da interagujem sa COM tehnologijom .

    Ali sve su to odvojene, nepovezane potrebe. Njihovo preplitanje u jednu jezičku karakteristiku je, po mom iskustvu, kontrakorisno. Zaista izaziva stvarne probleme:

        Ako želim funkciju pretvaranja klasa u zajednički interfejs API , ali ne želim mehanizam brojanja referenci (želim ručno da oslobađam objekte), onda su COM interfejsi problematični. Čak i kada je brojanje referenci onemogućeno posebnom implementacijom, _AddRefi _ReleaseRefdalje morate biti oprezni da nikada nemate privremenu referencu interfejsa koja visi, nakon što ste oslobodili instancu klase. Više o ovome u sledećem odeljku.

        Ako želim funkciju brojanja referenci , ali mi nije potrebna hijerarhija interfejsa koja predstavlja nešto drugačije od hijerarhije klasa, onda moram da dupliram API svojih klasa u interfejsima. Tako se stvara jedan interfejs za svaku klasu. Ovo je kontraproduktivno. Mnogo bih radije imao pametne pokazivače kao zasebnu jezičku funkciju, a ne isprepletene sa interfejsima (i srećom, to dolazi :).

    Zato savetujem korišćenje CORBA stilskih interfejsa i {$interfaces corba}direktiva u svim modernim kodovima koji se bave interfejsima.

    Samo ako vam je potrebno i "brojanje referenci" i "višestruko nasleđivanje" istovremeno, onda koristite COM interfejse . Takođe, Delfi za sada ima samo COM interfejse, tako da morate koristiti COM interfejse ako vaš kod mora biti kompatibilan sa Delfijem.
Možemo li imati brojanje referenci sa CORBA interfejsima?

    Da. Samo dodajte metode _AddRef/ _ReleaseRef. Nema potrebe za spuštanjem iz IUnknowninterfejsa. Iako u većini slučajeva, ako želite brojanje referenci sa vašim interfejsima, možete jednostavno koristiti COM interfejse.

10.4. Interfejsi sa brojanjem referenci (COM)

COM interfejsi donose dve dodatne funkcije:

    integracija sa COM-om (tehnologija iz Windows-a, dostupna i na Unix-u preko XPCOM-a , koju koristi Mozilla),

    brojanje referenci (što vam daje automatsko uništavanje kada sve reference interfejsa izađu van dometa).

Kada koristite COM interfejse , morate biti svesni njihovog mehanizma automatskog uništavanja i veze sa COM tehnologijom.

U praksi, to znači da:

    Vaša klasa treba da implementira magične metode _AddRef, _Releasei QueryInterface. Ili da potiče od nečega što ih već implementira. Određena implementacija ovih metoda može zapravo omogućiti ili onemogućiti funkciju brojanja referenci COM interfejsa (iako je onemogućavanje donekle opasno — videti sledeću tačku).

        Standardna klasa TInterfacedObjectimplementira ove metode kako bi omogućila brojanje referenci.

        Standardna klasa TComponentimplementira ove metode da bi onemogućila brojanje referenci.

    Morate biti oprezni pri oslobađanju klase kada na nju mogu referencirati neke promenljive interfejsa. Pošto se interfejs oslobađa pomoću virtuelne metode (jer može biti brojan po referencama, čak i ako hakujete metodu _AddRef da se ne broji po referencama… ), ne možete osloboditi instancu osnovnog objekta sve dok neka promenljiva interfejsa ukazuje na nju. Pogledajte "7.7 Brojanje referenci" u FPC priručniku ( http://freepascal.org/docs-html/ref/refse47.html ).

Najbezbedniji pristup korišćenju COM interfejsa je da

    prihvatiti činjenicu da se broje na osnovu referenci,

    izvesti odgovarajuće klase iz TInterfacedObject,

    i izbegavajte korišćenje instance klase, umesto toga pristupajte instanci uvek preko interfejsa, dozvoljavajući brojanju referenci da upravlja dealokacijom.

Ovo je primer takve upotrebe interfejsa:

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$interfaces com}
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils, Classes;

type
  IMyInterface = interface
  ['{3075FFCD-8EFB-4E98-B157-261448B8D92E}']
    procedure Shoot;
  end;

  TMyClass1 = class(TInterfacedObject, IMyInterface)
    procedure Shoot;
  end;

  TMyClass2 = class(TInterfacedObject, IMyInterface)
    procedure Shoot;
  end;

  TMyClass3 = class(TInterfacedObject)
    procedure Shoot;
  end;

procedure TMyClass1.Shoot;
begin
  WriteLn('TMyClass1.Shoot');
end;

procedure TMyClass2.Shoot;
begin
  WriteLn('TMyClass2.Shoot');
end;

procedure TMyClass3.Shoot;
begin
  WriteLn('TMyClass3.Shoot');
end;

procedure UseThroughInterface(I: IMyInterface);
begin
  Write('Shooting... ');
  I.Shoot;
end;

var
  C1: IMyInterface;  // COM takes care of destruction
  C2: IMyInterface;  // COM takes care of destruction
  C3: TMyClass3;     // YOU have to take care of destruction
begin
  C1 := TMyClass1.Create as IMyInterface;
  C2 := TMyClass2.Create as IMyInterface;
  C3 := TMyClass3.Create;
  try
    UseThroughInterface(C1); // no need to use "as" operator
    UseThroughInterface(C2);
    if Supports(C3, IMyInterface) then
      UseThroughInterface(C3 as IMyInterface); // this will not execute
  finally
    { C1 and C2 variables go out of scope and will be auto-destroyed now.

      In contrast, C3 is a class instance, not managed by an interface,
      and it has to be destroyed manually. }
    FreeAndNil(C3);
  end;
end.

10.5. Korišćenje COM interfejsa sa onemogućenim brojanjem referenci

Kao što je pomenuto u prethodnom odeljku, vaša klasa može biti potomak TComponent(ili slične klase kao što su TNonRefCountedInterfacedObjecti TNonRefCountedInterfacedPersistent) što onemogućava brojanje referenci za COM interfejse. Ovo vam omogućava da koristite COM interfejse, a da i dalje ručno oslobodite instancu klase.

U ovom slučaju morate biti oprezni da ne oslobodite instancu klase kada neka interfejsna promenljiva može da se odnosi na nju. Zapamtite da svako pretvaranje tipa Cx as IMyInterfacetakođe kreira privremenu interfejs promenljivu, koja može biti prisutna čak i do kraja trenutne procedure. Iz tog razloga, primer ispod koristi UseInterfacesproceduru i oslobađa instance klase van ove procedure (kada možemo biti sigurni da su privremene interfejsne promenljive van dometa).

Da biste izbegli ovaj nered, obično je bolje koristiti CORBA interfejse, ako ne želite brojanje referenci sa vašim interfejsima.

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$interfaces com}
{$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  SysUtils, Classes;

type
  IMyInterface = interface
  ['{3075FFCD-8EFB-4E98-B157-261448B8D92E}']
    procedure Shoot;
  end;

  TMyClass1 = class(TComponent, IMyInterface)
    procedure Shoot;
  end;

  TMyClass2 = class(TComponent, IMyInterface)
    procedure Shoot;
  end;

  TMyClass3 = class(TComponent)
    procedure Shoot;
  end;

procedure TMyClass1.Shoot;
begin
  WriteLn('TMyClass1.Shoot');
end;

procedure TMyClass2.Shoot;
begin
  WriteLn('TMyClass2.Shoot');
end;

procedure TMyClass3.Shoot;
begin
  WriteLn('TMyClass3.Shoot');
end;

procedure UseThroughInterface(I: IMyInterface);
begin
  Write('Shooting... ');
  I.Shoot;
end;

var
  C1: TMyClass1;
  C2: TMyClass2;
  C3: TMyClass3;

procedure UseInterfaces;
begin
  // In FPC, you could also check using "is", like:
  //if C1 is IMyInterface then ...

  if Supports(C1, IMyInterface) then
    UseThroughInterface(C1 as IMyInterface);
  if Supports(C2, IMyInterface) then
    UseThroughInterface(C2 as IMyInterface);
  if Supports(C3, IMyInterface) then
    UseThroughInterface(C3 as IMyInterface);
end;

begin
  C1 := TMyClass1.Create(nil);
  C2 := TMyClass2.Create(nil);
  C3 := TMyClass3.Create(nil);
  try
    UseInterfaces;
  finally
    FreeAndNil(C1);
    FreeAndNil(C2);
    FreeAndNil(C3);
  end;
end.

11. O ovom dokumentu

Autorska prava Mihalis Kamburelis.

Izvorni kod ovog dokumenta nalazi se u AsciiDoc-u na https://github.com/modern-pascal/modern-pascal-introduction . Predlozi za ispravke i dopune, kao i zakrpe i zahtevi za preuzimanje, su uvek dobrodošli :) Možete me kontaktirati preko GitHub-a ili putem e-pošte michalis@castle-engine.io . Moja početna stranica je https://michalis.xyz/ . Link do ovog dokumenta je dostupan u odeljku Dokumentacija na veb-sajtu Castle Game Engine -a https://castle-engine.io/ .

Možete slobodno distribuirati, pa čak i menjati ovaj dokument, pod istim licencama kao i Vikipedija https://en.wikipedia.org/wiki/Wikipedia:Copyrights :

    Creative Commons Autorstvo-Deljenje pod istim uslovima 3.0 Neportovana licenca (CC BY-SA)

    ili GNU-ova licenca za slobodnu dokumentaciju (GFDL) (neverzionisana, bez nepromenljivih odeljaka, tekstova na prednjoj ili zadnjoj korici) .

Hvala vam što ste čitali!
