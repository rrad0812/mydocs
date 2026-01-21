
# 6 Klase

[prev][f1] [content][f0] [next][f2]

U Delfiju mogućnosti OOP-a uglavnom se vrte oko koncepta “Classes”. Klasa može biti razmatrana kao pokazivač na objekat, ili pokazivač na zapis, sa pridruženim metodama.

Razlike izmedju objekata i klasa  su uglavanom da je objekat alociran na steku, kao što bi pravi zapis trebao biti, i da se klase uvek alociraju na heapu. U sledećem primeru:

```pascal
Var  
  A: TSomeObject; // an Object  
  B: TSomeClass;  // a Class
```

Glavana razlika je da će varijabla A zauzeti prostor na seku u zavisnosti veličine objekta(TSomeObject). Varijabla B, će uvek zauzeti na steku veličinu pointera. Aktuelni podaci klase su na heap.

Druga razlika je: klasa uvek mora inicijalizovana kroz svoj konstruktor, dok za objekat to nije obaveza. Pozivanje konstruktora alocira neophodnu memoriju na heap za podatke instance klase.

**Napomena**  
U ranijim verzijama Free Pascala bilo je obavezno, da bi koristili klase, da postavimo `objpas` unit u `uses` klauzuli unita ili programa. To više od verzije 0.99.12 nije potrebno. Od te verzije unit će biti učitana automatski kada `-Mobjfpc` or `-Mdelphi` opcije su specificirani, ili njihove odgovarajuće direktive se koriste:

```pascal
{$mode objfpc}  
{$mode delphi}
```

U stvari, kompajler će dati upozorenje ako pronađe `objpas` unit u `uses` klauzuli.

## 6.1 Definicija klase

Prototip deklaracije klase je kao što sledi:

**Napomena**  
U MacPas modu, Object ključna reč je zamenjen sa class ključnom reči zbog kompatabilnosti sa drugim pascal kompajlerima raspoloživim na Macu.

U deklaraciji klase, više vidljivih blokova kao potreba mogu biti korišćeni: različiti blokovi mogu biti ponovljenim i ne postoji specijalni poredak u kume se mogu pojavljivati.

Metode su noramlne funkcijeske ili proceduralne deklaracije. Kao što se može primetiti, deklaracija klase je najčešće identična deklaraciji objekta. Realna razlika izmedju objekta i klase je u načinu kako se kreiraju.

Vidljivost različitih sekcija je kao što sledi:

- `Private`  
  Svim poljima i metodama koje su u `private` bloku može se pristupiti u modulu (tj. unitu) koji sadrži definiciju klase. Njima se može pristupiti iz klasnih metoda ili izvan njih (tj. iz medoda drugih klasa unita.)
- `Strict Private`
  Svim poljima i metodama koje su u `strict private` bloku, može se pristupiti jedino iz metoda same klase. Druge klase unita, ili nasleđene klase, čak u istom unitu, ne mogu pristupiti `strict private` članovima.
- `Protected`
  Članovima `Protected` sekcije su takođe pristupačni drugim klasama implementiranim u istiom unitu kao i izvedenim klasama implementirane u ovom ili drugim modulima.
- `Strict Protected`
  `Strict protected` članovi su jedino vidljivi izvedenim klasama, ne za druge klase u istom ili drugim unitima.
- `Public`
  sekcije su uvek pristupne.
- `Published`
  Iz perspektive jezika, ovo je isto kao `Public` section, ali kompajler generiše informacije tipa koje su potrebne za automatski streaming te klase, ako je kompajler u {$M+} stanju. Polja definisana u `Published` sekciju moraju biti tipa klase. Niz properties ne može biti u `Published` sekciju.

Klase mogu sadržavati `class methods`: to su rutine koje ne zahtevaju instancu. Self identifikator je valida u tim metodama ali pokazuje na class pointer ( VMT ).

**Remark**  
Kao i kod funkcija i tipova pokazivača, ponekad je potrebna definicija klase unapred. Definicija klase unapred je jednostavno ime klase, sa ključnom rečju Class, kao u sledećem primeru:

```pascal
Type  
  TClassB = Class;  
  TClassA = Class  
    B : TClassB;  
  end;  
 
  TClassB = Class  
   A : TClassA;  
  end;
```

Kada koristite definiciju klase unapred, klasa mora biti definisana u istom unitu i u istom odeljku (interfejs/implementacija). Ne mora nužno biti definisan u odeljku istog tipa.

Referentni tipovi klasa se koriste za kreiranje instanci određene klase, koja još nije poznata u vreme kompajliranja, ali je specificirana u vreme izvršavanja. U suštini, promenljiva referentnog tipa klase sadrži pokazivač na definiciju navedene klase. Ovo se može koristiti za konstruisanje instance klase koja odgovara definiciji ili za proveru nasleđivanja. Sledeći primer pokazuje kako to funkcioniše:

```pascal
Type  
  TComponentClass = Class of TComponent;  

Function CreateComponent(AClass: TComponentClass; 
                             AOwner: TComponent): TComponent;  

begin  
  // ...  
  Result:=AClass.Create(AOwner);  
  // ...  
end;
```

Ovoj funkciji može biti prosleđena referenca bilo koje klasekoja nasledjuje `TComponent`. Sledeći poziv je validan:

```pascal
Var  
  C : TComponent;  

begin  
  C:=CreateComponent(TEdit, Form1);  
end;
```

Na povratku `CreateComponent` funkcije, C će sadržati instancu klase `TEdit`. Primetite da se sledeći primer neće kompajlirati:

```pascal
Var  
  C : TComponent;  

begin  
  C:=CreateComponent(TStream,Form1);  
end;
```

jer `TStream` ne nasleđuje iz `TComponent`, a `AClass` referencira na `TComponent` class. Kompajler može (i hoće) da proveri ovo u vremenu kompajliranja, i proizvešće grešku.

Referenca klase može takodje biti korišćena za proveru nasledjivanja:

```pascal
  TMinClass = Class of TMyClass;  
  TMaxClass = Class of TMyClassChild;  

Function CheckObjectBetween(Instance : TObject) : boolean;  

begin  
  If not (Instance is TMinClass)  
     or ((Instance is TMaxClass)  
          and (Instance.ClassType<>TMaxClass)) then  
    Raise Exception.Create(SomeError)  
end;
```

Gornji primer će podići izuzetak ako prosledjena instanca nije nasledjena iz `TMinClass` ili `TMaxClass`.

## 6.2 Abstraktne i zapečaćene klase

Klasa može biti deklarisana kao `sealed`. U tom slučaju nije moguće deklarisanje nasledjenih klasa. Kompajler će vratiti grešku:

```pascal
{$mode objfpc}  
{$h+}  
 
Type  
  TMyClass = Class Sealed  
    x : integer;  
  end;  
 
  TMyClass2 = Class(TMyClass)  
    Y : Integer;  
  end;  
 
begin  
end.
```

Ovo će rezultovati greškom:

```sh
Error: Cannot create a descendant of the sealed class "TMyClass"
```

Abstraktna klasa je klasa koja se ne može instancirati direktno. Umesto toga nasledna klasa mora uvek biti instancirana.

## 6.3 Normalna i statička polja

Klase mogu imati polja. U zavisnosti od toga kako su definisana, polja sadrže podatke specifične za instancu klase. Bez obzira kako su definisana, polja mogu imati nametnuta pravila vidljivosti kao i drugi članovi klase.

### 6.3.1 Normalna polja/varijable

Postoji dva načina za deklaraciju normalnih polja. Prvi je klasičan način, sličan definiciji objekta:

```pascal
{$mode objfpc}

type  
  cl=class  
    l : longint;  
  end;  

var  
  cl1,cl2 : cl;  

begin  
  cl1:=cl.create;  
  cl2:=cl.create;  
  cl1.l:=2;  
  writeln(cl1.l);  
  writeln(cl2.l);  
end.
```

Ovaj kod će napraviti izlaz:

```sh
2  
0
```

Ovaj primer demonstira da vrednosti polja su inicijalizovana sa nulom (ili ekvivalentom nule za ne ordinalne tipove, `empty string`, `empty array` and so on).

Drugi način za deklaracije polja (u najnovijim verzijama FreePascala) je korišćenje `var` block:

```pascal
{$mode objfpc}

type  
  cl=class  
  var  
    l : longint;  
  end;
```

Ova definicija je ekvivalentna prehodnoj.

**Remark**  
Od verzije 3.0 kompajlera, kompajler može preurediti poredak polja u memoriji ako to vodi boljem poravnjavanju i manjoj instanci. To znači, da u instanci polja ne moraju da se pojave u istom poretku kao u deklaraciji. RTTI generisan za klasu će reflektovati ovu promenu.

### 6.3.2 Polja/varijable klase

Slično objektima, klasa može da sadrži statička polja ili promenljive klase: ova polja ili promenljive su globalne za klasu i deluju kao globalne promenljive, ali su poznate samo kao deo klase. Na njih se može referencirati unutar metoda klasa, ali se mogu referencirati i izvan klase davanjem potpunog kvalifikovanog imena.

Opet, postoje dva načina da se definišu promenljive klase. Prvi je ekvivalentan načinu na koji se radi u objektima, koristeći `static` modifikator:

Na primer, izlaz sledećeg programa je isti kao i izlaz za verziju koja koristi objekat:

```pascal
{$mode objfpc}

type  
  cl = class  
    l : longint; static;  
  end;  

var  
  cl1,cl2 : cl;  

begin  
  cl1:=cl.create;  
  cl2:=cl.create;  
  cl1.l:=2;  
  writeln(cl2.l);  
  cl2.l:=3;  
  writeln(cl1.l);  
  Writeln(cl.l);  
end.
```

Izlaz ovo koda će biti:

```sh
2  
3  
3
```

Primetite da poslednja linja koda referencira samu klasu (`cl`), a ne instancu klase (cl1 ili cl2).

In addition to the static field approach, in classes, a Class Var can be used. Similar to the way a field can be defined in a variable block, a class variable can be declared in a class var block:

```pascal
{$mode objfpc}  

type  
  cl=class  
    class var  
      l : longint;  
  end;
```

Ova definicija je ekvivalentna prethodnoj.

Primetimo da je `class variable` povezan za određenu klasu. Nasledne klase će se odnositi na istu instancu, osim ako se promenljiva ponovo deklariše. Sledeći program to pokazuje:

```pascal
{$mode objfpc}  

type  
  TA = class                  // base type  
    class var Code: integer;  
  end;  
  TB = class(TA);  
  TC = class(TA);  
 
begin  
  TA.Code:=0;  
  TB.Code:=1;  
  TC.Code:=2;  
  Writeln(Ta.Code:2,Tb.Code:2,Tc.code:2);  
end.
```

Izlaz je:

```sh
 2 2 2
```

Pošto je povezana sa klasom, može biti preklopljeno u Delphi modu:

```pascal
{$mode delphi}  

type  
  TA = class // base type  
    class var code: integer;  
  end;  
  TB = class(TA)  
    Class var code : integer;  
  end;  
  TC = class(TA)  
    Class var code : integer;  
  end;  
 
begin  
  TA.Code:=0;  
  TB.Code:=1;  
  TC.Code:=2;  
  Writeln(Ta.Code:2,Tb.Code:2,Tc.code:2);  
end.
```

It will print the following:

```sh
 0 1 2
```

Medjutim, u OBJFPC modu to se neće kompajlirati, i daće `duplicate identifier error`.

## 6.4 Instanciranje

Klase moraju biti kreirane pomoću jednog od njihovih konstruktora (može biti više konstruktora). Zapamtite da je instanca klase pokazivač na objekat na heapu. Kada se deklariše promenljiva neke klase, kompajler samo dodeljuje prostor za ovaj pokazivač, a ne za ceo objekat. Konstruktor klase vraća pokazivač na inicijalizovanu instancu objekta na heapu. Dakle, da biste inicijalizovali instancu neke klase, uradili bi sledeće:

```pascal
  ClassVar := ClassType.ConstructorName;
```

Proširena sintaksa `new` i `dispose` se ne može se koristiti za instanciranje i uništavanje instanci klase: ta konstrukcija je rezervisana za upotrebu samo sa objektima. Pozivanje konstruktora će izazvati poziv virtuelne metode klase NewInstance, koja, u svojoj podrazumevanoj implementaciji, poziva GetMem, da dodeli dovoljno prostora za čuvanje podataka o instanci klase, a zatim isprazni memoriju.

Posle toga, kod konstrujtora je izvršen. Konstruktor ima pokazivač na svoje podatke, u `self`.

**Remark**:

- `{$PackRecords }` direktiva takođe se primenjuje na klase, tj. poravnavanje u memoriji
  različitih polja zavisi od ove direktive.
- Kao za `objects` i `records`, pakovane klase mogu biti deklarisane. Ovo ima isti efekat kao kod
  object ili record, elementi su poravnate na 1-byte granice, što bliže moguće.
- `SizeOf(class)` će vratiti isto kao `SizeOf(Pointer)`, pošto je klasa pokazivač na objekat.
  Da dobijete veličinu instance podataka, korišćenjem `TObject.InstanceSize` metoda.
- Ako se pojavi izuzetak za vreme izvršenja konstruktora, destruktor će biti pozvan automatski.

## 6.5 Destrukcija instance

Instance klase moraju biti uništene korišćenjem destructor. Za razliku od konstruktor, nema izbora za destruktor: destruktor mora imati ime `Destroy`, on mora preklopiti `Destroy` destruktor deklarisan u `TObject`, ne može imati argumente, i nasledjeni destruktor mora uvek biti pozvon.

Destroy će pozvati `FreeInstance`, koja će po podrazumevanoj implementaciji, pozvati `FreeMem` za oslobadjanje memorije zauzete instancom.

Da bi sprečili pozivanje destruktor na Nil instanci, najbolje je pozvati `Free` metod `TObject`-a. Ovaj metod će proveriti da li `Self` nije Nil, i ako nije, tada će pozvati `Destroy`. Ako je `Self` jednak Nil, on će izaći.

Uništavanje instance ne uklanja ili postavlja Nil na referencu instance:

```pascal
Var  
  A : TComponent;  
 
begin  
  A:=TComponent.Create;  
  A.Name:='MyComponent';  
  A.Free;  
  Writeln('A is still assigned: ', Assigned(A));  
end.
```

Posle poziva `Free`, varijabla A neće biti Nil, izlaz programa biće:

```sh
A is still assigned: TRUE
```

Da bi bili sigurni da je varijabla A očišćena pošto je destruktor pozvan, funkcija `FreeAndNil` iz `SysUtils` unit može biti korišćen. To će pozvati `Free` i potom postaviti `Nil` na pokazivač objekta. (A u gornjem primeru):

```pascal
Var  
  A : TComponent;  
 
begin  
  A:=TComponent.Create;  
  A.Name:='MyComponent';  
  FreeAndNil(A);  
  Writeln('A is still assigned: ',Assigned(A));  
end.
```

Posle poziva `FreeAndNil`, varijabla A će sadržavati Nil, izlaz će biti:

```sh
A is still assigned: FALSE
```

**Remark**  
Ako se pojavi izuzetak za vreme izvršavanje konstruktora, destruktor će biti pozvan automatski.

## 6.6 Metode

### 6.6.1 Deklaracija

Deklaracija metoda klase prati neka pravila kao deklaracija metoda u objektima.

Jedina razlika je `override`, `reintroduce` i `message` direktive.

### 6.6.2 Pozivanje metoda

Pozivanje metoda u klasama nije drugačije od objekata:

```pascal
Var  
  AnObject: TAnObject;  

begin  
  AnObject:= TAnObject.Create;  
  ANobject.AMethod;
```

### 6.6.3 Virtuelne metode

Klase imaju virtuelne metode, baš kao i objekti. Međutim, postoji razlika između to dvoje. Za objekte, dovoljno je ponovo deklarisati isti metod u objektu potomka sa ključnom rečju `virtual` da bi se on zamenio. Za klase je situacija drugačija: virtuelne metode moraju biti zamenjene ključnom reči `override`. Ako to ne uradite, pokrenuće se nova serija virtuelnih metoda, skrivajući prethodnu. Ključna reč `Inherited` neće skočiti na nasleđeni metod, ako je korišćen u virtuelnom.

The following code is wrong:

```pascal
Type  
  ObjParent = Class  
    Procedure MyProc; virtual;  
  end;  
  
  ObjChild  = Class(ObjPArent)  
    Procedure MyProc; virtual;  
  end;
```

Kompajler će prijaviti grešku:

```sh
Warning: An inherited method is hidden by OBJCHILD.MYPROC
```

Kompajler će da kompajlira, ali korišćenje `Inherited` može izazvati čudne efekte.

Korktna deklaracija je:

```pascal
Type  
  ObjParent = Class  
    Procedure MyProc; virtual;  
  end;  

  ObjChild  = Class(ObjPArent)  
    Procedure MyProc; override;  
  end;
```

Ovaj kod će se kompajlirati be upozorenja na greške.

Ako bi virtualni method trebao stvarno biti zamenjen sa metodom istog imena, tada `reintroduce` ključna reč može biti korišćena:

```pascal
Type  
  ObjParent = Class  
    Procedure MyProc; virtual;  
  end;  

  ObjChild  = Class(ObjPArent)  
    Procedure MyProc; reintroduce;  
  end;
```

Ovaj novi metod nije virtuelni.

Da bi se ovo omogućilo, kompajler pravi po klasi ( po tipu ) – tabelu sa virtuelnim metodama: VMT (Virtual Method Table). Ovo je jedna tabela sa pokazivačima na svaku virtuelnu metodu: svaki virtuelni metod ima fiksiranu lokaciju u ovaoj tabeli. Kompajler će koristiti ovu tabelu za look up aktuelni metod koji mora biti korišćen u runtime-u. Kada nasledni objekat preklopi metod, ulaz roditeljskog metoda je prepisan u VMT.

**Remark**  
Ključna reč `virtual` može biti prepisan sa `dynamic` ključnom reči: `dynamic` metode se ponašaju kao virtualne metode. Za razliku od Delphi, u FPC implementatcija `dynamic` metode su jednake za implementaciju kao virtuelne metode.

### 6.6.4 Class metode

`Class metode` se identifikuju sa ključnom reči `Class` ispred procedure ili function deklaracije:

```pascal
  Class Function ClassName : String;
```  

`Class metode` su metode koje nemaju instancu (tj. Self ne pokazuje na instancu klase) ali koja prati opseg i pravila nasledjivanja klase. Oni mogu biti korišćenje za povratak informacija o trenutnoj klasi, na primer za registraciju ili korišćenje u class factory. Sve dok instanca klase nije raspoloživa, nema informacije o instancama koje mogu biti korišćene.

Class metode mogu biti pozvane iz regularnog metoda ali mogu biti pozvane i korišćenjem identifikatora klase:

```pascal
Var  
  AClass : TClass; // AClass is of type "type of class"  

begin  
  ..  
  if CompareText(AClass.ClassName,'TCOMPONENT')=0 then  
  ...  
```

Ali pozivanje njih iz instance je takođe moguće:

```pascal
Var  
  MyClass : TObject;  

begin  
  ..  
  if MyClass.ClassNameis('TCOMPONENT') then  
  ...
```

Obrnuto nije moguće: Unutar class metoda, Self identifikator pokazuje na VMT tabelu klase. Nema polja, propertija ili regularnih metoda. Pristup regularnom propertiju ili metodi će rezultovati greškom kompajlera.

Primetimo, da class metode mogu biti `virtual`, i mogu biti `override`.

Class metode mogu biti korišćene kao `read` ili `write` specifikatori za regularan property, ali prirodno, taj property će imati istu vrednost za sve instance klase, pošto instance nisu raspoložive u class metodama.

### 6.6.5 Konstruktori i destruktori klase

Takođe se može kreirati konstruktor ili destruktor klase. Oni služe da instanciraju neke promenljive klase ili svojstva klase koja moraju biti inicijalizovana pre nego što se klasa može koristiti. Ovi konstruktori se pozivaju automatski pri pokretanju programa: konstruktor se poziva pre odeljka za inicijalizaciju unita u kome je deklarisan, destruktor se poziva posle sekcije finalizacije unita u kome je deklarisan.

Ovde su neke činjenice kada koristimo class destructors/constructors:

- Može biti samo jedan konstruktor po klasi. Ime je proizvoljno, ali ne moge imati parametre.
- Može biti samo jedan konstruktor po klasi. Ime je proizvoljno, ali ne može imati parametre.
- Ni konstruktor ni destruktor ne mogu biti virtual.
- Konstruktor/Destruktor je pozvan bez obzira na korišćenje klase: čak iako se klasa ne koristi,
  konstruktor i destruktor se pozivaju.
- Ne postoji garancija poretka pozivanja konstruktora klasa. Za ugnježdene klase, jedini
  garantovani poredak je da konstruktori ugneždenih klasa se pozivaju posle konstruktora obuhvatne klase, i da se deskriptori se pozivaju u kontra redosledu.

```pascal
{$mode objfpc}{$h+}  

Type  
  TA = Class(TObject)  
  Private  
    Function GetA : Integer;  
    Procedure SetA(AValue : integer);  

  public  
    Class Constructor create;  
    Class Destructor destroy;  
    Property A : Integer Read GetA Write SetA;  
  end;  

{Class} Function TA.GetA : Integer;  

begin  
  Result:=-1;  
end;  

{Class} Procedure TA.SetA(AValue : integer);  

begin  
  //  
end;  

Class Constructor TA.Create;  

begin  
  Writeln('Class constructor TA');  
end;  

Class Destructor TA.Destroy;  

begin  
  Writeln('Class destructor TA');  

end;  

Var  
  A : TA;  

begin  
end.
```

Will, when run, output the following:

```sh
Class constructor TA  
Class destructor TA
```

### 6.6.6 Statičke (class) metode

FPC knows static class methods in classes: these are class methods that have the Static keyword at the end. These methods behave completely like regular procedures or functions. This means that:

- They do not have a Self parameter. As a result, they cannot access roperties
  or fields or regular methods.
- They cannot be virtual.
- They can be assigned to regular procedural variables.

Their use is mainly to include the method in the namespace of the class as opposed to having the procedure in the namespace of the unit. Note that they do have access to all class variables, types etc, meaning something like this is possible:

```pascal
{$mode objfpc}  
{$h+}  

Type  
  TA = Class(TObject)  
  Private  
    class var myprivatea : integer;  
  public  
    class Function GetA : Integer;  static;  
    class Procedure SetA(AValue : Integer); static;  
  end;  

Class Function TA.GetA : Integer;  

begin  
  Result:=myprivateA;  
end;  

Class Procedure TA.SetA(AValue : integer);  

begin  
  myprivateA:=AValue;  
end;  

begin  
  TA.SetA(123);  
  Writeln(TA.MyPrivateA);  
end.
```

Which will output 123, when run.

In the implementation of a static class method, the Self identifier is not available. The method behaves as if Self is hardcoded to the declared class, not the actual class with which it was called. In regular class methods, Self contains the Actual class for which the method was called. The following example makes this clear:

```pascal
Type  
  TA = Class  
    Class procedure DoIt; virtual;  
    Class Procedure DoitStatic; static;  
  end;  

  TB = CLass(TA)  
    Class procedure DoIt; override;  
  end;  

Class procedure TA.DOit;  

begin  
  Writeln('TA.Doit : ',Self.ClassName);  
end;  

Class procedure TA.DOitStatic;  

begin  
  Doit;  
  Writeln('TA.DoitStatic : ',ClassName);  
end;  

Class procedure TB.DoIt;  

begin  
  Inherited;  
  Writeln('TB.Doit : ',Self.ClassName);  
end;  

begin  
  Writeln('Through static method:');  
  TB.DoItStatic;  
  Writeln('Through class method:');  
  TB.Doit;  
end.
```

When run, this example will print:

```sh
Through static method:  
TA.Doit : TA  
TA.DoitStatic : TA  
Through class method:  
TA.Doit : TB  
TB.Doit : TB
```

For the static class method, even though it was called using TB, the class (Self, if it were available) is set to TA, the class in which the static method was defined. For the class method, the class is set to the actual class used to call the method (TB).

### 6.6.7 Message methods

New in classes are message methods. Pointers to message methods are stored in a special table, together with the integer or string constant that they were declared with. They are primarily intended to ease programming of callback functions in several GUI toolkits, such as Win32 or GTK. In difference with Delphi, Free Pascal also accepts strings as message identifiers. Message methods are always virtual.

As can be seen in the class declaration diagram, message methods are declared with a Message keyword, followed by an integer constant expression.

Additionally, they can take only one var argument (typed or not):

```pascal
  Procedure TMyObject.MyHandler(Var Msg); Message 1;
```

The method implementation of a message function is not different from an ordinary method. It is also possible to call a message method directly, but this should not be done. Instead, the TObject.Dispatch method should be used. Message methods are automatically virtual, i. e. they can be overridden in descendent classes.

The TObject.Dispatch method can be used to call a message handler. It is declared in the system unit and will accept a var parameter which must have at the first position a cardinal with the message ID that should be called. For example:

```pascal
Type  
  TMsg = Record  
    MSGID : Cardinal;  
    Data : Pointer;  
Var  
  Msg : TMSg;  

MyObject.Dispatch (Msg);
```

In this example, the Dispatch method will look at the object and all its ancestors (starting at the object, and searching up the inheritance class tree), to see if a message method with message MSGID has been declared. If such a method is found, it is called, and passed the Msg parameter.

If no such method is found, DefaultHandler is called. DefaultHandler is a virtual method of TObject that doesn’t do anything, but which can be overridden to provide any processing that might be needed. DefaultHandler is declared as follows:

```pascal
procedure DefaultHandler(var message);virtual;
```

In addition to the message method with a Integer identifier, Free Pascal also supports a message method with a string identifier:

```pascal
Procedure TMyObject.MyStrHandler(Var Msg); Message 'OnClick';
```

The working of the string message handler is the same as the ordinary integer message handler:

The `TObject.DispatchStr` method can be used to call a message handler. It is declared in the system unit and will accept one parameter which must have at the first position a short string with the message ID that should be called. For example:

```pascal
Type  
  TMsg = Record  
    MsgStr : String[10]; // Arbitrary length up to 255 characters.  
    Data : Pointer;  
Var  
  Msg : TMSg;  

MyObject.DispatchStr (Msg);
```

In this example, the DispatchStr method will look at the object and all its ancestors (starting at the object, and searching up the inheritance class tree), to see if a message method with message MsgStr has been declared. If such a method is found, it is called, and passed the Msg parameter.

If no such method is found, DefaultHandlerStr is called. DefaultHandlerStr is a virtual method of TObject that doesn’t do anything, but which can be overridden to provide any processing that might be needed. DefaultHandlerStr is declared as follows:

```pascal
  procedure DefaultHandlerStr(var message);virtual;
```

### 6.6.8 Using inherited

In an overridden virtual method, it is often necessary to call the parent class’ implementation of the virtual method. This can be done with the inherited keyword. Likewise, the inherited keyword can be used to call any method of the parent class.

The first case is the simplest:

```pascal
Type  
  TMyClass = Class(TComponent)  
    Constructor Create(AOwner : TComponent); override;  
  end;  

Constructor TMyClass.Create(AOwner : TComponent);  

begin  
  Inherited;  
  // Do more things  
end;
```

In the above example, the Inherited statement will call Create of TComponent, passing it AOwner as a parameter: the same parameters that were passed to the current method will be passed to the parent’s method. They must not be specified again: if none are specified, the compiler will pass the same arguments as the ones received.

If no inherited method with the same name exists, the Inherited will have no effect in this case. The presence of Inherited in this form can thus be interpreted as “call the overridden method if it exists”.

The second case is slightly more complicated:

```pascal
Type  
  TMyClass = Class(TComponent)  
    Constructor Create(AOwner : TComponent); override;  
    Constructor CreateNew(AOwner : TComponent; DoExtra : Boolean);  
  end;  

Constructor TMyClass.Create(AOwner : TComponent);  
begin  
  Inherited;  
end;  

Constructor TMyClass.CreateNew(AOwner : TComponent; DoExtra : Boolean);  
begin  
  Inherited Create(AOwner);  
  // Do stuff  
end;
```

The CreateNew method will first call TComponent.Create and will pass it AOwner as a parameter. It will not call TMyClass.Create.

If no method with the given name exists in parent classes, the compiler will give an error.

Although the examples were given using constructors, the use of inherited is not restricted to constructors, it can be used for any procedure or function or destructor as well.

## 6.7 Properties

### 6.7.1 Definition

Classes can contain properties as part of their fields list. A property acts like a normal field, i. e. its value can be retrieved or set, but it allows to redirect the access of the field through functions and procedures. They provide a means to associate an action with an assignment of or a reading from a class “field”. This allows e. g. checking that a value is valid when assigning, or, when reading, it allows to construct the value on the fly. Moreover, properties can be read-only or write only.

A read specifier is either the name of a field that contains the property, or the name of a method function that has the same return type as the property type. In the case of a simple type, this function must not accept an argument. In case of an array property, the function must accept a single argument of the same type as the index. In case of an indexed property, it must accept a integer as an argument.

A read specifier is optional, making the property write-only. Note that class methods cannot be used as read specifiers.

A write specifier is optional: If there is no write specifier, the property is read-only. A write specifier is either the name of a field, or the name of a method procedure that accepts as a sole argument a variable of the same type as the property. In case of an array property, the procedure must accept two arguments: the first argument must have the same type as the index, the second argument must be of the same type as the property. Similarly, in case of an indexed property, the first parameter must be an integer.

The section (private, published) in which the specified function or procedure resides is irrelevant. Usually, however, this will be a protected or private method.

For example, given the following declaration:

```pascal
Type  
  MyClass = Class  
    Private  
    Field1 : Longint;  
    Field2 : Longint;  
    Field3 : Longint;  
    Procedure  Sety (value : Longint);  
    Function Gety : Longint;  
    Function Getz : Longint;  
    Public  
    Property X : Longint Read Field1 write Field2;  
    Property Y : Longint Read GetY Write Sety;  
    Property Z : Longint Read GetZ;  
    end;  

Var  
  MyClass : TMyClass;

The following are valid statements:
WriteLn ('X : ',MyClass.X);  
WriteLn ('Y : ',MyClass.Y);  
WriteLn ('Z : ',MyClass.Z);  
MyClass.X := 0;  
MyClass.Y := 0;
```

But the following would generate an error:

```pascal
MyClass.Z := 0;
```

because Z is a read-only property.

What happens in the above statements is that when a value needs to be read, the compiler inserts a call to the various getNNN methods of the object, and the result of this call is used. When an assignment is made, the compiler passes the value that must be assigned as a parameter to the various setNNN methods.

Because of this mechanism, properties cannot be passed as var arguments to a function or procedure, since there is no known address of the property (at least, not always).

### 6.7.2 Indexed properties

If the property definition contains an index, then the read and write specifiers must be a function and a procedure. Moreover, these functions require an additional parameter : An integer parameter. This allows to read or write several properties with the same function. For this, the properties must have the same type. The following is an example of a property with an index:

```pascal
{$mode objfpc}  
Type  
  TPoint = Class(TObject)  
  Private  
    FX,FY : Longint;  
    Function GetCoord (Index : Integer): Longint;  
    Procedure SetCoord (Index : Integer; Value : longint);  
  Public  
    Property X : Longint index 1 read GetCoord Write SetCoord;  
    Property Y : Longint index 2 read GetCoord Write SetCoord;  
    Property Coords[Index : Integer]:Longint Read GetCoord;  
  end;  

Procedure TPoint.SetCoord (Index : Integer; Value : Longint);  
begin  
  Case Index of  
   1 : FX := Value;  
   2 : FY := Value;  
  end;  
end;  

Function TPoint.GetCoord (INdex : Integer) : Longint;  
begin  
  Case Index of  
   1 : Result := FX;  
   2 : Result := FY;  
  end;  
end;  

Var  
  P : TPoint;  

begin  
  P := TPoint.create;  
  P.X := 2;  
  P.Y := 3;  
  With P do  
    WriteLn ('X=',X,' Y=',Y);  
end.
```

When the compiler encounters an assignment to X, then SetCoord is called with as first parameter the index (1 in the above case) and with as a second parameter the value to be set. Conversely, when reading the value of X, the compiler calls GetCoord and passes it index 1. Indexes can only be integer values.

### 6.7.3 Array properties

Array properties also exist. These are properties that accept an index, just as an array does. The index can be one-dimensional, or multi-dimensional. In difference with normal (static or dynamic) arrays, the index of an array property doesn’t have to be an ordinal type, but can be any type.

A read specifier for an array property is the name method function that has the same return type as the property type. The function must accept as a sole argument a variable of the same type as the index type. For an array property, one cannot specify fields as read specifiers.

A write specifier for an array property is the name of a method procedure that accepts two arguments: the first argument has the same type as the index, and the second argument is a parameter of the same type as the property type. As an example, see the following declaration:

```pascal
Type  
  TIntList = Class  
  Private  
    Function GetInt (I : Longint) : longint;  
    Function GetAsString (A : String) : String;  
    Procedure SetInt (I : Longint; Value : Longint;);  
    Procedure SetAsString (A : String; Value : String);  
  Public  
    Property Items [i : Longint] : Longint Read GetInt Write SetInt;  
    Property StrItems [S : String] : String Read GetAsString Write SetAsstring;  
  end;  

Var  
  AIntList : TIntList;

Then the following statements would be valid:
AIntList.Items[26] := 1;  
AIntList.StrItems['twenty-five'] := 'zero';  
WriteLn ('Item 26 : ',AIntList.Items[26]);  
WriteLn ('Item 25 : ',AIntList.StrItems['twenty-five']);
```

While the following statements would generate errors:

```pascal
AIntList.Items['twenty-five'] := 1;  
AIntList.StrItems[26] := 'zero';
```

Because the index types are wrong.

Array properties can be multi-dimensional:

```pascal
Type  
  TGrid = Class  
  Private  
    Function GetCell (I,J : Longint) : String;  
    Procedure SetCell (I,J : Longint; Value : String);  
  Public  
    Property Cellcs [Row,Col : Longint] : String Read GetCell  
                                            Write SetCell;  
  end;
```

If there are N dimensions, then the types of the first N arguments of the getter and setter must correspond to the types of the N index specifiers in the array property definition.

### 6.7.4 Default properties

Array properties can be declared as default properties. This means that it is not necessary to specify the property name when assigning or reading it. In the previous example, if the definition of the items property would have been

```pascal
Property Items[i : Longint]: Longint Read GetInt Write SetInt; Default;
```

Then the assignment

```pascal
AIntList.Items[26] := 1;
```

Would be equivalent to the following abbreviation.

```pascal
AIntList[26] := 1;
```

Only one default property per class is allowed, but descendent classes can redeclare the default property.

### 6.7.5 Published properties

Classes compiled in the {$M+} state (such as TPersistent from the classes unit) can have a published section. For methods, fields and properties in the Published section, the compiler generates RTTI information (Run Time Type Information), which can be used to query the defined methods, fields and properties in the published section(s). The typinfo unit contains the necessary routines to query this information, and this unit is used in the streaming system in FPC in the classes unit.

The RTTI is generated regardless of what the read and write specifiers are: fields, functions/procedures or indexed functions/procedures.

Only class-typed fields can be published. For properties, any simple property whose size is less than or equal to a pointer, can be declared published: floats, integers, sets (with less than 32 distinct elements), enumerated, classes or dynamic arrays (not array properties).

Although run-time type information is available for other types, these types cannot be used for a property or field definition in a published section. The information is present to describe for example arguments of procedures or functions.

### 6.7.6 Storage information

The compiler offers two specifiers to control whether a property is streamed using a streaming mechanism, such as the one implemented in the classes unit. These specifiers write extra information to the generated RTTI, which is later used in the streaming system:

- The stored specifier.
- The default specifier and its counterpart nodefault.

These two specifiers are both evaluated to decide whether a property must be streamed.

The argument to the stored specifier should be either a boolean constant, a boolean field of the class, or a parameterless function which returns a boolean result. If the argument of the stored specifier evaluates to False, the property will not be streamed.

Remark Absence of a stored specifier is equivalent to stored True.

If the evaluation of Stored resulted in True, the default for a property is considered:

The default specifier can be specified for ordinal types and sets. Properties that have as value this default value, will not be written to the stream by the streaming system, even if Stored is True. The default value is stored in the RTTI that is generated for the class.

String, floating-point and pointer properties have implicit default value of empty string, 0 or nil, respectively. Ordinal and set properties have no implicit default value.

The nodefault specifier must be used to indicate that a property has no default value. The effect is that the value of this property is always written to the stream when streaming the property, if stored is True.

**Remark**:

- When the class is instantiated, the default value is not automatically applied
  to the property, it is the responsibility of the programmer to do this in the constructor of the class.
- The value -2147483648 cannot be used as a default value, as it is used
  internally to denote nodefault.
- It is not possible to specify a default for array properties.
- It is not possible to specify the Stored directive for array properties.
- All storage specifiers can be overridden in descendent classes.
  
  property Test stored False;

- The nodefault specifier can be used to override the implicit default specifier
  of string, floating-point and pointer properties.
- The streaming mechanism described here is the one implemented in the classes
  unit of the RTL. Other streaming mechanisms can be implemented, and they can use the RTTI information in a different way.

### 6.7.7 Overriding and redeclaring properties

Properties can be both overridden and redeclared in descendent classes.

Property redeclaration takes action if the property type is declared, otherwise it is property override. The only difference is that property override replaces or extends the inherited modifiers with the new modifiers, whereas property redeclaration hides all inherited modifiers that are not present in the redeclaration. The type of the redeclared property does not have to be the same as the parent”s class property type.

The example below demonstrates the difference between property override and redeclaration.

```pascal
type  
  TAncestor = class  
  private  
    FP1 : Integer;  
  public  
    property P: integer Read FP1 write FP1;  
  end;  

  TP1 = class(TAncestor)  
  public  
    // property override  
    property P default 1;  
  end;  

  TPReadOnly = class(TAncestor)  
  public  
    // property redeclaration  
    property P: integer Read FP1;  
  end;
```

TP1 extends property P with a default value, TPReadOnly redeclares property P as read-only.

**Remark**  
TP1 should set the default value of P to 1 in its constructor.

In case of both property redeclaration and property override, the access to the getter and setter is always static. I.e. property override acts only on the RTTI of the object and is not to be confused with method override.

The keyword “inherited” can be used to refer to the parent definition of the property. For example consider the following code:

```pascal
type  
  TAncestor = class  
  private  
    FP1 : Integer;  
  public  
    property P: integer Read FP1 write FP1;  
  end;  

  TClassA = class(TAncestor)  
  private  
    procedure SetP(const AValue: char);  
    function getP : Char;  
  public  
    constructor Create;  
    property P: char Read GetP write SetP;  
  end;  

procedure TClassA.SetP(const AValue: char);  

begin  
  Inherited P:=Ord(AValue);  
end;  

procedure TClassA.GetP : char;  

begin  
  Result:=Char((Inherited P) and $FF);  
end;
```

TClassA redefines P as a character property instead of an integer property, but uses the parent”s P property to store the value.

Care must be taken when using virtual get/set routines for a property: setting the inherited property still observes the normal rules of inheritance for methods. Consider the following example:

```pascal
type  
  TAncestor = class  
  private  
    procedure SetP1(const AValue: integer); virtual;  
  public  
    property P: integer write SetP1;  
  end;  

  TClassA = class(TAncestor)  
  private  
    procedure SetP1(const AValue: integer); override;  
    procedure SetP2(const AValue: char);  
  public  
    constructor Create;  
    property P: char write SetP2;  
  end;  

constructor TClassA.Create;  
begin  
  inherited P:=3;  
end;
```

In this case, when setting the inherited property P, the implementation TClassA.SetP1 will be called, because the SetP1 method is overridden.

If the parent class implementation of SetP1 must be called, then this must be called explicitly:

```pascal
constructor TClassA.Create;  
begin  
  inherited SetP1(3);  
end;
```

The redeclared ancestor properties are also available from inside and outside the descendant object with a direct cast to the ancestor:

```pascal
function GetP(const AClassA: TClassA): Integer;  
begin  
  Result := TAncestor(AClassA).P;  
end;
```

## 6.8 Class properties

Class properties are very much like global property definitions. They are associated with the class, not with an instance of the class.

A consequence of this is that the storage for the property value must be a class var, not a regular field or variable of the class: normal fields or variables are stored in an instance of the class.

Class properties can have a getter and setter method like regular properties, but these must be static methods of the class.

That means that the following contains a valid class property definition:

```pascal
TA = Class(TObject)  
Private  
  class var myprivatea : integer;  
  class Function GetB : Integer;  static;  
  class Procedure SetA(AValue : Integer); static;  
  class Procedure SetB(AValue : Integer); static;  
public  
  Class property MyA : Integer Read MyPrivateA Write SetA;  
  Class property MyA : Integer Read GetB Write SetB;  
end;
```

The reason for the requirement is that a class property is associated to the particular class in which it is defined, but not to descendent classes. Since class methods can be virtual, this would allow descendent classes to override the method, making them unsuitable for class property access.

## 6.9 Nested types, constants and variables

A class definition can contain a type section, const section and a variable section. The type and constant sections act as a regular type section as found in a unit or method/function/procedure implementation. The variables act as regular fields of the class, unless they are in a class var section, in which case they act as if they were defined at the unit level, within the namespace of the class (section 6.3, page 308).

However, the visibility of these sections does play a role: private and protected (strict or not) constants, types and variables can only be used as far as their visibility allows.

Public types can be used outside the class, by their full name:

```pascal
type  
  TA = Class(TObject)  
  Public  
    Type TEnum = (a,b,c);  
    Class Function DoSomething : TEnum;  
  end;  

Class Function TA.DoSomething : TEnum;  

begin  
  Result:=a;  
end;  

var  
  E : TA.TEnum;  

begin  
  E:=TA.DoSomething;  
end.
```

Whereas

```pascal
type  
  TA = Class(TObject)  
  Strict Private  
    Type TEnum = (a,b,c);  
  Public  
    Class Function DoSomething : TEnum;  
  end;  

Class Function TA.DoSomething : TEnum;  

begin  
  Result:=a;  
end;  

var  
  E : TA.TEnum;  

begin  
  E:=TA.DoSomething;  
end.
```

Will not compile and will return an error:

```pascal
tt.pp(20,10) Error: identifier idents no member "TEnum"
```

Note that for writeable constants, the same rules apply as for class variables with regard to scope and overriding in descendents:

```pascal
{$mode delphi}{$J+}  

type  
  TA = class // base type  
    const CODE: integer = 99;  
  end;  
  TB = class(TA);  
  TC = class(TA);  

begin  
  TA.Code:=0;  
  TB.Code:=1;  
  TC.Code:=2;  
  Writeln(Ta.Code:2,Tb.Code:2,Tc.code:2);  
end.
```

Will write

```sh
 2 2 2
```

But

```pascal
{$mode delphi}{$J+}  

type  
  TA = class // base type  
    const CODE: integer = -99;  
  end;  
  TB = class(TA)  
    const code : integer = -98;  
  end;  
  TC = class(TA)  
    Const code : integer = -97;  
  end;  

begin  
  TA.Code:=0;  
  TB.Code:=1;  
  TC.Code:=2;  
  Writeln(Ta.Code:2,Tb.Code:2,Tc.code:2);  
end.
```

Will write

```sh
 0 1 2
```

## 6.15 Pregled vrsta metoda u Classes

Classes nude različite vrste metoda, sličnih onima u Objects, ali sa važnim razlikama:

 Tip metode | Deklaracija | Karakteristike | Kada koristiti |
 ---------- | ----------- | -------------- | -------------- |
 **Static (Normal)** | `procedure Doit;` | Poziv se određuje u vreme kompajliranja. Tip promenljive određuje metodu. Najbrža metoda poziva. Ne može se override-ovati | Kada polimorfizam nije potreban |
 **Virtual** | `procedure Doit; virtual;` | Poziv se određuje u vreme izvršavanja. Koristi VMT. U potomku se koristi `override`. Može se override-ovati | Standardni polimorfni metod |
 **Dynamic** | `procedure Doit; dynamic;` | Isto kao `virtual` u FPC. U Delphi ima drugu implementaciju. U potomku koristi `override` | Za Delphi kompatibilnost |
 **Abstract** | `procedure Doit; virtual; abstract;` | Mora biti `virtual`. Nema implementaciju.Mora se override-ovati u potomku. Ne može se kreirati instanca | Interfejsi/apstraktne bazne klase |
 **Class Method** | `class procedure Doit;` | Self pokazuje na VMT klase.- Pristup class poljima i metodama. Nema pristup običnim poljima. Može biti virtual i override | Fabrike, registracija tipova |
 **Static Class Method** | `class procedure Doit; static;` | Nema Self parametar. Pristup samo class poljima. Ne može biti virtual. Može se dodeliti proc. promenljivoj | Pomoćne funkcije u namespace-u klase |
 **Class Constructor** | `class constructor Create;` | Poziva se pre initialization sekcije. Samo jedan po klasi.- Bez parametara.- Ne može biti virtual | Inicijalizacija class polja |
 **Class Destructor** | `class destructor Destroy;` | Poziva se nakon finalization sekcije.Samo jedan po klasi. Bez parametara. Ne može biti virtual | Čišćenje class resursa |
 **Message Method** | `procedure Handler(var Msg); message 1;` | Automatski virtual. Poziva se preko Dispatch(). Jedan var parametar. Integer ili string ID | GUI callback funkcije |

### 6.15.1 Ključna razlika: Objects vs Classes

Najvažnija razlika u sintaksi virtuelnih metoda:

**U Objects:**

```pascal
Type
  TParent = Object
    procedure Doit; virtual;  
  end;
  
  TChild = Object(TParent)
    procedure Doit; virtual;   // Ponovo 'virtual'!
  end;
```

**U Classes:**

```pascal
Type
  TParent = Class
    procedure Doit; virtual;   
  end;
  
  TChild = Class(TParent)
    procedure Doit; override;  // Koristi 'override'!
  end;
```

Ako u Classes koristiš `virtual` umesto `override`, dobićeš **warning**:

```sh
Warning: An inherited method is hidden by TCHILD.DOIT
```

### 6.15.2 Primer: Razlika između različitih tipova metoda

```pascal
{$mode objfpc}
{$h+}
program ClassMethodsDemo;

type
  TExample = class
  private
    FValue: Integer;
    class var FClassValue: Integer;
  public
    // Normal method - ima Self, pristup svemu
    procedure NormalMethod;
    
    // Virtual method - može se override-ovati
    procedure VirtualMethod; virtual;
    
    // Abstract method - mora se implementirati u potomku
    procedure AbstractMethod; virtual; abstract;
    
    // Class method - Self pokazuje na VMT
    class procedure ClassMethod;
    
    // Static class method - nema Self
    class procedure StaticClassMethod; static;
    
    // Class constructor - poziva se automatski
    class constructor Create;
    
    // Class destructor - poziva se automatski  
    class destructor Destroy;
    
    // Message method - za callback funkcije
    procedure MessageHandler(var Msg); message 100;
  end;

procedure TExample.NormalMethod;
begin
  FValue := 10;                    // OK - pristup instance polju
  FClassValue := 20;               // OK - pristup class polju
  WriteLn('Normal: ', ClassName);  // OK - pristup class metodi
end;

procedure TExample.VirtualMethod;
begin
  WriteLn('Virtual method called');
end;

class procedure TExample.ClassMethod;
begin
  // FValue := 10;                 // GREŠKA - nema pristup instance poljima
  FClassValue := 30;               // OK - pristup class polju
  WriteLn('Class: ', ClassName);   // OK
end;

class procedure TExample.StaticClassMethod;
begin
  // FValue := 10;                 // GREŠKA
  FClassValue := 40;               // OK
  WriteLn('Static class method');  // OK
  // ClassName se tretira kao TExample.ClassName
end;

class constructor TExample.Create;
begin
  FClassValue := 0;
  WriteLn('Class constructor called');
end;

class destructor TExample.Destroy;
begin
  WriteLn('Class destructor called');
end;

procedure TExample.MessageHandler(var Msg);
begin
  WriteLn('Message handler called');
end;

// Potomak koji override-uje virtual metod
type
  TChild = class(TExample)
    procedure VirtualMethod; override;  // MORA biti 'override'!
    procedure AbstractMethod; override; // Implementacija abstract metoda
  end;

procedure TChild.VirtualMethod;
begin
  inherited;  // Pozovi roditeljski metod
  WriteLn('Child virtual method');
end;

procedure TChild.AbstractMethod;
begin
  WriteLn('Child abstract implementation');
end;

var
  Parent: TExample;
  Child: TChild;
  
begin
  // Class constructor je već pozvan ovde!
  
  Parent := TExample.Create;
  Child := TChild.Create;
  
  Parent.NormalMethod;
  Parent.VirtualMethod;
  
  Child.VirtualMethod;  // Poziva TChild.VirtualMethod
  Child.AbstractMethod;
  
  TExample.ClassMethod;
  TExample.StaticClassMethod;
  
  Parent.Free;
  Child.Free;
  
  // Class destructor će biti pozvan na kraju
end.
```

Output:

```sh
Class constructor called
Normal: TExample
Virtual method called
Virtual method called
Child virtual method
Child abstract implementation
Class: TExample
Static class method
Class destructor called
```

### 6.15.3 Kada koristiti koju vrstu metoda

 Situacija | Preporučena vrsta |
 ---------- | ----------------- |
 Običan metod bez nasleđivanja | Normal (static) |
 Polimorfni metod koji se override-uje | Virtual + override |
 Interfejs koji potomci moraju implementirati | Abstract |
 Metod koji treba pozivati bez instance | Class method |
 Pomoćna funkcija u namespace-u klase | Static class method |
 Inicijalizacija class polja | Class constructor |
 GUI event handler | Message method |
 Factory pattern | Class method (virtual) |

### 6.15.4 Česte greške i kako ih izbegavati

**1. Zaboravljen override:**

```pascal
// LOŠE - kompajler će dati warning
type
  TChild = class(TParent)
    procedure MyProc; virtual;  // Trebalo je: override
  end;
```

**2. Pokušaj pristupa instance poljima iz class metoda:**

```pascal
class procedure TExample.ClassMethod;
begin
  FValue := 10;  // GREŠKA - FValue je instance polje
end;
```

**3. Pozivanje abstract metoda:**

```pascal
var
  Obj: TAbstractClass;
begin
  Obj := TAbstractClass.Create;  // GREŠKA - ne može se kreirati
end;
```

**4. Zaboravljen inherited u destruktoru:**

```pascal
destructor TChild.Destroy;
begin
  // cleanup...
  inherited;  // MORA biti na kraju!
end;
```

### 6.15.5 Zašto Classes, a ne Objects?

 Prednost Classes | Objašnjenje |
 ----------------- | ----------- |
 **Modernija sintaksa** | `override` umesto ponovnog `virtual` |
 **Exception safety** | Automatski poziv destruktora pri greški |
 **Reference counting** | Podrška za automatsko upravljanje memorijom |
 **Interfejsi** | Potpuna podrška za interfejse |
 **RTTI** | Bolja run-time type information |
 **Properties** | Naprednije property funkcionalnosti |
 **Message methods** | Za GUI programiranje |
 **Class methods** | Moćniji class-level metodi |
 **Delphi kompatibilnost** | Lakša portabilnost koda |

**Zaključak:** Za novi kod, **koristi Classes**. Objects su tu za legacy kod ili specifične low-level slučajeve.

Detaljnije o Objects-ima vidi u [05_objects.md](05_objects.md).

[prev][f1] [content][f0] [next][f2]

[f1]: 05_objects.md
[f0]: 00_contents.md
[f2]: 07_.md
