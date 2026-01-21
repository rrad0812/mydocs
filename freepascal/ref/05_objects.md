# 5 Objects

[prev][f1] [content][f0] [next][f2]

## 5.1 Declaration

Free Pascal supports object oriented programming. In fact, most of the compiler is written using objects. Here we present some technical questions regarding object oriented programming in Free Pascal.

Objects should be treated as a special kind of record. The record contains all the fields that are declared in the objects definition, and pointers to the methods that are associated to the objects’ type.

An object is declared just as a record would be declared; except that now, procedures and functions can be declared as if they were part of the record. Objects can “inherit” fields and methods from “parent” objects. This means that these fields and methods can be used as if they were included in the objects declared as a “child” object.

Furthermore, a concept of visibility is introduced: fields, procedures and functions can be declared as public, protected or private. By default, fields and methods are public, and are exported outside the current unit.

Fields or methods that are declared private are only accessible in the current unit: their scope is limited to the implementation of the current unit.

As can be seen, any visibility block can be specified multiple times, as often as needed..

The following is a valid definition of an object:

```pascal
Type  
  TObj = object  
  Private  
    Caption : ShortString;  
  Public  
    Constructor init;  
    Destructor done;  
    Procedure SetCaption (AValue : String);  
    Function GetCaption : String;  
  end;
```

It contains a constructor/destructor pair, and a method to get and set a caption. The Caption field is private to the object: it cannot be accessed outside the unit in which TObj is declared.

Remark In MacPas mode, the Object keyword is replaced by the class keyword for compatibility with other pascal compilers available on the Mac. That means that objects cannot be used in MacPas mode.

**Remark**  
Free Pascal also supports the `packed` object. This is the same as an object, only the elements (fields) of the object are byte-aligned, just as in the packed record. The declaration of a packed object is similar to the declaration of a packed record:

```pascal
Type  
  TObj = packed object  
   Constructor init;  
   ...  
   end;  
  Pobj = ^TObj;  
Var PP : Pobj;
```

Similarly, the {$PackRecords } directive acts on objects as well.

## 5.2 Abstract and sealed objects

An object can be declared as sealed. In that case, it is not possible to declare a descendent object. The compiler will return an error if it encounters a declaration of a descendent:

```pascal
Type  
  TMyClass = object Sealed  
    x : integer;  
  end;  

  TMyClass2 = object(TMyClass)  
    Y : Integer;  
  end;  

begin  
end.
```

This will result in the following error:

```sh
Error: Cannot create a descendant of the sealed class "TMyClass"
```

An abstract class is a class that cannot be used directly. Instead, a descendent class must always be used. However, for Delphi compatibility, the compiler ignores this directive.

## 5.3 Fields

Object Fields are like record fields. They are accessed in the same way as a record field would be accessed: by using a qualified identifier. Given the following declaration:

```pascal
Type TAnObject = Object  
       AField : Longint;  
       Procedure AMethod;  
       end;  
Var AnObject : TAnObject;
```

then the following would be a valid assignment:

```pascal
  AnObject.AField := 0;
```

Inside methods, fields can be accessed using the short identifier:

```pascal
Procedure TAnObject.AMethod;  
begin  
  ...  
  AField := 0;  
  ...  
end;
```

Or, one can use the self identifier. The self identifier refers to the current instance of the object:

```pascal
Procedure TAnObject.AMethod;  
begin  
  ...  
  Self.AField := 0;  
  ...  
end;
```

One cannot access fields that are in a private or protected sections of an object from outside the objects’ methods. If this is attempted anyway, the compiler will complain about an unknown identifier.

It is also possible to use the with statement with an object instance, just as with a record:

```pascal
With AnObject do  
  begin  
  Afield := 12;  
  AMethod;  
  end;
```

In this example, between the begin and end, it is as if AnObject was prepended to the Afield and Amethod identifiers. More about this in section 13.2.8, page 725.

## 5.4 Class or Static fields

An object can contain class or static fields: these fields are global to the object type, and act like global variables, but are known only in the scope of the object. The difference between static and class variables is purely the mode in which they work: The static keyword will always work, the class keyword will need `{$MODE DELPHI}` or `{$MODE OBJFPC}`.

They can be referenced from within the objects methods, but can also be referenced from outside the object by providing the fully qualified name.

For instance, the output of the following program:

```pascal
{$mode objfpc}  
type  
  cl=object  
    l : longint; static;  
    class var v : integer;  
  end;  

var  
  cl1,cl2 : cl;  

begin  
  Writeln('Static');  
  cl1.l:=2;  
  writeln(cl2.l);  
  cl2.l:=3;  
  writeln(cl1.l);  
  Writeln(cl.l);  
  Writeln('Class');  
  cl1.v:=4;  
  writeln(cl2.v);  
  cl2.v:=5;  
  writeln(cl1.v);  
  Writeln(cl.v);  
end.
```

will be the following

```pascal
Static  
2  
3  
3  
Class  
4  
5  
5
```

Note that the last line of code references the object type itself (cl), and not an instance of the object (cl1 or cl2).

## 5.5 Constructors and destructors

As can be seen in the syntax diagram for an object declaration, Free Pascal supports constructors and destructors. The programmer is responsible for calling the constructor and the destructor explicitly when using objects.

A constructor/destructor pair is required if the object uses virtual methods. The reason is that for an object with virtual methods, some internal housekeeping must be done: this housekeeping is done by the constructor1.

In the declaration of the object type, a simple identifier should be used for the name of the constructor or destructor. When the constructor or destructor is implemented, a qualified method identifier should be used, i. e. an identifier of the form objectidentifier.methodidentifier.

Free Pascal supports also the extended syntax of the New and Dispose procedures. In case a dynamic variable of an object type must be allocated the constructor’s name can be specified in the call to New. The New is implemented as a function which returns a pointer to the instantiated object. Consider the following declarations:

```pascal
Type  
  TObj = object;  
   Constructor init;  
   ...  
   end;  
  Pobj = ^TObj;  
Var PP : Pobj;
```

Then the following three calls are equivalent:

```pascal
 pp := new (Pobj,Init);
```

and

```pascal
  new(pp,init);
```

and also

```pascal
  new (pp);  
  pp^.init;
```

In the last case, the compiler will issue a warning that the extended syntax of new and dispose must be used to generate instances of an object. It is possible to ignore this warning, but it’s better programming practice to use the extended syntax to create instances of an object. Similarly, the Dispose procedure accepts the name of a destructor. The destructor will then be called, before removing the object from the heap.

In view of the compiler warning remark, the following chapter presents the Delphi approach to object-oriented programming, and may be considered a more natural way of object-oriented programming.

## 5.6 Methods

Object methods are just like ordinary procedures or functions, only they have an implicit extra parameter: self. Self points to the object with which the method was invoked. When implementing methods, the fully qualified identifier must be given in the function header. When declaring methods, a normal identifier must be given.

### 5.6.1 Declaration

The declaration of a method is much like a normal function or procedure declaration, with some additional specifiers, as can be seen from the following diagram, which is part of the object declaration:

from the point of view of declarations, Method definitions are normal function or procedure declarations. Contrary to TP and Delphi, fields can be declared after methods in the same block, i. e. the following will generate an error when compiling with Delphi or Turbo Pascal, but not with FPC:

```pascal
Type  
  MyObj = Object  
    Field : Longint;  
    Procedure Doit;  
  end;
```

### 5.6.2 Method invocation

Methods are called just as normal procedures are called, only they have an object instance identifier prepended to them (see also chapter 13, page 668). To determine which method is called, it is necessary to know the type of the method. We treat the different types in what follows.

#### 5.6.2.1 Normal ( static ) methods

Normal (static) methods are methods that have been declared without a `abstract` or `virtual` keyword. When calling a static method, the declared (i. e. compile time) method of the object is used. For example, consider the following declarations:

```pascal
Type  
  TParent = Object  
    ...  
    procedure Doit;  
    ...  
    end;  
  PParent = ^TParent;  
  
  TChild = Object(TParent)  
    ...  
    procedure Doit;  
    ...  
    end;  
  PChild = ^TChild;
```

As it is visible, both the parent and child objects have a method called `Doit`. Consider now the following declarations and calls:

```pascal
Var  
  ParentA,ParentB : PParent;  
  Child           : PChild;  
 
begin  
   ParentA := New(PParent,Init);  
   ParentB := New(PChild,Init);  
   Child := New(PChild,Init);  
   ParentA^.Doit;  
   ParentB^.Doit;  
   Child^.Doit;
```

Of the three invocations of `Doit`, only the last one will call `TChild.Doit`, the other two calls will call `TParent.Doit`. This is because for `static` methods, the compiler determines at compile time which method should be called. Since `ParentB` is of type `TParent`, the compiler decides that it must be called with `TParent.Doit`, even though it will be created as a TChild. There may be times when the method that is actually called should depend on the actual type of the object at run-time. If so, the method cannot be a `static` method, but must be a `virtual` method.

#### 5.6.2.2 Virtual methods

To remedy the situation in the previous section, `virtual` methods are created. This is simply done by appending the method declaration with the `virtual` modifier. The descendent object can then override the method with a new implementation by re-declaring the method (with the same parameter list) using the `virtual` keyword.

Going back to the previous example, consider the following alternative declaration:

```pascal
Type  
  TParent = Object  
    ...  
    procedure Doit;virtual;  
    ...  
    end;  
  PParent = ^TParent;  
  
  TChild = Object(TParent)  
    ...  
    procedure Doit;virtual;  
    ...  
    end;  
  PChild = ^TChild;
```

As it is visible, both the parent and child objects have a method called Doit. Consider now the following declarations and calls:

```pascal
Var  
  ParentA,ParentB : PParent;  
  Child           : PChild;  
 
begin  
   ParentA := New(PParent,Init);  
   ParentB := New(PChild,Init);  
   Child := New(PChild,Init);  
   ParentA^.Doit;  
   ParentB^.Doit;  
   Child^.Doit;
```

Now, different methods will be called, depending on the actual run-time type of the object. For ParentA, nothing changes, since it is created as a TParent instance. For Child, the situation also doesn’t change: it is again created as an instance of TChild.

For ParentB however, the situation does change: Even though it was declared as a TParent, it is created as an instance of TChild. Now, when the program runs, before calling Doit, the program checks what the actual type of ParentB is, and only then decides which method must be called. Seeing that ParentB is of type TChild, TChild.Doit will be called. The code for this run-time checking of the actual type of an object is inserted by the compiler at compile time.

The `TChild.Doit` is said to override the TParent.Doit. It is possible to access the `TParent.Doit` from within the `TChild.Doit`, with the `inherited` keyword:

```pascal
Procedure TChild.Doit;  
begin  
  inherited Doit;  
  ...  
end;
```

In the above example, when `TChild.Doit` is called, the first thing it does is call `TParent.Doit`. The `inherited` keyword cannot be used in `static` methods, only on `virtual methods.

To be able to do this, the compiler keeps – per object type – a table with `virtual` methods: the VMT (Virtual Method Table). This is simply a table with pointers to each of the virtual methods: each virtual method has its fixed location in this table (an index). The compiler uses this table to look up the actual method that must be used. When a descendent object overrides a method, the entry of the parent method is overwritten in the VMT. More information about the VMT can be found in Programmer’s Guide.

As remarked earlier, objects that have a VMT must be initialized with a constructor: the object variable must be initialized with a pointer to the VMT of the actual type that it was created with.

#### 5.6.2.3 Abstract methods

An abstract method is a special kind of virtual method. A method that is declared abstract does not have an implementation for this method. It is up to inherited objects to override and implement this method.

From this it follows that a method can not be abstract if it is not virtual (this can be seen from the syntax diagram). A second consequence is that an instance of an object that has an abstract method cannot be created directly.

The reason is obvious: there is no method where the compiler could jump to! A method that is declared abstract does not have an implementation for this method. It is up to inherited objects to override and implement this method. Continuing our example, take a look at this:

```pascal
Type  
  TParent = Object  
    ...  
    procedure Doit;virtual;abstract;  
    ...  
    end;  
  PParent=^TParent;  
  TChild = Object(TParent)  
    ...  
    procedure Doit;virtual;  
    ...  
    end;  
  PChild = ^TChild;
```

As it is visible, both the parent and child objects have a method called Doit. Consider now the following declarations and calls:

```pascal
Var  
  ParentA,ParentB : PParent;  
  Child           : PChild;  

begin  
   ParentA := New(PParent,Init);  
   ParentB := New(PChild,Init);  
   Child := New(PChild,Init);  
   ParentA^.Doit;  
   ParentB^.Doit;  
   Child^.Doit;
```

First of all, Line 3 will generate a compiler error, stating that one cannot generate instances of objects with abstract methods: The compiler has detected that PParent points to an object which has an abstract method. Commenting line 3 would allow compilation of the program.

Remark If an abstract method is overridden, the parent method cannot be called with inherited, since there is no parent method; The compiler will detect this, and complain about it, like this:
testo.pp(32,3) Error: Abstract methods can't be called directly

If, through some mechanism, an abstract method is called at run-time, then a run-time error will occur. (run-time error 211, to be precise)

#### 5.6.2.4 Class or static methods

Class methods or methods declared with the static directive are methods that are global to the object type. When called, the implicit “self” pointer is not available. This means that normal methods cannot be called, and none of the fields of an object can be accessed. Class variables can be used, however.

Class or static methods are regular methods, they can be assigned to a procedural variable.

The following program demonstrates all this. The commented-out statements will not compile.

```pascal
{$APPTYPE CONSOLE}  
{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}  
type  
  TTest = object  
    const Epsylon = 100;  
    var f : integer;  
    class var cv1,cv2:integer;  
    procedure myproc;  
    class procedure testproc;  
    class procedure testproc2;static;  
    procedure testproc3; static;  
  end;  

  procedure TTest.myproc;  
  begin  
    cv1:=0;  
    f:=1;  
  end;  

  class procedure TTest.Testproc;  
  begin  
    cv1:=1;  
    // f:=1;  
  end;  

  class procedure TTest.Testproc2;  
  begin  `
    cv1:=2;  
    // f:=1;  
  end;  

  procedure TTest.Testproc3;  
  begin  
    cv1:=3;  
    // f:=1;  
  end;  

Var  
  P : Procedure;  

begin  
  P:=@TTest.Myproc;  
  P:=@TTest.Testproc;  
  P:=@TTest.Testproc2;  
  P:=@TTest.Testproc3;  
end.
```

Uncommenting one of the commented statements and trying to compile the resulting code will result in a compiler error:

```sh
ocv.pp(32,6) Error: Only class methods, class properties and  
  class variables can be accessed in class methods
```

## 5.7 Pregled vrsta metoda

Tabela ispod sumira sve vrste metoda koje su dostupne u **Object** tipu podataka:

 Tip metode | Deklaracija | Karakteristike | Kada koristiti |
 --------- | ----------- | -------------- | -------------- |
 **Static (Normal)** | `procedure Doit;` | Poziv se određuje u vreme kompajliranja. Tip promenljive određuje koja metoda se poziva. Najbrža metoda poziva. Ne može se override-ovati. Standardni metodi bez potrebe za polimorfizmom. | |
 **Virtual** | `procedure Doit; virtual;` | Poziv se određuje u vreme izvršavanja. Koristi VMT (Virtual Method Table). Može se nadjačati u potomcima. U potomku takođe koristi `virtual` (ne `override`!) | Kada je potreban polimorfizam |
 **Abstract** | `procedure Doit; virtual; abstract;` | Mora biti `virtual`. Nema implementaciju.  Mora se implementirati u potomku. Ne može se kreirati instanca objekta sa abstract metodama | Interfejsi, šablonski obrazac |
 **Class/Static** | `class procedure Doit`; ili `procedure Doit; static;` | Nema pristup `Self`. Nema pristup običnim poljima. Ima pristup class poljima. Može se dodeli proceduralnoj promenljivoj | Pomoćne funkcije bez potrebe za instancom |

### 5.7.1 Važne razlike u odnosu na C++ i Classes

#### Virtuelne metode u Objects vs Classes

U **Objects** (trenutni tip):

```pascal
Type
  TParent = Object
    procedure Doit; virtual;  // U roditeljskoj klasi
  end;
  
  TChild = Object(TParent)
    procedure Doit; virtual;  // U potomku TAKOĐE virtual!
  end;
```

U **Classes** (pogledaj [06_classes.md](06_classes.md)):

```pascal
Type
  TParent = Class
    procedure Doit; virtual;   // U roditeljskoj klasi
  end;
  
  TChild = Class(TParent)
    procedure Doit; override;  // U potomku override!
  end;
```

#### Ključne razlike

  Aspekt | Objects | Classes |
 ------- | ------- | ------- |
 **Alokacija** | Može se na steku ili heap-u | Uvek na heap-u (sa `Create`) |
 **Inicijalizacija** | `New(P, Constructor)` | `P := TClass.Create` |
 **Destrukcija** | `Dispose(P, Destructor)` | `P.Free` ili `P.Destroy` |
 **Override sintaksa** | `virtual` u oba | `virtual` → `override` |
 **Nasleđivanje** | Prototipsko | Klasično OOP |
 **Automatsko čišćenje** | Ne | Da (sa `try-finally`) |
 **RTTI** | Ograničeno | Potpuno |
 **Interfejsi** | Ne | Da |
 **Property** | Ograničeno | Potpuno |

### 5.7.2 Pojašnjenje: Static (Normal) vs Class/Static

**VAŽNO: Terminološka zabuna!**

**"Static (Normal)"** metod i **"Class/Static"** metod su **potpuno različite stvari**:

  Karakteristika | Static (Normal) metod | Class/Static metod |
  -------------- | --------------------- | ------------------ |
  **Druga imena** | Obični metod, Regular metod | Metod klase |
  **Self pokazivač** | ✅ DA (pokazuje na **instancu**) | ❌ NE (ili pokazuje na **VMT**) |
  **Pristup poljima instance** | ✅ DA | ❌ NE |
  **Pristup class poljima** | ✅ DA | ✅ DA |
  **"Static" znači** | Poziv određen u vreme **kompajliranja** | Pripada **klasi**, ne instanci |
  **Deklaracija** | `procedure Doit;` | `class procedure Doit;` ili `procedure Doit; static;` |

**Primer razlike:**

```pascal
type
  TExample = object
    FValue: Integer;                 // Obično polje (instance)
    class var FClassValue: Integer;  // Class polje
    
    procedure NormalMethod;          // Static(Normal) - ima Self
    class procedure ClassMethod; static;  // Class/Static - nema Self
  end;

procedure TExample.NormalMethod;
begin
  FValue := 10;          // ✅ OK - pristupa polju instance
  FClassValue := 20;     // ✅ OK - pristupa class polju
  WriteLn(Self.FValue);  // ✅ OK - Self postoji
end;

class procedure TExample.ClassMethod;
begin
  // FValue := 10;       // ❌ GREŠKA - nema pristup instance poljima!
  FClassValue := 30;     // ✅ OK - pristup class polju
  // WriteLn(Self.FValue); // ❌ GREŠKA - Self ne postoji!
end;
```

### 5.7.3 Kako rade Static (Normal) metode u nasleđivanju

**Ključno razumevanje**: TIP PROMENLJIVE određuje koja se metoda poziva, NE stvarni tip objekta!

**Radi kao u Pythonu** - poziv se određuje u vreme kompajliranja:

```pascal
program StaticInheritanceDemo;

type
  TAnimal = object
    procedure Speak; virtual;        // Virtual - biće override-ovan
    procedure Identify;              // Static - NEĆE biti override-ovan
  end;
  AnimalPointer: ^TAnimal;

  TDog = object(TAnimal)
    procedure Speak; virtual;        // Override virtual metoda
    procedure Identify;              // Ovo je NOVA metoda, ne override!
  end;
  DogPointer: ^TDog;

procedure TAnimal.Speak;
begin
  WriteLn('Some animal sound');
end;

procedure TAnimal.Identify;
begin
  WriteLn('I am an animal');
end;

procedure TDog.Speak;
begin
  WriteLn('Woof!');
end;

procedure TDog.Identify;
begin
  WriteLn('I am a dog');
end;

begin
  // Kreiramo TDog instancu
  New(DogPointer, Create);
  
  // Dečiji pokazivač dodelimo roditeljskom pokazivaču
  AnimalPointer := DogPointer;  // ✅ Dozvoljeno - TDog je TAnimal
  
  WriteLn('=== Poziv preko AnimalPointer (tip ^TAnimal) ===');
  AnimalPointer^.Speak;      // Output: "Woof!"
                             // ✅ Virtual - gleda STVARNI tip (TDog)
  
  AnimalPointer^.Identify;   // Output: "I am an animal"
                             // ❌ Static - gleda TIP PROMENLJIVE (TAnimal)
  
  WriteLn('=== Poziv preko DogPointer (tip ^TDog) ===');
  DogPointer^.Speak;         // Output: "Woof!"
  DogPointer^.Identify;      // Output: "I am a dog"
  
  Dispose(DogPointer, Destroy);
end.
```

**Output:**

```sh
=== Poziv preko AnimalPointer (tip ^TAnimal) ===
Woof!
I am an animal
=== Poziv preko DogPointer (tip ^TDog) ===
Woof!
I am a dog
```

**Objašnjenje:**

- `AnimalPointer` je **tipa** `^TAnimal`, ali pokazuje na **instancu** `TDog`
- Za **virtual** metode: koristi VMT → poziva `TDog.Speak` ✅
- Za **static** metode: koristi tip promenljive → poziva `TAnimal.Identify` ❌

Ovo je **kao u Pythonu**, ali u Pascalu se to zove "static binding" vs "dynamic binding".

### 5.7.4 Virtual metode mogu (ali ne moraju) biti override-ovane

Virtual metod u roditelju **IMA implementaciju**. Potomak može:

1. **Override-ovati potpuno** (zameniti kod):

```pascal
type
  TParent = object
    procedure DoWork; virtual;
  end;
  
  TChild = object(TParent)
    procedure DoWork; virtual;  // Potpuno nova implementacija
  end;

procedure TParent.DoWork;
begin
  WriteLn('Parent doing work');
end;

procedure TChild.DoWork;
begin
  WriteLn('Child doing work differently');  // Bez inherited
end;
```

2. **Override-ovati i ulančati** (dodati kod):

```pascal
procedure TChild.DoWork;
begin
  inherited DoWork;  // Pozovi roditeljski kod prvo
  WriteLn('Child adds extra work');
end;
```

3. **Ne override-ovati** (nasleđuje roditeljski kod):

```pascal
type
  TChild = object(TParent)
    // procedure DoWork;  // Ne deklarišemo - koristi roditeljski
  end;

var
  C: ^TChild;
begin
  New(C, Create);
  C^.DoWork;  // Poziva TParent.DoWork
end;
```

### 5.7.5 RTTI = Run-Time Type Information

RTTI omogućava proveru i manipulaciju tipova u vreme izvršavanja:

```pascal
type
  TAnimal = object
    constructor Create;
    procedure Speak; virtual;
  end;
  
  TDog = object(TAnimal)
    procedure Speak; virtual;
  end;

var
  Animal: ^TAnimal;
begin
  Animal := New(PDog, Create);
  
  // RTTI provere:
  if Animal is TDog then           // Provera tipa
    WriteLn('This is a dog!');
    
  // TypeInfo,ClassName i slično...
end;
```

**RTTI u Objects vs Classes:**

- **Objects**: Ograničen RTTI (samo osnovne provere)
- **Classes**: Pun RTTI (streaming, reflection, property inspection)

### 5.7.6 Pregled: Kako se pozivi metoda određuju

 Tip metoda | Određivanje poziva | Vreme određivanja | Primer |
 ---------- | ------------------ | ----------------- | ------- |
 **Static (Normal)** | Po **tipu promenljive** | **Compile-time** | `AnimalPtr^.Method` → `TAnimal.Method` |
 **Virtual** | Po **stvarnom tipu objekta** | **Run-time** (VMT) | `AnimalPtr^.Method` → `TDog.Method` |
 **Class/Static** | Po **klasi** (ne instanci) | **Compile-time** | `TAnimal.ClassMethod` |

**Analogija:**

- **Static metod** = telefonski broj "zakucan" u kod
- **Virtual metod** = telefonski imenik koji se konsultuje u runtime
- **Class metod** = poziv koji ne zahteva instancu

### 5.7.7 Praktični savet: Kada koristiti šta

  Scenario | Preporuka | Razlog |
  -------- | --------- | ------- |
 Metod se nikad ne menja | Static (Normal) | Brži, jednostavniji |
 Metod se override-uje u potomcima | Virtual | Polimorfizam |
 Metod bez potrebe za instancom | Class/Static | Ne zahteva objekat |
 Treba pristup poljima | Static (Normal) | Ima Self |
 Interfejs koji potomci implementiraju | Abstract | Forsira implementaciju |

### 5.7.8 Kompletan primer: Static vs Virtual

```pascal
program CompleteDemo;

type
  TAnimal = object
    procedure Speak; virtual;        // Virtual - dynamic binding
    procedure Identify;              // Static - static binding
  end;

  TDog = object(TAnimal)
    procedure Speak; virtual;        
    procedure Identify;              
  end;

procedure TAnimal.Speak;
begin
  WriteLn('Some animal sound');
end;

procedure TAnimal.Identify;
begin
  WriteLn('I am an animal');
end;

procedure TDog.Speak;
begin
  WriteLn('Woof!');
end;

procedure TDog.Identify;
begin
  WriteLn('I am a dog');
end;

var
  AnimalPointer: ^TAnimal;
  DogPointer: ^TDog;
  
begin
  New(DogPointer, Create);
  AnimalPointer := DogPointer;  // Dečiji → roditeljski pokazivač
  
  WriteLn('Poziv preko AnimalPointer (tip ^TAnimal):');
  AnimalPointer^.Speak;      // "Woof!" - virtual gleda stvarni tip
  AnimalPointer^.Identify;   // "I am an animal" - static gleda tip promenljive
  
  WriteLn('Poziv preko DogPointer (tip ^TDog):');
  DogPointer^.Speak;         // "Woof!"
  DogPointer^.Identify;      // "I am a dog"
  
  Dispose(DogPointer, Destroy);
end.
```

### 5.7.9 Preporuka

Za **novi kod**, razmotri korišćenje **Classes** umesto **Objects**:

- Sintaksa bliža modernom OOP-u (Delphi, C++, Java)
- `override` direktiva je eksplicitnija i bezbednija
- Bolja podrška za RTTI i interfejse
- Kompatibilno sa Delphi kodom

Vidi detaljnije u [Classes (06_classes.md)](06_classes.md).

**Objects** se i dalje koriste kada je:

- Potrebna alociranje na steku (bolje performanse)
- Potrebna kompatibilnost sa starijim kodom
- Rad sa low-level sistemskim kodom

[prev][f1] [content][f0] [next][f2]

[f1]: 04_variables.md
[f0]: 00_contents.md
[f2]: 06_classes.md
