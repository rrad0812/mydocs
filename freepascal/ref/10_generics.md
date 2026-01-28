
# 8 Generics

[prev][f1] [content][f0] [next][f2]

## 8.1 Introduction

Generics are templates for generating other types. These can be classes, objects, interfaces and even functions, arrays, records. It is a concept that comes from C++, where it is deeply integrated in the language. As of version 2.2, Free Pascal also officially has support for templates or Generics. They are implemented as a kind of macro which is stored in the unit files that the compiler generates, and which is replayed as soon as a generic class is specialized.

Creating and using generics is a 2-phase process.

- The definition of the generic is defined as a new type: this is a code
  template, a macro which can be replayed by the compiler at a later stage.
- A generic type is specialized: this defines a second type, which is a specific
  implementation of the generic type: the compiler replays the macro which was stored when the generic type was defined.

There are several units distributed with free pascal that implement generic containers and classes. For example the fgl unit.

## 8.2 Generic type definition

A generic type definition is much like a type definition, with the exception that it contains a list of placeholders for types.

For classes, objects, procedural types and extended records, the generic type declaration should be followed by a type implementation. It is the same as a normal class implementation with a single exception, namely that any identifier with the same name as one of the template identifiers must be a type identifier.

So, the generic type declaration is much like a normal type declaration, except that there is an as yet unknown type present. The unknown types are listed in the placeholder list, and they are unknown until the class is specialized.

The following is a valid generic class definition:

```pascal
Type  
  generic TList<_T>=class(TObject)  
  Public  
    type  
       TCompareFunc = function(const Item1, Item2: _T): Integer;  
    var  
      data : _T;  
    procedure Add(item: _T);  
    procedure Sort(compare: TCompareFunc);  
  end;
```

This class could be followed by an implementation as follows:

```pascal
procedure TList.Add(item: _T);  
begin  
  data:=item;  
end;  
 
procedure TList.Sort(compare: TCompareFunc);  
begin  
  if compare(data, 20) <= 0 then  
    halt(1);  
end;
```

There are some noteworthy things about this declaration and implementation:

- There is a single placeholder `_T`. It will be substituted by a type identifier
  when the generic class is specialized. The identifier `_T` may not be used for anything else than a type placeholder. This means that the following would be invalid:
  
  ```pascal
  procedure TList.Sort(compare: TCompareFunc);  

  Var  
    _t : integer;  
   
  begin  
    // do something.  
  end;
  ```

  The local type block contains a single type TCompareFunc. Note that the actual type is not yet known inside the generic class definition: the definition contains a reference to the placeholder _T. All other identifier references must be known when the generic class is defined, not when the generic class is specialized.  
  
  The local variable block is equivalent to the following:
  
  ```pascal
  generic TList<_T>=class(TObject)  
    Public  
      type  
         TCompareFunc = function(const Item1, Item2: _T): Integer;  
    Public  
      data : _T;  
      procedure Add(item: _T);  
      procedure Sort(compare: TCompareFunc);  
    end;
  ```

Note that the type parameter names in declaration and implementation must be the same.

Not only generic classes can be defined, but also other types:

```pascal
{$mode objfpc}  
{$INTERFACES CORBA}  

type  

  generic PlanarCoordinate<t> = record  
    x,y : t;  
  end;  
 
  TScreenCoordinate = specialize PLanarCoordinate<word>;  
  TDiscreteCoordinate = specialize PlanarCoordinate<integer>;  
  TRealCoordinate = specialize PlanarCoordinate<extended>;  
 
  generic TDistanceFunction<t> = function (x,y : t) : Extended of object;  
 
  TScreenDistance = specialize TDistanceFunction<word>;  
  TDiscreteDistance = specialize TDistanceFunction<integer>;  
  TRealDistance = specialize TDistanceFunction<Extended>;  
 
  generic TArray<t> = array of t;  
 
  TMyIntegerArray = specialize TArray<integer>;  
 
  generic IList<_T> = Interface  
    Function GetItem(AIndex : Integer) : _T;  
    Procedure SetItem(AIndex : Integer; AValue : _T);  
    Function GetCount : Integer;  
    Property Items [AIndex : Integer] : _T Read GetItem Write SetItem;  
    Property Count : Integer Read GetCount;  
  end;  
 
  generic TList<_T>=class(TObject, specialize IList<_T>)  
  public type  
    TCompareFunc = function(const Item1, Item2: _T): Integer;  
    Function GetItem(AIndex : Integer) : _T;  
    Procedure SetItem(AIndex : Integer; AValue : _T);  
    Function GetCount : Integer;  
  Public  
     data : _T;  
     procedure Add(item: _T);  
     procedure Sort(compare: TCompareFunc);  
  end;  
 
  generic TPointSet<t> = array of specialize PlanarCoordinate<t>;  
 
  TScreenPointSet = specialize TPointSet<word>;  
  TDiscretePointSet =  specialize TPointSet<integer>;  
  TRealPointSet = specialize TPointSet<extended>;
```

**Napomena**  
A word on visibility: the template types `T` or `_T` are available as strict private types. That means that the types are not available in the descendent classes, unless they are made available through some protected or private mechanism, as in the following example:

```pascal
  generic TList<_T>=class(TObject)  
  public type  
    TItemType = _T;  
  end;
```

## 8.3 Generic type specialization

Once a generic type is defined, it can be used to generate other types: this is like replaying the definition of the types, with the template placeholders filled in with actual type definitions.

This can be done in any Type definition block.

Which is a very simple definition. Given the declaration of TList in the previous section, the following would be a valid type definition:

```pascal
Type  
  TPointerList = specialize TList<Pointer>;  
  TIntegerList = specialize TList<Integer>;
```

As of version 3.0 of Free Pascal, the specialize keyword can also be used in a variable declaration:

```pascal
Var  
  P : specialize TList<Pointer>;
```

The specialize keyword is part of the specialized type, so when using fully qualified names, the specialize keyword must be after the unit name and parent type names.

The type in the specialize statement must be known, except in another generic type definition. Given the two generic class definitions:

```pascal
type  
  Generic TMyFirstType<T1> = Class(TMyObject);  
  Generic TMySecondType<T2> = Class(TMyOtherObject);
```

Then the following specialization is not valid:

```pascal
type  
  TMySpecialType = specialize TMySecondType<TMyFirstType>;
```

because the type TMyFirstType is a generic type, and thus not fully defined. The compiler will complain:

```sh
Error: Generics cannot be used as parameters when specializing generics
```

However, the following is allowed:

```pascal
type  
  TA = specialize TMyFirstType<Atype>;  
  TB = specialize TMySecondType<TA>;
```

because TA is already fully defined when TB is specialized.

However, the specialize keyword can be used in another generic type definition as shown in the example above:

```pascal
  generic TList<_T>=class(TObject, specialize IList<_T>)
```

and

```pascal
  generic TPointSet<t> = array of specialize PlanarCoordinate<t>;
```

In these definitions, the specialization is only performed when the generic type itself is specialized, and at that time, the types are known.

**Napomena**  
It is not possible to make a forward definition of a class which is a specialization of a generic, i.e. the following will not compile:

```pascal
  TMyClass = Class;  
 
  // Other declarations  
 
  TMyClass = specialize TList<T>;
```

## 8.4 Generic type restrictions

The diagram in section 8.1, page 432 shows that the type template list can have extra specifiers for the types. This is especially useful for object types: if the template type must descend from a certain class, then this can be specified in the template list:

```pascal
{$mode objfpc}{$h+}  

uses sysutils, classes;  
 
Type  
  generic TList<_T : TComponent> = class(TObject)  
  public  
    Type TCompareFunc = function(const Item1, Item2: _T): Integer;  
  Public  
    data : _T;  
    procedure Add(item: _T);  
    procedure Sort(compare: TCompareFunc);  
end;
```

Given the above definition, the following will compile:

```pascal
TPersistentList = specialize TList<TComponent>;
```

But this will not compile

```pascal
TPersistentList = specialize TList<TPersistent>;
```

The compiler will return an error:

```sh
Error: Incompatible types: got "TPersistent" expected "TComponent"
```

Multiple types can be grouped together:

```pascal
Type  
  generic TList<Key1,Key2 : TComponent; Value1 : TObject> = class(TObject)
```

Additionally, it is possible to specify more than one type identifier for class and interface type restrictions. If a class is specified, then the type used for the template must be equal to or descend from the indicated type:

```pascal
Type  
  generic TList<T: TComponent, IEnumerable> = class(TObject)
```

A class used to specialize T must descend from TComponent and must implement IEnumerable.

If an interface is specified, then the template type must implement at least this interface, but it can also be a descendent interface from this interface:

```pascal
Type  
  generic TGenList<T: IEnumerable> = class(TObject)  
 
  IMyEnum = Interface (IEnumerable)  
    Procedure DoMy;  
  end;  
 
  TList = specialize TGenList<IMyEnum>;  
  TSomeList = Specialize TGenList<TList>;
```

Multiple interfaces can be specified, in that case the class type must implement all listed interfaces: It is possible to mix one class name with several interface names.

If no type restrictions are in effect, the compiler will assume that template types are not assignment compatible.

This is specially important when the generic class contains overloaded methods. Given the following generic type declaration:

```pascal
type  
  generic TTest<T1, T2> = class  
    procedure Test(aArg: LongInt);  
    procedure Test(aArg: T1);  
    procedure Test(aArg: T2);  
  end;
```

Specializing the above will compile if T1 and T2 are of two different types and neither is also LongInt. The following will compile:

```pascal
T1 = specialize TTest<String, TObject>;

But the following two will not compile:
T2 = specialize TTest<String, String>;
```

or

```pascal
T2 = specialize TTest<String, Longint>;
```

## 8.5 Delphi compatibility

FPC’s generics support is implemented somewhat different from Delphi. In this section the main differences are highlighted.

### 8.5.1 Syntax elements

The syntax shown in the syntax diagrams is the syntax required in the ObjFPC mode of the compiler. However, in Delphi mode, the specialize and generic keywords must not be used, as shown in the following example:

```pascal
Type  
  TTest<T> = Class(TObject)  
  Private  
    FObj : T;  
  Public  
    Property Obj : T Read FObj Write FObj;  
  end;  
 
  TIntegerTest = TTest<Integer>;
```

In difference with Mode Objfpc, the template type names must be repeated in method definitions.

```pascal
Type  
  TTest<T> = Class(TObject)  
  Private  
    FObj : T;  
  Public  
    Procedure DoIt;  
    Property Obj : T Read FObj Write FObj;  
  end;  
 
Procedure TTest<T>.DoIt;  
 
begin  
end;
```

This requirement is directly related to the generic type overload capability mentioned in the next section.

### 8.5.2 Record type restrictions

In Delphi mode, the record type restrictions will also allow the use of simple types:

```pascal
Type  
  TList<_T : record> = class(TObject)  
  public  
    Type TCompareFunc = function(const Item1, Item2: _T): Integer;  
  Public  
    data : _T;  
    procedure Add(item: _T);  
    procedure Sort(compare: TCompareFunc);  
 end;  
 
TIntList = TList<Integer>;
```

The restriction is enforced when specializing the type. That means that the mode active when specializing a type determines whether a simple type can be used or not: if the restriction to record was compiled using ObjFPC mode, code written in Delphi mode can specialize it with a simple type anyway.

For example:

```pascal
unit tg;  
 
interface  
 
{$mode objfpc}  
 
Type  
  generic TList<_T : record> = class(TObject)  
  public  
    Type TCompareFunc = function(const Item1, Item2: _T): Integer;  
  Public  
    data : _T;  
    procedure Add(item: _T);  
    procedure Sort(compare: TCompareFunc);  
 end;  
 
implementation  
 
generic procedure TList<_T>.Add(item: _T);  
 
begin  
end;  
 
generic  procedure TList<_T>.Sort(compare: TCompareFunc);  
 
begin  
end;  
 
end.
```

can be used in {$MODE Delphi} for:

```pascal
{$mode delphi}  
uses tg;  
 
Type  
  TIntList = TList<Integer>;  
begin  
end. 
```

### 8.5.3 Type overloads

Delphi mode allows generic type overloads. This means that it is possible to declare the same generic class with different template type lists. The following declarations are therefore possible:

```pascal
Type  
  TTest<T> = Class(TObject)  
  Private  
    FObj : T;  
  Public  
    Property Obj : T Read FObj Write FObj;  
  end;  
 
  TTest<T,S> = Class(TObject)  
  Private  
    FObj1 : T;  
    FObj2 : S;  
  Public  
    Property Obj1 : T Read FObj1 Write FObj1;  
    Property Obj2 : S Read FObj2 Write FObj2;  
  end; 
```

### 8.5.4 Name space considerations

In Delphi mode, the generics do not interfere with the namespace for variables, this means that the following will also compile:

```pascal
Type  
  TTest<T> = Class(TObject)  
  Private  
    FObj : T;  
  Public  
    Property Obj : T Read FObj Write FObj;  
  end;  
 
Var  
  TTest : Integer;
```

However, this does not work for constants and functions.

## 8.6 Type compatibility

Whenever a generic class is specialized, this results in a new, distinct type. These types are assignment compatible if the same template types are used.

Take the following generic definition:

```pascal
{$mode objfpc}  
unit ua;  
 
interface  
 
type  
  Generic TMyClass<T> = Class(TObject)  
    Procedure DoSomething(A : T; B : INteger);  
  end;  
 
Implementation  
 
Procedure TMyClass.DoSomething(A : T; B : Integer);  
 
begin  
  // Some code.  
end;  
 
end.
```

And the following specializations:

```pascal
{$mode objfpc}  
unit ub;  
 
interface  
 
uses ua;  
 
Type  
  TB = Specialize TMyClass<string>;  
 
implementation  
 
end.
```

the following specializations is identical, but appears in a different unit:

```pascal
{$mode objfpc}  
unit uc;  
 
interface  
 
uses ua;  
 
Type  
  TB = Specialize TMyClass<string>;  
 
implementation  
 
end.
```

The following will then compile:

```pascal
{$mode objfpc}  
unit ud;  
 
interface  
 
uses ua,ub,uc;  
 
Var  
  B : ub.TB;  
  C : uc.TB;  
 
implementation  
 
begin  
  B:=C;  
end.
```

The types ub.TB and uc.TB are assignment compatible. It does not matter that the types are defined in different units. They could be defined in the same unit as well:

```pascal
{$mode objfpc}  
unit ue;  
 
interface  
 
uses ua;  
 
Type  
  TB = Specialize TMyClass<string>;  
  TC = Specialize TMyClass<string>;  
 
 
Var  
  B : TB;  
  C : TC;  
 
implementation  
 
begin  
  B:=C;  
end.
```

Each specialization of a generic class with the same types as parameters is a new, distinct type, but these types are assignment compatible if the template types used to specialize them are equal.

If the specialization is with a different template type, the types are still distinct, but no longer assignment compatible. i. e. the following will not compile:

```pascal
{$mode objfpc}  
unit uf;  
 
interface  
 
uses ua;  
 
Type  
  TB = Specialize TMyClass<string>;  
  TC = Specialize TMyClass<integer>;  
 
 
Var  
  B : TB;  
  C : TC;  
 
implementation  
 
begin  
  B:=C;  
end.

When compiling, an error will result:
Error: Incompatible types: got "TMyClass<System.LongInt>"  
                           expected "TMyClass<System.ShortString>" 
```

## 8.7 Using the default intrinsic

When writing generic routines, sometimes a variable must be initialized whose type is not known during the declaration of the generic. This is where the Default intrinsic (section 4.5, page 243) also comes into play. Given the following generic declaration:

```pascal
type  
  generic TTest<T> = class  
    procedure Test;  
  end;

The following code will correctly initialize the variable myt during specialization:
procedure TTest.Test;  
var  
  myt: T;  
begin  
  // will have the correct Default if class is specialized  
  myt := Default(T);  
end;
```

## 8.8 A word about scope

It should be stressed that all identifiers other than the template placeholders should be known when the generic class is declared. At the same time, nothing can be assumed about the template type (unless a restriction is placed on it).

This works in several ways.

In the absence of type restrictions, the generic code cannot make assumptions about the template type T. Consider the following unit:

```pascal
unit ts;  
 
interface  
{$modeswitch advancedrecords}  
 
type  
  PListEl = ^TListEl;  
  TListEl = packed record  
    Prev, Next: PListEl;  
  end;  
 
implementation  
 
type  
  generic LstEnumerator<T> = record  
  private  
    lst, lst_save: T;  
  public  
    constructor Create(const Value: T);  
    function MoveNext: boolean;  
  end;  
 
function LstEnumerator.MoveNext: boolean;  
begin  
  Result:=lst <> nil;  
  if Result then  
    lst:=lst^.next;  
end;  
 
constructor LstEnumerator.Create(const Value: T);  
begin  
  lst:= Value;  
  lst_save := nil;  
end;  
 
 
Type  
 TMyListEnum = specialize LstEnumerator<TListEl>;  
 
end.
```

The compiler will throw an error because when it compiles the generic definition, it cannot verify that

```pascal
   lst:=lst^.next;
```

is correct. lst is of type T, but the compiler does not (yet) know what T is, and hence cannot know it has a field next.

This problem can be solved with type restrictions:

```pascal
unit ts;  
{$mode delphi}  
interface  
 
type  
  TListEl = class  
    Prev, Next: TListEl;  
  end;  
 
  TMyRecord1 = Class(TListEl)  
    MyField : String;  
  end;  
 
  TMyRecord2 = Class(TListEl)  
    MyInteger : Integer;  
  end;  
 
implementation  
 
type  
  TLstEnumerator<T : TListEl> = class  
  private  
    lst, lst_save: T;  
  public  
    constructor Create(const Value: T);  
    function MoveNext: boolean;  
  end;  
 
function TLstEnumerator<T>.MoveNext: boolean;  
begin  
  Result:=lst <> T(nil);  
  if Result then  
    lst:=T(lst.next);  
end;  
 
constructor TLstEnumerator<t>.Create(const Value: T);  
begin  
  lst:= Value;  
  lst_save := T(nil);  
end;  
 
Type  
 TMyRecord1Enum = TLstEnumerator<TMyRecord1>;  
 TMyRecord2Enum = TLstEnumerator<TMyRecord2>;
```

Here, the compiler knows that lst is at least of type TListEl, and hence contains members Prev and Next.

Beside the template type, all other types used in the generic declaration must be known. This means that a type identifier with the same name must exist. The following unit will produce an error:

```pascal
{$mode objfpc}  
unit myunit;  
 
interface  
 
type  
  Generic TMyClass<T> = Class(TObject)  
    Procedure DoSomething(A : T; B : TSomeType);  
  end;  
 
Type  
  TSomeType = Integer;  
  TSomeTypeClass = specialize TMyClass<TSomeType>;  
 
Implementation  
 
Procedure TMyClass.DoSomething(A : T; B : TSomeType);  
 
begin  
  // Some code.  
end;  
 
end.
```

The above code will result in an error, because the type TSomeType is not known when the declaration is parsed:

```sh
home: >fpc myunit.pp  
myunit.pp(8,47) Error: Identifier not found "TSomeType"  
myunit.pp(11,1) Fatal: There were 1 errors compiling module, stopping
```

A second way in which this is visible, is the following. Assume a unit

```pascal
{$mode objfpc}  
unit mya;  
 
interface  
 
type  
  Generic TMyClass<T> = Class(TObject)  
    Procedure DoSomething(A : T);  
  end;  
 
 
Implementation  
 
Procedure DoLocalThings;  
 
begin  
  Writeln('mya.DoLocalThings');  
end;  
 
 
Procedure TMyClass.DoSomething(A : T);  
 
begin  
  DoLocalThings;  
end;  
 
end.
```

The compiler will not allow to compile this unit, since the DoLocalThings function will not be visible when the generic type is specialized:

```sh
Error: Global Generic template references static symtable
```

Now, if the unit is modified, and the DoLocalThings function is moved to the interface section, the unit will compile. When using this generic in a program:

```pascal
{$mode objfpc}  
program myb;  
 
uses mya;  
 
procedure DoLocalThings;  
 
begin  
  Writeln('myb.DoLocalThings');  
end;  
 
Type  
  TB = specialize TMyClass<Integer>;  
 
Var  
  B : TB;  
 
begin  
  B:=TB.Create;  
  B.DoSomething(1);  
end.
```

Despite the fact that generics act as a macro which is replayed at specialization time, the reference to DoLocalThings is resolved when TMyClass is defined, not when TB is defined. This means that the output of the program is:

```sh
home: >fpc -S2 myb.pp  
home: >myb  
mya.DoLocalThings
```

This behavior is dictated by safety and necessity:

- A programmer specializing a class has no way of knowing which local procedures
  are used, so he cannot accidentally “override” it.
- A programmer specializing a class has no way of knowing which local procedures
  are used, so he cannot implement it either, since he does not know the parameters.
- If implementation procedures are used as in the example above, they cannot be
  referenced from outside the unit. They could be in another unit altogether, and the programmer has no way of knowing he should include them before specializing his class.

## 8.9 Operator overloading and generics

Operator overloading (chapter 15, page 846) and generics are closely related. Imagine a generic class that has the following definition:

```pascal
{$mode objfpc}  
unit mya;  
 
interface  
 
type  
  Generic TMyClass<T> = Class(TObject)  
    Function Add(A,B : T) : T;  
  end;  
 
Implementation  
 
Function TMyClass.Add(A,B : T) : T;  
 
begin  
  Result:=A+B;  
end;  
 
end.
```

When the compiler replays the generics macro, the addition must be possible. For a specialization like this:

```pascal
TMyIntegerClass = specialize TMyClass<integer>;
```

This is not a problem, as the Add method would become:

```pascal
Procedure TMyIntegerClass.Add(A,B : Integer) : Integer;  
 
begin  
  Result:=A+B;  
end;
```

The compiler knows how to add two integers, so this code will compile without problems. But the following code:

```pascal
Type  
  TComplex = record  
   Re,Im : Double;  
  end;  
 
Type  
  TMyIntegerClass = specialize TMyClass<TComplex>;
```

Will not compile, unless the addition of two TComplex types is defined. This can be done using record operators:

```pascal
{$modeswitch advancedrecords}  
uses mya;  
 
Type  
  TComplex = record  
     Re,Im : Double;  
     class operator +(a,b : TComplex) : TComplex;  
  end;  
 
class operator TComplex.+ (a,b : TComplex) : TComplex;  
 
begin  
  Result.re:=A.re+B.re;  
  Result.im:=A.im+B.im;  
end;  
 
Type  
  TMyComplexClass = specialize TMyClass<TComplex>;  
 
begin  
  // Code here  
end.
```

Currently, due to an implementation restriction, it will not work using a global operator, i. e. the following does not work yet:

```pascal
uses mya;  
 
Type  
  TComplex = record  
     Re,Im : Double;  
  end;  
 
operator + (a,b : TComplex) : TComplex;  
 
begin  
  Result.re:=A.re+B.re;  
  Result.im:=A.im+B.im;  
end;  
 
Type  
  TMyComplexClass = specialize TMyClass<TComplex>;  
 
begin  
  // Code here  
end.
```

Support for this construct is expected in a future version of Free Pascal.

[prev][f1] [content][f0] [next][f2]

[f1]: 07_interfaces.md
[f0]: 00_contents.md
[f2]: 09_.md
