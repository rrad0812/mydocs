
# 11 Interface

[prev][f1] [content][f0] [next][f2]

## 11.1 Definition

As of version 1.1, FPC supports interfaces. Interfaces are an alternative to multiple inheritance (where a class can have multiple parent classes) as implemented for instance in C++. An interface is basically a named set of methods and properties: a class that implements the interface provides all the methods as they are enumerated in the Interface definition. It is not possible for a class to implement only part of the interface: it is all or nothing.

Interfaces can also be ordered in a hierarchy, exactly as classes: an interface definition that inherits from another interface definition contains all the methods from the parent interface, as well as the methods explicitly named in the interface definition. A class implementing an interface must then implement all members of the interface as well as the methods of the parent interface(s).

An interface can be uniquely identified by a GUID. GUID is an acronym for Globally Unique Identifier, a 128-bit integer guaranteed always to be unique1. Especially on Windows systems, the GUID of an interface can and must be used when using COM.

Along with this definition the following must be noted:

- Interfaces can only be used in DELPHI mode or in OBJFPC mode.
- There are no visibility specifiers. All members are public (indeed, it would
  make little sense to make them private or protected).
- The properties declared in an interface can only have methods as read and write
  specifiers.
- There are no constructors or destructors. Instances of interfaces cannot be
  created directly: instead, an instance of a class implementing the interface must be created.
- Only calling convention modifiers may be present in the definition of a method.
  Modifiers as virtual, abstract or dynamic, and hence also override cannot be present in the interface definition.

The following are examples of interfaces:

```pascal
IUnknown = interface ['{00000000-0000-0000-C000-000000000046}']  
  function QueryInterface(const iid : tguid;out obj) : longint;  
  function _AddRef : longint;  
  function _Release : longint;  
end;  
IInterface = IUnknown;  

IMyInterface = Interface  
  Function MyFunc : Integer;  
  Function MySecondFunc : Integer;  
end;
```

As can be seen, the GUID identifying the interface is optional.

## 11.2 Interface identification: A GUID

An interface can be identified by a GUID. This is a 128-bit number, which is represented in a text representation (a string literal):
`['{HHHHHHHH-HHHH-HHHH-HHHH-HHHHHHHHHHHH}']`

Each H character represents a hexadecimal number (0–9, A–F). The format contains 8-4-4-4-12 numbers. A GUID can also be represented by the following record, defined in the objpas unit (included automatically when in DELPHI or OBJFPC mode):

```pascal
PGuid = ^TGuid;  
TGuid = packed record  
   case integer of  
      1 : (  
           Data1 : DWord;  
           Data2 : word;  
           Data3 : word;  
           Data4 : array[0..7] of byte;  
          );  
      2 : (  
           D1 : DWord;  
           D2 : word;  
           D3 : word;  
           D4 : array[0..7] of byte;  
          );  
      3 : ( { uuid fields according to RFC4122 }  
           time_low : dword;  
           time_mid : word;  
           time_hi_and_version : word;  
           clock_seq_hi_and_reserved : byte;  
           clock_seq_low : byte;  
           node : array[0..5] of byte;  
           );  
end;
```

A constant of type TGUID can be specified using a string literal:

```pascal
{$mode objfpc}  
program testuid;  
 
Const  
  MyGUID : TGUID = '{10101010-1010-0101-1001-110110110110}';  
 
begin  
end.
```

Normally, the GUIDs are only used in Windows, when using COM interfaces. More on this in the next section.

## 11.3 Interface implementations

When a class implements an interface, it should implement all methods of the interface. If a method of an interface is not implemented, then the compiler will give an error. For example:

```pascal
Type  
  IMyInterface = Interface  
    Function MyFunc : Integer;  
    Function MySecondFunc : Integer;  
  end;  
 
  TMyClass = Class(TInterfacedObject,IMyInterface)  
    Function MyFunc : Integer;  
    Function MyOtherFunc : Integer;  
  end;  
 
Function TMyClass.MyFunc : Integer;  
 
begin  
  Result:=23;  
end;  
 
Function TMyClass.MyOtherFunc : Integer;  
 
begin  
  Result:=24;  
end;
```

will result in a compiler error:

```sh
Error: No matching implementation for interface method  
"IMyInterface.MySecondFunc:LongInt" found
```

Normally, the names of the methods that implement an interface, must equal the names of the methods in the interface definition. The compiler will look for matching methods in all visible methods: the methods of the class, and in parent classes methods with visibility protected or higher.

However, it is possible to provide aliases for methods that make up an interface: that is, the compiler can be told that a method of an interface is implemented by an existing method with a different name. This is done as follows:

```pascal
Type  
  IMyInterface = Interface  
    Function MyFunc : Integer;  
  end;  
 
  TMyClass = Class(TInterfacedObject,IMyInterface)  
    Function MyOtherFunction : Integer;  
    Function IMyInterface.MyFunc = MyOtherFunction;  
  end;
```

This declaration tells the compiler that the MyFunc method of the IMyInterface interface is implemented in the MyOtherFunction method of the TMyClass class.

## 11.4 Interface inheritance

It is possible to let one interface be a descendent from another interface:

```pascal
IParentInterface = interface  
  ['{0F78D56E-85A6-4024-98D7-720D7C7B9573}']  
  procedure Foo;  
end;  
 
IChildInterface = interface(IParentInterface)  
  ['{1AB2EB85-6843-462E-8CE4-32ECC065011E}']  
  procedure Bar;  
end;
```

IChildInterface will have two methods: foo and bar. Any class implementing this interface will therefore need to implement both interfaces:

```pascal
TImplementor = class(TInterfacedObject, IChildInterface)  
public  
  procedure Foo;  
  procedure Bar;  
end;  
 
procedure TImplementor.Foo;  
begin  
 
end;  
 
procedure TImplementor.Bar;  
begin  
 
end;
```

Note that when a class declares a child interface, it can be assigned to a variable with the child interface. Given the above declarations, the following will compile:

```pascal
var  
  Child: IChildInterface;  
 
begin  
  Child := TImplementor.Create;
```

But this does not imply that it automatically also is assignment compatible with a variable with the type of parent interface. The following will not compile:

```pascal
var  
  Parent: IParentInterface;  
 
begin  
  Parent := TImplementor.Create;
```

To make this compile, it is necessary to declare the class as:

```pascal
TImplementor = class(TInterfacedObject,  
                     IParentInterface,  
                     IChildInterface)  
public  
  procedure Foo;  
  procedure Bar;  
end;
```

The reason for this is that although the class actually implements the methods of IParentInterface, the compiler checks only actually declared interfaces when checking assignment compatibility: all declared interfaces are put in a table and only the contents of this table is checked.

The same check is performed at runtime: the compiler generates a table of all interfaces a class declares, and this table is checked at runtime. That means that although the following will compile if only IChildInterface is declared:

```pascal
  ParentImplementorInstance := (TImplementor.Create as IParentInterface);
```

it will still fail with a run-time error:

```sh
home:~> ./ti  
An unhandled exception occurred at $0000000000411A27:  
EInvalidCast: Invalid type cast  
$0000000000411A27
```

## 11.5 Interface delegation

Sometimes, the methods of an interface are implemented by a helper (or delegate) object, or the class instance has obtained an interface pointer for this interface and that should be used. This can be for instance when an interface must be added to a series of totally unrelated classes: the needed interface functionality is added to a separate class, and each of these classes uses an instance of the helper class to implement the functionality.

In such a case, it is possible to instruct the compiler that the interface is not implemented by the object itself, but actually resides in a helper class or interface. This can be done with the implements property modifier.

If the class has a pointer to the desired interface, the following will instruct the compiler that when the IMyInterface interface is requested, it should use the reference in the field:

```pascal
type  
  IMyInterface = interface  
    procedure P1;  
  end;  
 
  TMyClass = class(TInterfacedObject, IMyInterface)  
  private  
    FMyInterface: IMyInterface; // interface type  
  public  
    property MyInterface: IMyInterface  
       read FMyInterface implements IMyInterface;  
  end;
```

The interface should not necessarily be in a field, any read identifier can be used.

If the interface is implemented by a delegate object, (a helper object that actually implements the interface) then it can be used as well with the implements keyword:

```pascal
{$interfaces corba}  

type  
  IMyInterface = interface  
    procedure P1;  
  end;  
 
  // NOTE: Interface must be specified here  
  TDelegateClass = class(TObject, IMyInterface)  
  private  
    procedure P1;  
  end;  
 
  TMyClass = class(TInterfacedObject, IMyInterface)  
  private  
    FMyInterface: TDelegateClass; // class type  
    property MyInterface: TDelegateClass  
      read FMyInterface implements IMyInterface;  
  end;
```

Note that in difference with Delphi, the delegate class must explicitly specify the interface: the compiler will not search for the methods in the delegate class, it will simply check if the delegate class implements the specified interface.

It is possible to implement multiple interfaces using a single delegated object:

```pascal
{$interfaces corba}  

type  
  IMyInterface = interface  
    procedure P1;  
  end;  
  IMyInterface1 = interface  
    procedure P2;  
  end;  
 
  // NOTE: Interface must be specified here  
  TDelegateClass = class(TObject, IMyInterface,IMyInterface1)  
  private  
    procedure P1;  
    procedure P2;  
  end;  
 
  TMyClass = class(TInterfacedObject, IMyInterface, IMyInterface1)  
  private  
    FMyInterface: TDelegateClass; // class type  
    property MyInterface: TDelegateClass  
      read FMyInterface implements IMyInterface,IMyInterface1;  
  end;
```

It is not possible to mix method resolution and interface delegation. That means, it is not possible to implement part of an interface through method resolution and implement part of the interface through delegation. The following attempts to implement IMyInterface partly through method resolution (P1), and partly through delegation. The compiler will not accept the following code:

```pascal
{$interfaces corba}  

type  
  IMyInterface = interface  
    procedure P1;  
    procedure P2;  
  end;  
 
  TMyClass = class(TInterfacedObject, IMyInterface)  
    FI : IMyInterface;  
  protected  
    procedure IMyInterface.P1 = MyP1;  
    procedure MyP1;  
    property MyInterface: IMyInterface  read FI implements IMyInterface;  
  end;
```

The compiler will throw an error:

```sh
Error: Interface "IMyInterface" can't be delegated by "TMyClass",  
it already has method resolutions
```

However, it is possible to implement one interface through method resolution, and another through delegation:

```pascal
{$interfaces corba}  

type  
  IMyInterface = interface  
    procedure P1;  
  end;  
 
  IMyInterface2 = interface  
    procedure P2;  
  end;  
 
  TMyClass = class(TInterfacedObject,  
                   IMyInterface, IMyInterface2)  
    FI2 : IMyInterface2;  
  protected  
    procedure IMyInterface.P1 = MyP1;  
    procedure MyP1;  
  public  
    property MyInterface: IMyInterface2  
       read FI2 implements IMyInterface2;  
  end;
```

Note that interface delegation can be used to specify that a class implements parent interfaces:

```pascal
IGMGetFileName = interface(IUnknown)  
  ['{D3ECCB42-A563-4cc4-B375-79931031ECBA}']  
  function GetFileName: String; stdcall;  
  property FileName: String read GetFileName;  
end;  
 
IGMGetSetFileName = Interface(IGMGetFileName)  
  ['{ECFB879F-86F6-41a3-A685-0C899A2B5BCA}']  
  procedure SetFileName(const Value: String); stdcall;  
  property FileName: String read GetFileName write SetFileName;  
end;  
 
TIntfDelegator = class(TInterfacedObject, IGMGetFileName, IGMGetSetFileName)  
 protected  
  FGetSetFileName: IGMGetSetFileName;  
 public  
  constructor Create;  
  destructor Destroy; override;  
  property Implementor: IGMGetSetFileName read FGetSetFileName  
    implements IGMGetFileName, IGMGetSetFileName;  
end; 
```

## 11.6 Interfaces and COM

When using interfaces on Windows which should be available to the COM subsystem, the calling convention should be stdcall – this is not the default Free Pascal calling convention, so it should be specified explicitly.

COM does not know properties. It only knows methods. So when specifying property definitions as part of an interface definition, be aware that the properties will only be known in the Free Pascal compiled program: other Windows programs will not be aware of the property definitions.

## 11.7 CORBA and other Interfaces

COM is not the only architecture where interfaces are used. CORBA knows interfaces, UNO (the OpenOffice API) uses interfaces, and Java as well. These languages do not know the IUnknown interface used as the basis of all interfaces in COM. It would therefore be a bad idea if an interface automatically descended from IUnknown if no parent interface was specified. Therefore, a directive {$INTERFACES} was introduced in Free Pascal: it specifies what the parent interface is of an interface, declared without parent. More information about this directive can be found in the Programmer’s Guide.

Note that COM interfaces are by default reference counted, because they descend from IUnknown.

Corba interfaces are identified by a simple string so they are assignment compatible with strings and not with TGUID. The compiler does not do any automatic reference counting for the CORBA interfaces, so the programmer is responsible for any reference bookkeeping.

## 11.8 Reference counting

All COM interfaces use reference counting. This means that whenever an interface is assigned to a variable, its reference count is updated. Whenever the variable goes out of scope, the reference count is automatically decreased. When the reference count reaches zero, usually the instance of the class that implements the interface, is freed.

Care must be taken with this mechanism. The compiler may or may not create temporary variables when evaluating expressions, and assign the interface to a temporary variable, and only then assign the temporary variable to the actual result variable. No assumptions should be made about the number of temporary variables or the time when they are finalized – this may (and indeed does) differ from the way other compilers (e. g. Delphi) handle expressions with interfaces. E. g. a type cast is also an expression:

```pascal
Var  
  B : AClass;  
 
begin  
  // ...  
  AInterface(B.Intf).testproc;  
  // ...  
end;
```

Assume the interface intf is reference counted. When the compiler evaluates B.Intf, it creates a temporary variable. This variable may be released only when the procedure exits: it is therefore invalid to e. g. free the instance B prior to the exit of the procedure, since when the temporary variable is finalized, it will attempt to free B again.

Additionally, function results may point to a non-nil valid COM interface on entry: this is because the function result is treated as a var parameter.

[prev][f1] [content][f0] [next][f2]

[f1]: 06_classes.md
[f0]: 00_contents.md
[f2]: 08_generics.md
