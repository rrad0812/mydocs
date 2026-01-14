
# 6 Classes

In the Delphi approach to Object Oriented Programming, everything revolves around the concept of “Classes”. A class can be seen as a pointer to an object, or a pointer to a record, with methods associated with it.

The difference between objects and classes is mainly that an object is allocated on the stack, as an ordinary record would be, and that classes are always allocated on the heap. In the following example:

```pascal
Var  
  A : TSomeObject; // an Object  
  B : TSomeClass;  // a Class
```

The main difference is that the variable A will take up as much space on the stack as the size of the object (TSomeObject). The variable B, on the other hand, will always take just the size of a pointer on the stack. The actual class data is on the heap.

From this, a second difference follows: a class must always be initialized through its constructor, whereas for an object, this is not necessary. Calling the constructor allocates the necessary memory on the heap for the class instance data.

Remark In earlier versions of Free Pascal it was necessary, in order to use classes, to put the objpas unit in the uses clause of a unit or program. This is no longer needed as of version 0.99.12. As of this version, the unit will be loaded automatically when the -MObjfpc or -MDelphi options are specified, or their corresponding directives are used:

```pascal
{$mode objfpc}  
{$mode delphi}
```

In fact, the compiler will give a warning if it encounters the objpas unit in a uses clause.

## 6.1 Class definitions

The prototype declaration of a class is as follows:

Remark In MacPas mode, the Object keyword is replaced by the class keyword for compatibility with other pascal compilers available on the Mac. That means that in MacPas mode, the reserved word “class” in the above diagram may be replaced by the reserved word “object”.

In a class declaration, as many visibility blocks as needed can be used: the various blocks can be repeated, and there is no special order in which they must appear.

Methods are normal function or procedure declarations. As can be seen, the declaration of a class is almost identical to the declaration of an object. The real difference between objects and classes is in the way they are created (see further in this chapter).

The visibility of the different sections is as follows:

- Private
  All fields and methods that are in a private block, can only be accessed in the module (i. e. unit) that contains the class definition. They can be accessed from inside the classes’ methods or from outside them (e. g. from other classes’ methods)
- Strict Private
  All fields and methods that are in a strict private block, can only be accessed from methods of the class itself. Other classes or descendent classes (even in the same unit) cannot access strict private members.
- Protected
  Is the same as Private, except that the members of a Protected section are also accessible to descendent types, even if they are implemented in other modules.
- Strict Protected
  Is the same as Protected, except that the members of a Protected section are also accessible to other classes implemented in the same unit. Strict protected members are only visible to descendent classes, not to other classes in the same unit.
- Public
  sections are always accessible.
- Published
  From a language perspective, this is the same as a Public section, but the compiler generates also type information that is needed for automatic streaming of these classes if the compiler is in the {$M+} state. Fields defined in a published section must be of class type. Array properties cannot be in a published section.

In the syntax diagram, it can be seen that a class can list implemented interfaces. This feature will be discussed in the next chapter.

Classes can contain Class methods: these are functions that do not require an instance. The Self identifier is valid in such methods, but refers to the class pointer (the VMT).

Remark Like with functions and pointer types, sometimes a forward definition of a class is needed. A class forward definition is simply the name of the class, with the keyword Class, as in the following example:

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

When using a class forward definition, the class must be defined in the same unit, in the same section (interface/implementation). It must not necessarily be defined in the same type section.

Class reference types are used to create instances of a certain class, which is not yet known at compile time, but which is specified at run time. Essentially, a variable of a class reference type contains a pointer to the definition of the specified class. This can be used to construct an instance of the class corresponding to the definition, or to check inheritance. The following example shows how it works:

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

This function can be passed a class reference of any class that descends from TComponent. The following is a valid call:
Var  
  C : TComponent;  

begin  
  C:=CreateComponent(TEdit,Form1);  
end;
```

On return of the CreateComponent function, C will contain an instance of the class TEdit. Note that the following call will fail to compile:

```pascal
Var  
  C : TComponent;  

begin  
  C:=CreateComponent(TStream,Form1);  
end;
```

because TStream does not descend from TComponent, and AClass refers to a TComponent class. The compiler can (and will) check this at compile time, and will produce an error.

References to classes can also be used to check inheritance:

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

The above example will raise an exception if the passed instance is not a descendent of TMinClass or a descendent of TMaxClass.

## 6.2 Abstract and sealed classes

A class can be declared as sealed. In that case, it is not possible to declare a descendent class. The compiler will return an error if it encounters a declaration of a descendent:

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

This will result in the following error:

```sh
Error: Cannot create a descendant of the sealed class "TMyClass"
```

An abstract class is a class that cannot be instantiated directly. Instead, a descendent class must always be instantiated. However, for Delphi compatibility, the compiler ignores this directive.

6.3 Normal and static fields

. Classes can have fields. Depending on how they are defined, fields hold data specific to an instance of a class or to the class as a whole. Whatever the way they were defined, fields observe the rules of visibility just like any other member of the class.

### 6.3.1 Normal fields/variables

There are two ways to declare a normal field. The first one is the classical way, similar to a definition in an object:

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

will be the following
2  
0
```

The example demonstrates that values of fields are initialized with zero (or the equivalent of zero for non ordinal types: empty string, empty array and so on).

The second way to declare a field (only available in more recent versions of Free Pascal) is using a var block:

```pascal
{$mode objfpc}  
type  
  cl=class  
  var  
    l : longint;  
  end;
```

This definition is completely equivalent to the previous definition.

Remark As of version 3.0 of the compiler, the compiler can re-order the fields in memory if this leads to better alignment and smaller instances. That means that in an instance, the fields do not necessarily appear in the same order as in the declaration. RTTI generated for a class will reflect this change.

### 6.3.2 Class fields/variables

Similar to objects, a class can contain static fields or class variables: these fields or variables are global to the class, and act like global variables, but are known only as part of the class. They can be referenced from within the classes’ methods, but can also be referenced from outside the class by providing the fully qualified name.

Again, there are two ways to define class variables. The first one is equivalent to the way it is done in objects, using a static modifier:

For instance, the output of the following program is the same as the output for the version using an object:

```pascal
{$mode objfpc}  
type  
  cl=class  
    l : longint;static;  
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

The output of this will be the following:

```sh
2  
3  
3
```

Note that the last line of code references the class type itself (cl), and not an instance of the class (cl1 or cl2).

In addition to the static field approach, in classes, a Class Var can be used. Similar to the way a field can be defined in a variable block, a class variable can be declared in a class var block:

```pascal
{$mode objfpc}  
type  
  cl=class  
  class var  
    l : longint;  
  end;
```

This definition is equivalent to the previous one.

Note that a class variable is tied to a specific class. Descendent classes will refer to the same instance, unless the variable is redeclared. The following program demonstrates this:

```pascal
{$mode objfpc}  
type  
  TA = class // base type  
    class var CODE: integer;  
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

The output of this program is:

```sh
 2 2 2
```

Because it is tied to a class, it can be overridden in delphi mode:

```pascal
{$mode delphi}  

type  
  TA = class // base type  
    class var CODE: integer;  
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

However, in OBJFPC mode it will not compile, and will give a duplicate identifier error.

## 6.4 Class instantiation

Classes must be created using one of their constructors (there can be multiple constructors). Remember that a class is a pointer to an object on the heap. When a variable of some class is declared, the compiler just allocates room for this pointer, not the entire object. The constructor of a class returns a pointer to an initialized instance of the object on the heap. So, to initialize an instance of some class, one would do the following:

```pascal
  ClassVar := ClassType.ConstructorName;
```

The extended syntax of new and dispose can not be used to instantiate and destroy class instances: That construct is reserved for use with objects only. Calling the constructor will provoke a call to the virtual class method NewInstance, which, in its default implementation, calls GetMem, to allocate enough space to hold the class instance data, and then zeroes out the memory.

After that, the constructor’s code is executed. The constructor has a pointer to its data, in Self.

**Remark**:

- The {$PackRecords } directive also affects classes, i. e. the alignment in
  memory of the different fields depends on the value of the {$PackRecords } directive.
- Just as for objects and records, a packed class can be declared. This has the
  same effect as on an object, or record, namely that the elements are aligned on 1-byte boundaries, i. e. as close as possible.
- SizeOf(class) will return the same as SizeOf(Pointer), since a class is a
  pointer to an object. To get the size of the class instance data, use the TObject.InstanceSize method.
- If an exception happens during an the execution of a constructor, the
  destructor will be called automatically.

## 6.5 Class destruction

Class instances must be destroyed using the destructor. In difference with the constructor, there is no choice in destructors: the destructor must have the name Destroy, it must override the Destroy destructor declared in TObject, cannot have arguments, and the inherited destructor must always be called.

Destroy will call FreeInstance, which, in its default implementation, calls FreeMem to release the memory occupied by the instance.

To avoid calling a destructor on a Nil instance, it is best to call the Free method of TObject. This method will check if Self is not Nil, and if so, then it calls Destroy. If Self equals Nil, it will just exit.

Destroying an instance does not remove or Nil a reference to an instance:

```pascal
Var  
  A : TComponent;  
 
begin  
  A:=TComponent.Create;  
  A.Name:='MyComponent';  
  A.Free;  
  Writeln('A is still assigned: ',Assigned(A));  
end.
```

After the call to Free, the variable A will not be Nil, the output of this program will be:

```sh
A is still assigned: TRUE
```

To make sure that the variable A is cleared after the destructor was called, the function FreeAndNil from the SysUtils unit can be used. It will call Free and will then write Nil in the object pointer (A in the above example):

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

After the call to FreeAndNil, the variable A will contain Nil, the output of this program will be:

```sh
A is still assigned: FALSE
```

Remark if an exception happens during an the execution of a constructor, the destructor will be called automatically.

## 6.6 Methods

6.6.1 Declaration

Declaration of methods in classes follows the same rules as method declarations in objects.

The only differences are the override, reintroduce and message directives.

6.6.2 Invocation

Method invocation for classes is no different than for objects. The following is a valid method invocation:
Var  AnObject : TAnObject;  
begin  
  AnObject := TAnObject.Create;  
  ANobject.AMethod;

6.6.3 Virtual methods

Classes have virtual methods, just as objects do. There is however a difference between the two. For objects, it is sufficient to redeclare the same method in a descendent object with the keyword virtual to override it. For classes, the situation is different: virtual methods must be overridden with the override keyword. Failing to do so, will start a new batch of virtual methods, hiding the previous one. The Inherited keyword will not jump to the inherited method, if Virtual was used.

The following code is wrong:
Type  
  ObjParent = Class  
    Procedure MyProc; virtual;  
  end;  
  ObjChild  = Class(ObjPArent)  
    Procedure MyProc; virtual;  
  end;

The compiler will produce a warning:
Warning: An inherited method is hidden by OBJCHILD.MYPROC

The compiler will compile it, but using Inherited can produce strange effects.

The correct declaration is as follows:
Type  
  ObjParent = Class  
    Procedure MyProc; virtual;  
  end;  
  ObjChild  = Class(ObjPArent)  
    Procedure MyProc; override;  
  end;

This will compile and run without warnings or errors.

If the virtual method should really be replaced with a method with the same name, then the reintroduce keyword can be used:
Type  
  ObjParent = Class  
    Procedure MyProc; virtual;  
  end;  
  ObjChild  = Class(ObjPArent)  
    Procedure MyProc; reintroduce;  
  end;

This new method is no longer virtual.

To be able to do this, the compiler keeps – per class type – a table with virtual methods: the VMT (Virtual Method Table). This is simply a table with pointers to each of the virtual methods: each virtual method has its fixed location in this table (an index). The compiler uses this table to look up the actual method that must be used at runtime. When a descendent object overrides a method, the entry of the parent method is overwritten in the VMT. More information about the VMT can be found in Programmer’s Guide.

Remark The keyword “virtual” can be replaced with the “dynamic” keyword: dynamic methods behave the same as virtual methods. Unlike in Delphi, in FPC the implementation of dynamic methods is equal to the implementation of virtual methods.

6.6.4 Class methods

Class methods are identified by the keyword Class in front of the procedure or function declaration, as in the following example:
  Class Function ClassName : String;

Class methods are methods that do not have an instance (i. e. Self does not point to a class instance) but which follow the scoping and inheritance rules of a class. They can be used to return information about the current class, for instance for registration or use in a class factory. Since no instance is available, no information available in instances can be used.

Class methods can be called from inside a regular method, but can also be called using a class identifier:
Var  
  AClass : TClass; // AClass is of type "type of class"  

begin  
  ..  
  if CompareText(AClass.ClassName,'TCOMPONENT')=0 then  
  ...  

But calling them from an instance is also possible:
Var  
  MyClass : TObject;  

begin  
  ..  
  if MyClass.ClassNameis('TCOMPONENT') then  
  ...

The reverse is not possible: Inside a class method, the Self identifier points to the VMT table of the class. No fields, properties or regular methods are available inside a class method. Accessing a regular property or method will result in a compiler error.

Note that class methods can be virtual, and can be overridden.

Class methods can be used as read or write specifiers for a regular property, but naturally, this property will have the same value for all instances of the class, since there is no instance available in the class method.

6.6.5 Class constructors and destructors

A class constructor or destructor can also be created. They serve to instantiate some class variables or class properties which must be initialized before a class can be used. These constructors are called automatically at program startup: The constructor is called before the initialization section of the unit it is declared in, the destructor is called after the finalisation section of the unit it is declared in.

There are some caveats when using class destructors/constructors:

- There may be only one constructor per class. The name is arbitrary, but it can
  not have parameters.
- There may be only one destructor per class. The name is arbitrary, but it can
  not have parameters.
- Neither constructor nor destructor can be virtual.
- The class constructor/destructor is called irrespective of the use of the
  class: even if a class is never used, the constructor and destructor are called anyway.
- There is no guaranteed order in which the class constructors or destructors
  are called. For nested classes, the only guaranteed order is that the constructors of nested classes are called after the constructor of the encompassing class is called, and for the destructors the opposite order is used.

The following program:

{$mode objfpc}  
{$h+}  

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

Will, when run, output the following:
Class constructor TA  
Class destructor TA

6.6.6 Static class methods

FPC knows static class methods in classes: these are class methods that have the Static keyword at the end. These methods behave completely like regular procedures or functions. This means that:

- They do not have a Self parameter. As a result, they cannot access roperties
  or fields or regular methods.
- They cannot be virtual.
- They can be assigned to regular procedural variables.

Their use is mainly to include the method in the namespace of the class as opposed to having the procedure in the namespace of the unit. Note that they do have access to all class variables, types etc, meaning something like this is possible:

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

Which will output 123, when run.

In the implementation of a static class method, the Self identifier is not available. The method behaves as if Self is hardcoded to the declared class, not the actual class with which it was called. In regular class methods, Self contains the Actual class for which the method was called. The following example makes this clear:
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

When run, this example will print:
Through static method:  
TA.Doit : TA  
TA.DoitStatic : TA  
Through class method:  
TA.Doit : TB  
TB.Doit : TB

For the static class method, even though it was called using TB, the class (Self, if it were available) is set to TA, the class in which the static method was defined. For the class method, the class is set to the actual class used to call the method (TB).

6.6.7 Message methods

New in classes are message methods. Pointers to message methods are stored in a special table, together with the integer or string constant that they were declared with. They are primarily intended to ease programming of callback functions in several GUI toolkits, such as Win32 or GTK. In difference with Delphi, Free Pascal also accepts strings as message identifiers. Message methods are always virtual.

As can be seen in the class declaration diagram, message methods are declared with a Message keyword, followed by an integer constant expression.

Additionally, they can take only one var argument (typed or not):
 Procedure TMyObject.MyHandler(Var Msg); Message 1;

The method implementation of a message function is not different from an ordinary method. It is also possible to call a message method directly, but this should not be done. Instead, the TObject.Dispatch method should be used. Message methods are automatically virtual, i. e. they can be overridden in descendent classes.

The TObject.Dispatch method can be used to call a message handler. It is declared in the system unit and will accept a var parameter which must have at the first position a cardinal with the message ID that should be called. For example:
Type  
  TMsg = Record  
    MSGID : Cardinal;  
    Data : Pointer;  
Var  
  Msg : TMSg;  
 
MyObject.Dispatch (Msg);

In this example, the Dispatch method will look at the object and all its ancestors (starting at the object, and searching up the inheritance class tree), to see if a message method with message MSGID has been declared. If such a method is found, it is called, and passed the Msg parameter.

If no such method is found, DefaultHandler is called. DefaultHandler is a virtual method of TObject that doesn’t do anything, but which can be overridden to provide any processing that might be needed. DefaultHandler is declared as follows:
   procedure DefaultHandler(var message);virtual;

In addition to the message method with a Integer identifier, Free Pascal also supports a message method with a string identifier:
 Procedure TMyObject.MyStrHandler(Var Msg); Message 'OnClick';

The working of the string message handler is the same as the ordinary integer message handler:

The TObject.DispatchStr method can be used to call a message handler. It is declared in the system unit and will accept one parameter which must have at the first position a short string with the message ID that should be called. For example:
Type  
  TMsg = Record  
    MsgStr : String[10]; // Arbitrary length up to 255 characters.  
    Data : Pointer;  
Var  
  Msg : TMSg;  
 
MyObject.DispatchStr (Msg);

In this example, the DispatchStr method will look at the object and all its ancestors (starting at the object, and searching up the inheritance class tree), to see if a message method with message MsgStr has been declared. If such a method is found, it is called, and passed the Msg parameter.

If no such method is found, DefaultHandlerStr is called. DefaultHandlerStr is a virtual method of TObject that doesn’t do anything, but which can be overridden to provide any processing that might be needed. DefaultHandlerStr is declared as follows:
   procedure DefaultHandlerStr(var message);virtual; 

6.6.8 Using inherited

In an overridden virtual method, it is often necessary to call the parent class’ implementation of the virtual method. This can be done with the inherited keyword. Likewise, the inherited keyword can be used to call any method of the parent class.

The first case is the simplest:
Type  
  TMyClass = Class(TComponent)  
    Constructor Create(AOwner : TComponent); override;  
  end;  
 
Constructor TMyClass.Create(AOwner : TComponent);  
 
begin  
  Inherited;  
  // Do more things  
end;

In the above example, the Inherited statement will call Create of TComponent, passing it AOwner as a parameter: the same parameters that were passed to the current method will be passed to the parent’s method. They must not be specified again: if none are specified, the compiler will pass the same arguments as the ones received.

If no inherited method with the same name exists, the Inherited will have no effect in this case. The presence of Inherited in this form can thus be interpreted as “call the overridden method if it exists”.

The second case is slightly more complicated:
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

The CreateNew method will first call TComponent.Create and will pass it AOwner as a parameter. It will not call TMyClass.Create.

If no method with the given name exists in parent classes, the compiler will give an error.

Although the examples were given using constructors, the use of inherited is not restricted to constructors, it can be used for any procedure or function or destructor as well.

6.7 Properties

6.7.1 Definition

Classes can contain properties as part of their fields list. A property acts like a normal field, i. e. its value can be retrieved or set, but it allows to redirect the access of the field through functions and procedures. They provide a means to associate an action with an assignment of or a reading from a class “field”. This allows e. g. checking that a value is valid when assigning, or, when reading, it allows to construct the value on the fly. Moreover, properties can be read-only or write only. The prototype declaration of a property is as follows:

_________________________________________________________________________________________________________
Properties

-- --- --- - ------------------- property definition -class--|property identifier - --| ----property specifiers hintdirective -----------------property interface-----

-- ----------------------- - -- property interface -property parameter list : type identifier ------------------------------------------------------------------ -index -integerconstant--|

-- - --- -- ------------------ property parameter list [ -parameter declaration-|] ;

--property specifiers--|-------------|------------------- -read specifier-- ----write specifier----| ----------------------------------implements specifier--------------- - --|- -| - --| default specifier stored specifier defaultarraypropertyspecifier

--read specifier- read -field or method-------------------------------

--write specifier-write- field or method------------------------------

--implements specifier-implements -|-identifier ------------------------- ---, ----

--default specifier--default------------------------------------------ | -constant-| | ------nodefault ------|

--stored specifier-stored---constant---------------------------------- -identifier -|

-- ---- ------------------------------------ field or method - field identifier -| method identifier

--defaultarraypropertyspecifier- ;- default -------------------------------
___________________________________________________________________

A read specifier is either the name of a field that contains the property, or the name of a method function that has the same return type as the property type. In the case of a simple type, this function must not accept an argument. In case of an array property, the function must accept a single argument of the same type as the index. In case of an indexed property, it must accept a integer as an argument.

A read specifier is optional, making the property write-only. Note that class methods cannot be used as read specifiers.

A write specifier is optional: If there is no write specifier, the property is read-only. A write specifier is either the name of a field, or the name of a method procedure that accepts as a sole argument a variable of the same type as the property. In case of an array property, the procedure must accept two arguments: the first argument must have the same type as the index, the second argument must be of the same type as the property. Similarly, in case of an indexed property, the first parameter must be an integer.

The section (private, published) in which the specified function or procedure resides is irrelevant. Usually, however, this will be a protected or private method.

For example, given the following declaration:
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

But the following would generate an error:
MyClass.Z := 0;

because Z is a read-only property.

What happens in the above statements is that when a value needs to be read, the compiler inserts a call to the various getNNN methods of the object, and the result of this call is used. When an assignment is made, the compiler passes the value that must be assigned as a parameter to the various setNNN methods.

Because of this mechanism, properties cannot be passed as var arguments to a function or procedure, since there is no known address of the property (at least, not always).


6.7.2 Indexed properties

If the property definition contains an index, then the read and write specifiers must be a function and a procedure. Moreover, these functions require an additional parameter : An integer parameter. This allows to read or write several properties with the same function. For this, the properties must have the same type. The following is an example of a property with an index:
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

When the compiler encounters an assignment to X, then SetCoord is called with as first parameter the index (1 in the above case) and with as a second parameter the value to be set. Conversely, when reading the value of X, the compiler calls GetCoord and passes it index 1. Indexes can only be integer values.

6.7.3 Array properties

Array properties also exist. These are properties that accept an index, just as an array does. The index can be one-dimensional, or multi-dimensional. In difference with normal (static or dynamic) arrays, the index of an array property doesn’t have to be an ordinal type, but can be any type.

A read specifier for an array property is the name method function that has the same return type as the property type. The function must accept as a sole argument a variable of the same type as the index type. For an array property, one cannot specify fields as read specifiers.

A write specifier for an array property is the name of a method procedure that accepts two arguments: the first argument has the same type as the index, and the second argument is a parameter of the same type as the property type. As an example, see the following declaration:
Type  
  TIntList = Class  
  Private  
    Function GetInt (I : Longint) : longint;  
    Function GetAsString (A : String) : String;  
    Procedure SetInt (I : Longint; Value : Longint;);  
    Procedure SetAsString (A : String; Value : String);  
  Public  
    Property Items [i : Longint] : Longint Read GetInt  
                                           Write SetInt;  
    Property StrItems [S : String] : String Read GetAsString  
                                            Write SetAsstring;  
  end;  

Var  
  AIntList : TIntList;

Then the following statements would be valid:
AIntList.Items[26] := 1;  
AIntList.StrItems['twenty-five'] := 'zero';  
WriteLn ('Item 26 : ',AIntList.Items[26]);  
WriteLn ('Item 25 : ',AIntList.StrItems['twenty-five']);

While the following statements would generate errors:
AIntList.Items['twenty-five'] := 1;  
AIntList.StrItems[26] := 'zero';

Because the index types are wrong.

Array properties can be multi-dimensional:
Type  
  TGrid = Class  
  Private  
    Function GetCell (I,J : Longint) : String;  
    Procedure SetCell (I,J : Longint; Value : String);  
  Public  
    Property Cellcs [Row,Col : Longint] : String Read GetCell  
                                            Write SetCell;  
  end;

If there are N dimensions, then the types of the first N arguments of the getter and setter must correspond to the types of the N index specifiers in the array property definition.

6.7.4 Default properties

Array properties can be declared as default properties. This means that it is not necessary to specify the property name when assigning or reading it. In the previous example, if the definition of the items property would have been

 Property Items[i : Longint]: Longint Read GetInt  
                                      Write SetInt; Default;

Then the assignment

AIntList.Items[26] := 1;

Would be equivalent to the following abbreviation.

AIntList[26] := 1;

Only one default property per class is allowed, but descendent classes can redeclare the default property.

6.7.5 Published properties

Classes compiled in the {$M+} state (such as TPersistent from the classes unit) can have a published section. For methods, fields and properties in the Published section, the compiler generates RTTI information (Run Time Type Information), which can be used to query the defined methods, fields and properties in the published section(s). The typinfo unit contains the necessary routines to query this information, and this unit is used in the streaming system in FPC in the classes unit.

The RTTI is generated regardless of what the read and write specifiers are: fields, functions/procedures or indexed functions/procedures.

Only class-typed fields can be published. For properties, any simple property whose size is less than or equal to a pointer, can be declared published: floats, integers, sets (with less than 32 distinct elements), enumerated, classes or dynamic arrays (not array properties).

Although run-time type information is available for other types, these types cannot be used for a property or field definition in a published section. The information is present to describe for example arguments of procedures or functions.

6.7.6 Storage information

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

6.7.7 Overriding and redeclaring properties

Properties can be both overridden and redeclared in descendent classes.

Property redeclaration takes action if the property type is declared, otherwise it is property override. The only difference is that property override replaces or extends the inherited modifiers with the new modifiers, whereas property redeclaration hides all inherited modifiers that are not present in the redeclaration. The type of the redeclared property does not have to be the same as the parent”s class property type.

The example below demonstrates the difference between property override and redeclaration.
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

TP1 extends property P with a default value, TPReadOnly redeclares property P as read-only.

Remark TP1 should set the default value of P to 1 in its constructor.

In case of both property redeclaration and property override, the access to the getter and setter is always static. I.e. property override acts only on the RTTI of the object and is not to be confused with method override.

The keyword “inherited” can be used to refer to the parent definition of the property. For example consider the following code:
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

TClassA redefines P as a character property instead of an integer property, but uses the parent”s P property to store the value.

Care must be taken when using virtual get/set routines for a property: setting the inherited property still observes the normal rules of inheritance for methods. Consider the following example:
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

In this case, when setting the inherited property P, the implementation TClassA.SetP1 will be called, because the SetP1 method is overridden.

If the parent class implementation of SetP1 must be called, then this must be called explicitly:
constructor TClassA.Create;  
begin  
  inherited SetP1(3);  
end;

The redeclared ancestor properties are also available from inside and outside the descendant object with a direct cast to the ancestor:
function GetP(const AClassA: TClassA): Integer;  
begin  
  Result := TAncestor(AClassA).P;  
end;

6.8 Class properties

Class properties are very much like global property definitions. They are associated with the class, not with an instance of the class.

A consequence of this is that the storage for the property value must be a class var, not a regular field or variable of the class: normal fields or variables are stored in an instance of the class.

Class properties can have a getter and setter method like regular properties, but these must be static methods of the class.

That means that the following contains a valid class property definition:
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

The reason for the requirement is that a class property is associated to the particular class in which it is defined, but not to descendent classes. Since class methods can be virtual, this would allow descendent classes to override the method, making them unsuitable for class property access.

6.9 Nested types, constants and variables

A class definition can contain a type section, const section and a variable section. The type and constant sections act as a regular type section as found in a unit or method/function/procedure implementation. The variables act as regular fields of the class, unless they are in a class var section, in which case they act as if they were defined at the unit level, within the namespace of the class (section 6.3, page 308).

However, the visibility of these sections does play a role: private and protected (strict or not) constants, types and variables can only be used as far as their visibility allows.

Public types can be used outside the class, by their full name:
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

Whereas
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

Will not compile and will return an error:
tt.pp(20,10) Error: identifier idents no member "TEnum"

Note that for writeable constants, the same rules apply as for class variables with regard to scope and overriding in descendents:
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

Will write
 2 2 2

But
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

Will write
 0 1 2
