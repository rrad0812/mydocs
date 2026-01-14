
# 4 Variables

[prev][f1] [content][f0] [next][f2]

## 4.1 Definition

Variables are explicitly named memory locations with a certain type. When assigning values to variables, the Free Pascal compiler generates machine code to move the value to the memory location reserved for this variable. Where this variable is stored depends on where it is declared:

- **Global variables** are variables declared in a unit or program, but not
  inside a procedure or function. They are stored in fixed memory locations, and are available during the whole execution time of the program.
- **Local variables** are declared inside a procedure or function. Their value is
  stored on the program stack, i. e. not at fixed locations.

The Free Pascal compiler handles the allocation of these memory locations transparently, although this location can be influenced in the declaration.

The Free Pascal compiler also handles reading values from or writing values to the variables transparently. But even this can be explicitly handled by the programmer when using properties.

Variables must be explicitly declared when they are needed. No memory is allocated unless a variable is declared. Using a variable identifier (for instance, a loop variable) which is not declared first, is an error which will be reported by the compiler.

## 4.2 Declaration

The variables must be declared in a variable declaration block of a unit or a procedure or function (section 16.5, page 923).

This means that the following are valid variable declarations:

```pascal
Var  
  curterm1 : integer;  

  curterm2 : integer; cvar;  
  curterm3 : integer; cvar; external;  

  curterm4 : integer; external name 'curterm3';  
  curterm5 : integer; external 'libc' name 'curterm9';  

  curterm6 : integer absolute curterm1;  

  curterm7 : integer; cvar;  export;  
  curterm8 : integer; cvar;  public;  
  curterm9 : integer; export name 'me';  
  curterm10 : integer; public name 'ma';  

  curterm11 : integer = 1;
```

The difference between these declarations is as follows:

- The first form (curterm1) defines a regular variable. The compiler manages
  everything by itself.
- The second form (curterm2) declares also a regular variable, but specifies that
  the assembler name for this variable equals the name of the variable as written in the source.
- The third form (curterm3) declares a variable which is located externally: the
  compiler will assume memory is located elsewhere, and that the assembler name of this location is specified by the name of the variable, as written in the source. The name may not be specified.
- The fourth form is completely equivalent to the third, it declares a variable  
  which is stored externally, and explicitly gives the assembler name of the location. If cvar is not used, the name must be specified.
- The fifth form is a variant of the fourth form, only the name of the library in
  which the memory is reserved is specified as well.
- The sixth form declares a variable (curterm6), and tells the compiler that it
  is stored in the same location as another variable (curterm1).
- The seventh form declares a variable (curterm7), and tells the compiler that
  the assembler label of this variable should be the name of the variable (case sensitive) and must be made public. i. e. it can be referenced from other object files.
- The eighth form (curterm8) is equivalent to the seventh: “public” is an alias
  for “export”.
- The ninth and tenth form are equivalent: they specify the assembler name of the
  variable.
- The eleventh form declares a variable (curterm11) and initializes it with a
  value (1 in the above case).

Note that assembler names must be unique. It’s not possible to declare or export two variables with the same assembler name. In particular, do not attempt to export variables with a public name that starts with FPC_; the compiler uses some internal system routines with this name.

## 4.3 Scope

Variables, just as any identifier, obey the general rules of scope. In addition, initialized variables are initialized when they enter scope:

- **Global initialized variables** are initialized once, when the program starts.
- **Local initialized variables** are initialized each time the procedure is
  entered.

Note that the behavior for local initialized variables is different from the one of a local typed constant. A local typed constant behaves like a global initialized variable.

## 4.4 Initialized variables

By default, simple variables in Pascal are not initialized after their declaration. Any assumption that they contain 0 or any other default value is erroneous: They can contain rubbish. To remedy this, the concept of initialized variables exists. The difference with normal variables is that their declaration includes an initial value, as can be seen in the diagram in the previous section.

Remark 2 exceptions to this rule exist:

- **Managed types** are an exception to this rule: Managed types are always
  initialized with a default value: in general this means setting the reference count to zero, or setting the pointer value of the type to Nil. See section 3.9, page 226
- **Global variables** are initialized with the equivalent of zero.

Note that the behavior of zeroing out certain variables can result in invalid content for variables:

```pascal
Type  
  TWeekDays =  
    (monday,tuesday,wednesday,thursday,friday,saturday,sunday);  
  TWeekend = saturday..sunday;  
 
var  
  W : TWeekend;  
 
begin  
  Writeln(W);  
end.
```

The above will result, when run, in an error:

```sh
Runtime error 107 at $000000000040024A  
$000000000040024A  
$000000000042BF70  
$00000000004001D2
```

Therefore is is Highly recommended to always initialize variables before using them.

This can be easily done in the declaration of the variables. Given the declaration:

```pascal
Var  
  S : String = 'This is an initialized string';
```

The value of the variable following will be initialized with the provided value. The following is an even better way of doing this:

```pascal
Const  
  SDefault = 'This is an initialized string';  
 
Var  
  S : String = SDefault;
```

Initialization is often used to initialize arrays and records. For arrays, the initialized elements must be specified, surrounded by round brackets, and separated by commas. The number of initialized elements must be exactly the same as the number of elements in the declaration of the type. As an example:

```pascal
Var  
  tt : array [1..3] of string[20] = ('ikke', 'gij', 'hij');  
  ti : array [1..3] of Longint = (1,2,3);
```

For constant records, each element of the record that you wish to initialize must be specified in the form Field: Value, separated by semicolons, and surrounded by round brackets.. You can omit fields that you don’t wish to initialize, in fact you can skip all fields. If you skip fields, the compiler will emit a warning.

As an example:

```pascal
Type  
  Point = record  
    X,Y : Real  
    end;  
Var  
  Origin : Point = (X:0.0; Y:0.0);  
  Partial : Point = (X:0.0);  
  Empty : Point = ();
```

The above declarations will result in the following warnings:

```sh
iv.pp(7,27) Warning: Some fields coming after "X" were not initialized  
iv.pp(8,20) Warning: Some fields coming after "" were not initialized
```

The order of the fields in a constant record needs to be the same as in the type declaration, otherwise a compile-time error will occur.

Remark It should be stressed that initialized variables are initialized when they come into scope, in difference with typed constants, which are initialized at program start. This is also true for local initialized variables. Local initialized variables are initialized whenever the routine is called. Any changes that occurred in the previous invocation of the routine will be undone, because they are again initialized.

Remark Care should be taken when using initialized pointer types such as PChars. In the following examples, S is a pointer, pointing to a block of constant (read-only) program data. Assigning a character in the string will therefore not work. Assigning S itself will of course work. The first routine will give an error, the second not:

```pascal
procedure foo1;  
var  
  s: PChar = 'PChar';  
begin  
  s[0] := 'a';  
end;  
 
procedure foo2;  
var  
  s: PChar;  
begin  
  s := 'PChar';  
  s[0] := 'a';  
end;
```

## 4.5 Initializing variables using default

Some variables must be initialized because they contain managed types. For variables that are declared in the var section of a function or in the main program, this happens automatically. For variables that are allocated on the heap, this is not necessarily the case.

For this, the compiler contains the Default intrinsic. This function accepts a type identifier as the argument, and will return a correctly initialized variable of that type. In essence, it will zero out the whole variable.

The following gives an example of its use:

```pascal
type  
  TRecord = record  
    i: LongInt;  
    s: AnsiString;  
  end;  
 
var  
  i: LongInt;  
  o: TObject;  
  r: TRecord;  
begin  
  i := Default(LongInt); // 0  
  o := Default(TObject); // Nil  
  r := Default(TRecord); // ( i: 0; s: '')  
end.

The case where a variable is allocated on the heap, is more interesting:
type  
  TRecord = record  
    i: LongInt;  
    s: AnsiString;  
  end;  
 
var  
  i: ^LongInt;  
  o: ^TObject;  
  r: ^TRecord;  
begin  
  i:=GetMem(SizeOf(Longint));  
  i^ := Default(LongInt); // 0  
  o:=GetMem(SizeOf(TObject));  
  o^ := Default(TObject); // Nil  
  r:=GetMem(SizeOf(TRecord));  
  r^ := Default(TRecord); // ( i: 0; s: '')  
end.
```

It works for all types, except the various file types (or complex types containing a file type).

**Remark**:

- For generics, the use of Default is especially useful, since the type of a
  variable may not be known during the declaration of a generic. For more information section 8.7, page 484.
- Function results are available as a Result identifier, and as such resemble
  variables. They are not variables, but are treated as passed-by-reference parameters. They are therefore not initialized.

## 4.6 Thread Variables

For a program which uses threads, the variables can be really global, i. e. the same for all threads, or thread-local: this means that each thread gets a copy of the variable. Local variables (defined inside a procedure) are always thread-local. Global variables are normally the same for all threads. A global variable can be declared thread-local by replacing the var keyword at the start of the variable declaration block with Threadvar:

```pascal
Threadvar  
  IOResult : Integer;
```

If no threads are used, the variable behaves as an ordinary variable. If threads are used then a copy is made for each thread (including the main thread). Note that the copy is made with the original value of the variable, not with the value of the variable at the time the thread is started.

Threadvars should be used sparingly: There is an overhead for retrieving or setting the variable’s value. If possible at all, consider using local variables; they are always faster than thread variables.

Threads are not enabled by default. For more information about programming threads, see the chapter on threads in the Programmer’s Guide.

## 4.7 Properties

A global block can declare properties, just as they could be defined in a class. The difference is that the global property does not need a class instance: there is only one instance of this property. Other than that, a global property behaves like a class property. The read/write specifiers for the global property must also be regular procedures, not methods.

The concept of a global property is specific to Free Pascal, and does not exist in Delphi. ObjFPC mode is required to work with properties.

The concept of a global property can be used to “hide” the location of the value, or to calculate the value on the fly, or to check the values which are written to the property.

The following is an example:

```pascal
{$mode objfpc}  
unit testprop;  

Interface  

Function GetMyInt : Integer;  
Procedure SetMyInt(Value : Integer);  
 
Property  
  MyProp : Integer Read GetMyInt Write SetMyInt;  
 
Implementation  
 
Uses sysutils;  
 
Var  
  FMyInt : Integer;  
 
Function GetMyInt : Integer;  
 
begin  
  Result:=FMyInt;  
end;  
 
Procedure SetMyInt(Value : Integer);  
 
begin  
  If ((Value mod 2)=1) then  
    Raise Exception.Create('MyProp can only contain even value');  
  FMyInt:=Value;  
end;  
 
end.
```

The read/write specifiers can be hidden by declaring them in another unit which must be in the uses clause of the unit. This can be used to hide the read/write access specifiers for programmers, just as if they were in a private section of a class (discussed below). For the previous example, this could look as follows:

```pascal
{$mode objfpc}  

unit testrw;  
 
Interface  
 
Function GetMyInt : Integer;  
Procedure SetMyInt(Value : Integer);  
 
Implementation  
 
Uses sysutils;  
 
Var  
  FMyInt : Integer;  
 
Function GetMyInt : Integer;  
 
begin  
  Result:=FMyInt;  
end;  
 
Procedure SetMyInt(Value : Integer);  
 
begin  
  If ((Value mod 2)=1) then  
    Raise Exception.Create('Only even values are allowed');  
  FMyInt:=Value;  
end;  
 
end.
```

The unit testprop would then look like:

```pascal
{$mode objfpc}  
unit testprop;  
 
Interface  
 
uses testrw;  
 
Property  
  MyProp : Integer Read GetMyInt Write SetMyInt;  
 
Implementation  
 
end.
```

More information about properties can be found in chapter 6, page 294.

[prev][f1] [content][f0] [next][f2]

[f1]: 03_types.md
[f0]: 00_contents.md
[f2]: 06_classes.md
