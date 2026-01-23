
# 2 Constants

[prev][f1] [content][f0] [next][f2]

Just as in Turbo Pascal, Free Pascal supports both ordinary and typed constants. They are declared in a constant declaration block in a unit, program or class, function or procedure declaration (section 16.5, page 923).

## 2.1 Ordinary constants

Ordinary constants declarations are constructed using an identifier name followed by an “=” token, and followed by an optional expression consisting of legal combinations of numbers, characters, boolean values or enumerated values as appropriate.

The compiler must be able to evaluate the expression in a constant declaration at compile time. This means that most of the functions in the Run-Time library cannot be used in a constant declaration. Operators such as +, -, *, /, not, and, or, div, mod, ord, chr, sizeof, pi, int, trunc, round, frac, odd can be used, however. For more information on expressions, see chapter 12, page 583.

When a previously declared ordinary constant is used in the code, the compiler will insert the actual value of the constant instead of the constant name. That is, the following two pieces of code are entirely equivalent:

```pascal
Const  
  One = 1;  

begin  
  Writeln(One);  
end.
```

The above will produce the same code as if one had written:

```pascal
begin  
  Writeln(1);  
end.
```

Only constants of the following types can be declared:

- **Ordinal** types
- **Set** types
- **Pointer** types** (but the only allowed value is Nil).
- **Real** types
- **Char**
- **String**.

The following are all valid constant declarations:

```pascal
Const  
  e = 2.7182818;                    { Real type constant. }  
  a = 2;                            { Ordinal (Integer) type constant. }  
  c = '4';                          { Character type constant. }  
  s = 'This is a constant string';  {String type constant.}  
  sc = chr(32)  
  ls = SizeOf(Longint);  
  P = Nil;  
  Ss = [1,2];
```

Assigning a value to an ordinary constant is not permitted. Thus, given the previous declaration, the following will result in a compiler error:

```pascal
  s := 'some other string';
```

For string constants, the type of the string is dependent on some compiler switches. If a specific type is desired, a typed constant should be used, as explained in the following section.

Prior to version 1.9, Free Pascal did not correctly support 64-bit constants. As of version 1.9, 64-bit constants can be specified.

## 2.2 Typed constants

Sometimes it is necessary to specify the type of a constant, for instance for constants of complex structures (defined later in the manual). Their definition is quite simple.

Contrary to ordinary constants, a value can be assigned to them at run-time. This is an old concept from Turbo Pascal, which has been replaced with support for initialized variables: For a detailed description, see section 4.4, page 234.

Support for assigning values to typed constants is controlled by the `{$J}` directive: it can be switched off, but is on by default (for Turbo Pascal compatibility). Initialized variables are always allowed.

**Napomena**:  
It should be stressed that typed constants are automatically initialized at program start. This is also true for local typed constants and initialized variables. Local typed constants are also initialized at program start. If their value was changed during previous invocations of the function, they will retain their changed value, i. e. they are not initialized each time the function is invoked.

## Resource strings

A special kind of constant declaration block is the `Resourcestring` block. Resourcestring declarations are much like constant string declarations: resource strings act as constant strings, but they can be localized by means of a set of special routines in the objpas unit. A resource string declaration block is only allowed in the Delphi or Objfpc modes.

The following is an example of a resourcestring definition:

```pascal
Resourcestring  

  FileMenu = '&File...';  
  EditMenu = '&Edit...';
```

All string constants defined in the resourcestring section are stored in special tables. The strings in these tables can be manipulated at runtime with some special mechanisms in the objpas unit.

Semantically, the strings act like ordinary constants; It is not allowed to assign values to them (except through the special mechanisms in the objpas unit). However, they can be used in assignments or expressions as ordinary string constants. The main use of the resourcestring section is to provide an easy means of internationalization.

More on the subject of resourcestrings can be found in the Programmer’s Guide, and in the objpas unit reference.

Remark Note that a resource string which is given as an expression will not change if the parts of the expression are changed:

```pascla
resourcestring  
  Part1 = 'First part of a long string.';  
  Part2 = 'Second part of a long string.';  
  Sentence = Part1+' '+Part2;
```

If the localization routines translate Part1 and Part2, the Sentence constant will not be translated automatically: it has a separate entry in the resource string tables, and must therefore be translated separately. The above construct simply says that the initial value of Sentence equals Part1+’ ’+Part2.

**Napomena**:  
Likewise, when using resource strings in a constant array, only the initial values of the resource strings will be used in the array: when the individual constants are translated, the elements in the array will retain their original value.

```pascal
resourcestring  
  Yes = 'Yes.';  
  No = 'No.';  

Var  
  YesNo : Array[Boolean] of string = (No,Yes);  
  B : Boolean;  

begin  
  Writeln(YesNo[B]);  
end.
```

This will print “Yes.” or “No.” depending on the value of B, even if the constants Yes and No have been localized by some localization mechanism.

[prev][f1] [content][f0] [next][f2]

[f1]: 01_pascal_tokens.md
[f0]: 00_contents.md
[f2]: 03_types.md
