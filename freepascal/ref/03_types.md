
# 3 Types

[prev][f1] [content][f0] [next][f2]

All variables have a type. Free Pascal supports the same basic types as Turbo Pascal, with some extra types from Delphi as well as some of its own.

The programmer can declare his own types, which is in essence defining an identifier that can be used to denote this custom type when declaring variables further in the source code. Declaring a type happens in a Type block (section 16.5, page 923), which is a collection of type declarations, separated by semicolons.

Each of these cases will be examined separately.

## 3.1 Base types

The base or simple types of Free Pascal are the Delphi types. We will discuss each type separately.

### 3.1.1 Ordinal types

With the exception of floating point value types, all base types are `ordinal` types. Ordinal types have the following characteristics:

- **Ordinal types** are countable and ordered, i. e. it is, in principle, possible to start counting them one by one, in a specified order. This property allows the operation of functions as `Inc`, `Ord`, `Dec` on ordinal types to be defined.
- **Ordinal values** have a smallest possible value. Trying to apply the `Pred` function on the smallest possible value will generate a `range check error` if range checking is enabled `{R+}`.
- **Ordinal values** have a largest possible value. Trying to apply the `Succ` function on the largest possible value will generate a `range check error` if range checking is enabled `{R+}`.

**Napomena**:  
`Int64` and `QWord` are considered ordinal types on 64-bit CPUs. On 32-bit types they have some of the characteristics of ordinals, but they cannot be used e. g. in `for` loops.

#### 3.1.1.1 Integers

A list of predefined integer types is presented in table (3.1).

Table 3.1: Predefined integer types

 Name |
 ---- |
 Integer |
 Shortint |
 SmallInt |
 Longint |
 Longword |
 Int64 |
 Byte |
 Word |
 Cardinal |
 QWord |
 ByteBool |
 WordBool |
 LongBool |
 QWordBool |

The integer types, and their ranges and sizes, that are predefined in Free Pascal are listed in table (3.2). Please note that the qword and int64 types are not true ordinals, so some Pascal constructs will not work with these two integer types.

Table 3.2: Predefined integer types

 Type | Range | Size in bytes |
 ----- | ----- | ------------ |
 Byte | 0 .. 255 | 1 |
 Shortint | -128 .. 127 | 1 |
 Smallint | -32768 .. 32767 | 2 |
 Word | 0 .. 65535 | 2 |
 Integer | either smallint or longint | size 2 or 4 |
 Cardinal | longword | 4 |
 Longint | -2147483648 .. 2147483647 | 4 |
 Longword | 0 .. 4294967295 | 4 |
 Int64 | -9223372036854775808 .. 9223372036854775807 | 8 |
 QWord | 0 .. 18446744073709551615 | 8 |

The integer type is an alias to the smallint type in the default Free Pascal mode. It is an alias for the longint type in either Delphi or ObjFPC mode. The cardinal type is currently always mapped to the longword type.

Remark The compiler decides on the type of an integer constant based on the value: An integer constant gets the smallest possible signed type. The first match in table (3.3) is used.

Table 3.3: Integer constant type mapping

 Range | Type |
------ | ---- |
-128..127 | Shortint |
128..255 | Byte |
-32768..32767 | Smallint |
32768..65535 | Word |
-2147483648..2147483647 | longint |
2147483648..4294967295 | Cardinal (longword) |
-9223372036854775808 .. 9223372036854775807 | Int64 |

That means constants in the range -128..127 are mapped to shortint, constants in range 128..255 are mapped to byte, etc. Constants in the range 2147483647..high(cardinal) are parsed as cardinals, and all decimal constants which do not fit either of the above ranges are parsed as 64-bit integer constants.

**Napomena**  
In newer Delphi versions, the longint type is platform and CPU dependent. This is not so in FPC, where longint is 32-bit on all platforms.

As a pascal compiler, Free Pascal does automatic type conversion and upgrading in expressions where different kinds of integer types are used:

- Every platform has a “native” integer size, depending on whether the platform is 8-bit, 16-bit,
  32-bit or 64-bit. E. g. on AVR this is 8-bit.
- Every integer smaller than the “native” size is promoted to a signed version of the “native”
  size. Integers equal to the “native” size keep their signedness.
- The result of binary arithmetic operators (+, -, *, etc.) is determined in the following way:
  - If at least one of the operands is larger than the native integer size, the result is chosen to be the smallest type that encompasses the ranges of the types of both operands. This means that mixing an unsigned with a smaller or equal in size signed will produce a signed type that is larger than both of them.
  - If both operands have the same signedness, the result is the same type as them. The only exception is subtracting (-): in the case of unsigned - unsigned subtracting produces a signed result in FPC (as in Delphi, but not in TP7).
  - Mixing signed and unsigned operands of the “native” int size produces a larger signed result. This means that mixing longint and longword on 32-bit platforms will produce an int64. Similarly, mixing byte and shortint on 8-bit platforms (AVR) will produce a smallint.

#### 3.1.1.2 Boolean types

Free Pascal supports the `Boolean` type, with its two predefined possible values `True` and `False`. These are the only two values that can be assigned to a `Boolean` type. Of course, any expression that resolves to a boolean value, can also be assigned to a boolean type.

Table 3.4: Boolean types

Name | Size | Ord(True) |
---- | ---- | --------- |
Boolean | 1 | 1 |
Boolean16 | 2 | 1 |
Boolean32 | 4 | 1 |
Boolean64 | 8 | 1 |
ByteBool | 1 | Any nonzero value |
WordBool | 2 | Any nonzero value |
LongBool | 4 | Any nonzero value |
QWordBool | 8 | Any nonzero value |

In addition to the simple `Boolean` type, the additional `Boolean16`, `Boolean32` and `Boolean64` types exist. These are in fact integer types, which are assignment-compatible with the simple boolean type. As an integer, the values for  `True` and `False` are `1` and `0`. This can be used to interface with C code that defines a boolean of this size with values `0` and `1`.

To make interfacing with C even easier, Free Pascal also supports the `ByteBool`, `WordBool`, `LongBool` and `QWordBool` types. These are of type `Byte`, `Word`, `Longint` or `Int64`, but are again assignment compatible with a `Boolean`. The only difference with the Boolean16/32/64 types is in what values are considered `true` or `false`: The value `False` is equivalent to `0` (zero) and any nonzero value is considered `True` when converting to a boolean value. A boolean value of `True` is converted to Not(0) in case it is assigned to a variable of type `ByteBool`, `WordBool`, `LongBool` or `QWordBool`.

Assuming B to be of type `Boolean`, the following are valid assignments:

> B := True;  
> B := False;  
> B := 1<>2;  { Results in B := True }

Boolean expressions are also used in conditions.

**Napomena**:  
In Free Pascal, boolean expressions are by default always evaluated in such a way that when the result is known, the rest of the expression will no longer be evaluated: this is called short-cut boolean evaluation.

In the following example, the function Func will never be called, which may have strange side-effects.

```pascal  
 B := False;  
 A := B and Func;
```

Here Func is a function which returns a `Boolean` type.

This behavior is controllable by the `{$B}` compiler directive.

#### 3.1.1.3 Enumeration types

`Enumeration types` are supported in Free Pascal. On top of the Turbo Pascal implementation, Free Pascal allows also a C-style extension of the enumeration type, where a value is assigned to a particular element of the enumeration list.
(see chapter 12, page 583 for how to use expressions) When using assigned enumerated types, the assigned elements must be in ascending numerical order in the list, or the compiler will complain. The expressions used in assigned enumerated elements must be known at compile time. So the following is a correct enumerated type declaration:

```pascal
Type  
  Direction = ( North, East, South, West );
```

A C-style enumeration type looks as follows:

```pascal
Type  
  EnumType = (one, two, three, forty := 40, fortyone);
```

or you can use

```pascal
Type  
  EnumType = (one, two, three, forty = 40, fortyone);
```

The latter notation is mandatory in mode DELPHI.

As a result, the ordinal number of forty is 40, and not 3, as it would be when the ’:= 40’ wasn’t present. The ordinal value of fortyone is then 41, and not 4, as it would be when the assignment wasn’t present. After an assignment in an enumerated definition the compiler adds 1 to the assigned value to assign to the next enumerated value.

When specifying such an enumeration type, it is important to keep in mind that the enumerated elements should be kept in ascending order. The following will produce a compiler error:

```pascal
Type  
  EnumType = (one, two, three, forty := 40, thirty := 30);
```

It is necessary to keep forty and thirty in the correct order. When using enumeration types it is important to keep the following points in mind:

- The `Pred` and `Succ` functions cannot be used on this kind of enumeration types. Trying to do this anyhow will result in a compiler error.
- Enumeration types are stored using a default, independent of the actual number of values: the compiler does not try to optimize for space. This behavior can be changed with the `{$PACKENUM n}` compiler directive, which tells the compiler the minimal number of bytes to be used for enumeration types. For instance:

```pascal
Type  
{$PACKENUM 4}  
  LargeEnum = ( BigOne, BigTwo, BigThree );

{$PACKENUM 1}  
  SmallEnum = ( one, two, three );

Var S : SmallEnum;  
    L : LargeEnum;  
begin  
  WriteLn ('Small enum : ',SizeOf(S));  
  WriteLn ('Large enum : ',SizeOf(L));  
end.
```

will, when run, print the following:

```pascal
Small enum : 1  
Large enum : 4
```

More information can be found in the Programmer’s Guide, in the compiler directives section.

#### 3.1.1.4 Subrange types

A subrange type is a range of values from an ordinal type (the host type). To define a subrange type, one must specify its limiting values: the highest and lowest value of the type.

Some of the predefined integer types are defined as subrange types:

```pascal
Type  
  Longint  = $80000000..$7fffffff;  
  Integer  = -32768..32767;  
  shortint = -128..127;  
  byte     = 0..255;  
  Word     = 0..65535;
```

Subrange types of enumeration types can also be defined:

```pascal
Type  
  Days = (monday,tuesday,wednesday,thursday,friday,  
          saturday,sunday);  
  WorkDays = monday .. friday;  
  WeekEnd = Saturday .. Sunday;
```

#### 3.1.1.5 Character types

A `character type` is also an ordinal type: characters are ordered, can be counted. There are 2 character types:

- `AnsiChar`  
  This is a 1-byte character. The interpretation of the character depends on the codepage.
- `WideChar`
  This is a 2-byte character. The interpretation of the character depends on the codepage.

Characters can be used in a loop, one can use `prev` and `succ` on it, as well as `ord`.

For more information on characters, see section 3.2, page 108.

### 3.1.2 Real types

Free Pascal uses the math coprocessor (or emulation) for all its floating-point calculations. The Real native type is processor dependent, but it is either a Single or a Double. Only the IEEE floating point types are supported, and these depend on the target processor and emulation options. The true Turbo Pascal compatible types are listed in table (3.5).

Table 3.5: Supported Real types

Type | Range | Significant digits | Size |
---- | ----- | ------------------ | ---- |
Real | platform dependant | ??? | 4 or 8 |
Single | 1.5E-45 .. 3.4E38 | 7–8 | 4 |
Double | 5.0E-324 .. 1.7E308 | 15–16 | 8 |
Extended | 1.9E-4932 .. 1.1E4932 | 19–20 | 10 |
Comp | -2E64+1 .. 2E63-1 | 19–20 | 8 |
Currency | -922337203685477.5808 .. 922337203685477.5807 | 19–20 | 8 |

The Comp type is, in effect, a 64-bit integer and is not available on all target platforms. To get more information on the supported types for each platform, refer to the Programmer’s Guide.

The currency type is a fixed-point real data type which is internally used as an 64-bit integer type (automatically scaled with a factor 10000), this minimizes rounding errors. This type should be used with care: when used in expressions using e.g. multiplication, the evaluation of the expression may go wrong (losing precision) if intermediate results fall outside the currency range.

Note that not all float types are available on all platforms. The single float type is the only one guaranteed to be available on all platforms that have floating point support (so e.g. AVR does not have it). The double type is available on all platforms with coprocessor, and the extended type is available on all Intel x86 processors, except on the Windows 64-bit platform. More details on availability can be found in the Programmer’s Guide.

## 3.2 Character types

### 3.2.1 Char or AnsiChar

Free Pascal supports the type Char. A Char is exactly 1 byte in size, and contains one ASCII character.

A character constant can be specified by enclosing the character in single quotes, as follows: ’a’ or ’A’ are both character constants.

A character can also be specified by its character value (commonly an ASCII code), by preceding the ordinal value with the number symbol (#). For example specifying `#65` would be the same as `’A’`.

Also, the caret character (`^`) can be used in combination with a letter to specify a character with ASCII value less than 27. Thus `^G` equals `#7` - G is the seventh letter in the alphabet. The compiler is rather sloppy about the characters it allows after the caret, but in general one should assume only letters.

When the single quote character must be represented, it should be typed two times successively, thus `’’’’` represents the single quote character.

To distinguish `Char` from `WideChar`, the system unit also defines the AnsiChar type, which is the same as the char type. In future versions of FPC, the `Char` type may become an alias for either `WideChar` or `AnsiChar`.

Free Pascal supports the type `WideChar`. A `WideChar` is exactly 2 bytes in size, and contains one `UNICODE` character in `UTF-16` encoding.

A unicode character can be specified by its character value (an UTF-16 code), by preceding the ordinal value with the number symbol (`#`).

A normal ansi (1-byte) character literal can also be used for a widechar, the compiler will automatically convert it to a 2-byte UTF-16 character.

The following defines some greek characters (phi, omega):

```pascal
Const  
  C3 : widechar = #$03A8;  
  C4 : widechar = #$03A9;
```

The same can be accomplished by typecasting a word to widechar:

```pascal
Const  
  C3 : widechar = widechar($03A8);  
  C4 : widechar = widechar($03A9);
```

### 3.2.3 Other character types

Free Pascal defines some other character types in the system unit such as `UCS2Char`, `UCS4Char`, `UniCodeChar`. However, no special support for these character types exists, they have been defined for Delphi compatibility only.

### 3.2.4 Single-byte String types

Free Pascal supports the `String` type as it is defined in Turbo Pascal: a sequence of single-byte characters with an optional size specification. It also supports ansistrings (with unlimited length) and codepage information1 as in Delphi.

To declare a variable as a string, use the following type specification:

- If there is a size specifier (using square brackets), then its maximum value –
  indicating the maximum size of the string – is 255. If there is a codepage specifier, (using round brackets) it indicates an ansistring with associated code page information.

  The meaning of a string declaration statement without size and code page indication is interpreted differently depending on the {$H} switch:

```pascal
var  
A : String;
```

- If no size and code page indication indication is present, the above
  declaration can declare an ansistring or a short string.

Whatever the actual type, single byte strings can be used interchangeably. The compiler always takes care of the necessary type conversions. Note, however, that the result of an expression that contains ansistrings and short strings will always be an ansistring.

### 3.2.5 Short strings

A string declaration declares a short string in the following cases:

- If the `$H` switch is off: `{$H-}`, the string declaration will always be a short string declaration.
- If the switch is on `{$H+}`, and there is a maximum length (the size) specifier, the declaration is a short string declaration.

Short strings are always assumed to use the system code page. The predefined type `ShortString` is defined as a string of size 255:

```pascal
ShortString = String[255];
```

If the size of the string is not specified, 255 is taken as a default. The actual length of the string can be obtained with the `Length` standard runtime routine. For example in:

```pascal
{$H-}  
 
Type  
  NameString = String[10];  
  StreetString = String;
```

`NameString` can contain a maximum of 10 characters. While `StreetString` can contain up to 255 characters.

**Napomena**  
Short strings have a maximum length of 255 characters: when specifying a maximum length, the maximum length may not exceed 255. If a length larger than 255 is attempted, then the compiler will give an error message:

```pascal
Error: string length must be a value from 1 to 255
```

For short strings, the length is stored in the character at index 0. Old Turbo Pascal code relies on this, and it is implemented similarly in Free Pascal.

Despite this, to write portable code, it is best to set the length of a shortstring with the `SetLength` call, and to retrieve it with the `Length` call. These functions will always work, whatever the internal representation of the shortstrings or other strings in use: this allows easy switching between the various string types.

### 3.2.6 Ansistrings

Ansistrings are strings that have no length limit, and have a code page associated with them. They are reference counted and are guaranteed to be null terminated.

Internally, an ansistring is treated as a pointer: the actual content of the string is stored on the heap, as much memory as needed to store the string content is allocated.

If no codepage is given in the declaration, the system codepage is assumed. What codepage this is, is determined by the DefaultSystemCodePage constant in the system unit.

This is all handled transparently, i. e. they can be manipulated as a normal short string. Ansistrings can be defined using the predefined AnsiString type or using the string keyword in mode {$H+}.

**Napomena**  
The null-termination does not mean that null characters (char(0) or #0) cannot be used: the null-termination is not used internally, but is there for convenience when dealing with external routines that expect a null-terminated string (as most C routines do).

If the `{$H}` switch is on, then a string definition using the regular String keyword that doesn’t contain a length specifier, will be regarded as an ansistring as well. If a length specifier is present, a short string will be used, regardless of the {$H} setting.

If the string is empty (’’), then the internal pointer representation of the string pointer is `Nil`. If the string is not empty, then the pointer points to a structure in heap memory.

The internal representation as a pointer, and the automatic null-termination make it possible to typecast an `ansistring` to a `pchar`. If the string is empty (so the pointer is Nil) then the compiler makes sure that the typecast `pchar` will point to a null byte.

Assigning one `ansistring` to another doesn’t involve moving the actual string. A statement

```pascal
  S2:=S1;
```

results in the reference count of S2 being decreased with 1, The reference count of S1 is increased by 1, and finally S1 (as a pointer) is copied to S2. This is a significant speed-up in the code.

If the reference count of a string reaches zero, then the memory occupied by the string is deallocated automatically, and the pointer is set to Nil, so no memory leaks arise.

When an ansistring is declared, the Free Pascal compiler initially allocates just memory for a pointer, not more. This pointer is guaranteed to be Nil, meaning that the string is initially empty. This is true for local and global ansistrings or ansistrings that are part of a structure (arrays, records or objects).

**Napomena**  
Note that a function result in this regard is considered equivalent to a var parameter and hence will not be initialized to Nil. As a consequence it may point to a legitimate non-Nil ansistring when the function begins.

This does introduce an overhead. For instance, declaring:

```pascal
var  
  A : Array[1..100000] of string;
```

Will copy the value Nil 100,000 times into A. When A goes out of scope, then the reference count of the 100,000 strings will be decreased by 1 for each of these strings. All this happens invisible to the programmer, but when considering performance issues, this is important.

Memory for the string content will be allocated only when the string is assigned a value. If the string goes out of scope, then its reference count is automatically decreased by 1. If the reference count reaches zero, the memory reserved for the string is released.

If a value is assigned to a character of a string that has a reference count greater than 1, such as in the following statements:

```pascal
  S:=T;  { reference count for S and T is now 2 }  
  S[I]:='@';
```

then a copy of the string is created before the assignment. This is known as copy-on-write semantics. It is possible to force a string to have reference count equal to 1 with the UniqueString call:

```pascal
  S:=T;  
  R:=T; // Reference count of T is at least 3  
  UniqueString(T);  
  // Reference count of T is guaranteed 1
```

It’s recommended to do this e. g. when typecasting an `ansistring` to a `PChar var` and passing it to a C routine that modifies the string.

The `Length` function must be used to get the length of an ansistring: the length is not stored at character 0 of the ansistring. The construct

```pascal
  L:=ord(S[0]);
```

which was valid for Turbo Pascal shortstrings, is no longer correct for Ansistrings. The compiler will warn if such a construct is encountered.

To set the length of an ansistring, the SetLength function must be used. Constant ansistrings have a reference count of -1 and are treated specially, The same remark as for Length must be given: The construct

```pascal
  L:=12;  
  S[0]:=Char(L);
```

which was valid for Turbo Pascal shortstrings, is no longer correct for Ansistrings. The compiler will warn if such a construct is encountered.

Ansistrings are converted to short strings by the compiler if needed, this means that the use of ansistrings and short strings can be mixed without problems.

Ansistrings can be typecast to PChar or Pointer types:

```pascal
Var P : Pointer;  
    PC : PChar;  
    S : AnsiString;  
 
begin  
  S :='This is an ansistring';  
  PC:=PChar(S);  
  P :=Pointer(S);
```

There is a difference between the two typecasts. When an empty ansistring is typecast to a pointer, the pointer will be Nil. If an empty ansistring is typecast to a PChar, then the result will be a pointer to a zero byte (an empty string).

The result of such a typecast must be used with care. In general, it is best to consider the result of such a typecast as read-only, i. e. only suitable for passing to a procedure that needs a constant pchar argument.

It is therefore not advisable to typecast one of the following:

**Expressions**:
Strings that have reference count larger than 1. In this case you should call Uniquestring to ensure the string has reference count 1.

### 3.2.7 Code page conversions

Since strings have code page information associated with them, it is important to know which code page a string uses:

- Short strings always use the system code page.
- Plain ansistrings use the system code page.
- Single byte strings with declared code page use that code page.
- The RawByteString type has no code page information associated with it.
- Constant strings have the code page of the source file. If none is specified
  the system codepage is used (CP_ACP).
  See Programmer’s Guide, `{$CODEPAGE }` directive.

This code page is called the declared code page.

The compiler will convert the code page of strings as needed: When assigning a string, the actual codepage of the source string will be converted to the declared code page of the target string if the declared source and target code pages are different.

If a string with a declared page SOURCE_CP assigned to a string with declared code page DEST_CP, in a file with code page CODE_CP then the following describes the mechanism:

- if (SOURCE_CP=CP_NONE) or (DEST_CP = CP_NONE), see RawByteString.
- if (CODE_CP ¡¿ CP_ACP), then if (DEST_CP = CP_ACP) and (SOURCE_CP = CODE_CP)
  or vice versa, no conversion will occur, even if at run time DefaultSystemCodePage has a different value from SOURCE_CP.
- The reason for this “(CODE_CP ¡¿CP_ACP)” condition is backward compatibility
  with previous FPC versions: While they did not support AnsiStrings with arbitrary code pages, they did always reinterpret AnsiStrings according to the current value of the system code page.
- Otherwise,
  if (SOURCE_CP ¡¿ DEST_CP), the string data will be converted from codepage SOURCE_CP to codepage DEST_CP before assignment, whereby CP_ACP will be interpreted as the current value of DefaultSystemCodePage. Otherwise,
  if (SOURCE_CP = DEST_CP), no codepage conversion will be performed.

These rules mean that it is perfectly possible for an AnsiString variable to get a code page that differs from its declared code page. E. g. in the third case SOURCE_CP could be CP_ACP, while after the assignment it may have a dynamic code page equal to DefaultSystemCodePage.

**Note**:  
As mentioned above, whether or not a potential code page conversion happens only depends on the declared code pages of the involved strings. This means that if you assign one AnsiString(X) to another AnsiString(X) and the former’s dynamic code was different from X, the string data will not be converted to code page X by the assignment.

All this means that in the following code:

```pascal
{$h+}  
uses sysutils;  
 
Type  
  TString1 = Type String(1252);  
  TString2 = Type String(1251);  
 
Var  
  A : TString1;  
  B : TString2;  
 
begin  
  A:='123'+'345'+intToStr(123);  
  B:=A;  
  Writeln('B: "',B,'" : ',StringRefCount(B),' -> ',StringCodePage(B));  
  Writeln('A: "',A,'" : ',StringRefCount(A),' -> ',StringCodePage(A));  
end.

This will print:
B: "123345123" : 1 -> 1251  
A: "123345123" : 1 -> 1252
```

During the assignment of A to B, the contents of string A is converted to the codepage of string B. Note that if a code page conversion takes place, the reference count mechanism is not used: a new string will be allocated.

This automated conversion of code pages can slow down the code seriously, so care must be taken to see to it that the code page conversions are limited to a minimum.

The code page of a string can be set explicitly using the SetCodePage routine of the system unit. Calling this routine will convert the value of a string to the requested code page.

**Napomena**:  
Code page conversions can result in loss of data: if a certain character cannot be represented in the target code page, the output for that character is undefined.

**Napomena**  
When a string whose static code page equals the source file code page, to anything with code page CP_ACP (i. e., a plain ansistring, shortstring, or pchar), no conversion will be performed either. No code page conversion is done when s can result in loss of data: if a certain character cannot be represented in the target code page, the output for that character is undefined.

**Napomena**  
Code page support requires quite some helper routines, these are implemented in the unicodestring manager. On windows, the system routines are used for this. On Unices, the cwstring unit can be used to link to the C library and use the C library conversion support. Alternatively, the fpwidestring unit contains a unicodestring manager implemented natively in Object Pascal.

### 3.2.8 RawByteString

The predefined `RawByteString` type is an ansistring string type without codepage information (CP_NONE):

```pascal
Type  
  RawByteString = type ansistring(CP_NONE);
```

It is treated specially in the sense that if the conversion routines encounter CP_NONE in a source or target string, no code page conversion is performed, the code page of the source string is preserved.

For this reason, most single-byte string routines in the system and sysutils units use the RawByteString type.

### 3.2.9 UTF8String

Single-byte code page strings can only store the characters available in that code page. Characters that are not present in the code page, cannot be represented by that string. The UTF-8 unicode encoding is an encoding that can be used with single-byte strings: The ASCII characters (ordinal value ¡128) in this encoding map exactly to the CP_ACP encoding. This fact is used to define a single byte string type that can contain all characters:

```pascal
Type  
  UTF8String = type AnsiString(CP_UTF8);
```

The UTF8string string type can be used to represent all Unicode characters. This power comes as a price, though. Since a unicode character may require several bytes to be represented in the UTF-8 encoding, there are two points to take care of when using UTF8String:

- The character index – which retrieves a byte-sized char at a certain position
  – must be used with care: the expression S[i] will not necessarily be a valid character for a string S of type UTF8String.
- The byte length of the string is not equal to the number of characters in the
  string. The standard function length cannot be used to get the character length, it will always return the byte length.

For all other code pages, the number of characters in a single-byte code page string is equal to the byte length of the string.

### 3.2.10 Multi-byte String types

For multi-byte string types, the basic character has a size of at least 2. This means it can be used to store a unicode character in UTF16 or UCS2 encoding.

**UnicodeStrings**:

Unicodestrings (used to represent unicode character strings) are implemented in much the same way as ansistrings: reference counted, null-terminated arrays, only they are implemented as arrays of WideChars instead of regular Chars. A WideChar is a two-byte character (an element of a DBCS: Double Byte Character Set). Mostly the same rules apply for UnicodeStrings as for AnsiStrings. The compiler transparently converts UnicodeStrings to AnsiStrings and vice versa.

Similarly to the typecast of an Ansistring to a PChar null-terminated array of characters, a UnicodeString can be converted to a PUnicodeChar null-terminated array of characters. Note that the PUnicodeChar array is terminated by 2 null bytes instead of 1, so a typecast to a pchar is not automatic.

The compiler itself provides no support for any conversion from Unicode to ansistrings or vice versa. The system unit has a unicodestring manager record, which can be initialized with some OS-specific unicode handling routines. For more information, see the system unit reference.

A unicode string literal can be constructed in a similar manner as a widechar:

```pascal
Const  
  ws2: unicodestring = 'phi omega : '#$03A8' '#$03A9;
```

**WideStrings**:

The Widestring type (used to represent unicode character strings in COM applications) is implemented in much the same way as Unicodestring on Windows, and on other platforms, they are simply the same type. If interaction with COM is not required, the UnicodeString type should be used.

On Windows, unlike UnicodeString, the WideString type is not reference counted, and are allocated with a special windows function which allows them to be used for OLE automation. This means they are implemented as null-terminated arrays of WideChars instead of regular Chars. WideString obeys the same rules as for UnicodeStrings. Similar to unicodestrings, the compiler transparenty converts WideStrings to AnsiStrings and vice versa.

For typecasting and conversion, the same rules apply as for the UnicodeString type.

Note that on windows, because a WideString string is allocated using a special windows function, the memory layout differs from UnicodeString. The length for instance is stored in bytes rather than characters.

### 3.2.11 Constant strings

To specify a constant string, it must be enclosed in single-quotes, just as a Char type, only now more than one character is allowed. Given that S is of type String, the following are valid assignments:

```pascal
S := 'This is a string.';  
S := 'One'+', Two'+', Three';  
S := 'This isn't difficult!';  
S := 'This is a weird character : '#145' !';
```

As can be seen, the single quote character is represented by 2 single-quote characters next to each other. Strange characters can be specified by their character value (usually an ASCII code). The example shows also that two strings can be added. The resulting string is just the concatenation of the first with the second string, without spaces in between them. Strings can not be subtracted, however.

Whether the constant string is stored as an ansistring or a short string depends on the settings of the {$H} switch

### 3.2.12 PChar – Null terminated strings

Free Pascal supports the Delphi implementation of the PChar type. PChar is defined as a pointer to a Char type, but allows additional operations. The PChar type can be understood best as the Pascal equivalent of a C-style null-terminated string, i. e. a variable of type PChar is a pointer that points to an array of type Char, which is ended by a null-character (#0). Free Pascal supports initializing of PChar typed constants, or a direct assignment. For example, the following pieces of code are equivalent:

```pascal
program one;  

var P : PChar;  
begin  
  P := 'This is a null-terminated string.';  
  WriteLn (P);  
end.

Results in the same as
program two;  
const P : PChar = 'This is a null-terminated string.';  
begin  
  WriteLn (P);  
end.
```

These examples also show that it is possible to write the contents of the string to a file of type Text. The strings unit contains procedures and functions that manipulate the PChar type as in the standard C library. Since it is equivalent to a pointer to a type Char variable, it is also possible to do the following:

Program three;  

```pascal
Var S : tring[30];  
    P : PChar;  
begin  
  S := 'This is a null-terminated string.'#0;  
  P := @S[1];  
  WriteLn (P);  
end.
```

This will have the same result as the previous two examples. Null-terminated strings cannot be added as normal Pascal strings. If two PChar strings must be concatenated; the functions from the unit strings must be used.

However, it is possible to do some pointer arithmetic. The operators + and - can be used to do operations on PChar pointers. In table (3.6), P and Q are of type PChar, and I is of type Longint.

Table 3.6: PChar pointer arithmetic

Operation | Result |
--------- | ------ |
P + I | Adds I to the address pointed to by P. |
I + P | Adds I to the address pointed to by P. |
P - I | Subtracts I from the address pointed to by P. |
P - Q | Returns, as an integer, the distance between two addresses (or the number of characters between P and Q) |

### 3.2.13 String sizes

The memory occupied by a string depends on the string type. Some string types allocate the string data in memory on the heap, others have the string data on the stack. Table table (3.7) summarizes the memory usage of the various string types for the various string types. In the table, the following symbolic constants are used:

- L is the actual length of the string.
  HS depends on the version of Free Pascal, but is 16 bytes as of Free Pascal 2.7.1.
- UHS size is 8 bytes for all versions of Free Pascal.
  On Windows, WHS size is 4 bytes for all versions of Free Pascal. On all other platforms, WHS equals UHS because the WideString type equals the UnicodeString type.

Table 3.7: String memory sizes

String type | Stack size | heap size |
----------- | ---------- | --------- |
Shortstring | Declared length + 1 | 0 |
Ansistring | Pointer size | L + 1 + HS |
Widestring | Pointer size | 2*(L + 1) + WHS |
UnicodeString | Pointer size | 2*(L + 1) + UHS |
Pchar | Pointer size | L+1 |

## 3.3 Structured Types

A structured type is a type that can hold multiple values in one variable. Structured types can be nested to unlimited levels.

Unlike Delphi, Free Pascal does not support the keyword `Packed` for all structured types. In the following sections each of the possible structured types is discussed. It will be mentioned when a type supports the `packed` keyword.

### 3.3.1 Packed structured types

When a structured type is declared, no assumptions should be made about the internal position of the elements in the type. The compiler will lay out the elements of the structure in memory as it thinks will be most suitable. That is, the order of the elements will be kept, but the location of the elements are not guaranteed, and is partially governed by the `$PACKRECORDS` directive (this directive is explained in the Programmer’s Guide).

However, Free Pascal allows controlling the layout with the `Packed` and `Bitpacked` keywords. The meaning of these words depends on the context:

- **Bitpacked**
  In this case, the compiler will attempt to align ordinal types on bit boundaries, as explained below.
- **Packed**
  The meaning of the Packed keyword depends on the situation:
  - In MACPAS mode, it is equivalent to the `Bitpacked` keyword.
  - In other modes, with the `$BITPACKING` directive set to ON, it is also
    equivalent to the `Bitpacked` keyword.
  - In other modes, with the `$BITPACKING` directive set to OFF, it signifies
    normal packing on byte boundaries.

    Packing on byte boundaries means that each new element of a structured type starts on a byte boundary.

The byte packing mechanism is simple: the compiler aligns each element of the structure on the first available byte boundary, even if the size of the previous element (small enumerated types, subrange types) is less than a byte.

When using the bit packing mechanism, the compiler calculates for each ordinal type how many bits are needed to store it. The next ordinal type is then stored on the next free bit. Non-ordinal types – which include but are not limited to – sets, floats, strings, (bitpacked) records, (bitpacked) arrays, pointers, classes, objects, and procedural variables, are stored on the first available byte boundary.

Note that the internals of the bitpacking are opaque: they can change at any time in the future. What is more: the internal packing depends on the endianness of the platform for which the compilation is done, and no conversion between platforms are possible. This makes bitpacked structures unsuitable for storing on disk or transport over networks. The format is however the same as the one used by the GNU Pascal Compiler, and the Free Pascal team aims to retain this compatibility in the future.

There are some more restrictions to elements of bitpacked structures:

- The address cannot be retrieved, unless the bit size is a multiple of 8 and
  the element happens to be stored on a byte boundary.
- An element of a bitpacked structure cannot be used as a var parameter, unless
  the bit size is a multiple of 8 and the element happens to be stored on a byte boundary.

To determine the size of an element in a bitpacked structure, there is the `BitSizeOf` function. It returns the size – in bits – of the element. For other types or elements of structures which are not bitpacked, this will simply return the size in bytes multiplied by 8, i. e., the return value is then the same as 8*SizeOf.

The size of bitpacked records and arrays is limited:

- On 32 bit systems the maximal size is 229 bytes (512 MB).
- On 64 bit systems the maximal size is 261 bytes.

The reason is that the offset of an element must be calculated with the maximum integer size of the system.

### 3.3.2 Arrays

Free Pascal supports arrays as in Turbo Pascal. Multi-dimensional arrays and (bit)packed arrays are also supported, as well as the dynamic arrays of Delphi:

#### 3.3.2.1 Static arrays

When the range of the array is included in the array definition, it is called a `static` array. Trying to access an element with an index that is outside the declared range will generate a run-time error (if range checking `{R+}` is on). The following is an example of a valid array declaration:

```pascal
Type  
  RealArray = Array [1..100] of Real;
```

Valid indexes for accessing an element of the array are between 1 and 100, where the borders 1 and 100 are included. As in Turbo Pascal, if the array component type is in itself an array, it is possible to combine the two arrays into one multi-dimensional array. The following declaration:

```pascal
Type  
   APoints = array[1..100] of Array[1..3] of Real;
```

is equivalent to the declaration:

```pascal
Type  
   APoints = array[1..100, 1..3] of Real;
```

The functions `High` and `Low` return the high and low bounds of the leftmost index type of the array. In the above case, this would be 100 and 1. You should use them whenever possible, since it improves maintainability of your code. The use of both functions is just as efficient as using constants, because they are evaluated at compile time.

When static array-type variables are assigned to each other, the contents of the whole array is copied. This is also true for multi-dimensional arrays:

```pascal
program testarray1;  
 
Type  
  TA = Array[0..9,0..9] of Integer;  
 
var  
  A,B : TA;  
  I,J : Integer;  
begin  
  For I:=0 to 9 do  
    For J:=0 to 9 do  
      A[I,J]:=I*J;  
  For I:=0 to 9 do  
    begin  
    For J:=0 to 9 do  
      Write(A[I,J]:2,' ');  
    Writeln;  
    end;  
  B:=A;  
  Writeln;  
  For I:=0 to 9 do  
    For J:=0 to 9 do  
      A[9-I,9-J]:=I*J;  
  For I:=0 to 9 do  
    begin  
    For J:=0 to 9 do  
      Write(B[I,J]:2,' ');  
    Writeln;  
    end;  
end.
```

The output of this program will be two identical matrices.

#### 3.2.2.2 Dynamic arrays

As of version 1.1, Free Pascal also knows dynamic arrays: In that case the array range is omitted, as in the following example:

```pascal
Type  
  TByteArray = Array of Byte;
```

When declaring a variable of a dynamic array type, the initial length of the array is zero. The actual length of the array must be set with the standard SetLength function, which will allocate the necessary memory to contain the array elements on the heap.

The following example will set the length to 1000:

```pascal
Var  
  A : TByteArray;  
 
begin  
  SetLength(A,1000);
```

After a call to SetLength, valid array indexes are 0 to 999: the array index is always zero-based.

SetLength can also be used for multi-dimensional arrays. The following example will create a “rectangular” array:

```pascal
Var  
  A : Array of TByteArray;  
 
begin  
  SetLength(A, 10, 100);
```

After a call to `SetLength`, valid array indexes are 0 to 9 for the first dimension, and 0 to 99 for the second dimension.

In difference with static multi-dimensional arrays, dynamic arrays do not need to be “rectangular”, i. e. the various elements can have different lengths:

```pascal
var  
  a: array of array of array of LongInt;  
  i, j, k: LongInt;  
begin  
  SetLength(a, 10, 5);  
  SetLength(a[5], 3);  
 
  for i := Low(a) to High(a) do  
    for j := Low(a[i]) to High(a[i]) do begin  
      SetLength(a[i, j], i * 10 + j);  
      for k := Low(a[i, j]) to High(a[i, j]) do  
        a[i, j, k] := i * 10000 + j * 100 + k;  
    end;  
 
  for i := Low(a) to High(a) do begin  
    for j := Low(a[i]) to High(a[i]) do begin  
      for k := Low(a[i, j]) to High(a[i, j]) do  
        Writeln(a[i, j, k]);  
      Writeln('-------');  
    end;  
    Writeln('=======');  
  end;  
end.
```

Note that the length of the array is set in elements, not in bytes of allocated memory (although these may be the same). The amount of memory allocated is the size of the array multiplied by the size of 1 element in the array. The memory will be disposed of at the exit of the current procedure or function.

It is also possible to resize the array: in that case, as much of the elements in the array as will fit in the new size, will be kept. The array can be resized to zero, which effectively resets the variable.

At all times, trying to access an element of the array with an index that is not in the current length of the array will generate a run-time error.

Dynamic arrays are reference counted: assignment of one dynamic array-type variable to another will let both variables point to the same array. Contrary to ansistrings, an assignment to an element of one array will be reflected in the other: there is no copy-on-write. Consider the following example:

```pascal
Var  
  A,B : TByteArray;  
 
begin  
  SetLength(A,10);  
  A[0]:=33;  
  B:=A;  
  A[0]:=31;
```

After the second assignment, the first element in B will also contain 31.

It can also be seen from the output of the following example:

```pascal
program testarray1;  
 
Type  
  TA = Array of array of Integer;  
 
var  
  A,B : TA;  
  I,J : Integer;  
begin  
  Setlength(A,10,10);  
  For I:=0 to 9 do  
    For J:=0 to 9 do  
      A[I,J]:=I*J;  
  For I:=0 to 9 do  
    begin  
    For J:=0 to 9 do  
      Write(A[I,J]:2,' ');  
    Writeln;  
    end;  
  B:=A;  
  Writeln;  
  For I:=0 to 9 do  
    For J:=0 to 9 do  
      A[9-I,9-J]:=I*J;  
  For I:=0 to 9 do  
    begin  
    For J:=0 to 9 do  
      Write(B[I,J]:2,' ');  
    Writeln;  
    end;  
end.
```

The output of this program will be a matrix of numbers, and then the same matrix, mirrored.

As remarked earlier, dynamic arrays are reference counted: if in one of the previous examples A goes out of scope and B does not, then the array is not yet disposed of: the reference count of A (and B) is decreased with 1. As soon as the reference count reaches zero the memory, allocated for the contents of the array, is disposed of.

The SetLength call will make sure the reference count of the returned array is 1, that is, if two dynamic array variables were pointing to the same memory they will no longer do so after the setlength call:

```pascal
program testunique;  
 
Type  
  TA = array of Integer;  
 
var  
  A,B : TA;  
  I : Integer;  
 
begin  
  Setlength(A,10);  
  For I:=0 to 9 do  
    A[I]:=I;  
  B:=A;  
  SetLength(B,6);  
  A[0]:=123;  
  For I:=0 to 5 do  
    Writeln(B[I]);  
end.
```

It is also possible to copy and/or resize the array with the standard Copy function, which acts as the copy function for strings:

```pascal
program testarray3;  
 
Type  
  TA = array of Integer;  
 
var  
  A,B : TA;  
  I : Integer;  
 
begin  
  Setlength(A,10);  
  For I:=0 to 9 do  
      A[I]:=I;  
  B:=Copy(A,3,6);  
  For I:=0 to 5 do  
    Writeln(B[I]);  
end.
```

The `Copy` function will copy six elements of the array to a new array. Starting at the element at index 3 (i. e. the fourth element) of the array.

The `Length` function will return the number of elements in the array. The `Low` function on a dynamic array will always return 0, and the `High` function will return the value `Length-1`, i. e., the value of the highest allowed array index.

#### 3.2.2.3 Dynamic array Type compatibility

Object Pascal is a strictly typed language. Two technically distinct types are sometimes considered assignment compatible (i. e. a value of one type can be assigned to a variable of another type) under certain circumstances. Dynamic arrays are considered assignment compatible when they use the same element type. That means that the following will compile:

```pascal
{$mode objfpc}  
 
Type  
  TA = Array of Integer;  
  TB = Array of Integer;  
 
Var  
  A : TA;  
  B : TB;  
 
begin  
  SetLength(A,1);  
  A[0]:=1;  
  B:=A;  
end.
```

But the following will not, even though the integer and word types are assignment compatible:

```pascal
{$mode objfpc}  
 
Type  
  TA = Array of Word;  
  TB = Array of Integer;  
 
Var  
  A : TA;  
  B : TB;  
 
begin  
  SetLength(A,1);  
  A[0]:=1;  
  B:=A;  
end.
```

#### 3.2.2.4 Dynamic array constructor

As of version 3.0 of Free Pascal, Dynamic array types have a constructor. This is intrinsic, the compiler provides it. Up to version 2.6.4, the only way to initialize a dynamic array was as follows:

```pascal
Type  
  TIntegerArray = Array of Integer;  
 
var  
  A : TIntegerArray;  
 
begin  
  SetLength(A,3);  
  A[0]:=1;  
  A[1]:=2;  
  A[3]:=3;  
  Writeln(Length(A));  
end.
```

As of version 3.0 of Free Pascal, an dynamic array can be initialized using a constructor-like syntax. The constructor is called Create, and accepts as parameters a variable number of parameters of the element type of the array type. This means the above initialization can now be done as:

```pascal
Type  
  TIntegerArray = Array of Integer;  
 
var  
  A : TIntegerArray;  
 
begin  
  A:=TIntegerArray.Create(1,2,3);  
  Writeln(Length(A));  
end.
```

Note that this will not work for dynamic arrays for which no type was created. That is, the following will not work:

```pascal
var  
  A : Array of Integer;  
 
begin  
  A:=Array of Integer.Create(1,2,3);  
  Writeln(Length(A));  
end.
```

This approach also works recursively for multi-dimensional arrays:

```pascal
Type  
  TIntegerArray = Array of Integer;  
  TIntegerArrayArray = Array of TIntegerArray;  
 
var  
  A : TIntegerArrayArray;  
 
begin  
  A:=TIntegerArrayArray.Create(TIntegerArray.Create(1,2,3),  
                               TIntegerArray.Create(4,5,6),  
                               TIntegerArray.Create(7,8,9));  
  Writeln('Length ',length(A));  
end.
```

However, since it is a constructor (code is run at run-time) it is not possible to use this in an initialized variable syntax. That is, the following will not
work:

```pascal
Type  
  TIntegerArray = Array of Integer;  
 
var  
  A : TIntegerArray = TIntegerArray.Create(1,2,3);  
 
begin  
  Writeln('Length ',length(A));  
end.
```

#### 3.2.2.5 Dynamic array constant expressions

As of version 3.2 of the compiler, an array can be constructed using an array expression in an explicit assignment or in an initialized variable. However, the expression is different. In an assignment statement, it resembles a set expression, in an initializd variable, the same syntax as for a constant array of fixed length must be used:

```pascal
Type  
  TIntegerArray = Array of Integer;  
 
var  
  A : TIntegerArray = (1,2,3);  
  B : TIntegerArray;  
 
begin  
  B:=[3,4,5];  
end.
```

#### 3.2.2.6 Packing and unpacking an array

Arrays can be packed and bitpacked. Two array types which have the same index type and element type, but which are differently packed are not assignment compatible.

However, it is possible to convert a normal array to a bitpacked array with the pack routine. The reverse operation is possible as well; a bitpacked array can be converted to a normally packed array using the unpack routine, as in the following example:

```pascal
Var  
  foo : array [ 'a'..'f' ] of Boolean  
    = ( false, false, true, false, false, false );  
  bar : bitpacked array [ 42..47 ] of Boolean;  
  baz : array [ '0'..'5' ] of Boolean;  
 
begin  
  pack(foo,'a',bar);  
  unpack(bar,baz,'0');  
end.
```

More information about the pack and unpack routines can be found in the system unit reference.

### 3.3.3 Record types

Free Pascal supports fixed records and records with variant parts.

So the following are valid record type declarations:

```pascal
Type  
  Point = Record  
    X,Y,Z : Real;  
    end;  
  RPoint = Record  
    Case Boolean of  
      False : (X,Y,Z : Real);  
      True : (R,theta,phi : Real);  
    end;  
  BetterRPoint = Record  
    Case UsePolar : Boolean of  
      False : (X,Y,Z : Real);  
      True : (R,theta,phi : Real);  
    end;
```

The variant part must be last in the record. The optional identifier in the case statement serves to access the tag field value, which otherwise would be invisible to the programmer. It can be used to see which variant is active at a certain time3. In effect, it introduces a new field in the record.

Remark It is possible to nest variant parts, as in:

```pascal
Type  
  MyRec = Record  
    X : Longint;  
      Case byte of  
        2 : (Y : Longint;  
      case byte of  
        3 : (Z : Longint);  
      );  
      end;
```

#### 3.3.3.1 Record layout and size

The layout and size of a record is influenced by five aspects:

- The size of its fields.
  The alignment requirements of the types of the fields, which are platform-dependent. Note that the alignment requirements of a type inside a record may be different from those of a separate variable of that type. Additionally, the location of a field inside a record may also influence its type’s alignment requirements.
- The currently active `{$ALIGN N}` or `{$PACKRECORDS N}` setting (these settings override each
  other, so the last one specified is the active one; note that these directives to not accept exactly the same arguments, see the programmer’s manual for more information).
- The currently active `{$CODEALIGN RECORDMIN=X}` setting.
- The currently active `{$CODEALIGN RECORDMAX=X}` setting.

The layout and size of variant parts in records is determined by replacing them with a field whose type is a record with as first element a field of the tag field type if an identifier was declared for this tag field, followed by the elements of the biggest variant.

Field F2’s offset in a record is equal to the sum of the previous field F1’s offset and F1’s size, rounded up to a multiple of F2’s required alignment. This required alignment is calculated as follows:

- The required alignment is set to the default alignment of the field’s type,
  Possibly adjusted based on the fact that this type occurs in a record and on the field’s location in the record.
- If the required alignment is smaller than the currently active `{$CODEALIGN  RECORDMIN=X}`
  setting, it is changed to this X value.
- If the currently active `{$ALIGN N}` or {`$PACKRECORDS N}` setting is a numerical value: if the
  required alignment is larger than N, it is changed to N. I. e., if N is 1, all fields will be placed right after each other.
- RESET or DEFAULT: the resulting required alignment is target dependent.
- C: the required alignment is adjusted according to the rules specified in the
  official ABI for the current platform.
- POWER/POWERPC, MAC68K: the alignment value’s adjustment is determined by
  following the official ABI rules for resp. the (Classic) Macintosh PowerPC or Macintosh 680x0 platforms.

The size of a record is equal to the sum of the record’s last field’s offset and this field’s size, rounded up to a multiple of the record’s required alignment. The record’s required alignment is calculated as follows:

- The required alignment is set to the alignment of the record’s field with the largest alignment,
  as determined while laying out the record.
- If the current `{$ALIGN N}` or `{$PACKRECORDS N}` setting is different from C and the required
  alignment is larger than than the currently active {$CODEALIGN RECORDMAX=X}, the required alignment is changed to X.
- If the current `{$ALIGN N}` or `{$PACKRECORDS N}` setting is equal to C, the required alignment is determined by following the official ABI rules.

#### 3.3.3.2 Remarks and examples

Free Pascal also supports a “packed record”, which is a record where all the elements are byte-aligned. As a result, the two following declarations are equivalent:

```pascal
{$PackRecords 1}  
Trec2 = Record  
  A : Byte;  
  B : Word;  
  end;  
{$PackRecords default}
```

and

```pascal
Trec2 = Packed Record  
  A : Byte;  
  B : Word;  
  end;
```

Note the {$PackRecords Default} after the first declaration to restore the default setting!

Given the platform-dependent nature of how records are laid out in memory, the only way to ensure a compatible layout across platforms (assuming that all fields are declared using a type with the same meaning across these same platforms) is by using `{$PACKRECORDS 1}`.

In particular, if a typed file with records, produced by a Turbo Pascal program, must be read, then chances are that attempting to read that file correctly will fail. The reason is that Free Pascal’s default `{$PACKRECORDS N}` setting is not necessarily compatible with Turbo Pascal’s. It can be changed to `{$PACKRECORDS 1}` or `{$PACKRECORDS 2}` depending on the setting used in the Turbo Pascal program that create the file (although it may still fail with `{$PACKRECORDS 2}` due to different type alignment requirements between 16 bit MSDOS and your current platform).

The same remark goes for Delphi: exchanging data is only guaranteed to be possible if both the producer and consumer use a packed record, or if they are on the same platform and use the same `{$PACKRECORDS X}` setting.

### 3.3.4 Set types

Free Pascal supports the set types as in Turbo Pascal.

Each of the elements of SetType must be of type TargetType. TargetType can be any ordinal type with a range between 0 and 255. A set can contain at most 256 elements. The following are valid set declaration:

```pascal
Type  
  Junk = Set of Char;  
  Days = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);  
 
Var  
  WorkDays : Set of days;
```

Given these declarations, the following assignment is legal:

```pascal
WorkDays := [Mon, Tue, Wed, Thu, Fri];
```

Several operations can be done on sets: taking unions or differences, adding or removing elements, comparisons. These are documented in section 12.8.6, page 639

How the compiler stores sets depends on the mode and can be controlled with a directive. For more information, see the programmer’s guide.

### 3.3.5 File types

File types are types that store a sequence of some base type, which can be any type except another file type. It can contain (in principle) an infinite number of elements. File types are used commonly to store data on disk. However, nothing prevents the programmer, from writing a file driver that stores its data for instance in memory.

If no type identifier is given, then the file is an untyped file; it can be considered as equivalent to a file of bytes. Untyped files require special commands to act on them (see Blockread, Blockwrite). The following declaration declares a file of records:

```pascal
Type  
  Point = Record  
    X,Y,Z : real;  
    end;  
  PointFile = File of Point;
```

Internally, files are represented by the FileRec record, which is declared in the Dos or SysUtils units.

A special file type is the Text file type, represented by the TextRec record. A file of type Text uses special input-output routines. The default Input, Output and StdErr file types are defined in the system unit: they are all of type Text, and are opened by the system unit initialization code.

## 3.4 Pointers

Free Pascal supports the use of pointers. A variable of the pointer type contains an address in memory, where the data of another variable may be stored. A pointer type can be defined as follows:

As can be seen from this diagram, pointers are typed, which means that they point to a particular kind of data. The type of this data must be known at compile time.

Dereferencing the pointer (denoted by adding ^ after the variable name) behaves then like a variable. This variable has the type declared in the pointer declaration, and the variable is stored in the address that is pointed to by the pointer variable. Consider the following example:

```pascal
Program pointers;  
type  
  Buffer = String[255];  
  BufPtr = ^Buffer;  
Var B  : Buffer;  
    BP : BufPtr;  
    PP : Pointer;  
```

etc..

In this example, BP is a pointer to a Buffer type; while B is a variable of type Buffer. B takes 256 bytes memory, and BP only takes 4 (or 8) bytes of memory: enough memory to store an address.

The expression

```pascal
BP^
```

is known as the dereferencing of BP. The result is of type Buffer, so

```pascal
BP^[23]
```

Denotes the 23rd character in the string pointed to by BP.

Remark Free Pascal treats pointers much the same way as C does. This means that a pointer to some type can be treated as being an array of this type.

From this point of view, the pointer then points to the zeroeth element of this array. Thus the following pointer declaration

```pascal
Var p : ^Longint;
```

can be considered equivalent to the following array declaration:

```pascal
Var p : array[0..Infinity] of Longint;
```

The difference is that the former declaration allocates memory for the pointer only (not for the array), and the second declaration allocates memory for the entire array. If the former is used, the memory must be allocated manually, using the Getmem function. The reference P^ is then the same as p[0]. The following program illustrates this maybe more clear:

```pascal
program PointerArray;  

var 
  i : Longint;  
  p : ^Longint;  
  pp : array[0..100] of Longint;  

begin  
  for i := 0 to 100 do pp[i] := i; { Fill array }  
  p := @pp[0];                     { Let p point to pp }  
  for i := 0 to 100 do  
    if p[i]<>pp[i] then  
      WriteLn ('Ohoh, problem !')  
end.
```

Free Pascal supports pointer arithmetic as C does. This means that, if P is a typed pointer, the instructions

```pascal
Inc(P);  
Dec(P);
```

will increase, respectively decrease the address the pointer points to with the size of the type P is a pointer to. For example

```pascal
Var P : ^Longint;  
...  
 Inc (p);
```

will increase P by 4, because 4 is the size of a longint. If the pointer is untyped, a size of 1 byte is assumed (i. e. as if the pointer were a pointer to a byte: ^byte.)

Normal arithmetic operators on pointers can also be used, that is, the following are valid pointer arithmetic operations:

```pascal
var  
  p1,p2 : ^Longint;  
  L : Longint;  

begin  
  P1 := @P2;  
  P2 := @L;  
  L := P1-P2;  
  P1 := P1-4;  
  P2 := P2+4;  
end.
```

Here, the value that is added or subtracted is multiplied by the size of the type the pointer points to. In the previous example P1 will be decremented by 16 bytes, and P2 will be incremented by 16.

## 3.5 Forward type declarations

Programs often need to maintain a linked list of records. Each record then contains a pointer to the next record (and possibly to the previous record as well). For type safety, it is best to define this pointer as a typed pointer, so the next record can be allocated on the heap using the New call. In order to do so, the record should be defined something like this:

```pascal
Type  
  TListItem = Record  
    Data : Integer;  
    Next : ^TListItem;  
  end;
```

When trying to compile this, the compiler will complain that the TListItem type is not yet defined when it encounters the Next declaration: This is correct, as the definition is still being parsed.

To be able to have the Next element as a typed pointer, a “Forward type declaration” must be introduced:

```pascal
Type  
  PListItem = ^TListItem;  
  TListItem = Record  
    Data : Integer;  
    Next : PTListItem;  
  end;
```

When the compiler encounters a typed pointer declaration where the referenced type is not yet known, it postpones resolving the reference till later. The pointer definition is a “Forward type declaration”.

The referenced type should be introduced later in the same Type block. No other block may come between the definition of the pointer type and the referenced type. Indeed, even the word Type itself may not re-appear: in effect it would start a new type-block, causing the compiler to resolve all pending declarations in the current block.

In most cases, the definition of the referenced type will follow immediately after the definition of the pointer type, as shown in the above listing. The forward defined type can be used in any type definition following its declaration.

Note that a forward type declaration is only possible with pointer types and classes, not with other types.

## 3.6 Procedural types

Free Pascal has support for procedural types, although it differs a little from the Turbo Pascal or Delphi implementation of them. The type declaration remains the same, as can be seen in the following syntax diagram:

For a description of formal parameter lists, see chapter 14, page 740. The two following examples are valid type declarations:

```pascal
Type TOneArg = Procedure (Var X : integer);  
  TNoArg = Function : Real;  
var 
  proc : TOneArg;  
  func : TNoArg;
```

One can assign the following values to a procedural type variable:

- Nil, for both normal procedure pointers and method pointers.
- A variable reference of a procedural type, i. e. another variable of the same type.
- A global procedure or function address, with matching function or procedure header and calling
  convention.
- A method address.

Given these declarations, the following assignments are valid:

```pascal
Procedure printit (Var X : Integer);  
begin  
  WriteLn (x);  
end;  
...  
Proc := @printit;  
Func := @Pi;
```

From this example, the difference with Turbo Pascal is clear: In Turbo Pascal it isn’t necessary to use the address operator (@) when assigning a procedural type variable, whereas in Free Pascal it is required. In case the -MDelphi or -MTP switches are used, the address operator can be dropped.

Remark The modifiers concerning the calling conventions must be the same as the declaration; i. e. the following code would give an error:

```pascal
Type TOneArgCcall = Procedure (Var X : integer);cdecl;  
var proc : TOneArgCcall;  
Procedure printit (Var X : Integer);  
begin  
  WriteLn (x);  
end;  
begin  
Proc := @printit;  
end.
```

Because the TOneArgCcall type is a procedure that uses the cdecl calling convention.

In case the is nested modified is added, then the procedural variable can be used with nested procedures. This requires that the sources be compiled in macpas or ISO mode, or that the nestedprocvars modeswitch be activated:

```pascal
{$modeswitch nestedprocvars}  
program tmaclocalprocparam3;  
 
type  
  tnestedprocvar = procedure is nested;  
 
var  
  tempp: tnestedprocvar;  
 
procedure p1( pp: tnestedprocvar);  
begin  
  tempp:=pp;  
  tempp  
end;  
 
procedure p2( pp: tnestedprocvar);  
var  
  localpp: tnestedprocvar;  
begin  
  localpp:=pp;  
  p1( localpp)  
end;  
 
procedure n;  
begin  
  writeln( 'calling through n')  
end;  
 
procedure q;  
 
var qi: longint;  
 
  procedure r;  
  begin  
    if qi = 1 then  
      writeln( 'success for r')  
    else  
      begin  
      writeln( 'fail');  
      halt( 1)  
    end  
  end;  
 
begin  
  qi:= 1;  
  p1( @r);  
  p2( @r);  
  p1( @n);  
  p2( @n);  
end;  
 
begin  
  q;  
end.
```

In case one wishes to assign methods of a class to a variable of procedural type, the procedural type must be declared with the of object modifier.

The two following examples are valid type declarations for method procedural variables (also known as event handlers because of their use in GUI design):

```pascal
Type TOneArg = Procedure (Var X : integer) of object;  
  TNoArg = Function : Real of object;  
var  
  oproc : TOneArg;  
  ofunc : TNoArg;
```

A method of the correct signature can be assigned to these functions. When called, Self will be pointing to the instance of the object that was used to assign the method procedure.

The following object methods can be assigned to oproc and ofunc:

```pascal
Type  
  TMyObject = Class(TObject)  
    Procedure DoX (Var X : integer);  
    Function DoY: Real;  
  end;  
 
Var  
  M : TMyObject;  
 
begin  
  oproc:=@M.DoX;  
  ofunc:=@M.DOY;  
end;
```

When calling oproc and ofunc, Self will equal M.

This mechanism is sometimes called `Delegation`.

Remark When comparing two variables of type method, only the method’s address is compared, not the instance pointer. That means that the following program will print True:

```pascal
Type  
  TSomeMethod = Procedure  of object;  
 
  TMyObject = Class(TObject)  
    Procedure DoSomething;  
  end;  
 
Procedure TMyObject.DoSomething;  
 
begin  
  Writeln('In DoSomething');  
end;  
 
var  
  X,Y : TMyObject;  
  P1,P2 : TSomeMethod;  
 
begin  
  X:=TMyObject.Create;  
  Y:=TMyObject.Create;  
  P1:=@X.DoSomething;  
  P2:=@Y.DoSomething;  
  Writeln('Same method : ',P1=P2);  
end.
```

If both pointers must be compared, a typecast to TMethod must be done, and both pointers should be compared. TMethod is defined in the system unit as follows:

```pascal
TMethod = record  
  Code : CodePointer;  
  Data : Pointer;  
end;
```

The following program will therefore print False:

```pascal
Type  
  TSomeMethod = Procedure  of object;  
 
  TMyObject = Class(TObject)  
    Procedure DoSomething;  
  end;  
 
Procedure TMyObject.DoSomething;  
 
begin  
  Writeln('In DoSomething');  
end;  
 
var  
  X,Y : TMyObject;  
  P1,P2 : TMethod;  
 
begin  
  X:=TMyObject.Create;  
  Y:=TMyObject.Create;  
  P1:=TMethod(@X.DoSomething);  
  P2:=TMethod(@Y.DoSomething);  
  Writeln('Same method : ',(P1.Data=P2.Data) and (P1.Code=P1.Code));  
end. 
```

## 3.7 Variant types

### 3.7.1 Definition

As of version 1.1, FPC has support for variants. For maximum variant support it is recommended to add the variants unit to the uses clause of every unit that uses variants in some way: the variants unit contains support for examining and transforming variants other than the default support offered by the System or ObjPas units.

The type of a value stored in a variant is only determined at runtime: it depends what has been assigned to the variant. Almost any simple type can be assigned to variants: ordinal types, string types, int64 types.

Structured types such as sets, records, arrays, files, objects and classes are not assignment-compatible with a variant, as well as pointers. Interfaces and COM or CORBA objects can be assigned to a variant (basically because they are simply a pointer).

This means that the following assignments are valid:

```pascal
Type  
  TMyEnum = (One,Two,Three);  
 
Var  
  V : Variant;  
  I : Integer;  
  B : Byte;  
  W : Word;  
  Q : Int64;  
  E : Extended;  
  D : Double;  
  En : TMyEnum;  
  AS : AnsiString;  
  WS : WideString;  
 
begin  
  V:=I;  
  V:=B;  
  V:=W;  
  V:=Q;  
  V:=E;  
  V:=En;  
  V:=D;  
  V:=AS;  
  V:=WS;  
end;
```

And of course vice-versa as well.

A variant can hold an array of values: All elements in the array have the same type (but can be of type “variant”). For a variant that contains an array, the variant can be indexed:

```pascal
Program testv;  
 
uses variants;  
 
Var  
  A : Variant;  
  I : integer;  
 
begin  
  A:=VarArrayCreate([1,10],varInteger);  
  For I:=1 to 10 do  
    A[I]:=I;  
end.
```

For the explanation of VarArrayCreate, see Unit Reference.

Note that when the array contains a string, this is not considered an “array of characters”, and so the variant cannot be indexed to retrieve a character at a certain position in the string.

As can be seen from the definition above, most simple types can be assigned to a variant. Likewise, a variant can be assigned to a simple type: If possible, the value of the variant will be converted to the type that is being assigned to. This may fail: Assigning a variant containing a string to an integer will fail unless the string represents a valid integer. In the following example, the first assignment will work, the second will fail:

```pascal
program testv3;  
 
uses Variants;  
 
Var  
  V : Variant;  
  I : Integer;  
 
begin  
  V:='100';  
  I:=V;  
  Writeln('I : ',I);  
  V:='Something else';  
  I:=V;  
  Writeln('I : ',I);  
end.
```

The first assignment will work, but the second will not, as Something else cannot be converted to a valid integer value. An EConvertError exception will be the result.

The result of an expression involving a variant will be of type variant again, but this can be assigned to a variable of a different type – if the result can be converted to a variable of this type.

Note that expressions involving variants take more time to be evaluated, and should therefore be used with caution. If a lot of calculations need to be made, it is best to avoid the use of variants.

When considering implicit type conversions (e. g. byte to integer, integer to double, char to string) the compiler will ignore variants unless a variant appears explicitly in the expression.

Remark Dispatch interface support for variants is currently broken in the compiler.

Variants can contain a reference to an interface – a normal interface (descending from IInterface) or a dispatchinterface (descending from IDispatch). Variants containing a reference to a dispatch interface can be used to control the object behind it: the compiler will use late binding to perform the call to the dispatch interface: there will be no run-time checking of the function names and parameters or arguments given to the functions. The result type is also not checked. The compiler will simply insert code to make the dispatch call and retrieve the result.

This means basically, that you can do the following on Windows:

```pascal
Var  
  W : Variant;  
  V : String;  
 
begin  
  W:=CreateOleObject('Word.Application');  
  V:=W.Application.Version;  
  Writeln('Installed version of MS Word is : ',V);  
end;
```

The line

```pascal
  V:=W.Application.Version;
```

is executed by inserting the necessary code to query the dispatch interface stored in the variant W, and execute the call if the needed dispatch information is found.

## 3.8 Type aliases

Type aliases are a way to give another name to a type, but can also be used to create real new types. Which of the two depends on the way the type alias is defined:

The first case is just a means to give another name to a type:

```pascal
Type  
  MyInteger = Integer;
```

This creates a new name to refer to the Integer type, but does not create an actual new type. That is, two variables:

```pascal
Var  
  A : MyInteger;  
  B : Integer;
```

Will actually have the same type from the point of view of the compiler (namely: Integer).

The above presents a way to make types platform independent, by only using the alias types, and then defining these types for each platform individually. Any programmer who then uses these custom types doesn’t have to worry about the underlying type size: it is opaque to him. It also allows to use shortcut names for fully qualified type names. e. g. define system.longint as Olongint and then redefine longint.

The alias is frequently seen to re-expose a type:

```pascal
Unit A;  
 
Interface  
 
Uses B;  
 
Type  
  MyType = B.MyType;
```

This construction is often seen after some refactoring, when moving some declarations from unit A to unit B, to preserve backwards compatibility of the interface of unit A.

The second case is slightly more subtle:

```pascal
Type  
  MyInteger = Type Integer;
```

This not only creates a new name to refer to the Integer type, but actually creates a new type. That is, two variables:

```pascal
Var  
  A : MyInteger;  
  B :  Integer;
```

Will not have the same type from the point of view of the compiler. However, these two types will be assignment compatible. That means that an assignment

```pascal
  A:=B;
```

will work.

The difference can be seen when examining type information:

```pascal
If TypeInfo(MyInteger)<>TypeInfo(Integer) then  
  Writeln('MyInteger and Integer are different types');
```

The compiler function TypeInfo returns a pointer to the type information in the binary. Since the two types MyInteger and Integer are different, they will generate different type information blocks, and the pointers will differ.

There are three consequences of having different types:

- That they have different typeinfo, hence different RTTI (Run-Time Type
  Information).
- They can be used in function overloads, that is
  
  ```pascaode
  Procedure MyProc(A : MyInteger); overload;  
  Procedure MyProc(A : Integer); overload;
  ```

  will work. This will not work with a simple type alias.
- They can be used in operator overloads, that is
  
  ```pascaode
  Operator +(A,B : MyInteger) : MyInteger;
  ```

  will work too.

## 3.9 Managed types

By default, pascal types are unmanaged. That means that variables must be explicitly initialized, finalized, memory allocated and so on. However, in Object Pascal, several types are managed, which means that the compiler initializes and finalizes variables of this type: this is necessary, for instance for reference counted data types.

The following types are managed:

- **AnsiString**
  They are initialized to Nil.

- **UnicodeString**
  They are initialized to Nil.

- **WideString**
  They are initialized to Nil.

- **Interface**
  They are initialized to Nil.

- **Dynamic arrays**
  They are initialized to Nil.

And any record or array whose elements contain managed types.

Class instances containing managed types are also initialized, but the class instance pointer itself is not.

Variables of managed types will also be finalized: this means, in general, that their reference count will be decreased at the latest at the end of the current scope.

**Napomena**:  
Note that no assumption should be made about the exact time of this finalization. All that is guaranteed that they are finalized when they go out of scope.

[prev][f1] [content][f0] [next][f2]

[f2]: 04_variables.md
[f0]: 00_contents.md
[f1]: 02_constants.md
