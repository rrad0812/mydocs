
# 1 Pascal Tokens

[content][f0] [next][f2]

Tokens are the basic lexical building blocks of source code; they are the “words” of the language; characters are combined into tokens according to the rules of the programming language. There are five classes of tokens:

- **Reserved words**  
  These are words which have a fixed meaning in the language. They cannot be changed or redefined.
- **Identifiers**  
  These are names of symbols that the programmer defines. They can be changed and re-used. They are subject to the scope rules of the language.
- **Operators**  
  These are usually symbols for mathematical or other operations: +, -, * and so on.
- **Separators**  
  This is usually white-space.
- **Constants**  
  Numerical or character constants are used to denote actual values in the source code, such as 1 (integer constant) or 2.3 (float constant) or “String constant” (a string: a piece of text).

In this chapter we describe all the Pascal reserved words, as well as the various ways to denote strings, numbers, identifiers etc.

## 1.1 Symbols

Free Pascal allows all characters, digits and some special character symbols in a Pascal source file.

**Recognised symbols**:

- letter `A .. Z` and `a .. z`
- digit  `0 .. 9`
- hex digit `$A .. $F`, `$a .. $f`
- The following characters have a special meaning:  
  `' + - * / = < > [ ] . , ( ) : ^ @ { } $ # & %`
- and the following character pairs too:  
  `<< >> ** <> >< <= >= := += -= *= /= (* *) (. .) //`
- When used in a range specifier, the character pair `(`. is equivalent to the
  left square bracket `[`.
- Likewise, the character pair `.)` is equivalent to the right square bracket `]`.
- When used for comment delimiters, the character pair `(*` is equivalent to the
  left brace `{`.
- and the character pair `*)` is equivalent to the right brace `}`.

  These character pairs retain their normal meaning in string expressions.

## 1.2 Comments

Comments are pieces of the source code which are completely discarded by the compiler. They exist only for the benefit of the programmer, so he can explain certain pieces of code. For the compiler, it is as if the comments were not present.

The following piece of code demonstrates a comment:

```pascal
(* My beautiful function returns an interesting result *)  
Function Beautiful : Integer;  
```

The use of `(*` and `*)` as comment delimiters dates from the very first days of the Pascal language. It has been replaced mostly by the use of `{` and `}` as comment delimiters, as in the following example:

```pascal
{ My beautiful function returns an interesting result }  
Function Beautiful : Integer;  
```

The comment can also span multiple lines:

```pascal
{  
   My beautiful function returns an interesting result,  
   but only if the argument A is less than B.  
}  
Function Beautiful (A,B : Integer): Integer;
```

Single line comments can also be made with the `//` delimiter:

```pascal
// My beautiful function returns an interesting result  
Function Beautiful : Integer;  
```

The comment extends from the `//` character till the end of the line. This kind of comment was introduced by Borland in the Delphi Pascal compiler.

Free Pascal supports the use of nested comments. The following constructs are valid comments:

```pascal
(* This is an old style comment *)  
{  This is a Turbo Pascal comment }  
// This is a Delphi comment. All is ignored till the end of the line.
```

The following are valid ways of nesting comments:

```pascal
{ Comment 1 (* comment 2 *) }  
(* Comment 1 { comment 2 } *)  
{ comment 1 // Comment 2 }  
(* comment 1 // Comment 2 *)  
// comment 1 (* comment 2 *)  
// comment 1 { comment 2 }
```

The last two comments must be on one line. The following two will give errors:

```pascal
 // Valid comment { No longer valid comment !!  
    }
```

and

```pascal
 // Valid comment (* No longer valid comment !!  
    *)
```

The compiler will react with a “invalid character” error when it encounters such constructs, regardless of the -Mtp switch.

**Napomena**  
In TP and Delphi mode, nested comments are not allowed, for maximum compatibility with existing code for those compilers.

## 1.3 Reserved words

Reserved words are part of the Pascal language, and as such, cannot be redefined by the programmer.Pascal is not case sensitive so the compiler will accept any combination of upper or lower case letters for reserved words.

We make a distinction between Turbo Pascal and Delphi reserved words. In TP mode, only the Turbo Pascal reserved words are recognized, but the Delphi ones can be redefined. By default, Free Pascal recognizes the Delphi reserved words.

### 1.3.1 Turbo Pascal reserved words

The following keywords exist in Turbo Pascal mode

 ... | ... | ... | ... | ... | ... | ... | ... |
 --- | --- | --- | --- | --- | --- | --- | --- |
absolute | and | array | asm | begin | case | const | constructor |
destructor | div | do | downto | else | end | file | for |
function | goto | if | implementation | inherited | inline | interface | label |
mod | nil | object | of | operator | or | packed | procedure |
program | record | reintroduce | repeat | self | set | shl | shr |
string | then | to | type | unit | until | uses | var |
while | with | xor | | | | | |

### 1.3.2 Object Pascal reserved words

The reserved words of Object Pascal (used in Delphi or Objfpc mode) are the same as the Turbo Pascal ones, with the following additional keywords:

 ... | ... | ... | ... | ... | ... | ... |
 --- | --- | --- | --- | --- | --- | --- |
 as | class | dispinterface | except | exports | finalization | finally |
 initialization | inline | is | library | on | out | packed |
property | raise | resourcestring | threadvar | try | | |

### 1.3.3 Modifiers

The following is a list of all modifiers. They are not exactly reserved words in the sense that they can be used as identifiers, but in specific places, they have a special meaning for the compiler, i. e., the compiler considers them as part of the Pascal language.

 ... | ... | ... | ... | ... | ... | ... |
 --- | --- | --- | --- | --- | --- | --- |
 absolute | abstract | alias | assembler | bitpacked | break | cdecl |
 continue | cppdecl | cvar | default | deprecated | dynamic | enumerator |
 experimental | export | external | far | far16 | forward | generic |
 helper | implements | index | interrupt | iocheck | local | message |
 name | near | nodefault | noreturn | nostackframe | oldfpccall | otherwise |
 overload | override | pascal | platform | private | protected | public |
 published | read | register | reintroduce | result | safecall | saveregisters |
 softfloat | specialize | static | stdcall | stored | strict | unaligned |
 unimplemented | varargs | virtual | winapi | write | | |

**Napomena**  
Predefined types such as `Byte`, `Boolean` and `constants` such as `maxint` are not reserved words. They are identifiers, declared in the system unit. This means that these types can be redefined in other units. The programmer is however not encouraged to do this, as it will cause a lot of confusion.

**Napomena**  
As of version 2.5.1 it is possible to use reserved words as identifiers by escaping them with a `&` sign. This means that the following is possible

```pascal
var  
  &var : integer;  
 
begin  
  &var:=1;  
  Writeln(&var);  
end.
```

However, it is not recommended to use this feature in new code, as it makes code less readable. It is mainly intended to fix old code when the list of reserved words changes and encompasses a word that was not yet reserved (See also section 1.4, page 47).

## 1.4 Identifiers

Identifiers denote programmer defined names for specific:

- `constants`,
- `types`,
- `variables`,
- `procedures`,
- `functions`,
- `units` and
- `programs`.

All programmer defined names in the source code – excluding reserved words – are designated as identifiers.

Identifiers consist of between 1 and 127 significant characters (letters, digits and the underscore character), of which the first must be a letter (a–z or A–Z), or an underscore (_).

Like Pascal reserved words, identifiers are case insensitive, that is, both
  
```pascal
  myprocedure;
```

and

```pascal
  MyProcedure;
```

refer to the same procedure.

**Napomena**  
As of version 2.5.1 it is possible to specify a reserved word as an identifier by prepending it with an ampersand (&). This means that the following is possible:

```pascal
program testdo;  
 
procedure &do;  
 
begin  
end;  
 
begin  
  &do;  
end.
```

The reserved word `do` is used as an identifier for the declaration as well as the invocation of the procedure.

## 1.5 Hint directives

Most identifiers (constants, variables, functions or methods, properties) can have a `hint` directive appended to their definition.

Whenever an identifier marked with a hint directive is later encountered by the compiler, then a warning will be displayed, corresponding to the specified hint.

- **deprecated**  
  The use of this identifier is deprecated, use an alternative instead. The deprecated keyword can be followed by a string constant with a message. The compiler will show this message whenever the identifier is encountered.
- **experimental**  
  The use of this identifier is experimental: this can be used to flag new features that should
  be used with caution.
- **platform**  
  This is a platform-dependent identifier: it may not be defined on all platforms.
- **unimplemented**
  This should be used on functions and procedures only. It should be used to signal that a particular feature has not yet been implemented.

The following are examples:

```pascal
Const  
  AConst = 12 deprecated;  

var  
  p : integer platform;  

Function Something : Integer; experimental;  

begin  
  Something:=P+AConst;  
end;  

begin  
  Something;  
end.
```

This would result in the following output:

```sh
testhd.pp(11,15) Warning: Symbol "p" is not portable  
testhd.pp(11,22) Warning: Symbol "AConst" is deprecated  
testhd.pp(15,3) Warning: Symbol "Something" is experimental
```

`Hint directives` can follow all kinds of identifiers:

- `units`,
- `constants`,
- `types`,
- `variables`,
- `functions`,
- `procedures` and
- `methods`.

## 1.6 Numbers

Numbers are by default denoted in decimal notation. Real (or decimal) numbers are written using engineering or scientific notation (e. g. 0.314E1).

For integer type constants, Free Pascal supports four formats:

- Normal, **decimal format** (base 10). This is the standard format.
- **Hexadecimal format** (base 16), in the same way as Turbo Pascal does. To specify a constant
  value in hexadecimal format, prepend it with a dollar sign (`$`). Thus, the hexadecimal $FF
  equals 255 decimal. Note that case is insignificant when using hexadecimal constants.
- As of version 1.0.7, **Octal format** (base 8) is also supported. To specify a constant in octal
  format, prepend it with an ampersand (`&`). For instance 15 is specified in octal notation as &17.
- **Binary notation** (base 2). A binary number can be specified by preceding it with a percent
  sign (`%)`. Thus, 255 can be specified in binary notation as %11111111.

**Napomena**:  
`Octal` and `Binary` notation are not supported in TP or Delphi compatibility mode.

## 1.7 Labels

A label is a name for a location in the source code to which can be jumped to from another location with a goto statement. A Label is a standard identifier or a digit sequence.

**Napomena**:  
The `-Sg` or `-Mtp` switches must be specified before labels can be used. By default, Free Pascal doesn’t support label and goto statements. The {$GOTO ON} directive can also be used to allow use
of labels and the goto statement.

The following are examples of valid labels:

```pascal
Label  
  123,  
  abc;
```

## 1.8 Character strings

A character string (or string for short) is a sequence of zero or more characters (byte sized),enclosed in single quotes, and on a single line of the program source code: no literal carriage return or linefeed characters can appear in the string.

A character set with nothing between the quotes (’’) is an empty string.

The string consists of standard, 8-bit ASCII characters or Unicode (normally UTF-8 encoded)characters. The control string can be used to specify characters which cannot be typed on a
keyboard, such as #27 for the escape character.

The single quote character can be embedded in the string by typing it twice. The C construct of escaping characters in the string (using a backslash) is not supported in Pascal.

The following are valid string constants:
  
```pascal  
  'This is a pascal string'  
  ''  
  'a'  
  'A tabulator character: '#9' is easy to embed'
```

The following is an invalid string:

```pascal
  'the string starts here  
   and continues here'
```

The above string must be typed as:

```pas
  'the string starts here'#13#10'   and continues here'
```

or
  
```pascal  
  'the string starts here'#10'   and continues here'
```

```pascal
on unices (including Mac OS X), and as
  'the string starts here'#13'   and continues here'
```

on a classic Mac-like operating system.

It is possible to use other character sets in strings: in that case the codepage of the source file must be specified with the `{$CODEPAGE XXX}` directive or with the `-Fc` command line option for the compiler. In that case the characters in a string will be interpreted as characters from the specified codepage.

[content][f0] [next][f2]

[f0]: 00_contents.md
[f2]: 02_constants.md
