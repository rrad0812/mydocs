
# Strings

[Prev](12_lists.md) [Content](00_freepascal_cookbook.md) [Next](14_numbers.md)

## What is a String

Depending on compiler setting, a String in FPC is an alias for;

- **ShortString** (fixed 255 length),
- **AnsiString** (variable length) or
- **UnicodeString** (UTF16).

When `{$H+}` is not specified, or `{$H-}`, String is an alias for `ShortString`.

Any `ShortString` have a maximum length of 255 characters with the implicit codepage `CP_ACP`. `Shortstring`s are always assumed to use the system code page.

When `{$H+}` is specified, String is an alias for `AnsiString`.

Any String is essentially an `AnsiString` with the `DefaultSystemCodePage` declared in it; `AnsiString(CP_ACP)`. And if the default system code page is `65001`, then any String is `UTF-8`.

With `{$mode delpiunicode}` switch, string is an alias for `Unicodestring` string.

Commonly on Windows, the system code page is 1252. If the system code page is 1252, then any String is 1252.

**Refs**  
What is a String? <https://forum.lazarus.freepascal.org/index.php?topic=58131.0>

## Display UTF-8 on a console

```pascal
begin
  WriteLn('勤奋,勤勉になる,부지런하다!👍');
  WriteLn('Press Enter key to exit');
  ReadLn;
end.
```

Alternatively, you can assign your UTF-8 test to a string variable.

```pascal
var
  s: string = '勤奋,勤勉になる,부지런하다!👍';

begin
  WriteLn(s);
end.                        
```

> [!Note]

If you see garbage characters on console;

- your console might not support code page 65001, or
- your windows does not support UTF on API level (only read/write file in UTF-8)

See this answer from StackOverflow on how to enable code page 65001 on your console.

> [!Warning]

The same answer from StackOverflow also shows how to enable UTF-8 on Windows (system-wide).

DO NOT MISS the caveat section and comments in from that answer.

Enabling UTF-8 system-wide on Windows is currently in beta and could lead to unintended system-wide side effects.

**Refs**  
<https://wiki.freepascal.org/FPC_Unicode_support#Code_pages>  
<https://stackoverflow.com/a/57134096>
<https://superuser.com/a/1435645>

What is my system's default codepage?

See <https://www.freepascal.org/docs-html/rtl/system/defaultsystemcodepage.html>

```pascal
begin
  WriteLn(DefaultSystemCodePage); 
end.                                 
```

If it says 65001, then you should be able to see UTF-8 characters on the console.
Remove trailing chars at the end of a string

Contribution

Gustavo 'Gus' Carreno, from the Unofficial Free Pascal Discord server, shared a neat trick to remove trailing characters by using SetLength(str, length(str) - n);.

Let's say you have a loop that append strings with trailing characters at the end.

One way to remove trailing characters is use a flag to inside the for loop. The logic would be: do not add commas or spaces if we are at the end of the loop.

A simpler way is to use SetLength(str, length(str) - n_chars_to_remove);.

See the example below.

- The for loop completes a sentence with a comma and a space at the end. Line 19-20.
- The trick; `SetLength(line, length(line) - 2);` removes the last 2 chars from the end of the sentence. Line 29.

```pascal
program RemoveTrailingChars;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes;

var
  cities: array of string = ('Sydney', 'Melbourne', 'Surabaya', 'Malang');
  city, line: string;

begin

  // Building a sentence using the array of string.
  line := 'I have been to the following cities: ';
  for city in cities do
    line := line + city + ', ';

  // Now, line has a trailing comma and a space at the end.
  WriteLn(line);

  // Here is a trick to remove additional characters at the end of a string
  // by Gustavo 'Gus' Carreno,  2024-03-16 @ 14:42.
  // Simply use SetLength(str, length(str) - n_chars_to_remove);
  // Source: https://discord.com/channels/570025060312547359/896807098518732901/1218403991092985877
  SetLength(line, length(line) - 2);

  // Finally, just add a dot.
  line := line + '.';
  WriteLn(line);

  // Pause console
  ReadLn;
end.
```

[Prev](12_lists.md) [Content](00_freepascal_cookbook.md) [Next](14_numbers.md)
