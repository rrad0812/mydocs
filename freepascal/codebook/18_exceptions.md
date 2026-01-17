
# Exceptions

[Prev](17_dirs_paths_filenames.md) [Content](00_freepascal_cookbook.md) [Next](19_file_handling.md)

## When do I use try...finally blocks

Use them for the following tasks:

- clean up resources or Free memory within a block that allocates memory, or
- close files in case an exception occured.

## When do I use try...exception blocks

Use them to catch exceptions and provide a means to recover in the code.

## Can I nest exception handling

Yes you can. See Language Reference 17.4.

Here is an example. The snippet below has one outer try...except that can catch any error from TFileStream or TStreamReader.

```pascal
program ReadTextFile;

uses
  Classes, SysUtils, streamex;

// Procedure to read a text file
procedure ReadTextFile(const filename: string);
var
  fileStream: TFileStream;
  inputReader: TStreamReader;
  line: string;
begin
  try
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      inputReader := TStreamReader.Create(fileStream);
      try
        while not inputReader.EOF do
        begin
          line := inputReader.ReadLine;
          WriteLn(line);
        end;
      finally
        inputReader.Free;
      end;
    finally
      fileStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
end;

begin

  // Read a file and print to std out
  ReadTextFile('cupcake-ipsum.txt');
end.
```

## How can I make a routine with nested try...finally blocks in a try...except block more readable?

> [!Tip]
> .. your first goal ... to achieve a working solution, then you can think about how to make it pretty.

**Short answer**
Split your routine into sub-routines. Let the the last/outmost `try...except` block handle any uncaught exceptions.

**Better answer**
Consider the snippet of program ReadTextFile; from the previous question, and the following tips from Unofficial Free Pascal Discord.

> [!Tip]  
  Making your code readble

... Why not make it work with either a stream or a filename, using "method" overloading? The filename based version would create a corresponding stream and make use of the other variant, to avoid code duplication and improve readability.

> [!Tip]  
> The Beauty of exceptions

As for exception handling: The "beauty" of exceptions is that they trickle down the call chain until they finally get handled somewhere. That's why it makes sense to handle "low-level" stuff like an unconditional (= irrespective of error state) Free within the blocks that allocated memory.

... I tend to handle everything else at the "outmost" level possible/desirable. The code below has one outer try..except that is supposed to catch any error, be it from `ReadTextFile(filename: string)` (e.g. non-existing file), or the actual stream reader.

```pascal
program ReadTextFileOverloading;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, StreamEx, SysUtils;

// Procedure to read a text file from stream
procedure ReadTextFile(const AStream: TStream); override;
var
  inputReader: TStreamReader;
  line: string;
begin
  inputReader := TStreamReader.Create(AStream);
  try
    while not inputReader.EOF do
    begin
      line := inputReader.ReadLine;
      WriteLn(line);
    end;
  finally
    inputReader.Free;
  end;
end;

// Procedure to read a text file by file name
procedure ReadTextFile(const filename: string); override;
var
  fileStream: TFileStream;
begin
  fileStream := TFileStream.Create(filename, fmOpenRead);
  try
    ReadTextFile(fileStream);
  finally
    fileStream.Free;
  end;
end;

// MAIN
begin
  try
    ReadTextFile('ikel.pas');
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
end.
```

## Where can I get more info on handling exceptions

The Free Pascal wiki has an article on it: Exception.

[Prev](17_dirs_paths_filenames.md) [Content](00_freepascal_cookbook.md) [Next](19_file_handling.md)
