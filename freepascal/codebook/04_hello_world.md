
# Hello, World

How would you like to do your Hello World?

- I'm using the Lazarus IDE
- I'm using the CLI

## Using the Lazarus IDE

> **Note**:  
  This section assumes you have correctly set up the following for your OS.
>
> - The Free Pascal Compiler.
> - The Lazarus IDE.

### Create a Project

- Launch Lazarus IDE.
- Create a new Project.
  - On the top menu bar, click `Project -> Simple Program -> OK`.
- Save this Project.
  - Click Project -> Save Project.
  - Save the Project file as `HelloWorld.lpi` in a new folder.
  - **Note**: Lazarus will save the main source file as `HelloWorld.lpr`.

You will see a simple program in the Source Editor window. The program's name will be the same as the Project's name, as shown below.

```pascal
program HelloWorld;

begin
end.
```

### Add Code

Now insert the following lines between `begin` and `end`.

```pascal
WriteLn('Hello, World!');
ReadLn; // Add this to pause the program and see the output
```

The `WriteLn` function prints text on the console.  
The `ReadLn` waits for you to press Enter, keeping the console window open so
you can see the "Hello, World!" message.  
Add the following compiler directives after the program declaration.

```pascal
{$mode objfpc}{$H+}{$J-}
```

**Note**
This line `{$mode objfpc}{$H+}{$J-}` is a common setup for Free Pascal projects. It tells the compiler to use modern Object Pascal features and settings. It's good practice to include it.

Your final code would look as follows.

```pascal
program HelloWorld;

{$mode objfpc}{$H+}{$J-} // Add this line in your object pascal codes.

begin
  WriteLn('Hello, World!');
  ReadLn;
end.
```

Press Ctrl+S to save the code.

### Compile & Run

Press F9 to compile and run the program.

You should be able to see a console window open with Hello, World! displayed.

Lazarus IDE: Press the Enter key to exit the program and close the console.

## Using the CLI

> **Note**
> This section assumes you have correctly set up the Free Pascal Compiler and
  that fpc is in your PATH.

### Create a .pas File & Add Code

Launch your favourite text editor (e.g., Notepad, VS Code, Sublime Text, etc.).
Create a new file and put the following snippet in it.

```pascal
program HelloWorldCLI; // It's good practice to name your program
{$mode objfpc}{$H+}{$J-} 

begin
    WriteLn('Hello, World!');
    ReadLn; // Add this to pause the program and see the output
end.
```

Save it as HelloWorld.pas.

**Note**  
.pas is the standard file extension for Free Pascal source code. This tells the
compiler it's a Pascal program.

### Compile and Run

#### Windows CLI

On Windows, compile and run as follows.

```sh
fpc HelloWorld.pas && HelloWorld.exe
```

**Tip**
If running fpc from the Command Prompt (CLI) doesn't work, here are a few things to check:

- Full Path: Try using the full path to fpc.exe (e.g., C:\FPC\3.2.
  2\bin\i386-win32\fpc.exe HelloWorld.pas).
- PATH Variable: Ensure the directory containing fpc.exe (e.g., C:\FPC\3.2.
  2\bin\i386-win32\) is added to your system's PATH environment variable. You might need to restart your Command Prompt after updating PATH.
- Lazarus IDE: If CLI is tricky, using the Lazarus IDE (as shown above) is often
  easier for beginners.
- VSCode/VSCodium Users: If you're using VSCode or VSCodium, make sure the Pascal
  extension by Alessandro Fragnani is set up correctly to find the compiler
- OmniPascal: For a more integrated VSCode experience, you might also look into
  OmniPascal.

#### macOS CLI

On macOS, compile and run as follows.

```pascal
fpc HelloWorld.pas && ./HelloWorld
```

#### Linux CLI

On Linux, compile and run as follows.

```sh
fpc HelloWorld.pas && ./HelloWorld
```
