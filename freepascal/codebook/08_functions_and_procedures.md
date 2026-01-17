# Functions and Procedures

[Prev](07_command_line_parameters.md) [Content](00_freepascal_cookbook.md) [Next](09_immutability.md)

**Can a routine receive a pointer to the passed variable**?

Yes. Use the `var` parameter modifier.

The procedure gets a pointer to the variable that was passed, and uses this pointer to access the variable’s value. From this, it follows that any changes made to the parameter, will propagate back to the calling block. FPC Language Reference 14.4.2.

Here is an example.

```pascal
program ParamModifierVar;

{$mode objfpc}{$H+}{$J-}

uses
  sysutils;

procedure AddFour(var input: integer);
begin
  input := input + 4; // this line modifies the value of passed variable
end;

var
  myNumber: Integer = 10;

begin
  WriteLn('myNumber is ... ', myNumber); // the value will be 10
  AddFour(myNumber);                     // call the procedure
  WriteLn('myNumber is ... ', myNumber); // the value will be 14
  ReadLn;
end. 
```

**How to ensure a passed parameter won't be changed inside a routine**?

Use const.

> [!Note]  
> The compiler decides if the parameter will be passed by value or by reference.

Alternatively, use `constref`.

> [!Note]  
This parameter is the same as `const`. Plus, it also tells the compiler that the value should be passed by reference, like a var parameter.

Here is an example.

```pascal
program ParamModifierConst;

{$mode objfpc}{$H+}{$J-}

function CalcAreaCircle(const radius: Real): Real;
begin

  // Argument 'radius' has a const modifier.
  // Hence, it is not possible to re-assign it another value.
  // Uncommenting the following line may lead to a compile error.
  // radius:=radius*2;
  Result:= Pi * radius * radius;
end;

var
  myRadius:Real = 3.0;
  area:Real = 0.0;

begin
  area := CalcAreaCircle(myRadius);
  WriteLn('The radius is ', myRadius:0:2);
  WriteLn('The area of the circle is ', area:0:2);
  ReadLn;
end.
```

[Prev](07_command_line_parameters.md) [Content](00_freepascal_cookbook.md) [Next](09_immutability.md)