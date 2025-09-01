/*
Name Your Interface Parameters
==============================

Consider the following interface:

type Copier interface {
  Copy(string, string) int
}

This is a valid interface, but based on the code alone, can you deduce what kinds
of strings you should pass into the Copy function?

We know the function signature expects 2 string types, but what are they? Filenames?
URLs? Raw string data? For that matter, what the heck is that int that's being returned?

Let's add some named parameters and return data to make it more clear.

type Copier interface {
  Copy(sourceFile string, destinationFile string) (bytesCopied int)
}

Much better. We can see what the expectations are now. The first parameter is the sourceFile,
the second parameter is the destinationFile, and bytesCopied, an integer, is returned.
*/

// Are you required to name the arguments of an interface in order for your code to compile properly?
// Why would you name your interface's method's parameters?
package main
