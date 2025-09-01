/*
Interfaces in Go
================

Interfaces allow you to focus on what a type does rather than how it's built.
They can help you write more flexible and reusable code by defining behaviors
(like methods) that different types can share. This makes it easy to swap out
or update parts of your code without changing everything else.

Interfaces are just collections of method signatures. A type "implements" an
interface if it has methods that match the interface's method signatures.

In the following example, a "shape" must be able to return its area and perimeter.
Both rect and circle fulfill the interface.

type shape interface {
    area() float64
    perimeter() float64
}

type rect struct {
    width, height float64
}
func (r rect) area() float64 {
    return r.width * r.height
}
func (r rect) perimeter() float64 {
    return 2*r.width + 2*r.height
}

type circle struct {
    radius float64
}
func (c circle) area() float64 {
    return math.Pi * c.radius * c.radius
}
func (c circle) perimeter() float64 {
    return 2 * math.Pi * c.radius
}

When a type implements an interface, it can then be used as that interface type.

func printShapeData(s shape) {
	fmt.Printf("Area: %v - Perimeter: %v\n", s.area(), s.perimeter())
}

Because we say the input is of type shape, we know that any argument must implement
the .area() and .perimeter() methods.

As an example, because the empty interface doesn't require a type to have any methods
at all, every type automatically implements the empty interface, written as:

interface{}

Assignment
----------

The birthdayMessage and sendingReport structs already have implementations of the getMessage
method. The getMessage method returns a string, and any type that implements the method can
be considered a message (meaning it implements the message interface).

    Add the getMessage() method signature as a requirement on the message interface.
    Complete the sendMessage function. It should return:
        The content of the message.
        The cost of the message, which is the length of the message multiplied by 3.

Notice that your code doesn't care at all about whether a specific message is a birthdayMessage
or a sendingReport!
*/

package main

import (
	"fmt"
	"time"
)

func sendMessage(msg message) (string, int) {
	content := msg.getMessage()
	cost := len(content) * 3
	return content, cost
}

type message interface {
	getMessage() string
}

// don't edit below this line

type birthdayMessage struct {
	birthdayTime  time.Time
	recipientName string
}

func (bm birthdayMessage) getMessage() string {
	return fmt.Sprintf("Hi %s, it is your birthday on %s", bm.recipientName,
		bm.birthdayTime.Format(time.RFC3339))
}

type sendingReport struct {
	reportName    string
	numberOfSends int
}

func (sr sendingReport) getMessage() string {
	return fmt.Sprintf(`Your "%s" report is ready. You've sent %v messages.`,
		sr.reportName, sr.numberOfSends)
}
