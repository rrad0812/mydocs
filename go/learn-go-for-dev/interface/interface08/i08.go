/*
Type Switches
=============

A type switch makes it easy to do several type assertions in a series.

A type switch is similar to a regular switch statement, but the cases specify
types instead of values.

	func printNumericValue(num interface{}) {
		switch v := num.(type) {
		case int:
			fmt.Printf("%T\n", v)
		case string:
			fmt.Printf("%T\n", v)
		default:
			fmt.Printf("%T\n", v)
		}
	}

	func main() {
		printNumericValue(1)
		// prints "int"

		printNumericValue("1")
		// prints "string"

		printNumericValue(struct{}{})
		// prints "struct {}"
	}

	fmt.Printf("%T\n", v) prints the type of a variable.

Assignment
----------

After submitting our last snippet of code for review, a more experienced gopher
told us to use a type switch instead of successive assertions. Let's make that
improvement!

Implement the getExpenseReport function using a type switch.

	If the expense is an email, return the email's toAddress and the cost of
	the email.
	If the expense is an sms, return the sms's toPhoneNumber and its cost.
	If the expense has any other underlying type, return an empty string and
	0.0 for the cost.
*/
package main

func getExpenseReport(e expense) (string, float64) {
	switch v := e.(type) {
	case email:
		return v.toAddress, v.cost()
	case sms:
		return v.toPhoneNumber, v.cost()
	default:
		return "", 0.0
	}
}

// don't touch below this line

type expense interface {
	cost() float64
}

type email struct {
	isSubscribed bool
	body         string
	toAddress    string
}

type sms struct {
	isSubscribed  bool
	body          string
	toPhoneNumber string
}

type invalid struct{}

func (e email) cost() float64 {
	if !e.isSubscribed {
		return float64(len(e.body)) * .05
	}
	return float64(len(e.body)) * .01
}

func (s sms) cost() float64 {
	if !s.isSubscribed {
		return float64(len(s.body)) * .1
	}
	return float64(len(s.body)) * .03
}

func (i invalid) cost() float64 {
	return 0.0
}
