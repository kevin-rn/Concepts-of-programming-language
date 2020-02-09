You can see (some) program output!

It’s hard to debug your program if you cannot see output, we know. To make this easier we show you the output your program produces when ran against your own test suites. We prefer keeping our own tests secret so we won’t show you the output produced when we run your code against our own tests.

Let’s play around with this a bit. In the “Solution” editor just uncomment the print statement then [Save] and [▶ Your Test]. Can you see “Hello cruel world” in the box below the editor?!

Without making any more changes [▶ Spec-test]. You can’t see the output anymore!

Good. Make sure all our specification tests have passed and [Submit] your solution. See you at the next assignment.

```scala
// the solution 

object Solution { 

  def f(x: Int) = {
    println("Hello cruel world")
    x
  }

}

```

```scala
//test: Test

// test of the solution

import org.scalatest.FunSuite

import Solution._

class Test extends FunSuite {

  test("test1") {
    assertResult(42) {
      f(42)
    }
  }

}



```
