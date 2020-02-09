When we run your test we need to know what your test class is called. 
Without changing anything, just [Save] and [▶ Your Test]. 
Do you see the error?

Go to the “Test” editor and let’s fix this.

```scala
// the solution 

object Solution { 

  def f(x: Int) = { 1 }

}

```

```scala
//test: Test

/*
We expect the first line in every test editor to be of the form
//test: <ClassName>

Where '<ClassName> is the name of the test class. Look now to the first line
of this editor. In the case of the current test suite the name
specified on the first line should be Test. So go ahead and correct this.
When you're done the first line of this file should look like:

//test: Test

This line is quite fragile so don't add any more spaces or it will break.
After your tests pass, make sure that all the specification tests succeed
and then [Submit] your solution.
*/

import org.scalatest.FunSuite

import Solution._

class Test extends FunSuite {

  test("test1") {
    assertResult(1) {
      f(0)
    }
  }

}



```
